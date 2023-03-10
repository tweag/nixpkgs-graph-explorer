import "./QueryTool.css";

import {
  BottomNavigation,
  BottomNavigationAction,
  Fab,
  Paper,
  ThemeProvider,
  createTheme,
} from "@mui/material";
import { match, P } from "ts-pattern";
import React, { ChangeEventHandler, MouseEventHandler, useState } from "react";
import { useQuery } from "react-query";
import { PackageInfo, fetchPackageNames, QueryError } from "../api/Queries";
import MaterialTable, { Column } from "@material-table/core";

// Note: certain css values in Material UI need to be overrided via a custom theme which is what is happening here
const theme = createTheme({
  components: {
    // Name of the component
    MuiTable: {
      styleOverrides: {
        // Name of the slot
        root: {
          // Some CSS
          marginBottom: "0px",
          zIndex: 0,
          border: null,
        },
      },
    },
  },
});

const packageInfoColumns: Column<PackageInfo>[] = [
  { title: "Name", field: "name" },
];

interface QueryToolProps {
  editorProps: QueryEditorProps;
}

interface QueryAreaProps {
  isAdvancedModeEnabled: boolean;
  editorProps: QueryEditorProps;
}

interface QueryEditorProps {
  query: string;
  onQueryUpdated?: ChangeEventHandler<HTMLTextAreaElement>;
  onSubmitButtonClick?: MouseEventHandler<HTMLButtonElement>;
}

const QueryEditor = (props: QueryEditorProps) => (
  <div className="Query-advanced-container">
    Gremlin query
    <textarea
      className="Query-textarea"
      name="gremlinQuery"
      defaultValue={props.query}
      onChange={props.onQueryUpdated}
      rows={8}
    ></textarea>
    <div className="Query-submit-container">
      <Fab onClick={props.onSubmitButtonClick}>GO</Fab>
    </div>
  </div>
);

const QueryArea = (props: QueryAreaProps) => {
  const query = useQuery("packageNames", fetchPackageNames, {
    refetchOnMount: false,
    onSuccess: (result) => {
      return match(result)
        .with(
          { message: P.string, httpResponseCode: P.optional(P.number) },
          () => {
            // TODO: Better logging and perhaps a toast or something here
            console.error("Encountered an error fetching data!");
          }
        )
        .otherwise((x) => console.log(x));
    },
  });

  // Note: Since the query may also return an error type we need to check the type here
  // at runtime.
  const data: PackageInfo[] = match(query.data)
    .with(P.array({ name: P.string }), (x: PackageInfo[]) => x)
    .otherwise(() => {
      console.error(
        "Response when querying packages did not match expected schema. Returning an empty array."
      );
      return [];
    });

  if (!props.isAdvancedModeEnabled) {
    return (
      // Note: Wrapping in custom theme to override some CSS from MUI
      <ThemeProvider theme={theme}>
        <MaterialTable
          title="Packages"
          style={{ marginBottom: 0 }}
          columns={packageInfoColumns}
          data={data}
          isLoading={query.isLoading}
          options={{ maxBodyHeight: 600 }}
          components={{
            // Set the surrounding container
            Container: (props) => (
              <Paper {...props} elevation={0} variant={"outlined"} />
            ),
          }}
        />
      </ThemeProvider>
    );
  } else {
    return (
      <QueryEditor
        query={props.editorProps.query}
        onQueryUpdated={props.editorProps.onQueryUpdated}
        onSubmitButtonClick={props.editorProps.onSubmitButtonClick}
      />
    );
  }
};

export const QueryTool = (props: QueryToolProps) => {
  // Default state
  const [isAdvancedModeEnabled, setIsAdvancedModeEnabled] = useState(false);

  return (
    <Paper className="Query-container" elevation={3} style={{ minHeight: 500 }}>
      <QueryArea
        isAdvancedModeEnabled={isAdvancedModeEnabled}
        editorProps={props.editorProps}
      />
      <Paper
        sx={{
          position: "static",
          bottom: 0,
          left: 0,
          right: 0,
          minWidth: 500,
        }}
        elevation={0}
      >
        <BottomNavigation
          className="Query-bottom-navigation"
          showLabels
          onChange={(event, newValue) => {
            console.log(newValue);
            if (newValue == 0) {
              setIsAdvancedModeEnabled(false);
            }
            if (newValue == 1) {
              setIsAdvancedModeEnabled(true);
            }
          }}
        >
          <BottomNavigationAction label="Packages" />
          <BottomNavigationAction label="Advanced Query" />
        </BottomNavigation>
      </Paper>
    </Paper>
  );
};
