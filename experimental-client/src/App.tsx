import "./App.css";
import React from "react";
import { QueryTool } from "./components/QueryTool";
import Grid from "@mui/material/Unstable_Grid2";
import { QueryClient, QueryClientProvider, useQuery } from "react-query";
import { ResultsTool } from "./components/ResultsTool";

const queryClient = new QueryClient();

export const App = () => {
  // The state of the gremlin query used for fetching data.
  const [gremlinQuery, setGremlinQuery] = React.useState<string>(null);
  // The state of the Gremlin editor. Kept separate from `gremlinQuery` so that
  // data is only fetched when the users presses a button.
  const [gremlinEditorText, setGremlinEditorText] =
    React.useState<string>(null);

  return (
    <div className="App">
      <QueryClientProvider client={queryClient}>
        <Grid container spacing={2}>
          <Grid xs={8}>
            <QueryTool
              editorProps={{
                query: gremlinQuery,
                onQueryUpdated: (event) =>
                  setGremlinEditorText(event.target.value),
                onSubmitButtonClick: (_event) =>
                  setGremlinQuery(gremlinEditorText),
              }}
            />
          </Grid>
          <Grid xs={8}></Grid>
        </Grid>
        <div>
          <ResultsTool gremlinQuery={gremlinQuery} />
        </div>
      </QueryClientProvider>
    </div>
  );
};

export default App;
