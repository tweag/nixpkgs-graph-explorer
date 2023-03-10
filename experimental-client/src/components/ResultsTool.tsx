import "./ResultsTool.css";
// FIXME: Re-enable these imports when we implement the results table component
// import MaterialTable, { Column } from "@material-table/core";
// import RowData from "@material-table/core";
import React, { useState } from "react";
import { EventObjectCore, EventObjectNode } from "cytoscape";
import { useQuery } from "react-query";
import { submitGremlinQuery, QueryError } from "../api/Queries";
import Graph, { Edge, NodeId, Vertex } from "./Graph";
import { Container, ToggleButton, ToggleButtonGroup } from "@mui/material";
import { P, match } from "ts-pattern";

enum Mode {
  Graph = "graph",
  Table = "table",
  RawText = "raw",
}

interface ResultsAreaProps {
  mode: Mode;
  selectedNodeId: string;
  gremlinQuery: string;
  onNodeClick: (EventObjectNode) => void;
  onBackgroundClick: (EventObjectCore) => void;
}

const ResultsArea = (props: ResultsAreaProps) => {
  // Enable fetching of gremlin query results
  const query = useQuery(
    ["submitGremlinQuery", props.gremlinQuery],
    () => submitGremlinQuery(props.gremlinQuery),
    {
      refetchOnMount: false,
      placeholderData: {
        // FIXME: Update placeholders
        raw: "this is a placeholder raw value",
        warning: "foobar",
        cyto: {},
      },
    }
  );

  const graphStyle = {
    width: "800px",
    height: "800px",
    top: "0px",
    left: "0px",
  };

  // FIXME: Use queried data instead of constants
  const dummyVertices: Vertex[] = [
    {
      id: "package1",
      pname: "package1",
    },
    {
      id: "package2",
      pname: "package2",
    },
  ];

  const dummyEdges: Edge[] = [
    {
      source: "package1",
      target: "package2",
      label: "dependsOn",
    },
  ];

  if (props.mode === Mode.Graph) {
    return (
      <Graph
        vertices={dummyVertices} // FIXME - use real data
        edges={dummyEdges} // FIXME - use real data
        style={graphStyle}
        selectedNodeId={props.selectedNodeId}
        onNodeClick={props.onNodeClick}
        onBackgroundClick={props.onBackgroundClick}
      />
    );
  } else if (props.mode === Mode.Table) {
    /* FIXME: implement this with material table
      e.g.
       <MaterialTable
              columns={[
                { title: "ID", field: "id" },
                { title: "Package Name", field: "pname" },
              ]}
              data={dummyVertices}
              title="Demo Title"
              onRowClick={rowClickHandler}
            />
    */
    return <div>Table goes here</div>;
  } else if (props.mode === Mode.RawText) {
    const textValue = match(query.data)
      .with({ httpResponseCode: P.number, message: P.string }, () => "")
      .otherwise((x) => x.raw);
    return (
      <textarea
        name="rawResults"
        defaultValue={textValue}
        contentEditable={false}
        rows={4}
        cols={40}
      />
    );
  }
};

interface ResultsToolProps {
  gremlinQuery: string;
}

export const ResultsTool = (props: ResultsToolProps) => {
  const [mode, setMode] = useState(Mode.Graph);

  const [selectedNodeId, setSelectedNodeId] = React.useState<NodeId>("foo");

  // FIXME: We might want to use some of these handlers for the graph and table results views
  // function nodeClickHandler(eventObject: EventObjectNode): void {
  //   setSelectedNodeId(eventObject.target.id());
  // }
  // function backgroundClickHandler(eventObject: EventObjectCore): void {
  //   setSelectedNodeId(null);
  // }
  // function rowClickHandler(
  //   event?: React.MouseEvent<Element, MouseEvent>,
  //   rowData?: ResultsRow,
  //   toggleDetailPanel?: (panelIndex?: number) => void
  // ): void {
  //   console.log(rowData);
  //   if (rowData !== null) {
  //     setSelectedNodeId(rowData.id);
  //   }
  // }

  return (
    <Container className="Results-tool-container" maxWidth="lg">
      <ToggleButtonGroup
        className="Results-mode-toggle"
        orientation="horizontal"
        value={Mode.Graph}
        exclusive
        onChange={(_event, newValue) =>
          newValue === mode ? null : setMode(newValue)
        }
        aria-label="text alignment"
      >
        <ToggleButton value={Mode.Graph} aria-label="graph">
          Graph
        </ToggleButton>
        <ToggleButton value={Mode.Table} aria-label="table">
          TABLE
        </ToggleButton>
        <ToggleButton value={Mode.RawText} aria-label="raw text">
          RAW
        </ToggleButton>
      </ToggleButtonGroup>
      <ResultsArea
        mode={mode}
        gremlinQuery={props.gremlinQuery}
        selectedNodeId={selectedNodeId}
        onNodeClick={() => null}
        onBackgroundClick={() => null}
      />
    </Container>
  );
};
