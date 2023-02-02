import "./App.css";
import MaterialTable, { Column } from "@material-table/core";
import RowData from "@material-table/core";
import React from "react";
import Graph, { Edge, NodeId, Vertex } from "./Graph";
import { EventObject, EventObjectCore, EventObjectNode } from "cytoscape";

// const lookup = { true: "Available", false: "Unavailable" };
// const columns = [
//   { title: "First Name", field: "firstName" },
//   { title: "Last Name", field: "lastName" },
//   { title: "Birth Year", field: "birthYear", type: "numeric" },
//   { title: "Availablity", field: "availability", lookup },
// ];
// const data = [
//   { firstName: "Tod", lastName: "Miles", birthYear: 1987, availability: true },
//   {
//     firstName: "Jess",
//     lastName: "Smith",
//     birthYear: 2000,
//     availability: false,
//   },
//   {
//     firstName: "Jess",
//     lastName: "Smith",
//     birthYear: 2000,
//     availability: false,
//   },
// ];

// <header className="App-header">
// <img src={logo} className="App-logo" alt="logo" />
// <p>
//   Edit <code>src/App.js</code> and save to reload.
// </p>

// </header>

type MyRow = {
  name?: string;
  surname?: string;
  birthYear?: number;
  birthCity?: number;
};

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

type ResultsRow = Vertex;

const data: MyRow[] = [
  { name: "Mehmet", surname: "Baran", birthYear: 1987, birthCity: 63 },
];

export const App = () => {
  // FIXME: Set initial states with non-hard coded values
  const [selectedNode, setSelectedNode] = React.useState<NodeId>(null);
  const [vertices, setVertices] = React.useState<Vertex[]>(dummyVertices);
  const [edges, setEdges] = React.useState<Edge[]>(dummyEdges);

  function nodeClickHandler(eventObject: EventObjectNode): void {
    setSelectedNode(eventObject.target.id());
  }
  function backgroundClickHandler(eventObject: EventObjectCore): void {
    setSelectedNode(null);
  }

  function rowClickHandler(
    event?: React.MouseEvent<Element, MouseEvent>,
    rowData?: ResultsRow,
    toggleDetailPanel?: (panelIndex?: number) => void
  ): void {
    console.log(rowData);
    if (rowData !== null) {
      setSelectedNode(rowData.id);
    }
  }

  const graphStyle = {
    width: "800px",
    height: "800px",
    border: "5px solid black",
    top: "0px",
    left: "0px",
  };
  return (
    <div className="App">
      <Graph
        vertices={vertices}
        edges={edges}
        style={graphStyle}
        selectedNodeId={selectedNode}
        onNodeClick={nodeClickHandler}
        onBackgroundClick={backgroundClickHandler}
      />
      <MaterialTable
        columns={[
          { title: "ID", field: "id" },
          { title: "Package Name", field: "pname" },
        ]}
        data={dummyVertices}
        title="Demo Title"
        onRowClick={rowClickHandler}
      />
    </div>
  );
};

export default App;

/**
 *
 * TODO:
 *
 * - Add Query text box component
 * - Add Results table component
 * - Shared state between components
 * - Data fetching
 */
