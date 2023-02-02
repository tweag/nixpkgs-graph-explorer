import React, { Component } from "react";
import cytoscape, {
  Core,
  ElementDefinition,
  EventHandler,
  EventObject,
  EventObjectCore,
  NodeDataDefinition,
  EventObjectNode,
  LayoutOptions,
  Stylesheet,
  StylesheetStyle,
  EdgeDataDefinition,
} from "cytoscape";
import { CytoscapeOptions } from "cytoscape";
import { textChangeRangeIsUnchanged } from "typescript";
// import 'cytoscape-elk' // TODO: need types for this

export type NodeEventHandler = (
  event: EventObjectNode,
  extraParams?: any
) => void;
export type CoreEventHandler = (
  event: EventObjectCore,
  extraParams?: any
) => void;

export type Vertex = {
  id: NodeId;
  pname: string;
};

export type Edge = {
  source: NodeId;
  target: NodeId;
  label?: string;
};

interface GraphProps {
  vertices: Vertex[];
  edges: Edge[];
  // The Component's styling
  style: React.CSSProperties;
  // Optional current node
  selectedNodeId?: NodeId;
  // Callback which will be invoked when nodes are clicked
  onNodeClick: NodeEventHandler;
  // Callback which will be invoked when background is clicked
  onBackgroundClick: CoreEventHandler;
}

export type NodeId = string | null;

function isEventObjectCore(
  eventObject: EventObject
): eventObject is EventObjectCore {
  return (eventObject as EventObjectCore) !== undefined;
}

function isEventObjectNode(
  eventObject: EventObject
): eventObject is EventObjectNode {
  return (eventObject as EventObjectNode) !== undefined;
}

function runNodeEventHandler(handler: NodeEventHandler): EventHandler {
  return (eventObject: EventObject, extraParams?: any) => {
    if (!isEventObjectNode(eventObject)) {
      console.error(
        "Attempted to invoke a node event handler on an event which was not a node event."
      );
      return;
    }
    return handler(eventObject as EventObjectNode);
  };
}

function runCoreEventHandler(
  handler: CoreEventHandler,
  cyCore: Core
): EventHandler {
  return (eventObject: EventObject, extraParams?: any) => {
    if (!isEventObjectCore(eventObject)) {
      console.error(
        "Attempted to invoke a core event handler on an event which was not a core event."
      );
      return;
    }
    const coreEvent = eventObject as EventObjectCore;
    if (coreEvent.target !== cyCore) {
      return;
    }
    return handler(coreEvent);
  };
}
export default class Graph extends Component<GraphProps> {
  private containerRef: React.RefObject<HTMLDivElement>;
  private cy: cytoscape.Core;

  constructor(props: GraphProps) {
    super(props);
    this.containerRef = React.createRef();
  }

  componentDidMount(): void {
    const style: Stylesheet[] = [
      {
        selector: "container",
        style: {
          height: "auto",
          width: "auto",
        },
      },
      {
        selector: "node",
        style: {
          "background-color": "#777",
          label: "data(id)",
        },
      },
      {
        selector: "edge",
        style: {
          width: 3,
          "line-color": "#ccc",
          "target-arrow-color": "#ccc",
          "target-arrow-shape": "triangle",
          "curve-style": "bezier",
        },
      },
    ];
    // FIXME: Query this data
    const vertexElements: ElementDefinition[] = this.props.vertices.map((v) => {
      return { data: { ...v } as NodeDataDefinition };
    });
    const edgeElements: ElementDefinition[] = this.props.edges.map((e) => {
      return { data: { ...e } as EdgeDataDefinition };
    });
    const elements = vertexElements.concat(edgeElements);
    // FIXME: Enable ELK layout cytoscape extension
    // const layout: LayoutOptions = {
    //   name: 'elk',
    //   elk: {
    //     algorithm: 'layered'
    //   },
    // }
    var options: CytoscapeOptions = {
      container: this.containerRef.current,
      style: style,
      elements: elements,
    };
    this.cy = cytoscape(options);

    // Assign event handlers
    this.cy.on("tap", "node", runNodeEventHandler(this.props.onNodeClick));
    this.cy.on(
      "tap",
      runCoreEventHandler(this.props.onBackgroundClick, this.cy)
    );

    // Configure graph based on current properties
    this.handleSelection(null, this.props.selectedNodeId);
  }

  componentDidUpdate(
    prevProps: Readonly<GraphProps>,
    prevState: Readonly<{}>,
    snapshot?: any
  ): void {
    // Configure graph based on current properties
    this.handleSelection(prevProps.selectedNodeId, this.props.selectedNodeId);
  }

  private handleSelection(previousNodeId: NodeId, nodeId: NodeId): void {
    if (nodeId !== null) {
      if (previousNodeId !== null) {
        this.cy.nodes(`#${previousNodeId}`).unselect().removeStyle();
      }
      const selectedNode = this.cy.nodes(`#${this.props.selectedNodeId}`);
      // Select the current "selected node"
      selectedNode.select();
      // Style edges
      selectedNode
        .connectedEdges()
        .style("line-color", "red")
        .style("target-arrow-color", "red");
      // Traverse to neighboring nodes and style them
      this.cy.nodes(":unselected").removeStyle();
      selectedNode
        .connectedEdges()
        .connectedNodes()
        .style("background-color", "red");
      // Style it
      this.cy.nodes(":selected").style("background-color", "blue");
    } else {
      this.cy.edges().unselect();
      this.cy.nodes().unselect();
      this.cy.edges().removeStyle();
      this.cy.nodes().removeStyle();
    }
  }

  componentWillUnmount(): void {
    this.cy.destroy();
  }

  render(): JSX.Element {
    const { style } = this.props;
    return <div id="cy" ref={this.containerRef} style={style}></div>;
  }
}
