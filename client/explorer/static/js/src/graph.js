  // Select the node that will be observed for mutations
const targetNode = document.getElementById('query-result');

// Options for the observer (which mutations to observe)
const config = { attributes: true, childList: true, subtree: true };

// Callback function to execute when mutations are observed
const callback = (mutationList, observer) => {
  // create graph plot
  let graphData = JSON.parse($('#data-graph').attr('data-graph'))['graph-data'];
  let edgeColour = 'gray'
  let hiEdgeColour = 'red'
  let cy = cytoscape({
    container: document.getElementById('cy'),
    layout: {name: 'breadthfirst'},
    elements: graphData['elements'],
    style: [
      { selector: 'edge',
        style: {lineColor: edgeColour}
      }
    ],
  });

  // create data table
  let tableData = JSON.parse($('#data-graph').attr('data-graph'))['table-data'];
  let table = new Tabulator('#table-graph', {
      selectable:1,
      columns:[
      {title:'Name', field:'name', sorter: 'number'},
      {title:'Neighbours', field:'neighbours', sorter:'number', hozAlign:'left'},
      ],
      data:tableData,
  });


  function nodeSelected(node) {
    cy.edges().animate({
      style: {lineColor: edgeColour}
    })
    node.connectedEdges().animate({
      style: {lineColor: hiEdgeColour}
    })
  };

  // when a node is selected on the graph, select the row in the table
  cy.on('tap', 'node', function(evt) {
    table.deselectRow();
    table.selectRow(evt.target.id());
  });

  // when a row is selected in the table, select it on the graph
  table.on("rowSelectionChanged", function(data, rows) {
    if (data.length > 0) {
      cy.nodes().unselect();
      let node = cy.nodes(`#${ data[0].id }`);
      node.select();
      nodeSelected(node);
    }
  });
};

// Create an observer instance linked to the callback function
const observer = new MutationObserver(callback);

// Start observing the target node for configured mutations
observer.observe(targetNode, config);

// Later, you can stop observing
// observer.disconnect();

