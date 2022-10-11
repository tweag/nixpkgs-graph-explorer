  // Select the node that will be observed for mutations
const targetNode = document.getElementById('query-result');

// Options for the observer (which mutations to observe)
const config = { attributes: true, childList: true, subtree: true };

// Callback function to execute when mutations are observed
const callback = (mutationList, observer) => {
  // create graph plot
  let graphData = JSON.parse($('#data-graph').attr('data-graph'))['graph-data'];
  let unselectedColour = 'gray'
  let selectedColour = 'red'
  let cy = cytoscape({
    container: document.getElementById('cy'),
    layout: {name: 'breadthfirst'},
    elements: graphData['elements'],
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

  $('#cy').addClass('outlined');
  $('#table-graph').addClass('outlined');

  function cleanUp() {
    cy.edges().unselect();
    cy.nodes().unselect();
    cy.edges().removeStyle();
    cy.nodes().removeStyle();
  };

  // select a node and style neighbouring elements
  function nodeSelected(node) {
    cleanUp();

    // select node
    node.select();

    // edges
    cy
      .edges()
      .removeStyle();
    node
      .connectedEdges()
      .style('line-color', 'red');

    // nodes
    cy
      .nodes(':unselected')
      .removeStyle();
    node
      .connectedEdges()
      .connectedNodes()
      .style('background-color', 'red')
      .style('label', function (ele) { return ele.data('id')})
      .style('text-opacity', 1);
    cy
      .nodes(':selected')
      .style('background-color', 'blue');
  };

  // when a node is selected on the graph, select the row in the table
  cy.on('tap', 'node', function(evt) {
    table.deselectRow();
    table.selectRow(evt.target.id());
  });

  cy.on('tap', 'edge', function(evt) {
    cleanUp();
  });

  // when a row is selected in the table, select it on the graph
  table.on("rowSelectionChanged", function(data, rows) {
    if (data.length > 0) {
      let node = cy.nodes(`#${ data[0].id }`);
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

