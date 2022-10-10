  // Select the node that will be observed for mutations
const targetNode = document.getElementById('query-result');

// Options for the observer (which mutations to observe)
const config = { attributes: true, childList: true, subtree: true };

// Callback function to execute when mutations are observed
const callback = (mutationList, observer) => {
  // create graph plot
  let graphData = JSON.parse($('#data-graph').attr('data-graph'))['graph-data'];
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

  // when a node is selected on the graph, select the row in the table
  cy.on('tap', 'node', function(evt){
    let node = evt.target;
    table.deselectRow();
    table.selectRow(node.id());
  });

  // when a row is selected in the table, select it on the graph
  table.on("rowSelectionChanged", function(data, rows){
    if (data.length > 0) {
      cy.nodes().unselect();
      cy.nodes(`#${ data[0].id }`).select();
    }
  });
};

// Create an observer instance linked to the callback function
const observer = new MutationObserver(callback);

// Start observing the target node for configured mutations
observer.observe(targetNode, config);

// Later, you can stop observing
// observer.disconnect();

