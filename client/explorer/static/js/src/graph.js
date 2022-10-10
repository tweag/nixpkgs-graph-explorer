  // Select the node that will be observed for mutations
const targetNode = document.getElementById('query-result');

// Options for the observer (which mutations to observe)
const config = { attributes: true, childList: true, subtree: true };

// Callback function to execute when mutations are observed
const callback = (mutationList, observer) => {
  let graphData = JSON.parse($('#data-graph').attr('data-graph'))['graph-data'];
  let tableData = JSON.parse($('#data-graph').attr('data-graph'))['table-data'];
  let cy = cytoscape({
    container: document.getElementById('cy'),
    layout: {name: 'breadthfirst'},
    elements: graphData['elements'],
  });
  var table = new Tabulator('#table-graph', {
      columns:[
      {title:'Name', field:'name', sorter: 'number'},
      {title:'Neighbours', field:'neighbours', sorter:'number', hozAlign:'left'},
      ],
      data:tableData,
  });
};

// Create an observer instance linked to the callback function
const observer = new MutationObserver(callback);

// Start observing the target node for configured mutations
observer.observe(targetNode, config);

// Later, you can stop observing
// observer.disconnect();

