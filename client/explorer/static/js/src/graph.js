
function populateGraphAndTable(data) {
  let graphData = data['graph-data'];
  let unselectedColour = 'gray'
  let selectedColour = 'red'

  // create cytoscape graph
  let cy = cytoscape({
    container: document.getElementById('cy'),
    layout: {
      name: 'breadthfirst',
    },
    elements: graphData['elements'],
    style: [ // the stylesheet for the graph
      {
        selector: 'node',
        style: {
          'background-color': '#666',
          'label': 'data(id)'
        }
      },

      {
        selector: 'edge',
        style: {
          'width': 3,
          'line-color': '#ccc',
          'target-arrow-color': '#ccc',
          'target-arrow-shape': 'triangle',
          'curve-style': 'bezier',
          'label': 'data(label)'
        }
      }
    ],
  });

  // create data table
  let tableData = data['table-data'];
  let table = new Tabulator('#table-graph', {
      selectable:1,
      columns:[
      {title:'ID', field:'id', sorter: 'string'},
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
      .style('line-color', 'red')
      .style('target-arrow-color', 'red');

    // nodes
    cy
      .nodes(':unselected')
      .removeStyle();
    node
      .connectedEdges()
      .connectedNodes()
      .style('background-color', 'red')
      // .style('label', function (ele) { return ele.data('id')})
      // .style('text-opacity', 1);
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


function sendData() {
  const XHR = new XMLHttpRequest();
  XHR.responseType = 'json';

  // Bind the FormData object and the form element
  const FD = new FormData(form);

  // Define what happens on successful data submission
  XHR.addEventListener("load", (event) => {
    data = event.target.response;
    if ('error' in data) {
      swal('Oops! Something went wrong.');
      swal({
        title: "Error",
        text: "We couldn't generate a graph from your query!",
        icon: "error",
        button: "OK",
      });
    } else {
      populateGraphAndTable(data);
    }
  });

  // Define what happens in case of error
  XHR.addEventListener("error", (event) => {
    alert('Oops! Something went wrong.');
  });

  // Set up our request
  XHR.open("POST", "/");

  // The data sent is what the user provided in the form
  XHR.send(FD);
}

// Get the form element
const form = document.getElementById("query-form");

// Add 'submit' event handler
form.addEventListener("submit", (event) => {
  event.preventDefault();

  sendData();
});

