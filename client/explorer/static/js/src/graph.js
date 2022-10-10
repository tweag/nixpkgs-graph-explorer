  // Select the node that will be observed for mutations
const targetNode = document.getElementById('query-result');

// Options for the observer (which mutations to observe)
const config = { attributes: true, childList: true, subtree: true };

// Callback function to execute when mutations are observed
const callback = (mutationList, observer) => {
  let graphData = JSON.parse($("#data-graph").attr("data-graph"))["graph-data"];
  let tableData = JSON.parse($("#data-graph").attr("data-graph"))["table-data"];
  let cy = cytoscape({
    container: document.getElementById('cy'),
    layout: {name: 'breadthfirst'},
    elements: graphData
  });
  // let tableData = [
  //   {id:1, name:"Billy Bob", age:"12", gender:"male", height:1, col:"red", dob:"", cheese:1},
  //   {id:2, name:"Mary May", age:"1", gender:"female", height:2, col:"blue", dob:"14/05/1982", cheese:true},
  //   {id:3, name:"Christine Lobowski", age:"42", height:0, col:"green", dob:"22/05/1982", cheese:"true"},
  //   {id:4, name:"Brendon Philips", age:"125", gender:"male", height:1, col:"orange", dob:"01/08/1980"},
  //   {id:5, name:"Margret Marmajuke", age:"16", gender:"female", height:5, col:"yellow", dob:"31/01/1999"},
  //   {id:6, name:"Billy Bob", age:"12", gender:"male", height:1, col:"red", dob:"", cheese:1},
  //   {id:7, name:"Mary May", age:"1", gender:"female", height:2, col:"blue", dob:"14/05/1982", cheese:true},
  //   {id:8, name:"Christine Lobowski", age:"42", height:0, col:"green", dob:"22/05/1982", cheese:"true"},
  //   {id:9, name:"Brendon Philips", age:"125", gender:"male", height:1, col:"orange", dob:"01/08/1980"},
  //   {id:10, name:"Margret Marmajuke", age:"16", gender:"female", height:5, col:"yellow", dob:"31/01/1999"},
  // ]
  var table = new Tabulator("#table-graph", {
      height:"311px",
      columns:[
      {title:"Name", field:"name"},
      {title:"Age", field:"age", sorter:"number"},
      {title:"Gender", field:"gender"},
      {title:"Height", field:"height"},
      {title:"Favourite Color", field:"col"},
      {title:"Date Of Birth", field:"dob", hozAlign:"center"},
      {title:"Cheese?", field:"cheese"},
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

