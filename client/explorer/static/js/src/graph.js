  // Select the node that will be observed for mutations
const targetNode = document.getElementById('query-result');

// Options for the observer (which mutations to observe)
const config = { attributes: true, childList: true, subtree: true };

// Callback function to execute when mutations are observed
const callback = (mutationList, observer) => {
  let elements = JSON.parse($("#data-graph").attr("data-graph"))["data"];
  var cy = cytoscape({
    container: document.getElementById('cy'),
    layout: {name: 'breadthfirst'},
    elements: elements
  });
  console.log(elements);
};

// Create an observer instance linked to the callback function
const observer = new MutationObserver(callback);

// Start observing the target node for configured mutations
observer.observe(targetNode, config);

// Later, you can stop observing
// observer.disconnect();

