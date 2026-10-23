const statsData = JSON.parse(
    document.getElementById("stats-plot-data").innerHTML
);

const statsGraph = document.getElementById("stats-graphs");

const perfGraphDiv = document.createElement("div");
perfGraphDiv.id = "stats-performance-graph";
perfGraphDiv.classList.add("stats-graph");

statsGraph.appendChild(perfGraphDiv);

// ----------

var trace1 = {
  x: statsData.dates,
  y: statsData.performance,
  type: 'bar',
  text: statsData.performance.map(String),
  textposition: 'auto',
  hoverinfo: 'none',
  marker: {
    color: 'rgb(158,202,225)',
    opacity: 0.6,
    line: {
      color: 'rgb(8,48,107)',
      width: 1.5
    }
  }
};

var data = [trace1];

var layout = {
  title: {
    text: 'Performance vs Winner'
  },
  barmode: 'stack'
};
// ----------

Plotly.newPlot(
    perfGraphDiv,
    data,
    layout,
);

