var plotData = JSON.parse(document.getElementById("stats-plot-data").innerHTML);

var statsData = plotData.stats_data;
var darkMode = plotData.dark_mode === "1";

var config = {
  responsive: true,
  displayModeBar: false,
};

var layout = {
  hovermode: "x unified",
  hoverdistance: 100,
  dragmode: false,
  scrollZoom: false,
  xaxis: {
    autorange: true,
    rangeselector: {
      buttons: [
        {
          count: 6,
          label: "6m",
          step: "month",
          stepmode: "backward",
        },
        {
          count: 1,
          label: "1y",
          step: "year",
          stepmode: "backward",
        },
        {
          step: "all",
        },
      ],
      type: "date",
    },
  },
  yaxis: {
    autorange: true,
    type: "linear",
  },
  height: 250,
  margin: {
    l: 60,
    r: 60,
    t: 40,
    b: 40,
  },

  autosize: true,
};

function createPerfGraph() {
  const perfGraphDiv = document.getElementById("stats-performance-graph");
  const perfGraphTitle = perfGraphDiv.textContent;
  perfGraphDiv.textContent = "";

  var trace1 = {
    x: statsData.dates,
    y: statsData.performance,
    type: "bar",
    text: statsData.performance.map(String),
    textposition: "auto",
    hoverinfo: "none",
    marker: {
      color: "rgb(158,202,225)",
      opacity: 0.6,
      line: {
        color: "rgb(8,48,107)",
        width: 1.5,
      },
    },
  };

  var data = [trace1];

  const layout_copy = JSON.parse(JSON.stringify(layout));
  if (!layout_copy.title) {
    layout_copy.title = {};
  }
  layout_copy.title.text = perfGraphTitle;

  layout_copy.hovermode = "none";
  layout_copy.barmode = "stack";

  const bgColor = darkMode ? "hsl(37, 10%, 8%)" : "hsl(37, 10%, 92%)";
  layout_copy.plot_bgcolor = bgColor;
  layout_copy.paper_bgcolor = bgColor;

  const config_copy = JSON.parse(JSON.stringify(config));

  Plotly.newPlot(perfGraphDiv, data, layout_copy, config_copy);
}

// TODO: maybe we should just create the whole page and then set one tab content to hidden so the data is always there
function createStatsGraphs() {
  createPerfGraph();
}

createStatsGraphs();

document.body.addEventListener("htmx:afterSwap", function (event) {
  if (!event.detail.target.matches("#tab-section")) {
    return;
  }

  if (!document.getElementById("stats-graphs")) {
    return;
  }
  createStatsGraphs();
});
