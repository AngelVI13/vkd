function getFontColor(darkMode) {
  return darkMode ? "hsl(0, 0%, 73%)" : "hsl(0, 0%, 30%)";
}

var plotData = JSON.parse(document.getElementById("stats-plot-data").innerHTML);

var statsData = plotData.stats_data;
var darkMode = plotData.dark_mode === "1";
var fontColor = getFontColor(darkMode);
var bgColor = darkMode ? "hsl(37, 7%, 14%)" : "hsl(0, 0%, 100%)";
var sWidth = window.innerWidth;
var smallScreenThreshold = 450;
var midScreenThreshold = 600;

var config = {
  responsive: true,
  displayModeBar: false,
};

var rangeselector = {
  buttons: [
    {
      count: 3,
      label: "3m",
      step: "month",
      stepmode: "backward",
    },
    {
      count: 6,
      label: "6m",
      step: "month",
      stepmode: "backward",
    },
    {
      step: "all",
    },
  ],
  type: "date",
};

var rangeselector_small = {
  buttons: [
    {
      count: 2,
      label: "2m",
      step: "month",
      stepmode: "backward",
    },
    {
      count: 4,
      label: "4m",
      step: "month",
      stepmode: "backward",
    },
    {
      step: "all",
    },
  ],
  type: "date",
};

var layout = {
  hovermode: "x unified",
  hoverdistance: 100,
  dragmode: false,
  scrollZoom: false,
  title: {
    font: {
      color: fontColor,
    },
  },
  xaxis: {
    autorange: false,
    rangeselector:
      sWidth < smallScreenThreshold ? rangeselector_small : rangeselector,
    tickfont: {
      color: fontColor,
    },
  },
  yaxis: {
    autorange: true,
    type: "linear",
  },
  hoverlabel: {
    font: {
      color: fontColor,
    },
  },
  plot_bgcolor: bgColor,
  paper_bgcolor: bgColor,
  height: 180,
  margin: {
    l:
      sWidth < smallScreenThreshold
        ? 10
        : sWidth < midScreenThreshold
          ? 20
          : 60,
    r:
      sWidth < smallScreenThreshold
        ? 10
        : sWidth < midScreenThreshold
          ? 20
          : 60,
    t: 50,
    b: 50,
  },

  autosize: true,
};

// calculate the default date range based on last available result and N
// months before (depending on screen size)
var showLastNMonths = layout.xaxis.rangeselector.buttons[0].count;
var mostRecentEventDate = statsData.dates[statsData.dates.length - 1];
var latest = new Date(mostRecentEventDate);
// NOTE: here we add 3 days to the latest date just so that the visual bar of the
// latest data can fit into the graph and not be cutoff to the right
latest.setDate(latest.getDate() + 3);
var nMonthsAgo = new Date(mostRecentEventDate);
nMonthsAgo.setMonth(latest.getMonth() - showLastNMonths);
layout.xaxis.range = [nMonthsAgo, latest];

function createPerfGraph() {
  const graphDiv = document.getElementById("stats-performance-graph");
  const graphTitle = graphDiv.textContent;
  graphDiv.textContent = "";

  var trace1 = {
    x: statsData.dates,
    y: statsData.performance,
    type: "bar",
    text: statsData.performance.map(String),
    textposition: "auto",
    hoverinfo: "x+y",
    marker: {
      color: "darkcyan",
      line: {
        color: fontColor,
        width: 1.5,
      },
    },
  };

  var data = [trace1];

  const layout_copy = JSON.parse(JSON.stringify(layout));

  layout_copy.title.text = graphTitle;
  layout_copy.barmode = "stack";
  layout_copy.yaxis.showticklabels = false;

  const config_copy = JSON.parse(JSON.stringify(config));

  Plotly.newPlot(graphDiv, data, layout_copy, config_copy);
}

function createMistakeNumGraph() {
  const graphDiv = document.getElementById("stats-mistake-num-graph");
  const graphTitle = graphDiv.textContent;
  graphDiv.textContent = "";

  var trace1 = {
    x: statsData.dates,
    y: statsData.mistake_num,
    type: "bar",
    text: statsData.mistake_num.map(String),
    textposition: "auto",
    hoverinfo: "x+y",
    marker: {
      color: "indianred",
      line: {
        color: fontColor,
        width: 1.5,
      },
    },
  };

  var data = [trace1];

  const layout_copy = JSON.parse(JSON.stringify(layout));

  layout_copy.title.text = graphTitle;
  layout_copy.barmode = "stack";
  layout_copy.yaxis.showticklabels = false;

  const config_copy = JSON.parse(JSON.stringify(config));

  Plotly.newPlot(graphDiv, data, layout_copy, config_copy);
}

function createStatsGraphs() {
  // TODO: maybe create these as subplots or something so that we can have
  // unified hover accross all of them
  createPerfGraph();
  createMistakeNumGraph();
}

createStatsGraphs();
