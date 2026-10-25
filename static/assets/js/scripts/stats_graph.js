var plotData = JSON.parse(document.getElementById("stats-plot-data").innerHTML);

var statsData = plotData.stats_data;
var darkMode = plotData.dark_mode === "1";

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
  xaxis: {
    autorange: true,
    rangeselector: rangeselector,
  },
  yaxis: {
    autorange: true,
    type: "linear",
  },
  height: 180,
  margin: {
    l: 60,
    r: 60,
    t: 50,
    b: 50,
  },

  autosize: true,
};

function getFontColor(darkMode) {
  return darkMode ? "hsl(0, 0%, 73%)" : "hsl(0, 0%, 30%)";
}

function createPerfGraph() {
  const perfGraphDiv = document.getElementById("stats-performance-graph");
  const perfGraphTitle = perfGraphDiv.textContent;
  perfGraphDiv.textContent = "";

  const fontColor = getFontColor(darkMode);

  var trace1 = {
    x: statsData.dates,
    y: statsData.performance,
    type: "bar",
    text: statsData.performance.map(String),
    textposition: "auto",
    hoverinfo: "x+y",
    marker: {
      color: "darkcyan",
      // opacity: 0.6,
      line: {
        color: fontColor,
        width: 1.5,
      },
    },
  };

  var data = [trace1];

  const layout_copy = JSON.parse(JSON.stringify(layout));
  if (!layout_copy.title) {
    layout_copy.title = {};
    if (!layout_copy.title.font) {
      layout_copy.title.font = {};
    }
  }

  layout_copy.title.text = perfGraphTitle;
  layout_copy.title.font.color = fontColor;

  layout_copy.barmode = "stack";

  // const bgColor = darkMode ? "hsl(37, 10%, 8%)" : "hsl(37, 10%, 92%)";
  const bgColor = darkMode ? "hsl(37, 7%, 14%)" : "hsl(0, 0%, 100%)";
  layout_copy.plot_bgcolor = bgColor;
  layout_copy.paper_bgcolor = bgColor;

  layout_copy.yaxis.showticklabels = false;

  //adjust layout margin if we are on small screens
  let showLastNMonths = 3;
  const screenWidth = window.innerWidth;
  if (screenWidth < 450) {
    layout_copy.margin.l = 10;
    layout_copy.margin.r = 10;
    layout_copy.xaxis.rangeselector = rangeselector_small;
    showLastNMonths = 2;
  } else if (screenWidth < 600) {
    layout_copy.margin.l = 20;
    layout_copy.margin.r = 20;
  }

  // calculate the default date range based on last available result and N
  // months before (depending on screen size)
  const mostRecentEventDate = statsData.dates[statsData.dates.length - 1];
  const latest = new Date(mostRecentEventDate);
  // NOTE: here we add 3 days to the latest date just so that the visual bar of the
  // latest data can fit into the graph and not be cutoff to the right
  latest.setDate(latest.getDate() + 3);
  const nMonthsAgo = new Date(mostRecentEventDate);
  nMonthsAgo.setMonth(latest.getMonth() - showLastNMonths);

  layout_copy.xaxis.autorange = false;
  layout_copy.xaxis.range = [nMonthsAgo, latest];

  if (!layout_copy.xaxis.tickfont) {
    layout_copy.xaxis.tickfont = {};
  }
  layout_copy.xaxis.tickfont.color = fontColor;

  if (!layout_copy.hoverlabel) {
    layout_copy.hoverlabel = {};
    if (!layout_copy.hoverlabel.font) {
      layout_copy.hoverlabel.font = {};
    }
  }
  layout_copy.hoverlabel.font.color = fontColor;

  const config_copy = JSON.parse(JSON.stringify(config));

  Plotly.newPlot(perfGraphDiv, data, layout_copy, config_copy);
}

function createStatsGraphs() {
  createPerfGraph();
}

createStatsGraphs();
