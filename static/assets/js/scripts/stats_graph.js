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

function createStatsGraphs() {
  const graphDivs = [
    document.getElementById("stats-performance-graph"),
    document.getElementById("stats-mistake-num-graph"),
    document.getElementById("stats-mistake-time-graph"),
  ];

  const titles = graphDivs.map((div) => div.textContent.trim());
  const graphDiv = graphDivs[0];

  graphDiv.textContent = "";
  graphDivs.slice(1).forEach((div) => {
    div.hidden = true;
  });

  const formatTime = (seconds) => {
    const minutes = Math.floor(seconds / 60);
    const remainingSeconds = seconds % 60;
    return `${minutes}:${String(remainingSeconds).padStart(2, "0")}`;
  };

  const data = [
    {
      name: titles[0],
      x: statsData.dates,
      y: statsData.performance,
      xaxis: "x",
      yaxis: "y",
      type: "bar",
      text: statsData.performance.map(String),
      textposition: "auto",
      hovertemplate: `${titles[0]}: %{y}<extra></extra>`,
      marker: {
        color: "darkcyan",
        line: { color: fontColor, width: 1.5 },
      },
    },
    {
      name: titles[1],
      x: statsData.dates,
      y: statsData.mistake_num,
      xaxis: "x",
      yaxis: "y2",
      type: "bar",
      text: statsData.mistake_num.map(String),
      textposition: "outside",
      hovertemplate: `${titles[1]}: %{y}<extra></extra>`,
      marker: {
        color: "indianred",
        line: { color: fontColor, width: 1.5 },
      },
      textfont: { color: fontColor },
    },
    {
      name: titles[2],
      x: statsData.dates,
      y: statsData.mistake_time,
      customdata: statsData.mistake_time.map(formatTime),
      xaxis: "x",
      yaxis: "y3",
      type: "bar",
      text: statsData.mistake_time.map(formatTime),
      textposition: "outside",
      hovertemplate: `${titles[2]}: %{customdata}<extra></extra>`,
      marker: {
        color: "indianred",
        line: { color: fontColor, width: 1.5 },
      },
      textfont: { color: fontColor },
    },
  ];

  const combinedLayout = JSON.parse(JSON.stringify(layout));
  delete combinedLayout.title;

  // TODO: calculate height based on number of graphs
  combinedLayout.height = 560;
  combinedLayout.hovermode = "x unified";
  combinedLayout.hoversubplots = "axis";
  combinedLayout.showlegend = false;
  combinedLayout.margin.t = 10;
  combinedLayout.margin.b = 30;

  // All traces use this one x-axis, so the range buttons affect every subplot.
  combinedLayout.xaxis = {
    ...combinedLayout.xaxis,
    anchor: "y3",
    rangeselector: {
      ...combinedLayout.xaxis.rangeselector,
      y: 1.03,
    },
  };

  // TODO: calculate domains automatically based on number of graphs
  // TODO: on small screens don't show tick labels for y axis but on bigger screens DO
  combinedLayout.yaxis = {
    autorange: true,
    type: "linear",
    showticklabels: false,
    domain: [0.68, 0.94],
    anchor: "x",
  };

  combinedLayout.yaxis2 = {
    autorange: true,
    type: "linear",
    showticklabels: false,
    domain: [0.35, 0.61],
    anchor: "x",
  };

  combinedLayout.yaxis3 = {
    autorange: true,
    type: "linear",
    showticklabels: false,
    domain: [0.02, 0.28],
    anchor: "x",
  };

  // TODO: calculate y positions automatically based on number of graphs
  // TODO: Change title font size based on window size
  combinedLayout.annotations = titles.map((title, index) => ({
    text: title,
    x: 0.5,
    xref: "paper",
    y: [0.95, 0.62, 0.28][index],
    yref: "paper",
    yanchor: "bottom",
    showarrow: false,
    font: { color: fontColor },
  }));

  Plotly.newPlot(graphDiv, data, combinedLayout, config);
}

createStatsGraphs();
