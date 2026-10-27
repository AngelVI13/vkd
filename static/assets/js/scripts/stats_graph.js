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

function formatTime(seconds) {
  const minutes = Math.floor(seconds / 60);
  const remainingSeconds = seconds % 60;

  return `${minutes}:${String(remainingSeconds).padStart(2, "0")}`;
}

function getGraphTitle(elementId) {
  return document.getElementById(elementId)?.textContent.trim() || "";
}

const statsGraphDefinitions = [
  {
    elementId: "stats-performance-graph",
    title: getGraphTitle("stats-performance-graph"),
    description: "very long long long description",
    values: statsData.performance,
    type: "bar",
    color: "darkcyan",
    textposition: "auto",
  },
  {
    elementId: "stats-mistake-num-graph",
    title: getGraphTitle("stats-mistake-num-graph"),
    description: "very long long long description",
    values: statsData.mistake_num,
    type: "bar",
    color: "indianred",
    textposition: "outside",
  },
  {
    elementId: "stats-mistake-time-graph",
    title: getGraphTitle("stats-mistake-time-graph"),
    description: "very long long long description",
    values: statsData.mistake_time,
    type: "bar",
    color: "indianred",
    formatter: formatTime,
    textposition: "outside",
    yaxisTicks: {
      formatter: formatTime,
      includeZero: false,
    },
  },
  {
    elementId: "stats-tilt-rate-graph",
    title: getGraphTitle("stats-tilt-rate-graph"),
    description:
      "The tendency to make a mistake right after making a mistake. " +
      "For example, you made a mistake at control 3 and then at control 4, " +
      "but managed to find control 5 without mistakes. In this case your " +
      "Error-Caused Instability would be 50%.",
    values: statsData.tilt_rate,
    type: "bar",
    color: "chocolate",
    textposition: "auto",
  },
  //

  // Example scatter graph:
  // {
  //   elementId: "stats-example-scatter-graph",
  //   title: getGraphTitle(
  //     "stats-example-scatter-graph",
  //     "Example Scatter",
  //   ),
  //   values: statsData.example_values,
  //   type: "scatter",
  //   mode: "lines+markers",
  //   color: "royalblue",
  // },
];

function getResponsiveGraphSettings(width) {
  if (width < smallScreenThreshold) {
    return {
      plotHeight: 110,
      titleHeight: 24,
      subplotGap: 10,
      titleFontSize: 13,
      showYAxisTickLabels: false,
      leftMargin: 10,
      rightMargin: 10,
      descriptionMargin: 20,
    };
  }

  if (width < midScreenThreshold) {
    return {
      plotHeight: 125,
      titleHeight: 26,
      subplotGap: 12,
      titleFontSize: 14,
      showYAxisTickLabels: true,
      leftMargin: 40,
      rightMargin: 20,
      descriptionMargin: 30,
    };
  }

  return {
    plotHeight: 145,
    titleHeight: 28,
    subplotGap: 14,
    titleFontSize: 16,
    showYAxisTickLabels: true,
    leftMargin: 60,
    rightMargin: 60,
    descriptionMargin: 60,
  };
}

function getNiceTickStep(range, tickCount) {
  const roughStep = range / Math.max(1, tickCount - 1);
  const magnitude = 10 ** Math.floor(Math.log10(roughStep));
  const fraction = roughStep / magnitude;

  const niceFraction =
    fraction <= 1 ? 1 : fraction <= 2 ? 2 : fraction <= 5 ? 5 : 10;

  return niceFraction * magnitude;
}

function createTickValues(values, tickCount, includeZero) {
  const numericValues = values.filter(Number.isFinite);

  if (numericValues.length === 0) {
    return [];
  }

  let minimum = Math.min(...numericValues);
  let maximum = Math.max(...numericValues);

  if (includeZero) {
    minimum = Math.min(minimum, 0);
    maximum = Math.max(maximum, 0);
  }

  if (minimum === maximum) {
    return [minimum];
  }

  const step = getNiceTickStep(maximum - minimum, tickCount);
  const firstTick = Math.ceil(minimum / step) * step;
  const lastTick = Math.floor(maximum / step) * step;
  const ticks = [];

  for (let value = firstTick; value <= lastTick + step * 1e-9; value += step) {
    // Avoid floating-point values such as 0.30000000000000004.
    ticks.push(Number(value.toPrecision(12)));
  }

  return ticks;
}

function getYAxisTickOptions(graph) {
  const settings = graph.yaxisTicks;

  if (!settings?.formatter) {
    return {};
  }

  const configuredValues =
    typeof settings.values === "function"
      ? settings.values(graph.values, graph)
      : settings.values;

  const tickValues =
    configuredValues ??
    createTickValues(
      graph.values,
      settings.count ?? 5,
      settings.includeZero ?? graph.type === "bar",
    );

  return {
    tickmode: "array",
    tickvals: tickValues,
    ticktext: tickValues.map((value, index) =>
      String(settings.formatter(value, index, tickValues)),
    ),
  };
}

function getYAxisId(index) {
  return index === 0 ? "y" : `y${index + 1}`;
}

function getYAxisLayoutName(index) {
  return index === 0 ? "yaxis" : `yaxis${index + 1}`;
}

function createTrace(graph, index) {
  const formatter = graph.formatter || String;
  const formattedValues = graph.values.map(formatter);

  const trace = {
    name: graph.title,
    x: statsData.dates,
    y: graph.values,
    xaxis: "x",
    yaxis: getYAxisId(index),
    type: graph.type,
    customdata: formattedValues,
    hovertemplate: `${graph.title}: %{customdata}<extra></extra>`,
    text: formattedValues,
    textfont: {
      color: fontColor,
    },
    marker: {
      color: graph.color,
      line: {
        color: fontColor,
        width: 1.5,
      },
    },
  };

  if (graph.type === "bar") {
    trace.textposition = graph.textposition || "outside";
  }

  if (graph.type === "scatter") {
    trace.mode = graph.mode || "lines+markers";
    trace.line = {
      color: graph.color,
    };
  }

  return {
    ...trace,
    ...graph.traceOptions,
    marker: {
      ...trace.marker,
      ...graph.traceOptions?.marker,
    },
    line: {
      ...trace.line,
      ...graph.traceOptions?.line,
    },
  };
}

function createStatsDescriptionPanel(graphDiv) {
  const panelId = `${graphDiv.id}-descriptions`;
  document.getElementById(panelId)?.remove();

  const graphsWithDescriptions = statsGraphDefinitions.filter((graph) =>
    graph.description?.trim(),
  );

  if (graphsWithDescriptions.length === 0) {
    return;
  }
  const width = window.innerWidth;
  const settings = getResponsiveGraphSettings(width);

  const details = document.createElement("details");
  details.id = panelId;
  details.className = "stats-descriptions";

  const summary = document.createElement("summary");
  summary.textContent = "About these statistics";
  details.appendChild(summary);

  const content = document.createElement("div");
  content.className = "stats-descriptions__content";
  content.style.marginLeft = `${settings.descriptionMargin}px`;
  content.style.marginRight = `${settings.descriptionMargin}px`;

  graphsWithDescriptions.forEach((graph) => {
    const section = document.createElement("section");

    const heading = document.createElement("h3");
    heading.textContent = graph.title;

    const description = document.createElement("p");
    description.textContent = graph.description;

    section.append(heading, description);
    content.appendChild(section);
  });

  details.appendChild(content);
  graphDiv.insertAdjacentElement("afterend", details);
}

function createStatsGraphs() {
  const graphCount = statsGraphDefinitions.length;

  if (graphCount === 0) {
    return;
  }

  const width = window.innerWidth;
  const settings = getResponsiveGraphSettings(width);
  const graphDivs = statsGraphDefinitions.map(({ elementId }) =>
    document.getElementById(elementId),
  );
  const graphDiv = graphDivs[0];

  graphDiv.textContent = "";

  graphDivs.slice(1).forEach((div) => {
    div.hidden = true;
  });

  /*
   * Domains use the drawable paper height rather than hardcoded percentages.
   * Each subplot gets a fixed pixel height, making the resulting normalized
   * domains independent of the number of graphs.
   */
  const paperHeight =
    graphCount * (settings.titleHeight + settings.plotHeight) +
    (graphCount - 1) * settings.subplotGap;

  const marginTop = 55;
  const marginBottom = 35;

  const combinedLayout = JSON.parse(JSON.stringify(layout));
  delete combinedLayout.title;

  combinedLayout.height = marginTop + paperHeight + marginBottom;
  combinedLayout.hovermode = "x unified";
  combinedLayout.hoversubplots = "axis";
  combinedLayout.showlegend = false;

  combinedLayout.margin = {
    l: settings.leftMargin,
    r: settings.rightMargin,
    t: marginTop,
    b: marginBottom,
  };

  combinedLayout.xaxis = {
    ...combinedLayout.xaxis,
    anchor: getYAxisId(graphCount - 1),
    rangeselector: {
      ...(width < smallScreenThreshold ? rangeselector_small : rangeselector),
      // Keep the selector a consistent number of pixels above the paper.
      y: 1 + 8 / paperHeight,
    },
  };

  const annotations = [];
  let rowTop = paperHeight;

  statsGraphDefinitions.forEach((graph, index) => {
    const domainTop = rowTop - settings.titleHeight;
    const domainBottom = domainTop - settings.plotHeight;
    const yaxisName = getYAxisLayoutName(index);

    combinedLayout[yaxisName] = {
      autorange: true,
      type: "linear",
      showticklabels:
        graph.yaxisOptions?.showticklabels ?? settings.showYAxisTickLabels,
      tickfont: {
        color: fontColor,
      },
      gridcolor: darkMode ? "hsl(0, 0%, 25%)" : "hsl(0, 0%, 85%)",
      ...graph.yaxisOptions,
      ...getYAxisTickOptions(graph),
      domain: [domainBottom / paperHeight, domainTop / paperHeight],
      anchor: "x",
    };

    annotations.push({
      text: graph.title,
      x: 0.5,
      xref: "paper",
      y: rowTop / paperHeight,
      yref: "paper",
      xanchor: "center",
      yanchor: "top",
      showarrow: false,
      font: {
        color: fontColor,
        size: settings.titleFontSize,
      },
    });

    rowTop = domainBottom - settings.subplotGap;
  });

  combinedLayout.annotations = annotations;

  const data = statsGraphDefinitions.map(createTrace);

  Plotly.newPlot(graphDiv, data, combinedLayout, config);
  createStatsDescriptionPanel(graphDiv);
}

createStatsGraphs();
