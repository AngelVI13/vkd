const data = JSON.parse(
    document.getElementById("stats-plot-data").innerHTML
);

Plotly.newPlot(
    "stats-graph-main",
    data.traces,
    data.layout,
    data.config
);
