// TODO: remove old HTML element and then recreate it
var stats_data = JSON.parse(
    document.getElementById("stats-plot-data").innerHTML
);

Plotly.newPlot(
    "stats-graph-main",
    stats_data.traces,
    stats_data.layout,
    stats_data.config
);
