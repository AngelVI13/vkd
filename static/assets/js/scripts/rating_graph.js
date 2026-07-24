const data = JSON.parse(
    document.getElementById("plot-data").innerHTML
);

Plotly.newPlot(
    "rating-graph",
    data.traces,
    data.layout,
    data.config
);
