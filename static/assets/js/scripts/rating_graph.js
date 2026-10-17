const rating_data = JSON.parse(
    document.getElementById("rating-plot-data").innerHTML
);

Plotly.newPlot(
    "rating-graph",
    rating_data.traces,
    rating_data.layout,
    rating_data.config
);
