const initialStatsData = JSON.parse(
    document.getElementById("stats-plot-data").innerHTML
);

Plotly.newPlot(
    "stats-graph-main",
    initialStatsData.traces,
    initialStatsData.layout,
    initialStatsData.config
);



document.body.addEventListener("htmx:afterSwap", function (event) {
    if (!event.detail.target.matches("#stat-compare-form")) {
        return;
    }

    const dataElement = document.getElementById("stats-plot-data");
    const graphElement = document.getElementById("stats-graph-main");

    if (!dataElement || !graphElement) {
        return;
    }

    const statsData = JSON.parse(dataElement.textContent);

    Plotly.react(
        graphElement,
        statsData.traces,
        statsData.layout,
        statsData.config,
    )
    // NOTE: If react is overkill and we only change traces we can just use the fns below
    // Plotly.deleteTraces(
    //     graphElement,
    //     Array.from(Array(graphElement.data.length).keys())
    // );
    // Plotly.addTraces(
    //     graphElement,
    //     statsData.traces,
    //     Array.from(Array(statsData.traces.length).keys())
    // );
});
