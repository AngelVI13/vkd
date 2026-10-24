const rating_data = JSON.parse(
  document.getElementById("rating-plot-data").innerHTML,
);

//adjust layout margin if we are on small screens
const screenWidth = window.innerWidth;
if (screenWidth < 450) {
  rating_data.layout.margin.l = 30;
  rating_data.layout.margin.r = 30;
}

let lastDate = null;
for (const trace of rating_data.traces) {
  const traceDates = trace.x.map((dateStr) => new Date(dateStr));
  const traceLastDate = traceDates[traceDates.length - 1];
  if (!lastDate || traceLastDate > lastDate) {
    lastDate = traceLastDate;
  }
}

if (lastDate) {
  // set the x axis range to be 3 months before the last date to the last date
  const threeMonthsBeforeLastDate = new Date(lastDate);
  threeMonthsBeforeLastDate.setMonth(lastDate.getMonth() - 3);
  rating_data.layout.xaxis.range = [threeMonthsBeforeLastDate, lastDate];
}

Plotly.newPlot(
  "rating-graph",
  rating_data.traces,
  rating_data.layout,
  rating_data.config,
);
