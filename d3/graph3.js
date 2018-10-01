var t = 0,
ticksx = 6,
duration = 2000;
// parse the date
var parseDate = d3.timeParse("%Y-%m-%d");


function refilter() {

  var margin = {top: 30, right: 30, bottom: 30, left: 30},
      width = 1000 - margin.left - margin.right,
      height = 500 - margin.top - margin.bottom;

  var x = d3.scaleTime()
     .domain([new Date(1996, 0, 1), new Date(1996, ticksx, 1)])
     .range([0, width]);

  var y = d3.scaleLinear()
    .domain([1000,2300])
    .range([height, 0]);

  var xAxis = d3.axisBottom()
    .scale(x)
    .tickFormat(d3.timeFormat("%b %Y"))
    .tickSize(6);

  var yAxis = d3.axisLeft()
     .scale(y)
     .ticks(5);

  var svg = d3.select("body").append("svg")
     .attr("width", width + margin.left)
     .attr("height", height + margin.top + margin.bottom)
   .append("g")
     .attr("transform",
           "translate(" + margin.left + "," + margin.top + ")");


  var chartGroup = d3.select("body").append("svg")
     .attr("width", width - 200)
     .attr("height", height + margin.top)
     .attr("transform", "translate(-"+ (width) +",0)")
     .append("g");

  // extra svg to clip the graph and x axis as they transition in and out
  var graph = svg.append("svg")
    .attr("width", width)
    .attr("height", height + margin.top);

  var xaxis = svg.append("g")
    .attr("class", "x axis")
    .attr("transform", "translate(0," + (height) + ")")
    .call(xAxis);

  var yaxis = svg.append("g")
      .attr("class", "y axis")
      .attr("transform", "translate("+(width)+",0)")
      .call(yAxis);

  var xTickLine = graph.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0,0)")
        .call(d3.axisBottom()
          .scale(x)
          .tickSize(height)
          .tickFormat(""));

  // define the line
  var valueline = d3.line()
      .x(function(d) { return x(d.Date); })
      .y(function(d) { return y(d.rating); })
      .curve(d3.curveStepAfter);

  var color = d3.scaleOrdinal(d3.schemeCategory10);

  //////////////////////////////////////////////////////////////////////////////

  d3.csv("fightsEloLong2.csv", function(error, data) {
    if (error) throw error;

    // format the data
    data.forEach(function(d) {
        d.Link = d.Link.replace(/\d|-/g, " ").trim(),
        d.Date = parseDate(d.Date);
        d.rating = +d.rating;
    });
    // Nest the entries by
    var dataNest = d3.nest()
        .key(function(d) {return d.Link;})
        .entries(data);

    // Plot points and lines
    dataNest.forEach(function(d,i) {
        chartGroup.append("path")
            .attr("class", "line")
            .style("stroke", function() { // Add the colours dynamically
                return d.color = color(d.key); })
            .attr("id", 'tag'+d.key.replace(/\s+/g, '')) // assign ID
            .attr("d", valueline(d.values))
            .attr("width",50);

        chartGroup.selectAll("dot")
            .data(d.values)
            .enter().append("circle")
            .attr("r", 2.5)
            .attr("cx", function(d) { return x(d.Date); })
            .attr("cy", function(d) { return y(d.rating); })
            .style("fill", function() { // Add the colours dynamically
                return d.color = color(d.key); })
            .style("stroke", "black")
            .style("stroke-width", "1");

        graph.append("text")
            .attr("transform", "translate(" +
                0 + "," + y(d.values[d.values.length-1].rating) + ")")
            .attr("dy", ".35em")
            .attr("text-anchor", "start")
            .style("fill", function() { // Add the colours dynamically
                return d.color = color(d.key); })
            .text(d.values[d.values.length-1].Link + ", " +
                  d.values[d.values.length-1].rating);

                  console.log(t, d);

        function tick() {
          t++;



          var curDx = x.domain();
          var newDx = [curDx[0].setMonth(curDx[0].getMonth() + 1),
                      curDx[1].setMonth(curDx[1].getMonth() + 1)]

          var curminRate = d3.min(data.filter(function(d) {
              return d.Date >= curDx[0] & d.Date <= curDx[1] }),
              function(d) {return d.rating});
          var curmaxRate = d3.max(data.filter(function(d) {
              return d.Date >= curDx[0] & d.Date <= curDx[1] }),
              function(d) {return d.rating});

          var curDy = y.domain();
          var newDy = [curminRate, curmaxRate];

          x.domain(newDx);

          chartGroup.transition()
              .attr("transform", "translate("+ -width/ticksx  * t +")")
              .duration(duration)
              .ease(d3.easeLinear);

          xTickLine.transition()
              .duration(duration)
              .ease(d3.easeLinear)
              .call(d3.axisBottom()
                .scale(x)
                .tickSize(height)
                .tickFormat(""));

          xaxis.transition()
              .duration(duration)
              .ease(d3.easeLinear)
              .call(xAxis);

          yaxis.transition()
              .duration(duration)
              .ease(d3.easeLinear)
              .call(yAxis)
              .on("end", tick);
        }
        tick();
    });

  });
  //////////////////////////////////////////////////////////////////////////////
}
refilter();
