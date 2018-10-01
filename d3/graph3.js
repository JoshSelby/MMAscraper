var t = 0,
ticksx = 36,
duration = 500;
// parse the date
var parseDate = d3.timeParse("%Y-%m-%d");


function refilter() {

  var margin = {top: 30, right: 30, bottom: 30, left: 30},
      width = 1000 - margin.left - margin.right,
      height = 500 - margin.top - margin.bottom;

 var x = d3.scaleTime()
     .domain([new Date(2000, 0, 1), new Date(2000, ticksx, 1)])
     .range([0, width]);

 var y = d3.scaleLinear()
    .domain([1000,2300])
    .range([height, 0]);

 var xAxis = d3.axisBottom()
    .scale(x)
    .ticks(5)
    .tickFormat(d3.timeFormat("%b-%d-%Y"));

 var yAxis = d3.axisRight()
     .scale(y)
     .ticks(5);

 var svg = d3.select("body").append("svg")
     .attr("width", width + margin.left + margin.right)
     .attr("height", height + margin.top + margin.bottom)
   .append("g")
     .attr("transform",
           "translate(" + margin.left + "," + margin.top + ")");

  // extra svg to clip the graph and x axis as they transition in and out
  var graph = svg.append("svg")
    .attr("width", width + margin.left)
    .attr("height", height + margin.top);

  var xaxis = graph.append("g")
    .attr("class", "x axis")
    .attr("transform", "translate(-10," + (height+10) + ")")
    .call(xAxis);

  var yaxis = graph.append("g")
      .attr("class", "y axis")
      .attr("transform", "translate("+(width-10)+",10)")
      .call(yAxis);

  // gridlines in x axis function
  function make_x_gridlines() {
      return d3.axisBottom(x)
          .ticks(5)
  }

  // gridlines in y axis function
  function make_y_gridlines() {
      return d3.axisLeft(y)
          .ticks(5)
  }

  // define the line
  var valueline = d3.line()
      .x(function(d) { return x(d.Date); })
      .y(function(d) { return y(d.rating); })
      .curve(d3.curveStepAfter);

  var color = d3.scaleOrdinal(d3.schemeCategory10);

  var chartGroup = svg.append("g")
      .attr("width", width)
      .attr("height", height + margin.top);




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
            .attr("width",500);

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

        svg.append("text")
            .attr("transform", "translate(" +
                (x(d.values[d.values.length-1].Date)+10) + "," +
                y(d.values[d.values.length-1].rating) + ")")
            .attr("dy", ".35em")
            .attr("text-anchor", "start")
            .style("fill", function() { // Add the colours dynamically
                return d.color = color(d.key); })
            .text(d.values[d.values.length-1].Link + ", " +
                  d.values[d.values.length-1].rating);

    });

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

      console.log(y.domain());
      shiftX = newDx[0] - curDx[1].valueOf()

      chartGroup.transition()
                .attr("transform", "translate("+ -width/ticksx  * t +",0)")
                .duration(duration)
                .ease(d3.easeLinear);

      // slide the x-axis left
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
  //////////////////////////////////////////////////////////////////////////////

}

refilter();
