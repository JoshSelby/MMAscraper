var t = 0,
    duration = 1000,
    ticksx = 6;

// set the dimensions and margins of the graph
var margin = {top: 50, right: 50, bottom: 50, left: 50},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

// parse the date
var parseDate = d3.timeParse("%Y-%m-%d");

// set the ranges
var x = d3.scaleTime()
    .domain([new Date(1989, 0, 1), new Date(1989, 6, 1)])
    .range([0, width]);
var y = d3.scaleLinear()
    .domain([900,1200])
    .range([height, 0]);

// define the line
var valueline = d3.line()
    .x(function(d) { return x(d.Date); })
    .y(function(d) { return y(d.rating); })
    .curve(d3.curveStepAfter);

// append the svg obgect to the body of the page
// appends a 'group' element to 'svg'
// moves the 'group' element to the top left margin
var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .style("background-color", "lightgrey")
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

var chartGroup = d3.select("body").append("svg")
   .attr("width", width)
   .attr("height", height)
   .attr("transform", "translate(-"+ (width + margin.left) +","+(-margin.top)+")")
   .style("background-color", "red")
   .style("opacity", 0.5)
   .append("g");

var xAxis = d3.axisBottom()
  .scale(x)
  .tickFormat(d3.timeFormat("%b %Y"))
  .tickSize(6);

var yAxis = d3.axisLeft()
   .scale(y)
   .ticks(5);

var xaxis = svg.append("g")
  .attr("class", "x axis")
  .attr("transform", "translate(0," + (height) + ")")
  .call(xAxis);

var yaxis = svg.append("g")
    .attr("class", "y axis")
    .attr("transform", "translate("+(width)+",0)")
    .call(yAxis);

var color = d3.scaleOrdinal(d3.schemeCategory10);

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


d3.csv("fightsEloLong.csv", function(error, data) {
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
          .style("stroke", function() { 
              return d.color = color(d.key); })
          .attr("id", 'tag'+d.key.replace(/\s+/g, '')) // assign ID
          .attr("d", valueline(d.values));

      chartGroup.selectAll("dot")
          .data(d.values)
          .enter().append("circle")
          .attr("r", 2.5)
          .attr("cx", function(d) { return x(d.Date); })
          .attr("cy", function(d) { return y(d.rating); })
          .style("fill", function() {
              return d.color = color(d.key); })
          .style("stroke", "black")
          .style("stroke-width", "1");

      // chartGroup.append("text")
      //     .attr("transform", "translate(" +
      //         (x(d.values[d.values.length-1].Date)+10) + "," +
      //         y(d.values[d.values.length-1].rating) + ")")
      //     .attr("dy", ".35em")
      //     .attr("text-anchor", "start")
      //     .style("fill", function() { // Add the colours dynamically
      //         return d.color = color(d.key); })
      //     .text(d.values[d.values.length-1].Link + ", " +
      //           d.values[d.values.length-1].rating);

  });



  function tick() {
    t++;
    // Scale the range of the data
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
    y.domain(newDy);

    chartGroup.transition()
        .attr("transform", "translate("+ -width/ticksx  * t +")")
        .duration(duration)
        .ease(d3.easeLinear);

    yaxis.transition()
        .duration(duration/2)
        .ease(d3.easeLinear)
        .call(yAxis);

    xaxis.transition()
        .duration(duration)
        .ease(d3.easeLinear)
        .call(xAxis)
        .on("end", tick);

    ;
  }
  tick();


});
