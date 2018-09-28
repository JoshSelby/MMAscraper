// set the dimensions and margins of the graph
var margin = {top: 50, right: 150, bottom: 50, left: 50},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

// parse the date
var parseDate = d3.timeParse("%Y-%m-%d");

// set the ranges
var x = d3.scaleTime()
          .range([0, width]);
var y = d3.scaleLinear()
          .range([height, 0]);

var yAxis = d3.axisLeft(y)
              .scale(y);
var xAxis = d3.axisBottom(x)
              .scale(x);

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
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

var circleTransition = d3.select("circle")
                         .transition();

var color = d3.scaleOrdinal(d3.schemeCategory10);

var t = 2000;

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


d3.csv("fightsEloLong2.csv", function(error, data) {
  if (error) throw error;

  // format the data
  data.forEach(function(d) {
      d.Link = d.Link.replace(/\d|-/g, " ").trim(),
      d.Date = parseDate(d.Date);
      d.rating = +d.rating;
  });


  var i = 1;
  function dataNestLoop(limit) {
    var limit = data.length+1;
    var ref = setInterval(() => {
      dataNest = d3.nest()
        .key(function(d) {return d.Link})
        .entries(data.slice(0,i));
      update();
      createGraph();
      i++;
      if (i == limit) clearInterval(ref);
    }, t);
  };

  function update (){
    minDate = d3.min(data.slice(0,i), function(d) { return d.Date });
    maxDate = d3.max(data.slice(0,i), function(d) { return d.Date });
    minrating = d3.min(data.slice(0,i), function(d) { return d.rating });
    maxrating = d3.max(data.slice(0,i), function(d) { return d.rating });

    yearMax = maxDate.getFullYear()
    monthMax = maxDate.getMonth()
    dayMax = maxDate.getDate()

    // Scale the range of the data
    x.domain([new Date(yearMax - 1, monthMax + 1, dayMax),
                 new Date(yearMax, monthMax + 1, dayMax)]);
    y.domain([minrating, maxrating]);

  }

  function createGraph() {

    // Plot points and lines
    dataNest.forEach(function(d,i) {
      // data-join
      var dot = svg.selectAll("dot")
          .data(d.values);
      console.log(dataNest);

      // update
      dot.transition(t)
          .ease(d3.easeLinear)
          .attr("r", 5)
          .attr("cx", function(d) { return x(d.Date); })
          .attr("cy", function(d) { return y(d.rating); })
          .style("fill", function() { // Add the colours dynamically
              return d.color = color(d.key); });

      // enter
      dot.enter().append("circle")
          .attr("r", 3)
          .attr("cx", function(d) { return x(d.Date); })
          .attr("cy", function(d) { return y(d.rating); })
          .style("fill", function() { // Add the colours dynamically
              return d.color = color(d.key); })
          .style("stroke", "black")
          .style("stroke-width", "1")
        .transition()
        .ease(d3.easeLinear)
          .duration(t)
          .remove();


        svg.append("path")
            .attr("class", "line")
            .style("stroke", function() { // Add the colours dynamically
                return d.color = color(d.key); })
            .attr("id", 'tag'+d.key.replace(/\s+/g, '')) // assign ID
            .attr("d", valueline(d.values))
            .transition()
              .ease(d3.easeLinear)
              .duration(t)
              .remove();


        svg.append("text")
            .attr("transform", "translate(" +
                (x(d.values[d.values.length-1].Date)+10) + "," +
                y(d.values[d.values.length-1].rating) + ")")
            .attr("dy", ".35em")
            .attr("text-anchor", "start")
            .style("fill", function() { // Add the colours dynamically
                return d.color = color(d.key); })
            .text(d.values[d.values.length-1].Link + ", " +
                  d.values[d.values.length-1].rating)
            .transition()
              .duration(t)
              .remove();

        svg.select(".x")
           .transition(t)
             .ease(d3.easeLinear)
           .call(xAxis);

        svg.select(".y")
           .transition(t)
             .ease(d3.easeLinear)
           .call(yAxis);


        });

  };


  dataNestLoop();

  // add the X gridlines
  svg.append("g")
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(make_x_gridlines()
          .tickSize(-height)
          .tickFormat("")
      )

  // add the Y gridlines
  svg.append("g")
      .attr("class", "grid")
      .call(make_y_gridlines()
          .tickSize(-width)
          .tickFormat("")
      )

  svg.append("g")
      .attr("class", "axis x")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);

  svg.append("g")
      .attr("class", "axis y")
      .call(yAxis);


});
