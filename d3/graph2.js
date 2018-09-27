var limit = 60 * 1,
    duration = 750,
    now = new Date(Date.now() - duration)

// set the dimensions and margins of the graph
var margin = {top: 50, right: 150, bottom: 50, left: 50},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

// parse the date
var parseDate = d3.timeParse("%Y-%m-%d");

// set the ranges
var x = d3.scaleTime().range([0, width]);
var y = d3.scaleLinear().range([height, 0]);

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

var color = d3.scaleOrdinal(d3.schemeCategory10);

var paths = svg.append('g');

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


  // Scale the range of the data
  x.domain([d3.min(data, function(d) { return d.Date }),
            d3.max(data, function(d) { return d.Date })]);
  y.domain([d3.min(data, function(d) { return d.rating }),
            d3.max(data, function(d) { return d.rating })]);

  // Nest the entries by
  var fighters = d3.nest()
      .key(function(d) {return d.Link;})
      .entries(data);

  var fighters = Object.assign({}, fighters)
  // Plot points and lines
  for (var indFighter in fighters) {
      var fighter = fighters[indFighter]
      fighter.path = paths.append('path')
          .data([fighter.data])
          .attr('class', indFighter + ' group')
          .style('stroke', fighter.color)
  };

  function tick() {

      // Add new values
      for (var indFighter in fighters) {
          var fighter = fighters[indFighter]
          //group.data.push(group.value) // Real values arrive at irregular intervals
          fighter.values.push(20 + Math.random() * 100)
          console.log(fighter);
      }


      // Slide x-axis left
      axis.transition()
          .duration(duration)
          .ease('linear')
          .call(x.axis)

      // Slide paths left
      paths.attr('transform', null)
          .transition()
          .duration(duration)
          .ease('linear')
          .attr('transform', 'translate(' + x(now - (limit - 1) * duration) + ')')
          .each('end', tick)

      // Remove oldest data point from each group
      for (var name in groups) {
          var group = groups[name]
          group.data.shift()
      }
  }

  tick()


});
