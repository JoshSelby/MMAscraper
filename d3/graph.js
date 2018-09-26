// set the dimensions and margins of the graph
var margin = {top: 50, right: 100, bottom: 50, left: 50},
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
    .y(function(d) { return y(d.rating); });

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


d3.csv("fightsEloLong2.csv", function(error, data) {
  if (error) throw error;

  // format the data
  data.forEach(function(d) {
      d.Link = d.Link.replace(/\d|-/g, " ").trim(),
      d.Date = parseDate(d.Date);
      d.rating = +d.rating;
  });

  // Scale the range of the data
  x.domain(d3.extent(data, function(d) { return d.Date; }));
  y.domain(d3.extent(data, function(d) { return d.rating; }));

  // Nest the entries by
  var dataNest = d3.nest()
      .key(function(d) {return d.Link;})
      .entries(data);

  legendSpace = width/dataNest.length;

  dataNest.forEach(function(d,i) {

      svg.append("path")
          .attr("class", "line")
          .style("stroke", function() { // Add the colours dynamically
              return d.color = color(d.key); })
          .attr("id", 'tag'+d.key.replace(/\s+/g, '')) // assign ID
          .attr("d", valueline(d.values));

      svg.selectAll("dot")
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
              (x(data[data.length-1].Date)+10) + "," +
              y(data[data.length-1].rating) + ")")
          .attr("dy", ".35em")
          .attr("text-anchor", "start")
          .style("fill", "red")
          .text(dataNest[0].key);

      console.log(dataNest[0].key);


      // Add the Legend
      // svg.append("text")
      //     .attr("x", (legendSpace/2)+i*legendSpace)  // space legend
      //     .attr("y", height + (margin.bottom/2)+ 5)
      //     .attr("class", "legend")    // style the legend
      //     .style("fill", function() { // Add the colours dynamically
      //         return d.color = color(d.key); })
      //     .on("click", function(){
      //         // Determine if current line is visible
      //         var active   = d.active ? false : true,
      //         newOpacity = active ? 0 : 1;
      //         // Hide or show the elements based on the ID
      //         d3.select("#tag"+d.key.replace(/\s+/g, ''))
      //             .transition().duration(100)
      //             .style("opacity", newOpacity);
      //         // Update whether or not the elements are active
      //         d.active = active;
      //         })
      //     .text(d.key);

  });



  // Add the X Axis
  svg.append("g")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x));

  // Add the Y Axis
  svg.append("g")
      .call(d3.axisLeft(y));

});
