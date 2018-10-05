var t = 0,
    duration = 2000,
    ticksx = 6;

// set the dimensions and margins of the graph
var margin = {top: 50, right: 50, bottom: 50, left: 50},
    width = 960 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

// parse the date
var parseDate = d3.timeParse("%Y-%m-%d");

// set the ranges
var x = d3.scaleTime()
    .domain([new Date(2010, 1, 1), new Date(2010,7, 1)])
    .range([0, width]);
var y = d3.scaleLinear()
    .domain([900,1300])
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
    .style("background-color", "lightgrey");


var chartGroup = svg.append("g")
      .attr("transform",
            "translate(" + margin.left + "," + margin.top + ")");

var xAxis = d3.axisBottom()
  .scale(x)
  .tickFormat(d3.timeFormat("%b %Y"))
  .tickSize(6);

var yAxis = d3.axisLeft()
   .scale(y)
   .ticks(5);

var xaxis = svg.append("g")
  .attr("class", "x axis")
  .attr("transform", "translate("+margin.left+"," + (height +margin.top) + ")")
  .call(xAxis);

var yaxis = svg.append("g")
    .attr("class", "y axis")
    .attr("transform", "translate("+(width+margin.left)+","+margin.top+")")
    .call(yAxis);

var color = d3.scaleOrdinal(d3.schemeCategory10);

d3.csv("fightsEloLong3.csv", function(error, data) {
  if (error) throw error;

  // format the data
  data.forEach(function(d) {
      d.Link = d.Link.replace(/\d|-/g, " ").trim(),
      d.Date = parseDate(d.Date);
      d.rating = +d.rating;
  });

  var dataNest = d3.nest()
      .key(function(d) {return d.Link;})
      .entries(data);

  // dataNest.forEach(function(d,i) {
  //   chartGroup.append("path")
  //       .attr("class", "line")
  //       .style("stroke", function() {
  //           return d.color = color(d.key); })
  //       .attr("id", d.key) // assign ID
  //       .attr("d", valueline(d.values));
  //
  //     console.log(d.values[0].Date, d.values[d.values.length - 1].Date)
  // });


  chartGroup.selectAll(".line")
      .data(dataNest)
      .enter()
        .append("path")
        .attr("class", "line")
        .style("stroke", function(d) {
                  return d.color = color(d.key); })
        .attr("id", function(d) { return d.key })
        .attr("d", function(d) { return valueline(d.values); });



  function tick() {
    t++;

      var curDx = x.domain().slice();
      var curDx2 = [new Date(curDx[0].getTime()), new Date(curDx[1].getTime())]
      var nxtMonth = [curDx2[0].addMonths(6), curDx2[1].addMonths(1)]

      //remove dots
      chartGroup.selectAll("g")
          .filter(function(d,i) {
            return d.Date < new Date(curDx[0].getTime()).addMonths(-1)})
          .remove()

      //enter
      chartGroup.selectAll(".dot")
          .data(data.filter(function(d,i) {
            return d.Date >nxtMonth[0] & d.Date <= nxtMonth[1] }))
          .enter().append("g")
          .append("circle")
              .attr("r", 4)
              .attr("cx", function(d) { return x(d.Date); })
              .attr("cy", function(d) { return y(d.rating); })
              .attr("fill", function(d) {return d.color = color(d.Link)});

      var newDx = [curDx[0].setMonth(curDx[0].getMonth() + 1),
                    curDx[1].setMonth(curDx[1].getMonth() + 1)]

      var newdata = data.filter(function(d) {
        return d.Date >= new Date(curDx[0].getTime()).addYears(-1) &
               d.Date <= new Date(curDx[1].getTime()).addDays(15) })

      var curmaxRate = d3.max(newdata,
          function(d) {return d.rating});


      var curminRate = curmaxRate - 300;

      var curDy = y.domain();
      var newDy = [curminRate, curmaxRate];

      x.domain(newDx);
      y.domain(newDy);

      yaxis.transition()
        .duration(duration/2)
        .ease(d3.easeLinear)
        .call(yAxis);

      xaxis.transition()
        .duration(duration)
        .ease(d3.easeLinear)
        .call(xAxis)
        .on("end", tick);

      //update lines
      chartGroup.selectAll(".line")
            .transition()
            .attr("class", "line")
            .style("stroke", function(d) {
                      return d.color = color(d.key); })
            .attr("d", function(d) { return valueline(d.values); })
            .duration(duration)
            .ease(d3.easeLinear);

      //update dots
      chartGroup.selectAll("circle")
          .filter(function(d,i) {
            return d.Date > new Date(curDx[0].getTime()).addMonths(-1) })
          .transition()
          .attr("cx", function(d) {return x(d.Date); })
          .attr("cy", function(d) {return y(d.rating); })
          .duration(duration)
          .ease(d3.easeLinear);
    }

    tick();


  });
