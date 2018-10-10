var t = 0,
    duration = 1000,
    ticksx = 6;

// set the dimensions and margins of the graph
var margin = {top: 50, right: 50, bottom: 50, left: 50},
    width = 1720 - margin.left - margin.right,
    height = 900 - margin.top - margin.bottom;

// parse the date
var parseDate = d3.timeParse("%Y-%m-%d");

// set the ranges
var x = d3.scaleTime()
    .domain([new Date(1989, 6, 1), new Date(1989,12, 1)])
    .range([0, width]);
var y = d3.scaleLinear()
    .domain([900,1100])
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
    .style("background-color", "black");

svg.append("clipPath")
    .attr("id", "rect-clip")
    .append("rect")
    .attr("width", width * 3/4)
    .attr("height", height);

svg.append("clipPath")
    .attr("id", "rect-clip2")
    .append("rect")
    .attr("transform",
          "translate(" + (width * 3/4) + ",0)")
    .attr("width", width * 1/4)
    .attr("height", height);

var chartGroup = svg.append("g")
      .attr("transform",
            "translate(" + margin.left + "," + margin.top + ")")
      .attr("clip-path", "url(#rect-clip)");

var textGroup = svg.append("g")
      .attr("transform",
            "translate(" + margin.left + "," + margin.top + ")")
      .attr("clip-path", "url(#rect-clip2)");

var xAxis = d3.axisBottom()
  .scale(x)
  .tickFormat(d3.timeFormat("%b %Y"))
  .tickSize(-height);

var yAxis = d3.axisRight()
   .scale(y)
   .ticks(5)
   .tickSize(-width);

var xaxis = svg.append("g")
  .attr("class", "x axis")
  .attr("transform", "translate("+margin.left+"," + (height + margin.top + 20) + ")")
  .call(xAxis);

var yaxis = svg.append("g")
    .attr("class", "y axis")
    .attr("transform", "translate("+width+","+margin.top+")")
    .call(yAxis);

var color = d3.scaleOrdinal(d3.schemeCategory20);

var textlabel = textGroup.append("g")
    .attr("class", "label")
    .append("text")
        .attr("x", width * 3/4 + 20)
        .attr("dy", ".35em");

d3.csv("fightsEloLong15.csv", function(error, data) {
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

  chartGroup.selectAll(".line")
      .data(dataNest)
      .enter()
        .append("path")
        .attr("class", "line")
        .style("stroke", function(d) {
                  return d.color = color(d.key); })
        .style("stroke-width", 3)
        .attr("id", function(d,i) { return "line"+i ; })
        .attr("d", function(d) { return valueline(d.values); });

  function tick() {
    t++;

      var curDx = x.domain().slice();
      var curDx2 = [new Date(curDx[0].getTime()), new Date(curDx[1].getTime())]
      var nxtMonth = [curDx2[0].addMonths(6), curDx2[1].addMonths(1)]

      textData = _.compact(chartGroup.selectAll(".line")
          .nodes().map(function(d) {
            return d.__data__.values
              .filter(function(d) {
                return d.Date <= new Date(curDx[1].getTime()).addDays(-45)
          }).pop()}))

      //remove text
      textGroup.selectAll("text")
          .remove();

      //remove path
      chartGroup.selectAll(".line").filter(function(d,i){
          var totalLength = [d3.selectAll(".line").nodes()[i].getPointAtLength(10000000000).x];
          var lastFight = totalLength.map(function (d) { return x.invert(d); });
          return lastFight[0] < x.domain()[0]
        }).remove();

      //remove dots
      chartGroup.selectAll("g")
          .filter(function(d,i) {
            return d.Date < new Date(curDx[0].getTime()).addYears(-1)})
          .exit()
          .remove();

      //enter text
      textGroup.selectAll("text")
          .data(textData)
          .enter().append("g")
            .append("text")
              .attr("x", width * 0.76)
              .attr("y", function(d) { return y(d.rating); })
              .attr("dy", ".35em")
              .attr("fill", function(d) { return d.color = color(d.Link); })
              .text(function(d) { return d.Link + ", " +d.rating; })
              .attr("font-family", "sans-serif")
              .attr("font-weight", "bold")
              .attr("font-size", "20px");

      //enter dots
      chartGroup.selectAll(".dot")
          .data(data.filter(function(d,i) {
            return d.Date > new Date(curDx[1].getTime()).addDays(-45) &
            d.Date <= new Date(curDx[1].getTime()).addDays(-14) }))
          .enter().append("g")
          .append("circle")
              .attr("r", 10)
              .attr("cx", function(d) { return x(d.Date); })
              .attr("cy", function(d) { return y(d.rating); })
              .attr("fill", function(d) { return d.color = color(d.Link)})
              .attr("stroke", "white")
              .attr("stroke-width", 2);

      var newDx = [curDx[0].setMonth(curDx[0].getMonth() + 1),
                    curDx[1].setMonth(curDx[1].getMonth() + 1)]

      var newdata = data.filter(function(d) {
        return d.Date >= new Date(curDx[0].getTime()).addYears(-1) &
               d.Date <= new Date(curDx[1].getTime()).addDays(-45) })

      var curmaxRate = d3.max(newdata,
          function(d) {return d.rating});

      maxes = {}
      newdata.forEach(function(e) {
        maxes[e.Link] = e.Link in maxes ? Math.max(maxes[e.Link], e.rating) : e.rating;
      })

      function getKeysWithHighestValue(o){
        var keys = Object.keys(o);
        keys.sort(function(a,b){
          return o[b] - o[a];
        })
        var values = Object.values(o);
        values.sort(function(a,b){
          return b - a;
        })
        return [keys, values];
      }

      maxes = getKeysWithHighestValue(maxes)

      if (maxes[1].length < 15) {
        var curminRate = maxes[1][maxes[1].length-1]
      } else {
        var curminRate = maxes[1][14];
      }

      var curDy = y.domain();
      if (isNaN(curminRate)) {
        var newDy = [900, 1100]
      } else {
        var newDy = [curminRate-25, curmaxRate+25];
      }

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

      //update text
      textGroup.selectAll("text")
          .transition()
            .attr("y", function(d) { return y(d.rating); })
            .attr("fill", function(d) { return d.color = color(d.Link); })
            .text(function(d) { return d.Link + ", " +d.rating; })
            .duration(duration)
            .ease(d3.easeLinear);

      //update lines
      chartGroup.selectAll(".line")
            .transition()
            .attr("class", "line")
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
