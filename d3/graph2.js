var t = 0,
ticksx = 6,
duration = 2000;

var parseDate = d3.timeParse("%Y-%m-%d");
var svg = d3.select("svg"),
   margin = {top: 20, right: 30, bottom: 20, left: 30},
   width = +svg.attr("width") - margin.left - margin.right,
   height = +svg.attr("height") - margin.top - margin.bottom,
   g = svg.append("g").attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var x = d3.scaleTime()
   .domain([new Date(2010, 0, 1), new Date(2010, ticksx, 1)])
   .range([0, width]);

var y = d3.scaleLinear()
  .domain([900,2200])
  .range([height, 0]);

var xAxis = d3.axisBottom()
  .scale(x)
  .tickFormat(d3.timeFormat("%b %Y"))
  .tickSize(6);

var yAxis = d3.axisLeft()
   .scale(y)
   .ticks(5);

var xaxis = svg.append("g")
 .attr("class", "x axis")
 .attr("transform", "translate("+margin.right+"," + (height+margin.top) + ")")
 .call(xAxis);

var yaxis = svg.append("g")
   .attr("class", "y axis")
   .attr("transform", "translate("+(width + margin.right)+","+margin.top+")")
   .call(yAxis);

var valueline = d3.line()
   .x(function(d) { return x(d.Date); })
   .y(function(d) { return y(d.rating); })
   .curve(d3.curveStepAfter);

var color = d3.scaleOrdinal(d3.schemeCategory10);


d3.csv("fightsEloLong2.csv", function(error, data) {
  if (error) throw error;

  // format the data
  data.forEach(function(d) {
      d.Link = d.Link.replace(/\d|-/g, " ").trim(),
      d.Date = parseDate(d.Date);
      d.rating = +d.rating;
  });

  function update() {
    xdomain = [new Date(x.domain()[1].setMonth(x.domain()[1].getMonth() - 1)),
              new Date(x.domain()[1].setMonth(x.domain()[1].getMonth() + 1))]
    datarange = data.filter(function(d) {
      return d.Date >= xdomain[0] & d.Date <= xdomain[1]
    })
    dataNest = d3.nest()
        .key(function(d) {return d.Link;})
        .entries(datarange);
    var curDx = x.domain();
    var newDx = [new Date(curDx[0].setMonth(curDx[0].getMonth() + 1)),
                new Date(curDx[1].setMonth(curDx[1].getMonth() + 1))]
    var curmaxRate = d3.max(data.filter(function(d) {
        return d.Date >= curDx[0] & d.Date <= curDx[1] }),
        function(d) {return d.rating});
    var curminRate = curmaxRate - 200;
    var curDy = y.domain();
    var newDy = [curminRate, curmaxRate];
    x.domain(newDx);
    return dataNest;
  };

  function render(dataNest) {
    dataNest.forEach(function(d,i) {
    // Enter
    d3.select("body").selectAll("dot")
        .data(dataNest)
        .enter()
        .append("circle")
          .attr("r", 3)

    // Update
    d3.select("body").selectAll("dot")
        .data(dataNest)
          .attr("r", 2.5)
          .attr("cx", function(d) { return x(d.Date); })
          .attr("cy", function(d) { return y(d.rating); })
          .style("fill", function() {
              return d.color = color(d.key); })
          .style("stroke", "black")
          .style("stroke-width", "1");

    // Exit
    d3.select("body").selectAll("dot")
        .data(dataNest)
        .exit()
          .remove();

    });
  };

  setInterval(function () {
    render(update())
  }, duration)


});
