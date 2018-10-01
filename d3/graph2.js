var data = [];
var time_frame = 10000;
var height = 400;
var width = 700;
var margin = 30;
var duration = 1000;
var time = Date.now();

var xScale = d3.scaleTime()
    .domain([time + duration, time - (duration * 2) - time_frame])
    .range([width - (margin *2), 0]);
var yScale = d3.scaleLinear()
    .domain([0,10])
    .range([height - (margin*2), 0]);

var xAxis = d3.axisBottom(xScale)
    .ticks(4);

var yAxis = d3.axisRight(yScale)
    .ticks(4);

var svg = d3.select('body')
    .append('svg')
    .attr('width', width)
    .attr('height', height)
    .append('g');

svg.append('g')
    .attr('transform', 'translate(' + (width - margin * 2) + ',' + margin + ')')
    .classed('y axis', true)
    .call(yAxis);

svg.append('g')
    .attr('transform', 'translate(0,' + (height - margin) + ')')
    .classed('x axis', true);

svg.append('defs')
    .append('clipPath')
    .attr('id', 'clip')
    .append('rect')
    .attr('width', (width - (margin * 4)))
    .attr('height', (height - (margin * 2)))
    .attr('transform', 'translate(-1.5, 20)');

svg.append('g')
    .attr('clip-path', 'url(#clip)')
    .classed('line_', true)
    .append('path')
    .datum(data)
    .classed('line', true)
    .style('fill', 'none')
    .style('stroke', 'steelblue')
    .style('stroke-width', '1.5px');

var interval = d3.interval(update, 1000);

function update(){
    time = Date.now();
    data.push({'x': time, 'y': d3.randomUniform(0,9)()});
    draw();
}

function draw(){
    var line = d3.line()
        .x(function(d) { return xScale(d.x); })
        .y(function(d) { return yScale(d.y); });

    var lineselection = svg.selectAll('.line_')
        .select('path');


    lineselection.interrupt()
    		.transition()
        .duration(duration)
        .ease(d3.easeLinear)
        //.attr('transform', 'translate(' + -(xScale.range()[0]/((duration / 100)-2)) + ',' + margin + ')');
        .attr('transform', 'translate(' + -(xScale(data[data.length-1].x) -xScale.range()[0]) + ',' + margin + ')');


    if (data[0].x < time - time_frame - duration ){
            console.log('shift');
                data.shift();
    }

    lineselection.attr('d',line)
        .attr('transform', 'translate(0,' + margin + ')');


    xScale.domain([time, time + (duration * 2) - time_frame])
        .range([width - (margin *2),0]);

    d3.select('body')
        .select('svg')
        .select('g')
        .selectAll('.x.axis')
        .transition()
        .duration(duration)
        .ease(d3.easeLinear)
        .call(xAxis);
}
