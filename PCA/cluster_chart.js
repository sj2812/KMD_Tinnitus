// 
//
// r2d3: https://rstudio.github.io/r2d3
//


// set the dimensions and margins of the graph
var innerRadius = Math.min(width, height) / 4,
    outerRadius = Math.min(width, height)/2;   // the outerRadius goes from the middle of the SVG area to the border

// append the svg object
svg=svg.append("g")
    .attr("transform", "translate(" +width / 2  + "," + height / 2+ ")");

// Scales
  var x = d3.scaleBand()
      .range([0, 2 * Math.PI])    // X axis goes from 0 to 2pi = all around the circle. If I stop at 1Pi, it will be around a half circle
      .align(0)                  // This does nothing
      .domain(data.map(function(d) { return d.feature; })); // The domain of the X axis is the list of features.
  var y = d3.scaleLinear()
      .range([innerRadius, outerRadius])   // Domain will be define later.
      .domain([0, 2]); // Domain of Y is from 0 to the max seen in the data

  // Add the bars
  svg.append("g")
    .selectAll("path")
    .data(data)
    .enter()
    .append("path")
    .attr("fill", d=>d.mean_difference<0?"#B53737":"#69b3a2")
    .attr("d", d3.arc()     // imagine your doing a part of a donut plot
    .innerRadius(innerRadius)
    .outerRadius(function(d) { return y(d.mean_difference); })
    .startAngle(function(d) { return x(d.feature); })
    .endAngle(function(d) { return x(d.feature) + x.bandwidth(); })
    .padAngle(0.01)
    .padRadius(innerRadius));

   // Add the labels
  svg.append("g")
      .selectAll("g")
      .data(data)
      .enter()
      .append("g")
        .attr("text-anchor", function(d) { return (x(d.feature) + x.bandwidth() / 2 + Math.PI) % (2 * Math.PI) < Math.PI ? "end" : "start"; })
        .attr("transform", function(d) { return "rotate(" + ((x(d.feature) + x.bandwidth() / 2) * 180 / Math.PI - 90) + ")"+"translate(" + (y(d.mean_difference<0?0:d.mean_difference)+10) + ",0)"; })
      .append("text")
        .text(function(d){return(d.feature)})
        .attr("transform", function(d) { return (x(d.feature) + x.bandwidth() / 2 + Math.PI) % (2 * Math.PI) < Math.PI ? "rotate(180)" : "rotate(0)"; })
        .style("font-size", "11px")
        .attr("alignment-baseline", "middle");