// 
//
// r2d3: https://rstudio.github.io/r2d3
//


// set the dimensions and margins of the graph
var innerRadius = Math.min(width, height) / 4,
    outerRadius = Math.min(width, height)/2;   // the outerRadius goes from the middle of the SVG area to the border
    
// Scales
var x = d3.scaleBand()
      .range([0, 2 * Math.PI])    // X axis goes from 0 to 2pi = all around the circle. If I stop at 1Pi, it will be around a half circle
      .align(0)                  // This does nothing
      .domain(data.map(function(d) { return d.feature; })); // The domain of the X axis is the list of features.
      
var y = d3.scaleLinear()
      .range([innerRadius, outerRadius])   // Domain will be define later.
      .domain([0, 4]); // Domain of Y is from 0 to the max seen in the data
      
var axis_scale = d3.scaleLinear()
      .range([0, outerRadius])   // Domain will be define later.
      .domain([-2, 2]); // Domain of Y is from 0 to the max seen in the data
      
var axis = d3.axisLeft(axis_scale);

var tooltip = d3.select("body")
      .append("div")
      .attr("class", "toolTip")
      .style("position", "absolute")
      .style("display","none")
      .style("height","auto")
      .style("background","black")
      .style("border","1px solid #6F257F")
      .style("color","white");
      

// append the svg object
svg=svg.append("g")
    .attr("transform", "translate(" +width / 2  + "," + height / 2+ ")");

  // Add the bars
  svg.append("g")
    .selectAll("path")
    .data(data)
    .enter()
    .append("path")
    .attr("fill", d=>d.scaled_cluster_feature_value<0?"#B53737":"#69b3a2")
    .attr("d", d3.arc()     // imagine your doing a part of a donut plot
    .innerRadius(innerRadius)
    .outerRadius(function(d) { return y(d.scaled_cluster_feature_value); })
    .startAngle(function(d) { return x(d.feature); })
    .endAngle(function(d) { return x(d.feature) + x.bandwidth(); })
    .padAngle(0.01)
    .padRadius(innerRadius))
    .on("mousemove", function(d){
            tooltip
              .style("left", d3.event.pageX - 50 + "px")
              .style("top", d3.event.pageY - 70 + "px")
              .style("display", "inline-block")
              .html(("Feature Value: "+ Math.round(d.cluster_feature_value*100)/100) + "<br>" + "Population Mean:" + Math.round((d.population_mean)*100)/100);
        })
    		.on("mouseout", function(d){ tooltip.style("display", "none");});
    
   // Add the labels
  svg.append("g")
      .selectAll("g")
      .data(data)
      .enter()
      .append("g")
        .attr("text-anchor", function(d) { return (x(d.feature) + x.bandwidth() / 2 + Math.PI) % (2 * Math.PI) < Math.PI ? "end" : "start"; })
        .attr("transform", function(d) { return "rotate(" + ((x(d.feature) + x.bandwidth() / 2) * 180 / Math.PI - 90) + ")"+"translate(" + (y(d.scaled_cluster_feature_value<0?0:d.scaled_cluster_feature_value)+10) + ",0)"; })
      .append("text")
        .text(function(d){return(d.feature+"("+Math.round(d.scaled_cluster_feature_value*100)/100+")")})
        .attr("transform", function(d) { return (x(d.feature) + x.bandwidth() / 2 + Math.PI) % (2 * Math.PI) < Math.PI ? "rotate(180)" : "rotate(0)"; })
        .style("font-size", "11px")
        .attr("alignment-baseline", "middle");
        
  //svg.call(axis);