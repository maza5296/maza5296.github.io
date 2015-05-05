
var margin = {top: 20, right: 20, bottom: 30, left: 40},
    width = 900 - margin.left - margin.right,
    height = 600 - margin.top - margin.bottom;

/* 
 * value accessor - returns the value to encode for a given data object.
 * scale - maps value to a visual display encoding, such as a pixel position.
 * map function - maps from data value to display value
 * axis - sets up axis
 */ 

// setup x 
// xValue = function(d) { return parseInt(d['classes']);}, // data -> value
var xScale = d3.scale.linear().range([0, width - 70]), // value -> display
    xAxis = d3.svg.axis().scale(xScale).orient("bottom");

// setup y
//yValue = function(d) { return parseFloat(d['ratings']['Course Overall']);}, // data -> value
var yScale = d3.scale.linear().range([height, 0]), // value -> display
    yAxis = d3.svg.axis().scale(yScale).orient("left");

var xValue, yValue;

var xMap = function(d) { return xScale(xValue(d));}
var yMap = function(d) { return yScale(yValue(d));}

var color = d3.scale.category10();

var toggled_fields = []

var story_2_helper = 0,
    timer, clean_time ;


// add the graph canvas to the body of the webpage
var svg = d3.select("#vis").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

var rect = svg.append("rect")
    .attr("class", "background")
    // .attr("width", width)
    // .attr("height", height)

var container = svg.append('g')




// add the tooltip area to the webpage
var tooltip = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 1)
    .style("display", "none");

// add the story area to the webpage
var story_area = d3.select("#story_text")
    .attr("class", "story_area")
    .style("opacity", 1)
    .style("display", "none");

var all_data
var xaxis_svg, yaxis_svg, legend;

var active_year = 2012;
var yAxis_label = 'Difficulty'
var xAxis_label = 'Workload'
var active_categories = [];

// load data
d3.json("fields.json", function(error, loaded_data) {
  all_data = loaded_data;
  color.domain(all_data.map(function(d){return d["category"]}));
  selectAxis();
});


function selectAxis(){
  if (xAxis_label != 'classes' && xAxis_label != 'enrollment'){
    xValue = function(d) { return parseFloat(d['ratings'][xAxis_label]);}
  }
  else{
    xValue = function(d){return parseFloat(d[xAxis_label])}
  }

  if (yAxis_label != 'classes' && yAxis_label != 'enrollment'){
    yValue = function(d) { return parseFloat(d['ratings'][yAxis_label]);}
  }
  else{
    yValue = function(d){return parseFloat(d[yAxis_label])}
  }

  if(active_categories.length == 0){ selectAllData() }
  else{ selectData(active_categories) }

}

function selectData(categories){
  var data = all_data.filter(function (d){
    return (categories.indexOf(d.category) > -1) && (parseInt(d['year']) == active_year)
  });
  drawVis(data)
}

function selectAllData(){
  var data = all_data.filter(function (d){
    return (parseInt(d['year']) == active_year)
  });
  drawVis(data)
}

drawVis = function(data) {
  //if (!xaxis_svg){
  yScale.domain([d3.min(all_data, yValue) * .95, d3.max(all_data, yValue) * 1.05]);
  xScale.domain([d3.min(all_data, xValue) * .95, d3.max(all_data, xValue) * 1.05]);
  //}

  // x-axis
  if(!xaxis_svg){
  xaxis_svg = container.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis)
    .append("text")
      .attr("class", "x label")
      .attr("x", width - 70)
      .attr("y", -6)
      .style("text-anchor", "end")
      .text("Classes");
  }
    
  // y-axis
  if (!yaxis_svg){
  yaxis_svg = container.append("g")
      .attr("class", "y axis")
      .call(yAxis)
    .append("text")
      .attr("class", "y label")
      .attr("transform", "rotate(-90)")
      .attr("y", 6)
      .attr("dy", ".71em")
      .style("text-anchor", "end")
      .text("Overall Score (mean)");
  }

  // join data
  var circles = container.selectAll("circle")
      .data(data, function(d) { return d["name"]; });

  //update axis
  svg.select(".y.axis")
    .call(yAxis)

  svg.select(".y.label")
    .text(yAxis_label)

  svg.select(".x.axis")
    .call(xAxis)

  svg.select(".x.label")
    .text(xAxis_label)

  //update points
  circles.attr("class", "update")
    .transition()
    .duration(750)
    .attr("cx", xMap)
    .attr('cy', yMap)      
    .attr("r", function(d){
        return Math.pow( parseFloat(d['enrollment']), 1/2) * .6
      });

  // draw dots
  circles.enter().append("circle")
      .attr("class", "enter")
      .attr("r", function(d){
        return Math.pow( parseFloat(d['enrollment']), 1/2) * .6
      })
      .attr("cx", xMap)
      .attr("cy", yMap)
      .style("fill", function(d) { return color(d['category']);}) 
      .style("fill-opacity", ".6")
      .style('stroke', function(d) { return color(d['category']); })
      .on("mouseover", function(d) {
          tooltip.transition()
               .duration(200)
               .style("display", "")
               .style("opacity", .8);
          tooltip.html(d['name'] +  
            "<br/> "+ toTitleCase(xAxis_label) + ":  "+ Math.round((100.0*parseFloat(xValue(d))) )/ 100  +
          "<br/> "+ toTitleCase(yAxis_label) + ":  "+ Math.round((100.0*parseFloat(yValue(d))) )/ 100 )
               .style("left", (d3.event.pageX + 5) + "px")
               .style("top", (d3.event.pageY - 28) + "px");
      })
      .on("mouseout", function(d) {
          tooltip.transition()
               .duration(500)
               .style("opacity", 0);
      });

  // EXIT
  // Remove old elements as needed.
  circles.exit()
      .attr("class", "exit")
    .transition()
      .duration(50)
      .style("fill-opacity", 1e-6)
      .style("stroke-opacity", 1e-6)
      .remove();

  // draw legend
  if (!legend){
    legend = svg.selectAll(".legend")
        .data(color.domain())
      .enter().append("g")
        .attr("class", "legend")
        .attr("transform", function(d, i) { return "translate(-50," + i * 20+ ")"; });
    // draw legend colored rectangles
    legend.append("rect")
        .attr("class", "rect legend")
        .attr("x", width - 18)
        .attr("width", 18)
        .attr("height", 18)
        .style("fill", color);

    // draw legend text
    legend.append("text")
        .attr("x", width - 24)
        .attr("y", 9)
        .attr("dy", ".35em")
        .style("text-anchor", "end")
        .text(function(d) { return d;})
  }

  var rects = svg.selectAll(".legend .rect")


}



function cleanup () {
  d3.selectAll('circle')
    .style("fill", function(d){ return color(d.category) })
    .style("fill-opacity", function(d){ return .6 })

  story_2_helper = 0
  clearInterval(clean_time)
  clearInterval(timer)
}

function story_2 () {
  if( (2006 + story_2_helper) == 2013){
    clearInterval(timer)
    clean_time = setInterval(function(){cleanup()}, 2000)
    return 0
  }
  //Changes axis and active "types"
  active_year = 2006 + story_2_helper
  active_categories = ['Humanities', 'Math and Engineering']
  xAxis_label = "enrollment"
  yAxis_label = "classes"
  d3.select("input[value=\"xenrollment\"]").attr('checked', true)
  d3.select("input[value=\"yclasses\"]").attr('checked', true)
  if(story_2_helper == 3){story_2_helper = story_2_helper + 2 }
  else{ story_2_helper += 1  }
  
  //Changes years
  d3.selectAll(".years li").attr("class", "")
  d3.select('#' + "y_" + active_year).attr("class", "active")

  //Highlight CS/English
  d3.selectAll('circle')
    .style("fill", function(d){
      if (d.name == "Computer Science") { return  'purple'}
      else if (d.name == "English"){ return '#f3f315' }
      else{ return color(d.category) }
    })
    .style("fill-opacity", function(d){
      if (d.name == "Computer Science") { return .9}
      else if (d.name == "English"){ return .9}
      else{ return .6 }
    })
  story_area.transition()
             .style("display", "")
             .style("opacity", 1);
  story_area.html("<font size= '3'> SEAS: Big Growth, Big Classes </font><br/><br/>" +
                 "The School of Engineering and Applied Sciences has seen tremendous " +
                 "growth, especially in CS. Here CS is displayed here in purple and "+
                 "English in yellow, we notice the general trend of a decline in English enrollment in favor of CS and Stat." +
                 "The rise in enrollment has not seen an equal rise in offered courses. CS only grew from 27 courses offered in 2006 to 32 in 2012 " +
                 "Finally we note that while Computer Science" +
                 "boasted 700 more enrolled students in 2012, English offers nearly 3 times the number of courses." + 
                 "(Note 2010 data collection had faults and is not included in show)")
  selectAxis()
}

function story_changer(story){
  if(story == "SEAS Growth"){
    story_2()
    timer = setInterval(function(){story_2()}, 2000)
  }

  else if(story == 'CS Workload'){
    active_year = 2012
    active_categories = ['Social Science', 'Math and Engineering']
    xAxis_label = "Workload"
    yAxis_label = "Difficulty"
    d3.select("input[value=\"xWorkload\"]").attr('checked', true)
    d3.select("input[value=\"yDifficulty\"]").attr('checked', true)
    story_area.transition()
               .style("display", "")
               .style("opacity", 1);
    story_area.html("<font size= '3'> This is why we’re up at 2am </font><br/><br/>" +
                    "Every dining hall at 2 am has that same small, tired group of " +
                    "students staring intensely at screens. The few proud hackers of the house. We are joined by our fellow concentrators in the SEAS department, " +
                    "which have higher workloads on average than any of the concentrations in the social sciences. " +
                    "Furthermore as expected, these higher workloads are paired with higher difficulties.")  
    selectAxis()
  }
  else if(story == 'Gen Eds'){
    active_year = 2012
    active_categories = ['General Education', 'Humanities', 'Art', 'Math and Engineering']
    xAxis_label = "Course Overall"
    yAxis_label = "Difficulty"
    d3.select("input[value=\"xWorkload\"]").attr('checked', true)
    d3.select("input[value=\"yDifficulty\"]").attr('checked', true)
    story_area.transition()
               .style("display", "")
               .style("opacity", 1);
    story_area.html("<font size= '3'> Gen Eds: That Easy, Boring, Schedule Filler </font><br/><br/>" +
                    "Gen Ed’s often make up our fourth or fifth class, are selected 2 days before " +
                    "school starts, and pretty much decided by “which one looks easy”. We never seem " +
                    "to expect very much going into gen ed’s and unfortunately this seems to be true." +
                    " Looking at the difficulty versus the overall score, " +
                    "gen ed's (in red) generally fall below a 4.0 in overall and below a " +
                    "3.0 in difficulty. These are much lower " +
                    "concentrations in the arts, the humanities, and SEAS.")
    selectAxis()
  }
  else if(story == ' Humanities'){

    active_year = 2012
    active_categories = ['Humanities', 'Math and Engineering']
    xAxis_label = "Course Overall"
    yAxis_label = "enrollment"
    d3.select("input[value=\"xCourse Overall\"]").attr('checked', true)
    d3.select("input[value=\"yenrollment\"]").attr('checked', true)
    story_area.transition()
               .style("display", "")
               .style("opacity", 1);
    story_area.html("<font size= '3'> Humanities Concentrators ARE Happier </font><br/><br/>" +
                    "Humanities majors really are happier than the rest of us. Looking at enrollment vs. " +
                    "'course overall', we see that humanities (orange) rank their classes " +
                    "higher than the math and engineering concentrations. While the cause of this is unknown, " + 
                    "what is known is that for some overall well designed courses that shouldn't be missed.")
    selectAxis()
  }
}

d3.selectAll(".years li")
  .on("click", function() {
    d3.selectAll(".years li").attr("class", "");
    d3.select(this).attr("class", "active");
    active_year = parseInt(d3.select(this).text())
    cleanup()
    if(active_categories.length == 0){ selectAllData() }
    else{selectData(active_categories)}
  });

  //   rects.on('click', function(d){
  //   var cat = String(d)
  //   if(active_categories.indexOf(cat) > -1){
  //     active_categories.splice(active_categories.indexOf(cat), 1)
  //   }
  //   else{
  //     active_categories.push(cat)
  //   }
  //   if(active_categories.length == 0){ selectAllData() }
  //   else{ selectData(active_categories) };
  // })

d3.selectAll("#category-buttons li")
  .on("click", function(){
    var cat = d3.select(this).text();

    if (active_categories.indexOf(cat) > -1){
      active_categories.splice(active_categories.indexOf(cat), 1)
    }
    else{
      active_categories.push(cat)
    }
    d3.selectAll("#category-buttons li").attr("class", "");
    if (cat == "All"){
       selectAllData();
       active_categories = [];
    }
    else if(active_categories.length == 0 ){ selectAllData() }
    else{ 
      selectData(active_categories);
      active_categories.map(function (c) {
        d3.select('#' + c.split(' ')[0]).attr("class", "active")
      })
    };

  })




d3.selectAll(".stories li")
  .on("click", function() {
    cleanup()
    d3.selectAll(".stories li").attr("class", "");
    d3.select(this).attr("class", "active");
    story = String(d3.select(this).text())
    story_changer(story)
  });

//from http://stackoverflow.com/questions/196972/convert-string-to-title-case-with-javascript
function toTitleCase(str)
{
    return str.replace(/\w\S*/g, function(txt){return txt.charAt(0).toUpperCase() + txt.substr(1).toLowerCase();});
}

d3.select("input[value=\"yDifficulty\"]").on("click", function(){
  yAxis_label = 'Difficulty'
  cleanup()
  selectAxis()
})
d3.select("input[value=\"yCourse Overall\"]").on("click", function(){
  yAxis_label = "Course Overall"
  cleanup()
  selectAxis()
});
d3.select("input[value=\"yenrollment\"]").on("click", function(){
  yAxis_label = "enrollment"
  cleanup()
  selectAxis()
});
d3.select("input[value=\"yclasses\"]").on("click", function(){
  yAxis_label = 'classes'
  cleanup()
  selectAxis()
});
d3.select("input[value=\"yWorkload\"]").on("click", function(){
  yAxis_label = 'Workload'
  cleanup()
  selectAxis()
});


d3.select("input[value=\"xDifficulty\"]").on("click", function(){
  xAxis_label = 'Difficulty'
  cleanup()
  selectAxis()
})
d3.select("input[value=\"xCourse Overall\"]").on("click", function(){
  xAxis_label = "Course Overall"
  cleanup()
  selectAxis()
});
d3.select("input[value=\"xenrollment\"]").on("click", function(){
  xAxis_label = "enrollment"
  cleanup()
  selectAxis()
});
d3.select("input[value=\"xclasses\"]").on("click", function(){
  xAxis_label = 'classes'
  cleanup()
  selectAxis()
});
d3.select("input[value=\"xWorkload\"]").on("click", function(){
  xAxis_label = 'Workload'
  cleanup()
  selectAxis()
});
