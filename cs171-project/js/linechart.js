LineChart = function(_parentElement, _data, _eventHandler){
  
  this.parentElement = _parentElement;
  this.data = _data;
  this.eventHandler = _eventHandler;
  this.displayData = [];
  this.color = d3.scale.category10()
  this.countries = 5;
  this.title = "Top arrivals of non-resident passengers";
  this.selection;


  this.margin = {top: 20, right: 20, bottom: 150, left: 60},
  this.width =  500 - this.margin.left - this.margin.right,
  this.height = 380 - this.margin.top - this.margin.bottom;
  this.initVis();
}


LineChart.prototype.initVis = function(){

  var that = this; 

  this.svg = this.parentElement.select("svg")
        .append("g")
        .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");

  this.xScale = d3.scale.linear().range([0, this.width]).domain([2005,2010]);
  this.yScale = d3.scale.linear().range([this.height, 0]);
  
  this.xAxis = d3.svg.axis()
    .scale(this.xScale)
    .tickFormat(d3.format("d"))
    .orient("bottom");

  this.yAxis = d3.svg.axis()
    .scale(this.yScale)
    .orient("left");

  this.svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + this.height + ")")

  this.svg.append("g")
        .attr("class", "y axis")

  this.wrangleData();
  this.updateVis();
  
}


LineChart.prototype.wrangleData= function(_ranges){

    var tmp = this.filterAndAggregate(_ranges); 
    var that = this;

    var res = tmp.map(function(d){
        return {
          name: d["name"],
          continent: d["continent"],
          data : [
              {
                "year": "2005",
                "value": parseInt(d["data"]["Arrivals"]["2005"].removeComma()),
              },{
                "year": "2006",
                "value": parseInt(d["data"]["Arrivals"]["2006"].removeComma()),
              },{
                "year": "2007",
                "value": parseInt(d["data"]["Arrivals"]["2007"].removeComma()),
              },{
                "year": "2008",
                "value": parseInt(d["data"]["Arrivals"]["2008"].removeComma()),
              },{
                "year": "2009",
                "value": parseInt(d["data"]["Arrivals"]["2009"].removeComma()),
              },{
                "year": "2010",
                "value": parseInt(d["data"]["Arrivals"]["2010"].removeComma()),
              },
          ]
        }
    });
    this.displayData = res.sort(function (a,b){
        return d3.descending (parseInt(a["data"][5]["value"]), parseInt(b["data"][5]["value"]));
    })
}

LineChart.prototype.filterAndAggregate = function(ranges){

    if(ranges == null) {
      this.countries = 5;
      this.selection = "(Wordwide)";
      return this.data;
    } else if(ranges["level"] == "world") {
      this.countries = 5;
      this.selection = "(Wordwide)";
      return this.data;
    } else if (ranges["level"] == "country") {
      this.countries = 1;
      this.selection = "(" + ranges["subitemClicked"]["properties"]["name"] + ")";
      return this.data.filter(function(d){
        return d["name"] == ranges["subitemClicked"]["properties"]["name"];
      });
    } else if (ranges["level"] == "continent") {
      this.countries = 5;
      this.selection = "(" + ranges["subitemClicked"]["id"] + ")";
      return this.data.filter(function(d){
        return d["continent"] == ranges["subitemClicked"]["id"];
      });
    }
}

LineChart.prototype.updateVis = function(){
  var that = this;

  this.titleElement = d3.select("#lineheading").text("" + this.title + " " + this.selection);

  this.svg.selectAll(".lines").remove();
  this.svg.selectAll(".legend").remove();

  var tmp = [];
  for (i = 0; i < that.countries; i ++) {
    tmp = tmp.concat(d3.extent(that.displayData[i]["data"], function(d){return parseInt(d["value"])}));
  }

  this.yScale.domain(d3.extent(tmp));

  this.svg.select(".x.axis")
        .call(this.xAxis)
        .append("text")
        .attr("x",210)
        .attr("dy", "28")
        .style("text-anchor", "end")
        .text("Years");

  this.svg.select(".y.axis")
        .call(this.yAxis)
        .append("text")
        .attr("x",-5)
        .attr("dy", "-45")
        .attr("transform", function(d) {
                return "rotate(-90)" 
                })
        .style("text-anchor", "end")
        .text("Arrival passenger numbers (thousands)");

  for (i = 0; i < that.countries; i++){
    this.line = d3.svg.line()
    .x(function(d) { return that.xScale(parseInt(d["year"])); })
    .y(function(d) { return that.yScale(parseInt(d["value"])); })
    .interpolate("monotone");;
    var path = this.svg.append("path")
    .attr("class", "lines")
    .attr('d', that.line(that.displayData[i]["data"]))
    .attr('stroke', that.color(i))
    .attr('stroke-width', 2)
    .attr('fill', 'none')
    .attr("data-legend", that.displayData[i]["name"])    
    
    path.append("text")
      .datum(function(d) { return {name: that.displayData[i]["name"], value: that.displayData[i]["data"][5]}; })
      .attr("transform", function(d) { return "translate(" + that.xScale(parseInt(d["value"]["year"])) + "," + that.yScale(parseInt(d["value"]["value"])) + ")"; })
      .attr("x", 3)
      .attr("dy", ".35em")
      .text(function(d) { return d.name; });
  };

  legend = this.svg.append("g")
    .attr("class","legend")
    .attr("transform","translate(30,15)")
    .style("font-size","12px")
    .call(d3.verticallegend)
}


LineChart.prototype.onSelectionChange= function (_ranges){
  this.wrangleData(_ranges);
  this.updateVis();
}