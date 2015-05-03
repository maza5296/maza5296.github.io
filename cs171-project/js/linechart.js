LineChart = function(_parentElement, _data, _eventHandler){
  
  this.parentElement = _parentElement;
  this.data = _data;
  this.eventHandler = _eventHandler;
  this.displayData = [];
  this.color = d3.scale.category10()


  this.margin = {top: 20, right: 20, bottom: 170, left: 60},
  this.width =  650 - this.margin.left - this.margin.right,
  this.height = 440 - this.margin.top - this.margin.bottom;
  console.log(_data);
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
    .ticks(5)
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


LineChart.prototype.wrangleData= function(_filterFunction){
    var tmp = this.filterAndAggregate("Japan"); 
    console.log(tmp);
    var that = this;

    this.displayData = tmp.map(function(d){
        return {
          name: d["name"],
          continent: d["continent"],
          data : [
              {
                "year": "2005",
                "value": d["data"]["Arrivals"]["2005"],
              },{
                "year": "2006",
                "value": d["data"]["Arrivals"]["2006"],
              },{
                "year": "2007",
                "value": d["data"]["Arrivals"]["2007"],
              },{
                "year": "2008",
                "value": d["data"]["Arrivals"]["2008"],
              },{
                "year": "2009",
                "value": d["data"]["Arrivals"]["2009"],
              },{
                "year": "2010",
                "value": d["data"]["Arrivals"]["2010"],
              },
          ]
        }
    });
    console.log(that.displayData);
}

LineChart.prototype.filterAndAggregate = function(_filter){

    var filter = _filter || function(){return false;}

    var that = this;

    var filteredData =  this.data.filter(function(d) {
      return d["name"] == filter;
    });
    console.log(filteredData[0]["data"]["Arrivals"]);
    return filteredData;
}

LineChart.prototype.updateVis = function(){
  var that = this;
  debugger;

  var max = d3.max(that.displayData[0]["data"], function(d){return d["value"]})
  var min = d3.min(that.displayData[0]["data"], function(d){return d["value"]})
  this.yScale.domain([parseInt(min), parseInt(max)])

  this.svg.select(".x.axis")
        .call(this.xAxis)

  this.svg.select(".y.axis")
        .call(this.yAxis)

  for (i = 0; i < 5; i++){

    this.line = d3.svg.line()
    .x(function(d) { return that.xScale(parseInt(d["year"])); })
    .y(function(d) { debugger; return that.yScale(parseInt(d["value"])); })
    .interpolate("monotone");;
    console.log(that.displayData[0]["data"])
    this.svg.append("path")
    .attr('d', that.line(that.displayData[0]["data"]))
    .attr('stroke', that.color(i))
    .attr('stroke-width', 2)
    .attr('fill', 'none')
    .text(that.displayData[0]["name"]);;
  }

}


LineChart.prototype.onSelectionChange= function (ranges){
}