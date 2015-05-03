/**
 * Created by Hendrik Strobelt (hendrik.strobelt.com) on 1/28/15.
 */


//TODO: DO IT ! :) Look at agevis.js for a useful structure

/*
 *
 * ======================================================
 * We follow the vis template of init - wrangle - update
 * ======================================================
 *
 * */

/**
 * PrioVis object for HW3 of CS171
 * @param _parentElement -- the HTML or SVG element (D3 node) to which to attach the vis
 * @param _data -- the data array
 * @param _metaData -- the meta-data / data description object
 * @constructor
 */

PrioVis = function(_parentElement, _data, _metaData, _eventHandler){
	this.parentElement = _parentElement;
 	this.data = _data;
 	this.metaData = _metaData;
 	this.eventHandler = _eventHandler;
  this.displayData = [];
  this.filteredData = [];

	this.margin = {top: 20, right: 0, bottom: 170, left: 60},
  this.width =  650 - this.margin.left - this.margin.right,
  this.height = 440 - this.margin.top - this.margin.bottom;

  this.initVis();
}

/**
 * Method that sets up the SVG and the variables
 */
PrioVis.prototype.initVis = function(){
	var that = this;

	this.svg = this.parentElement.select("svg")
        .append("g")
        .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");;

    this.x = d3.scale.ordinal()
      .rangeRoundBands([0, this.width], .1);

  	this.y = d3.scale.linear()
  	  .range([this.height, 0]);

  	this.color = d3.scale.category20();

  	this.xAxis = d3.svg.axis()
  	  .scale(this.x)
  	  .orient("bottom");

  	this.yAxis = d3.svg.axis()
  	  .scale(this.y)
  	  .orient("left");

  	this.svg.append("g")
  	  .attr("class", "x axis")
  	  .attr("transform", "translate(0," + this.height + ")");

  	this.svg.append("g")
  	  .attr("class", "y axis")
  	  .attr("transform", "translate(0, 0)");

    this.svg.append("g")
        .attr("class", "y axis")
      .append("text")
        .attr("x", 350)
        .attr("dy", ".71em")
        .style("text-anchor", "end")
        .text("dispribution of priorities");

  	this.wrangleData(null);
  	this.updateVis();
}


PrioVis.prototype.updateVis = function(){

    this.y.domain(d3.extent(this.displayData, function(d) { return d.count; }));
    this.x.domain(this.displayData.map(function(d) { return d.type; }));
    this.color.domain(this.displayData.map(function(d) { return d.type }));

    this.svg.select(".x.axis")
        .call(this.xAxis)
        .selectAll("text")
        .style("text-anchor", "end")
        .attr("transform", function(d) {
                return "rotate(-65)" 
                });

    this.svg.select(".y.axis")
    	.call(this.yAxis);

    var that = this;

    var bar = this.svg.selectAll(".bar")
      .data(this.displayData, function(d) { return d.type; });

    var rect = this.svg.selectAll("rect")
      .data(this.displayData, function(d) { return d.type; });

    // Append new bar groups, if required
    var bar_enter = bar.enter().append("g");

    // Append a rect and a text only for the Enter set (new g)
    bar_enter.append("rect");
    bar_enter.append("text");

    bar
      .attr("class", "bar")
      .attr("transform", function(d, i) { 
          return "translate(" + that.x(d.type) +",0)"; })

    // Remove the extra bars
    // Update all inner rects and texts (both update and enter sets)
    bar.exit().remove();

    bar.selectAll("rect")
      .attr("x", 0)
      .attr("y", function(d){return (that.y(d.count) - 3)})
      .attr("width", this.x.rangeBand())
      .style("fill", function(d,i) {return that.color(d.type);})
      .attr("height", function(d, i) {
          return that.height - that.y(d.count) + 3;
      });
      console.log(this.displayData[0]["count"])
}

/**
 * Method to wrangle the data. In this case it takes an options object
 * @param _filterFunction - a function that filters data or "null" if none
 */
PrioVis.prototype.wrangleData = function(_filterFunction){

    // displayData should hold the data whiche is visualized
    this.displayData = this.filterAndAggregate(_filterFunction);

    //// you might be able to pass some options,
    //// if you don't pass options -- set the default options
    //// the default is: var options = {filter: function(){return true;} }
    //var options = _options || {filter: function(){return true;}};
}

PrioVis.prototype.filterAndAggregate = function(_filter){
    // Set filter to a function that accepts all items
    // ONLY if the parameter _filter is NOT null use this parameter
    var that = this;
    if (_filter == null) {
        this.filteredData = this.data;
    }

    var res = d3.range(16).map(function (i) {
        return 0;
    });

    // accumulate all values that fulfill the filter criterion
    // TODO: implement the function that filters the data and sums the values
    res.map(function(d, i){
        that.filteredData.map(function(n){
            res[i] = res[i] + n.prios[i]; 
        })
    });

    var test = [];
    res.map(function(d, i){
        var tmp = {
    		type: that.metaData.priorities[i]["item-title"],
    		count: res[i]
    	}
        test.push(tmp);
    });
    return test;
}

PrioVis.prototype.onSelectionChange = function (selectionStart, selectionEnd){
    if(selectionStart != null){
        this.filteredData = this.data.filter(function (d){
            if (d.time >= selectionStart && d.time <= selectionEnd){
                return d.time;
            }
        })
        this.wrangleData(true);
    } else {
        this.wrangleData();
    }
    this.updateVis();
}
