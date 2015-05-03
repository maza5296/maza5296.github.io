/**
 * Created by Hendrik Strobelt (hendrik.strobelt.com) on 1/28/15.
 */



/*
 *
 * ======================================================
 * We follow the vis template of init - wrangle - update
 * ======================================================
 *
 * */

/**
 * AgeVis object for HW3 of CS171
 * @param _parentElement -- the HTML or SVG element (D3 node) to which to attach the vis
 * @param _data -- the data array
 * @param _metaData -- the meta-data / data description object
 * @constructor
 */
AgeVis = function(_parentElement, _data, _metaData, _eventHandler){
    this.parentElement = _parentElement;
    this.data = _data;
    this.metaData = _metaData;
    this.displayData = [];
    this.filteredData = [];
    this.eventHandler = _eventHandler;



    // TODO: define all constants here
    this.margin = {top: 20, right: 0, bottom: 30, left: 30},
    this.width =  230 - this.margin.left - this.margin.right,
    this.height = 330 - this.margin.top - this.margin.bottom;

    this.initVis();

}


/**
 * Method that sets up the SVG and the variables
 */
AgeVis.prototype.initVis = function(){

    var that = this; // read about the this


    //TODO: construct or select SVG
    //TODO: create axis and scales
    this.svg = this.parentElement.select("svg")
        .append("g")
        .attr("transform", "translate(" + this.margin.left + "," + this.margin.top + ")");;

    this.x = d3.scale.linear()
      .range([0, this.width]);

    this.y = d3.scale.linear()
      .range([this.height, 0]);

    this.xAxis = d3.svg.axis()
      .scale(this.x)
      .orient("bottom");

    this.yAxis = d3.svg.axis()
      .scale(this.y)
      .orient("left");

    this.area = d3.svg.area()
      .interpolate("linear")
      .x(function(d) {return that.x(d.count); })
      .x0(0)
      .y0(this.height)
      .y1(function(d) {return that.y(d.age)});

    this.svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + this.height + ")");

    this.svg.append("g")
        .attr("class", "y axis")
      .append("text")
        .attr("y", 0)
        .attr("x", this.width/2)
        .attr("dy", ".71em")
        .style("text-anchor", "end")
        .text("age distribution");

    // filter, aggregate, modify data
    this.wrangleData(null);

    // call the update method
    this.updateVis();
}


/**
 * Method to wrangle the data. In this case it takes an options object
 * @param _filterFunction - a function that filters data or "null" if none
 */
AgeVis.prototype.wrangleData= function(_filterFunction){

    // displayData should hold the data which is visualized
    this.displayData = this.filterAndAggregate(_filterFunction);

    //// you might be able to pass some options,
    //// if you don't pass options -- set the default options
    //// the default is: var options = {filter: function(){return true;} }
    //var options = _options || {filter: function(){return true;}};

}


/**
 * the drawing function - should use the D3 selection, enter, exit
 */
AgeVis.prototype.updateVis = function(){

    // Dear JS hipster,
    // you might be able to pass some options as parameter _option
    // But it's not needed to solve the task.
    // var options = _options || {};


    // TODO: implement...
    // TODO: ...update scales
    // TODO: ...update graphs


    this.x.domain(d3.extent(this.displayData, function(d){return d.count}));
    this.y.domain(d3.extent(this.displayData, function(d){return d.age}));

    this.svg.select(".y.axis")
        .call(this.yAxis);

    var path = this.svg.selectAll(".area")
      .data([this.displayData])

      // this.displayData.map(function(d){
      //   console.log(d.count[0]);
      // })

    path.enter()
      .append("path")
      .attr("class", "area");

    path
      .attr("d", this.area);

    path.exit()
      .remove();


}


/**
 * Gets called by event handler and should create new aggregated data
 * aggregation is done by the function "aggregate(filter)". Filter has to
 * be defined here.
 * @param selection
 */
AgeVis.prototype.onSelectionChange= function (selectionStart, selectionEnd){

    // TODO: call wrangle function
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


/*
*
* ==================================
* From here on only HELPER functions
* ==================================
*
* */



/**
 * The aggregate function that creates the counts for each age for a given filter.
 * @param _filter - A filter can be, e.g.,  a function that is only true for data of a given time range
 * @returns {Array|*}
 */
AgeVis.prototype.filterAndAggregate = function(_filter){


    // Set filter to a function that accepts all items
    // ONLY if the parameter _filter is NOT null use this parameter
    var that = this;
    if (_filter == null) {
        this.filteredData = this.data;
    }
    //Dear JS hipster, a more hip variant of this construct would be:
    // var filter = _filter || function(){return true;}

    // create an array of values for age 0-100
    var res = d3.range(100).map(function (i) {
        return 0;
    });

    // accumulate all values that fulfill the filter criterion

    // TODO: implement the function that filters the data and sums the values
    res.map(function(d, i){
        that.filteredData.map(function(n){
            res[i] = res[i] + n.ages[i]; 
        })
    });

    var test = [];
    res.map(function(d, i){
        if(i != 99){
            var tmp = {
                age: i,
                count: res[i]
            }
        } else {
            var tmp = {
                age: 99,
                count: 0
            }
        }
        test.push(tmp);
    });
    return test;

}




