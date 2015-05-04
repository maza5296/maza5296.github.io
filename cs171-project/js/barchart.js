/* TODO: 
Color / Continent
Selection Changed (incl. title changing)
reduce to one chart? / understand, check data

*/

BarChart = function(_parentElement, _data, _eventHandler){
  
  this.parentElement = _parentElement;
  this.data = _data;
  this.eventHandler = _eventHandler;
  this.displayData = [];  

  this.country_codes = {
    "United Kingdom": "UK",
    "United States": "US",
    "Canada": "CA",

    "France": "FR",
    "Russia": "RU",
    "China": "CN",
    "Germany": "DE",
    "Netherlands": "NL",
    "Spain": "ES",
    "Belgium": "BE",
    "Austria": "AT",
    "Italy": "IT",


    "South Africa": "ZA",
    "Kenya": "KE",
    "Egypt": "EG",
    "Morocco": "MA",
    "Ethiopia": "ET",
    "Algeria": "DZ",
    "Tunisia": "TN",
    "Cameroon": "CM",

    "Japan": "JP",
    "Turkey": "TR",
    "Singapore": "SG",
    "Hong Kong": "HK",
    "United Arab Emirates": "AE",
    "India": "IN",
    "Thailand": "TH",

    "Australia": "AU",
    "New Zealand": "NZ",
    "Fiji": "FJ"




  }


  this.initVis();
}


BarChart.prototype.initVis = function(){

  var that = this; 

  var margin = {
      top: 10,
      right: 120,
      bottom: 10,
      left: 120*1.4
  };

  this.margin = margin;
  this.width = width = 360 - margin.left - margin.right;
  this.height = height = 200 - margin.top - margin.bottom;



  this.svg = this.parentElement.append("svg")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom + 30)
      .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");


 this.titleElement = this.svg.insert("text", ":first-child")
    .attr("class", "title")
    .attr("transform", "translate(" + -margin.left + ",0)");
  
    this.onSelectionChange({level:"world"}, true);

}

// level: default: city, alternative: country
BarChart.prototype.wrangleData= function(_filterFunction, _value, _label, _title){

  that = this;

  this.title = _title;

  this.colorScale = d3.scale.category10()
  this.colorScale.domain(["Americas", "Africa",  "Asia",  "Europe",  "Oceania"]);

  this.displayData = this.filterAndAggregate(_filterFunction, _value).map(function (d) {
      return {
          label: _label(d),
          value: _value(d),
          continent: d.continent,
          color: that.colorScale(d.continent),
      };

  });

  console.log(this.displayData);

}

BarChart.prototype.filterAndAggregate = function(_filter, _value){
  return this.data.filter(function(d) {
    return _filter(d);
  })
  .sort(function(a,b) {return _value(b)-_value(a); })
  .slice(0,10)

}



BarChart.prototype.updateVis = function(init){
  var that = this;

  this.titleElement.text(this.title);

  var selectedColumn = "value";
      
      
      this.max = d3.max(this.displayData, function(d) { return d[selectedColumn]; } );
      this.min = 0;

      this.offset = 20;
  
      this.xScale = d3.scale.linear().range([0, width]);

      this.xScale.domain([this.min, this.max]);
      
      this.bar_height = 10 
      
      groups = this.svg.selectAll("g").data(this.displayData);    
      
      var groups_new = groups.enter()
          .append("g")

        bars =  groups_new.append("rect")
                  .attr("width", function(d) { 
          return that.xScale(d[selectedColumn]); })
        .attr("height", this.bar_height)
                  .attr("x", 3)
                  .attr("y", 0)
        .attr("fill", function(d) { 
          return d.color }); 

      labels = groups_new.append("text")
        .attr("x", 0)
        .attr("y", 0)
          .attr("dy", ".8em")
        .attr("text-anchor", "end")
        .attr("class", "labels")
          
      values =  groups_new.append("text")
        .attr("y", 0)
        .attr("dy", ".8em")
        .attr("class", "values")


    groups.exit().selectAll("*").remove();
    groups.exit().remove();
        

      
      //Update all
      groups
      .attr("transform",  function(d, i) {
        return "translate("+that.xScale(that.min)+","+((that.bar_height + 5) * i+that.offset)+")";
       });
 
      groups.select("rect")
        .attr("width", function(d) { 
          return that.xScale(d[selectedColumn]); })
        .attr("fill", function(d) { 
          return d.color })
       .attr("data-legend", function(d) { return d.continent; })  
  
      groups.select("text.labels")
        .text(function(d) { return d.label; });    
          
      groups.select("text.values")
        .attr("x", function(d) { return that.xScale(d[selectedColumn]) + 6; })
        .text(function(d) { return d3.format(",")(d[selectedColumn]); }); 


         this.svg.selectAll(".legend").remove();

       legend = this.svg.append("g")
      .attr("class","legend")
      .attr("transform", "translate(" + (-this.margin.left+20) + ","+(height+25)+")")
      .style("font-size","12px")
      .call(d3.legend, 80)
      
      if (!init) {
          groups.style("fill-opacity", 0)
          .transition()
          .duration(500)
          .style("fill-opacity", 1);
          
        groups.
        select("rect").style("fill-opacity", 0)
          .transition()
          .duration(500)
          .style("fill-opacity", 1);
      }
          
    }



BarChart.prototype.onSelectionChange= function (args, init){

  var that = this;
  if (args.level == "world") {
      this.wrangleData(
      function(d) { return true; }, 
      function(d) { return d.number_of_routes; },
      function(d) { return d.most_active_airport.name + " (" + that.country_codes[d.country] + ")"; },
      "Top 10 airports by number of routes (Worldwide)"
    );
  
    this.updateVis();
  } else if (args.level == "country") {
    var country = args.subitemClicked.properties.name;

    this.wrangleData(
      function(d) { return d.country ==  country}, 
      function(d) { return d.number_of_routes; },
      function(d) { return d.most_active_airport.name;},
      "Top 10 airports by number of routes ("  + country + ")"
    );

    this.updateVis();
  } else if (args.level == "continent") {
    var continent = args.subitemClicked.id;

   this.wrangleData(
      function(d) { return d.continent ==  continent}, 
      function(d) { return d.number_of_routes; },
      function(d) { return d.most_active_airport.name + " (" + that.country_codes[d.country] + ")"; },
      "Top 10 airports by number of routes ("  + continent + ")"
    );
  
    this.updateVis();
  }
}

  
BarChart.prototype.sortBars = function(sortingAnimation) {
      // Sort Elements     

      that  = this;
      var selectedSortColumn = "value";
        
      
      groupsToSort = this.svg.selectAll("g").sort(function (a,b) {
        if (a[selectedSortColumn] == b[selectedSortColumn]) 
        {
          return smartSort(a["name"], b["name"]); // break ties by sorting by country name
        } else {
          return smartSort(a[selectedSortColumn], b[selectedSortColumn]); 
        }
        
      })
    
      if (sortingAnimation > 0) {
        groupsToSort =  groupsToSort
          .transition()
          .duration(500)
      }
      
      groupsToSort.attr("transform",  function(d, i) {
        return "translate("+that.xScale(that.min)+","+((that.bar_height + 5) * i+that.offset)+")";
       });


     //Sorts columns with strings lexicographic (A-Z) and columns with numbers numerically (Big to Small)
      function smartSort(a, b) {
        switch (typeof(a)) {
          case "string": return d3.ascending(a, b);
          case "number": return a - b; //https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort
          default: return d3.ascending(a, b);
        }
      } 
    }



