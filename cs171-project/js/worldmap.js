WorldMap = function(_parentElement, _data, _metaData, _countriesToCountries, _completeTable, _UNData, _eventHandler){

  var that = this;
  
  this.parentElement = _parentElement;
  this.data = _data;
  this.metaData = _metaData;
  this.countriesToCountries = _countriesToCountries;
  this.completeTable = _completeTable;
  this.UNData = _UNData;
  this.eventHandler = _eventHandler;
  this.displayData = [];

  this.args = null;

  this.selectableCountries = ["Americas",  "Africa",  "Asia",  "Europe",  "Oceania","USA", "DEU", "JPN", "VNM", "CHN"];

  this.initVis();
}


WorldMap.prototype.initVis = function(){

  var that = this; 

  d3.select("#routevis").on("click", function(d) { 
    console.log(that.zoomBehavior.scale());
    console.log(that.zoomBehavior.translate());
  } )

  this.arc_color_scale =  d3.scale.log()
      .range(["#99CCFF", "#3333CC", "#000080"])
      .domain(d3.extent(this.data, function(l) {
        return l.number_of_routes;
      }))

  this.arcWidthScale = d3.scale.log()
    .range([0.3, 1.4]);

  this.arcOpacityScale = d3.scale.log()
    .range([0.01, 1.0]);

  this.zoomBehavior = d3.behavior.zoom();

 this.map =  new Datamap({
    element: document.getElementById("worldmap"),
    projection: "mercator",
    arcConfig: {
      strokeColor: "#3333CC",
      strokeWidth: 0.3,
      arcSharpness: 0.5,
      animationSpeed: 500
    },
   zoomConfig: {
            zoomOnClick: true,
            zoomFactor: 0.9,
            onlyWorldMap: true
    },
    done: function(datamap) {
      // Pan & Zoom
      datamap.svg.call(that.zoomBehavior.on("zoom", redraw));

      function redraw() {
        datamap.svg.selectAll("g").attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
      }

      d3.selectAll("path.datamaps-subunit").filter(function(d) {
        return that.selectableCountries.indexOf(d.id) >= 0;
      })
      .style("stroke", "red")
      .each(function() { this.parentNode.appendChild(this); });;
    },
    subunitClick: function(g) {
      that.wrangleData(null);

      if(g.type == "Feature")
      {

        d3.select("#heatmap_legend").style("display", "none");
        that.map.options["fills"]["defaultFill"] = "#ABDDA4";
        d3.select(".btn-group").disabled = true;
        d3.select(".btn-group").style("opacity", "0.5");
      }
      that.updateVis();               
    }, 
    subunitClicked: function(g) {
      if (g.type == "continent") {


            // TODO: Show all flights routes for selected continent

          d3.select("#addBackToWorldButton").attr("style", "");
          d3.select("#addResetZoomButton").attr("style", "display:none");

            // TODO: Show all flights ruoutes for selected continent

            $(that.eventHandler).trigger("selectionChanged", {level: "continent", subitemClicked: g});

      } else {
        that.map.updateScope(g.id);

        that.zoomBehavior.scale(1).translate([0, 0]);  
        that.map.resetZoom(0); 

        d3.select("#addBackToWorldButton").attr("style", "");
        d3.select("#addResetZoomButton").attr("style", "");

        that.map.svg.selectAll("g")
          .style("opacity", 0)
          .transition()
          .duration(750)
          .delay(300)
          .style("opacity", 1)
          .call(that.map.endAll, function () {

            that.wrangleData(function(d) { return d.origin.country == g.properties.name  && d.origin.country == d.destination.country}, "city");
            that.updateVis();    
            that.map.options.done(that.map);

            that.zoomBehavior.scale(1).translate([0, 0]);  
            that.map.resetZoom(0); 

            $(that.eventHandler).trigger("selectionChanged", {level: "country", subitemClicked: g});
          });
      }

    },
   subunitMouseover: function(g) {
        if (that.map.options.scope != "world")
            return;


        that.wrangleData(function(d) { return d.origin.country == g.properties.name 
                                        && d.origin.country != d.destination.country}, "country");
        that.updateVis();      
    },
    subunitMouseout: function() {
      if (that.map.options.scope != "world")
          return;

      //TODO: set a default filter 
    } 
  }, this);


  this.addResetZoomButton(this.parentElement);
  this.addBackToWorldButton(this.parentElement);

// Setup Continent Filters

  d3.selectAll(".continent_filter").on("click", function() { 

      if (!that.args || that.args.level != "country")
        that.map.zoomContinent(this.name);
  });


 this.wrangleData(null);
  this.updateVis();
  this.setHeatMap();
}

WorldMap.prototype.onSelectionChange= function (args){
  if (args.level == "world")  {
    this.setHeatMap();
}
  }

// level: default: city, alternative: country
WorldMap.prototype.setHeatMap= function(){
  var that = this;
  var choropleth = {};


  var heatmap_field = function(d){return parseInt(d.data["Tourism expenditure in other countries"]["2012"].removeComma())};
  var o =  d3.scale.linear()//d3.scale.ordinal()
      .domain(d3.extent(that.UNData,heatmap_field)) 
     //.range(["white", "steelblue"]);
     .range(["white", "steelblue"]);

    this.UNData.forEach(function(d) {
      var geometry = that.map.worldTopo.objects.world.geometries.filter(function(b){return d.name == b.properties.name;})[0]
      
      if (geometry) {
        choropleth[geometry.id] = o(heatmap_field(d)); 
      }
    });

    choropleth["GRL"] = choropleth["COD"] = choropleth["TZA"] = choropleth["SOM"] = choropleth["CIV"] = choropleth["SYR"] = choropleth["COG"] = choropleth["SSD"] = choropleth["HRV"] = choropleth["SIB"]= choropleth["SRB"]= choropleth["MKD"]=choropleth["LTU"]=choropleth["LAO"]= choropleth["BIH"] = choropleth["MDA"] = "#FFFFFF";

    this.map.updateChoropleth(choropleth);

    d3.select("#heatmap_legend").style("display", "");

  var heatmap_svg = d3.select("#heatmap_legend");

    heatmap_svg.selectAll(".legend").remove();

     // Add a legend for the color values.
  var legend = heatmap_svg.selectAll(".legend")
      .data(o.ticks(6).slice(1).reverse())
    .enter().append("g")
      .attr("class", "legend")
      .attr("transform", function(d, i) { return "translate(" + (0 + 20) + "," + (20 + i * 20) + ")"; });

  legend.append("rect")
      .attr("width", 20)
      .attr("height", 20)
      .style("fill", o);

  legend.append("text")
      .attr("x", 26)
      .attr("y", 10)
      .attr("dy", ".35em")
      .text(String);

  heatmap_svg.append("text")
      .attr("class", "label")
      .attr("x", 0 + 20)
      .attr("y", 10)
      .attr("dy", ".35em")
      .text("Tourism expenditure abroad (US$ Mn):");


}


// level: default: city, alternative: country
WorldMap.prototype.wrangleData= function(_filterFunction, level){
  this.displayData = this.filterAndAggregate(_filterFunction, level);
}

WorldMap.prototype.showAirlineRoutes = function(airline_name) {
var that = this;

 var level_filter = function (d) { return true;}

if (this.args == null || this.args.level == null)
      var level_filter = function (d) { return true;}
 else if (this.args.level == "continent")
      var level_filter = function (d) { return d.destination.continent == that.args.subitemClicked.id && d.origin.continent == that.args.subitemClicked.id ;}
  else if (this.args.level == "country")
      var level_filter = function (d) { return d.destination.country == that.args.subitemClicked.properties.name && d.origin.country == that.args.subitemClicked.properties.name;}  

  

  this.displayData = this.completeTable.filter(function(d) { return d.airline_name == airline_name; }).filter(level_filter);

  this.updateVis(true);
}

WorldMap.prototype.filterAndAggregate = function(_filter, level){

    var filter = _filter || function(){return false;}

    var data_level = [];

    if (level ==  "country")
        data_level = this.countriesToCountries;
    else
        data_level = this.data; //default is city to city data


    var that = this;

    var res =  data_level.filter(function(d) {
      return filter(d);
    });


   var level_filter = function (d) { return true;}

  if (this.args == null || this.args.level == null)
        var level_filter = function (d) { return true;}
   else if (this.args.level == "continent")
        var level_filter = function (d) { return d.destination.continent ==  d.origin.continent ;}
    else if (this.args.level == "country")
        var level_filter = function (d) { return d.destination.country == that.args.subitemClicked.properties.name && d.origin.country == that.args.subitemClicked.properties.name;}  

    res = res.filter(level_filter);
    return res;

}



WorldMap.prototype.updateVis = function(no_color){
  var that = this;


  this.arcOpacityScale.domain(d3.extent(d3.selectAll(".datamaps-arc"), function(l) { 
    return l.number_of_routes; }));

  if (!no_color) {
    this.displayData.forEach(function(d){
      d.options = {};
      d.options.strokeColor = that.arc_color_scale(d.number_of_routes);
      d.options.strokeWidth = that.arcWidthScale(d.number_of_routes);
      d.options.greatArc = true;
    })
  }
  this.map.arc(this.displayData);


  d3.selectAll(".datamaps-arc").style("opacity", function(d) { 
      return that.arcOpacityScale(d.number_of_routes);
      }
    );
}


WorldMap.prototype.addResetZoomButton = function(container){
    var that = this;

    var button = container.insert("button", ":first-child")
      .attr("class", "btn btn-sm btn-primary")
      .attr("id", "addResetZoomButton")
      .text("⇔ Reset Zoom")
      .on("click", function() { 
        that.zoomBehavior.scale(1).translate([0, 0]);  
        that.map.resetZoom();
      })
}


WorldMap.prototype.addBackToWorldButton = function(container){
    var that = this;

    var button = container.insert("button", ":first-child")
      .attr("id", "addBackToWorldButton")
      .attr("class", "btn btn-sm btn-primary")
      .attr("style", "display: none")
      .on("click", function() { 
        var button = d3.select(this);
        d3.select(".btn-group").disabled = false;
        d3.select(".btn-group").style("opacity", "1");

        that.map.svg.selectAll("g")
          .transition()
          .duration(750)
          .style("opacity", 0)
          .call(that.map.endAll, function () {
            button.attr("style", "display: none");

            that.map.updateScope("world");
            that.zoomBehavior.scale(1).translate([0, 0]);  
            that.map.resetZoom();

            that.wrangleData(null);
            that.updateVis(); 


            that.map.svg.selectAll("g")
              .transition()
              .duration(750)
              .delay(750)
              .style("opacity", 1)
              .call(that.map.endAll, function () {

              d3.select("#addBackToWorldButton").attr("style", "display:none");
              d3.select("#addResetZoomButton").attr("style", "");

                //TODO: set a default filter 
                that.map.options.done(that.map);


                $(that.eventHandler).trigger("selectionChanged", {level: "world" });
              });
          });

      })
      button.text("↩ To World Map");
}

WorldMap.prototype.backToWorldMap = function (){
  var that = this;

            that.map.updateScope("world");
            that.zoomBehavior.scale(1).translate([0, 0]);  
            that.map.resetZoom();

            that.wrangleData(null);
            that.updateVis(); 


            that.map.svg.selectAll("g")
              .transition()
              .duration(750)
              .delay(750)
              .style("opacity", 1)
              .call(that.map.endAll, function () {

              d3.select("#addBackToWorldButton").attr("style", "display:none");
              d3.select("#addResetZoomButton").attr("style", "");

                //TODO: set a default filter 
                that.map.options.done(that.map);

                $(that.eventHandler).trigger("selectionChanged", {level: "world" });
              });

}

WorldMap.prototype.zoomCountry = function(sel){
    var subitem = d3.selectAll('.datamaps-subunit')[0].filter(function(d){
      return (d.__data__.id==sel)
    })[0];
    this.map.publicZoom.call(this.map, subitem["__data__"]);
}


// HELPERS

