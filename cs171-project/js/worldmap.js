WorldMap = function(_parentElement, _data, _metaData, _countriesToCountries, _eventHandler){

  var that = this;
  
  this.parentElement = _parentElement;
  this.data = _data;
  this.metaData = _metaData;
  this.countriesToCountries = _countriesToCountries;
  this.eventHandler = _eventHandler;
  this.displayData = [];


  this.selectableCountries = ["USA", "DEU", "JPN"];

  this.initVis();
}


WorldMap.prototype.initVis = function(){

  var that = this; 


  d3.select("#routevis").on("click", function(d) { 
    console.log(that.zoomBehavior.scale());
    console.log(that.zoomBehavior.translate());
  } )

  this.arc_color_scale =  d3.scale.log()
      .range(["red", "blue", "black"])
      .domain(d3.extent(this.data, function(l) {
        return l.number_of_routes;
      }))

  this.arcWidthScale = d3.scale.log()
    .range([0.4, 3.7]);

  this.arcOpacityScale = d3.scale.log()
    .range([0.01, 1.0]);

  this.zoomBehavior = d3.behavior.zoom();

 this.map =  new Datamap({
    element: document.getElementById("worldmap"),
    projection: "mercator",
    arcConfig: {
      strokeColor: "darkblue",
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
      .style("stroke", "black")
      .each(function() { this.parentNode.appendChild(this); });;
    },
    subunitClick: function(g) {
      that.wrangleData(null);
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


  d3.selectAll(".continent_filter").on("change", function() { 
    if (this.checked)
      that.map.zoomContinent(this.value);
  });


 this.wrangleData(null);

  this.updateVis();
}

// level: default: city, alternative: country
WorldMap.prototype.wrangleData= function(_filterFunction, level){
  this.displayData = this.filterAndAggregate(_filterFunction, level);
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

    return res;

}



WorldMap.prototype.updateVis = function(){
  var that = this;


  this.arcOpacityScale.domain(d3.extent(d3.selectAll(".datamaps-arc"), function(l) { 
    return l.number_of_routes; }));

  this.displayData.forEach(function(d){
    d.options = {};
   // d.options.strokeColor = that.arc_color_scale(d.number_of_routes);
    //d.options.strokeWidth = that.arcWidthScale(d.number_of_routes);
    d.options.greatArc = true;
  })

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

// HELPERS





