Table = function(_parentElement, _data,_eventHandler){
  
  this.parentElement = _parentElement;
  this.data = _data;
  this.eventHandler = _eventHandler;
  this.displayData = [];

  this.initVis();
}


Table.prototype.initVis = function(){

  var that = this; 

  var titles = ["Airline Name", "Airline Country", "Number of Routes"];
  var variable_names = ["airline", "country", "count"]


  function draw(data){
    var table = d3.select("#table").append("table"),
        thead = table.append("thead")
                     .attr("class", "thead");

        tbody = table.append("tbody");

        table.attr("class", "table table-hover table-bordered table-condensed");
        table.attr("data-search", "true")
        table.attr("id", "airline_ranking")
        table.attr("cellspacing", "0");
        table.attr("width", "80%");


        d3.select("caption").style("color", "black")
        

        thead.append("tr").selectAll("th")
          .data(titles)
        .enter()
          .append("th")
          .text(function(d) {return d; })


        var rows = tbody.selectAll("tr.row")
          .data(data)
          .enter()
          .append("tr").attr("class", "row");

        var cells = rows.selectAll("td")
          .data(function(row) {
            return variable_names.map(function(column) {
              return row[column]
            });
          })
          .enter()
          .append("td")
          .attr("class", "cell")
          .text(function(d) { return d; }); 

    }

    draw(that.data)

    $('#airline_ranking').dataTable({
        "scrollY":        "400px",
        "scrollCollapse": true,
        "paging":         false
    });


    //FILTER FUNCTION: filter data that has continent = name
    function getData(data, name) {
          return that.data.filter(function(d){
            return d["continent"] == name;
          });
    }

    d3.selectAll("input").each(function(d) 
        { 
          //FILTER
          if(d3.select(this).attr("type") == "checkbox") 
          {
            d3.selectAll("input").on("change", function() 
            {
              var name = d3.select(this).attr("name");
              var new_data = getData(that.data, name)
              console.log(that.data)
              console.log(new_data)
              d3.select("table").remove()
              draw(new_data)
            })       
          }//END FILTER 
        })


  this.updateVis();
}



Table.prototype.updateVis = function(){
  var that = this;
  
  
}


Table.prototype.onSelectionChange= function (ranges){
}



// HELPERS






