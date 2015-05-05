Table = function(_parentElement, _data,_eventHandler){
  
  this.parentElement = _parentElement;
  this.data = _data;
  this.eventHandler = _eventHandler;
  this.displayData = [];
  draw(this.data)
}




  var that = this; 

  var titles = ["Airline Name", "Airline Country", "Number of Routes"];
  var variable_names = ["airline", "country", "count"]


  function draw(data){
    var container = d3.select("#table").append("div")
                          .attr("class", "contain");
    var table = d3.select(".contain").append("table"),


        thead = table.append("thead")
                     .attr("class", "thead");

        tbody = table.append("tbody");

        table.attr("class", "table table-hover table-bordered");
        table.attr("data-search", "true")
        table.attr("id", "airline_ranking")
        table.attr("cellspacing", "0");
        table.attr("width", "100%");


        d3.select("caption").style("color", "black")
        

        thead.append("tr").selectAll("th")
          .data(titles)
        .enter()
          .append("th")
          .text(function(d) {return d; })


        var rows = tbody.selectAll("tr.row")
          .data(data)
          .enter()
          .append("tr")
          .attr("class", "row");

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

          $('#airline_ranking').dataTable({
              "order": [[ 2, "desc" ]],
            });


    }




Table.prototype.updateVis = function(){
  var that = this;
  
  
}

//FILTER FUNCTION: filter data that has continent = name
function getData(data, name, key) {
          return data.filter(function(d){
            return d[key] == name;
          });
}

Table.prototype.onSelectionChange= function (ranges){
  if(ranges.level == "country")
  {
    var name = ranges["subitemClicked"]["properties"].name;
    key = "country"
    var new_data = getData(this.data, name, key)
    d3.select("#airline_ranking_wrapper").remove()
    draw(new_data)
  }
  if(ranges.level == "continent")
  {
    var name = ranges["subitemClicked"].id;
    key = "continent"
    var new_data = getData(this.data, name, key)
    d3.select("#airline_ranking_wrapper").remove()
    draw(new_data)
  }
  if(ranges.level == "world")
  {
    d3.select("#airline_ranking_wrapper").remove()
    draw(this.data);
  }
  


}



// HELPERS






