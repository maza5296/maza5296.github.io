## Design Studio Feedback
*Tuesday, April 14th*

Team 1: Politivis - Amanda Boss, Hillary Do, Takehiro Matsuzawa 

Team 2: Flight data - Nhu Nguyen, Zeno Ziemke, Masahiro Kusunoki

**Feedback 1**

Filtering the numbers of routes that are shown on the map is an Feedback. The map gets too crowded with links, which makes it hard to understand.
> Yes, one way to do this will be to add plenty of methods for filtering so the user can choose just the right number to visualize. Another way we may do this, is to aggregate routes and represent the quantities by the thickness of the lines. However, this can be reduction because it will not allow us to show the number of flights per route that each airline offers, which is important if we are going to be able to make a visualization where people can compare how different airlines compare with each other.


**Feedback 2**

How do you want to arrange the different visualizations on one page? If the user needs to scroll a lot this might be bad.
> Yes, we will need to put some thought into that. The chart for airlines may become very long and so might the line chart if we are also going to add brushing features. We will have to do this once we finish all of the visualizations and fine tune (by perhaps listing only the top 5 for example)

**Feedback 3**

If arcs are connecting points that are close together, the arcs are unnecessary long after zooming in.
<p align="center">
	<img src="img/arc_curve.png" width="300"/>
</p>
 
> We have not figured a fix for this yet. One way might be to re-render the lines accordingly when you zoom so that they take into account the screen size. Another way could be that we render only domestic flights when we zoom and then the lines will not be as long 


**Feedback 4**

You could use color as a design variable on the map. For example countries could be colored according to how well they are connected with other countries (numbers of routes, passengers,..) or you could cluster countries by color to show which countries are closely connected to each other.
> Yes, that would be a very good next step for this project. 
We could shade the different destination countries in different colors to show which are the countries that are the best connected to the selected country. We are also thinking of creating a search feature where you can click two countries and search the prices and times of the flights. In that case, we will use different color schemes to show start location, destination and transit location.
