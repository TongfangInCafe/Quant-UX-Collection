#Inspired by @timelyportfolio - All My Roads Lead Back to Finance-PIMCO Sankey
#http://timelyportfolio.blogspot.co.uk/2013/07/all-my-roads-lead-back-to-financepimco.html

#Now let's create a Sankey diagram - we need to install RCharts
##http://ramnathv.github.io/rCharts/
require(rCharts)

#Download and unzip @timelyportfolio's Sankey/rCharts package
#Take note of where you put it!
#https://github.com/timelyportfolio/rCharts_d3_sankey

sankeyPlot <- rCharts$new()

#We need to tell R where the Sankey library is.
#I put it as a subdirectory to my current working directory (.)
sankeyPlot$setLib('./rCharts_d3_sankey-gh-pages/')

#We also need to point to an HTML template page
sankeyPlot$setTemplate(script = "./rCharts_d3_sankey-gh-pages/layouts/chart.html")


#The plotting routines require column names to be specified as:
##source, target, value
#to show what connects to what and by what thickness line

#If we want to plot from enduse to energytype we need this relabelling
workingdata=DECC.overall.energy
colnames(workingdata)=c('Sector','source','target','value')


sankeyPlot$set(
  data = workingdata,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 750,
  height = 500,
  labelFormat = ".1%"
)

sankeyPlot