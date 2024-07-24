shinyUI(pageWithSidebar(
  headerPanel("AP Tests by Region (2012-2022)"),
  sidebarPanel(
    textInput('startdate', 'Starting Year', 2012),
    textInput('enddate', 'Ending Year', 2022),
    radioButtons('maptype', 'Color Fill', c('Count', 'Percent'), 'Count'),
    submitButton('Create Map')
  ),
  mainPanel(
    textOutput('error'),
    uiOutput('mymap'),
    uiOutput('table')
  )
))