library(shiny)
library(bslib)
library(igraph)

# Create the graph
g <- make_graph(~ "Greg"--"From", "From"--"Born in", "From"--"Raised in", "Raised in"--"Lake Geneva, WI", "Born in"--"Chicago, IL", "Greg"--"Hobbies", "Hobbies"--"Outdoors", "Hobbies"--"Guitar", "Outdoors"--"Hiking", "Outdoors"--"Swimming", "Outdoors"--"Kayaking", "Lake Geneva, WI"--"Swimming", "Lake Geneva, WI"--"Kayaking", "Hobbies"--"Interests", "Interests"--"Econ", "Interests"--"AI", "Interests"--"ML", "Interests"--"Data Viz.", "AI"--"ML", "Greg"--"Education", "Education"--"College", "College"--"UWW", "UWW"--"Econ", "UWW"--"Math", "Econ"--"Math", "Math"--"ML", "UWW"--"Clubs", "Clubs"--"Econ Society", "Clubs"--"Women in Econ", "Clubs"--"Fin. Mgmt.", "Clubs"--"Investment Group", "Econ"--"Econ Society", "Econ"--"Women in Econ", "Education"--"Bootcamps", "Bootcamps"--"The Data Incubator", "Bootcamps"--"Revature", "The Data Incubator"--"ML", "The Data Incubator"--"Data Viz.", "Greg"--"Career", "Career"--"FERC", "Career"--"CareFree Enzymes", "Career"--"FIS", "FERC"--"Econ", "FERC"--"Econometrics", "FERC"--"Data Viz.", "Econ"--"Econometrics", "Math"--"Econometrics", "CareFree Enzymes"--"SEO", "FIS"--"ML", "FIS"--"Data Viz.", "FIS"--"Cloud", "Kayaking"--"Swimming") %>% set_vertex_attr("class", value = c("Greg", "Location", "Location", "Location", "Location", "Location", "Hobbies" ,"Hobbies", "Hobbies", "Hobbies", "Hobbies", "Hobbies", "Hobbies", "Skills", "Skills", "Skills", "Skills", "Education", "Education", "Education", "Skills", "Education", "Education", "Education", "Education", "Education", "Education", "Education", "Education", "Career", "Career", "Career", "Career", "Skills", "Skills", "Skills"))
#g
#summary(g)
#as_adjacency_matrix(g)


# filtering

# remove class Career
vertices_to_remove <- which(V(g)$class == "Career")
g <- delete_vertices(g, vertices_to_remove)

# remove class Education
vertices_to_remove <- which(V(g)$class == "Education")
g <- delete_vertices(g, vertices_to_remove)

# Remove nodes
g <- delete.vertices(g, V(g)$name == "SEO")
g <- delete.vertices(g, V(g)$name == "Cloud")


# plotting

# layouts
#layout <- layout_nicely(g)
#layout <- layout_as_tree(g)
#layout <- layout_with_fr(g)
layout <- layout_with_kk(g)

# custom color palette
custom_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FF69B4")


# UI
ui <- page_sidebar(
  title = "About Me Network Graph",
  plotOutput(outputId = "graph_plot")
)

# Server
server <- function(input, output) {
  
  output$graph_plot <- renderPlot({
    
    # set background color to black
    par(bg="black")
    
    # plot graph
    plot(
      g,
      layout=layout,
      #vertex.color = as.factor(V(g)$class),
      vertex.color = custom_palette[as.numeric(factor(V(g)$class))],
      vertex.label.color = "white",
      vertex.label.dist = 0,
      vertex.label.family = "sans",  # Font family
      edge.lty = "solid",
      edge.width = 4,
      edge.curved = 0.3
    )
  })
}


# run Shiny App
shinyApp(ui = ui, server = server)
