## Usability Test (sample solution)

# dev
devtools::load_all()


# In this script, you will be asked to perform several tasks on example data.
# Please insert your function calls right below each task.
# Use "task1", "task2", etc. as filenames when asked to generate a plot.
# The first set of tasks will be on the creation of facet plots, the second part on nested plots.

## Task 0:
# Have a look at the first set of example data:
SMTQ
# The input of the data using excel sheets is omitted here for brevity.
# If you would like to see an example on that, open "examples/example_input_excel.R"
# after you are done with the tasks below.

## Task 1:
# Use an IPV function to generate the coordinates for a facet plot from 'SMTQ'.
coord <- model_facets(SMTQ,subradius = .5)
# Have a look at the coordinates.
coord

## Task 2:
# Use an IPV function to create the actual plot from coord.
task2 <- plot_facets(coord,filename = "task2")
# Have a look at the .pdf file you just created.

## Task 3:
# Change your function calls creating a version of the plot that is rotated by 180°.
coord <- model_facets(SMTQ,subradius = .5,rotate = pi)
task3 <- plot_facets(coord,filename = "task3")

## Task 4:
# Change your function calls creating a version of the plot using your favorite colour.
task4 <- plot_facets(coord,filename = "task4",colour = "red")

## Good job, you are done with the first part!
# Now we need some data to create a nested plot, have a look at it:
self_confidence

## Task 5:
# Use an IPV function to generate the coordinates for a nested plot from 'self_confidence'.
coord <-
