## Usability Test (sample solution)

# dev
library("ggplot2")
library("ggforce")
library("extrafont")
devtools::load_all()

## Hello.
# In this script, you will be asked to perform several tasks on example data.
# Please insert your function calls right below each task. Use "task1",
# "task2", etc. as filenames when asked to generate a plot. The first set of
# tasks will be on the creation of facet plots, the second part on nested
# plots.

# Have a look at the first set of example data:
SMTQ
# The input of the data using excel sheets is omitted here for brevity. If you
# like to see an example on that, open "examples/example_input_excel.R"
# after(!) you are done with the tasks below.

## Task 1/10:
# Use an IPV function to generate the coordinates for a facet plot from 'SMTQ'.
coord <-
# Have a look at the coordinates.
coord

## Task 2/10:
# Use an IPV function to create the actual plot from coord.
task2 <-
# Have a look at the .pdf file you just created.

## Task 3/10:
# Call the functions again, but this time rotate the plot by 180°.
coord <-
task3 <-

## Task 4/10:
# Call the functions again, but this time add some colour.
task4 <-

## Good job, you are done with the first part!
# Now we need some data to create a nested plot, have a look at it:
self_confidence

## Task 5/10:
# Use an IPV function to generate the coordinates for a nested plot from
# 'self_confidence'.
coord <-
# Have a look at the coordinates. You might notice,
# that the coordinates for the facet plot we created earlier are included.
coord

## Task 6/10:
# Use an IPV function to create the actual plot from coord.
task6 <-
# Have a look at the .pdf file you just created.

## Task 7/10:
# Change your function calls creating a version of the plot where everything is
# about 44% bigger.
task7 <-

## Task 8/10:
# The following code creates input to add three correlation arrows to the plot.
sc_arrows <- data.frame(V1_factor=rep(NA,3),
                        V1_subfactor=rep(NA,3),
                        V2_factor=rep(NA,3),
                        V2_subfactor=rep(NA,3),
                        value=rep(NA,3))
sc_arrows[1,] <- c("DSSEI","Ab","RSES","Ps",".67")
sc_arrows[2,] <- c("DSSEI","Ab","SMTQ","Cs",".81")
sc_arrows[3,] <- c("SMTQ","Ct","RSES","Ns",".76")
# Have a look at this input.
sc_arrows
# Now create the nested plot again, but with the arrows.
coord <-
task8 <-
# Have a look at the .pdf file you just created.

## Task 9/10:
# There are several other parameters to customize your plot.
# Have a look at the .pdf from task 8 again. Is there something
# that is too small or to big in your eyes? Change its size to
# make it look better.
task9 <-
# There are even more options using the "model_" and "plot_" functions. If you
# are interested, have a short look into the documentation.
# There is also another plot not included here. If you are interested, have a
# short look into "examples/example_itemplot".

## Task 10/10:
# Before you go back to the instructions in the MS Word document, make sure that
# you added your code for each task and save the script so you can later send it
# back via e-mail.
