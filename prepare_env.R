# To use Keras in R, make sure you have python environment on you PC

# First, start with installing reticulate - to communicate with Python
install.packages("reticulate")
# Installing miniconda allows to run keras flawlessly - but you can also use other already-present virtual environments
reticulate::install_miniconda()
# Now install keras
install.packages("keras")
# We have miniconda, but not tensorflow - install it with install_tensorflow() function (you can specify cpu/gpu version)
tensorflow::install_tensorflow(version="2.8-cpu")

# Now the rest of necessary packages 
install.packages('plotly')
install.packages("DALEX")
install.packages("dotenv")
# Gr8, we're good to go! :)
