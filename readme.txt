Hi,

Here are some notes to help you navigate my R code.

1. You should download the zip file from github and then run the R script. 
I have not used file paths and relied on the datasets and the R script being in the same folder.

2. One visualisation's legend is hidden because it shares its legend with another similar visualisation. 
Some other visualisations don't need a legend.

3. Each visualisation loads its data separately. 
You may find this odd, but it was useful to start from a clean slate everytime.

4. In case you want to download the images to your local drive, te ggsave command is commented out.
Please change the path before running it.

5. Many thresholds used to subset data are arbitrary, in order to keep the number of data-points "just right".

6. The code loads fonts and uses Avenir. In case you don't have Avenir on your local drive, 
please replce Avenir with Arial, or something else.
