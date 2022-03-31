masstools::setwd_project()

rm(list = ls())

source("code/pseudoMS-image-converter/convert2imageR.R")

mz.range = c(70, 1000)
rt.range = c(50, 1000)
mz.pixel = 1024
rt.pixel = 1024
noise.threshold = 500
output.path = "."
threads = 4
mz.shift = FALSE
rt.shift = FALSE
int.shift = FALSE

setwd("demo_data/")
file.name <- "QCP11.mzXML"

system.time(
  convert2image(
    file.name = file.name,
    mz.range = mz.range,
    rt.range = rt.range,
    mz.pixel = mz.pixel,
    rt.pixel = rt.pixel,
    noise.threshold = noise.threshold,
    output.path = output.path,
    threads = threads,
    mz.shift = mz.shift,
    rt.shift = rt.shift,
    int.shift = int.shift
  )
)
