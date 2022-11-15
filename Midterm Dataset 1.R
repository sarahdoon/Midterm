# Number 1
# put train file in variable
data <- read.csv("/Users/sarahferaidoon/Desktop/train.csv.gz")
data
# Length(data[,1]) = 42000
length(data[,1])

# make data frame
pixels <- data.frame(data[,-1])
pixels
# use smoothScatter
smoothScatter(pixels)
pixels

# make a matrix and remove the label, first column, so we only have pixels
label1<-pixels[1,]
mode(label1)
mode(label1) = "numeric"
label1
# make a matrix
my_label1 <- matrix(label1, nrow=28, byrow=TRUE)
par(mar = c(5,5,5,5))
# use image to plot it out
image(my_label1, axes = FALSE, col = grey.colors(256, start=0, end=1))
my_label1      
matplot((my_label1),type="l")

# make a for-loop
r <- c(1:length(pixels[,1]))
for (x in r){
  label1 <- pixels[x,]
  mode(label1) = "numeric"
  my_label1 <- matrix(label1, nrow=28, byrow=TRUE)
  par(mar=c(5,5,5,5))
  image(my_label1, axes = FALSE, col = grey.colors(256, start=0, end=1))
  my_label1
}

# Number 2
# everyone who writes a certain number will be put into a certain variable
# 0 into label_0
# take the average, after we do this and see if it still looks like its number
n <- c(0:9)

for (x in n){
  assign(paste0("label_",x),data[grep(x,data[,1]),])
}
# label is the number and the pixels of where the data is drawn
# 0 is black all the way to 256 (white) and shades of grey in between
label_0
label_1
label_2
label_3
label_4
label_5
label_6
label_7
label_8
label_9

# Problem 2a
# take all the data that has label handwriting as 0 and remove the first column 
# so there are only pixels that represent 0
pixel_label_0 <- label_0[-1]
length(pixel_label_0) # -> 784
# make a variable to keep track of every number from 1 to max length of columns 
c <- c(1:length(pixel_label_0))
# make an empty list to append the average values for each column to
ave_pix <- c()
# make a for loop
# loop through c with x
# make x the column in pixel_label_0 variable
for (x in c){
  # look at column, take mean of entire column, append the mean to the ave_pix variable
  ave_pix <- append(ave_pix, mean(pixel_label_0[,x]))
}
# make a matric with means from ave-pix
# each entry represents the mean number for each column in the dataset labeled 0
matrix_ave_pix <- matrix(ave_pix, nrow=28, ncol=28)
# plot the matrix with image function
# make the colors shades of grey
image(matrix_ave_pix, axes=FALSE, col=grey.colors(256,start=0,end=1))

# Repeat for "1" digit
pixel_label_1 <- label_1[-1]
length(pixel_label_1) # -> 784
c <- c(1:length(pixel_label_1))
ave_pix_1 <- c()
for (x in c){
  ave_pix_1 <- append(ave_pix_1, mean(pixel_label_1[,x]))
}
matrix_ave_pix_1 <- matrix(ave_pix_1, nrow=28, ncol=28)
image(matrix_ave_pix_1, axes=FALSE, col=grey.colors(256,start=0,end=1))

# Make a Function
myfunction <- function(r){
  pixel_label_0 <- r[,-1]
  c <- c(1:length(pixel_label_0))
  ave_pix <- c()
  for (x in c) {
    ave_pix <- append(ave_pix, mean(pixel_label_0[,x]))
  }
  matrix_ave_pix <- matrix(ave_pix, nrow=28, ncol=28)
  image(matrix_ave_pix, axes=FALSE, col=grey.colors(256,start=0,end=1))
}
myfunction(label_0)
myfunction(label_1)
myfunction(label_2)
myfunction(label_3)
myfunction(label_4)
myfunction(label_5)
myfunction(label_6)
myfunction(label_7)
myfunction(label_8)
myfunction(label_9)
# 0, 3, and 8 look the best because they are both distinguishable and in the right orientation
# 1 and 9 look the most blurry

# Correct the orientation
for (x in r){ # r represents the current row in the dataframe pixels
  label1 <- pixels[x,] # pass entire row into variable
  mode(label1) = "numeric" # change mode of number so it can be imaged
  my_label1 <- matrix(rev(label1), nrow=28, byrow=FALSE) # make that row into a 28x28 matrix, reverse the list of pixels
  my_label1 <- apply(my_label1,2,rev) # reverse the matrix with apply
  par(mar=c(5,5,5,5))
  image(my_label1,axes=FALSE, col=grey.colors(256,start=0,end=1)) # plot points in matrix with image
  if (x==10){break} # just see the first 10 rows
}

install.packages("raster")

# Problem 3
# 3a
# make a function
# c will keep track of the column numbered 2-783
# make empty variable var_of_col to pass all of the variance results to
func_each_label <- function(L,k){
  length(L)
  c <- c(2:length(L))
  var_of_col <- c()
  for (x in c){
    # in each column calculate variance of entire column and append that
    var_of_col <- append(var_of_col, var(L[,x]))
  }
  max(var_of_col)
  cat("the column number for ", k, "that has the highest variance is: ", which(var_of_col==max(var_of_col))+1, " and the variance is ", print(max(var_of_col)), "\n")
  # didn't include first column so +1
}
func_each_label(label_0, "label_0") 
func_each_label(label_1, "label_1") 
func_each_label(label_2, "label_2") 
func_each_label(label_3, "label_3")
func_each_label(label_4, "label_4")
func_each_label(label_5, "label_5")
func_each_label(label_6, "label_6")
func_each_label(label_7, "label_7")
func_each_label(label_8, "label_8")
func_each_label(label_9, "label_9")


# Problem 4
library(raster)
for (n in x){
  k <- paste0("hand_writing_",n,".png")
  im <- load.image(k)
  # im <- load.image(file)
  im.r <- as.raster(im,interpolate=F)
  str(im.r)
  plot(im.r)
}
dev.off()
