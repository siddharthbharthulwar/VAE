library(keras)
K <- keras::backend()

mnist <- dataset_mnist()

###Assign training/validation and test datasets + scale to (0, 1) 
train_val_raw <- mnist$train$x / 255
test_raw <- mnist$test$x / 255

###Separate training and validation
train_raw <- train_val_raw[1:50000, , ]
val_raw <- train_val_raw[50001:60000, , ]

resize <- function(img, new_width = 12, new_height = 12) {
  new_img <- apply(img, 2, function(y) {return(spline(y, n = new_height)$y)})
  new_img <- t(apply(new_img, 1, function(y) {return(spline(y, n = new_width)$y)}))
  new_img[new_img < 0] <- 0
  return(new_img)
}

train_resized <- array(dim = c(dim(train_raw)[1], 12, 12))
val_resized <- array(dim = c(dim(val_raw)[1], 12, 12))
test_resized <- array(dim = c(dim(test_raw)[1], 12, 12))

###Resize the images
for (s in 1:dim(train_raw)[1]) train_resized[s, , ] <- resize(train_raw[s, , ])
for (s in 1:dim(val_raw)[1]) val_resized[s, , ] <- resize(val_raw[s, , ])
for (s in 1:dim(test_raw)[1]) test_resized[s, , ] <- resize(test_raw[s, , ])

###Resizing changed scale, so rescale
train_resized_scale <- train_resized / max(train_resized)
val_resized_scale <- val_resized / max(val_resized)
test_resized_scale <- test_resized / max(test_resized)

x_train <- array(dim = c(dim(train_resized_scale)[1], 12, 12, 1))
x_val <- array(dim = c(dim(val_resized_scale)[1], 12, 12, 1))
x_test <- array(dim = c(dim(test_resized_scale)[1], 12, 12, 1))

###Fill keras data objects
x_train[ , , , 1] <- train_resized_scale
x_val[ , , , 1] <- val_resized_scale
x_test[ , , , 1] <- test_resized_scale

par(mfcol = c(1, 2))
plot(as.raster(train_raw[1, , ]))
plot(as.raster(train_resized_scale[1, , ]))


