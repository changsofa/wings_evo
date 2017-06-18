# https://stackoverflow.com/questions/40999545/method-for-calculating-distance-between-all-points-in-a-dataframe-containing-a-l
# Load the tidyverse
install.packages("tidyverse")
library(tidyverse)

x = c(2309,2422,2497,2550,2600,2660,2700,2757,2800,2832,2865,2887,2923,2947,2968,2966,2948,2929,2905,2877,2850,2813,2773,2724,2692,2649,2619,2598,2574,2544,2521,2491,2441,2411,2386,2357,2343,2329,2321,2317,2318,2316,2320,2320,2308,2284,2285,2289,2293,2309,2306,2306,2302,2294)
y = c(2873,2782,2705,2651,2605,2551,2511,2447,2391,2344,2292,2258,2195,2133,2068,2000,1953,1929,1915,1910,1912,1917,1934,1958,1982,2011,2034,2054,2071,2094,2116,2146,2189,2230,2272,2309,2326,2379,2439,2455,2490,2498,2516,2547,2578,2602,2625,2675,2695,2732,2760,2775,2820,2845)

# x<-M2[,1]
# y<-M2[,2]

ID = 1:length(x)
df= data.frame(ID,x,y)


# Set up a fake key to join on (just a constant)
df <- df %>% mutate(k = 1) 

# Perform the join, remove the key, then create the distance
# df %>% 
#   full_join(df, by = "k") %>% 
#   mutate(dist = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)) %>%
#   select(-k)
results<- df %>% 
  full_join(df, by = "k") %>% 
  filter(ID.x != ID.y) %>%
  mutate(dist = sqrt((x.x - x.y)^2 + (y.x - y.y)^2)) %>%
  select(-k)

# save results into a matrix  
names(results)-> column.names
results <- data.frame(matrix(unlist(results), nrow=dim(results)[1], byrow=F))
colnames(results) <- column.names

## identify max distance based on axis 
# assume wing base is the last point max(results$ID.x) 
# (because the first wing point has been at the top of the wing base axis)   
last.point = max(results$ID.x)

# results[results$ID.x %in% last.point,] # only select those where starting pt is last pt
# results[results$ID.x %in% last.point,][7]  # column 7 are the calculated distances
# max(results[results$ID.x %in% last.point,][7]) # maximum of the calculated distances

maxID <- which( results$ID.x==last.point & 
                 results$dist == max(results[results$ID.x %in% last.point,][7]))
# which.max( test[,7] )
# test[which.max( test[,7] ),]
# results[17]
# names(results)

## x, y coordinates of wing base & wing tip
wl.base <- data.frame(x=results[maxID,2],y=results[maxID,3]) 
wl.tip  <- data.frame(x=results[maxID,5],y=results[maxID,6]) 


## plot and view
plot(x, y, type = "n", xlab = "Time", ylab = "Distance")
polygon(x, y, col = "gray", border = "red")
lines(c(wl.base$x,wl.tip$x), c(wl.base$y,wl.tip$y), col =3)

# points(wl.base$x,wl.base$y)
# points(wl.tip$x,wl.tip$y)
# Cool functions
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}


## Rotate by alpha angle ====
#https://stackoverflow.com/questions/15463462/rotate-graph-by-angle
M <- cbind(x,y)
#plot data
plot(M[,1],M[,2],xlim=c(1500,3500),ylim=c(2000,4000))
#Rotation angle calculated:
alpha<- atan(wl.base$y/wl.tip$x)
#Rotation matrix
rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)
#shift, rotate, shift back
M2 <- t(rotm %*% (
  t(M)-c(M[1,1],M[1,2])
)+c(M[1,1],M[1,2]))
#plot rotated wing
points(M2[,1],M2[,2],xlim=c(1500,3500),ylim=c(2000,4000), col = 2)
# check straight line based on original segment position
lines(1500:3500, rep(wl.base$y,2001)) 
# draw original wing segment
lines(c(wl.base$x,wl.tip$x), c(wl.base$y,wl.tip$y), col =3)


wl.base$y
# c(min(M2[,1])-max(M2[,1]), min(M2[,2])-max(M2[,2]))
# c(min(M[,1])-max(M[,1]), min(M[,2])-max(M[,2]))


# rotation test 2====

# coordinate transform: cartesian plane rotation
wl.base
wl.tip
pairs <-rbind(wl.base, wl.tip)

xyrot<-function(pairs,ang){
  # pairs must be Nx2 matrix w/ x in first column and y in second
  xrot <- pairs[,1]*cos(ang) - pairs[,2]*sin(ang)
  yrot <- pairs[,1]*sin(ang) + pairs[,2]*cos(ang)
  return(cbind(xrot,yrot))
}
xyrot(pairs,deg2rad(45))
xrot1 <- xyrot(pairs,deg2rad(45))[1,1]
xrot2 <- xyrot(pairs,deg2rad(45))[2,1]
yrot1 <- xyrot(pairs,deg2rad(45))[1,2]
yrot2 <- xyrot(pairs,deg2rad(45))[2,2]

xyrot(pairs,deg2rad(45))
lines(c(wl.base$x,wl.tip$x), c(wl.base$y,wl.tip$y), col =3)
lines(c(xrot1,xrot2), c(yrot1,yrot2), col =3)



## shelved for later ====
## Add more points to increase the resolution of the "max distance calculation? 
#   ## Show treatment of 'ties' :
#   
#   x <- c(2,2:4,4,4,5,5,7,7,7)
#   y <- c(1:6, 5:4, 3:1)
#   approx(x, y, xout = x)$y # warning
#   (ay <- approx(x, y, xout = x, ties = "ordered")$y)
#   stopifnot(ay == c(2,2,3,6,6,6,4,4,1,1,1))
#   approx(x, y, xout = x, ties = min)$y
#   approx(x, y, xout = x, ties = max)$y
#   
#   require(graphics)
#   
#   x <- 1:10
#   y <- rnorm(10)
#   par(mfrow = c(1,1))
#   plot(x, y, main = "approx(.) and approxfun(.)")
#   points(approx(x, y), col = 2, pch = "*")
#   points(approx(x, y, method = "constant"), col = 3, pch = "*")
#   points(x[length(x)], y[length(y)], pch="@")
#   
# f <- approxfun(x, y)
#   curve(f(x), 1900, 2900, col = "green2")
#   points(x, y)
#   is.function(fc <- approxfun(x, y, method = "const")) # TRUE
#   curve(fc(x), 0, 10, col = "darkblue", add = TRUE)
#   ## different extrapolation on left and right side :
#   plot(approxfun(x, y, rule = 2:1), 0, 11,
#        col = "tomato", add = TRUE, lty = 3, lwd = 2)
  
  