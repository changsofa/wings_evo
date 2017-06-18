
# Load the tidyverse
install.packages("tidyverse")
library(tidyverse)

## Enter data ====
x = c(2309,2422,2497,2550,2600,2660,2700,2757,2800,2832,2865,2887,2923,2947,2968,2966,2948,2929,2905,2877,2850,2813,2773,2724,2692,2649,2619,2598,2574,2544,2521,2491,2441,2411,2386,2357,2343,2329,2321,2317,2318,2316,2320,2320,2308,2284,2285,2289,2293,2309,2306,2306,2302,2294)
y = c(2873,2782,2705,2651,2605,2551,2511,2447,2391,2344,2292,2258,2195,2133,2068,2000,1953,1929,1915,1910,1912,1917,1934,1958,1982,2011,2034,2054,2071,2094,2116,2146,2189,2230,2272,2309,2326,2379,2439,2455,2490,2498,2516,2547,2578,2602,2625,2675,2695,2732,2760,2775,2820,2845)

# x<-M2[,1]
# y<-M2[,2]

## Calculate maximum inner distance from wing base ====
# https://stackoverflow.com/questions/40999545/method-for-calculating-distance-between-all-points-in-a-dataframe-containing-a-l
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

## identify max distance starting from the wing base
# Wing base is the last point clicked, because the first point is at the leading edge of the wing. The last point would be immediately before the leading edge, near the wing base.   
wingbase_ID = max(results$ID.x)

# results[results$ID.x %in% wingbase_ID,] # only select those where starting pt is last pt
# results[results$ID.x %in% wingbase_ID,][7]  # column 7 are the calculated distances
# max(results[results$ID.x %in% wingbase_ID,][7]) # maximum of the calculated distances

maxID <- which( results$ID.x==wingbase_ID & 
                 results$dist == max(results[results$ID.x %in% wingbase_ID,][7]))

## x, y coordinates of wing base & wing tip
wing_base <- data.frame(x=results[maxID,2],y=results[maxID,3]) 
wing_tip  <- data.frame(x=results[maxID,5],y=results[maxID,6]) 


## plot and view imported shape ====
plot(x, y, type = "n", xlab = "Time", ylab = "Distance")
polygon(x, y, col = "gray", border = "red")
lines(c(wing_base$x,wing_tip$x), c(wing_base$y,wing_tip$y), col =3)

# points(wing_base$x,wing_base$y)
# points(wing_tip$x,wing_tip$y)
# Cool functions
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}


## Rotate shape by alpha angle to make shape horizontal ====
#https://stackoverflow.com/questions/15463462/rotate-graph-by-angle
M <- cbind(x,y)
#plot data
plot(M[,1],M[,2],xlim=c(1500,3500),ylim=c(2000,4000))
#Rotation angle calculated:
alpha<- atan(wing_base$y/wing_tip$x)
#Rotation matrix
rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)
#shift, rotate, shift back
M2 <- t(rotm %*% (
  t(M)-c(M[1,1],M[1,2])
)+c(M[1,1],M[1,2]))
#plot rotated wing
points(M2[,1],M2[,2],xlim=c(1500,3500),ylim=c(2000,4000), col = 2)
# check straight line based on original segment position
lines(1500:3500, rep(wing_base$y,2001)) 
# draw original wing segment
lines(c(wing_base$x,wing_tip$x), c(wing_base$y,wing_tip$y), col =3)


  