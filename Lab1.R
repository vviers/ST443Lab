# setwd() if not working from within an R project

# Lab1 basic commands

x <- seq(-pi, pi, length = 50)
y = x
# f is a 50x50 matrix which is the result of applying function to (x, y)
f = outer(x, y, function(x, y) cos(y) / (1 + x^2))

fo = outer(x, y) #is an outerproduct (by default, function is %o%)

contour(x, y, f)
contour(x, y, f, nlevels = 15)

persp(x, y, f) #3D plot
persp(x, y, f, theta = 45, phi = 0) #change viewpoint


# read data in
auto <- read.csv("auto_mpg.csv")
str(auto)

# when reading data in, always make sure that you fixed na.strings etc. (look at $Horsepower)
auto <- read.csv("auto_mpg.csv", na.strings = "?")
str(auto) # better :)
dim(auto)

auto <- na.omit(auto) # remove NAs (5 of them)
dim(auto)


# attach() means you don't have to access columns with `$`
mean(auto$Mpg)
attach(auto)
mean(Mpg)
detach(auto)

attach(auto)

# Lab 1 Additional Graphical and Numerical Summaries
Cylinders=as.factor(Cylinders)
plot(Cylinders,Mpg)
plot(Cylinders,Mpg,col="red")
plot(Cylinders,Mpg,col="red",varwidth=T)
plot(Cylinders,Mpg,col="red",varwidth=T,horizontal=T)
plot(Cylinders,Mpg,col="red",varwidth=T,xlab="Cylinders",ylab="MPG")
hist(Mpg)
hist(Mpg,col=2)
hist(Mpg,col=2,breaks=15)

# pairwise plots
pairs(auto)
pairs(~Mpg+Displacement+Horsepower+Weight+Acceleration, data = auto, col = 2)


plot(Horsepower,Mpg)
identify(Horsepower,Mpg,Name) #can interact with plot (click on all points you want to ID)