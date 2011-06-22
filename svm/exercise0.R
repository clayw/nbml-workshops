meshgrid = function(xgrid, ygrid)
{
   xmat = outer(xgrid, ygrid*0, FUN="+")
   ymat = outer(xgrid*0, ygrid, FUN="+")
   return(list(x=xmat, y=ymat))
}

linearboundary_example = function()
{
   #parameters of a hyperplane
   a = 0.1
   b = -0.3
   c = 0.05
   inc = 0.1

   #compute some points on the hyperplane
   x = seq(-2, 2, by=inc)
   y = -(1/b)*(a*x + c)

   #plot hyperplane (decision boundary)
   plot(x, y, type="l", col="red", lwd=3, main="Linear Decision Boundary")

   #create a grid of points surrounding the decision boundary
   xseq = x
   yseq = seq(-2, 2, by=inc)
   mgrid = meshgrid(xseq, yseq)
   xgrid = mgrid$x
   ygrid = mgrid$y
   
   #compute the margin at each point in the grid
   z = a*xgrid + b*ygrid + c

   #compute the class of each point by the sign of it's margin
   zcls = sign(z)
   cls1_index = zcls == -1
   cls2_index = zcls == 1

   #plot the grid of points by class
   points(xgrid[cls1_index], ygrid[cls1_index], pch=0)
   points(xgrid[cls2_index], ygrid[cls2_index], pch=17)
}

nonlinearboundary_example = function()
{
   #parameters of a hyperplane
   a = 0.1
   b = -0.3
   c = 0.05
   inc = 0.1

   #compute some points on the hyperplane
   x = seq(-2, 2, by=inc)
   y = -(1/b)*(a*x^2 + c)

   #plot hyperplane (decision boundary)
   plot(x, y, type="l", col="red", lwd=3, main="Nonlinear Decision Boundary")

   #create a grid of points surrounding the decision boundary
   xseq = x
   yseq = seq(-2, 2, by=inc)
   mgrid = meshgrid(xseq, yseq)
   xgrid = mgrid$x
   ygrid = mgrid$y
   
   #compute the margin at each point in the grid
   z = a*xgrid^2 + b*ygrid + c

   #compute the class of each point by the sign of it's margin
   zcls = sign(z)
   cls1_index = zcls == -1
   cls2_index = zcls == 1

   #plot the grid of points by class
   points(xgrid[cls1_index], ygrid[cls1_index], pch=0)
   points(xgrid[cls2_index], ygrid[cls2_index], pch=17)
}



linearboundary_example()

X11()

nonlinearboundary_example()

