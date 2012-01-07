
Manipulating Matrices in R




A <- cbind(x1,x2,x3)    ### bind vectors x1, x2, and x3 into a matrix; 
			### treat each as a column

A <- rbind(x1,x2,x3)    ### bind vectors x1, x2, and x3 into a matrix; 
			### treat each as a row

A <- matrix(x,4,5)      ### change vector x 
			### into a 4 by 5 matrix

dim(A)         ### get the dimensions of a matrix
nrow(A)        ### number of rows
ncol(A)        ### number of columns
A %*% B        ### multiple matrices
t(A)           ### transpose of A
sum(diag(A))   ### trace of A
solve(A)       ### inverse of A
det(A)         ### determinant of A
help(array)    ### get more information
### The following function will compute the square root of A:
sqrt.fun <- function(A){
     e      <- eigen(A,symmetric=TRUE)
     sqrt.A <- e$vectors %*% diag(sqrt(e$values)) %*% t(e$vectors)
     return(sqrt.A)
     }
