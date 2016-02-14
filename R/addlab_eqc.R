addlab_eqc <-
function(x){
    points(x[,1], x[,2], pch = 19, col = 2, cex = 1.2)
    text(x[,1], x[,2] + 5, substring(row.names(x), 6,10))
}
