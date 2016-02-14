add_ecliptic <-
function(){
    a <- ecc(1:360,rep(0,360),1:360)
    aa = as.eqc(a)
    points((aa[,1])[-nrow(aa)], aa[,2][-(nrow(aa))], type = "l")
}
