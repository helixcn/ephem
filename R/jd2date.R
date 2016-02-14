jd2date <-
function(jd){
    jdate2vect <- function(xxx){
        year = xxx[[1]]
        month = xxx[[2]]
        day <- xxx[[3]]
        frac <- xxx[[6]]/(60*60*24)+xxx[[5]]/(60*24)+xxx[[4]]/24
        day.dec <- day + frac
        res <- c(year, month, day.dec)
        names(res) <- c("Year","Month","Day")
        return(res)
    }
 
   D = floor(jd+0.5)
   F= jd+0.5-D
   if(D>=2299161) {
      c = floor((D-1867216.25)/36524.25)
      D = D + 1+c-floor(c/4);
   }
   D = D + 1524;              
   r.Y = floor((D-122.1)/365.25);
   D = D - floor(365.25*r.Y);  
   r.M = floor(D/30.601);
   D = D- floor(30.601*r.M);  
   r.D = D;
   if(r.M>13){
      r.M=r.M - 13
      r.Y=r.Y  - 4715;
   }
   else{
      r.M =r.M - 1
      r.Y = r.Y- 4716
   }
   F= F*24;
   r.h=floor(F);
   F= F -r.h;
   F = F * 60;
   r.m=floor(F);
   F= F - r.m;
   F = F * 60;
   r.s=F;
   res <- list(r.Y, r.M, r.D, r.h, r.m, round(r.s,2))
   names(res) <- c("Year","Month","Day","Hour","Minute","Second")
   return(paste(unlist(jdate2vect(res)), collapse = "-"));
}
