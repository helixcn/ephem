Julian2Date <-
function(jd){
    int2 <-
    function (v) {
       return (floor(v));
    } 
        r = list();
       D = int2(jd+0.5);
       F= jd+0.5-D 
       if(D >= 2299161) {
            c = int2((D-1867216.25)/36524.25);
            D = D + 1 + c - int2(c/4);
        }
       D  = D + 1524;              
       r$Y = int2((D - 122.1)/365.25);
      
       D  = D - int2(365.25*r$Y);  
       r$M = int2(D/30.601);
       D  = D - int2(30.601*r$M);
       r$D = D;
       if(r$M>13) {
           r$M = r$M - 13;
           r$Y = r$Y - 4715;
        } else {
           r$M = r$M -  1;
           r$Y = r$Y - 4716;
        }
       F = F * 24;
       r$h=int2(F);
       F = F - r$h;
       F = F * 60;
       r$m=int2(F);
       F = F -r$m;
       F = F * 60;
       r$s=F;
       return(r);
    }
