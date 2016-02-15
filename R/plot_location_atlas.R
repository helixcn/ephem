plot_location_atlas <-
function(JD){
    data(resstar)
    background()
    add_ecliptic()
    add_stars(mag = 5, pch = 19, times = 1, col = "white")
    add_bright_lab_en(cex = 1)
    add_planet(JD, cex = c(3.5, 3.5, 1, 2,1.8,3,2.8,2.4, 2.4, 1.2), 
               col = c("#FFEC8B","#FFFF00", "#F0FFFF","#FFF68F","#FF7256", 
               "#FFD39B","#CD950C","#F0F8FF", "#98F5FF","#FFF8DC"), 
               pch = 19, labels = TRUE)
    
    add_planet(JD, cex = c(3.5, 3.5, 1, 2,1.8,3,2.8,2.4, 2.4, 1.2), 
               col = "black", pch = 21, labels = FALSE)
    
    title(paste("", "SKY ATLAS FOR ", jd2date(JD)," (epoch 2000.0)" ))
}
