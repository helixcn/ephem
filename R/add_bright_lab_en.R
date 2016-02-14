add_bright_lab_en <-
function( ...){
     data(bright)
     starlab <- c("Sirius", "Canopus", "Rigel-Kent", "Arcturus", "Vega", "Capella", 
                  "Rigel", "Procyon", "Achernar", "Betelgeuse", "Hadar", "Acrux", 
                  "Altair", "Aldebaran", "Spica", "Antares", "Pollux", "Fomalhaut", 
                  "Deneb", "Mimosa", "Regulus", "Adhara", "Castor")
         text(bright[,1], bright[,2] + 5, starlab, font = 8, col = grey(0.3), ...)             
}
