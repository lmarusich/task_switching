#Visual angle for viewing distance and screen size 
#Source: http://stephenrho.github.io/visual-angle.html

visAngle <- function(size, distance){
  # this function calculates visual angle
  # size and distance must be in the same units
  Rad = 2*atan(size/(2*distance))
  Ang = Rad*(180/pi)
  return(Ang)
}

visAngle(size = 1, distance = 57) # why people use 57 cm as viewing distance

desiredSize <- function(visAngle, distance){
  # this function gives desired size of stimulus
  # given visual angle and distance
  # size returned is in same units and distance
  Rad = visAngle/(180/pi) # or pi*(visAngle/180)
  size = 2*distance*tan(Rad/2)
  return(size)
}


#Smith et al. 2019
#Exp 1: 
#Viewing distance = 76.2 cm 
#15" monitor (diagonal) 
#Fixation cross = 1.13 degrees
#Block letters = 1.13 degrees width and 2.26 height

#Exp 2: 
#Viewing distance = 57 cm 
#Unknown size Monitor 
#Stim = 12 degrees
#Box/outline = 25 degrees

#Stim size
stim.size.cm <- desiredSize(12, 57)
stim.size.cm

stim.size.in <- stim.size.cm/2.54
stim.size.in

#Box/outline size
#cm
box.size.cm <- stim.size.cm*(25/12)
box.size.cm

#in
box.size.in <- box.size.cm/2.54
box.size.in 

#TAMUC setup
#12 deg vis ang and 113 cm viewing dist
  #23.75 cm stim
    stim_size_cm <- desiredSize(12, 113) 
    stim_size_cm

  #9.35 inches
    stim_size_cm/2.54

#25 deg vis angle
  #50.10 cm box/outline 
    box_size_cm <- desiredSize(25, 113)
    box_size_cm

  #19.72 inches
    box_size_cm/2.54

#For a monitor 21.5" diag
factor_in <- 21.5/sqrt(16^2 + 9^2) #16 + 9 for the aspect ratio factor

unitsx_in <- factor_in*16
unitsx_in

unitsy_in <- factor_in*9
unitsy_in

  #Sanity check
  (unitsx_in^2 + unitsy_in^2)
  (21.5)^2

#40" monitor
factor_in2 <- 40/sqrt(16^2 + 9^2) #16 + 9 for the aspect ratio factor

unitsx_in2 <- factor_in2*16
unitsx_in2

unitsy_in2 <- factor_in2*9
unitsy_in2

#Sanity check
(unitsx_in2^2 + unitsy_in2^2)
(40)^2


#Exp 3: 
#Viewing distance = 90 cm viewing dist
#18" monitor  (diagonal) 
#Since this was a CRT monitor, it's almost certainly not 16:9. Probably 5:4     
#Fixation cross = 1.13 degrees
#Search area = 25 degrees (width) x 16 degrees (height)
#Letters = 1.13 degrees width and 2.26 height

#Fixation
#1.77 cm, ~0.70 inches
    desiredSize(1.13, 90)

#Search area  
#40 cm width, ~15 inches 
  desiredSize(25, 90)

#25 cm height, ~9.96 inches 
  desiredSize(16, 90)

  
    