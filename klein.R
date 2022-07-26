# La botella de Klein con R
# www.overfitting.net
# https://www.overfitting.net/2018/04/la-botella-de-klein-con-r.html

library(rgl)



getColor <- function(v) {
  if(v == 1) {
    return(1)
  } else {
    return(2)
  }
}

conway_nextState <- function(M_start) {
  M = matrix(0,nrow(M_start),ncol(M_start))
  
  for(x in (1:ncol(M_start))) {
    for(y in (1:nrow(M_start))) {
      neighbourCount = 0;
      
      for(dx in (-1:1)) {
        for(dy in (-1:1)) {
          if(dx!=0 || dy!=0) {
            if(conway_read(M_start,x+dx,y+dy) == 1) {
              neighbourCount = neighbourCount + 1
            }
          }
        }
      }
      
      if(M_start[y,x] == 1) {
        if(neighbourCount == 2 || neighbourCount == 3) {
          M[y,x] = 1
        }
      } else {
        if(neighbourCount == 3) {
          M[y,x] = 1
        }
      }
    }
  }
  
  return(M)
}

conway_read <- function(M, x, y) {
  realX = x
  realY = y
  
  if(x == 0) {
    realX = ncol(M)
  }
  
  if(x == ncol(M)+1) {
    realX = 1
  }
    
  if(y == 0) {
    realY = nrow(M)
    realX = (-x+8) %% ncol(M) + 1
  }
  
  if(y == nrow(M)+1) {
    realY = 1
    realX = (-x+8) %% ncol(M) + 1
  }
  
  return(M[realY,realX])
}

# Botella de Klein (parametrización de Paul Chang)
kleinbottle3d=function(M,n){ # 16 x 26
  x = 0
  y = 0
  z = 0
  w = 16
  bottom_h = 4
  body_h = 8
  top_h = 6
  handle_h = 8
  
  for (xi in 0:(w-1)) {
    for (yi in 0:(top_h-1)) {
      kleinbottle3d.top(x, y, z, n, col=getColor(M[yi+1,xi+1]), alpha=0.5, w=w, xi=xi, h=top_h, yi=top_h-yi-1)
    }
    for (yi in 0:(handle_h-1)) {
      kleinbottle3d.handle(x, y, z, n, col=getColor(M[top_h+yi+1,xi+1]), alpha=0.5, w=w, xi=-xi+3/16*w, h=handle_h, yi=handle_h-yi-1)
    }
    for (yi in 0:(bottom_h-1)) {
      kleinbottle3d.bottom(x, y, z, n, col=getColor(M[top_h+handle_h+yi+1,xi+1]), alpha=0.5, w=w, xi=xi, h=bottom_h, yi=bottom_h-yi-1)
    }
    for (yi in 0:(body_h-1)) {
      kleinbottle3d.body(x, y, z, n, col=getColor(M[top_h+handle_h+bottom_h+yi+1,xi+1]), alpha=0.5, w=w, xi=xi, h=body_h, yi=yi)
    }
  }
  
  # kleinbottle3d.body(x, y, z, n, ...)
  # kleinbottle3d.top(x, y, z, n, ...)
  # kleinbottle3d.handle(x, y, z, n, ...)
}

kleinbottle3d.bottom=function(x=0, y=0, z=0, n=101, w=1, h=1, xi=0, yi=0, ...){
  f=function(s, t) cbind((2.5+1.5*cos(s))*cos(t) + x,
                         (2.5+1.5*cos(s))*sin(t) + y,
                         -2.5    *sin(s)         + z)
  persp3d(f, slim=c((yi/h)*pi, ((yi+1)/h)*pi), tlim=c((xi/w)*2*pi, ((xi+1)/w)*2*pi), n=n, add=T, ...)
}

kleinbottle3d.body=function(x=0, y=0, z=0, n=101, w=1, h=1, xi=0, yi=0, ...){
  f=function(s, t) cbind((2.5+1.5*cos(s))*cos(t) + x,
                         (2.5+1.5*cos(s))*sin(t) + y,
                         3*s                     + z)
  persp3d(f, slim=c((yi/h)*pi, ((yi+1)/h)*pi), tlim=c((xi/w)*2*pi, ((xi+1)/w)*2*pi), n=n, add=T, ...)
}

kleinbottle3d.top=function(x=0, y=0, z=0, n=101, w=1, h=1, xi=0, yi=0, ...){
  f=function(s, t) cbind(2+(2+cos(t))*cos(s)    + x,
                         sin(t)                 + y,
                         3*pi+(2+cos(t))*sin(s) + z)
  persp3d(f, slim=c((yi/h)*pi, ((yi+1)/h)*pi), tlim=c((xi/w)*2*pi, ((xi+1)/w)*2*pi), n=n, add=T, ...)
}

kleinbottle3d.handle=function(x=0, y=0, z=0, n=101, w=1, h=1, xi=0, yi=0, ...){
  f=function(s, t) cbind(2-2*cos(s)+sin(t) + x,
                         cos(t)            + y,
                         3*s               + z)
  persp3d(f, slim=c((yi/h)*pi, ((yi+1)/h)*pi), tlim=c((xi/w)*2*pi, ((xi+1)/w)*2*pi), n=n, add=T, ...)
}

iif = function(condicion, val1, val2) {
  if (condicion) return(val1)
  return(val2)
}


# EJEMPLOS

# Botella de Klein
drawKlein <- function(M,n,startIndex,endIndex) {
  open3d()
  bg3d(color="lightblue")
  view3d(theta=0, phi=0)
  um=par3d()$userMatrix
  um=rotate3d(um, -5*pi/8, 1, 0, 0)
  par3d(FOV=0, zoom=0.75, userMatrix=um, windowRect=c(10,10,500,800))
  
  M2 = M
  
  if(startIndex > 1) {
    for(i in (1:(startIndex-1))) {
      M2 = conway_nextState(M2)
    }
  }
  
  for(i in (startIndex:endIndex)) {
    kleinbottle3d(M2,n)
    M2 = conway_nextState(M2)
    snapshot3d(paste0("klein", iif(i<10, "00", iif(i<100, "0", "")), i, ".png"),fmt="png", top=TRUE)
    clear3d()
  }
}

doesItRepeat <- function(M) {
  n = 0
  M2 = M
  
  while(TRUE) {
    M2 = conway_nextState(M2)
    n = n+1
    if(all.equal(M,M2) == TRUE) {
      break
    }
    
    if(n>10000) {
      break
    }
  }
  
  print(n)
}


# # Botella de Klein animada
# SEPMAX=3
# N=50  # Núm. de frames
# open3d()
# 
# for (n in 0:(N-1)) {
#   clear3d()
#   
#   # Falsas esferas para fijar la vista
#   spheres3d.plus(3.7*SEPMAX, 0, 0, radius=0)
#   spheres3d.plus(-1.5*SEPMAX, 0, 0, radius=0)
#   spheres3d.plus(0, 1.5*SEPMAX, 0, radius=0)
#   spheres3d.plus(0, -1.5*SEPMAX, 0, radius=0)
#   spheres3d.plus(0, 0, -2.2*SEPMAX, radius=0)
#   spheres3d.plus(0, 0, 5.2*SEPMAX, radius=0)
#   
#   SEP=SEPMAX*(1-cos(pi*n/(N-1)))/2  # Movimiento suave
#   kleinbottle3d.body(0, 0, 0, col=2, alpha=0.7)
#   kleinbottle3d.bottom(0, 0, -SEP, col=2, alpha=0.7)
#   kleinbottle3d.top(0, 0, SEP, col=2, alpha=0.7)
#   kleinbottle3d.handle(2*SEP, 0, 0, col=2, alpha=0.7)
#   
#   bg3d(color="gray50")
#   view3d(theta=0, phi=-75)
#   um=par3d()$userMatrix
#   um=rotate3d(um, -pi/8, 0, 0, 1)
#   par3d(FOV=0, zoom=0.6, userMatrix=um, windowRect=c(10,10,512,660))
#   
#   snapshot3d(paste0("klein", iif(n<10, "00", iif(n<100, "0", "")), n, ".png"),
#     fmt="png", top=TRUE)
# }
# 
# # Primitivas clásicas
# open3d()
# cylinder3d(0.5, z=-1.3, height=0.1, radius=2.6, col='green', n=5)
# cylinder3d(height=2.5, radius=0.8, col=5, alpha=0.5)
# torus3d(-0.5, -1, 0.7, r=0.2, col=7)
# moebius3d(2, 0, col=6)
# cone3d(2, 0, -1.3, radius=0.2, height=2.2, col=6)
# spheres3d.plus(2, 0, -1.3+2.2+0.3, radius=0.3, col=terrain.colors(5))
# bg3d(color="gray64")

