# La botella de Klein con R
# www.elmomentodecisivo.com

library(rgl)


# FUNCIONES 3D

# Esfera mejorada
spheres3d.plus=function(x=0, y=0, z=0, radius=1, n=101, ...){
  f=function(s, t) cbind(radius*cos(t)*cos(s) + x,
                         radius*       sin(s) + y,
                         radius*sin(t)*cos(s) + z)
  persp3d(f, slim=c(-pi/2, pi/2), tlim=c(0, 2*pi), n=n, add=T, ...)
}

# Cilindro
cylinder3d=function(x=0, y=0, z=0, radius=1, height=1, n=101, ...){
  f=function(s, t) cbind(radius*cos(t) + x,
                         radius*sin(t) + y,
                         height*s      + z)
  persp3d(f, slim=c(-1/2, 1/2), tlim=c(0, 2*pi), n=n, add=T, ...)
  
  t=seq(0, 2*pi, len=n)  # Tapas
  xp=radius*cos(t)
  yp=radius*sin(t)
  polygon3d(xp+x, yp+y, seq(z-height/2, z-height/2, len=n), ...)
  polygon3d(xp+x, yp+y, seq(z+height/2, z+height/2, len=n), ...)
}

# Cono
cone3d=function(x=0, y=0, z=0, radius=1, height=1, n=101, ...){
  f=function(s, t) cbind(radius*(1-s)*cos(t) + x,
                         radius*(1-s)*sin(t) + y,
                         height*s      + z)
  persp3d(f, slim=c(0, 1), tlim=c(0, 2*pi), n=n, add=T, ...)
  
  t=seq(0, 2*pi, len=n)  # Tapa
  polygon3d(radius*cos(t)+x, radius*sin(t)+y, seq(z, z, len=n), ...)
}

# Banda de Moebius
moebius3d=function(x=0, y=0, z=0, radius=1, width=1, n=101, ...){
  f=function(s, t) cbind((radius+s/2*cos(t/2))*cos(t) + x,
                         (radius+s/2*cos(t/2))*sin(t) + y,
                           width*s/2*sin(t/2)         + z)
  persp3d(f, slim=c(-1/2, 1/2), tlim=c(0, 2*pi), n=n, add=T, ...)
}

# Toro
torus3d=function(x=0, y=0, z=0, radius=1, R=1, n=101, ...){
  f=function(s, t) cbind((R+radius*cos(t))*cos(s) + x,
                         (R+radius*cos(t))*sin(s) + y,
                         radius*sin(t)            + z)
  persp3d(f, slim=c(-pi, pi), tlim=c(0, 2*pi), n=n, add=T, ...)
}

# Reloj de arena
sandclock3d=function(x=0, y=0, z=0, radius=1, height=1, n=101, ...){
  f=function(s, t) cbind(radius*s*2*cos(t) + x,
                         radius*s*2*sin(t) + y,
                         height*s      + z)
  persp3d(f, slim=c(-1/2, 1/2), tlim=c(0, 2*pi), n=n, add=T, ...)
  
  t=seq(0, 2*pi, len=n)  # Tapas
  xp=radius*cos(t)
  yp=radius*sin(t)
  polygon3d(xp+x, yp+y, seq(z-height/2, z-height/2, len=n), ...)
  polygon3d(xp+x, yp+y, seq(z+height/2, z+height/2, len=n), ...)
}

# Botella de Klein (parametrización de Paul Chang)
kleinbottle3d=function(x=0, y=0, z=0, n=101, ...){
  kleinbottle3d.bottom(x, y, z, n, ...)
  kleinbottle3d.body(x, y, z, n, ...)
  kleinbottle3d.top(x, y, z, n, ...)
  kleinbottle3d.handle(x, y, z, n, ...)
}

kleinbottle3d.bottom=function(x=0, y=0, z=0, n=101, ...){
  f=function(s, t) cbind((2.5+1.5*cos(s))*cos(t) + x,
                         (2.5+1.5*cos(s))*sin(t) + y,
                         -2.5    *sin(s)         + z)
  persp3d(f, slim=c(0, pi), tlim=c(0, 2*pi), n=n, add=T, ...)
}

kleinbottle3d.body=function(x=0, y=0, z=0, n=101, ...){
  f=function(s, t) cbind((2.5+1.5*cos(s))*cos(t) + x,
                         (2.5+1.5*cos(s))*sin(t) + y,
                         3*s                     + z)
  persp3d(f, slim=c(0, pi), tlim=c(0, 2*pi), n=n, add=T, ...)
}

kleinbottle3d.top=function(x=0, y=0, z=0, n=101, ...){
  f=function(s, t) cbind(2+(2+cos(t))*cos(s)    + x,
                         sin(t)                 + y,
                         3*pi+(2+cos(t))*sin(s) + z)
  persp3d(f, slim=c(0, pi), tlim=c(0, 2*pi), n=n, add=T, ...)
}

kleinbottle3d.handle=function(x=0, y=0, z=0, n=101, ...){
  f=function(s, t) cbind(2-2*cos(s)+sin(t) + x,
                         cos(t)            + y,
                         3*s               + z)
  persp3d(f, slim=c(0, pi), tlim=c(0, 2*pi), n=n, add=T, ...)
}

iif = function(condicion, val1, val2) {
  if (condicion) return(val1)
  return(val2)
}


# EJEMPLOS

# Botella de Klein
open3d()
kleinbottle3d(col=terrain.colors(3), alpha=0.5, n=201)
bg3d(color="lightblue")
view3d(theta=0, phi=0)
um=par3d()$userMatrix
um=rotate3d(um, -pi/2, 1, 0, 0)
par3d(FOV=0, zoom=0.75, userMatrix=um, windowRect=c(10,10,500,800))

# Botella de Klein animada
SEPMAX=3
N=50  # Núm. de frames
open3d()

for (n in 0:(N-1)) {
  clear3d()
  
  # Falsas esferas para fijar la vista
  spheres3d.plus(3.7*SEPMAX, 0, 0, radius=0)
  spheres3d.plus(-1.5*SEPMAX, 0, 0, radius=0)
  spheres3d.plus(0, 1.5*SEPMAX, 0, radius=0)
  spheres3d.plus(0, -1.5*SEPMAX, 0, radius=0)
  spheres3d.plus(0, 0, -2.2*SEPMAX, radius=0)
  spheres3d.plus(0, 0, 5.2*SEPMAX, radius=0)
  
  SEP=SEPMAX*(1-cos(pi*n/(N-1)))/2  # Movimiento suave
  kleinbottle3d.body(0, 0, 0, col=2, alpha=0.7)
  kleinbottle3d.bottom(0, 0, -SEP, col=2, alpha=0.7)
  kleinbottle3d.top(0, 0, SEP, col=2, alpha=0.7)
  kleinbottle3d.handle(2*SEP, 0, 0, col=2, alpha=0.7)
  
  bg3d(color="gray50")
  view3d(theta=0, phi=-75)
  um=par3d()$userMatrix
  um=rotate3d(um, -pi/8, 0, 0, 1)
  par3d(FOV=0, zoom=0.6, userMatrix=um, windowRect=c(10,10,512,660))
  
  snapshot3d(paste0("klein", iif(n<10, "00", iif(n<100, "0", "")), n, ".png"),
    fmt="png", top=TRUE)
}

# Primitivas clásicas
open3d()
cylinder3d(0.5, z=-1.3, height=0.1, radius=2.6, col='green', n=5)
cylinder3d(height=2.5, radius=0.8, col=5, alpha=0.5)
torus3d(-0.5, -1, 0.7, r=0.2, col=7)
moebius3d(2, 0, col=6)
cone3d(2, 0, -1.3, radius=0.2, height=2.2, col=6)
spheres3d.plus(2, 0, -1.3+2.2+0.3, radius=0.3, col=terrain.colors(5))
bg3d(color="gray64")
