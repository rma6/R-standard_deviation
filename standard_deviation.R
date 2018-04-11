x = c()
while(1)
{
  num = readline()
  if(identical(num, "break"))
  {
    break
  }
  num = as.numeric(num)
  if(!is.na(num))
  {
    x[length(x)+1]=num
    print(mean(x))
    m=mean(x)
  }
}
x = c((x-m)**2)
print(sqrt(mean(x)))
sd = sqrt(mean(x))
print(sd/sqrt(length(x)))

dV <- function(x, t, dx, dt) {
  dx/dt+x*dt/(t**2)
}

dE <- function(m, v, dm, dv) {
  (v**2*dm/2)+(2*m*v*dv)
}

Ec <- function(m, v)
{
  m*v**2/2
}

dp <- function(m, v, dm, dv) {
  (v*dm)+(m*dv)
}