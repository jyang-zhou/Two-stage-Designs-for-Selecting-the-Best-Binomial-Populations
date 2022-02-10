n1=9  #sample size of sample one
n2=9  #sample size of sample two
p1=0.3  
p2=0.3
x1=4  #sample size of sample one in stage 1
x2=4  #sample size of sample two in stage 1
a1=15  #critical point of sample one in stage 1 
a2=15  #critical point of sample two in stage 1 
r1=20  #critical point of sample one in stage 2
r2=20  #critical point in sample two in stage 2

for (y1 in (a1:x1))
{ 
  pIIy1_1=choose(y1-1,a1-1)*(p1^a1)*((1-p1)^(y1-a1))
  y2 <- (r1-a1):(n1-y1)
  pIIRy2_1=choose(y2-1,r1-a1-1)*(p1^(r1-a1))*((1-p1)^(y2-r1+a1))
  pIIRy2_2=choose(y2-1,r2-a2-1)*(p2^(r2-a2))*((1-p2)^(y2-r2+a2))
  phat=(r1+r2)/(2*(y1+y2))
  for (i in 1:(n1-y1-r1+a1+1))
  {
    SE=sqrt(phat[i]*(1-phat[i])*(1/n1+1/n2))
    z=(pIIRy2_1[i]-pIIRy2_2[i])*pIIy1_1/SE
    if (abs(z)>1.96){
      x<-sprintf("when y1=%2d, y2=%2d, z=%.5f, we reject H0: p1=p2", y1, y2[i], z)
      print(x)
    }else{
      x<-sprintf("when y1=%2d, y2=%2d, z=%.5f, we accept H0: p1=p2", y1, y2[i], z)
      print(x)
  }}
}
#EIIRy2=(pIIy1*sum(y2*pIIRy2))+EIIRy2
#m_1=n1-y1-r1+a1+1
#y2 <- (m_1):(n1-y1)
#pIIAy2_1[y1]=choose(y2-1,m_1-1)*(p1^(y2-m_1))*(1-p1)^m_1
#EIIAy2=(pIIy1*sum(y2*pIIAy2))+EIIAy2