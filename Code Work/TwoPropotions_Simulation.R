critical<-c(5,	6,	7,	8,	8,	9,	9,	9,	9,	9,	9,	9,	9,	8,	8,	8,	7,	6,	4)
x=0
p=0
for (h in 1:19){
p1=h/20
p2=h/20
n1=50  #sample size of sample one
n2=50  #sample size of sample two
sub=0
p1h=0;p2h=0
s1=0;s2=0
acpt=0
rjct=0

for(i in 1:10000){
  s1=rbinom(n1,1,p1)
  s2=rbinom(n2,1,p2)
  if(abs(sum(s1)-sum(s2))<critical[h])
    acpt=acpt+1
  else
    rjct=rjct+1}
p[h+1]=p1
x[h+1]=rjct/(rjct+acpt)
}
y<-data.frame(p,x)
write.table (y, file ="E:/Syracuse University/Math Research/data_tps.csv", sep =",", row.names =TRUE, col.names =FALSE, quote =TRUE)

