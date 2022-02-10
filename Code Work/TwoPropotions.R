pb=0
submax=0
sump_reject=0
for (h in 1:19){
  p1=h/20
  p2=h/20
  n1=45  #sample size of sample one
  n2=45  #sample size of sample two
  i=1
  p=0
  sub=0
  x1=0
  x2=0
for (r1 in 0:n1){
  for (r2 in 0:n2){
    p[i]=choose(n1,r1)*(p1^r1)*((1-p1)^(n1-r1))*choose(n2,r2)*(p2^r2)*((1-p2)^(n2-r2))
    p1h=r1/n1
    p2h=r2/n2
    sub[i]=(p1h-p2h)
    x1[i]=r1
    x2[i]=r2  #record r1 r2
    i=i+1
  }
}
for (i in 1:((n1+1)*(n2+1))){
  for (j in 1:((n1+1)*(n2+1))){
    if (sub[i]<sub[j]){
      termS=sub[i]
      term1=x1[i]
      term2=x2[i]
      termP=p[i]
      sub[i]=sub[j]
      x1[i]=x1[j]
      x2[i]=x2[j]
      p[i]=p[j]
      sub[j]=termS
      x1[j]=term1
      x2[j]=term2
      p[j]=termP
    }
  }
}

sump=0
n=(n1+1)*(n2+1)
for(i in 1:floor(n/2)){
  if (sump>0.05){
    break
  }
  sump=sump+p[i]
  # print(i)
  # print(sump)
}
k=i-1
submax_l=x1[i]-x2[i]-1

sump=0
for(i in n:ceiling(n/2)){
  if (sump>0.05){
    break}
  sump=sump+p[i]
  # print(i)
  # print(sump)
}
k=i+1
submax_r=x1[i]-x2[i]+1

sump1=0
sump2=0
for (i in 1:n){
  if(x1[i]-x2[i]<=submax_l)
    sump1=sump1+p[i]
  if(x1[i]-x2[i]>=submax_r)
    sump2=sump2+p[i]
}

#根据x1大小排序
# for(i in k:l){
#   for(j in k:l){
#     if(x1[i]<x1[j]){
#       termS=sub[i]
#       term1=x1[i]
#       term2=x2[i]
#       termP=p[i]
#       sub[i]=sub[j]
#       x1[i]=x1[j]
#       x2[i]=x2[j]
#       p[i]=p[j]
#       sub[j]=termS
#       x1[j]=term1
#       x2[j]=term2
#       p[j]=termP
#     }
#   }
# }

# for(i in k:l){
#   x<-sprintf("r1=%f, r2=%f", x1[i], x2[i])
#   print(x)
# }

# x<-sprintf("p1=p2=%f, maxsub=%d, p_reject=%f", p1, submax_r, sump1+sump2)
pb[h+1]=p1
sump_reject[h+1]=sump1+sump2
submax[h+1]=abs(submax_l)

# print(x)
}
x<-data.frame(pb,sump_reject,submax)
write.table (x, file ="E:/Syracuse University/Math Research/data_tp.csv", sep =",", row.names =TRUE, col.names =FALSE, quote =TRUE)

