pb=0
submax=0
sump_reject=0

for (h in 1:19){
n1=10  #sample size of sample one
n2=10  #sample size of sample two
p1=h/20
p2=h/20
s1=5  #sample size of sample one in stage 1
s2=5  #sample size of sample two in stage 1
critical1=2  #critical point in the first stage
i=1
sub=0
x1=0;x2=0
phat=0

for (r1 in 0:n1){
  for (r2 in 0:n2){
    if (r1<critical1){
      if (r2<critical1){
        x1[i]=r1
        x2[i]=r2
        sub[i]=r1-r2
        phat[i]=choose(s1,r1)*(p1^r1)*((1-p1)^(s1-r1))*choose(s2,r2)*(p2^r2)*((1-p2)^(s2-r2))
        i=i+1}
      else{
        x1[i]=r1
        x2[i]=r2
        sub[i]=r1-r2
        p1h=choose(s1,r1)*(p1^r1)*((1-p1)^(s1-r1))
        index<-(critical1):r2
        p2h=sum(choose(s1,index)*(p2^index)*(1-p2)^(s1-index)*choose(n2-s1,r2-index)*(p2^(r2-index))*(1-p2)^(n2-s1-r2+index))
        phat[i]=p1h*p2h
        i=i+1}}
    
    else{
      if (r2<critical1){
        x1[i]=r1
        x2[i]=r2
        sub[i]=r1-r2
        index<-(critical1):r1
        p1h=sum(choose(s1,index)*(p1^index)*(1-p1)^(s1-index)*choose(n1-s1,r1-index)*(p1^(r1-index))*(1-p1)^(n1-s1-r1+index))
        p2h=choose(s2,r2)*(p2^r2)*((1-p2)^(s2-r2))
        phat[i]=p1h*p2h
        i=i+1}
      else{
        x1[i]=r1
        x2[i]=r2
        sub[i]=r1-r2
        index<-(critical1):r1
        p1h=sum(choose(s1,index)*(p1^index)*(1-p1)^(s1-index)*choose(n1-s1,r1-index)*(p1^(r1-index))*(1-p1)^(n1-s1-r1+index))
        index<-(critical1):r2
        p2h=sum(choose(s1,index)*(p2^index)*(1-p2)^(s1-index)*choose(n2-s1,r2-index)*(p2^(r2-index))*(1-p2)^(n2-s1-r2+index))
        phat[i]=p1h*p2h
        i=i+1}}
      
  }
}

p_rejt_l=0
p_rejt_r=0
i=1
while (i<=(n1+1)*(n2+1)){
  if (x1[i]<critical1 && x2[i]>=critical1){
    p_rejt_l=phat[i]+p_rejt_l
    phat[i]=0
  }else if (x1[i]>=critical1 && x2[i]<critical1){
    p_rejt_r=phat[i]+p_rejt_r
    phat[i]=0
  }else{
    phat[i]=phat[i]
}
  i=i+1
}

for (i in 1:((n1+1)*(n2+1))){
  for (j in 1:((n1+1)*(n2+1))){
    if (sub[i]<sub[j]){
      termS=sub[i]
      term1=x1[i]
      term2=x2[i]
      termP=phat[i]
      sub[i]=sub[j]
      x1[i]=x1[j]
      x2[i]=x2[j]
      phat[i]=phat[j]
      sub[j]=termS
      x1[j]=term1
      x2[j]=term2
      phat[j]=termP
    }
  }
}

sump=0
n=(n1+1)*(n2+1)
i=1
while (sump<=0.05){
  sump=sump+phat[i]
  i=i+1
}
submax_l=x1[i]-x2[i]-1

sump=0
for(i in n:ceiling(n/2)){
  if (sump>0.05){
    break}
  sump=sump+phat[i]
  # print(i)
  # print(sump)
}
submax_r=x1[i+1]-x2[i+1]+1

sump1=p_rejt_l
sump2=p_rejt_r
for (i in 1:n){
  if(x1[i]-x2[i]<=submax_l)
    sump1=sump1+phat[i]
  if(x1[i]-x2[i]>=submax_r)
    sump2=sump2+phat[i]
}

pb[h+1]=p1
sump_reject[h+1]=sump1+sump2
submax[h+1]=abs(submax_l)
}

x<-data.frame(pb,sump_reject,submax)
write.table (x, file ="E:/Syracuse University/Math Research/data_tpts.csv", sep =",", row.names =TRUE, col.names =FALSE, quote =TRUE)
