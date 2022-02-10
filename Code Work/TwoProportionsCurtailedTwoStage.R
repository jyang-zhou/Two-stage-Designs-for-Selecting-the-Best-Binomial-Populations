pb=0
submax=0
sump_reject=0

n1=45  #sample size of sample one
n2=45  #sample size of sample two
s1=ceiling(n1/2)  #sample size of sample one in stage 1
s2=ceiling(n2/2)  #sample size of sample two in stage 1
a1=ceiling(3*s1/5)
a2=2*a1
count=1
submax=0

# for (a1 in 1:s1){
#   for (a2 in (a1+1):(a1+n1-s1)){
    for(h in 1:19){
      
      p1=h/20
      p2=h/20
      i=1
      sub=0
      x1=0;x2=0
      phat=0
      p1hat=0
      p2hat=0
      sump=0
      
      for (r1 in (s1-a1+1):s1){     #r1 is the number of patients teseted
        for (r2 in (s2-a1+1):s2){     #r2 is the number of patients tested
          x1[i]=r1
          x2[i]=r2
          sub[i]=r1-r2
          p1hat[i]=choose(r1-1,s1-a1)*(p1^(r1-s1+a1-1))*(1-p1)^(s1-a1+1)
          p2hat[i]=choose(r2-1,s2-a1)*(p2^(r2-s2+a1-1))*(1-p2)^(s2-a1+1)
          phat[i]=p1hat[i]*p2hat[i]
          i=i+1}}
      
      for (r1 in a1:s1){
        for (r2 in a1:s2){
          pxIs=choose(r1-1,a1-1)*(p1^a1)*(1-p1)^(r1-a1)
          pyIs=choose(r2-1,a1-1)*(p2^a1)*(1-p2)^(r2-a1)
          for (m1 in (a2-a1):(n1-r1)){
            for (m2 in (a2-a1):(n2-r2)){
              pxIIs=pxIs*(choose(m1-1,a2-a1-1)*(p1^(a2-a1))*((1-p1)^(m1-a2+a1)))
              pyIIs=pyIs*(choose(m2-1,a2-a1-1)*(p2^(a2-a1))*((1-p2)^(m2-a2+a1)))
              sub[i]=m1+r1-m2-r2
              phat[i]=pxIIs*pyIIs
              i=i+1
            }}
          for (m1 in (n1-r1-(a2-a1)+1):(n1-r1)){
            for (m2 in (n2-r2-(a2-a1)+1):(n2-r2)){
              pxIIf=pxIs*choose((m1-1),(n1-r1-a2+a1))*(p1^(m1-(n1-r1-a2+a1+1)))*((1-p1)^(n1-r1-a2+a1+1))
              pyIIf=pyIs*choose((m2-1),(n2-r2-a2+a1))*(p2^(m2-(n2-r2-a2+a1+1)))*((1-p2)^(n2-r2-a2+a1+1))
              phat[i]=pxIIf*pyIIf
              sub[i]=m1+r1-m2-r2
              i=i+1
            }
          }}}
      
      tot=i-1
      
      for (i in 1:tot){
        for (j in 1:tot){
          if (sub[i]<sub[j]){
            termS=sub[i]
            termP=phat[i]
            sub[i]=sub[j]
            phat[i]=phat[j]
            sub[j]=termS
            phat[j]=termP
          }
        }
      }
      
      p_rejt_l=0
      p_rejt_r=0
      
      
      sump=0
      i=1
      while (sump<=0.05){
        sump=sump+phat[i]
        i=i+1
      }
      submax_l=sub[i]
      
      sump=0
      for(i in tot:ceiling(tot/2)){
        if (sump>0.05){
          break}
        sump=sump+phat[i]
        # print(i)
        # print(sump)
      }
      submax_r=sub[i]
      
      # sump1=p_rejt_l
      # sump2=p_rejt_r
      # for (i in 1:n){
      #   if(x1[i]-x2[i]<=submax_l)
      #     sump1=sump1+phat[i]
      #   if(x1[i]-x2[i]>=submax_r)
      #     sump2=sump2+phat[i]
      # }
      
      submax[count]=submax_r
      count=count+1
    }
#   }
# }

  
#   pb[h+1]=p1
#   sump_reject[h+1]=sump1+sump2
#   submax[h+1]=abs(submax_l)
# 
# 
# x<-data.frame(pb,sump_reject,submax)
# write.table (x, file ="E:/Syracuse University/Math Research/data_tpts.csv", sep =",", row.names =TRUE, col.names =FALSE, quote =TRUE)
