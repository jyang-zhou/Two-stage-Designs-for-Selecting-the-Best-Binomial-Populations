#initial conditions
n1=16
a1=8
p=0.6
n=46
r2=24
total=0
totalmonth=0

for (y in 1:10000){
  
  #initial conditions for each loop
  total1=0;positive1=0;total2=0
  positive2=0;maxtime=0;timein=0
  timeout=0;s1=0;s2=0;totaltime=0
  number=0;maxtime1=0;maxtime2=0
  
  
  #stage_one design
  for (x1 in 1:n1){
    if (x1==1){
      timein[x1]=rexp(1,rate=1)}
    else{
      timein[x1]=rexp(1,rate=1)+timein[x1-1]}
    timeout[x1]=runif(1,min=1,max=6)
    totaltime[x1]=timein[x1]+timeout[x1]
    number[x1]=x1
    s=rbinom(1,1,p)
    s1[x1]=s
    if (s1[x1]>=1){
      positive1=positive1+1}}
  
  #put the time in order
  for (i in 1:n1){
    for (j in 1:n1){
      if (totaltime[i]<totaltime[j]){
        term.t=totaltime[i]
        term.s=s1[i]
        term.n=number[i]
        term.ti=timein[i]
        term.to=timeout[i]
        totaltime[i]=totaltime[j]
        s1[i]=s1[j]
        number[i]=number[j]
        timein[i]=timein[j]
        timeout[i]=timeout[j]
        totaltime[j]=term.t
        s1[j]=term.s
        number[j]=term.n
        timein[j]=term.ti
        timeout[j]=term.to}}}
 
   #if (n1-a1+1) failures are first found
  if ((n1-positive1)>=n1-a1+1){
    failure1=0
    for (i in 1:n1){
      if (failure1<(n1-a1+1)&&s1[i]==0)
        {maxtime=totaltime[i]
         failure1=failure1+1
         total1=i}}
    cutoff1=total1
    if (cutoff1<n1){ 
    for (i in (cutoff1+1):n1){
      if (timein[i]<maxtime1){
        total1=total1+1}}}
    total=total+total1
    totalmonth=maxtime+totalmonth}
  
  #if a1 successes are first found
  else{
  success1=0
  extrasuccess1=0
  extrafailure1=0
  #find the first a1 successes, and number needed to find first a1 success
  for (i in 1:n1){
    if (success1<a1&&s1[i]==1){
      maxtime1=totaltime[i]
      success1=success1+1
      total1=i}}
  cutoff1=total1
  if (cutoff1<n1){
  for (i in (cutoff1+1):n1){
    if (timein[i]<maxtime1){
      total1=total1+1
      if (s1[i]==1)
        extrasuccess1=extrasuccess1+1
      else
        extrafailure1=extrafailure1+1}}}
  
  #stage_two design
  for (x2 in (total1+1):n){
    if (x2==total1+1){
      timein[x2]=rexp(1,rate=1)+maxtime1}
    else{
      timein[x2]=rexp(1,rate=1)+timein[x2-1]}
    timeout[x2]=runif(1,min=1,max=6)
    totaltime[x2]=timein[x2]+timeout[x2]
    s=rbinom(1,1,p)
    s2[x2]=s
    if (s2[x2]>=1){
      positive2=positive2+1}}
  
  #put the time in order
  for (i in (total1+1):n){
    for (j in (total1+1):n){
      if (totaltime[i]<totaltime[j]){
        term.t=totaltime[i]
        term.s=s2[i]
        term.ti=timein[i]
        term.to=timeout[i]
        totaltime[i]=totaltime[j]
        s2[i]=s2[j]
        timein[i]=timein[j]
        timeout[i]=timeout[j]
        totaltime[j]=term.t
        s2[j]=term.s
        timein[j]=term.ti
        timeout[j]=term.to}}}
  
  #if (n1-a1+1) failures are first found
  if (n-positive2-success1-extrasuccess1+extrafailure1>=n-r2+1){
    failure2=0
    for (i in (total1+1):n){
      if (failure2+(total1-success1-extrasuccess1+extrafailure1)<n-r2+1&&s2[i]==0)
      {maxtime2=totaltime[i]
       failure2=failure2+1
       total2=i}}
    cutoff2=total2
    if (cutoff2<n){
    for (i in (cutoff2+1):n){
      if (timein[i]<maxtime2){
        total1=total1+1}}}
    total=total+total2
    totalmonth=maxtime2+totalmonth}
  
  #if r2 successes are first found
  else{
    success2=0
    extrasuccess2=0
    extrafailure2=0
    #find the first r2 successes, and number needed to find first r2 success
    for (i in (total1+1):n){
      if (success2+success1+extrasuccess1<r2&&s2[i]==1){
        maxtime2=totaltime[i]
        success2=success2+1
        total2=i}}
    cutoff2=total2
    if (cutoff2<n){
    for (i in (cutoff2+1):n){
      if (timein[i]<maxtime2){
        total2=total2+1}}}
    total=total+total2
    totalmonth=maxtime2+totalmonth}}}

total/10000
totalmonth/10000