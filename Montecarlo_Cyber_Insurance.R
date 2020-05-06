rm(list = ls())

A.r=read.csv("D:\\links.csv",header = FALSE)
A=as.matrix(A.r,30,30,byrow = T)
View(A)   #大写字母

k.r=read.csv("D:\\BA_model.csv",header = FALSE)
k=as.matrix(k.r[,2],byrow = T)
View(k)   #小写字母，节点度数为k

h1= function(mu1,k){      #内部攻击成功概率
  return(mu1*k)
}

h2= function(mu2,k){      #外部攻击成功概率
  return(mu2*k)
}

wk= function(rho,k){      #节点的初始财富  
  return(rho*k)
}

ct= function(tau,k,t,rho){    #定义成本函数，节点恢复期间的成本率c2和k有关，这里定义为正比例函数ct=c2*r+c3*wk
  return(tau*k*t+0.5*rho*k)
}

mn= function(fai,N){      #节点的无形损失和N有关，这里定义为正比例函数
  return(fai*N)
}


simucyber=function(d,b,g,N,t,v,A,mu1,mu2,k){
  ts1=0                        #初始时间为0
  all.mat<-NULL
  while(ts1<t)
  {
    t_all_time<-rep(NA,N)      #创建一个向量来存储每个节点的所有恢复和感染时间
    
    for(i in (1:N)[v==1])      #对于已经感染的节点，生成受感染节点的恢复时间，d为恢复概率
    {
      t_all_time[i]<-rexp(1,d) 
    }
    
    for(j in (1:N)[v==0])        
    {
      n=sum(A[j,]*v)           #对于健康节点，计算节点j的被感染邻居的数量n
      t_all_time[j]=rexp(1,rate=(n*b*(1-h1(mu1,k[j]))+g*(1-h2(mu2,k[j]))))  #健康节点的感染时间，内部感染n*b*h1外部感染s*h2
    }
    
    dt=min(t_all_time)         #最小时间
    
    loc=which.min(t_all_time)  #发生改变的节点的位置
    
    ts1=ts1+dt     #更新ts1时间
    
    cc=0
    if(ts1<t & (v[loc]==1))       #ts1小于t且节点初始是感染状态，发生恢复过程，节点状态从1到0
    {
      v[loc]=0
      cc=1                                  #原本为1
      
    }
    
    else if(ts1<t)                          #否则发生感染过程，节点状态从0到1                   
    {
      v[loc]=1
      cc=0                                  #原本为0
    }
    all.mat=rbind(all.mat,c(v,ts1,which.min(t_all_time),min(t_all_time),cc))
    
  }
  
  return(all.mat)                           #返回值
}

t=365  #时间

N=30  #网络中的节点数

v=rep(0,N) #初始节点的状态,均为健康状态

d=.5   #恢复概率

b=.2  #通过链接的感染率

g=.1  #通过外界的感染率

mu1=.09  #内部攻击成功概率函数参数

mu2=.08  #外部攻击成功概率函数参数

rho=2

tau=5

fai=5

zz<-simucyber(d,b,g,N,t,v,A,mu1,mu2,k)

View(zz)   #显示t时间内all.mat矩阵

for(j in 1:1000){
  
  zz<-simucyber(d,b,g,N,t,v,A,mu1,mu2,k) #引入上面函数
  ll_inf<-rep(0,N)               #记录每个节点由于新感染而造成的损失
  ll<-rep(0,N)                   #记录由于服务中断而造成的损失
  nn<-dim(zz)[1]                 #zz输出的矩阵有多少行
  k2=matrix(k,1,N)               #将k变为行向量
  t_t=rep(0,N)                   #每个节点营业中断的总时间
  
  ll_inf[zz[1,32]]<-wk(rho,k2[zz[1,32]])*rbeta(1,2,3) #对首先由0到1的节点（which.min(t_all_time)的节点）根据beta分布产生一个随机损失填入ll_inf中
 
  for(i in 2:nn){                   
    
    ind_tem<-zz [i,1:30]-zz[(i-1),1:30]         #更新时间后节点情况的变化
    
    if(i<=nn-1){
      
      if(any(ind_tem==1))        #对于更新时间后由0到1的节点，计算新感染累计损失
      {
        ll_inf[ind_tem==1]<-ll_inf[ind_tem==1]+wk(rho,k2[which(ind_tem==1)])*rbeta(sum(ind_tem==1),2,3)   #损失比例服从beta分布，还要乘以初始财富
      }   
      
      if(any(ind_tem==0 & zz[(i-1),1:30]==1))    #对于更新时间后状态仍为1没变的节点，计算服务中断累计损失，数据的直接损失是单次损失
      {
        t_t[ind_tem==0 & zz[(i-1),1:30]==1]=t_t[ind_tem==0 & zz[(i-1),1:30]==1]+zz[i,33]
        ll[ind_tem==0 & zz[(i-1),1:30]==1]<-ll[ind_tem==0 & zz[(i-1),1:30]==1]+ct(tau,k2[which(ind_tem==0 & zz[(i-1),1:30]==1)],zz[i,33],rho)
      }
      
      if(any(ind_tem==-1))       #对于更新时间后由1到0的节点，计算服务中断累计损失
      {
        t_t[ind_tem==-1]=t_t[ind_tem==-1]+zz[i,33]
        ll[ind_tem==-1]<-ll[ind_tem==-1]+ct(tau,k2[which(ind_tem==-1)],zz[i,33],rho)
      }
    }
    
    else{                        #对最后一行的节点
      if(any(ind_tem==0 & zz[(i-1),1:30]==1))    #对于更新时间后状态仍为1没变的节点，计算服务中断累计损失
      {
        t_t[ind_tem==0 & zz[(i-1),1:30]==1]=t_t[ind_tem==0 & zz[(i-1),1:30]==1]+t-zz[(i-1),31]
        ll[ind_tem==0 & zz[(i-1),1:30]==1]<-ll[ind_tem==0 & zz[(i-1),1:30]==1]+ct(tau,k2[which(ind_tem==0 & zz[(i-1),1:30]==1)],(t-zz[(i-1),31]),rho) 
      }
      
      if(any(ind_tem==-1))                        #对于更新时间后由1到0的节点，计算服务中断累计损失
      {
        t_t[ind_tem==-1]=t_t[ind_tem==-1]+t-zz[(i-1),31]
        ll[ind_tem==-1]<-ll[ind_tem==-1]+ct(tau,k2[which(ind_tem==-1)],(t-zz[(i-1),31]),rho)
      }
      
    }
  }
  
  ll_total=ll_inf+ll        #每个节点的总损失
  
  write.table(t(c(j,ll_total)),"D:\\t1000_loss.txt",append=TRUE,
              row.names =F,col.names = F)
  
  write.table(t(c(j,t_t)),"D:\\t1000_time.txt",append=TRUE,
              row.names =F,col.names = F)
  
  rm(zz)
}

inf_ttime<-read.table("D:\\t1000_time.txt") 

inf_tlost<-read.table("D:\\t1000_loss.txt")

df1=vector()
df_time = rbind(df1, round(apply(inf_ttime[,-1],2,mean),4),     #每个节点的感染总时间
                    round(apply(inf_ttime[,-1],2,sd),4),
                    round(apply(inf_ttime[,-1],2,max),4),
                    round(apply(inf_ttime[,-1],2,min),4))
df2=vector()
df_rate = rbind(df2, round(apply(inf_ttime[,-1]/t,2,mean),4),   #每个节点的感染概率
                round(apply(inf_ttime[,-1]/t,2,sd),4),
                round(apply(inf_ttime[,-1]/t,2,max),4),
                round(apply(inf_ttime[,-1]/t,2,min),4))

df3=vector()
df_lose = rbind(df3, round(apply(inf_tlost[,-1],2,mean),4),     #每个节点的总损失
                round(apply(inf_tlost[,-1],2,sd),4),
                round(apply(inf_tlost[,-1],2,max),4),
                round(apply(inf_tlost[,-1],2,min),4))

Net_Total_lost=sum(apply(inf_tlost[,-1],2,mean))+mn(fai,N)      #整个网络的总损失

Net_Total_lost #整个网络的总损失为所有节点损失之和+无形损失mn   


