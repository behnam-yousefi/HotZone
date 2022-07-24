## Plot persistant homology

Thr = seq(.02,.15,.01)

FC1 = matrix(0,Ninv1,length(Thr))

for (indv in 1:Ninv1){
  Adj = IndvAdjlist[[indv1]]
  
  for (i in 1:length(Thr)){
    Adj_bin = ifelse(Adj>Thr[i],1,0)
    g = graph_from_adjacency_matrix(Adj_bin, mode = "undirected")
    FC1[indv,i] = FiedlerValue(g, max = 5)[["FiedlerValue"]]
    # FC1[indv,i] = log(mean(degree(g)))
  }
}


df1 = data.frame(Thr = Thr, MeanFC = apply(FC1, 2, mean), sd = apply(FC1, 2, sd), cl = rep("1",length(Thr)))
df2 = data.frame(Thr = Thr, MeanFC = apply(FC2, 2, mean), sd = apply(FC2, 2, sd), cl = rep("2",length(Thr)))

df = rbind(df1,df2)

ggplot(df, aes(x=Thr, y=MeanFC, group=cl, color=cl)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=MeanFC-sd, ymax=MeanFC+sd), width=.005,
                position=position_dodge(0.0003))+
  labs(title="Persistence Diagrams", x="Threshols", y = "Node Degree")+
  theme_classic()+
  scale_color_manual(values=c("red", "green"))

