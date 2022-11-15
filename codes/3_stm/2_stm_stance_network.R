library(stm)
library(jiebaR)
library(dplyr)

library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(sysfonts)
library(showtext)
showtext_auto(enable = TRUE)

library(furrr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(stminsights)
library(igraph)
library(ggraph)

library(tidystm)
library(igraph)
library(visNetwork)
library(corrplot)

#run_stminsights()

topicNames <- labelTopics(model_25)
k <- 25
topic <- data.frame(
  topicnames = c("TOPIC 1: Registration of personal information (0.004)",
                 "TOPIC 2: Government policy of LeaveHomeSafe (0.036***)",
                 "TOPIC 3: Resumption of normal traveller clearance between Hong Kong and Mainland (0.018***)",
                 "TOPIC 4: Election and LeaveHomeSafe use (-0.023***)",
                 "TOPIC 5: Civic Passion party (-0.051***)",
                 "Topic 6",
                 "TOPIC 7: LeaveHomeSafe use at restaurants (0.004)",
                 "TOPIC 8: Boycott of LeaveHomeSafe use (-0.087***)",
                 "TOPIC 9: Elderly's LeaveHomeSafe use (0.028***)",
                 "TOPIC 10: Yellow and blue economic circle (0.001)",
                 
                 "TOPIC 11: Witman Hung partygate (-0.006)",
                 "TOPIC 12: Civil servants' LeaveHomeSafe use (-0.007)",
                 "TOPIC 13: Government surveillance and privacy (-0.082***)",
                 "TOPIC 14: Vaccine pass (0.021***)",
                 "TOPIC 15: COVID-19 vaccination (-0.003)",
                 "TOPIC 16: Witman Hung partygate and officer's apology (-0.013***)",
                 "TOPIC 17: Vaccine pass and eHealth (0.032***)",
                 "TOPIC 18: Mobile device security and privacy (-0.045***)",
                 "TOPIC 19: Violation of regulations (0.015**)",
                 "TOPIC 20: Compulsory testing (0.018***)",
                 
                 "TOPIC 21: Entry of premises using LeaveHomeSafe (0.018***)",
                 "TOPIC 22: Privacy (un)concerns (0.053***)",
                 "TOPIC 23: Restrictions on restaurants and registration of personal information (0.025***)",
                 "TOPIC 24: Insults for yellow (liberals) (0.066***)",
                 "Topic 25"),
  TopicNumber = 1:k,
  TopicProportions = colMeans(model_25$theta))

####
prep <- estimateEffect(1:k ~ stance + s(day) + sentiment, model_25, meta = out$meta, uncertainty = "Global")

par(mfrow=c(1,1))
Result <- plot(prep, "stance", method = "difference", topics = c(1:25), model = model_25, 
               cov.value1 = "blue", cov.value2 = "yellow",
               xlab = "More Likely Yellow                         Not Significant                          More Likely Blue",
               main = "Effect of Sentiment on Topic Prevelance",
               xlim = c(-.15, .15), labeltype = "custom",
               custom.labels = c(1:25))

effect <- extract.estimateEffect(prep, c("stance"), model = model_25, method = "difference", cov.value1 = "blue", cov.value2 = "yellow")
knitr::kable(effect)
write.csv(effect,'stance_effect.csv')

# order based on Expected Topic Proportion
rank = order(unlist(Result$means))
topic <- topic[rank,]

newdata <- topic[-c(5, 15), ] 

par(mfrow = c(1,1),mar = c(6, 2, 2, 2))
STMresults <- plot(prep, "stance", method = "difference", cov.value1 = "blue", 
                   cov.value2 = "yellow", 
                   topics = newdata$TopicNumber,
                   verbose.labels = F, 
                   ylab = "", 
                   labeltype = "custom",
                   xlab = "                                                                                                                      More Likely Negative View                                      Not Significant                                      More Likely Positive View",
                   custom.labels  = newdata$topicnames, 
                   main = " ",
                   xlim = c(-.16,.08),
                   width = 100)


par(mfrow = c(1,1))

mod.out.corr <- topicCorr(model_25, cutoff = .01)

corrplot(mod.out.corr$cor, order="hclust", hclust.method="ward.D2", method = "circle", type = "lower", diag = F)

mod.out.corr <- topicCorr(model_25, method = "huge")

links2 <- as.matrix(mod.out.corr$posadj)
net2 <- graph_from_adjacency_matrix(links2, mode = "undirected")
table(V(net2)$type)

net2 <- simplify(net2, remove.multiple = F, remove.loops = T) 

links <- as_data_frame(net2, what="edges")
nodes <- as_data_frame(net2, what="vertices")

# Community Detection
clp <- cluster_label_prop(net2)
nodes$community <- clp$membership

means <- as.data.frame(unlist(STMresults$means))
colnames(means) <- "means"
color <- colorRamp(c("white","#FFB200"))(abs(means$means)/0.095)
means$colorDem <- rgb(color[,1],color[,2],color[,3],  maxColorValue=255)

color <- colorRamp(c("white","#277BC0"))(abs(means$means)/0.1)
means$colorRep <- rgb(color[,1],color[,2],color[,3],  maxColorValue=255)

means$color <- ifelse(means$means>0,means$colorDem,means$colorRep)

#visNetwork edits
nodes$shape <- "dot"  
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$title <- apply(topicNames$prob, 1, function(x) paste0(x, collapse = " + "))[rank] # Text on click
nodes$label <- topic$topicnames # Node label
nodes$size <- (topic$TopicProportions / max(topic$TopicProportions)) * 40 # Node size
nodes$borderWidth <- 2 # Node border width

nodes$color.background <- means$color
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"
nodes$id <- topic$TopicNumber

visNetwork(nodes, links, width="100%",  height="1200px", main="Topic Sentiment Correlation Network") %>% visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical")) %>% 
  visInteraction(navigationButtons = TRUE)

write.csv(links,'stance_edges.csv')
write.csv(nodes,'stance_nodes.csv')

stm_corrs <- get_network(model = model_25,
                         method = 'huge',
                         labels = paste('Topic', 1:25),
                         cutoff = 0.01,
                         cutiso = FALSE)

write.table(stm_corrs , file = "stance_corr.csv", sep=",", row.names=FALSE)
