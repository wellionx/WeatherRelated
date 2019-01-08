library(httr)
dd <- read.table(text = content(GET("https://gist.githubusercontent.com/MrFlick/c1183c911bc5398105d4/raw/715868fba2d0d17a61a8081de17c468bbc525ab1/elements.txt")),sep=',',header=TRUE)
library(ggplot2)                
ggplot(dd, aes(Column, -Row)) +
  geom_tile(data=dd,aes(fill=GroupName),color="black") +
  geom_text(aes(label=ElementSymbol)) + 
  geom_text(aes(label=AtomicNumber), nudge_x=-.3, nudge_y=.3, size=2)+
  scale_fill_brewer(palette = "Set3") +
  theme_void() + theme(legend.position = "bottom")
