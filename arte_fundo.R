library(ggplot2)
library(RColorBrewer)

set.seed(345)


ngroup=30
names=paste("G_",seq(1,ngroup),sep="")
DAT=data.frame()

for(i in seq(1:30)){
  data=data.frame( matrix(0, ngroup , 3))
  data[,1]=i
  data[,2]=sample(names, nrow(data))
  data[,3]=prop.table(sample( c(rep(0,100),c(1:ngroup)) ,nrow(data)))
  DAT=rbind(DAT,data)
}
colnames(DAT)=c("Year","Group","Value")
DAT=DAT[order( DAT$Year, DAT$Group) , ]


coul = brewer.pal(12, "Paired")
coul = brewer.pal(n = 100, "Reds")
coul = colorRampPalette(coul)(ngroup)
coul=coul[sample(c(1:length(coul)) , size=length(coul) ) ]

ggplot(DAT, aes(x=Year, y=Value, fill=Group )) +
  geom_area(alpha=1  ) +
  theme_bw() +
  #scale_fill_brewer(colour="red", breaks=rev(levels(DAT$Group)))+
  scale_fill_manual(values = coul)+
  theme(
    text = element_blank(),
    line = element_blank(),
    title = element_blank(),
    legend.position="none",
    panel.border = element_blank(),
    panel.background = element_blank())

library(aRtsy)

# paleta <- c("#042940","#005C53","#9FC131","#DBF227","#D6D58E")
paleta <- c("#28156e","#291e4e","#f3be0c","#d9241b","#a3192b")
# paleta <- c("#B85C38", "#8A4E24","#D9C4A1", "#4E3620", "#6B3E26")

angles <- matrix(0, 200, 200)
angles[1:100, ] <- seq(from = 0, to = 2 * pi, length = 100)
angles[101:200, ] <- seq(from = 2 * pi, to = 0, length = 100)
angles <- angles + rnorm(200 * 200, sd = 0.1)


canvas_flow(colors = paleta, lines = 2000,
            background = "white",
            lwd = 1, outline = "square", angles = angles)


canvas_phyllotaxis(
  paleta,
  background = "#fafafa",
  iterations = 100000,
  angle = 30,
  size = 1,
  alpha = 1,
  p = 0.1
)

canvas_chladni(
  paleta,
  waves = c(1, 4, 1, 1),
  warp = 4,
  resolution = 1000,
  angles = "worley",
  distances = "cubic",
  flatten = FALSE
)

ggplot2::ggsave(filename = "fundo_2.png")

canvas_cobweb(
  paleta,
  background = "black",
  lines = 300,
  iterations = 100
)


canvas_collatz(
  paleta,
  background = "#fafafa",
  n = 500,
  angle.even = 0.0075,
  angle.odd = 0.0145,
  side = FALSE
)

paleta <- c("#005C53","#DBF227")

canvas_flame(
  paleta,
  # background = "gray99",
  iterations = 1e7,
  variations = c(10, 17),
  # symmetry = -1,
  # blend = TRUE,
  # weighted = FALSE,
  # post = FALSE,
  # final = FALSE,
  # extra = FALSE,
  # display = c("colored", "logdensity"),
  # zoom = 1,
  resolution = 1000,
  # gamma = 5
)


canvas_flow(
  background = "transparent",
  colors = paleta, lines = 390,
  # iterations = 1000,
  lwd = 0.5, polar = F
)

# ggplot2::ggsave(filename = "teste.png")


canvas_squares(
  paleta,
  background = "transparent",
  cuts = 300,
  resolution = 200,
  noise = F
)

# ggplot2::ggsave(filename = "fundo_1.png")
