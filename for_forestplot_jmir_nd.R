## Load libraries
library(gridExtra)
#install.packages("ggplot2")
library(ggplot2)

## Example data frame ---depressed
#dat <- data.frame(
#  Index = c(1, 2, 3, 4), ## This provides an order to the data
#  label = c("Sleep", "Sedentary", "LPA", "MVPA"),
#  OR =  c(0.486, 1.003, 0.998, 0.861),
#  LL = c(0.229, 0.999, 0.994, 0.635), 
#  UL = c(1.029, 1.007, 1.001, 1.167),
#  CI = c("0.229, 1.029", "0.999, 1.007", "0.994, 1.001", "0.635, 1.167")
#)
#dat

###not depressed
dat <- data.frame(
  Index = c(1, 2, 3, 4), ## This provides an order to the data
  label = c("Sleep", "Sedentary", "LPA", "MVPA"),
  OR =  c(0.882, 1.000, 0.999, 0.770),
  LL = c(0.655, 0.999, 0.998, 0.686), 
  UL = c(1.187, 1.002, 1.001, 0.865),
  CI = c("0.655, 1.187", "0.999, 1.002", "0.998, 1.001", "0.686, 0.865")
)
dat


## Plot forest plot
plot1 <- ggplot(dat, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:4, labels = dat$label, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x.bottom = element_text(size = 10, colour = "black"),
        axis.title.x = element_text(size = 14, colour = "black"))
plot1




## Create the table-base pallete
table_base <- ggplot(dat, aes(y=label)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=14), 
        axis.text.x = element_text(color="white", hjust = -3, size = 10), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## OR point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.3f", round(OR, digits = 3))), size = 3.5) + ## decimal places
  ggtitle("OR")

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = CI), size =3.5) + 
  ggtitle("95% CI")

## Merge tables with plot
lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
grid.arrange(plot1, tab1, tab2, layout_matrix = lay)