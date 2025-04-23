## Below is the code to generate all data figure components of Figure 1

## load required packages
library("tidyverse")
library("ggtext")
library("ggbeeswarm")
library("ggbreak")

## set theme for plots
willie <- 
  function (base_size = 18, base_family = "", base_line_size = base_size/22, 
            base_rect_size = base_size/22){
    theme_bw(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
      theme(text = element_text(size = base_size, family = base_family, face = 'bold', color = 'black'),
            panel.border = element_rect(color = "black", fill=NA, size=2),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black",
                                     linewidth = rel(1)), legend.key = element_blank(), 
            strip.background = element_rect(fill = "white", colour = "black", 
                                            linewidth = rel(2)), complete = TRUE)
  }


## load in the conditioned media growth curve data. Blank well values have already been removed
## from these optical density values.

dataset_1 <- read.csv("dataset_1.csv")

dataset_1 %>%
  dplyr::filter(!id %in% c("Blank")) %>%
  dplyr::mutate(media = factor(media, levels = c("wild-type conditioned", "Overproducer *∆metJ* conditioned"))) %>%
  dplyr::mutate(id = factor(id, levels = c("wt", "∆metJ", "∆metB"))) %>%
  ggplot(aes(Time/60, value, col=id))+
  geom_smooth(size=2) +
  willie() +
  xlim(0,20) +
  scale_y_continuous(breaks = seq(0, 0.6, 0.1)) +
  scale_color_manual(values = c("#ababab", "#0072b2", "#d55e00")) +
  facet_wrap(~media, ncol=1, nrow=2) +
  labs(x = 'Time (hours)', y = 'OD @ 600nm', colour = 'Strain') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(face = "bold", size = 18),
        #strip.text = ggtext::element_markdown(),
        axis.text = element_text(size=18))

## load in the 90 day co-culture CFU and frequency data.

dataset_2 <- read.csv("dataset_2.csv")

cflabels <- c("2-day cycle", "10-day cycle", "Monoculture", "Co-culture", "Population 1", "Population 2", "Population 3")
names(cflabels) <- c("2", "10", "mono", "cf", "a", "b", "c")

dataset_2 %>%
  ggplot(aes(time, relfreq, col=eco)) +
  stat_summary(
    fun = mean,
    geom='line',
    size = 1.25,
    aes(colour=eco)) +
  stat_summary(
    fun=mean,
    geom='point',
    size = 1.75, 
    aes(stroke=1)) +
  stat_summary(
    fun.data=mean_se,
    geom='errorbar',
    width=1.5) +
  scale_y_continuous(breaks = seq(0,1,0.25)) +
  scale_x_continuous(breaks = seq(0, 90, 30)) +
  scale_color_manual(values = c("#d55e00", "#0072b2"))+
  facet_wrap(~cycle, labeller = labeller(popabc=cflabels, cycle=cflabels), nrow=1, ncol=2) +
  willie() +
  labs(x= "Time (days)", y = 'Relative Frequency') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(size = 18),
        axis.text = element_text(size=18)
  )

