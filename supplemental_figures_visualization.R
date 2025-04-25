## Below is code to generate all supplementary figures. 

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

dataset_2 <- read.csv("dataset_2.csv")

cflabels <- c("2-day cycle", "10-day cycle", "Monoculture", "Co-culture", "Population 1", "Population 2", "Population 3")
names(cflabels) <- c("2", "10", "mono", "cf", "a", "b", "c")

## to generate figure S1
dataset_2 %>%
  ggplot(aes(time, cfu, col=eco)) +
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
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 90, 30)) +
  scale_color_manual(values = c("#d55e00", "#0072b2"))+
  facet_wrap(~cycle, labeller = labeller(popabc=cflabels, cycle=cflabels)) +
  willie() +
  labs(x= "Time (days)", y = 'CFU/mL') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(size = 18))

## to generate Figure S2
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
  facet_grid(vars(cycle), vars(popabc), labeller = labeller(popabc=cflabels, cycle=cflabels)) +
  willie() +
  labs(x= "Time (days)", y = 'Relative Frequency') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18))

## to generate figure S3
dataset_2 %>%
  dplyr::filter(!(time %in% seq(0,28,2))) %>%
  ggplot(aes(r_dep, relfreq, col=eco)) +
  geom_point() +
  scale_color_manual(values = c("#d55e00", "#0072b2"), labels=c("Auxotroph", "Overproducer"))+
  scale_y_continuous(labels = 25 * (0:4)) +
  geom_smooth(method ="lm") +
  willie() +
  facet_wrap(~cycle, labeller = labeller(cycle=cflabels), scales = "free_x") +
  geom_vline(xintercept=0, linetype="dotted") +
  labs(x="Selection Rate (r)", y="% of Population") +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(size = 18))

## below is the code using the "lm' function used in the geom_smooth linear modeling in the plot above
## to collect intercept values for each mutant in each transfer condition
dataset_2_overproducer_2day <- dataset_2 %>%
  dplyr::filter(!(time %in% seq(0,28,2))) %>%
  dplyr::filter(cycle %in% c(2)) %>%
  dplyr::filter(eco %in% c("op")) %>%
  dplyr::select(c("r_dep", "relfreq"))
lm(relfreq ~ r_dep, data = dataset_2_overproducer_2day)

dataset_2_auxotroph_2day <- dataset_2 %>%
  dplyr::filter(!(time %in% seq(0,28,2))) %>%
  dplyr::filter(cycle %in% c(2)) %>%
  dplyr::filter(eco %in% c("aux")) %>%
  dplyr::select(c("r_dep", "relfreq"))
lm(relfreq ~ r_dep, data = dataset_2_auxotroph_2day)

dataset_2_overproducer_10day <- dataset_2 %>%
  dplyr::filter(!(time %in% seq(0,28,2))) %>%
  dplyr::filter(cycle %in% c(10)) %>%
  dplyr::filter(eco %in% c("op")) %>%
  dplyr::select(c("r_dep", "relfreq"))
lm(relfreq ~ r_dep, data = dataset_2_overproducer_10day)

dataset_2_auxotroph_10day <- dataset_2 %>%
  dplyr::filter(!(time %in% seq(0,28,2))) %>%
  dplyr::filter(cycle %in% c(10)) %>%
  dplyr::filter(eco %in% c("aux")) %>%
  dplyr::select(c("r_dep", "relfreq"))
lm(relfreq ~ r_dep, data = dataset_2_auxotroph_10day)

## to generate figure S4. Blank row and one errant evaporated row were filtered from this data set.
dataset_S1 <- read.csv("dataset_S1.csv")

dataset_S1 %>% 
  dplyr::mutate(strain = factor(strain, levels = c("wild-type (supplemented)", "wild-type (unsupplemented)", "auxotroph ∆metB", "overproducer ∆metJ"))) %>%
  dplyr::mutate(media = factor(media, levels = c("Met (+)", "Met (-)"))) %>%
  dplyr::filter(Time < 1095) %>%
  ggplot(aes(Time/60, value, col=strain))+
  geom_vline(xintercept = c(6, 12), linetype = "dashed", colour="black") +
  geom_smooth(linewidth=2) +
  willie() +
  scale_x_continuous(breaks = c(0, 2, 4, 6,  10, 14, 18)) +
  scale_y_continuous(breaks = seq(0, 1.25, 0.25)) +
  scale_color_manual(values = c("#ababab", "#ababab", "#d55e00", "#0072b2"))+
  facet_wrap(~media)+
  labs(x = 'Time (hours)', y = 'OD @ 600nm', colour = 'Culture')+
  theme(axis.title.x = element_text(color="black", size=20, face="bold"),
        axis.title.y = element_text(color="black", size=20, face="bold"),
        #strip.text.x = element_blank(),
        legend.title = element_text(color="black", size=20, face="bold"),
        legend.text = element_text(face = "italic", size=18),
        legend.position = "none")

## To generate figure S5
dataset_S2 <- read.csv("dataset_S2.csv")

dataset_S2 %>%
  dplyr::filter(!(count %in% c(0)) & !(cfu %in% c(0)))%>%
  ggplot(aes(time, frequency, col=strain, shape=Culture)) +
  stat_summary(
    fun = mean,
    geom='line',
    aes(colour=strain),
    linewidth = 1.25) +
  geom_quasirandom(width = 0.05) +
  scale_color_manual(values = c("#d55e00", "#0072b2"))+
  stat_summary(
    fun.data=mean_se,
    geom='errorbar',
    width=0.3) +
  scale_y_continuous(breaks = seq(0,1,0.25)) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  willie() +
  labs(x = 'Time (days)', y = 'Relative Frequency', colour = 'Strain') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_blank())


## To generate figure S6
dataset_10 <- read.csv("dataset_10.csv")

met_comp_labels <- c("Met (-)", "Met (+) Mutant Co-Culture")
names(met_comp_labels) <- c("tube", "tube_met")

dataset_10 %>%
  dplyr::filter(!(cfu %in% c(0))) %>%
  dplyr::filter(evo_group %in% c("tube_met")) %>%
  ggplot(aes(time, cfu, col=strain)) +
  stat_summary(
    fun = mean,
    geom='line',
    size = 1.25,
    linetype="dashed",
    aes(colour=strain)) +
  geom_quasirandom(size = 1.75, width = 0.2, alpha = 0.4) +
  scale_color_manual(values = c("#d55e00", "#0072b2", "#ababab"))+
  stat_summary(
    fun.data=mean_se,
    geom='errorbar',
    width=0.5) +
  geom_hline(yintercept = c(10^8, 10^7, 10^9, 10^6, 10^10), linetype = "dashed", alpha = 0.1) +
  scale_y_log10() +
  facet_wrap(~evo_group, labeller = labeller(evo_group=met_comp_labels)) +
  scale_x_continuous(breaks=seq(0,10,2)) +
  willie() +
  labs(x = 'Time (days)', y = 'CFU/mL', colour = 'Ecotype') +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", size = 20),
        axis.title.y = element_text(face = "bold", size = 20),
        strip.text.x = element_text(face = "bold", size = 18),
        axis.text = element_text(face = "bold", size = 18)
  )

