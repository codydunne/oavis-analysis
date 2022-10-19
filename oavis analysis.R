library(tidyverse)
library(stringr)
library(forcats)
library(extrafont)
library(Cairo)
library(readr)
library(svglite)
extrafont::loadfonts(device="win", quiet=TRUE)
extrafont::loadfonts(device="pdf", quiet=TRUE)
windowsFonts(Times=windowsFont("TT Times New Roman"))

data = read_csv('openaccessvis-all.csv')
problems(data)
OADomains = c("osf.io", "arxiv.org", "biorxiv.org", "psyarxiv.org", "/hal.", "/eprints.", "/openaccess.")


duplicates = tribble(
  ~Authors, ~Authors2,
  "Steve Franconeri", "Steven Franconeri",
  "John Stasko", "John T. Stasko"
)

# PDF open category
data = data %>% 
  mutate(AuthorPDF = ifelse(is.na(AuthorPDF), "", AuthorPDF)) %>% 
  mutate(isClosedAccess = AuthorPDF == "") %>% 
  rowwise() %>% 
  mutate(isFullyOpen = sum(str_detect(AuthorPDF, OADomains)) > 0 ) %>% 
  mutate(openCategory = ifelse(isClosedAccess, "Hidden     ", ifelse(isFullyOpen, "Reliable Open Access     ", "Open     ")))

openLevels = c("Reliable Open Access     ", "Open     ", "Hidden     ")
openLevelsMaterials = c("Reliable Open Access     ", "Open     ", "Hidden or Not Applicable")
venueLevels = c("InfoVis", "VAST", "SciVis", "TVCG", "CG&A")
colors = c(InfoVis = "#6666FF", 
           VAST =    "#66CC66", 
           SciVis =  "#FF6666", 
           TVCG =    "#C9C91D", 
           `CG&A` =  "#C9C91D")
data = data %>% mutate(openCategory = factor(openCategory, openLevels))
data = data %>% mutate(ReviewVenue = factor(ReviewVenue, venueLevels))

# drop data and materials without a paper
data = data %>% mutate(SourceMaterials = ifelse(openCategory == "Hidden     ", NA, SourceMaterials))
data = data %>% mutate(Data = ifelse(openCategory == "Hidden     ", NA, Data))
data = data %>% mutate(ExplanationPage = ifelse(openCategory == "Hidden     ", NA, ExplanationPage))

# open data by availability category
data = data %>% 
  mutate(Data = ifelse(is.na(Data), "", Data)) %>% 
  rowwise() %>% 
  mutate(Data = ifelse(Data == "", "Hidden or Not Applicable", ifelse(sum(str_detect(Data, OADomains)) > 0, "Reliable Open Access     ", "Open     "))) %>% 
  mutate(Data = factor(Data, openLevelsMaterials))

# open materials by availability category
data = data %>% 
  mutate(SourceMaterials = ifelse(is.na(SourceMaterials), "", SourceMaterials)) %>% 
  rowwise() %>% 
  mutate(SourceMaterials = ifelse(SourceMaterials == "", "Hidden or Not Applicable", ifelse(sum(str_detect(SourceMaterials, OADomains)) > 0, "Reliable Open Access     ", "Open     "))) %>% 
  mutate(SourceMaterials = factor(SourceMaterials, openLevelsMaterials))

# open explanation page by availability category
data = data %>% 
  mutate(ExplanationPage = ifelse(is.na(ExplanationPage), "", ExplanationPage)) %>% 
  rowwise() %>% 
  mutate(ExplanationPage = ifelse(ExplanationPage == "", "Hidden or Not Applicable", ifelse(sum(str_detect(ExplanationPage, OADomains)) > 0, "Reliable Open Access     ", "Open     "))) %>% 
  mutate(ExplanationPage = factor(ExplanationPage, openLevelsMaterials))

theme_steve = 
  theme_classic(28) +
  theme(
    axis.line.x = element_blank(),
    axis.text.y = element_text(),
    axis.ticks.y = element_blank(),
    legend.justification = "left",
    legend.margin = margin(3, 0, -5, 0),
    legend.position="top", 
    legend.spacing = unit(10, "lines"),
    legend.title=element_blank(),
    legend.text = element_text(margin = margin(l=.5, r=2, unit="char")),
    plot.subtitle = element_text(family="Segoe UI"),
    plot.caption = element_text(family="Segoe UI Semilight"),
    text = element_text(family="Segoe UI Semilight"),
    title = element_text(family="Segoe UI Semibold"))

theme_steve_xline = theme_steve + theme(axis.line.x = element_line(size=1))

write.csv(data,"openaccessvis-all-processed.csv", row.names = TRUE)

###########################################################


####### Overview #######
plot1 = data %>% 
  group_by(openCategory) %>% 
  summarize(count = n()) %>% 
ggplot() +
  aes(x=1, y=count, alpha=fct_rev(openCategory)) +
  geom_hline(yintercept = nrow(data)/2, linetype = "dotted") +
  geom_col(color = "black", width=1, position = position_stack(.5)) +
  geom_text(aes(label=count, y=count), alpha = 1, size=6, family="Segoe UI", position = position_stack(.5, reverse=TRUE)) +
  coord_flip() + 
  scale_alpha_discrete(breaks = fct_rev(openLevels), range = c(0, 1)) +
  scale_x_discrete(expand = c(0,0), breaks = venueLevels) +
  scale_y_continuous(expand = c(0,0), breaks=c(nrow(data)/2), labels=c("50%")) +
  labs(alpha = NULL, x=NULL, y=NULL,
       title = "Availability of Papers"
  ) +
  theme_steve
plot1
ggsave("open overview.png", width = 10, height = 1.75, scale = 1)

####### Overview Materials #######
plot1Materials = data %>% 
group_by(SourceMaterials) %>% 
summarize(count = n()) %>% 
  ggplot() +
  aes(x=1, y=count, alpha=fct_rev(SourceMaterials)) +
  geom_hline(yintercept = nrow(data)/2, linetype = "dotted") +
  geom_col(color = "black", width=1, position = position_stack(.5)) +
  geom_text(aes(label=count, y=count), alpha = 1, size=6, family="Segoe UI", position = position_stack(.5, reverse=TRUE)) +
  coord_flip() + 
  scale_alpha_discrete(breaks = fct_rev(openLevelsMaterials), range = c(0, 1)) +
  scale_x_discrete(expand = c(0,0), breaks = venueLevels) +
  scale_y_continuous(expand = c(0,0), breaks=c(nrow(data)/2), labels=c("50%")) +
  labs(alpha = NULL, x=NULL, y=NULL,
       title = "Availability of Materials"
  ) +
  theme_steve
plot1Materials
ggsave("open overview data.png", dpi=100, width=10.00, height=1.75, scale=1)


####### Overview Data #######
plot1Data = data %>% 
  group_by(Data) %>% 
  summarize(count = n()) %>% 
  ggplot() +
  aes(x=1, y=count, alpha=fct_rev(Data)) +
  geom_hline(yintercept = nrow(data)/2, linetype = "dotted") +
  geom_col(color = "black", width=1, position = position_stack(.5)) +
  geom_text(aes(label=count, y=count), alpha = 1, size=6, family="Segoe UI", position = position_stack(.5, reverse=TRUE)) +
  coord_flip() + 
  scale_alpha_discrete(breaks = fct_rev(openLevelsMaterials), range = c(0, 1), drop = FALSE) +
  scale_x_discrete(expand = c(0,0), breaks = venueLevels) +
  scale_y_continuous(expand = c(0,0), breaks=c(nrow(data)/2), labels=c("50%")) +
  labs(alpha = NULL, x=NULL, y=NULL,
       title = "Availability of Data"
  ) +
  theme_steve
plot1Data
ggsave("open overview data.png", dpi=100, width=10.00, height=1.75, scale=1)


####### Overview Explanation #######
plot1Explanation = data %>% 
  group_by(ExplanationPage) %>% 
  summarize(count = n()) %>% 
  ggplot() +
  aes(x=1, y=count, alpha=fct_rev(ExplanationPage)) +
  geom_hline(yintercept = nrow(data)/2, linetype = "dotted") +
  geom_col(color = "black", width=1, position = position_stack(.5)) +
  geom_text(aes(label=count, y=count), alpha = 1, size=6, family="Segoe UI", position = position_stack(.5, reverse=TRUE)) +
  coord_flip() + 
  scale_alpha_discrete(breaks = fct_rev(openLevelsMaterials), range = c(0, 1), drop = FALSE) +
  scale_x_discrete(expand = c(0,0), breaks = venueLevels) +
  scale_y_continuous(expand = c(0,0), breaks=c(nrow(data)/2), labels=c("50%")) +
  labs(alpha = NULL, x=NULL, y=NULL,
       title = "Availability of Explanations"
  ) +
  theme_steve
plot1Explanation
ggsave("open overview explanation.png", dpi=100, width=10.00, height=1.75, scale=1)

library(patchwork)
plot1 + plot_spacer() + plot1Materials + plot_spacer() + plot1Data + plot_spacer() + plot1Explanation + plot_layout(ncol=1, heights = c(1, 0.2, 1, 0.2, 1, 0.2, 1))
ggsave("open overview all.png", dpi=200, width=10.00, height=9, scale=1.4)

####### By Review Venue #######
plot2 = data %>% 
  group_by(openCategory, ReviewVenue) %>% 
  summarize(count = n()) %>% 
ggplot() +
  aes(x=fct_rev(ReviewVenue), y=count, fill=ReviewVenue, color=ReviewVenue, alpha=fct_rev(openCategory)) +
  geom_col(position = position_stack(.5), size=1) +
  geom_text(aes(label=count, y=count), color = "black", alpha = .5, size=6, family="Segoe UI", position = position_stack(.5, reverse=TRUE)) +
  coord_flip(clip = "off") + 
  scale_alpha_discrete(breaks = fct_rev(openLevels), range = c(0, 1)) +
  scale_x_discrete(breaks = venueLevels, expand=expand_scale(mult = 0, add=c(.54, 0))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,60)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  guides(
    color = "none", 
    fill = "none",
    alpha = guide_legend(override.aes = list(color = "#666666"))
  ) +
  labs(alpha = NULL, x=NULL, y=NULL,
    title = "Open vs Hidden Papers by Review Venue"
  ) +
  theme_steve_xline + theme(axis.line.y = element_blank())
plot2
ggsave("open by venue.png", dpi=200, width=10.00, height=5.25, scale=1.4)


plot2Fill = data %>% 
  group_by(openCategory, ReviewVenue) %>% 
  summarize(count = n()) %>% 
ggplot() +
  aes(x=fct_rev(ReviewVenue), y=count, fill=ReviewVenue, color=ReviewVenue, alpha=fct_rev(openCategory)) +
  geom_hline(yintercept = .5, linetype = "dotted", alpha = 0.5) +
  geom_col(position = position_fill(.5)) +
  geom_text(aes(label=count, y=count), color = "black", alpha = .5, size=6, family="Segoe UI", position = position_fill(.5, reverse=TRUE)) +
  coord_flip() + 
  scale_alpha_discrete(breaks = fct_rev(openLevels), range = c(0, 1)) +
  scale_x_discrete(breaks = venueLevels) +
  scale_y_continuous(expand = c(0,0)) + #expand_limits(y = 60) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  guides(color = "none", fill = "none") +
  labs(alpha = NULL, x=NULL, y=NULL,
       title = "Open vs Hidden Papers by Review Venue"
  ) +
  theme_steve_xline + theme(plot.margin = margin(1, 1, 1, 8.5, "lines"))
plot2Fill
CairoPNG("open by venue fill.png", 1000, 500)
plot2Fill
dev.off()


####### Data by author #######
authorData = data %>% 
  separate_rows(Authors, sep = ", ") %>% 
  left_join(duplicates) %>% 
  mutate(Author = ifelse(is.na(Authors2), Authors, Authors2)) %>% 
  group_by(Author) %>% 
  summarise(
    count = n(), 
    closedCount = sum(isClosedAccess),
    openCount = sum(!isClosedAccess),
    fullyOpenCount = sum(isFullyOpen),
    propOpenAccess = openCount/n(),
    propFullyOpen = fullyOpenCount/n())


authorData %>% 
  filter(fullyOpenCount > 1)

authorData %>% 
  filter(closedCount > 1)

######## Author Most Fully Open #########
plot3a = authorData %>% 
  filter(fullyOpenCount > 1) %>% 
  arrange(fullyOpenCount, desc(Author)) %>% 
  mutate(Author = fct_inorder(Author)) %>% 
  ggplot() +
  aes(x=Author, y=fullyOpenCount) +
  geom_col(color="black") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) + expand_limits(y = 6) +
  labs(x=NULL, y=NULL,
       title = "Authors with the Most Papers in an Open Access Repository"
  )  +
  theme_steve_xline
plot3a
CairoPNG("open fully by author.png", 1000, 500)
last_plot()
dev.off()

######### Author Most Open #########
plot3b = authorData %>% 
  filter(openCount > 2) %>% 
  arrange(openCount, desc(Author)) %>% 
  mutate(Author = fct_inorder(Author)) %>% 
ggplot() +
  aes(x=Author, y=openCount) +
  geom_col(color="black", alpha=.5) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x=NULL, y=NULL,
       title = "Authors with the Most Open Papers"
  )  +
  theme_steve_xline
plot3b
CairoPNG("open by author.png", 1000, 500)
last_plot()
dev.off()
  
######### Author most closed #########
plot3c = authorData %>% 
  filter(closedCount >= 2) %>% 
  arrange(closedCount, desc(Author)) %>% 
  mutate(Author = fct_inorder(Author)) %>% 
ggplot() +
  aes(x=Author, y=closedCount) +
  geom_col(color="black", fill="transparent") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0,6)) +
  labs(x=NULL, y=NULL,
       title = "Authors with the Most Hidden Papers",
       caption = "\n"
  )  +
  theme_steve_xline
plot3c
CairoPNG("open by author 2.png", 1000, 425)
last_plot()
dev.off()

############ Cowplot ####################

cowplot::plot_grid(plot3a + theme(plot.margin = margin(1, 1, 4, 1, "lines")),  
                   plot3b,  
                   plot3c + theme(plot.margin = margin(4, 1, 1, 1, "lines")), 
                   ncol=1, align="v", rel_heights=c(1, 1.3, 1.3))
ggsave("open.svg", dpi = 100, width=10, height=15)
CairoPNG("open.png", 1000, 1500)
last_plot()
dev.off()

############# PIE ###############

plotPie1 = data %>% 
group_by(openCategory) %>% 
summarize(count = n()) %>% 
ggplot() +
aes(x=1, y=count, alpha=fct_rev(openCategory)) +
# geom_hline(yintercept = nrow(data)/2, linetype = "dotted") +
geom_col(color = "black", width=1, position = position_stack(.5)) +
geom_text(aes(label=count), alpha = 1, size=6, family="Segoe UI", position = position_stack(.5, reverse=TRUE)) +
coord_polar(theta = "y") +
scale_alpha_discrete(breaks = fct_rev(openLevels), range = c(0, 1)) +
scale_x_discrete(expand = c(0,0), breaks = venueLevels) +
scale_y_continuous(expand = c(0,0), breaks=c(nrow(data)/2), labels=c("50%")) +
labs(alpha = NULL, x=NULL, y=NULL,
     title = "Open vs Hidden Papers"
     #subtitle = "More than half of papers are available, but only a fraction are in reliable open access repositories"#,
     #caption = "Steve Haroz - @sharoz"
) +
theme_steve
plotPie1
output = CairoPNG("open overall pie.png", 1000, 1000)
plotPie1
dev.off(output)


plotPie2 = data %>% 
  group_by(openCategory, ReviewVenue) %>% 
  summarize(count = n()) %>% 
  ggplot() +
  aes(x=1, y=count, fill=ReviewVenue, color=ReviewVenue, alpha=fct_rev(openCategory)) +
  geom_col(position = position_fill(.5)) +
  geom_text(aes(label=count), color = "black", alpha = .5, size=6, family="Segoe UI", position = position_fill(.5, reverse=TRUE)) +
  coord_polar(theta = "y") +
  scale_alpha_discrete(breaks = fct_rev(openLevels), range = c(0, 1)) +
  #scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  guides(color = "none", fill = "none") +
  facet_grid(. ~ ReviewVenue) +
  labs(alpha = NULL, x=NULL, y=NULL,
       title = "Open vs Hidden Papers by Review Venue"
  ) +
  theme_steve + theme(axis.line.y = element_blank(), axis.text = element_blank(),
                      #panel.background = element_rect("#00000008"),
                      #plot.margin = margin(0, 10, 10, 10),
                      strip.background = element_rect(color = "transparent"), 
                      strip.text = element_text(family = "Segoe UI Semibold")
                      )
plotPie2

output = CairoPNG("open by venue pie.png", 1000, 400)
plotPie2
dev.off(output)