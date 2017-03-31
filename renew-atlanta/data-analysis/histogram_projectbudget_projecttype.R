library(ggplot2); library(scales)

ggplot(project_details_df %>% 
         filter(project_budget <= 1000000 & !is.na(project_type))) + 
  geom_histogram(aes(project_budget, fill = project_type)) + 
  facet_wrap(~project_type) + guides(fill = FALSE) + 
  scale_x_continuous(labels = comma_format()) + 
  xlab("Project Budget") + ylab("# Projects") + 
  ggtitle("Renew Atlanta Projects: Histogram of Project Budget", 
          subtitle = "Budget less than $1M") +
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 10))

ggsave(filename = "renew-atlanta/data-analysis/histogram_projectbudget_projecttype.png",
       width = 7, height = 5)
