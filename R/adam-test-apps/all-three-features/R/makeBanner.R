library(ggplot2)
library(ggthemes)

# Dummy data for survival curves
set.seed(123)
time <- c(seq(0, 150, by = 10), seq(0, 120, by = 10), seq(0, 180, by = 10))
survival <- c(runif(16, 0.4, 1), runif(13, 0.3, 0.9), runif(19, 0.5, 1))
group <- rep(c("Group 1", "Group 2", "Group 3"), times = c(16, 13, 19))
df <- data.frame(time, survival, group)

# Create banner
ggplot(df, aes(x = time, y = survival, color = group)) +
  geom_step(linewidth = 1.5) +
  scale_color_manual(values = c("#ff757c", "#377EB8", "#4DAF4A")) +
  labs(title = "Survival Analysis: Explore Clinical Outcomes") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "#f5f5f5"),
    text = element_text(family = "Montserrat", color = "#6a0033", size = 20),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  annotate("text", x = 75, y = 0.1, label = "Kaplan-Meier Curves", size = 5, color = "#6a0033")

# Save as banner
ggsave("survival_analysis_banner.png", width = 8, height = 2, dpi = 100)

library(ggplot2)

# Dummy data for t-SNE
set.seed(456)
tsne1 <- rnorm(100, mean = c(-10, 0, 10), sd = 3)
tsne2 <- rnorm(100, mean = c(-5, 5, 0), sd = 3)
group <- rep(c("Cluster 1", "Cluster 2", "Cluster 3"), times = c(30, 40, 30))
df <- data.frame(tsne1, tsne2, group)

# Create banner
ggplot(df, aes(x = tsne1, y = tsne2, color = group)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("#e4016e", "#ff527b", "#01bbb7")) +
  labs(title = "t-SNE: Visualize Tumor Heterogeneity") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "#f5f5f5"),

    text = element_text(family = "Montserrat", color = "#6a0033", size = 20),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  annotate("text", x = 0, y = -15, label = "Sample Clustering", size = 5, color = "#6a0033")

# Save as banner
ggsave("tsne_banner.png", width = 8, height = 2, dpi = 100)

library(ggplot2)

# Dummy data for boxplots
set.seed(789)
gene <- rep(c("Gene A", "Gene B", "Gene C"), each = 20)
expression <- c(rnorm(20, 5, 1), rnorm(20, 7, 1.5), rnorm(20, 6, 1.2))
df <- data.frame(gene, expression)

# Create banner
ggplot(df, aes(x = gene, y = expression, fill = gene)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "#6a0033") +
  scale_fill_manual(values = c("#ff757c", "#5292ff", "#8ab5ff")) +
  labs(title = "mRNA Expression: Uncover Gene Patterns") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "#f5f5f5"),
    text = element_text(family = "Montserrat", color = "#6a0033", size = 20),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  annotate("text", x = 2, y = 2, label = "Gene Expression Levels", size = 5, color = "#6a0033")

# Save as banner
ggsave("mrna_expression_banner.png", width = 8, height = 2, dpi = 100)