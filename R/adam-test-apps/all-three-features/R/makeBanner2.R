library(ggplot2)
library(ggthemes)

###############################################################################
# Kaplan-Meier Survival Banner
###############################################################################
set.seed(123)
time <- c(seq(0, 150, by = 10), seq(0, 120, by = 10), seq(0, 180, by = 10))
survival <- c(runif(16, 0.4, 1), runif(13, 0.3, 0.9), runif(19, 0.5, 1))
group <- rep(c("Group 1", "Group 2", "Group 3"), times = c(16, 13, 19))
df_surv <- data.frame(time, survival, group)

surv_plot <- ggplot(df_surv, aes(x = time, y = survival, color = group)) +
    geom_step(linewidth = 1.5, lineend = "round") +
    scale_color_manual(values = c("#ff757c", "#377EB8", "#4DAF4A")) +
    labs(title = "Survival Analysis: Explore Clinical Outcomes") +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "#f5f5f5"),
        text = element_text(family = "Montserrat", color = "#6a0033", size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none"
    ) +
    annotate("text",
        x = 75, y = 0.15,
        label = "",
        size = 5, color = "#6a0033"
    )

ggsave(
    filename = "survival_analysis_banner.png",
    plot = surv_plot,
    width = 8, height = 2, dpi = 100
)

###############################################################################
# t-SNE Banner
###############################################################################
set.seed(456)
tsne1 <- rnorm(100, mean = c(-10, 0, 10), sd = 3)
tsne2 <- rnorm(100, mean = c(-5, 5, 0), sd = 3)
group <- rep(c("Cluster 1", "Cluster 2", "Cluster 3"), times = c(30, 40, 30))
df_tsne <- data.frame(tsne1, tsne2, group)

tsne_plot <- ggplot(df_tsne, aes(x = tsne1, y = tsne2, color = group)) +
    geom_point(size = 3, alpha = 0.8) +
    scale_color_manual(values = c("#e4016e", "#ff527b", "#01bbb7")) +
    labs(title = "t-SNE: Visualize Tumor Heterogeneity") +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "#f5f5f5"),
        text = element_text(family = "Montserrat", color = "#6a0033", size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none"
    ) +
    annotate("text",
        x = 0, y = -13,
        label = "",
        size = 5, color = "#6a0033"
    )

ggsave(
    filename = "tsne_banner.png",
    plot = tsne_plot,
    width = 8, height = 2, dpi = 100
)

###############################################################################
# mRNA Expression Banner
###############################################################################
set.seed(789)
gene <- rep(c("Gene A", "Gene B", "Gene C"), each = 20)
expression <- c(rnorm(20, 5, 1), rnorm(20, 7, 1.5), rnorm(20, 6, 1.2))
df_expr <- data.frame(gene, expression)

expr_plot <- ggplot(df_expr, aes(x = gene, y = expression, fill = gene)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, color = "#6a0033") +
    geom_jitter(width = 0.2, alpha = 0.6, color = "#6a0033") +
    scale_fill_manual(values = c("#ff757c", "#5292ff", "#ff8af5")) +
    labs(title = "mRNA Expression: Uncover Gene Patterns") +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "#f5f5f5"),
        text = element_text(family = "Montserrat", color = "#6a0033", size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none"
    ) +
    annotate("text",
        x = 2, y = 2.5,
        label = "",
        size = 5, color = "#6a0033"
    )

ggsave(
    filename = "mrna_expression_banner.png",
    plot = expr_plot,
    width = 8, height = 2, dpi = 100
)