#' PLOTTING GAM CRPS SKILL
#' Caleb Robbins & Cole Brookson
#' April 2025

# 1. [SET UP ] -----------------------------------------------------------------
source(here::here("./src/R/00-global-funs.R"))
library(ggplot2)
# read in data
comb_preds_sh <- readr::read_csv(here::here(
  "./data/GAM_crps_skill_summarized.csv"
))

comb_preds_sh_summarized <- comb_preds_sh |>
  dplyr::group_by(horizon, variable) |>
  dplyr::summarize(median_est = stats::median(estimate)) |> # CV here, maybe median?
  dplyr::ungroup() |>
  dplyr::mutate(
    variable = forcats::fct_recode(
      variable,
      Temperature = "temperature",
      Oxygen = "oxygen",
      RCC90 = "rcc_90",
      GCC90 = "gcc_90",
      LE = "le",
      NEE = "nee",
      `Chl. A` = "chla"
    )
  )
# 2. [PLOT] --------------------------------------------------------------------
comb_preds_sh_summarized |>
  dplyr::mutate(variable = forcats::fct_reorder(
    variable,
    dplyr::desc(median_est)
  ))
# across-site variation in CRPS-based skill
ggplot(
  comb_preds_sh_summarized |>
    dplyr::mutate(variable = forcats::fct_reorder(
      variable,
      dplyr::desc(median_est)
    )),
  aes(x = variable, y = median_est)
) +
  geom_point() +
  facet_wrap(. ~ horizon) +
  ylab("Coefficient of Variation") +
  theme(axis.text.x = element_text(angle = 90))

color_palette <- MoMAColors::moma.colors(
  n = 7,
  palette = "Flash",
  type = "discrete"
)

p <- ggplot2::ggplot(
  comb_preds_sh_summarized |>
    dplyr::mutate(
      variable = forcats::fct_reorder(variable, dplyr::desc(median_est))
    ),
  ggplot2::aes(
    x = variable, y = median_est,
    group = base::factor(horizon),
    fill = base::factor(horizon)
  )
) +
  ggplot2::geom_point(
    shape = 21, colour = "black",
    position = ggplot2::position_dodge(width = 0.6), size = 4
  ) +
  ggplot2::scale_fill_manual("Horizon (days)", values = color_palette) +
  ggplot2::guides(
    color = ggplot2::guide_legend(title = "Horizon (days)")
  ) +
  ggplot2::labs(x = "Variable", y = "CRPS Skill (ML vs Climatological)") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(size = 16),
    axis.text = ggplot2::element_text(size = 12),
    axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.6)
  ) +
  theme_base()

ggplot2::ggsave(
  filename = here::here("./figs/GAM_crps_skill.png"),
  plot = p,
  width = 11,
  height = 5,
  dpi = 300
)
