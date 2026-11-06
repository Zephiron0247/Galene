# Statistical Analysis & Hypothesis Testing: Plain-English Findings

This document explains the statistical tests performed in `R/05_statistics.R` in simple, everyday language, detailing the research question, the test used, the empirical result, and its operational meaning for Citi Bike rebalancing.

---

## 1. Trip Duration Distribution (Normality Test)
- **Question**: Are Citi Bike trip durations normally distributed (forming a symmetric bell curve)?
- **Test Used**: Shapiro-Wilk test on a random sample of 5,000 trips, plus sample skewness across all trips.
- **Result**: The data strongly rejects a normal distribution ($W = 0.82$, $p < 2.2 \times 10^{-16}$) with positive right skewness of 0.96.
- **Everyday Meaning**: Most bike rides are short utilitarian commuter hops (median ~9 minutes), but a long tail of leisure rides stretches up to 30+ minutes. Because the bell curve assumption fails, we must rely on non-parametric median-based tests rather than standard t-tests.

---

## 2. Trip Duration: Annual Members vs. Casual Riders
- **Question**: Do casual single-ride/day-pass users ride longer than annual commuter subscribers?
- **Test Used**: Wilcoxon rank-sum test (Mann-Whitney U) on a 300,000-trip sample, accompanied by 2,000 bootstrap resamples on 20,000 trips to generate a 95% confidence interval for the median difference.
- **Result**: Casual riders ride significantly longer than members ($W = 7.14 \times 10^9$, $p < 2.2 \times 10^{-16}$; rank-biserial effect size $r = -0.32$). The bootstrap 95% confidence interval shows casual riders ride **between 2.96 and 3.65 minutes longer** in median duration.
- **Everyday Meaning**: Annual members use Citi Bike for efficient point-to-point transit (median 8.6 minutes), whereas casual riders take more exploratory or leisurely trips (median 11.9 minutes). Docks near tourist corridors hold bikes longer before returning them to circulation.

---

## 3. Trip Duration Across Days of the Week
- **Question**: Does ride duration change depending on the day of the week?
- **Test Used**: Kruskal-Wallis non-parametric one-way ANOVA across all seven days, with epsilon-squared ($\epsilon^2$) effect size.
- **Result**: Significant variation exists across days ($H = 4310.2$, $df = 6$, $p < 2.2 \times 10^{-16}$), with an overall effect size of $\epsilon^2 = 0.014$.
- **Everyday Meaning**: Ride duration increases on Saturdays and Sundays compared to weekdays, but weekday trip durations remain highly consistent Monday through Friday. Rebalancing teams have more predictable turnover windows on weekdays.

---

## 4. Rider Membership vs. Bike Hardware Choice
- **Question**: Do members and casual riders choose classic pedal bikes versus electric pedal-assist bikes in different proportions?
- **Test Used**: Chi-Square Test of Independence with Cramer's V effect size.
- **Result**: Significant association ($\chi^2 = 5028.6$, $p < 2.2 \times 10^{-16}$; Cramer's $V = 0.129$).
- **Everyday Meaning**: Both groups embrace e-bikes, but casual riders utilize classic pedal bikes proportionally more often, while members strategically select e-bikes for longer bridge crossings and speed.

---

## 5. Citywide Demand: Weekdays vs. Weekends
- **Question**: Does the city as a whole experience significantly different total hourly trip volumes on weekends compared to weekdays?
- **Test Used**: Wilcoxon rank-sum test on the 720 hourly records of September 2024.
- **Result**: The overall median hourly trip volume does not significantly differ between weekdays and weekends ($W = 54,611$, $p = 0.94$; median ~6,300 trips/h).
- **Everyday Meaning**: Although weekday demand concentrates into sharp commuter peaks (08:00 and 17:00-18:00) and weekend demand spreads into a broad midday leisure plateau, total daily system volume remains comparable, requiring sustained rebalancing capacity seven days a week.

---

## 6. Weather Drivers of Bike Demand (Spearman Rank Correlations)
- **Question**: How strongly do temperature, rain, wind, and humidity correlate with hourly ridership?
- **Test Used**: Pairwise Spearman rank correlation coefficients ($\rho$).
- **Result**:
  - Temperature: positive correlation ($\rho = +0.50$)
  - Precipitation: negative correlation ($\rho = -0.18$)
  - Relative Humidity: negative correlation ($\rho = -0.42$)
  - Wind Speed: minor positive diurnal correlation ($\rho = +0.14$, tracking peak afternoon wind coinciding with commute rush)
- **Everyday Meaning**: Warm, clear, dry weather drives ridership surges. Humid and rainy conditions sharply suppress bike demand.

---

## 7. Weather Regression & Incidence Rate Ratios (Negative Binomial GLM)
- **Question**: How much does each specific weather change multiply or decrease hourly demand, holding time-of-day and calendar days constant?
- **Model Used**: Poisson GLM yielded extreme overdispersion (residual deviance / df = 547.25), proving that counts are clumped. We fitted a Negative Binomial regression (`MASS::glm.nb`) to calculate Incidence Rate Ratios (IRR).
- **Empirical Readings**:
  - **Temperature: Each additional +1°C changes expected hourly trips by +3.61% (IRR = 1.036, p = 7.66e-09).**
  - **Precipitation: Each additional +1 mm of rain changes expected hourly trips by -7.11% (IRR = 0.929, p = 0.295).**
  - **Wind Speed: Each additional +1 km/h of wind changes expected hourly trips by -0.42% (IRR = 0.996, p = 0.310).**
  - **Relative Humidity: Each +1% higher humidity changes expected hourly trips by -0.19% (IRR = 0.998, p = 0.139).**
- **Everyday Meaning**: Temperature is the single strongest positive driver (+3.6% per degree C), while rain is the single strongest suppressor (-7.1% per mm of precipitation). On wet rainy days, rebalancing vans can focus on station repairs and preventative maintenance rather than urgent dock turnover.
