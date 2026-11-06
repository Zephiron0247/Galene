# Machine Learning Rebalancing Benchmark: Plain-English Interpretation

## Executive Summary
This chapter evaluates **9 supervised machine learning classifiers** (alongside a naive baseline and an attempted SVM) to predict 2-hour station rebalancing states across New York City:
- **Drainer** ($\Delta \le -5$): Stations experiencing severe net inventory depletion ($\le -5$ bikes in 2 hours), signaling potential van replenishment needs.
- **Collector** ($\Delta \ge 5$): Stations experiencing heavy net inbound accumulation ($\ge +5$ bikes in 2 hours), signaling potential dock congestion.
- **Balanced** ($-5 < \Delta < 5$): Stations maintaining stable equilibrium requiring no rebalancing intervention.

> **Operational Measurement Note**:
> Rebalancing states represent **net-flow imbalance (arrivals minus departures in a 2-hour window)**. Citi Bike does not publish historical bike counts per station, so true empty/full events cannot be observed from trip data.

---

## 1. Overview & Performance of the Supervised Architectures
A diverse suite of 9 operational classifiers was trained alongside the baseline:
1. **Baseline (Null Model)**: A trivial heuristic that blindly guesses the majority class ("Balanced") every time. While its raw accuracy is 85.84% due to class imbalance, it provides **0.0% recall on Drainers and Collectors**, yielding Macro ROC-AUC = 0.5000, Macro PR-AUC = 0.3333, and Macro F1 = 0.3079 (Balanced F1 = 0.9238, Drainer F1 = 0, Collector F1 = 0).
2. **Random Forest (Ranger)**: An ensemble of 300 decorrelated trees with random feature subsampling. Top-performing architecture: Macro ROC-AUC = 0.8988, Balanced Accuracy = 0.7939, Macro F1 = 0.5946.
3. **XGBoost (Extreme Gradient Boosting)**: Gradient-boosted decision trees sequentially correcting previous residuals: Macro ROC-AUC = 0.8890 (tuned to 0.8955), Balanced Accuracy = 0.7851, Macro F1 = 0.5809.
4. **Neural Network (Multilayer Perceptron)**: Single-hidden-layer feedforward network with 8 hidden units: Macro ROC-AUC = 0.8871, Balanced Accuracy = 0.7821.
5. **Bagged Trees**: 25 unpruned bootstrap-aggregated decision trees: Macro ROC-AUC = 0.8818, Balanced Accuracy = 0.7832.
6. **Multinomial Logistic Regression**: Fast regularized linear log-odds classifier (16.6s train time): Macro ROC-AUC = 0.8790, Balanced Accuracy = 0.7408.
7. **Linear Discriminant Analysis (LDA)**: Classical Gaussian generative classifier: Macro ROC-AUC = 0.8644, Balanced Accuracy = 0.7049.
8. **K-Nearest Neighbors (KNN, k=15)**: Instance-based non-parametric classifier: Macro ROC-AUC = 0.8275, Balanced Accuracy = 0.7234.
9. **Decision Tree (CART)**: Single hierarchical decision tree: Macro ROC-AUC = 0.8172, Balanced Accuracy = 0.7881.
10. **Naive Bayes**: Conditional independence probabilistic classifier: Macro ROC-AUC = 0.7855, Balanced Accuracy = 0.5567.
11. **Support Vector Machine (SVM RBF)**: Failed. `kernlab::ksvm` encountered a numerical failure in the Platt scaling line search during pairwise probability calibration, recorded as "Failed".

---

## 2. Why Raw Accuracy and the Baseline are Severely Deceptive
In the NYC Citi Bike system, approximately **85.8% of all 2-hour station windows are in a Balanced state**, while critical inventory imbalances (Drainers and Collectors) represent only **~7.1% each**.

- If an operator evaluates models purely on **Raw Accuracy**, the dumb Null Baseline looks deceptive with **85.84% accuracy**.
- However, the baseline model would allow **100% of severe net-flow imbalance events to occur**, dispatching zero rebalancing support.
- Therefore, operational success must be measured by **Macro ROC-AUC**, **Balanced Accuracy**, and **Drainer/Collector Recall**, which evaluate the system's true ability to detect net inventory shocks.

---

## 3. What Hyperparameter Tuning Changed
Tuning was conducted using **rolling chronological cross-validation** (Split 1: Days 1-7 predicting Days 8-14; Split 2: Days 1-14 predicting Days 15-21) to eliminate future leakage:
- **XGBoost**: Tuning improved Macro ROC-AUC by **+0.0065** (from 0.8890 to 0.8955). Tuning `tree_depth = 6`, `learn_rate = 0.036`, `trees = 271`, and `min_n = 2` curtailed early overfitting on localized weather fluctuations, delivering stable probability calibration.
- **Random Forest**: Tuning did **not** improve performance (-0.0018, from 0.8988 to 0.8970). The default Ranger parameters (`mtry = 4`, `min_n = 5`) were already optimal for this tabular spatio-temporal feature space.

---

## 4. Statistical Validation: Bootstrap 95% Confidence Intervals
A 100-replicate bootstrap analysis on the test benchmark revealed:
- **Random Forest**: Mean Macro ROC-AUC = 0.8972 (95% CI: [0.8932, 0.9006])
- **XGBoost**: Mean Macro ROC-AUC = 0.8956 (95% CI: [0.8915, 0.8992])
- **Statistical Comparison**: Random Forest and tuned XGBoost are **statistically tied** with overlapping bootstrap 95% confidence intervals. Random Forest was selected as the final production deployment model due to its marginally higher mean score and greater stability without requiring complex learning rate schedules.

---

## 5. Full 9-Day Out-of-Sample Performance & Alert Quality
Scored on all **239,112 windows** across New York City from September 22 to September 30 (the full 9-day held-out test horizon), the final Random Forest model achieved:
- **Macro ROC-AUC**: **0.9005**
- **Balanced Accuracy**: **0.7977**
- **Raw Accuracy**: **0.7840**

### Alert Quality Breakdown (Full Test Horizon)
The operational trade-off between recall and false alarms is summarized below:

| Rebalancing Class | Actual Windows | True Positives (TP) | False Positives (FP) | False Negatives (FN) | Precision | Recall | F1-Score |
|:---|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| **Drainer** ($\Delta \le -5$) | 16,989 (7.1%) | 11,837 | 22,077 | 5,152 | 34.90% | **69.67%** | 46.52% |
| **Collector** ($\Delta \ge 5$) | 16,842 (7.0%) | 11,440 | 23,276 | 5,402 | 32.95% | **67.93%** | 44.38% |
| **Balanced** ($-5 < \Delta < 5$) | 205,281 (85.9%) | 164,197 | 6,285 | 41,084 | 96.31% | 79.99% | 87.39% |

**Operational Takeaway**:
The production model captures approximately **70% of real Drainer and Collector net-flow imbalance windows** ahead of time (Recall = 69.7% for Drainers, 67.9% for Collectors). However, because the system is dominated by Balanced windows (~85.9%), approximately two out of three rebalancing alerts are false alarms (Precision ~33–35%). In practice, fleet dispatchers should use predicted probabilities to prioritize the highest-confidence alerts rather than treating binary threshold outputs equally.
