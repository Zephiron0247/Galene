# Machine Learning Rebalancing Benchmark: Plain-English Interpretation

## Executive Summary
This chapter evaluates **10 machine learning architectures** (alongside a naive baseline) to predict 2-hour station rebalancing states across New York City:
- **Drainer** ($\Delta \le -5$): Stations suffering severe net inventory depletion requiring van/truck replenishment.
- **Collector** ($\Delta \ge 5$): Stations overwhelmed by inbound commuter arrivals requiring dock clearance.
- **Balanced** ($-5 < \Delta < 5$): Stations maintaining stable equilibrium requiring no rebalancing intervention.

---

## 1. One-Sentence Overview & Performance of the 10 Architectures

1. **Baseline (Null Model)**: A trivial heuristic that blindly guesses the majority class ("Balanced") every time; it achieves 85.8% accuracy by cheating on class imbalance while delivering **0.0% recall on Drainers and Collectors** (Macro ROC-AUC = 0.500).
2. **Multinomial Logistic Regression**: A linear probabilistic classifier that applies regularized log-odds across all three classes; it is fast (trained in 16.64s) and provides a solid interpretable baseline (Macro ROC-AUC = 0.879).
3. **Linear Discriminant Analysis (LDA)**: A classical Gaussian generative classifier assuming shared covariance across classes; it offers rapid inference but struggles slightly with nonlinear boundary shifts (Macro ROC-AUC = 0.864).
4. **Naive Bayes**: A probabilistic classifier assuming strict feature independence given the station state; it performs adequately on weather features but suffers due to strong correlation between spatial subway proximity and historical lags (Macro ROC-AUC = 0.785).
5. **K-Nearest Neighbors (KNN, k=15)**: A non-parametric instance-based algorithm that classifies a station window by voting among its 15 most similar historical scenarios; it achieves reasonable accuracy but carries heavy computational overhead during real-time retrieval (Macro ROC-AUC = 0.828).
6. **Decision Tree (CART)**: A single tree-structured set of hierarchical recursive splits; it is intuitive and transparent but prone to high variance and sharp boundary artifacts (Macro ROC-AUC = 0.817).
7. **Bagged Trees**: An ensemble of 25 unpruned bootstrap-aggregated decision trees that averages out individual tree variance; it dramatically outperforms the single tree (Macro ROC-AUC = 0.882).
8. **Random Forest (Ranger)**: A sophisticated ensemble of 300 decorrelated trees with random feature subsampling; it excels at capturing complex non-linear spatial-temporal interactions (Macro ROC-AUC = 0.899).
9. **XGBoost (Extreme Gradient Boosting)**: A high-performance gradient-boosted decision tree framework that sequentially corrects the residual errors of preceding trees; it stands as one of the premier performers (Macro ROC-AUC = 0.889).
10. **Support Vector Machine (SVM RBF)**: A kernel-based classifier mapping station features into infinite-dimensional Hilbert space via radial basis functions; it failed Platt scaling calibration during multinomial optimization (Macro ROC-AUC = N/A (Platt scaling optimization failure in kernlab)).
11. **Neural Network (Multilayer Perceptron)**: A single-hidden-layer feedforward network with 8 hidden units and weight decay; it captures smooth non-linear synergies but requires normalized numeric inputs and extensive tuning (Macro ROC-AUC = 0.887).

---

## 2. Why Raw Accuracy and the Baseline are Severely Deceptive
In the real-world Citi Bike system, approximately **84.8% of all 2-hour station windows are in a Balanced state**, while critical inventory bottlenecks (Drainers and Collectors) represent only **~7.6% each**. 

- If an operator evaluates models purely on **Raw Accuracy**, the dumb Null Baseline looks sensational with **85.8% accuracy**.
- However, the baseline model would allow **100% of dock-out and dock-blocking emergencies to occur**, dispatching zero rebalancing trucks.
- Therefore, operational success must be measured by **Macro ROC-AUC**, **Balanced Accuracy**, and **Drainer/Collector Recall**, which evaluate the system's true ability to protect stations from starvation and overflow.

---

## 3. What Hyperparameter Tuning Changed
Tuning was conducted using **rolling chronological cross-validation** (Split 1: Days 1-7 predicting Days 8-14; Split 2: Days 1-14 predicting Days 15-21) to eliminate future leakage:
- For **Random Forest**, tuning `mtry = 10` and `min_n = 9` optimized the depth and diversity of individual trees, refining probability estimates on boundary cases.
- For **XGBoost**, tuning `tree_depth = 6`, `learn_rate = 0.036`, `trees = 271`, and `min_n = 2` curtailed early overfitting on localized weather fluctuations, delivering stable probability calibration.

---

## 4. Statistical Validation: Bootstrap 95% Confidence Intervals
A 100-replicate bootstrap analysis on the 40,000-row test benchmark revealed:
- **Random Forest**: Mean Macro ROC-AUC = 0.8972 (95% CI: [0.8932, 0.9006])
- **XGBoost**: Mean Macro ROC-AUC = 0.8956 (95% CI: [0.8915, 0.8992])
- **Interval Overlap**: **Yes (models perform comparably within sampling variance)**

---

## 5. Full 9-Day Out-of-Sample Performance
Scored on all **239,112 windows** across New York City from September 22 to September 30, the winning architecture (**Random Forest**) achieved:
- **Macro ROC-AUC**: **0.9005**
- **Balanced Accuracy**: **0.7977**
- **Macro F1-Score**: **0.5943**
These results prove that combining station commute personalities, subway proximity, historical rolling lags, and forecasted meteorology enables robust, proactive inventory rebalancing dispatch.

