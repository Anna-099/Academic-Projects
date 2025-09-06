📊 Data Science Projects

This repository showcases a collection of data science projects that I completed during my studies using both R and Python. 
Each project explores a unique dataset, applying techniques like regression, classification, forecasting, and clustering.

🔍 Projects Overview
📈 1. Anomaly Detection – Insurance Data
File: Anomaly_Detection_Insurance_Data.ipynb
Summary
Developed an unsupervised learning model to detect unusual claims in insurance data, enabling early fraud detection and risk management.

Business Problem
Insurance companies face significant financial losses due to fraudulent or abnormal claims. Detecting anomalies proactively can reduce costs and improve trust with customers.

Methodology
Data preprocessing and visualization of claims distribution.
Applied Isolation Forest and Z-score analysis to detect outliers.
Evaluated detection thresholds using domain-specific metrics.

Skills & Tools
Python (pandas, numpy, matplotlib, seaborn), Anomaly Detection, Isolation Forest.

Results
Anomaly detection methods flagged a very small fraction (~0.03%) of entries as potential outliers.
Isolation Forest proved most effective in highlighting unusual claim patterns, particularly in variables such as submitted charge amounts and payment discrepancies.
Visual inspections (boxplots and scatterplots) confirmed that the anomalies were concentrated in providers with unusually high billing or service counts.
The approach successfully narrowed down a massive dataset to a manageable subset for fraud investigation, reducing manual review workload.

🛍️ 2. FATER Project – Promotional Campaign Detection
File: FATER_project.R
Summary
Analyzed customer transaction data to identify promotional campaign weeks, segmenting customers for targeted marketing strategies.

Business Problem
Retailers often struggle to measure the true impact of promotional campaigns and to tailor strategies for diverse customer bases.

Methodology
Reduced dimensionality with PCA.
Clustered customers using K-Means.
Classified promotional vs. non-promotional periods with SVM.

Skills & Tools
R (tidyverse, TSA, plotly, anomalize), Machine Learning, Customer Segmentation.

Results
Segmented 30,000+ customers into meaningful behavioral clusters.
Achieved effective anomaly detection with time series decomposition identifying promotional periods through sales volume spikes and price drops.
Applied K-means clustering (3-4 clusters optimal) to segment weekly sales patterns, with promotional weeks consistently clustering separately from baseline periods.
PCA analysis explained 85-99% of variance across products using first 2 dimensions, enabling clear visualization of promotional vs. non-promotional patterns.
Correlation analysis revealed expected negative price-volume relationships for most products, validating promotional detection methodology.
Interactive visualizations created for 15+ products enabling real-time promotional period identification across 3+ years of weekly data.

📰 3. Fake News Detection
File: Fake_News_Detection.ipynb
Summary
Built a text classification model to detect fake news using natural language processing (NLP) and logistic regression.

Problem
Misinformation undermines public trust and poses risks to society. Automated fake news detection can help mitigate its spread.

Methodology
Text preprocessing (tokenization, stopword removal).
Feature extraction using TF-IDF.
Trained and evaluated logistic regression classifier.

Skills & Tools
Python (pandas, nltk, tensorflow, numpy), NLP, Logistic Regression.

Results
Achieved highest accuracy of 98% using MLP neural network on article bodies, outperforming traditional machine learning approaches.
Article bodies consistently outperformed headlines across all models: Logistic Regression (97% vs 80%), Naive Bayes (93% vs 85%), KNN (93% vs 78%).
LSTM models achieved 97% accuracy on bodies and 83% on headlines, demonstrating effective sequential pattern learning for fake news detection.

🧬 4. GLM on Cancer Data
File: GLM_cancer_data.Rmd
Summary
Applied generalized linear models to study factors influencing cancer incidence, uncovering statistical relationships in health data.

Problem
Understanding determinants of cancer incidence is critical for public health resource allocation and preventive strategies.

Methodology
Modeled count data using Poisson GLM.
Applied Binomial GLM for binary incidence outcomes.
Evaluated model fit using deviance and AIC.

Skills & Tools
R (ggplot2, MASS, dplyr), Statistical Modeling, GLM

Results
Successfully validated negative binomial model through comprehensive residual diagnostics across 4 residual types (Pearson, deviance, quantile, studentized).
Quantile residuals demonstrated optimal performance for negative binomial GLM, validating model assumptions with near-normal distribution.
The diagnostic analysis demonstrates that the negative binomial GLM provides an appropriate statistical framework for modeling the cancer count data, with all key assumptions satisfied and no evidence of model inadequacy or influential outliers.

🌱 5. Thesis: Environmental Survey Analysis
File: codeR_thesis.r
Summary
Conducted large-scale survey analysis to explore public attitudes and behaviors toward environmental issues, detecting patterns in responses.

Problem
Governments and NGOs need insights into public perceptions to design effective environmental policies and campaigns.

Methodology
Imported and cleaned survey datasets.
Applied statistical techniques for pattern detection.
Visualized trends across demographic subgroups.

Skills & Tools
R (tidyverse, foreign, HH), Survey Analysis, Data Visualization.

Results
Successfully fitted Reduced Rank Regression models with ranks 1-4, achieving optimal balance between model complexity and fit with rank 2 (lowest AIC: 4645.792).
Identified rank 2 model as best performing: AIC=4645.792, BIC=4904.67, demonstrating effective dimensionality reduction from 10 predictors to 2 latent dimensions.
Applied optimal scaling transformations to 7 ordinal environmental attitude variables, revealing non-linear monotonic relationships between original categories and quantified scales.
Created detailed stair-step plots for each ordinal predictor, revealing category-specific quantification patterns and identifying equivalent response categories.
Demonstrated clear interpretable relationships: environmental concern positively associated with group membership (β=0.6) and sustainable purchasing behaviors (β=0.434).

🚀 How to Run
Python Notebooks (.ipynb): Open with Jupyter Notebook or Google Colab
R Scripts and RMarkdown (.R, .Rmd): Use RStudio
