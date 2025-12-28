# Fraud Detection – End‑to‑End EDA & Machine Learning Case Study

## Overview
This project presents a complete **end‑to‑end fraud detection pipeline** built on a subset of the **IEEE‑CIS Fraud Detection Dataset**. It demonstrates how exploratory data analysis (EDA), rigorous data preparation, feature engineering, and modern machine‑learning models can be combined to build a **realistic and deployable fraud detection system**.

The workflow is designed to mirror **real‑world production constraints**, including severe class imbalance, temporal ordering of transactions, threshold governance, and interpretability requirements.

---

## Objectives
- Perform in‑depth **EDA** to uncover fraud patterns
- Clean and prepare a **high‑dimensional transactional dataset**
- Engineer **behavioral, temporal, and interaction features**
- Address **class imbalance** without introducing data leakage
- Build and compare **baseline and advanced ML models**
- Optimize **decision thresholds** for operational trade‑offs
- Interpret key drivers of fraud risk
- Provide **deployment and governance recommendations**

---

## Dataset Summary
- **Source:** IEEE‑CIS Fraud Detection Dataset  
- **Records:** 100,000 transactions  
- **Original Features:** 101  
- **Final Features:** 58  
- **Target Variable:** `isFraud`  
  - `0` → Legitimate  
  - `1` → Fraud  
- **Fraud Rate:** ~11.3% (highly imbalanced, realistic)

## How to Run
1. Place `A1_data.csv` in the working directory
2. Open `fraud_detection_data_analysis.R`
3. Install required R packages
4. Run the script end‑to‑end

> **Note:** Full execution may take time due to dataset size and model training.

---

## Conclusion
This project demonstrates how **EDA‑driven insights**, **robust feature engineering**, **time‑aware modeling**, and **threshold optimization** can be combined into a **practical fraud detection solution**. The resulting framework balances predictive power, interpretability, and operational feasibility, making it suitable for real‑world deployment.
