# 💳 Customer Credit Risk Segmentation

## 📘 Overview  
This project explores **customer credit behavior** through segmentation analysis.  
Using **K-means clustering** and **performance evaluation metrics**, it aims to identify distinct customer groups based on their **borrowing risk levels**.  

The analysis is conducted in **R** using the script `analysis.R`, with data provided in `credit.txt`.  
The workflow covers **data preparation**, **clustering**, and **model assessment** to determine the most suitable segmentation model.  

---

## 🧾 Data Description  
The file **`credit.txt`** contains a dataset detailing customer credit behavior.  
Each record represents a customer and includes attributes related to:  
- Credit score  
- Spending habits  
- Borrowing tendencies  

---

## 🧹 Data Preprocessing  
Before analysis, several preprocessing steps were performed to ensure data quality and consistency:

1. **Credit Score Correction**  
   - Some credit scores contained four digits, where the fourth digit was always zero.  
   - This extra digit was removed to obtain the correct score values.  

2. **Recategorization**  
   - Inconsistent category labels (different spellings or synonymous terms) were standardized into unified groups.  

3. **Outlier Removal**  
   - Extreme values such as `999999999` and other unrealistic entries were filtered out to prevent distortion in clustering results.  

---

## ⚙️ Analysis Methodology  
The **`analysis.R`** script performs the following steps:

1. Loads and preprocesses the dataset.  
2. Applies **K-means clustering** to segment customers based on borrowing risk.  
3. Evaluates model performance using appropriate clustering metrics.  
4. Identifies the **optimal number of clusters** that best represent the customer segments.  

---

## 🎯 Objectives  
- Segment customers into **meaningful risk-based groups**.  
- Enhance understanding of **borrowing patterns** and **credit behaviors**.  
- Support **data-driven decision-making** for risk management and customer profiling.  

---

## 📂 File Structure  
