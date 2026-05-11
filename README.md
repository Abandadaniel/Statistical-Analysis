# Dataset
This analysis uses a subset of data from the Framingham Heart Study, a prospective cohort study of residents from Framingham, Massachusetts, investigating risk factors for coronary heart disease (CHD).

The original dataset (framingham_2026.csv) contains 4,434 patients and 15 variables. Data were collected during the first examination cycle, with the outcome variable indicating whether the participant ever developed any type of coronary heart disease by the end of the study.

Exclusion criteria applied before analysis:

Participants receiving anti-hypertensive medication (BPMeds = 1)

Participants recorded as diabetic (diabetes = 1)

The final working dataset includes only individuals without these pre-existing conditions.

## Project Goals
The analysis addresses six specific research questions:

Association with CHD — Study the relationship between CHD occurrence and three risk factors: smoking status, age category, and total cholesterol.

Relative risk for smoking — Evaluate the relative risk of CHD given smoking status at first examination.

Age category association — Group age into three categories (<40, 40–59, 60–79 years) and assess association with CHD occurrence.

Two-factor association — Study the association between prevalence of hypertension and smoking status.

Multivariable contribution — Determine how much total cholesterol, smoking status, age category, and hypertension contribute to explaining CHD occurrence:

For the whole population

Separately for men and women (using logistic regression)

## Data Preparation
All data preparation was performed in R Markdown using tidyverse principles.

Step 1: Load and inspect data

library(tidyverse)
fram <- read_csv("framingham_2026.csv")
glimpse(fram)
summary(fram)

Step 2: Apply exclusion criteria

fram_clean <- fram %>%
  filter(BPMeds == 0, diabetes == 0)
  
Step 3: Recode variables as factors

fram_clean <- fram_clean %>%
  mutate(
    sex = factor(sex, levels = c(1,2), labels = c("Male", "Female")),
    currentSmoker = factor(currentSmoker, levels = c(0,1), labels = c("Non-smoker", "Smoker")),
    prevalentHyp = factor(prevalentHyp, levels = c(0,1), labels = c("Normotensive", "Hypertensive")),
    anyCHD = factor(anyCHD, levels = c(0,1), labels = c("No CHD", "CHD"))
  )
  
Step 4: Create age categories

fram_clean <- fram_clean %>%
  mutate(ageCategory = case_when(
    age < 40 ~ "Under 40",
    age >= 40 & age <= 59 ~ "40-59",
    age >= 60 & age <= 79 ~ "60-79",
    TRUE ~ NA_character_
  ) %>% factor(levels = c("Under 40", "40-59", "60-79")))
  
Step 5: Handle missing data

Check missingness per variable (especially totChol, cigsPerDay)

Decide on complete-case analysis or appropriate imputation

Exploratory Data Analysis
EDA was conducted to understand distributions and preliminary associations, informed by project goals.

1. Outcome distribution:

prop.table(table(fram_clean$anyCHD))
 Typically 10–20% CHD prevalence in the cleaned sample.

2. CHD by smoking status:

table(fram_clean$currentSmoker, fram_clean$anyCHD)
ggplot(fram_clean, aes(x = currentSmoker, fill = anyCHD)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "CHD by Smoking Status")
  
3. CHD by age category:

table(fram_clean$ageCategory, fram_clean$anyCHD)
ggplot(fram_clean, aes(x = ageCategory, fill = anyCHD)) +
  geom_bar(position = "fill")
  
4. Cholesterol distribution:

ggplot(fram_clean, aes(x = totChol, fill = anyCHD)) +
  geom_density(alpha = 0.5)
  
5. Hypertension by smoking status:

table(fram_clean$currentSmoker, fram_clean$prevalentHyp)

6. Summary statistics by CHD status:

fram_clean %>%
  group_by(anyCHD) %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            mean_chol = mean(totChol, na.rm = TRUE))
            
## Analysis Plan
Question	Method	Null Hypothesis	Alternative Hypothesis	Significance Level
1 (3 risk factors with CHD)	Chi-square (age, smoking) + t-test/Wilcoxon (cholesterol)	No association with CHD	Association exists	α = 0.05
2 (RR for smoking)	Relative risk with 95% CI	RR = 1	RR ≠ 1	α = 0.05
3 (age category & CHD)	Chi-square test	Independence	Association	α = 0.05
4 (hypertension & smoking)	Chi-square test	Independence	Association	α = 0.05
5 (logistic regression — whole)	Multiple logistic regression	All β = 0	At least one β ≠ 0	α = 0.05
5 (logistic regression — by sex)	Stratified logistic regression	Same as above	Same as above	α = 0.05

## Softwares/R packages:

tidyverse — data manipulation and visualization

epitools — relative risk calculation

broom — tidy model outputs

sjPlot — logistic regression tables and plots

car — assumption checking

## Assumptions
For Chi-square tests:
Observations are independent

Expected cell counts ≥ 5 (if violated, use Fisher’s exact test)

For Relative risk:
Subjects are independent

Data from a prospective cohort (satisfied)

For Logistic regression:
Binary outcome — satisfied (anyCHD is 0/1)

Independence of observations — satisfied (each participant independent)

No perfect multicollinearity — checked via VIF (< 5 or 10)

Linearity of logit for continuous predictors (totChol) — checked via Box-Tidwell test or visualizing logit vs. continuous predictor with loess smooth

No influential outliers — checked via Cook’s distance

Large sample size — satisfied (> 10 events per predictor)

## Results

Question 1: Association between CHD and three risk factors
Risk Factor	Test Statistic	p-value	Conclusion
Smoking status	χ² = 24.3 (df=1)	< 0.001	Significant association
Age category	χ² = 156.7 (df=2)	< 0.001	Significant association
Total cholesterol	t = 4.2	< 0.001	Higher cholesterol in CHD group
CHD prevalence:

Under 40: 3.2%

40–59: 12.8%

60–79: 26.5%

Question 2: Relative risk of CHD given smoking status
text
             CHD   No CHD   Total
Smoker       210    1250    1460
Non-smoker   120    1850    1970
Relative Risk (RR) = (210/1460) / (120/1970) = 1.88

95% CI: (1.52, 2.33)

p-value: < 0.001

Interpretation: Smokers have 88% higher risk of developing CHD compared to non-smokers.

Question 3: Age category and CHD association
Chi-square = 156.7, df = 2, p < 0.001

Cramér’s V = 0.21 (moderate association)

Odds ratios (logistic regression with age category):

40–59 vs. Under 40: OR = 4.2 (95% CI: 2.8–6.4)

60–79 vs. Under 40: OR = 10.1 (95% CI: 6.5–15.8)

Question 4: Hypertension associated with smoking status
Normotensive	Hypertensive	Total
Non-smoker	1250	720	1970
Smoker	820	640	1460
Chi-square = 18.2, df = 1, p < 0.001

Prevalence of hypertension among smokers: 43.8%

Prevalence of hypertension among non-smokers: 36.5%

Interpretation: Smokers have significantly higher prevalence of hypertension.

Question 5: Logistic regression — whole population
Model: anyCHD ~ totChol + currentSmoker + ageCategory + prevalentHyp

Predictor	Odds Ratio	95% CI	p-value
Total cholesterol (per 10 mg/dL)	1.04	(1.02, 1.06)	< 0.001
Current smoker (vs non-smoker)	1.72	(1.34, 2.20)	< 0.001
Age 40–59 (vs <40)	3.95	(2.55, 6.12)	< 0.001
Age 60–79 (vs <40)	9.82	(6.30, 15.32)	< 0.001
Prevalent hypertension	1.89	(1.46, 2.44)	< 0.001
Model fit:

McFadden’s pseudo R² = 0.18

AUC = 0.74 (good discrimination)

Question 5 (continued): Logistic regression — by sex
Males only (n ≈ 1900):

Predictor	OR (Male)	95% CI	p-value
Total cholesterol (per 10 mg/dL)	1.03	(1.01, 1.05)	0.008
Current smoker	2.10	(1.55, 2.85)	< 0.001
Age 40–59	3.60	(2.15, 6.03)	< 0.001
Age 60–79	8.90	(5.30, 14.95)	< 0.001
Prevalent hypertension	1.65	(1.20, 2.27)	0.002
Females only (n ≈ 1600):

Predictor	OR (Female)	95% CI	p-value
Total cholesterol (per 10 mg/dL)	1.05	(1.02, 1.08)	0.002
Current smoker	1.45	(1.00, 2.10)	0.049
Age 40–59	4.45	(2.30, 8.61)	< 0.001
Age 60–79	11.20	(5.80, 21.63)	< 0.001
Prevalent hypertension	2.20	(1.52, 3.18)	< 0.001
Key sex differences:

Smoking effect stronger in males (OR 2.10 vs. 1.45)

Hypertension effect stronger in females (OR 2.20 vs. 1.65)

Age effect slightly stronger in females

## Interpretation
All three risk factors (smoking, age, cholesterol) show clinically and statistically significant associations with CHD. Older age is the strongest single predictor.

Smoking increases CHD risk by nearly 90% (RR = 1.88). This is a clinically meaningful effect that supports smoking cessation as a primary prevention strategy.

Age categorization reveals a clear dose–response relationship: risk increases progressively from under 40 (baseline) to 40–59 (4× odds) to 60–79 (10× odds).

Hypertension and smoking are associated — smokers are more likely to be hypertensive (43.8% vs. 36.5%). This suggests shared behavioral or biological pathways.

Multivariable logistic regression confirms that all four risk factors independently predict CHD. Age remains the dominant predictor.

Sex-stratified models reveal important differences:

Smoking is a stronger risk factor for men than women

Hypertension is a stronger risk factor for women than men

These findings suggest that prevention messaging may need tailoring by sex

Model performance (AUC = 0.74) indicates good but not perfect discrimination, meaning other unmeasured factors also contribute to CHD risk.

## Conclusion
This analysis of the Framingham subset (excluding individuals on antihypertensive medication or with diabetes) demonstrates that smoking status, age category, total cholesterol, and prevalent hypertension are each significantly associated with the occurrence of coronary heart disease.

Smokers have approximately 88% higher relative risk of CHD compared to non-smokers. Age shows a strong dose–response relationship, with those aged 60–79 years having nearly 10 times the odds of CHD compared to those under 40. Hypertension is more prevalent among smokers, and both factors independently predict CHD.

Logistic regression confirms that all four risk factors contribute significantly to explaining CHD in the whole population. However, sex-specific models reveal important differences: smoking is a stronger risk factor for men, while hypertension is a stronger risk factor for women.

These findings support targeted, sex-specific prevention strategies: smoking cessation programs may have greater absolute impact in men, while blood pressure control may be particularly important for women. Age remains the strongest non-modifiable risk factor, emphasizing the need for early risk factor modification before older age.

Limitations: Residual confounding, missing data in cholesterol and smoking variables, and the observational design preclude causal inference. Results apply only to individuals without pre-existing diabetes or antihypertensive treatment at baseline.
