# CRIS-parents-with-psychosis
R code used to clean data from CRIS and then analysis a project which looked at how many parents there were in a sample with psychosis and what characteristics were associated with being a parent.

## Cleaning data entry sondaug
Once child details had been found through reading clinical notes and written into an excel sheet, the file was to check that there were no errors e.g. that those parents who were marked '0' for 'Evidence of child' had no child details reported in subsequent columns. A function was written to report whether children were over or under 18 when looking at their ages.

## Combining participants child details with characteristics
Once all the child details had been found through reading clinical notes and this was cleaned, patient (parent) characteristics were downloaded from CRIS. This code took the most recent (non-missing) report of patient characteristics and combining all files into one file to work on.

## LSOA
Similarly to the other characteristics, the most recent (non-missing) report of the patient's LSOA was found. Then this data was combined with Index of Multiple Deprivation Levels (2019). After removing those with missing data, 5077 had LSOA data. This was divided by 9 to make nine groups of 564 to be compared in the analysis.

## Cleaning patient characteristics
Collapsing patient characteristics into smaller categories.

## OU18 unknowns
This code was written firstly to use t-tests to check whether there were any differences between the parents whose children's ages were missing and those were this wasn't missing in terms of the parent's age and gender. Once it was established that there the parent's age or gender did not predict missingness.

Then for each child (first born, second born etc...), the average parent's age for children that were under 18 was calculated and the average parent's age for children that were 18 and over was calculated. Parents whose child's age was unknown and were the same age or younger than the average age of parents whose child was under 18, had 'under' inputted and parents whose child's age was unknown and were the same age or older han the average age of parents whose child was 18 and over had 'over' inputted. Parents whose age lay in between these two numbers had this data remaining missing.

## Analysis
I'd forgotten to clean patient's diagnosis alongside the other characteristics so i did that at the beginning of this file. Then I worked out counts for each variable for the parents and non-parents in the sample. Then how many parents had any child under 18, how many had all children 18 and over and how many were all unknown was calculated. Whether there was a difference in whether a child was recorded in the structured 'contacts' field or the free text field depending on age was calculated (and confirmed that dependants were more likely to be recorded).

The file was changed from wide to long i.e. from each row being a parent to each row being a child (so some parents had more than one row). The counts for the genders and ages of the children were calculated.

T-tests and chi-squared tests looking at the difference between parents and non-parents were then calculated for all 10 variables - age, ethnicity, gender, marital status, diagnosis, accommodation, employment, ward stays, smoking, lsoa.

Ward stay means for each demographic group were calculated.

Then before the binomial regression was run, the variables were converted into factors and the base variable was selected which made the most sense for the model. The binomial regression was run and a stepwise test using AIC was run to check whether any variable should be removed and the result was that no variable's removal improved the model fit. The odds ratio with the model's results was then calculated. The VIF (collinearity) was checked. An exploratory analysis was calculated to look at whether ethnicity had a moderating effect on diagnosis but this did not improve the model.

The same binomial regression was run but comparing non-parents to parents of dependants (i.e. parents of non-dependants were excluded from this analysis). A stepwise test was run which resulted in only ethnicity being removed. Odds ratios were calculated again. An exploratory analysis between diagnosis and current age was run but this did not improve the model.

## Survival analysis

I'd forgotten to clean patient's diagnosis alongside the other characteristics so i did that at the beginning of this file. I read in the file which contained all patients' ward stays and I checked this file for errors and counts. I created a variable called 'presence_of_WS' to indiate whether participants had a ward stay or not. Then I joined this ward stay file to the main file. I created some a child-related variable called 'child_born_inb' which calculcated whether a child was born in between ward stays, which I didn't end up using because there was too much missing data. I also made a new variable called 'anyunder18' which calculates whether a parent has any child under 18 (or whether all are over 18).

Then I run a survival analysis with all of these variables. A backwards stepwise regression using AIC indicated that all should be tkaen out except age, marital status, diagnosis and smoking.

The final bit of the code is creating Kaplan Meier figures.

