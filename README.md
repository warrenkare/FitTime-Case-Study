__[< Back to Portfolio](https://warrenkare.github.io)__

author: Warren Kare

date: January 28, 2024

output: html_document


<!-- ```{r setup, include=FALSE}
library(formatR)
library(knitr)
knitr::opts_chunk$set(echo = TRUE)
opts_chunk$set(tidy.opts=list(width.cutoff=50),tidy=TRUE) 
``` -->


### Introduction

![FitTime Logo](https://warrenkare.github.io/FitTime-Case-Study/assets/img/FitTimeLogo.png)

<sup>_logo generated with DALL-E 3 and cleaned in Photoshop_

FitTime is a (fictional) technology manufacturing company focused on creating health products for women. They have developed a smart watch called Flexi that captures fitness and lifestyle activity of its wearers. Using the data gathered on 33 random participants, the company hopes to better understand their customers to make informed decisions on how to better market the product and enhance their customer base.

#### Stakeholders
* Mia Lee: Chief Creative Officer
* Ryan Smith: Co-Founder, Director of Operations
* Zara Khan: Data Analytics Manager
* FitTime Marketing Team

#### Key Questions
1. What are some trends we can identify with the Flexi smart watch?
2. How might these trends be relevant to FitTime customers?
3. How can FitTime use these insights to develop a strong marketing strategy?

&nbsp;  
 
### Preparing the Data
Note: the dataset used in this case study is [FitBit Fitness Tracker Data](https://www.kaggle.com/datasets/arashnic/fitbit) (CC0: Public Domain). 

Packages needed: _tidyverse_, _lubridate_, and _ggpubr_.

------------------------

Before we dive into the data, we should assess the credibility of the dataset with the __ROCCC__ test.

* __Reliable:__ Data was sourced from Amazon Mechanical Turk, which is a platform that pays participants to partake in surveys. This incentivized system could possibly lead to bias and skewed results.

* __Original:__ Although this data was not collected by FitTime directly, it is still considered second-party data which can be useful.

* __Comprehensive:__ The data collected was over a two-month period for a very small sample size of 33 participants, which is certainly not representative of all customers.

* __Current:__ The dataset is from 2016 (8 years old), but it is unlikely to make a significant difference in physical activity and lifestyle habits.

* __Cited:__ Data was collected directly by Amazon Mechanical Turk and does not reference other sources.

Taking all these factors into consideration, this may not be the best dataset to work with. However, it wouldn't hurt to explore the numbers and see what was observed.

&nbsp;  
Let's load the packages first.
<!-- ```{r tidyverseback, include=FALSE}
library(tidyverse)
library(lubridate)
library(ggpubr)
``` -->

```{r tidyversefront, eval=FALSE}
library(tidyverse)
library(lubridate)
library(ggpubr)
```


&nbsp;  
Next, let's set our directory and create our data frames from the dataset files.
<!-- ```{r csvback, include=FALSE}
setwd("/Users/warrenkare/FitTime")
daily_activity <- read_csv("dailyActivity_merged.csv")
daily_sleep <- read_csv("sleepDay_merged.csv")
``` -->

```{r csvfront, eval=FALSE}
setwd("/Users/warrenkare/FitTime")
daily_activity <- read_csv("dailyActivity_merged.csv")
daily_sleep <- read_csv("sleepDay_merged.csv")
```

&nbsp;  
Now, let's get a quick glance of our data and ensure the column names and observation formats are consistent, then we can proceed with cleaning our data.

```{r head}
head(daily_activity)
head(daily_sleep)
```

| Id | ActivityDate | TotalSteps | 12 More Columns.. |
|----|---------------|------------|-------------------|
|1503960366 | 4/12/2016 | 13162 | ... |

| Id | SleepDay | TotalMinutesAsleep | 2 More Columns.. |
|----|---------------|------------|-------------------|
|1503960366 | 4/12/2016 12:00:00 AM | 327 | ... |


&nbsp;  
__Rename columns:__

```{r colrename}
colnames(daily_activity)[2] = "ActivityDay"
colnames(daily_sleep)[2] = "ActivityDay"
```

__Convert ActivityDate from chr to date format:__

```{r datefix}
daily_activity$ActivityDay <- mdy(daily_activity$ActivityDay)
daily_sleep$ActivityDay <- as.Date(mdy_hms(daily_sleep$ActivityDay))
```

__Double-check changes were made correctly:__

```{r headcheck}
head(daily_activity)
head(daily_sleep)
```

| Id | ActivityDay | TotalSteps | 12 More Columns.. |
|----|---------------|------------|-------------------|
|1503960366 | 2016-04-12 | 13162 | ... |

| Id | ActivityDay | TotalMinutesAsleep | 2 More Columns.. |
|----|---------------|------------|-------------------|
|1503960366 | 2016-04-12 | 327 | ... |

&nbsp;  
All changes successful. Let's do a quick check of the Id's in both tables to ensure there were equal participants in each data collection.

```{r Idcount}
n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
```

> [1] 33
> 
> [1] 24

It appears not all participants were part of the sleep activity. We will keep this in mind when we analyze our data. For now, let's do an inner join of the two tables to cross-reference Activity and Sleep.

```{r mergehealth}
daily_health <- merge(daily_activity, daily_sleep)
```

&nbsp;  
 
### Process & Analyze the Data
Let's gather insights on the activity levels of our participants. When looking at this data, I have a few questions I want to answer:


#### 1. Is there a relationship between number of steps and calories burned?

```{r stepscalories_plot}
ggplot(data = daily_activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + 
  geom_smooth(method = "loess") + 
  stat_cor(method = "pearson", label.x = 0, label.y = 4600) + 
  labs(x = "Total Steps", y = "Calories Burned", title = "Steps vs. Calories Burned") +
  annotate("text", x = 25000, y = 700, label = "Slight positive correlation", color = "red")
```

![Steps vs. Calories](https://warrenkare.github.io/FitTime-Case-Study/assets/img/stepsvscalories.png)

We can observe a slightly positive correlation between total steps taken and calories burned. It is possible the participants burned calories in ways other than walking or running (ex: weightlifting or other aerobic activity tracked separately from the pedometer feature).


#### 2. Is there a relationship between number of steps and amount of time spent in bed?

```{r bedminutes_steps plot}
ggplot(data = daily_health, aes(y=TotalTimeInBed, x=TotalSteps)) + 
  geom_point() + 
  geom_smooth(method = "loess") + 
  stat_cor(method = "pearson", label.x = 0, label.y = 1100) +
  labs(x = "Number of Steps Taken", y = "Total Time In Bed (Minutes)", title = "Steps Taken vs. Minutes In Bed") +
  annotate("text", x = 17800, y = 850, label = "Little to no correlation", color = "red")
```

![Steps vs. Bed](https://warrenkare.github.io/FitTime-Case-Study/assets/img/stepsvsbed.png)

Most of the data points aggregate around the range of 300-600 minutes in bed (about 5-10 hours), but there does not appear to be a strong correlation between the number of steps taken and total time in bed.


#### 3. How are users exercising and sleeping throughout the week?

In this step, we will create bar graphs to measure the average number of steps and amount of sleep by day of the week. Sleep minutes were converted to sleep hours to increase readability.

```{r weekday}
daily_health$DayofWeek <- weekdays(daily_health$ActivityDay)
daily_health$DayofWeek <- factor(daily_health$DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
```

```{r step sleep table}
steps_sleep_week <- daily_health %>% 
  group_by(DayofWeek) %>% 
  summarize(AvgSteps = mean(TotalSteps), AvgSleepHours = mean(TotalMinutesAsleep)/60)
```

```{r stepsbyday_plot}
ggplot(data=steps_sleep_week, aes(x = DayofWeek, y=AvgSteps)) + 
  geom_bar(stat = "identity", fill = hcl(32,36,69)) + 
  geom_text(aes(label=round(AvgSteps, digits=0)), vjust=5) + 
  labs(x = "Day of Week", y = "Average Number of Steps", title = "Average Steps by Day of Week")
```

![Steps by Day](https://warrenkare.github.io/FitTime-Case-Study/assets/img/avgstepsweek.png)

```{r sleepbyday_plot}
ggplot(data=steps_sleep_week, aes(x = DayofWeek, y=AvgSleepHours)) + 
  geom_bar(stat = "identity", fill = hcl(201,36,69)) + 
  geom_text(aes(label=round(AvgSleepHours, digits=2)), vjust=5) + 
  labs(x = "Day of Week", y = "Average Sleep Duration (hours)", title = "Sleep Duration by Day of Week")
```

![Sleep by Day](https://warrenkare.github.io/FitTime-Case-Study/assets/img/avgsleepweek.png)

##### Observations:

* Average step count was lowest on Sundays, but highest on Saturdays.
* Average sleep duration was relatively constant throughout the week, but notably highest on Sunday.

##### Takeaways:

* Sunday is the day before a typical Monday-Friday work week, which could explain the increase in rest time and decrease in overall physical activity. 
* Saturday is a popular day of the week to partake in additional activities and events, which could explain the increased number of steps.


#### 4. What is the distribution of activity levels (Sedentary, Lightly, Fairly, Very)?

In this step, we want to find out how much time our customers spent in different activity levels. We will transform the table to properly plot our data. Next, we will convert minutes to hours to make the visuals easier to read.


```{r activetime}
activetime <- daily_activity[,c("Id", "ActivityDay", "VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes")]
```

```{r}
activetime_hours <- activetime %>% 
  mutate(across(c(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes), function(x) x/60)) %>% 
  rename("VeryActive"=VeryActiveMinutes, "FairlyActive" = FairlyActiveMinutes, "LightlyActive" = LightlyActiveMinutes, "Sedentary" = SedentaryMinutes)
```

```{r activetime_long}
activetime_long <- activetime_hours %>%
  pivot_longer(cols = 3:6,
               names_to = "ActivityLevel",
               values_to = "ActivityHours")
```

```{r activetime_longlevels}
activetime_long$ActivityLevel <- factor(activetime_long$ActivityLevel, levels = c("Sedentary", "LightlyActive", "FairlyActive", "VeryActive"))
```

```{r activetime_long plot}
ggplot(data = activetime_long, aes(x = ActivityLevel, y = ActivityHours, fill = ActivityLevel)) +
  geom_bar(stat = "identity") + 
  labs(x = "Activity Level", y = "Total Activity Time (hours)", title = "Total Time Spent in Each Activity Level")
```

![Activity Distribution](https://warrenkare.github.io/FitTime-Case-Study/assets/img/activitydist.png)

We can see that Sedentary Minutes is extremely high compared to all other levels of activity. Let's explore this further.

```{r sedentaryhours}
sedentaryhours <- activetime_long %>% 
  summarize(mean(ActivityHours[ActivityLevel=="Sedentary"]))
print.AsIs(sedentaryhours)
```

> mean(ActivityHours[ActivityLevel == "Sedentary"])
> 
> 16.52018

The average duration spent sedentary is about 16.5 hours. Let's compare this to the sleep data.

```{r sleephours}
sleephours <- summarize(daily_health, mean(TotalMinutesAsleep)/60)
print.AsIs(sleephours)
```

> mean(TotalMinutesAsleep)/60
> 
> 6.991122

The average amount of time spent sedentary (16.5 hours) plus the average amount of time spent sleeping (6.99 hours) totals about 33.5 hours. This would not make sense because there are only 24 hours in a day. The SedentaryMinutes measure could be inflated. There could be a few reasons for this:

* Unclear differentiation between sitting, laying down, and sleeping
* User removes the device but sedentary time still gets logged
* Device app limitation/defect or user error

Next, we can see LightlyActive is the second highest activity level. LightlyActive includes walking and general day-to-day movements, which means the participants likely wore the Flexi watch casually throughout the day and not just exclusively during intentional exercise.

&nbsp; 

### Recommendations

Using the insights from our analysis, here are my recommendations for the data, development, and marketing teams. We need to first better understand our customer base.

#### Data Team

1. Work with the Development team to put out a survey via email campaign and within the FitTime mobile app to ask customers about their usage. To optimize data collection, most questions must be simple and have close-ended response options. Moreover, selecting an answer to a question should automatically move the customer to the next question. We can use their responses to help calibrate the accuracy of the Flexi watch and personalize user experience. Questions can include but are not limited to:

  * How often do you wear Flexi? (Rarely, Sometimes, Often, Always on!)
  * How would you describe your activity level? (Sedentary, Light, Moderate, High)
  * What are your health and fitness goals? __*multiple answers allowed*__ (Lose weight, Improve overall health, Reduce stress, Achieve new milestones...)
  * What types of activities do you enjoy? (Walking, Hiking, Running, Swimming, Weightlifting...)

2. Gather additional customer attribute data (age, ethnicity, income, etc.). We can use this information to measure different relationships. For example, how does usage vary based on age and household income? The data team should conduct internal discussions to determine which data should be made voluntary.

#### Development Team

1. Allow users to utilize a "Step Reminder" feature. When enabled, the watch vibrates or chimes lightly as a reminder to get moving. This will be triggered if the step threshold is not met by a certain time each day. Settings can be customized as needed. This will encourage customers to stay active throughout the week.

2. Roll out a level system (ex: sapphire, bronze, silver, gold) alongside fun achievements (ex: "Moonwalker" title after 100,000 total steps completed) to promote app stickiness.

3. Research if SedentaryMinutes and TotalMinutesAsleep are intermingled. If so, work on a way to better differentiate the two activity types.

#### Marketing Team

1. Create short-form ads that will play within streaming platforms and social media platforms. Example themes include but are not limited to:

* Creatively highlight the most popular features of Flexi centering on the theme of wearing the watch all-day, every day.
* Employ UGC (user-generated content) creators to talk about the benefits of Flexi.
* Showcase a diverse customer base wearing Flexi to promote the inclusiveness of the FitTime brand.

2. Start a campaign with a catchy name (ex: #WatchMeFlexi) encouraging participants to showcase how they use their Flexi watch. Winners will be awarded eye-catching prizes, and all participants will be automatically entered into a giveaway for a Flexi product bundle.

&nbsp; 

### Final Thoughts

The dataset used in this analysis was a good first step in gaining insights on our customers. However, it had its limitations that prevented us from getting a complete picture of how our customers use the product. We would benefit from further analyses and team discussions.

Thank you for reading through this case study!

__[< Back to Portfolio](https://warrenkare.github.io)__
