## CUPED - Controlled-experiment Using Pre-Experiment Data ##

In any AB test, it becomes important to detect small effects. The idea is that if you reduce variance, then you can identify small effects better

The concept of CUPED is as follows:\
The variance that pre-experiment data can explain in a metric is unrelated to any effects of the experiment. This variance can be removed. For example a metric like - # of number of reservations per property per day 0- 1000s, so variance in this metric can be enormous.  We can use average reservations-per-day for each property before the experiment and compare results against it
