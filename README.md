# intro-strain

Repository for the paper "Here Comes the STRAIN: Analyzing Defensive Pass Rush in American Football with Player Tracking Data" by [Quang Nguyen](https://github.com/qntkhvn/), [Ronald Yurko](https://github.com/ryurko), and [Gregory J. Matthews](https://github.com/gjm112).

A preprint is available at https://arxiv.org/abs/2305.10262.

# Abstract

In American football, a pass rush is an attempt by the defensive team to disrupt
the offense and prevent the quarterback (QB) from completing a pass. Existing
metrics for assessing pass rush performance are either discrete-time quantities or
based on subjective judgment. Using player tracking data, we propose STRAIN, a
novel metric for evaluating pass rushers in the National Football League (NFL) at the
continuous-time within-play level. Inspired by the concept of strain rate in materials
science, STRAIN is a simple and interpretable means for measuring defensive pressure
in football. It is a directly-observed statistic as a function of two features: the distance
between the pass rusher and QB, and the rate at which this distance is being reduced.
Our metric possesses great predictability of pressure and stability over time. We also
fit a multilevel model for STRAIN to understand the defensive pressure contribution
of every pass rusher at the play-level. We apply our approach to NFL data and
present results for the first eight weeks of the 2021 regular season. In particular, we
provide comparisons of STRAIN for different defensive positions and play outcomes,
and rankings of the NFL’s best pass rushers according to our metric

# Related links

* NFL Big Data Bowl 2023 data: https://www.kaggle.com/competitions/nfl-big-data-bowl-2023/data
* Kaggle notebook: https://www.kaggle.com/code/statsinthewild/strain-sacks-tackles-rushing-aggression-index
