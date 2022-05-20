library(tidyverse)

# Load the final data set csv as a tibble
df <- as_tibble(read.csv('final_dataset_topics.csv'))
# Clean the data:
# Filter unwanted topics
df <- subset(df, !(topics == "abc" | topics == "#N/A" | topics == "#VALUE!"))
# Trim leading/trailing white space from topics
df$topics <- df$topics %>% trimws()
# Replace " - " with "-"
df$topics <- df$topics %>% str_replace_all(" - ","-")
# Replace " " with "_"
df$topics <- df$topics %>% str_replace_all(" ","_")
#############################################################################
# Hypothesis 1: 
# HA: Topic of the article has an influence on
# hate value of the tweets?
#############################################################################

# hate_score ~ beta0 + beta1*topic(category)
df$hate_score = df$pos_score_hate - df$neg_score_hate
# linear regression model
fit_hscore <- lm(df$hate_score ~ df$topics) 

# summary(fit_hscore)
# Coeffiecients:
#                                   		Estimate  Std.Error    t-value   Pr(>|t|)                    
#                                (Intercept)  -0.718844   0.020693    -34.739     <2e-16      ***
#                       df$topicsafghanistan   0.036343   0.029654      1.226   0.220358         
#                   df$topicsantitrust_cases   0.087452   0.023221      3.766   0.000166      ***
#                       df$topicscelebrities  -0.003036   0.021398     -0.142   0.887187         
#                   df$topicscongress/senate   0.042377   0.021856      1.939   0.052514        .
#                     df$topicsconservatives  -0.056221   0.022238     -2.528   0.011468        *
# df$topicsdisinformation/fraudulent_content  -0.035415   0.032232     -1.099   0.271875         
#                         df$topicseducation  -0.080678   0.029883     -2.700   0.006939       **
#                      df$topicselection_day   0.025433   0.021168      1.201   0.229572         
#                         df$topicselections  -0.011669   0.026377     -0.442   0.658198         
#              df$topicsenvironmental_crisis  -0.005188   0.025941     -0.200   0.841492         
#                       df$topicsevangelists   0.111761   0.025127      4.448   8.68e-06      ***
#                    df$topicsfinance/stocks  -0.099194   0.030082     -3.298   0.000976      ***
#                      df$topicshomelessness  -0.112866   0.031372     -3.598   0.000321      ***
#                         df$topicshomicides  -0.115988   0.054863     -2.114   0.034506        *
#                        df$topicsimmigrants   0.008054   0.026265      0.307   0.759117         
#                       df$topicsimpeachment   0.094045   0.022463      4.187   2.83e-05      ***
#                        df$topicsjournalism   0.145899   0.024138      6.044   1.50e-09      ***
#                         df$topicsjudiciary   0.057248   0.029240      1.958   0.050247        .
#                       df$topicslegislation  -0.069192   0.023325     -2.966   0.003013       **
#                             df$topicslgbtq   0.265186   0.024098     11.004     <2e-16      ***
#                          df$topicsmilitary  -0.121548   0.029713     -4.091   4.30e-05      ***
#             df$topicsmurder_investigations  -0.030059   0.031025     -0.969   0.332604         
#                          df$topicsolympics   0.293715   0.022210     13.224     <2e-16      ***
#                  df$topicspalestine-israel   0.133692   0.037959      3.522   0.000428      ***
#              df$topicspresidential_debates   0.054516   0.022208      2.455   0.014099        *
#                       df$topicsprosecution   0.090331   0.024164      3.738   0.000185      ***
#                          df$topicsprotests   0.089419   0.021266      4.205   2.61e-05      ***
#                            df$topicsracism   0.390050   0.022319     17.476     <2e-16      ***
#                     df$topicsunvaccination  -0.047811   0.027212     -1.757   0.078916        .
#            df$topicsUS_and_Russia_politics   0.025168   0.023127      1.088   0.276484         
#                       df$topicsvaccination  -0.079463   0.021960     -3.619   0.000296      ***
#               df$topicswildfires/disasters  -0.099780   0.037123     -2.688   0.007193       **
#
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.7222 on 169279 degrees of freedom
# Multiple R-squared:  0.02368,	Adjusted R-squared:  0.02349 
# F-statistic: 128.3 on 32 and 169279 DF,  p-value: < 2.2e-16

#############################################################################
# Hypothesis 2: HA: Does the topic of an article has an influence on
# the bias score of the article?) 1 7,500 7,500



#############################################################################

# => bias_score ~ beta0 + beta1*topic (category)
# linear regression model
fit_bias <- lm(bias_score ~ topics, data = df)

# summary(fit_bias)
# Coefficients:
#                                   		Estimate  Std.Error    t-value   Pr(>|t|)                    
#                                (Intercept)     1.1141     0.2548      4.372   1.23e-05      ***
#                          topicsafghanistan     1.6711     0.3652      4.576   4.73e-06      ***
#                      topicsantitrust_cases    -2.8295     0.2859     -9.895     <2e-16      ***
#                          topicscelebrities    -9.5021     0.2635    -36.061     <2e-16      ***
#                      topicscongress/senate     2.6211     0.2691      9.739     <2e-16      ***
#                        topicsconservatives    -5.7183     0.2738    -20.881     <2e-16      ***
#    topicsdisinformation/fraudulent_content   -25.9103     0.3969    -65.280     <2e-16      ***
#                            topicseducation    -2.6130     0.3680     -7.101   1.25e-12      ***
#                         topicselection_day    -3.9441     0.2607    -15.131     <2e-16      ***
#                            topicselections    -8.8935     0.3248    -27.380     <2e-16      ***
#                 topicsenvironmental_crisis    -3.4830     0.3194    -10.903     <2e-16      ***
#                          topicsevangelists     1.2678     0.3094      4.097   4.18e-05      ***
#                       topicsfinance/stocks    -7.0908     0.3704    -19.142     <2e-16      ***
#                         topicshomelessness     2.0012     0.3863      5.180   2.22e-07      ***
#                            topicshomicides    -7.2404     0.6756    -10.717     <2e-16      ***
#                           topicsimmigrants     4.4746     0.3234     13.835     <2e-16      ***
#                          topicsimpeachment   -10.1604     0.2766    -36.730     <2e-16      ***
#                           topicsjournalism     6.1665     0.2972     20.745     <2e-16      ***
#                            topicsjudiciary   -10.6673     0.3601    -29.626     <2e-16      ***
#                          topicslegislation    -2.6961     0.2872     -9.387     <2e-16      ***
#                                topicslgbtq    -3.7130     0.2968    -12.512     <2e-16      ***
#                             topicsmilitary    -1.7091     0.3659     -4.671   3.00e-06      ***
#                topicsmurder_investigations   -10.1161     0.3820    -26.479     <2e-16      ***
#                             topicsolympics    -4.0811     0.2735    -14.922     <2e-16      ***
#                     topicspalestine-israel    -5.9225     0.4674    -12.670     <2e-16      ***
#                 topicspresidential_debates     1.2563     0.2735      4.594   4.36e-06      ***
#                          topicsprosecution     3.5562     0.2976     11.951     <2e-16      ***
#                             topicsprotests     0.9140     0.2619      3.490   0.000483      ***
#                               topicsracism    -5.7000     0.2748    -20.739     <2e-16      ***
#                        topicsunvaccination     0.4463     0.3351      1.332   0.182905         
#               topicsUS_and_Russia_politics     3.5337     0.2848     12.408     <2e-16      ***
#                          topicsvaccination    -2.1084     0.2704     -7.797   6.38e-15      ***
#                  topicswildfires/disasters    -1.1644     0.4571     -2.547   0.010862        *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 8.893 on 169279 degrees of freedom
# Multiple R-squared:  0.2117,	Adjusted R-squared:  0.2116 
# F-statistic:  1421 on 32 and 169279 DF,  p-value: < 2.2e-16


#############################################################################
# HA: Topic of the article has an influence on sentiment value of the tweets.
#############################################################################

# => sentiment_value = pos_score_sentiment - neg_score_sentiment : Gives a sentiment value
# either positive or negative. Neutral has a mean 0 (neither pos/neg).
# sentiment_value + 2*neg_score_sentiment = 1
# => sentiment_value ~ beta0 + beta1*topics(category)
df$sentiment_value <-  df$pos_score_sentiment - df$neg_score_sentiment
# linear regression model
fit_sentiment <- lm(sentiment_value ~ topics, data = df)
# summary(fit_sentiment)
#                                              Estimate  Std.Error    t-value   Pr(>|t|)                    
#                                (Intercept)  -0.012723   0.027367     -0.465   0.641986         
#                          topicsafghanistan   0.052133   0.039218      1.329   0.183747         
#                      topicsantitrust_cases   0.036171   0.030711      1.178   0.238878         
#                          topicscelebrities   0.314670   0.028300     11.119     <2e-16      ***
#                      topicscongress/senate  -0.060500   0.028905     -2.093   0.036347        *
#                        topicsconservatives   0.285604   0.029411      9.711     <2e-16      ***
#    topicsdisinformation/fraudulent_content  -0.121499   0.042627     -2.850   0.004369       **
#                            topicseducation  -0.069458   0.039521     -1.757   0.078836        .
#                         topicselection_day   0.099776   0.027996      3.564   0.000365      ***
#                            topicselections  -0.094984   0.034885     -2.723   0.006474       **
#                 topicsenvironmental_crisis   0.121676   0.034307      3.547   0.000390      ***
#                          topicsevangelists   0.079675   0.033231      2.398   0.016503        *
#                       topicsfinance/stocks   0.040427   0.039784      1.016   0.309554         
#                         topicshomelessness   0.038903   0.041490      0.938   0.348429         
#                            topicshomicides  -0.014914   0.072559     -0.206   0.837145         
#                           topicsimmigrants  -0.112959   0.034737     -3.252   0.001147       **
#                          topicsimpeachment  -0.007069   0.029709     -0.238   0.811935         
#                           topicsjournalism   0.067801   0.031924      2.124   0.033684        *
#                            topicsjudiciary  -0.033665   0.038671     -0.871   0.384002         
#                          topicslegislation  -0.069855   0.030848     -2.264   0.023546        *
#                                topicslgbtq  -0.025934   0.031871     -0.814   0.415796         
#                             topicsmilitary   0.224745   0.039297      5.719   1.07e-08      ***
#                topicsmurder_investigations   0.083122   0.041031      2.026   0.042784        *
#                             topicsolympics  -0.162072   0.029374     -5.518   3.44e-08      ***
#                     topicspalestine-israel  -0.109092   0.050202     -2.173   0.029775        *
#                 topicspresidential_debates   0.046512   0.029371      1.584   0.113286         
#                          topicsprosecution  -0.015578   0.031958     -0.487   0.625928         
#                             topicsprotests  -0.011998   0.028125     -0.427   0.669665         
#                               topicsracism   0.100012   0.029517      3.388   0.000704      ***
#                        topicsunvaccination   0.111608   0.035988      3.101   0.001928       **
#               topicsUS_and_Russia_politics   0.048912   0.030586      1.599   0.109789         
#                          topicsvaccination  -0.010406   0.029043     -0.358   0.720109         
#                  topicswildfires/disasters   0.097399   0.049097      1.984   0.047278        *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.9551 on 169279 degrees of freedom
# Multiple R-squared:  0.01711,	Adjusted R-squared:  0.01692 
# F-statistic: 92.06 on 32 and 169279 DF,  p-value: < 2.2e-16

