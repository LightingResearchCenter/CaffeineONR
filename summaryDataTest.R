

GNG <- read_csv("//root/projects/Caffeine_ONR_Study/performanceTestData/outier-removed-data-sets/2018-02-01_112935_GNG_addedTimeCropped.csv")


GNG$Subject_id <- as.factor(GNG$Subject_id )
GNG$condition <- as.factor(GNG$condition )
GNG$condition2 <- as.factor(GNG$condition2 )
GNG$light <- as.factor(GNG$light )
GNG$test_period <- as.factor(GNG$test_period )
GNG$response_time <- as.numeric(GNG$response_time )
GNG$session <- as.factor(GNG$session )

go_only <- subset(GNG, display == "go")
no_only <- subset(GNG, display == "no_go")
gng_rt <- subset(GNG, display == "go" & response_time >= .1 & time2 > 0)

sum1 <- summarySEwithin(gng_rt, measurevar = "response_time", betweenvars = "test_period", withinvars = "condition", idvar = "Subject_id", na.rm = TRUE, conf.interval = 0.95)

sum2 <- summarySEwithin(gng_rt, measurevar = "response_time", betweenvars = "condition", withinvars = "test_period", idvar = "Subject_id", na.rm = TRUE, conf.interval = 0.95)



gng_rt_subs <- aggregate(response_time~ condition + test_period + Subject_id, data = gng_rt, FUN = mean)

sum3 <- summarySE(gng_rt_subs, measurevar = "response_time", groupvars = c("condition", "test_period") )
