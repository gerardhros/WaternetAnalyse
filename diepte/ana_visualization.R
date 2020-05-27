# Analyze relations between water depth and other variables
# 'loc_sf' needed

library(ggplot2); ggplot(data.table)


## water width --------------

## Explore relation between water depth and water width


## ditch width vs depth (line features only)
setDT(loc_sf)
loc_sf2 <- loc_sf[!is.na(med_wd) & !is.na(med_wb) & MORFOLOGIE == "lijnvormig",] 
loc_sf3 <- loc_sf[!is.na(med_wd) & !is.na(breedte) & MORFOLOGIE == "lijnvormig",] 
loc_sf <- st_as_sf(loc_sf)

ggplot(loc_sf2) + 
  geom_point(aes(x = med_wb, y = med_wd, col = soiltypen))  +
  geom_smooth(aes(x = med_wb, y = med_wd), formula = y ~log(x),
              method = "lm", se = FALSE, color="black", lty = 2) +
  xlab("sloot breedte (m)") + ylab("sloot diepte (m)")
# summary(lm(med_wd ~ log(med_wb), data = loc_sf2))

ggplot(loc_sf3) + 
  geom_point(aes(x = breedte, y = med_wd, col = soiltypen))  +
  geom_smooth(aes(x = breedte, y = med_wd), formula = y ~log(x),
              method = "lm", se = FALSE, color="black", lty = 2) +
  xlab("sloot breedte (m)") + ylab("sloot diepte (m)")
# summary(lm(med_wd ~ log(breedte), data = loc_sf3))

# # non-linear fitting
# # fit <- nls(data = loc_sf2, 
# #            formula = med_wd ~ L / (1 + (L-P0)/P0 * exp(-k * med_wb)),
# #            start = list(L = 2, k = 0.1, P0 = 0.1))
# fit <- nls(data = loc_sf2,
#            formula = med_wd ~ L / (1 + (L-0.1)/0.1 * exp(-k * med_wb)),
#            start = list(L = 2, k = 0.1))
# new_x = data.frame(med_wb = seq(from = 0, to = 50, length.out = 100))
# ggplot(loc_sf2) + 
#   geom_point(aes(x = med_wb, y = med_wd, col = soiltype.n))  +
#   geom_line(data = new_x,
#             aes(x=med_wb, y=predict(fit, newdata=new_x)),
#             color='black', lty = 2) +
#   xlab("sloot breedte (m)") + ylab("sloot diepte (m)")



# compare measured and computed water width
plot(loc_sf$med_wb, loc_sf$pnt_breedte, pch = 16, xlim = c(0,150),
     xlab = "water breedte (m) gemeten", ylab = "water breedte (m) berekend")

## Peil vs AHN ---------------
ggplot(loc_sf) + 
  geom_point(aes(x=PEIL, y = pnt_ahn, col = soiltypen, pch = MORFOLOGIE)) + 
  scale_color_discrete(name="Bodemtype", breaks=c("1", "2", "3"),
                      labels=c("zand", "klei", "veen")) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Waterpeil (m)") + ylab("AHN (m)") 

# Compute deviation of ahn from peil 
# (A positive value means that actual water surface is higher than peil)
setDT(loc_sf)
loc_sf[, afw_ahn := pnt_ahn - PEIL]
loc_sf <- st_as_sf(loc_sf)


## Compare deviation in AHN vs other variables
# deviation vs slibdiepte
ggplot(loc_sf) + 
  geom_point(aes(x=med_sd, y = afw_ahn, col = soiltypen)) +
  scale_color_discrete(name="Bodemtype", breaks=c("1", "2", "3"),
                       labels=c("zand", "klei", "veen")) +
  #scale_x_log10() + 
  xlab("slib diepte (m)") + ylab("afwijking AHN van peil (AHN - peil, m)")
  
# deviation vs soiltype
ggplot(loc_sf) + 
  geom_boxplot(aes(x=soiltypen, y = afw_ahn, col = soiltypen)) +
  scale_color_discrete(name="Bodemtype", breaks=c("1", "2", "3"),
                       labels=c("zand", "klei", "veen")) +
  xlab("bodemtype") + ylab("afwijking AHN van peil (AHN - peil, m)")

# deviation vs waterbreedte
ggplot(loc_sf) + 
  geom_point(aes(x=breedte, y = afw_ahn, col = soiltypen)) +
  scale_color_discrete(name="Bodemtype", breaks=c("1", "2", "3"),
                       labels=c("zand", "klei", "veen")) +
  xlim(c(0,50)) +
  xlab("water breedte (m)") + ylab("afwijking AHN van peil (AHN - peil, m)")

# deviation vs seepage
ggplot(loc_sf) + 
  geom_point(aes(x=KWEL, y = afw_ahn, col = soiltypen)) +
  scale_color_discrete(name="Bodemtype", breaks=c("1", "2", "3"),
                       labels=c("zand", "klei", "veen")) +
  xlab("kwel (m)") + ylab("afwijking AHN van peil (AHN - peil, m)")

# rf_res <- randomForest(afw_ahn ~ soiltypen + med_sd + breedte + KWEL + MORFOLOGIE, 
#                        data = loc_sf, 
#                        mtry = 2, ntree = 400)


## Waterpeil -----------
ggplot(loc_sf) +
  geom_point(aes(x = PEIL, y = med_wd, col = MORFOLOGIE)) +
  ylim(c(0,3)) +  ylab("sloot water diepte (m)")

## Soil type  -----------
# # ANOVA
# aov_res <- aov(log(loc_sf$med_sd+0.01) ~ as.factor(loc_sf$soiltype.n))
# aov_res2 <- aov(log(loc_sf$med_sd+0.01) ~ as.factor(loc_sf$soiltype.n) + loc_sf$MORFOLOGIE)
# TukeyHSD(aov_res)


ggplot(loc_sf) + geom_boxplot(aes(x = soiltype.n, y = med_wd)) +
  facet_wrap(.~MORFOLOGIE, scales = "free") +
  ylab("sloot water diepte (m)") +  scale_y_log10() 
ggplot(loc_sf) + geom_boxplot(aes(x = soiltype.n, y = med_sd)) +
  facet_wrap(.~MORFOLOGIE, scales = "free") +
  ylab("sloot slib diepte (m)") +  scale_y_log10()


## Seepage --------------
ggplot(loc_sf) + geom_point(aes(x = KWEL, y = med_wd, col = MORFOLOGIE)) +
  facet_wrap(.~MORFOLOGIE, scales = "free") +
  xlab("kwel") + ylab("sloot water diepte (m)")


## Theoretical water depth -----------
ggplot(loc_sf2) + geom_point(aes(x = theo_dep, y = med_wd, col = MORFOLOGIE)) + ylim(c(0,3)) +
  geom_abline(intercept = 0, slope = 1)  +  
  xlab("theor. water diepte (m)") + ylab("sloot water diepte (m)")
ggplot(loc_sf2) + geom_point(aes(x = theo_dep, y = med_td, col = MORFOLOGIE)) + ylim(c(0,3)) +
  geom_abline(intercept = 0, slope = 1)  +  
  xlab("theor. water diepte (m)") + ylab("total (water + slib) diepte (m)")
#cor.test(loc_sf2$theo_dep[loc_sf2$MORFOLOGIE == "lijnvormig"], loc_sf2$med_td[loc_sf2$MORFOLOGIE == "lijnvormig"])


## Water type  -----------

ggplot(loc_sf) + geom_boxplot(aes(x = WATERTYPE, y = med_wd)) +
  #facet_wrap(.~MORFOLOGIE, scales = "free") +
  ylab("sloot water diepte (m)") +  scale_y_log10() 