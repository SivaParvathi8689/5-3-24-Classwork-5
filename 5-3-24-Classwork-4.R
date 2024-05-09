#Sivaparvathi Yanikapati
inc <- read.csv("C:\\Users\\yspar\\Downloads\\Income.csv") # Open Income.csv
head(inc) # 5 variables
# Load ggplot2 package
library(ggplot2)
# Now you can use qplot
qplot(x = inc$Age, y = inc$Income) # Example plot
# Lets look around first
qplot( x = inc$Education , y= inc$Income) # A little bit on Education
qplot( x = inc$Gender , y= inc$Income) # not so much on gender
results = lm ( inc$Income~inc$Age+inc$Education+inc$Gender )
results
summary(results)


inc <- read.csv("C:\\Users\\yspar\\Downloads\\Income.csv")
head(inc) # 5 variables
# Lets look around first
qplot( x = inc$Age , y= inc$Income) # I guess income depends on Age
qplot( x = inc$Education , y= inc$Income) # A little bit on Education
qplot( x = inc$Gender , y= inc$Income) # not so much on gender
results2 = lm ( inc$Income~inc$Age+inc$Education )
results2
summary(results2)


# Generate a linearly relation
x <- runif(75,0,10) # 75 random numbers of uniform distribution
x <- sort(x)
y <- 20 + 10*x + rnorm(75,0,10) # y= 20+10x and a bit of variety
# see it!
plot(x,y)
# Now lets do the regression
lr <- lm(y~x)
lr # print out the deducted equation
# draw the linear regression line
points( x, lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 )


x <- runif(75,0,10) # 75 random numbers of uniform distribution
x<- sort(x)
y <- 200 + x^3 - 10 * x^2 + x + rnorm(75,0,20)
plot(x,y)
lr <- lm(y~x)
lr
points(x,lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 )


x <- runif(75,0,10) # 75 random numbers of uniform distribution
x<- sort(x)
y <- 200 + x^3 - 10 * x^2 + x + rnorm(75,0,20)
plot(x,y)
lr <- lm(y~x)
lr
poly <- loess(y~x) # Polynomial regression
fit <- predict(poly)
points(x,fit, type="l", col=2)
points(x,lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 )


churn_input <- read.csv("C:\\Users\\yspar\\Downloads\\Churn.csv") # open the Churn.csv file on blackboard
head(churn_input)
sum(churn_input$Churned)
# Lets look around first at those who actually churned
ch <- churn_input[churn_input$Churned=="1",]
qplot( x = ch$Churned_contacts )
qplot( x = ch$Age )
qplot( x = ch$Married )
qplot( x = ch$Cust_years)
# Now let's see those who did not churn
nch <- churn_input[churn_input$Churned=="0",]
qplot( x = nch$Churned_contacts )
qplot( x = nch$Age )
qplot( x = nch$Married )
qplot( x = nch$Cust_years)


Churn_logistic1 <- glm (Churned~Age + Married + Cust_years + Churned_contacts,
                        data=churn_input, family=binomial(link="logit"))
Churn_logistic1
summary(Churn_logistic1) # Eliminate Married and Cust_years
Churn_logistic2 <- glm (Churned~Age + Churned_contacts,
                        data=churn_input, family=binomial(link="logit"))
Churn_logistic2
summary(Churn_logistic2)

#Execute below code to generate three new vectors
Countries_2012_Dataset <- c("Aruba","Afghanistan","Angola","Albania","United Arab Emirates","Argentina","Armenia","Antigua and Barbuda","Australia","Austria","Azerbaijan","Burundi","Belgium","Benin","Burkina Faso","Bangladesh","Bulgaria","Bahrain","Bahamas, The","Bosnia and Herzegovina","Belarus","Belize","Bermuda","Bolivia","Brazil","Barbados","Brunei Darussalam","Bhutan","Botswana","Central African Republic","Canada","Switzerland","Chile","China","Cote d'Ivoire","Cameroon","Congo, Rep.","Colombia","Comoros","Cabo Verde","Costa Rica","Cuba","Cayman Islands","Cyprus","Czech Republic","Germany","Djibouti","Denmark","Dominican Republic","Algeria","Ecuador","Egypt, Arab Rep.","Eritrea","Spain","Estonia","Ethiopia","Finland","Fiji","France","Micronesia, Fed. Sts.","Gabon","United Kingdom","Georgia","Ghana","Guinea","Gambia, The","Guinea-Bissau","Equatorial Guinea","Greece","Grenada","Greenland","Guatemala","Guam","Guyana","Hong Kong SAR, China","Honduras","Croatia","Haiti","Hungary","Indonesia","India","Ireland","Iran, Islamic Rep.","Iraq","Iceland","Israel","Italy","Jamaica","Jordan","Japan","Kazakhstan","Kenya","Kyrgyz Republic","Cambodia","Kiribati","Korea, Rep.","Kuwait","Lao PDR","Lebanon","Liberia","Libya","St. Lucia","Liechtenstein","Sri Lanka","Lesotho","Lithuania","Luxembourg","Latvia","Macao SAR, China","Morocco","Moldova","Madagascar","Maldives","Mexico","Macedonia, FYR","Mali","Malta","Myanmar","Montenegro","Mongolia","Mozambique","Mauritania","Mauritius","Malawi","Malaysia","Namibia","New Caledonia","Niger","Nigeria","Nicaragua","Netherlands","Norway","Nepal","New Zealand","Oman","Pakistan","Panama","Peru","Philippines","Papua New Guinea","Poland","Puerto Rico","Portugal","Paraguay","French Polynesia","Qatar","Romania","Russian Federation","Rwanda","Saudi Arabia","Sudan","Senegal","Singapore","Solomon Islands","Sierra Leone","El Salvador","Somalia","Serbia","South Sudan","Sao Tome and Principe","Suriname","Slovak Republic","Slovenia","Sweden","Swaziland","Seychelles","Syrian Arab Republic","Chad","Togo","Thailand","Tajikistan","Turkmenistan","Timor-Leste","Tonga","Trinidad and Tobago","Tunisia","Turkey","Tanzania","Uganda","Ukraine","Uruguay","United States","Uzbekistan","St. Vincent and the Grenadines","Venezuela, RB","Virgin Islands (U.S.)","Vietnam","Vanuatu","West Bank and Gaza","Samoa","Yemen, Rep.","South Africa","Congo, Dem. Rep.","Zambia","Zimbabwe")
Codes_2012_Dataset <- c("ABW","AFG","AGO","ALB","ARE","ARG","ARM","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BWA","CAF","CAN","CHE","CHL","CHN","CIV","CMR","COG","COL","COM","CPV","CRI","CUB","CYM","CYP","CZE","DEU","DJI","DNK","DOM","DZA","ECU","EGY","ERI","ESP","EST","ETH","FIN","FJI","FRA","FSM","GAB","GBR","GEO","GHA","GIN","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUM","GUY","HKG","HND","HRV","HTI","HUN","IDN","IND","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAR","MDA","MDG","MDV","MEX","MKD","MLI","MLT","MMR","MNE","MNG","MOZ","MRT","MUS","MWI","MYS","NAM","NCL","NER","NGA","NIC","NLD","NOR","NPL","NZL","OMN","PAK","PAN","PER","PHL","PNG","POL","PRI","PRT","PRY","PYF","QAT","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SLB","SLE","SLV","SOM","SRB","SSD","STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCD","TGO","THA","TJK","TKM","TLS","TON","TTO","TUN","TUR","TZA","UGA","UKR","URY","USA","UZB","VCT","VEN","VIR","VNM","VUT","PSE","WSM","YEM","ZAF","COD","ZMB","ZWE")
Regions_2012_Dataset <- c("The Americas","Asia","Africa","Europe","Middle East","The Americas","Asia","The Americas","Oceania","Europe","Asia","Africa","Europe","Africa","Africa","Asia","Europe","Middle East","The Americas","Europe","Europe","The Americas","The Americas","The Americas","The Americas","The Americas","Asia","Asia","Africa","Africa","The Americas","Europe","The Americas","Asia","Africa","Africa","Africa","The Americas","Africa","Africa","The Americas","The Americas","The Americas","Europe","Europe","Europe","Africa","Europe","The Americas","Africa","The Americas","Africa","Africa","Europe","Europe","Africa","Europe","Oceania","Europe","Oceania","Africa","Europe","Asia","Africa","Africa","Africa","Africa","Africa","Europe","The Americas","The Americas","The Americas","Oceania","The Americas","Asia","The Americas","Europe","The Americas","Europe","Asia","Asia","Europe","Middle East","Middle East","Europe","Middle East","Europe","The Americas","Middle East","Asia","Asia","Africa","Asia","Asia","Oceania","Asia","Middle East","Asia","Middle East","Africa","Africa","The Americas","Europe","Asia","Africa","Europe","Europe","Europe","Asia","Africa","Europe","Africa","Asia","The Americas","Europe","Africa","Europe","Asia","Europe","Asia","Africa","Africa","Africa","Africa","Asia","Africa","Oceania","Africa","Africa","The Americas","Europe","Europe","Asia","Oceania","Middle East","Asia","The Americas","The Americas","Asia","Oceania","Europe","The Americas","Europe","The Americas","Oceania","Middle East","Europe","Europe","Africa","Middle East","Africa","Africa","Asia","Oceania","Africa","The Americas","Africa","Europe","Africa","Africa","The Americas","Europe","Europe","Europe","Africa","Africa","Middle East","Africa","Africa","Asia","Asia","Asia","Asia","Oceania","The Americas","Africa","Europe","Africa","Africa","Europe","The Americas","The Americas","Asia","The Americas","The Americas","The Americas","Asia","Oceania","Middle East","Oceania","Middle East","Africa","Africa","Africa","Africa")


#---------------------creating data frames

mydf <-data.frame(Countries_2012_Dataset,Codes_2012_Dataset,Regions_2012_Dataset)
mydf
head(mydf)
colnames(mydf) <- c("Country", "Code", "Region")
head(mydf)

# another way to do it, one step
rm(mydf)
mydf <-data.frame(Country= Countries_2012_Dataset,Code= Codes_2012_Dataset,Region=Regions_2012_Dataset)
head(mydf)

tot <- cbind(stats, mydf) # assuming same ordering
head(tot)
qplot(data= tot, x= Internet.users, y=Birth.rate, color=Region)

#better merging

merged <- merge(stats, mydf, by.x= "Country.Code", by.y="Code")
head(merged)
merged$Country <- NULL
head(merged)
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region)

#shapes  0-25
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(17))
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(2))
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(15))
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(23))

# Transparency  0---1

qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(19), alpha=I(0.7))
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(19), alpha=I(0.3))
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(19), alpha=I(0.5))
# All four variables
qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=Income.Group, alpha=I(0.5))
# Add a title

qplot(data= merged, x= Internet.users, y=Birth.rate, color=Region,
      size = I(4), shape=I(19), alpha=I(0.7),
      main="Birth rate vs Internet Users")

cont <-
  data.frame(Countries_2012_Dataset,Codes_2012_Dataset,Regions_2012_Dataset)
cont
head(cont)
colnames(cont) <- c("Country", "Code", "Region")
head(cont)
# Create the data frame
cont <- data.frame(Country = Countries_2012_Dataset, Code = Codes_2012_Dataset, Region = Regions_2012_Dataset)

# View the first few rows of the data frame
head(cont)

