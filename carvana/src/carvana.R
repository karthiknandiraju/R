library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages("xlsx")
carvana <- read.csv("carvanaData.csv")
#1.Numebr of cars purchased 
nrow(carvana)
length(unique(carvana$RefId))
### Number of cars purchased is 121690 

nonlemoncars <- carvana %>% filter(carvana$IsBadBuy == 0)
#2.Number of lemon cars 

lemoncars <- carvana %>% filter(carvana$IsBadBuy == 1)
nrow(lemoncars)
(8976/121690)*100

# 8976 out of 121690 cars are lemon which is 7.37 percentage

#3.lemon cars based on the Color

lemoncolor <- lemoncars %>% select(RefId, Color) %>% group_by(Color) %>% summarise(lemons = n()) 
lemoncolor
## Silver color cars are more in lemon(1843) , next white and blue 
## yellow color cars are less lemon
colvals <- c('beige','black','blue','brown','gold','green','grey','maroon','darkblue','pink','orange','darkgreen','purple','red',"#F0FFFF",'white','yellow')
plot <- ggplot(data = lemoncolor, aes(x = Color, y = lemons )) + geom_bar(aes(fill = Color), stat = "identity") +scale_fill_manual(values = colvals) + ylab('# Lemon Cars') + theme_dark()
plot

#4. lemon cars based on Transmission

lemontransmission <- lemoncars %>% select(RefId, Transmission) %>% group_by(Transmission) %>% summarise(count = n()) 
lemontransmission
plot2 <- ggplot(data = lemontransmission, aes(x = Transmission, y = count)) + geom_bar(aes(fill = Transmission), stat = 'identity') + ylab('# Lemon Cars') + theme_dark()
plot2


# Auto cars(8676) are more in lemon when compared to Manual(299)

#5. The silver lemon auto cars 

silverautolemon <- lemoncars %>% select(RefId, Color, Transmission) %>% filter(Transmission == 'AUTO') %>% group_by(Color) %>% summarise(count=n()) 
nrow(silverautolemon)


### Silver Auto lemon cars are 1786, White Auto lemon cars are 1450

### White, Silver, Blue Auto cars are more in lemon category 
colvals <- c('beige','black','blue','brown','gold','green','grey','maroon','darkblue','orange','darkgreen','purple','red',"#F0FFFF",'white','yellow')
colvals
plot3 <- ggplot(data = silverautolemon , aes(x = Color, y = count)) + geom_bar(aes(fill = Color),stat = 'identity') + ylab('# Auto Lemon Cars') + theme_dark() + scale_fill_manual(values = colvals)
plot3

##6. Lemon Cars wheel type 

lemonwheel <- lemoncars %>% select(RefId, WheelType) %>% group_by(WheelType) %>% summarise(count = n())

plot4 <- ggplot(data = lemonwheel , aes(x = WheelType, y = count)) + geom_bar(aes(fill = WheelType),stat = 'identity') + ylab('#Lemon Cars') + theme_dark()
plot4

#Lemon cars with Alloy wheels are more in number(3985) when compared to Cover(2655) and Special(99) Wheel cars.

##7. Lemon cars average vehicle age

lemoncars_age <- lemoncars %>% select(RefId, VehicleAge)  %>% summarize(lemon_average_age = mean(VehicleAge, na.rm =  TRUE))
lemoncars_age
plot5 <- ggplot(data = lemoncars , aes(x = VehicleAge)) +geom_histogram(binwidth = 4)
plot5
### Non lemon cars average age 

nonlemoncars_age <- nonlemoncars %>% select(RefId, VehicleAge) %>% filter(VehicleAge > 2.5) %>% summarize(nonlemon_average_age = mean(VehicleAge, na.rm =  TRUE))
nonlemoncars_age
plot6 <- ggplot(data = nonlemoncars , aes(x = VehicleAge)) +geom_histogram(binwidth = 4, aes(fill = VehicleAge ))
plot6

###### Non lemon car analysis ##########################################

plot <- ggplot(data = carvana, aes(x = IsBadBuy)) + geom_bar()
plot
nonlemoncars <- carvana %>% filter(carvana$IsBadBuy == 0)
#1..Number of non lemon cars 

nrow(nonlemoncars)
(64007/121690)*100

# 64007 out of 121690 cars are non lemon which is 52.59841 percentage

#2.Non lemon cars based on the Color

nonlemoncolor <- nonlemoncars %>% select(RefId, Color) %>% group_by(Color) %>% summarise(nonlemons = n()) 
nonlemoncolor
## Silver color cars are more in non lemon(13032) , next white and blue 
## yellow color cars are less lemon
colvals <- c('beige','black','blue','brown','gold','green','grey','maroon','darkblue','pink','orange','darkgreen','purple','red',"#F0FFFF",'white','yellow')
plot <- ggplot(data = nonlemoncolor, aes(x = Color, y = nonlemons )) + geom_bar(aes(fill = Color), stat = "identity") +scale_fill_manual(values = colvals) + ylab('#Non Lemon Cars') + theme_dark()
plot

#3.Non lemon cars based on Transmission

nonlemontransmission <- nonlemoncars %>% select(RefId, Transmission) %>% group_by(Transmission) %>% summarise(count = n()) 
nonlemontransmission
plot2 <- ggplot(data = nonlemontransmission, aes(x = Transmission, y = count)) + geom_bar(aes(fill = Transmission), stat = 'identity') + ylab('#Non Lemon Cars') + theme_dark()
plot2

##4.Non Lemon Cars wheel type 

nonlemonwheel <- nonlemoncars %>% select(RefId, WheelType) %>% group_by(WheelType) %>% summarise(count = n())

plot4 <- ggplot(data = nonlemonwheel , aes(x = WheelType, y = count)) + geom_bar(aes(fill = WheelType),stat = 'identity') + ylab('#Lemon Cars') + theme_dark()
plot4


##5.Non Lemon cars average vehicle age

nonlemoncars_age <- nonlemoncars %>% select(RefId, VehicleAge)  %>% summarize(nonlemon_average_age = mean(VehicleAge, na.rm =  TRUE))
nonlemoncars_age
plot5 <- ggplot(data = lemoncars , aes(x = VehicleAge)) +geom_histogram(binwidth = 4)
plot5

