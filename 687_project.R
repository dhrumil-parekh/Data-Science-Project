library(tidyverse)
library(ggplot2)
library(viridis)
library(hrbrthemes)

URL <- "https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv"
data_hotel <- read_csv(URL)
view(data_hotel)
is.na(hotel$IsCanceled)
summary(data_hotel)
str(data_hotel)
table(hotel$MarketSegment)
plot(as.factor(hotel$IsCanceled))
sum(is.na(data_hotel$IsCanceled))

new_data <- gsub(" ", "", data_hotel$Babies)



not_repeated_guest <- subset(hotel, IsRepeatedGuest==0)
repeated_guest
a <- nrow(not_repeated_guest)

repeated_guest <- subset(hotel, IsRepeatedGuest==1)
b <- nrow(repeated_guest)

table(hotel$DepositType)

subset(hotel, DepositType=="Non Refund")




not_cancelled <- subset(data_hotel, IsCanceled==0)
view(not_cancelled)
dim(not_cancelled)


#Q. Calculating no. of non-cancellations stays in weekend nights
sum(not_cancelled$StaysInWeekendNights) 
#32813

#Q. Calculating no. of non-cancellations stays in week nights
sum(not_cancelled$StaysInWeekNights)
#87074
#therefore we come to know that people prefer to come to hotel on week nights instead of weekend nights.

as.factor(not_cancelled$IsCanceled)


#market data 
boxplot(IsCanceled~MarketSegment, data = data_hotel, xlab = "Market Segment",
        ylab = "Number of bookings", main = "Market Data")

boxplot(StaysInWeekendNights, data = hotel_data,
        ylab = "weekend Nights Bookings", main = "Market Data")





#total stay duration
ggplot(not_cancelled, aes(StaysInWeekNights + StaysInWeekendNights)) + 
  geom_density(col = "red") + theme_bw()

#barchart
ggplot(not_cancelled, aes(x=MarketSegment, y=LeadTime*0.0001)) + geom_bar(stat = "identity")
#online TA market have much more lead time than other users



ggplot(data = hotel_data, aes(
  x = ,
  y = lead_time,
  fill = factor(is_canceled)
)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Cancellation By Hotel Type",
    subtitle = "Based on Lead Time",
    x = "Hotel Type",
    y = "Lead Time (Days)"
  ) +
  scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Cancelled", "Not Cancelled")
  ) + theme_light()



#boxplot(data_hotel$IsCanceled,data_hotel$StaysInWeekendNights,
        #data=data_hotel, main="No. of Cancellation", xlab="no. of cancellation", ylab="Stay on Weekends")






#boxplot for bookings in week nights in each market Segment with assigned room type
dev.new()
hotel_data %>%
  ggplot( aes(x=MarketSegment, y=StaysInWeekNights, fill=AssignedRoomType)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("Market Segment") +ylab("Weekend Nights Bookings")



#Boxplot for bookings in weeknd nights according to customer type and reserved room type
dev.new()
data_hotel %>%
  ggplot(aes(x=CustomerType,y=StaysInWeekendNights, fill=ReservedRoomType)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("Customer Type") +ylab("Weekend Nights Bookings")


#Boxplot for weekend Night bookings with Deposit Type
dev.new()
hotel_data %>%
  ggplot( aes(x=DepositType,y=StaysInWeekendNights)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  ggtitle("Boxplot for weekend Night bookings with Deposit Type") +
  xlab("") +ylab("Weekend Nights Bookings")




dev.new()
babies <- ggplot(data_hotel, aes(x=Babies))+geom_histogram(binwidth = 1)+ ggtitle("Distribution of Babies Variable")+xlab("Babies")
babies
dev.off()

dev.new()
adults <- ggplot(data_hotel , aes(x=Adults))+geom_histogram(binwidth = 5)+ ggtitle("Distribution of Adults Variable")+xlab("Adults")+ theme(plot.title = element_text(hjust = 0.5))
adults







#association rule mining
library("arules")

hotel_data_fac <- data.frame(IsCanceled = as.factor(data_hotel$IsCanceled),
                             LeadTime = as.factor(data_hotel$LeadTime),
                             StaysInWeekendNights = as.factor(data_hotel$StaysInWeekendNights),
                             StaysInWeekNights = as.factor(data_hotel$StaysInWeekNights),
                             Adults = as.factor(data_hotel$Adults),
                             Children = as.factor(data_hotel$Children),
                             Babies = as.factor(data_hotel$Babies),
                             Meal = as.factor(data_hotel$Meal),
                             Country = as.factor(data_hotel$Country),
                             MarketSegment = as.factor(data_hotel$MarketSegment),
                             IsRepeatedGuest = as.factor(data_hotel$IsRepeatedGuest),
                             PreviousCancellations = as.factor(data_hotel$PreviousCancellations),
                             PreviousBookingsNotCanceled = as.factor(data_hotel$PreviousBookingsNotCanceled),
                             ReservedRoomType = as.factor(data_hotel$ReservedRoomType),
                             AssignedRoomType = as.factor(data_hotel$AssignedRoomType),
                             BookingChanges = as.factor(data_hotel$BookingChanges),
                             DepositType = as.factor(data_hotel$DepositType),
                             CustomerType = as.factor(data_hotel$CustomerType),
                             RequiredCarParkingSpaces = as.factor(data_hotel$RequiredCarParkingSpaces),
                             TotalOfSpecialRequests = as.factor(data_hotel$TotalOfSpecialRequests))

CanceledBook <- table(hotel_data_fac$IsCanceled)
prop.table(CanceledBook) #27.76335 Canceled Bookings

hotel_data_tran <- as(hotel_data_fac, "transactions")







rules1 <- apriori(hotel_data_tran,
                  parameter=list(supp=0.007, conf=0.55, minlen=4, maxlen = 5),
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("IsCanceled=1")))
inspect(rules1[1:10])

