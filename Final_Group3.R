# Loading Required Libraries
library(dplyr)
library(tidyr)
library(googleVis)
library(fmsb)
library(treemap)
library(ggplot2)

# Loading the data
data <- read.csv('cricket_data.csv',header = T)
View(data)
str(data)

# Cleaning up data
data[data==''] <- NA
data[data=='-'] <- NA

data[,c(20:181)] <- lapply(data[,c(20:181)],as.numeric )



#------------------ TREE MAP ----------------------------
# Creating a subset with required columns
tree <- subset(data,select = c(2,32,46,60,74,88,102,
                               #Stumps
                               33,47,61,75,89,103))
View(tree)
str(tree)

# Change Column Names to a correct format
colnames(tree)[2:13]<-c('Catches Test',
                        'Catches ODIs',
                        'Catches T20Is',
                        'Catches First Class',
                        'Catches List A',
                        'Catches T20s',
                        'Stumps Test',
                        'Stumps ODIs',
                        'Stumps T20Is',
                        'Stumps First Class',
                        'Stumps List A',
                        'Stumps T20s')
# Fill NA with zeroes
tree[is.na(tree)] <- 0

# Score Players
tree <- mutate(tree,Score=(tree$`Catches Test`+
                       tree$`Catches ODIs`+
                       tree$`Catches T20Is`+
                       tree$`Catches First Class`+
                       tree$`Catches List A`+
                       tree$`Catches T20s`+
                       2*(tree$`Stumps Test`)+
                      2*(tree$`Stumps First Class`)+
                      2*(tree$`Stumps T20Is`)+
                      2*(tree$`Stumps List A`)+
                      2*(tree$`Stumps ODIs`)+
                      2*(tree$`Stumps T20s`)))

# Select Top10 best players
tree <- tree %>% arrange(desc(Score)) %>% head(10)

tree_map <- gather(tree,'Format','Value',2:13)
View(tree_map)
str(tree)

# Generating a Tree Map
treemap(tree_map,index = c('NAME','Format'),
        vSize = 'Value',
        type = 'value',
        vColor = 'Value',
        align.labels = list(c("left", "top"),
                            c("left","bottom"),c("right","top")),
        title = 'Top 10 Wicketkeepers')



#------------- BAR CHART ----------------------
#Bowling
s1 <- subset(data, select = c(NAME, BOWLING_Tests_Inns,BOWLING_Tests_Wkts, BOWLING_ODIs_Inns, BOWLING_ODIs_Wkts, BOWLING_T20Is_Inns, BOWLING_T20Is_Wkts))
str(s1)

# Selecting top 4 bowlers
s2 <- s1 %>% filter(NAME %in% c("Harbhajan Singh",
                                "Muttiah Muralitharan",
                                "Shaun Pollock",
                                "Dale Steyn"))


s3 <- na.omit(s2)
str(s3)

s4 <- gather(s3 ,key = 'Format',value = 'Value', 2:7)
str(s4)
s4$Format <- as.character(gsub("BOWLING_","",s4$Format))

counts <- table(s4$Value)
p1 <- ggplot(s4, aes(x = Format, y = Value, fill = Format), position = "dodge") + geom_bar(stat='identity') + facet_wrap(~NAME) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = 'Top 4 Bowlers Performance in International Cricket')

p1




#------------------- Radar Chart --------------------------
unique(data$Playing.role)
spider <- subset(data,select = 
                   c(2,12,
                     # Innings
                     21,35,49,63,77,91,
                     # Batting
                     23,37,51,65,79,93,
                     # Wickets
                     108,121,134,147,160,173,
                     # Catches
                     32,46,60,74,88,102,
                     #Centuries
                     28,42,56,70,84,98
                     ))
View(spider)

# Filtering only All rounders
spider <- spider %>% filter(Playing.role=='Allrounder')

# Filling all NA values with 0
spider[is.na(spider)] <- 0

# Creating a score column
spider <- spider %>% mutate(Score = (BATTING_ODIs_Inns+
                    BATTING_T20Is_Inns+
                    BATTING_First.class_Inns+
                    BATTING_List.A_Inns+
                    BATTING_Tests_Inns+
                    BATTING_T20s_Inns+
                    2*(BATTING_ODIs_Runs)+
                    2*(BATTING_T20Is_Runs)+
                    2*(BATTING_First.class_Runs)+
                    2*(BATTING_List.A_Runs)+
                    2*(BATTING_Tests_Runs)+
                    2*(BATTING_T20s_Runs)+
                    2*(BOWLING_ODIs_Wkts)+
                    2*(BOWLING_T20Is_Wkts)+
                    2*(BOWLING_First.class_Wkts)+
                    2*(BOWLING_List.A_Wkts)+
                    2*(BOWLING_Tests_Wkts)+
                    2*(BOWLING_T20s_Wkts)+
                    BATTING_ODIs_Ct+
                    BATTING_T20Is_Ct+
                    BATTING_First.class_Ct+
                    BATTING_List.A_Ct+
                    BATTING_Tests_100+
                    BATTING_T20s_100+
                    BATTING_ODIs_100+
                    BATTING_T20Is_100+
                    BATTING_First.class_100+
                    BATTING_List.A_100+
                    BATTING_Tests_100+
                    BATTING_T20s_100)/10)
# Filtering the top5 score players
spider_arr <- spider %>% arrange(desc(Score)) %>% top_n(5)
View(spider_arr)
str(spider_arr)

# Creating Individual Values 
spider_arr <- spider_arr %>% mutate(Innings=BATTING_ODIs_Inns+
                        BATTING_T20Is_Inns+
                        BATTING_First.class_Inns+
                        BATTING_List.A_Inns+
                        BATTING_Tests_Inns+
                        BATTING_T20s_Inns,
                      Runs=BATTING_ODIs_Runs+
                        BATTING_T20Is_Runs+
                        BATTING_First.class_Runs+
                        BATTING_List.A_Runs+
                        BATTING_Tests_Runs+
                        BATTING_T20s_Runs,
                      Wickets=BOWLING_ODIs_Wkts+
                        BOWLING_T20Is_Wkts+
                        BOWLING_First.class_Wkts+
                        BOWLING_List.A_Wkts+
                        BOWLING_Tests_Wkts+
                        BOWLING_T20s_Wkts,
                      Catches=BATTING_ODIs_Ct+
                        BATTING_T20Is_Ct+
                        BATTING_First.class_Ct+
                        BATTING_List.A_Ct+
                        BATTING_Tests_Ct+
                        BATTING_T20s_Ct,
                      Centuries=BATTING_Tests_100+
                        BATTING_ODIs_100+
                        BATTING_T20Is_100+
                        BATTING_First.class_100+
                        BATTING_List.A_100+
                        BATTING_T20s_100)
# Finding the minimum and maximum values
cmax <- apply(spider_arr,2, function(x) max(x))
cmin <- c(rep(0,38))

# Bind them with the data set
spider_arr<- rbind(spider_arr,cmax,cmin)
View(spider_arr)
row.names(spider_arr) <- c(paste(spider_arr[c(1:5),1]),"Max","Min")
str(spider_arr)
spider_arr[,c(3:38)] <- lapply(spider_arr[,c(3:38)],as.numeric)

# Creating radarcharts
for (i in 1:5){
  radarchart(spider_arr[c(6,7,i),c(34:38)],
             plty = 1, 
             plwd = 2,
             pcol=4,
             pfcol = rgb(0, 0.4, 1, 0.25),
             title = row.names(spider_arr)[i])
}


#---------- SCATTER PLOT --------------
# Creating a subset of data
b1 <- subset(data, select = c(NAME, BATTING_Tests_Inns,BATTING_Tests_Runs, 
                              BATTING_ODIs_Inns, BATTING_ODIs_Runs, BATTING_T20Is_Inns, BATTING_T20Is_Runs))
b2 <- na.omit(b1)

# Selecting top 4 batsmen
b3 <- b2 %>% filter(NAME %in% c("Virat Kohli",
                                "Sachin Tendulkar",
                                "Sir Alastair Cook",
                                "Ricky Pointing",
                                "Jacques Kallis"))


b4 <- gather(b3 ,key = 'Format',value = 'Value', 2:7)
b4$Format <- as.character(gsub("BATTING_","",b4$Format))
counts <- table(b4$Value)
View(b4)
str(b4)

# Creating a Scatterplot
p2 <- ggplot(b4, aes(x = Format,y = Value, color = NAME,label=NAME)) +
  geom_jitter(aes(size=Value)) +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Top 4 Batsmen's Stats in International Cricket")
  
p2


#---------------- HEAT MAP ----------------------------
# HEATMAP of Boundaries from each country
map <- subset(data, select = c(3,30,31,44,45,58,59,72,73,86,87,100,101))
View(map)
str(map)

# Remove NA values
map[is.na(map)] <- 0

# Check if any null values are left
sum(is.na(map))

# Countries with Most Boundaries (Interactive Geographical Heat map)
# Gather to get all columns for each country
map_all <- gather(map,key = 'Type',value = 'Boundaries',2:13)
View(map_all)

map_sum <- map_all %>% 
  group_by(COUNTRY) %>% summarise(Boundaries=sum(Boundaries)) %>% 
    filter(Boundaries>1)
View(map_sum)

 for(i in 1:2){
   map_sum <- rbind(map_sum,map_sum[59,])
   }
# Jamaica, Cuba, Haiti are few countries in West Indies

# Rename England, West Indies
map_sum[map_sum=='England'] <- "United Kingdom"
map_sum[[59,1]] <- "Haiti"
map_sum[[61,1]] <- "Cuba"
map_sum[[62,1]] <- "Jamaica"
# Hong Kong is also under China
map_sum[map_sum=='Hong Kong'] <- 'China'

# Plot the interactive heat map
Map <-gvisGeoChart(map_sum ,locationvar = 'COUNTRY',sizevar = 'Boundaries',
                  options = list(width=800,height=650,
                                 title='Boundaries from each Country'))

# Map Table
map_table <- map_all %>% group_by(COUNTRY,Type) %>%
  summarise(Boundaries=sum(Boundaries)) %>% filter(Boundaries>1)
View(map_table)
str(map_table)

map_table$Type<- as.character(gsub('BATTING_','',map_table$Type))
map_table$Type<- gsub('_','-',map_table$Type)

# Table showing sum of boundaries for each country and format of game.
Table<-gvisTable(map_table,
          options = list(width=600,height=650))

# Merged Plot
plot(gvisMerge(Map,Table,horizontal = T))


