dataframe <- read.csv(file.choose())
head(dataframe)
df2 <- dataframe[,-21]#removed housing
head(df2)
df3 <- df2[complete.cases(df2), ]#removed NA so data of 2019,2020 gone
df3orig <- df3 #all datapts including april
df3
df3 <- df3[df3$Month == "April" , ]#since beginning of new financial year
df3FG<- df3[,c(1,2,3,16,17,20,21,28)]
df3FGU <- df3FG[df3FG$Sector == "Urban",]#urban data of the FG
df3FGR <- df3FG[df3FG$Sector == "Rural",]#rural data of FG
df3FG #this includes 5 major weight groups
df3FGU
df3FGR
df3NFG <- df3[,c(-16,-17,-20,-21,-28)] #this is w/o 5 major grps
df3NFGU <- df3NFG[df3NFG$Sector == "Urban",]#urban data of the NFG
df3NFGR <- df3NFG[df3NFG$Sector == "Rural",]#rural data of NFG
df3NFG #this includes 5 major weight groups
df3NFGU
df3NFGR

df3FB <- df3[,c(1:15)] #grp1
df3FBU <- df3FB[df3FB$Sector == "Urban",]#urban data of the group1
df3FBR <- df3FB[df3FB$Sector == "Rural",]#rural data of group 1
head(df3FB)
head(df3FBU)#head so showing 5 years but all years present except NA ones
head(df3FBR)
df3PTI <- df3[,c(1,2,3,17)] #grp2
df3PTIU <- df3PTI[df3PTI$Sector == "Urban",]#urban data of the group2
df3PTIR <- df3PTI[df3PTI$Sector == "Rural",]#rural data of group 2
df3PTI
df3PTIU
df3PTIR
df3CF <- df3[,c(1,2,3,18,19)] #grp3
df3CFU <- df3CF[df3CF$Sector == "Urban",]#urban data of the group3
df3CFR <- df3CF[df3CF$Sector == "Rural",]#rural data of group 3
df3CF
df3CFU
df3CFR

df3FL <- df3[,c(1,2,3,21)] #grp4
df3FLU <- df3CF[df3FL$Sector == "Urban",]#urban data of the group4
df3FLR <- df3CF[df3FL$Sector == "Rural",]#rural data of group 4
df3FL
df3FLU
df3FLR
df3M <- df3[,c(1,2,3,22:27)] #grp5
df3MU <- df3M[df3M$Sector == "Urban",]#urban data of the group5
df3MR <- df3M[df3M$Sector == "Rural",]#rural data of group 5
head(df3M)
df3MU
df3MR
# df3FG,df3NFG,df3FB,df3PTI,df3CF,df3FL,df3M

#install.packages("ggplot2")
library(ggplot2)
#starting with first group df3FB
head(df3FB)
#df3FB$MY <- Map(c, df3FB$Month, df3FB$Year) no need now since only april
head(df3FB)
df3FB <- as.data.frame(lapply(df3FB, unlist))
#plot1G1 between Cereals.and.products and year
ggplot(data=df3FB, aes(x=Year, y=Cereals.and.products, group=Sector, color = Sector )) +
  geom_line(size=1)+
  geom_point(size=4)+
  scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
  scale_y_continuous(breaks = round(seq(min(df3FB$Cereals.and.products), max(df3FB$Cereals.and.products), by = 10),1))
geom_label(aes(label = Cereals.and.products),
           nudge_x = 0.25,
           nudge_y = 0.25,
           size = 2)+
  labs(
    caption = "Source: data.gov.in"
  ) +
  theme(
    plot.caption = element_text(face = "italic", hjust = 0)
  )
#plot1G1heatmap between Cereals.and.productsR and Cereals.and.productsU
library(corrplot)
cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
corrplot::corrplot(cor_mat,method = "number")

#plot2G1 between Meat.and.fish and year
ggplot(data=df3FB, aes(x=Year, y=Meat.and.fish, group=Sector, color = Sector )) +
  geom_line(size=1)+
  geom_point(size=4)+
  scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
  scale_y_continuous(breaks = round(seq(min(df3FB$Meat.and.fish), max(df3FB$Meat.and.fish), by = 10),1))
geom_label(aes(label = Meat.and.fish),
           nudge_x = 0.25,
           nudge_y = 0.25,
           size = 2)+
  labs(
    caption = "Source: data.gov.in"
  ) +
  theme(
    plot.caption = element_text(face = "italic", hjust = 0)
  )
#plot2G1heatmap between Meat.and.fishR and Meat.and.fishsU
library(corrplot)
cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
corrplot::corrplot(cor_mat,method = "color")

#plot3G1 between egg and year
ggplot(data=df3FB, aes(x=Year, y=Egg, group=Sector, color = Sector )) +
  geom_line(size=1)+
  geom_point(size=4)+
  scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
  scale_y_continuous(breaks = round(seq(min(df3FB$Egg), max(df3FB$Egg), by = 10),1))
    geom_label(aes(label = Egg),
            nudge_x = 0.25,
            nudge_y = 0.25,
            size = 2)+
  labs(
    caption = "Source: data.gov.in"
  ) +
  theme(
    plot.caption = element_text(face = "italic", hjust = 0)
  )

#plot3G1heatmap between eggR and egg U
    cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat)
    
    #plot4G1 between Milk.and.products and year
    ggplot(data=df3FB, aes(x=Year, y=Milk.and.products, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FB$Milk.and.products), max(df3FB$Milk.and.products), by = 10),1))
    geom_label(aes(label = Milk.and.products),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot4G1heatmap between Milk.and.productsR and Milk.and.productsU
    cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method="shade",diag=FALSE)

    
    #plot5G1 between Oils.and.fats and year
    ggplot(data=df3FB, aes(x=Year, y=Oils.and.fats, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FB$Oils.and.fats), max(df3FB$Oils.and.fats), by = 10),1))
    geom_label(aes(label = Oils.and.fats),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot5G1heatmap between Oils.and.fatsR and Oils.and.fatsU
    cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method = 'square', order = 'FPC', diag = FALSE)
    
    #plot6G1 between Fruits and year
    ggplot(data=df3FB, aes(x=Year, y=Fruits, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FB$Fruits), max(df3FB$Fruits), by = 10),1))
    geom_label(aes(label = Fruits),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot6G1heatmap between FruitsR and FruitsU
    cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method = 'ellipse')
    
    #plot7G1 between Vegetables and year
    ggplot(data=df3FB, aes(x=Year, y=Vegetables, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FB$Vegetables), max(df3FB$Vegetables), by = 10),1))
    geom_label(aes(label = Vegetables),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot7G1heatmap between VegetablesR and VegetablesU
    cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
    corrplot::corrplot.mixed(cor_mat)

    #plot8G1 between Pulses.and.products and year
    ggplot(data=df3FB, aes(x=Year, y=Pulses.and.products, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FB$Pulses.and.products), max(df3FB$Pulses.and.products), by = 10),1))
    geom_label(aes(label = Pulses.and.products),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot8G1heatmap between Pulses.and.productsR and pulsesandproductsU
    cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
    corrplot::corrplot.mixed(cor_mat,lower = 'shade', upper = 'pie', order = 'hclust')

    #plot9G1 between Sugar.and.Confectionery and year
    ggplot(data=df3FB, aes(x=Year, y=Sugar.and.Confectionery, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FB$Sugar.and.Confectionery), max(df3FB$Sugar.and.Confectionery), by = 10),1))
    geom_label(aes(label = Sugar.and.Confectionery),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot9G1heatmap between Sugar.and.ConfectioneryR and Sugar.and.ConfectioneryU
    cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,order = 'hclust', addrect = 2)
    
    #plot10G1 between Spices and year
    ggplot(data=df3FB, aes(x=Year, y=Spices, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FB$Spices), max(df3FB$Spices), by = 10),1))
    geom_label(aes(label = Spices),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot10G1heatmap between SpicesR and SpicesU
    cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method = 'square', diag = FALSE, order = 'hclust',
                       addrect = 3, rect.col = 'blue', rect.lwd = 3, tl.pos = 'd')

    #plot11G1 between Non.alcoholic.beverages and year
    ggplot(data=df3FB, aes(x=Year, y=Non.alcoholic.beverages, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FB$Year), max(df3FB$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FB$Non.alcoholic.beverages), max(df3FB$Non.alcoholic.beverages), by = 10),1))
    geom_label(aes(label = Non.alcoholic.beverages),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot11G1heatmap between Non.alcoholic.beveragesR and Non.alcoholic.beveragesU
    COL1(sequential = c("Oranges", "Purples", "Reds", "Blues", "Greens", 
                        "Greys", "OrRd", "YlOrRd", "YlOrBr", "YlGn"), n = 200)
    
    COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)
    
    cor_mat <- cor(df3FBR[4:15],df3FBU[4:15]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat, addCoef.col = 'black', tl.pos = 'd',
                       cl.pos = 'n', col = COL2('PiYG'))
    #plot1G3 between Clothing and year
    ggplot(data=df3CF, aes(x=Year, y=Clothing, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3CF$Year), max(df3CF$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3CF$Clothing), max(df3CF$Clothing), by = 10),1))
    geom_label(aes(label = Clothing),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot1G3heatmap between ClothingR and ClothingU
    
    cor_mat <- cor(df3CFR[4:5],df3CFU[4:5]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method ='number')
    
    #plot2G3 between Footwear and year
    ggplot(data=df3CF, aes(x=Year, y=Footwear, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3CF$Year), max(df3CF$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3CF$Footwear), max(df3CF$Footwear), by = 10),1))
    geom_label(aes(label = Footwear),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot2G3heatmap between FootwearR and FootwearU
    
    cor_mat <- cor(df3CFR[4:5],df3CFU[4:5]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat)

    #plot1G5 between Household.goods.and.services and year
    ggplot(data=df3M, aes(x=Year, y=Household.goods.and.services, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3M$Year), max(df3M$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3M$Household.goods.and.services), max(df3M$Household.goods.and.services), by = 10),1))
    geom_label(aes(label = Household.goods.and.services),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot1G5heatmap between Household.goods.and.servicesR and Household.goods.and.servicesU
    
    cor_mat <- cor(df3MR[4:9],df3MU[4:9]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat)

    #plot2G5 between Health and year
    ggplot(data=df3M, aes(x=Year, y=Health, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3M$Year), max(df3M$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3M$Health), max(df3M$Health), by = 10),1))
    geom_label(aes(label = Health),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot2G5heatmap between HealthR and HealthU
    
    cor_mat <- cor(df3MR[4:9],df3MU[4:9]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method="number")
    #plot3G5 between Transport.and.communication and year
    ggplot(data=df3M, aes(x=Year, y=Transport.and.communication, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3M$Year), max(df3M$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3M$Transport.and.communication), max(df3M$Transport.and.communication), by = 10),1))
    geom_label(aes(label = Transport.and.communication),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot3G5heatmap between Transport.and.communicationR and Transport.and.communicationU
    
    cor_mat <- cor(df3MR[4:9],df3MU[4:9]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method="color")
    #plot4G5 between Recreation.and.amusement and year
    ggplot(data=df3M, aes(x=Year, y=Recreation.and.amusement, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3M$Year), max(df3M$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3M$Recreation.and.amusement), max(df3M$Recreation.and.amusement), by = 10),1))
    geom_label(aes(label = Recreation.and.amusement),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot4G5heatmap between Recreation.and.amusementR and Recreation.and.amusementU
    
    cor_mat <- cor(df3MR[4:9],df3MU[4:9]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method = 'square', order = 'FPC', diag = FALSE)
    
    #plot5G5 between Education and year
    ggplot(data=df3M, aes(x=Year, y=Education, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3M$Year), max(df3M$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3M$Education), max(df3M$Education), by = 10),1))
    geom_label(aes(label = Education),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot5G5heatmap between EducationR and EducationU
    
    cor_mat <- cor(df3MR[4:9],df3MU[4:9]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method = 'ellipse')
    #plot6G5 between Personal.care.and.effects and year
    ggplot(data=df3M, aes(x=Year, y=Personal.care.and.effects, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3M$Year), max(df3M$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3M$Personal.care.and.effects), max(df3M$Personal.care.and.effects), by = 10),1))
    geom_label(aes(label = Personal.care.and.effects),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plot6G5heatmap between Personal.care.and.effectsR and Personal.care.and.effectsU
    
    cor_mat <- cor(df3MR[4:9],df3MU[4:9]) #now change heatmap type for each item
    corrplot::corrplot.mixed(cor_mat,lower = 'shade', upper = 'pie', order = 'hclust')
    
    #plotG1 between Food.and.beverages and year
    ggplot(data=df3FG, aes(x=Year, y=Food.and.beverages, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FG$Year), max(df3FG$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FG$Food.and.beverages), max(df3FG$Food.and.beverages), by = 10),1))
    geom_label(aes(label = Food.and.beverages),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plotG1heatmap between Food.and.beveragesR and Food.and.beveragesU
    
    cor_mat <- cor(df3FGU[4:8],df3FGR[4:8]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method="number") #here
    #plotG2 between Pan..tobacco.and.intoxicants and year
    ggplot(data=df3FG, aes(x=Year, y=Pan..tobacco.and.intoxicants, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FG$Year), max(df3FG$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FG$Pan..tobacco.and.intoxicants), max(df3FG$Pan..tobacco.and.intoxicants), by = 10),1))
    geom_label(aes(label = Pan..tobacco.and.intoxicants),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    
    #plotG2heatmap between Pan..tobacco.and.intoxicantsR and Pan..tobacco.and.intoxicantsU
    
    cor_mat <- cor(df3FGU[4:8],df3FGR[4:8]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method="color") #here only use 2 plots for 5 major groups since need clarity   
    #plotG3 between Clothing.and.footwear and year
    ggplot(data=df3FG, aes(x=Year, y=Clothing.and.footwear, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FG$Year), max(df3FG$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FG$Clothing.and.footwear), max(df3FG$Clothing.and.footwear), by = 10),1))
    geom_label(aes(label = Clothing.and.footwear),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    #plotG4 between Fuel.and.light and year
    ggplot(data=df3FG, aes(x=Year, y=Fuel.and.light, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FG$Year), max(df3FG$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FG$Fuel.and.light), max(df3FG$Fuel.and.light), by = 10),1))
    geom_label(aes(label = Fuel.and.light),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    #plotG5 between Miscellaneous and year
    ggplot(data=df3FG, aes(x=Year, y=Miscellaneous, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3FG$Year), max(df3FG$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3FG$Miscellaneous), max(df3FG$Miscellaneous), by = 10),1))
    geom_label(aes(label = Miscellaneous),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    #plotAllGcommodU 

    matplot(df3FGU[4:8], type = 'l',xlab="Year",ylab = "Value",lty=5, lwd=2)
    legend("top", colnames(df3FGU[4:8]),col=seq_len(5),cex=0.8,fill=seq_len(5))
    
    #plotAllGcommodR 
 
    matplot(df3FGR[4:8], type = 'l',xlab="Year",ylab = "Value",lty=5, lwd=2)
    legend("top", colnames(df3FGR[4:8]),col=seq_len(5),cex=0.8,fill=seq_len(5))
    
    #plotAllNFGcommodU 
    
    matplot(df3NFGU[4:23], type = 'l',xlab="Year",ylab = "Value",lty=5, lwd=2)
    legend("topleft", colnames(df3NFGU[4:23]),col=seq_len(5),cex=0.8,fill=seq_len(5))
    
    #plotAllNFGcommodR 
    
    matplot(df3NFGR[4:23], type = 'l',xlab="Year",ylab = "Value",lty=5, lwd=2)
    legend("topleft", colnames(df3NFGR[4:23]),col=seq_len(5),cex=0.8,fill=seq_len(5))
    
    #correlFGcommodU
    cor_mat <- cor(df3FGU[4:8],df3FGU[4:8]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method="number")
    
    #correlFGcommodR
    cor_mat <- cor(df3FGR[4:8],df3FGR[4:8]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method="number")
    
    #correlNFGcommodU
    cor_mat <- cor(df3NFGU[4:23],df3NFGU[4:23]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method="color")
    #correlNFGcommodR
    cor_mat <- cor(df3NFGR[4:23],df3NFGR[4:23]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method="color")
    
    #general index plot across sectors
    ggplot(data=df3NFG, aes(x=Year, y=General.index, group=Sector, color = Sector )) +
      geom_line(size=1)+
      geom_point(size=4)+
      scale_x_continuous(breaks = round(seq(min(df3NFG$Year), max(df3NFG$Year), by = 1),1))+
      scale_y_continuous(breaks = round(seq(min(df3NFG$General.index), max(df3NFG$General.index), by = 10),1))
    geom_label(aes(label = General.index),
               nudge_x = 0.25,
               nudge_y = 0.25,
               size = 2)+
      labs(
        caption = "Source: data.gov.in"
      ) +
      theme(
        plot.caption = element_text(face = "italic", hjust = 0)
      )
    #correlations between general index r and u
    cor_mat <- cor(df3NFGU[24],df3NFGR[24]) #now change heatmap type for each item
    corrplot::corrplot(cor_mat,method="number")#useless dont show
    