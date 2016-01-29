library(ggplot2)
library(animation)

# set animation parameters
ani.options(convert = 'D:\\Program Files\\ImageMagick-6.9.1-Q16\\convert.exe', ani.width = 900, ani.height = 600)

get_fit.logit <- function(sample, crit){
  model <- glm(outcome~sample[,crit],family = binomial(link="logit"), data = sample)
  sample$fit <- predict(model, data.frame(sample[,crit]), type="response")
  return(sample)
}

get_fit.linear <- function(sample, crit){
  model <- lm(y~sample[,crit], data = sample)
  sample$fit <- predict(model, data.frame(sample[,crit]))
  return(sample)
}

draw.logit <- function(i, end, crit, new_data, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label){
  sample1 <- get_fit.logit(new_data[new_data[,crit]<=i&new_data$treatment=="A",], crit)
  sample2 <- get_fit.logit(new_data[new_data[,crit]<=i&new_data$treatment=="B",], crit)
  final <- rbind(sample1, sample2)
  if(crit == "date"){
  pic <- ggplot(final, aes(group=treatment, color = treatment)) + 
  geom_point(aes(x=date, y = outcome), size=point_size)
  ggtitle(main) + 
  geom_line(aes(x = date, y =fit), size=line_width) + ylab(ylab) + xlab(xlab) +
  scale_x_continuous(limits=c(0, end)) + 
  scale_y_continuous(limits=c(0, 1)) + 
  theme(text = element_text(size=20)) +
  scale_colour_manual(name=legend_name, values=color, labels=legend_label)  
  }
  else if(crit == "sequence"){
  pic <- ggplot(final, aes(group=treatment, color = treatment)) + 
  geom_point(aes(x=sequence, y = outcome), size=point_size)
  ggtitle(main) + 
  geom_line(aes(x = sequence, y =fit), size=line_width) + ylab(ylab) + xlab(xlab) +
  scale_x_continuous(limits=c(0, end)) + 
  scale_y_continuous(limits=c(0, 1)) + 
  theme(text = element_text(size=20)) +
  scale_colour_manual(name=legend_name, values=color, labels=legend_label)  
  }
  print(pic)
}

draw.linear <- function(i, end, crit, new_data, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label){
  sample1 <- get_fit.linear(new_data[new_data[,crit]<=i&new_data$treatment=="A",], crit)
  sample2 <- get_fit.linear(new_data[new_data[,crit]<=i&new_data$treatment=="B",], crit)
  final <- rbind(sample1, sample2)
  if(crit == "date"){
  pic <- ggplot(final, aes(group=treatment, color = treatment)) + 
  geom_point(aes(x=date, y = y), size=point_size) + 
  ggtitle(main) + 
  geom_line(aes(x = date, y =fit), size=line_width) + ylab(ylab) + xlab(xlab) +
  scale_x_continuous(limits=c(0, end)) + 
  scale_y_continuous(limits=c(0, 1)) + 
  theme(text = element_text(size=20)) +
  scale_colour_manual(name=legend_name, values=color, labels=legend_label)  
  }
  else if(crit == "sequence"){
  pic <- ggplot(final, aes(group=treatment, color = treatment)) + 
  geom_point(aes(x=sequence, y = y), size=point_size) + 
  ggtitle(main) + 
  geom_line(aes(x = sequence, y =fit), size=line_width) + ylab(ylab) + xlab(xlab) +
  scale_x_continuous(limits=c(0, end)) + 
  scale_y_continuous(limits=c(0, 1)) + 
  theme(text = element_text(size=20)) +
  scale_colour_manual(name=legend_name, values=color, labels=legend_label)  
  }
  print(pic)
}


gif_plot.logit <- function(start, end, crit, new_data, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label){
  lapply(seq(start, end, 1),function(i){
         draw.logit(i, end, crit, new_data, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label)}
         )
}

gif_plot.linear <- function(start, end, crit, new_data, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label){
   lapply(seq(start, end, 1),function(i){
         draw.linear(i, end, crit, new_data, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label)}
         )
}



gif_init <- function(data_in, start = 1, end = 0, interval = 30, crit = "date", speed = 0.5, output, main, xlab, ylab, line_width = 2, point_size = 2, color, legend_name="Treatment", legend_label){
   if(interval == 1){
     if(end == 0){
       end <- data_in[,crit][nrow(data_in)]
     }
     saveGIF(gif_plot.logit(start, end = end, crit, data_in, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label), interval=speed, movie.name=output)
   }
   else if(interval > 1){
     num <- ceiling(data_in[,crit][nrow(data_in)]/interval)
     if(end == 0){
       end <- num 
     }
     x <- rep(c(1:num), 2)
     y <- rep(NA, 2*num)
     treatment <- c(rep("A",num), rep("B",num))
     for(i in 1:num){
       y[i] <- ifelse(nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)& data_in$treatment == "A",])>0,
              nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)&data_in$treatment == "A"&data_in$outcome==1,])/
              nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)&data_in$treatment == "A",]),NA)
       y[i+num] <- ifelse(nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)&data_in$treatment == "B",])>0,
              nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)&data_in$treatment == "B"&data_in$outcome==1,])/
              nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)&data_in$treatment == "B",]),NA)
     }
     new_data_in <- data.frame(x,y,treatment)
     colnames(new_data_in) <- c(crit,"y","treatment")
     saveGIF(gif_plot.linear(start, end = end, crit, new_data_in, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label), interval=speed, movie.name=output)
   }
}


# read in dataset
data_in <- read.csv("D:/ALIAS2Raw.csv", header= TRUE)
#####################################################
#####################################################
Options
data_in: input data
start:start point
end: end point
interval: interval, default = 30
crit: date or sequence, default = "date"
speed: speed of the animaton, default = 0.5s
output: output file
main: title of the plot
xlab: x label
ylab: y label
line_width: width of the plot line, default = 2
point_size: size of the point, default = 2
color: vector, the color of the line
legend_name: the title of the legend
legend_label: the labels of the legend
####################################################
####################################################


gif_init(data_in = data_in, interval = 30, start = 1, output="D:/trace.gif", main = "ALIAS Monthly Response Rate", xlab="Month", ylab="Response Rate", line_width=2, point_size=4, color=c("red","blue"), legend_label = c("Arm1", "Arm2"))
