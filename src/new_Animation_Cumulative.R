library(ggplot2)
library(animation)

# set animation parameters
ani.options(convert = 'D:\\Program Files\\ImageMagick-6.9.1-Q16\\convert.exe', ani.width = 900, ani.height = 600)

draw <- function(i, end, crit, new_data, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label){
  final <- new_data[new_data[,crit]<=i,]
  if(crit == "date"){
  pic <- ggplot(final, aes(group=treatment, color = treatment)) + 
  geom_point(aes(x=date, y = y2), size=point_size) + 
  ggtitle(main) + 
  geom_line(aes(x = date, y =y), size=line_width) + ylab(ylab) + xlab(xlab) +
  scale_x_continuous(limits=c(0, end)) + 
  scale_y_continuous(limits=c(0, 1)) + 
  theme(text = element_text(size=20)) +
  scale_colour_manual(name=legend_name, values=color, labels=legend_label)  
  }
  else if(crit == "sequence"){
  pic <- ggplot(final, aes(group=treatment, color = treatment)) + 
  geom_point(aes(x=sequence, y = y2), size=point_size) + 
  ggtitle(main) + 
  geom_line(aes(x = sequence, y =y), size=line_width) + ylab(ylab) + xlab(xlab) +
  scale_x_continuous(limits=c(0, end)) + 
  scale_y_continuous(limits=c(0, 1)) + 
  theme(text = element_text(size=20)) +
  scale_colour_manual(name=legend_name, values=color, labels=legend_label)  
  }
  print(pic)
}


gif_plot<- function(start, end, crit, new_data, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label){
   lapply(seq(start, end, 1),function(i){
         draw(i, end, crit, new_data, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label)}
         )
}



gif_init <- function(data_in, start = 1, end = 0, interval = 30, crit = "date", speed = 0.5, output, main, xlab, ylab, line_width = 2, point_size = 2, color, legend_name="Treatment", legend_label){
     num <- ceiling(data_in[,crit][nrow(data_in)]/interval)
     if(end == 0){
       end <- num 
     }
     x <- rep(c(1:num), 2)
     y <- rep(NA, 2*num)
     y2 <- rep(NA, 2*num)
     treatment <- c(rep("A",num), rep("B",num))
     for(i in 1:num){
       y[i] <- ifelse(nrow(data_in[data_in[,crit] <= i*interval&data_in$treatment == "A",])>0,
              nrow(data_in[data_in[,crit] <= i*interval&data_in$treatment == "A"&data_in$outcome==1,])/
              nrow(data_in[data_in[,crit] <= i*interval&data_in$treatment == "A",]),NA)
       y[i+num] <- ifelse(nrow(data_in[data_in[,crit] <= i*interval&data_in$treatment == "B",])>0,
              nrow(data_in[data_in[,crit] <= i*interval&data_in$treatment == "B"&data_in$outcome==1,])/
              nrow(data_in[data_in[,crit] <= i*interval&data_in$treatment == "B",]),NA)
       y2[i] <- ifelse(nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)& data_in$treatment == "A",])>0,
              nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)&data_in$treatment == "A"&data_in$outcome==1,])/
              nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)&data_in$treatment == "A",]),NA)
       y2[i+num] <- ifelse(nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)&data_in$treatment == "B",])>0,
              nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)&data_in$treatment == "B"&data_in$outcome==1,])/
              nrow(data_in[data_in[,crit] <= i*interval&data_in[,crit] > ifelse(i==1,-1,(i-1)*interval)&data_in$treatment == "B",]),NA)


     } 
     new_data_in <- data.frame(x,y,treatment,y2)
     print(new_data_in)
     colnames(new_data_in) <- c(crit,"y","treatment","y2")
     saveGIF(gif_plot(start, end = end, crit, new_data_in, main, xlab, ylab, line_width, point_size, color, legend_name, legend_label), interval=speed, movie.name=output)
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
# gif_init(data_in, start = 1, end = 0, interval = 1, crit = "date", speed = 0.5, output, main, xlab, ylab, line_width = 2, point_size = 2, color, legend_name="Treatment", legend_label)
gif_init(data_in = data_in, interval = 30, start = 1, output="D:/cum.gif", main = "ALIAS Cumulative Stroke Trial", xlab="Month", ylab="Cumulative Response Rate", line_width=2, point_size=4, color=c("red","blue"), legend_label = c("Arm1", "Arm2"))
