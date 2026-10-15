data1 <-read.csv("~/Library/CloudStorage/OneDrive-TheUniversityofSydney(Staff)/Research/Swans/Swans 2024/Swans Stiffness 2024/Data_Trimmed/Jack Buller/Jack Buller-Single Leg Hop Test-2024.02.09-13.33.09_4_Left.csv", stringsAsFactors = F)
#remove NAs
newdata <-  na.omit(data1)

#Extract Fz column number 4 and convert character to numeric
Fz2 <-  as.numeric(newdata[,3])

# Create time vector for 500Hz data
time_500Hz <- seq(0, (length(Fz2) - 1) / 500, by = 1 / 500)

# Generate time vector for 1000Hz data
time_1000Hz <- seq(0, max(time_500Hz), by = 1 / 1000)

# Interpolate Fz2 to 1000Hz
Fz2_interp <- spline(time_500Hz, Fz2, xout = time_1000Hz)$y

# Create a data frame for the interpolated data
Fz2_interp <- as.data.frame(Fz2_interp)

Fz2 <-  as.data.frame(Fz2)




## Filter Design: 4th-order low-pass Butterworth filter with 20 Hz cutoff frequency
## Assuming your sampling rate is 500 Hz
#sampling_rate <- 500
#cutoff_frequency <- 15
#nyquist_frequency <- sampling_rate / 2
#butter_filter <- butter(4, cutoff_frequency / nyquist_frequency, type = "low")
## 
## # Apply the filter
#Fz2$Fz2 <- filtfilt(butter_filter, Fz2$Fz2)


#manual BW and mass
BW = 84*9.8
Mass = 84



# Detect initial flight (force below 10N)
Flight <- which(Fz2$Fz2 < 5)[1]

# Define window size to confirm true contact
min_frames_rising <- 20  # Ensures sustained contact signal for 20 frames

# Identify the first contact instance after flight
Contact <- Flight + which(
  sapply(Flight + 1:(length(Fz2$Fz2) - Flight), function(i) {
    segment <- Fz2$Fz2[i:(i + min_frames_rising - 1)]
    all(segment > 10)  # All frames in the window must exceed threshold
  })
)[1]

# Preallocate for efficiency
Contact <- numeric(12)
Flight <- numeric(12)

# Initialize the first flight
Flight[1] <- which(Fz2$Fz2 < 5)[1]

# Loop to detect and list all contact and flight instances
for (r in 1:12) {
  t <- r + 1
  
  # Detect contact: All frames in the window must stay above 10N
  Contact[r] <- Flight[r] + which(
    sapply(Flight[r] + 1:(length(Fz2$Fz2) - Flight[r]), function(i) {
      # Print the iteration and the segment being evaluated
      segment <- Fz2$Fz2[i:(i + min_frames_rising - 1)]
      all(segment > 10)  # Check if all frames in the window are above 10N
    })
  )[1]
  
  # Detect flight: Force drops below 10 N
  Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 5)[1]
}

time_500Hz <- seq(0, (nrow(Fz2) - 1) * 0.002, by = 0.002)
time_1000Hz <- seq(0, (nrow(Fz2_interp) - 1) * 0.001, by = 0.001)

# Combine into one data frame for ggplot
plot_data_500Hz <- data.frame(Time = time_500Hz, Force = Fz2$Fz2)
plot_data_1000Hz <- data.frame(Time = time_1000Hz, Force = Fz2_interp$Fz2_interp)

# Plot both datasets
p <- ggplot() +
  geom_line(data = plot_data_1000Hz, aes(x = Time, y = Force), 
            color = "red") +
  geom_line(data = plot_data_500Hz, aes(x = Time, y = Force), 
            color = "blue", linetype = "dashed") +

  
  # Add vertical lines for flight and contact points
  geom_vline(xintercept = Flight * 0.001, col = "green") +   # Adjusted for 1000Hz scale
  geom_vline(xintercept = Contact * 0.001, col = "red") +    # Adjusted for 1000Hz scale
  
  labs(title = "Force Plate Data - 500Hz vs 1000Hz", 
       x = "Time (seconds)", 
       y = "Force (N)") +
  theme_minimal()  

ggplotly(p)

    