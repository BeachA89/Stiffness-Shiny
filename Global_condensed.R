# Define the analysis function
analyze_hop_data <- function(file_path, mass) {
  data1 <- read.csv(file_path, stringsAsFactors = FALSE, skip = 17)
  
  # Remove NAs
  newdata <- na.omit(data1)
  
  # Extract Fz column number 4 and convert character to numeric
  Fz2 <- as.numeric(newdata[, 4])
  
  # Make that its own data frame
  Fz2 <- as.data.frame(Fz2)
  
  # Calculate BW from Mass
  BW <- mass * 9.8
  
  # Define first flight instance
  Flight <- which(Fz2 < 15)[1]
  # Define first contact instance after first flight instance
  Contact <- Flight[1] - 1 + 5 + which(Fz2$Fz2[Flight[1] + 5:length(Fz2$Fz2)] > 15)[1]
  
  # List first Flight instance
  Flight[1] <- which(Fz2$Fz2 < 15)[1]
  
  # Loop to detect and list all contact and flight instances after first flight
  for (x in 1:20) {
    t <- x + 1
    Contact[x] <- Flight[x] - 1 + 5 + which(Fz2$Fz2[Flight[x] + 5:length(Fz2$Fz2)] > 15)
    if (length(Contact[x] - 1 + 5 + which(Fz2$Fz2[Contact[x] + 5:length(Fz2$Fz2)] < 15)) == 0) {
      break
    } else {
      Flight[t] <- Contact[x] - 1 + 5 + which(Fz2$Fz2[Contact[x] + 5:length(Fz2$Fz2)] < 15)
    }
  }
  
  # Number of hops
  r <- x - 1
  
  output$projectplotLeft1.7 <- renderPlot({
    df = Fz2$Fz2
    ggplot(Fz2, aes(x=1:length(Fz2), y=Fz2)) +
      geom_line() +
      geom_vline(xintercept=Flight[1], col="green") +
      geom_vline(xintercept=Contact[1], col="red") +
      
      geom_vline(xintercept=Flight[2], col="green") +
      geom_vline(xintercept=Contact[2], col="red") +
      
      geom_vline(xintercept=Flight[3], col="green") +
      geom_vline(xintercept=Contact[3], col="red") +
      
      geom_vline(xintercept=Flight[4], col="green") +
      geom_vline(xintercept=Contact[4], col="red") +
      
      geom_vline(xintercept=Flight[5], col="green") +
      geom_vline(xintercept=Contact[5], col="red") +
      
      geom_vline(xintercept=Flight[6], col="green") +
      geom_vline(xintercept=Contact[6], col="red") +
      
      geom_vline(xintercept=Flight[7], col="green") +
      geom_vline(xintercept=Contact[7], col="red") +
      
      geom_vline(xintercept=Flight[8], col="green") +
      geom_vline(xintercept=Contact[8], col="red") +
      
      geom_vline(xintercept=Flight[9], col="green") +
      geom_vline(xintercept=Contact[9], col="red") +
      
      geom_vline(xintercept=Flight[10], col="green") +
      geom_vline(xintercept=Contact[10], col="red") +
      
      geom_vline(xintercept=Flight[11], col="green") +
      geom_vline(xintercept=Contact[11], col="red") +
      
      geom_vline(xintercept=Flight[12], col="green") +
      geom_vline(xintercept=Contact[12], col="red") +
      
      geom_vline(xintercept=Flight[13], col="green") +
      geom_vline(xintercept=Contact[13], col="red") +
      
      geom_vline(xintercept=Flight[14], col="green") +
      geom_vline(xintercept=Contact[14], col="red") +
      
      geom_vline(xintercept=Flight[15], col="green") +
      geom_vline(xintercept=Contact[15], col="red") +
      
      geom_vline(xintercept=Flight[16], col="green") +
      geom_vline(xintercept=Contact[16], col="red") +
      
      geom_vline(xintercept=Flight[17], col="green") +
      geom_vline(xintercept=Contact[17], col="red") +
      
      geom_vline(xintercept=Flight[18], col="green") +
      geom_vline(xintercept=Contact[18], col="red") +
      
      geom_vline(xintercept=Flight[19], col="green") +
      geom_vline(xintercept=Contact[19], col="red") +
      
      geom_vline(xintercept=Flight[20], col="green") +
      geom_vline(xintercept=Contact[20], col="red")
  })
  
  # Split the full data into FullHops from contact to contact
  FullHops <- list()
  for (q in 1:r) {
    t <- q + 1
    FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
  }
  
  # Split the data into Hops from flight to flight
  Hops <- list()
  for (q in 1:r) {
    t <- q + 1
    Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
  }
  
  # Initialize variables for calculations
  PeakHop <- c()
  PeakHopBW <- c()
  ContactTimeHop <- c()
  FlightTimeHop <- c()
  MinHopDisp <- c()
  EccHopDisp <- c()
  EccStiffHop <- c()
  JumpFreqHop <- c()
  
  # Perform calculations for each hop
  for (w in 1:q) {
    Hop <- Hops[[w]]
    HopAcc <- (Hop - BW) / mass
    
    # Create a rolling average of Acc
    HopAccavg <- rollapply(HopAcc, 2, mean)
    HopAccavg2 <- insert(HopAccavg, 1, 0)
    
    # Calculate Acceleration Area
    HopAccarea <- HopAccavg2 * 0.002
    
    # Initial velocity
    HopVelInit <- 1.3
    
    # Calculate velocity
    HopVela <- HopAccarea
    HopVela[1] <- HopVelInit + HopAccarea[1]
    HopVel <- cumsum(HopVela)
    HopVelavg <- rollapply(HopVel, 2, mean)
    HopVelavg2 <- insert(HopVelavg, 1, 0)
    HopVelarea <- HopVelavg2 * 0.002
    
    # Calculate displacement
    HopDisp <- cumsum(HopVelarea)
    l <- length(HopDisp)
    
    # Adjust initial velocity
    if (HopDisp[l] > 0) {
      while (HopDisp[l] > 0.0001) {
        HopAccavg <- rollapply(HopAcc, 2, mean)
        HopAccavg2 <- insert(HopAccavg, 1, 0)
        HopAccarea <- HopAccavg2 * 0.002
        HopVelInit <- HopVelInit - 0.002
        HopVela <- HopAccarea
        HopVela[1] <- HopVelInit + HopAccarea[1]
        HopVel <- cumsum(HopVela)
        HopVelavg <- rollapply(HopVel, 2, mean)
        HopVelavg2 <- insert(HopVelavg, 1, 0)
        HopVelarea <- HopVelavg2 * 0.002
        HopDisp <- cumsum(HopVelarea)
      }
    } else {
      while (HopDisp[l] < 0.0001) {
        HopAccavg <- rollapply(HopAcc, 2, mean)
        HopAccavg2 <- insert(HopAccavg, 1, 0)
        HopAccarea <- HopAccavg2 * 0.002
        HopVelInit <- HopVelInit + 0.002
        HopVela <- HopAccarea
        HopVela[1] <- HopVelInit + HopAccarea[1]
        HopVel <- cumsum(HopVela)
        HopVelavg <- rollapply(HopVel, 2, mean)
        HopVelavg2 <- insert(HopVelavg, 1, 0)
        HopVelarea <- HopVelavg2 * 0.002
        HopDisp <- cumsum(HopVelarea)
      }
    }
    
    # Calculate output variables
    ContactFrame <- 5 + which(Hop[5:length(Hop)] > 10)[1]
    PeakHop[w] <- max(Hop)
    PeakHopBW[w] <- max(Hop) / BW
    ContactTimeHop[w] <- (length(HopDisp) - ContactFrame) * 0.002
    FlightTimeHop[w] <- (length(HopDisp) - (length(HopDisp) - ContactFrame)) * 0.002
    MinHopDisp[w] <- min(HopDisp)
    EccHopDisp[w] <- (HopDisp[ContactFrame] - min(HopDisp))
    EccStiffHop[w] <- ((max(Hop) - BW) / (HopDisp[ContactFrame] - min(HopDisp))) / mass
    JumpFreqHop[w] <- 1 / (length(FullHops[[w]]) * 0.002)
  }
  
  # Calculate average of middle 5 hops as the "most reliable"
  avgEccCOMDisp <- round(mean(EccHopDisp[4:9]), digits = 2)
  avgContactTime <- round(mean(ContactTimeHop[4:9]), digits = 2)
  avgFlightTime <- round(mean(FlightTimeHop[4:9]), digits = 2)
  avgEccStiffness <- round(mean(EccStiffHop[4:9]), digits = 2)
  avgPeakForceBW <- round(mean(PeakHopBW[4:9]), digits = 2)
  avgJumpFreq <- round(mean(JumpFreqHop[4:9]), digits = 2)
  
  # Convert output variables to data.frame and round each value to 2 dp
  PeakHop_df <- data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits = 2)
  PeakHopBW_df <- data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits = 2)
  ContactTimeHop_df <- data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits = 2)
  FlightTimeHop_df <- data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits = 2)
  MinHopDisp_df <- data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits = 2)
  EccHopDisp_df <- data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits = 2)
  EccStiffHop_df <- data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits = 2)
  JumpFreqHop_df <- data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits = 2)
  
  # Create a variable listing the hop numbers
  hopnumber <- c()
  for (n in 1:q) {
    hopnumber[n] <- paste0("Hop", n, sep = "")
  }
  hopnumber[q + 1] <- "Average"
  hopnumber <- data.frame(hopnumber)
  
  # Combine the data for each hop as rows and the average as the final row
  EccCOMDisp2 <- rbind(EccHopDisp_df, avgEccCOMDisp)
  ContactTime2 <- rbind(ContactTimeHop_df, avgContactTime)
  FlightTime2 <- rbind(FlightTimeHop_df, avgFlightTime)
  EccStiffness2 <- rbind(EccStiffHop_df, avgEccStiffness)
  PeakForce2 <- rbind(PeakHopBW_df, avgPeakForceBW)
  JumpFreq2 <- rbind(JumpFreqHop_df, avgJumpFreq)
  
  # Combine into a single data frame
  FullResults <- cbind(hopnumber, ContactTime2, FlightTime2, EccCOMDisp2, EccStiffness2, PeakForce2, JumpFreq2)
  colnames(FullResults) <- c("Hop #", "Contact Time (s)", "Flight Time (s)", "Eccentric Displacement (m)", "Eccentric Stiffness (N/m/kg)", "Peak Force (BW)", "Jump Frequency (Hz)")
  
  # Return the results
  return(FullResults)
}