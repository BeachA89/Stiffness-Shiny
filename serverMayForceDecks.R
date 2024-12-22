#Currently server500Hz
# changed freq to 0.002 from 0.001
# changed flight threshold to 10N

# Define server logic to read selected file ----
server <- function(input, output) {
  values <- reactiveValues()
  output$Summaryhead <- renderText({
    "Averages"
  })
  # everything wrapped into this output for Left 1.7Hz trial, each output is duplicated for each file, gobutton will process all at once
  output$resultstableLeft1 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left1$datapath, stringsAsFactors = F))
    
    #remove NAs
    newdata <-  na.omit(data1)
    
    #Extract Fz column number 4 and convert character to numeric
    Fz2 <-  as.numeric(newdata[,3])
    
    #make that its own data frame
    Fz2 <-  as.data.frame(Fz2)
    
    #manual BW and mass
    #BW = 85*9.8
    #Mass = 85
    
    #Calculate BW from Mass that is entered on the Shiny - should give an error if no Mass is entered
    BW = (input$Mass)*9.8
    
    
    # Find the first flight instance
    Flight <- which(Fz2$Fz2 < 5)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 3  # Number of frames that force must rise consecutively to confirm contact
    
    Contact <- Flight + which(
      sapply(Flight + 1:(length(Fz2$Fz2) - Flight), function(i) {
        segment <- Fz2$Fz2[i:(i + min_frames_rising - 1)]
        all(segment > 20) && all(diff(segment) > 0)
      })
    )[1]
    
    # Loop to detect and list all contact and flight instances
    Contact <- numeric(12)  # Preallocate for efficiency
    Flight <- numeric(12)   # Preallocate for efficiency
    
    # Initialize the first flight
    Flight[1] <- which(Fz2$Fz2 < 5)[1]
    
    for (r in 1:12) {
      t <- r + 1
      # Detect contact: Force > 20 N, rising for min_frames_rising consecutive frames
      Contact[r] <- Flight[r] + which(
        sapply(Flight[r] + 1:(length(Fz2$Fz2) - Flight[r]), function(i) {
          segment <- Fz2$Fz2[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
      # Detect flight: Force drops below 5 N
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 5)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotLeft1 <- renderPlotly({
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
        geom_vline(xintercept=Contact[12], col="red") #+
      
      #geom_vline(xintercept=Flight[13], col="green") +
      #geom_vline(xintercept=Contact[13], col="red") +
      
      #geom_vline(xintercept=Flight[14], col="green") +
      #geom_vline(xintercept=Contact[14], col="red") +
      
      #geom_vline(xintercept=Flight[15], col="green") +
      #geom_vline(xintercept=Contact[15], col="red") +
      
      #geom_vline(xintercept=Flight[16], col="green") +
      #geom_vline(xintercept=Contact[16], col="red")
      
      #geom_vline(xintercept=Flight[17], col="green") +
      #geom_vline(xintercept=Contact[17], col="red")
      #geom_vline(xintercept=Flight[18], col="green") +
      # geom_vline(xintercept=Contact[18], col="red")
      #geom_vline(xintercept=Flight[19], col="green") +
      # geom_vline(xintercept=Contact[19], col="red")
      #geom_vline(xintercept=Flight[20], col="green") +
      # geom_vline(xintercept=Contact[20], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    #Split the full data into FullHops which is from contact to contact, using the final r value from the 1:20 loop above, which should be the last contact instance - subtract 1 to give the number of complete hops
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
    }
    
    #Split the data into Hops from flight to flight
    Hops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
    }
    
    #Define blank variables for the loops below
    PeakHop = c()
    PeakHopBW = c()
    ContactTimeHop = c()
    FlightTimeHop = c()
    MinHopDisp = c()
    EccHopDisp = c()
    EccStiffHop = c()
    JumpFreqHop = c()
    
    
    # the final q value after the loop is now the number of full hops, so it can be reused below
    
    for (w in 1:q){
      
      #Hop1
      
      #Calculate Acceleration using F=m*a
      
      Hop <-  Hops[[w]]
      #HopAcc <- (Hop-BW)/input$Mass
      HopAcc <- (Hop-BW)/input$Mass
      
      
      #Create a rolling average of Acc, 2 frame window (why?)
      HopAccavg = rollapply(HopAcc, 2, mean)
      #insert 0 at the first row
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Calculate Acceleration Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopAccarea = HopAccavg2*0.001
      
      #HopVelInit is a random starting velocity to apply the acceleration to, and will be adjusted later 
      HopVelInit = 1.3
      
      #Relabel HopAccarea as HopVela  add HopVelInit
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      
      #cumulative sum of the acceleration will give the velocity at each point
      HopVel = cumsum(HopVela)
      
      #Create a rolling average of Vel, 2 frame window (why?)
      HopVelavg = rollapply(HopVel, 2, mean)
      #insert 0 at the first row
      HopVelavg2 = insert(HopVelavg, 1, 0)
      #Calculate Velocity Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopVelarea = HopVelavg2*0.001
      
      #Caluclate Displacement as the cumsum of Velocity Area
      HopDisp = cumsum(HopVelarea)
      
      #Define length of the Displacement data
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacement close to zero e.g. if Final displacement (l) is >0.0001, repeat the calculations above with HopVelInit -0.002, and continue until Final displacement is within 0.0001 of zero.
      #Conversely, do the opposite if Final displacement is <0.0001
      if (HopDisp[l]>0){
        while (HopDisp[l] > 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.001
          HopVelInit = HopVelInit - 0.001
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.001
          HopDisp = cumsum(HopVelarea)}
      }else (HopDisp[l]<0) 
      {
        while (HopDisp[l] < 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.001
          HopVelInit = HopVelInit + 0.001
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.001
          HopDisp = cumsum(HopVelarea)
        }
      }
      
      
      #Calculate output variables
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
      PeakHop[w] = max(Hop)
      PeakHopBW[w] = max(Hop)/BW
      ContactTimeHop[w] = (length(HopDisp) - ContactFrame)*0.001
      FlightTimeHop[w] = (length(HopDisp) - (length(HopDisp) - ContactFrame))*0.001
      MinHopDisp[w] = min(HopDisp)
      EccHopDisp[w] = (HopDisp[ContactFrame] - min(HopDisp))
      EccStiffHop[w] = ((max(Hop) - BW)/(HopDisp[ContactFrame] - min(HopDisp)))/input$Mass
      JumpFreqHop[w] = 1/(length(FullHops[[w]])*0.001)
      
    }
    
    #Convert Output variables to data.frame and round each value to 2 dp
    PeakHop_df = data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits=2)
    PeakHopBW_df = data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits=2)
    ContactTimeHop_df = data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    FlightTimeHop_df = data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    MinHopDisp_df = data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccHopDisp_df = data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccStiffHop_df = data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits=2)
    JumpFreqHop_df = data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Calculate average of middle 5 hops as the "most reliable"
    avgEccCOMDisp = round(mean(EccHopDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[4:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[4:9]),digits=2)
    
    #create a variable listing the hopnumbers as Hop1, Hop2 etc, looping to q (number of hops from above)
    hopnumber = c()
    for(n in 1:q){
      hopnumber[n] <- paste0("Hop", n, sep = "")
    }
    #add to that a col header "Average"
    hopnumber[q+1] = "Average"
    #convert to data.frame for rbind below
    hopnumber = data.frame(hopnumber)
    
    #Create reactive variable that stays alive for combined final summary table
    values$Left1Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
    #combine the data for each hop as rows and the average as the final row
    EccCOMDisp2 <-  rbind(EccHopDisp_df, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTimeHop_df, avgContactTime)
    FlightTime2 <-  rbind(FlightTimeHop_df, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffHop_df, avgEccStiffness)
    PeakForceBW2 <-  rbind(PeakHopBW_df, avgPeakForceBW)
    JumpFreq2 <-  rbind(JumpFreqHop_df, avgJumpFreq)
    
    #Create output data frame withe the hopnumber variable as the first column and data from above as columns
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForceBW2, JumpFreq2)
    
    #rename column names of output data frame (default is the variable name)
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    
    #define output file for Shiny
    OutputdfLeft1 <-  as.data.frame(Output)
    
    ({OutputdfLeft1})
    
  })
  outputOptions(output, "resultstableLeft1", suspendWhenHidden = FALSE)
  
  
  output$resultstableLeft2.0 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left2.0$datapath, stringsAsFactors = F, skip = 17))
    
    #remove NAs
    newdata <-  na.omit(data1)
    
    #Extract Fz column number 4 and convert character to numeric
    Fz2 <-  as.numeric(newdata[,4])
    
    #make that its own data frame
    Fz2 <-  as.data.frame(Fz2)
    
    #manual BW and mass
    #BW = 85*9.8
    #Mass = 85
    
    #Calculate BW from Mass that is entered on the Shiny - should give an error if no Mass is entered
    BW = (input$Mass)*9.8
    
    #define first flight instance
    Flight <- which(Fz2 <5)[1]
    #define first contact instance after first flight instance
    
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    
    #zero offset using average of values during flight - doesn't really work if there are negative values, and small enough to not worry
    #zerooffset = mean(Fz2$Fz2[Flight:Contact])
    #Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    #list first Flight instance - is this not the same as above??
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    #loop to detect and list all contact and flight instances after first flight - use 10N as the threshold for each - r 1:20 to cover max number of hops, shouldn't cause an error when it gets to the max
    for (r in 1:20){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <10)
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotLeft2.0 <- renderPlot({
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
        geom_vline(xintercept=Contact[16], col="red")
      
      geom_vline(xintercept=Flight[17], col="green") +
        geom_vline(xintercept=Contact[17], col="red")
      geom_vline(xintercept=Flight[18], col="green") +
        geom_vline(xintercept=Contact[18], col="red")
      geom_vline(xintercept=Flight[19], col="green") +
        geom_vline(xintercept=Contact[19], col="red")
      geom_vline(xintercept=Flight[20], col="green") +
        geom_vline(xintercept=Contact[20], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    #Split the full data into FullHops which is from contact to contact, using the final r value from the 1:20 loop above, which should be the last contact instance - subtract 1 to give the number of complete hops
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
    }
    
    #Split the data into Hops from flight to flight
    Hops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
    }
    
    #Define blank variables for the loops below
    PeakHop = c()
    PeakHopBW = c()
    ContactTimeHop = c()
    FlightTimeHop = c()
    MinHopDisp = c()
    EccHopDisp = c()
    EccStiffHop = c()
    JumpFreqHop = c()
    
    
    # the final q value after the loop is now the number of full hops, so it can be reused below
    
    for (w in 1:q){
      
      #Hop1
      
      #Calculate Acceleration using F=m*a
      
      Hop <-  Hops[[w]]
      #HopAcc <- (Hop-BW)/input$Mass
      HopAcc <- (Hop-BW)/input$Mass
      
      
      #Create a rolling average of Acc, 2 frame window (why?)
      HopAccavg = rollapply(HopAcc, 2, mean)
      #insert 0 at the first row
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Calculate Acceleration Area under the curve, using the time variable (500hz = 0.001 seconds)
      HopAccarea = HopAccavg2*0.002
      
      #HopVelInit is a random starting velocity to apply the acceleration to, and will be adjusted later 
      HopVelInit = 1.3
      
      #Relabel HopAccarea as HopVela  add HopVelInit
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      
      #cumulative sum of the acceleration will give the velocity at each point
      HopVel = cumsum(HopVela)
      
      #Create a rolling average of Vel, 2 frame window (why?)
      HopVelavg = rollapply(HopVel, 2, mean)
      #insert 0 at the first row
      HopVelavg2 = insert(HopVelavg, 1, 0)
      #Calculate Velocity Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopVelarea = HopVelavg2*0.002
      
      #Caluclate Displacement as the cumsum of Velocity Area
      HopDisp = cumsum(HopVelarea)
      
      #Define length of the Displacement data
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacement close to zero e.g. if Final displacement (l) is >0.0001, repeat the calculations above with HopVelInit -0.002, and continue until Final displacement is within 0.0001 of zero.
      #Conversely, do the opposite if Final displacement is <0.0001
      if (HopDisp[l]>0){
        while (HopDisp[l] > 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit - 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)}
      }else (HopDisp[l]<0) 
      {
        while (HopDisp[l] < 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit + 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)
        }
      }
      
      
      #Calculate output variables
      
      ContactFrame <- 5 + which(Hop[5:length(Hop)] >10)[1]
      PeakHop[w] = max(Hop)
      PeakHopBW[w] = max(Hop)/BW
      ContactTimeHop[w] = (length(HopDisp) - ContactFrame)*0.002
      FlightTimeHop[w] = (length(HopDisp) - (length(HopDisp) - ContactFrame))*0.002
      MinHopDisp[w] = min(HopDisp)
      EccHopDisp[w] = (HopDisp[ContactFrame] - min(HopDisp))
      EccStiffHop[w] = ((max(Hop) - BW)/(HopDisp[ContactFrame] - min(HopDisp)))/input$Mass
      JumpFreqHop[w] = 1/(length(FullHops[[w]])*0.002)
      
    }
    
    #Convert Output variables to data.frame and round each value to 2 dp
    PeakHop_df = data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits=2)
    PeakHopBW_df = data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits=2)
    ContactTimeHop_df = data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    FlightTimeHop_df = data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    MinHopDisp_df = data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccHopDisp_df = data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccStiffHop_df = data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits=2)
    JumpFreqHop_df = data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Calculate average of middle 5 hops as the "most reliable"
    avgEccCOMDisp = round(mean(EccHopDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[4:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[4:9]),digits=2)
    
    #create a variable listing the hopnumbers as Hop1, Hop2 etc, looping to q (number of hops from above)
    hopnumber = c()
    for(n in 1:q){
      hopnumber[n] <- paste0("Hop", n, sep = "")
    }
    #add to that a col header "Average"
    hopnumber[q+1] = "Average"
    #convert to data.frame for rbind below
    hopnumber = data.frame(hopnumber)
    
    #Create reactive variable that stays alive for combined final summary table
    values$Left2.0Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
    #combine the data for each hop as rows and the average as the final row
    EccCOMDisp2 <-  rbind(EccHopDisp_df, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTimeHop_df, avgContactTime)
    FlightTime2 <-  rbind(FlightTimeHop_df, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffHop_df, avgEccStiffness)
    PeakForceBW2 <-  rbind(PeakHopBW_df, avgPeakForceBW)
    JumpFreq2 <-  rbind(JumpFreqHop_df, avgJumpFreq)
    
    #Create output data frame withe the hopnumber variable as the first column and data from above as columns
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForceBW2, JumpFreq2)
    
    #rename column names of output data frame (default is the variable name)
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    
    #define output file for Shiny
    OutputdfLeft2.0 <-  as.data.frame(Output)
    
    ({OutputdfLeft2.0})
    
  })
  outputOptions(output, "resultstableLeft2.0", suspendWhenHidden = FALSE)
  
  output$resultstableLeft2.3 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left2.3$datapath, stringsAsFactors = F, skip = 17))
    
    #remove NAs
    newdata <-  na.omit(data1)
    
    #Extract Fz column number 4 and convert character to numeric
    Fz2 <-  as.numeric(newdata[,4])
    
    #make that its own data frame
    Fz2 <-  as.data.frame(Fz2)
    
    #manual BW and mass
    #BW = 85*9.8
    #Mass = 85
    
    #Calculate BW from Mass that is entered on the Shiny - should give an error if no Mass is entered
    BW = (input$Mass)*9.8
    
    #define first flight instance
    Flight <- which(Fz2 <5)[1]
    #define first contact instance after first flight instance
    
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    
    #zero offset using average of values during flight - doesn't really work if there are negative values, and small enough to not worry
    #zerooffset = mean(Fz2$Fz2[Flight:Contact])
    #Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    #list first Flight instance - is this not the same as above??
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    #loop to detect and list all contact and flight instances after first flight - use 10N as the threshold for each - r 1:20 to cover max number of hops, shouldn't cause an error when it gets to the max
    for (r in 1:20){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <10)
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotLeft2.3 <- renderPlot({
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
        geom_vline(xintercept=Contact[16], col="red")
      
      geom_vline(xintercept=Flight[17], col="green") +
        geom_vline(xintercept=Contact[17], col="red")
      geom_vline(xintercept=Flight[18], col="green") +
        geom_vline(xintercept=Contact[18], col="red")
      geom_vline(xintercept=Flight[19], col="green") +
        geom_vline(xintercept=Contact[19], col="red")
      geom_vline(xintercept=Flight[20], col="green") +
        geom_vline(xintercept=Contact[20], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    #Split the full data into FullHops which is from contact to contact, using the final r value from the 1:20 loop above, which should be the last contact instance - subtract 1 to give the number of complete hops
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
    }
    
    #Split the data into Hops from flight to flight
    Hops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
    }
    
    #Define blank variables for the loops below
    PeakHop = c()
    PeakHopBW = c()
    ContactTimeHop = c()
    FlightTimeHop = c()
    MinHopDisp = c()
    EccHopDisp = c()
    EccStiffHop = c()
    JumpFreqHop = c()
    
    
    # the final q value after the loop is now the number of full hops, so it can be reused below
    
    for (w in 1:q){
      
      #Hop1
      
      #Calculate Acceleration using F=m*a
      
      Hop <-  Hops[[w]]
      #HopAcc <- (Hop-BW)/input$Mass
      HopAcc <- (Hop-BW)/input$Mass
      
      
      #Create a rolling average of Acc, 2 frame window (why?)
      HopAccavg = rollapply(HopAcc, 2, mean)
      #insert 0 at the first row
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Calculate Acceleration Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopAccarea = HopAccavg2*0.002
      
      #HopVelInit is a random starting velocity to apply the acceleration to, and will be adjusted later 
      HopVelInit = 1.3
      
      #Relabel HopAccarea as HopVela  add HopVelInit
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      
      #cumulative sum of the acceleration will give the velocity at each point
      HopVel = cumsum(HopVela)
      
      #Create a rolling average of Vel, 2 frame window (why?)
      HopVelavg = rollapply(HopVel, 2, mean)
      #insert 0 at the first row
      HopVelavg2 = insert(HopVelavg, 1, 0)
      #Calculate Velocity Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopVelarea = HopVelavg2*0.002
      
      #Caluclate Displacement as the cumsum of Velocity Area
      HopDisp = cumsum(HopVelarea)
      
      #Define length of the Displacement data
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacement close to zero e.g. if Final displacement (l) is >0.0001, repeat the calculations above with HopVelInit -0.002, and continue until Final displacement is within 0.0001 of zero.
      #Conversely, do the opposite if Final displacement is <0.0001
      if (HopDisp[l]>0){
        while (HopDisp[l] > 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit - 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)}
      }else (HopDisp[l]<0) 
      {
        while (HopDisp[l] < 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit + 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)
        }
      }
      
      
      #Calculate output variables
      
      ContactFrame <- 5 + which(Hop[5:length(Hop)] >10)[1]
      PeakHop[w] = max(Hop)
      PeakHopBW[w] = max(Hop)/BW
      ContactTimeHop[w] = (length(HopDisp) - ContactFrame)*0.002
      FlightTimeHop[w] = (length(HopDisp) - (length(HopDisp) - ContactFrame))*0.002
      MinHopDisp[w] = min(HopDisp)
      EccHopDisp[w] = (HopDisp[ContactFrame] - min(HopDisp))
      EccStiffHop[w] = ((max(Hop) - BW)/(HopDisp[ContactFrame] - min(HopDisp)))/input$Mass
      JumpFreqHop[w] = 1/(length(FullHops[[w]])*0.002)
      
    }
    
    #Convert Output variables to data.frame and round each value to 2 dp
    PeakHop_df = data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits=2)
    PeakHopBW_df = data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits=2)
    ContactTimeHop_df = data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    FlightTimeHop_df = data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    MinHopDisp_df = data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccHopDisp_df = data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccStiffHop_df = data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits=2)
    JumpFreqHop_df = data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Calculate average of middle 5 hops as the "most reliable"
    avgEccCOMDisp = round(mean(EccHopDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[4:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[4:9]),digits=2)
    
    #create a variable listing the hopnumbers as Hop1, Hop2 etc, looping to q (number of hops from above)
    hopnumber = c()
    for(n in 1:q){
      hopnumber[n] <- paste0("Hop", n, sep = "")
    }
    #add to that a col header "Average"
    hopnumber[q+1] = "Average"
    #convert to data.frame for rbind below
    hopnumber = data.frame(hopnumber)
    
    #Create reactive variable that stays alive for combined final summary table
    values$Left2.3Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
    #combine the data for each hop as rows and the average as the final row
    EccCOMDisp2 <-  rbind(EccHopDisp_df, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTimeHop_df, avgContactTime)
    FlightTime2 <-  rbind(FlightTimeHop_df, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffHop_df, avgEccStiffness)
    PeakForceBW2 <-  rbind(PeakHopBW_df, avgPeakForceBW)
    JumpFreq2 <-  rbind(JumpFreqHop_df, avgJumpFreq)
    
    #Create output data frame withe the hopnumber variable as the first column and data from above as columns
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForceBW2, JumpFreq2)
    
    #rename column names of output data frame (default is the variable name)
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    
    #define output file for Shiny
    OutputdfLeft2.3 <-  as.data.frame(Output)
    
    ({OutputdfLeft2.3})
    
  })
  outputOptions(output, "resultstableLeft2.3", suspendWhenHidden = FALSE)
  
  output$resultstableLeft2.6 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left2.6$datapath, stringsAsFactors = F, skip = 17))
    
    #remove NAs
    newdata <-  na.omit(data1)
    
    #Extract Fz column number 4 and convert character to numeric
    Fz2 <-  as.numeric(newdata[,4])
    
    #make that its own data frame
    Fz2 <-  as.data.frame(Fz2)
    
    #manual BW and mass
    #BW = 85*9.8
    #Mass = 85
    
    #Calculate BW from Mass that is entered on the Shiny - should give an error if no Mass is entered
    BW = (input$Mass)*9.8
    
    #define first flight instance
    Flight <- which(Fz2 <5)[1]
    #define first contact instance after first flight instance
    
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    
    #zero offset using average of values during flight - doesn't really work if there are negative values, and small enough to not worry
    #zerooffset = mean(Fz2$Fz2[Flight:Contact])
    #Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    #list first Flight instance - is this not the same as above??
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    #loop to detect and list all contact and flight instances after first flight - use 10N as the threshold for each - r 1:20 to cover max number of hops, shouldn't cause an error when it gets to the max
    for (r in 1:20){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <10)
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotLeft2.6 <- renderPlot({
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
        geom_vline(xintercept=Contact[16], col="red")
      
      geom_vline(xintercept=Flight[17], col="green") +
        geom_vline(xintercept=Contact[17], col="red")
      geom_vline(xintercept=Flight[18], col="green") +
        geom_vline(xintercept=Contact[18], col="red")
      geom_vline(xintercept=Flight[19], col="green") +
        geom_vline(xintercept=Contact[19], col="red")
      geom_vline(xintercept=Flight[20], col="green") +
        geom_vline(xintercept=Contact[20], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    #Split the full data into FullHops which is from contact to contact, using the final r value from the 1:20 loop above, which should be the last contact instance - subtract 1 to give the number of complete hops
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
    }
    
    #Split the data into Hops from flight to flight
    Hops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
    }
    
    #Define blank variables for the loops below
    PeakHop = c()
    PeakHopBW = c()
    ContactTimeHop = c()
    FlightTimeHop = c()
    MinHopDisp = c()
    EccHopDisp = c()
    EccStiffHop = c()
    JumpFreqHop = c()
    
    
    # the final q value after the loop is now the number of full hops, so it can be reused below
    
    for (w in 1:q){
      
      #Hop1
      
      #Calculate Acceleration using F=m*a
      
      Hop <-  Hops[[w]]
      #HopAcc <- (Hop-BW)/input$Mass
      HopAcc <- (Hop-BW)/input$Mass
      
      
      #Create a rolling average of Acc, 2 frame window (why?)
      HopAccavg = rollapply(HopAcc, 2, mean)
      #insert 0 at the first row
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Calculate Acceleration Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopAccarea = HopAccavg2*0.002
      
      #HopVelInit is a random starting velocity to apply the acceleration to, and will be adjusted later 
      HopVelInit = 1.3
      
      #Relabel HopAccarea as HopVela  add HopVelInit
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      
      #cumulative sum of the acceleration will give the velocity at each point
      HopVel = cumsum(HopVela)
      
      #Create a rolling average of Vel, 2 frame window (why?)
      HopVelavg = rollapply(HopVel, 2, mean)
      #insert 0 at the first row
      HopVelavg2 = insert(HopVelavg, 1, 0)
      #Calculate Velocity Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopVelarea = HopVelavg2*0.002
      
      #Caluclate Displacement as the cumsum of Velocity Area
      HopDisp = cumsum(HopVelarea)
      
      #Define length of the Displacement data
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacement close to zero e.g. if Final displacement (l) is >0.0001, repeat the calculations above with HopVelInit -0.002, and continue until Final displacement is within 0.0001 of zero.
      #Conversely, do the opposite if Final displacement is <0.0001
      if (HopDisp[l]>0){
        while (HopDisp[l] > 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit - 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)}
      }else (HopDisp[l]<0) 
      {
        while (HopDisp[l] < 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit + 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)
        }
      }
      
      
      #Calculate output variables
      
      ContactFrame <- 5 + which(Hop[5:length(Hop)] >10)[1]
      PeakHop[w] = max(Hop)
      PeakHopBW[w] = max(Hop)/BW
      ContactTimeHop[w] = (length(HopDisp) - ContactFrame)*0.002
      FlightTimeHop[w] = (length(HopDisp) - (length(HopDisp) - ContactFrame))*0.002
      MinHopDisp[w] = min(HopDisp)
      EccHopDisp[w] = (HopDisp[ContactFrame] - min(HopDisp))
      EccStiffHop[w] = ((max(Hop) - BW)/(HopDisp[ContactFrame] - min(HopDisp)))/input$Mass
      JumpFreqHop[w] = 1/(length(FullHops[[w]])*0.002)
      
    }
    
    #Convert Output variables to data.frame and round each value to 2 dp
    PeakHop_df = data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits=2)
    PeakHopBW_df = data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits=2)
    ContactTimeHop_df = data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    FlightTimeHop_df = data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    MinHopDisp_df = data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccHopDisp_df = data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccStiffHop_df = data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits=2)
    JumpFreqHop_df = data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Calculate average of middle 5 hops as the "most reliable"
    avgEccCOMDisp = round(mean(EccHopDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[4:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[4:9]),digits=2)
    
    #create a variable listing the hopnumbers as Hop1, Hop2 etc, looping to q (number of hops from above)
    hopnumber = c()
    for(n in 1:q){
      hopnumber[n] <- paste0("Hop", n, sep = "")
    }
    #add to that a col header "Average"
    hopnumber[q+1] = "Average"
    #convert to data.frame for rbind below
    hopnumber = data.frame(hopnumber)
    
    #Create reactive variable that stays alive for combined final summary table
    values$Left2.6Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
    #combine the data for each hop as rows and the average as the final row
    EccCOMDisp2 <-  rbind(EccHopDisp_df, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTimeHop_df, avgContactTime)
    FlightTime2 <-  rbind(FlightTimeHop_df, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffHop_df, avgEccStiffness)
    PeakForceBW2 <-  rbind(PeakHopBW_df, avgPeakForceBW)
    JumpFreq2 <-  rbind(JumpFreqHop_df, avgJumpFreq)
    
    #Create output data frame withe the hopnumber variable as the first column and data from above as columns
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForceBW2, JumpFreq2)
    
    #rename column names of output data frame (default is the variable name)
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    
    #define output file for Shiny
    OutputdfLeft2.6 <-  as.data.frame(Output)
    
    ({OutputdfLeft2.6})
    
  })
  outputOptions(output, "resultstableLeft2.6", suspendWhenHidden = FALSE)
  
  output$resultstableLeftSS <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$LeftSS$datapath, stringsAsFactors = F, skip = 17))
    
    #remove NAs
    newdata <-  na.omit(data1)
    
    #Extract Fz column number 4 and convert character to numeric
    Fz2 <-  as.numeric(newdata[,4])
    
    #make that its own data frame
    Fz2 <-  as.data.frame(Fz2)
    
    #manual BW and mass
    #BW = 85*9.8
    #Mass = 85
    
    #Calculate BW from Mass that is entered on the Shiny - should give an error if no Mass is entered
    BW = (input$Mass)*9.8
    
    #define first flight instance
    Flight <- which(Fz2 <5)[1]
    #define first contact instance after first flight instance
    
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    
    #zero offset using average of values during flight - doesn't really work if there are negative values, and small enough to not worry
    #zerooffset = mean(Fz2$Fz2[Flight:Contact])
    #Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    #list first Flight instance - is this not the same as above??
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    #loop to detect and list all contact and flight instances after first flight - use 10N as the threshold for each - r 1:20 to cover max number of hops, shouldn't cause an error when it gets to the max
    for (r in 1:20){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <10)
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotLeftSS <- renderPlot({
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
        geom_vline(xintercept=Contact[16], col="red")
      
      geom_vline(xintercept=Flight[17], col="green") +
        geom_vline(xintercept=Contact[17], col="red")
      geom_vline(xintercept=Flight[18], col="green") +
        geom_vline(xintercept=Contact[18], col="red")
      geom_vline(xintercept=Flight[19], col="green") +
        geom_vline(xintercept=Contact[19], col="red")
      geom_vline(xintercept=Flight[20], col="green") +
        geom_vline(xintercept=Contact[20], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    #Split the full data into FullHops which is from contact to contact, using the final r value from the 1:20 loop above, which should be the last contact instance - subtract 1 to give the number of complete hops
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
    }
    
    #Split the data into Hops from flight to flight
    Hops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
    }
    
    #Define blank variables for the loops below
    PeakHop = c()
    PeakHopBW = c()
    ContactTimeHop = c()
    FlightTimeHop = c()
    MinHopDisp = c()
    EccHopDisp = c()
    EccStiffHop = c()
    JumpFreqHop = c()
    
    
    # the final q value after the loop is now the number of full hops, so it can be reused below
    
    for (w in 1:q){
      
      #Hop1
      
      #Calculate Acceleration using F=m*a
      
      Hop <-  Hops[[w]]
      #HopAcc <- (Hop-BW)/input$Mass
      HopAcc <- (Hop-BW)/input$Mass
      
      
      #Create a rolling average of Acc, 2 frame window (why?)
      HopAccavg = rollapply(HopAcc, 2, mean)
      #insert 0 at the first row
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Calculate Acceleration Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopAccarea = HopAccavg2*0.002
      
      #HopVelInit is a random starting velocity to apply the acceleration to, and will be adjusted later 
      HopVelInit = 1.3
      
      #Relabel HopAccarea as HopVela  add HopVelInit
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      
      #cumulative sum of the acceleration will give the velocity at each point
      HopVel = cumsum(HopVela)
      
      #Create a rolling average of Vel, 2 frame window (why?)
      HopVelavg = rollapply(HopVel, 2, mean)
      #insert 0 at the first row
      HopVelavg2 = insert(HopVelavg, 1, 0)
      #Calculate Velocity Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopVelarea = HopVelavg2*0.002
      
      #Caluclate Displacement as the cumsum of Velocity Area
      HopDisp = cumsum(HopVelarea)
      
      #Define length of the Displacement data
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacement close to zero e.g. if Final displacement (l) is >0.0001, repeat the calculations above with HopVelInit -0.002, and continue until Final displacement is within 0.0001 of zero.
      #Conversely, do the opposite if Final displacement is <0.0001
      if (HopDisp[l]>0){
        while (HopDisp[l] > 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit - 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)}
      }else (HopDisp[l]<0) 
      {
        while (HopDisp[l] < 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit + 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)
        }
      }
      
      
      #Calculate output variables
      
      ContactFrame <- 5 + which(Hop[5:length(Hop)] >10)[1]
      PeakHop[w] = max(Hop)
      PeakHopBW[w] = max(Hop)/BW
      ContactTimeHop[w] = (length(HopDisp) - ContactFrame)*0.002
      FlightTimeHop[w] = (length(HopDisp) - (length(HopDisp) - ContactFrame))*0.002
      MinHopDisp[w] = min(HopDisp)
      EccHopDisp[w] = (HopDisp[ContactFrame] - min(HopDisp))
      EccStiffHop[w] = ((max(Hop) - BW)/(HopDisp[ContactFrame] - min(HopDisp)))/input$Mass
      JumpFreqHop[w] = 1/(length(FullHops[[w]])*0.002)
      
    }
    
    #Convert Output variables to data.frame and round each value to 2 dp
    PeakHop_df = data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits=2)
    PeakHopBW_df = data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits=2)
    ContactTimeHop_df = data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    FlightTimeHop_df = data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    MinHopDisp_df = data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccHopDisp_df = data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccStiffHop_df = data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits=2)
    JumpFreqHop_df = data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Calculate average of middle 5 hops as the "most reliable"
    avgEccCOMDisp = round(mean(EccHopDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[4:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[4:9]),digits=2)
    
    #create a variable listing the hopnumbers as Hop1, Hop2 etc, looping to q (number of hops from above)
    hopnumber = c()
    for(n in 1:q){
      hopnumber[n] <- paste0("Hop", n, sep = "")
    }
    #add to that a col header "Average"
    hopnumber[q+1] = "Average"
    #convert to data.frame for rbind below
    hopnumber = data.frame(hopnumber)
    
    #Create reactive variable that stays alive for combined final summary table
    values$LeftSSAverages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
    #combine the data for each hop as rows and the average as the final row
    EccCOMDisp2 <-  rbind(EccHopDisp_df, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTimeHop_df, avgContactTime)
    FlightTime2 <-  rbind(FlightTimeHop_df, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffHop_df, avgEccStiffness)
    PeakForceBW2 <-  rbind(PeakHopBW_df, avgPeakForceBW)
    JumpFreq2 <-  rbind(JumpFreqHop_df, avgJumpFreq)
    
    #Create output data frame withe the hopnumber variable as the first column and data from above as columns
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForceBW2, JumpFreq2)
    
    #rename column names of output data frame (default is the variable name)
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    
    #define output file for Shiny
    OutputdfLeftSS <-  as.data.frame(Output)
    
    ({OutputdfLeftSS})
    
  })
  outputOptions(output, "resultstableLeftSS", suspendWhenHidden = FALSE)
  
  output$resultstableRight1.7 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right1.7$datapath, stringsAsFactors = F, skip = 17))
    
    #remove NAs
    newdata <-  na.omit(data1)
    
    #Extract Fz column number 4 and convert character to numeric
    Fz2 <-  as.numeric(newdata[,4])
    
    #make that its own data frame
    Fz2 <-  as.data.frame(Fz2)
    
    #manual BW and mass
    #BW = 85*9.8
    #Mass = 85
    
    #Calculate BW from Mass that is entered on the Shiny - should give an error if no Mass is entered
    BW = (input$Mass)*9.8
    
    #define first flight instance
    Flight <- which(Fz2 <5)[1]
    #define first contact instance after first flight instance
    
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    
    #zero offset using average of values during flight - doesn't really work if there are negative values, and small enough to not worry
    #zerooffset = mean(Fz2$Fz2[Flight:Contact])
    #Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    #list first Flight instance - is this not the same as above??
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    #loop to detect and list all contact and flight instances after first flight - use 10N as the threshold for each - r 1:20 to cover max number of hops, shouldn't cause an error when it gets to the max
    for (r in 1:20){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <10)
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRight1.7 <- renderPlot({
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
        geom_vline(xintercept=Contact[16], col="red")
      
      geom_vline(xintercept=Flight[17], col="green") +
        geom_vline(xintercept=Contact[17], col="red")
      geom_vline(xintercept=Flight[18], col="green") +
        geom_vline(xintercept=Contact[18], col="red")
      geom_vline(xintercept=Flight[19], col="green") +
        geom_vline(xintercept=Contact[19], col="red")
      geom_vline(xintercept=Flight[20], col="green") +
        geom_vline(xintercept=Contact[20], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    #Split the full data into FullHops which is from contact to contact, using the final r value from the 1:20 loop above, which should be the last contact instance - subtract 1 to give the number of complete hops
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
    }
    
    #Split the data into Hops from flight to flight
    Hops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
    }
    
    #Define blank variables for the loops below
    PeakHop = c()
    PeakHopBW = c()
    ContactTimeHop = c()
    FlightTimeHop = c()
    MinHopDisp = c()
    EccHopDisp = c()
    EccStiffHop = c()
    JumpFreqHop = c()
    
    
    # the final q value after the loop is now the number of full hops, so it can be reused below
    
    for (w in 1:q){
      
      #Hop1
      
      #Calculate Acceleration using F=m*a
      
      Hop <-  Hops[[w]]
      #HopAcc <- (Hop-BW)/input$Mass
      HopAcc <- (Hop-BW)/input$Mass
      
      
      #Create a rolling average of Acc, 2 frame window (why?)
      HopAccavg = rollapply(HopAcc, 2, mean)
      #insert 0 at the first row
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Calculate Acceleration Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopAccarea = HopAccavg2*0.002
      
      #HopVelInit is a random starting velocity to apply the acceleration to, and will be adjusted later 
      HopVelInit = 1.3
      
      #Relabel HopAccarea as HopVela  add HopVelInit
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      
      #cumulative sum of the acceleration will give the velocity at each point
      HopVel = cumsum(HopVela)
      
      #Create a rolling average of Vel, 2 frame window (why?)
      HopVelavg = rollapply(HopVel, 2, mean)
      #insert 0 at the first row
      HopVelavg2 = insert(HopVelavg, 1, 0)
      #Calculate Velocity Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopVelarea = HopVelavg2*0.002
      
      #Caluclate Displacement as the cumsum of Velocity Area
      HopDisp = cumsum(HopVelarea)
      
      #Define length of the Displacement data
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacement close to zero e.g. if Final displacement (l) is >0.0001, repeat the calculations above with HopVelInit -0.002, and continue until Final displacement is within 0.0001 of zero.
      #Conversely, do the opposite if Final displacement is <0.0001
      if (HopDisp[l]>0){
        while (HopDisp[l] > 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit - 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)}
      }else (HopDisp[l]<0) 
      {
        while (HopDisp[l] < 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit + 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)
        }
      }
      
      
      #Calculate output variables
      
      ContactFrame <- 5 + which(Hop[5:length(Hop)] >10)[1]
      PeakHop[w] = max(Hop)
      PeakHopBW[w] = max(Hop)/BW
      ContactTimeHop[w] = (length(HopDisp) - ContactFrame)*0.002
      FlightTimeHop[w] = (length(HopDisp) - (length(HopDisp) - ContactFrame))*0.002
      MinHopDisp[w] = min(HopDisp)
      EccHopDisp[w] = (HopDisp[ContactFrame] - min(HopDisp))
      EccStiffHop[w] = ((max(Hop) - BW)/(HopDisp[ContactFrame] - min(HopDisp)))/input$Mass
      JumpFreqHop[w] = 1/(length(FullHops[[w]])*0.002)
      
    }
    
    #Convert Output variables to data.frame and round each value to 2 dp
    PeakHop_df = data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits=2)
    PeakHopBW_df = data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits=2)
    ContactTimeHop_df = data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    FlightTimeHop_df = data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    MinHopDisp_df = data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccHopDisp_df = data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccStiffHop_df = data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits=2)
    JumpFreqHop_df = data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Calculate average of middle 5 hops as the "most reliable"
    avgEccCOMDisp = round(mean(EccHopDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[4:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[4:9]),digits=2)
    
    #create a variable listing the hopnumbers as Hop1, Hop2 etc, looping to q (number of hops from above)
    hopnumber = c()
    for(n in 1:q){
      hopnumber[n] <- paste0("Hop", n, sep = "")
    }
    #add to that a col header "Average"
    hopnumber[q+1] = "Average"
    #convert to data.frame for rbind below
    hopnumber = data.frame(hopnumber)
    
    #Create reactive variable that stays alive for combined final summary table
    values$Right1.7Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
    #combine the data for each hop as rows and the average as the final row
    EccCOMDisp2 <-  rbind(EccHopDisp_df, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTimeHop_df, avgContactTime)
    FlightTime2 <-  rbind(FlightTimeHop_df, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffHop_df, avgEccStiffness)
    PeakForceBW2 <-  rbind(PeakHopBW_df, avgPeakForceBW)
    JumpFreq2 <-  rbind(JumpFreqHop_df, avgJumpFreq)
    
    #Create output data frame withe the hopnumber variable as the first column and data from above as columns
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForceBW2, JumpFreq2)
    
    #rename column names of output data frame (default is the variable name)
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    
    #define output file for Shiny
    OutputdfRight1.7 <-  as.data.frame(Output)
    
    ({OutputdfRight1.7})
    
  })
  outputOptions(output, "resultstableRight1.7", suspendWhenHidden = FALSE)
  
  output$resultstableRight2.0 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right2.0$datapath, stringsAsFactors = F, skip = 17))
    
    #remove NAs
    newdata <-  na.omit(data1)
    
    #Extract Fz column number 4 and convert character to numeric
    Fz2 <-  as.numeric(newdata[,4])
    
    #make that its own data frame
    Fz2 <-  as.data.frame(Fz2)
    
    #manual BW and mass
    #BW = 85*9.8
    #Mass = 85
    
    #Calculate BW from Mass that is entered on the Shiny - should give an error if no Mass is entered
    BW = (input$Mass)*9.8
    
    #define first flight instance
    Flight <- which(Fz2 <5)[1]
    #define first contact instance after first flight instance
    
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    
    #zero offset using average of values during flight - doesn't really work if there are negative values, and small enough to not worry
    #zerooffset = mean(Fz2$Fz2[Flight:Contact])
    #Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    #list first Flight instance - is this not the same as above??
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    #loop to detect and list all contact and flight instances after first flight - use 10N as the threshold for each - r 1:20 to cover max number of hops, shouldn't cause an error when it gets to the max
    for (r in 1:20){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <10)
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRight2.0 <- renderPlot({
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
        geom_vline(xintercept=Contact[16], col="red")
      
      geom_vline(xintercept=Flight[17], col="green") +
        geom_vline(xintercept=Contact[17], col="red")
      geom_vline(xintercept=Flight[18], col="green") +
        geom_vline(xintercept=Contact[18], col="red")
      geom_vline(xintercept=Flight[19], col="green") +
        geom_vline(xintercept=Contact[19], col="red")
      geom_vline(xintercept=Flight[20], col="green") +
        geom_vline(xintercept=Contact[20], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    #Split the full data into FullHops which is from contact to contact, using the final r value from the 1:20 loop above, which should be the last contact instance - subtract 1 to give the number of complete hops
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
    }
    
    #Split the data into Hops from flight to flight
    Hops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
    }
    
    #Define blank variables for the loops below
    PeakHop = c()
    PeakHopBW = c()
    ContactTimeHop = c()
    FlightTimeHop = c()
    MinHopDisp = c()
    EccHopDisp = c()
    EccStiffHop = c()
    JumpFreqHop = c()
    
    
    # the final q value after the loop is now the number of full hops, so it can be reused below
    
    for (w in 1:q){
      
      #Hop1
      
      #Calculate Acceleration using F=m*a
      
      Hop <-  Hops[[w]]
      #HopAcc <- (Hop-BW)/input$Mass
      HopAcc <- (Hop-BW)/input$Mass
      
      
      #Create a rolling average of Acc, 2 frame window (why?)
      HopAccavg = rollapply(HopAcc, 2, mean)
      #insert 0 at the first row
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Calculate Acceleration Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopAccarea = HopAccavg2*0.002
      
      #HopVelInit is a random starting velocity to apply the acceleration to, and will be adjusted later 
      HopVelInit = 1.3
      
      #Relabel HopAccarea as HopVela  add HopVelInit
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      
      #cumulative sum of the acceleration will give the velocity at each point
      HopVel = cumsum(HopVela)
      
      #Create a rolling average of Vel, 2 frame window (why?)
      HopVelavg = rollapply(HopVel, 2, mean)
      #insert 0 at the first row
      HopVelavg2 = insert(HopVelavg, 1, 0)
      #Calculate Velocity Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopVelarea = HopVelavg2*0.002
      
      #Caluclate Displacement as the cumsum of Velocity Area
      HopDisp = cumsum(HopVelarea)
      
      #Define length of the Displacement data
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacement close to zero e.g. if Final displacement (l) is >0.0001, repeat the calculations above with HopVelInit -0.002, and continue until Final displacement is within 0.0001 of zero.
      #Conversely, do the opposite if Final displacement is <0.0001
      if (HopDisp[l]>0){
        while (HopDisp[l] > 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit - 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)}
      }else (HopDisp[l]<0) 
      {
        while (HopDisp[l] < 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit + 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)
        }
      }
      
      
      #Calculate output variables
      
      ContactFrame <- 5 + which(Hop[5:length(Hop)] >10)[1]
      PeakHop[w] = max(Hop)
      PeakHopBW[w] = max(Hop)/BW
      ContactTimeHop[w] = (length(HopDisp) - ContactFrame)*0.002
      FlightTimeHop[w] = (length(HopDisp) - (length(HopDisp) - ContactFrame))*0.002
      MinHopDisp[w] = min(HopDisp)
      EccHopDisp[w] = (HopDisp[ContactFrame] - min(HopDisp))
      EccStiffHop[w] = ((max(Hop) - BW)/(HopDisp[ContactFrame] - min(HopDisp)))/input$Mass
      JumpFreqHop[w] = 1/(length(FullHops[[w]])*0.002)
      
    }
    
    #Convert Output variables to data.frame and round each value to 2 dp
    PeakHop_df = data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits=2)
    PeakHopBW_df = data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits=2)
    ContactTimeHop_df = data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    FlightTimeHop_df = data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    MinHopDisp_df = data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccHopDisp_df = data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccStiffHop_df = data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits=2)
    JumpFreqHop_df = data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Calculate average of middle 5 hops as the "most reliable"
    avgEccCOMDisp = round(mean(EccHopDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[4:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[4:9]),digits=2)
    
    #create a variable listing the hopnumbers as Hop1, Hop2 etc, looping to q (number of hops from above)
    hopnumber = c()
    for(n in 1:q){
      hopnumber[n] <- paste0("Hop", n, sep = "")
    }
    #add to that a col header "Average"
    hopnumber[q+1] = "Average"
    #convert to data.frame for rbind below
    hopnumber = data.frame(hopnumber)
    
    #Create reactive variable that stays alive for combined final summary table
    values$Right2.0Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
    #combine the data for each hop as rows and the average as the final row
    EccCOMDisp2 <-  rbind(EccHopDisp_df, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTimeHop_df, avgContactTime)
    FlightTime2 <-  rbind(FlightTimeHop_df, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffHop_df, avgEccStiffness)
    PeakForceBW2 <-  rbind(PeakHopBW_df, avgPeakForceBW)
    JumpFreq2 <-  rbind(JumpFreqHop_df, avgJumpFreq)
    
    #Create output data frame withe the hopnumber variable as the first column and data from above as columns
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForceBW2, JumpFreq2)
    
    #rename column names of output data frame (default is the variable name)
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    
    #define output file for Shiny
    OutputdfRight2.0 <-  as.data.frame(Output)
    
    ({OutputdfRight2.0})
    
  })
  outputOptions(output, "resultstableRight2.0", suspendWhenHidden = FALSE)
  
  output$resultstableRight2.3 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right2.3$datapath, stringsAsFactors = F, skip = 17))
    
    #remove NAs
    newdata <-  na.omit(data1)
    
    #Extract Fz column number 4 and convert character to numeric
    Fz2 <-  as.numeric(newdata[,4])
    
    #make that its own data frame
    Fz2 <-  as.data.frame(Fz2)
    
    #manual BW and mass
    #BW = 85*9.8
    #Mass = 85
    
    #Calculate BW from Mass that is entered on the Shiny - should give an error if no Mass is entered
    BW = (input$Mass)*9.8
    
    #define first flight instance
    Flight <- which(Fz2 <5)[1]
    #define first contact instance after first flight instance
    
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    
    #zero offset using average of values during flight - doesn't really work if there are negative values, and small enough to not worry
    #zerooffset = mean(Fz2$Fz2[Flight:Contact])
    #Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    #list first Flight instance - is this not the same as above??
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    #loop to detect and list all contact and flight instances after first flight - use 10N as the threshold for each - r 1:20 to cover max number of hops, shouldn't cause an error when it gets to the max
    for (r in 1:20){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <10)
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRight2.3 <- renderPlot({
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
        geom_vline(xintercept=Contact[16], col="red")
      
      geom_vline(xintercept=Flight[17], col="green") +
        geom_vline(xintercept=Contact[17], col="red")
      geom_vline(xintercept=Flight[18], col="green") +
        geom_vline(xintercept=Contact[18], col="red")
      geom_vline(xintercept=Flight[19], col="green") +
        geom_vline(xintercept=Contact[19], col="red")
      geom_vline(xintercept=Flight[20], col="green") +
        geom_vline(xintercept=Contact[20], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    #Split the full data into FullHops which is from contact to contact, using the final r value from the 1:20 loop above, which should be the last contact instance - subtract 1 to give the number of complete hops
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
    }
    
    #Split the data into Hops from flight to flight
    Hops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
    }
    
    #Define blank variables for the loops below
    PeakHop = c()
    PeakHopBW = c()
    ContactTimeHop = c()
    FlightTimeHop = c()
    MinHopDisp = c()
    EccHopDisp = c()
    EccStiffHop = c()
    JumpFreqHop = c()
    
    
    # the final q value after the loop is now the number of full hops, so it can be reused below
    
    for (w in 1:q){
      
      #Hop1
      
      #Calculate Acceleration using F=m*a
      
      Hop <-  Hops[[w]]
      #HopAcc <- (Hop-BW)/input$Mass
      HopAcc <- (Hop-BW)/input$Mass
      
      
      #Create a rolling average of Acc, 2 frame window (why?)
      HopAccavg = rollapply(HopAcc, 2, mean)
      #insert 0 at the first row
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Calculate Acceleration Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopAccarea = HopAccavg2*0.002
      
      #HopVelInit is a random starting velocity to apply the acceleration to, and will be adjusted later 
      HopVelInit = 1.3
      
      #Relabel HopAccarea as HopVela  add HopVelInit
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      
      #cumulative sum of the acceleration will give the velocity at each point
      HopVel = cumsum(HopVela)
      
      #Create a rolling average of Vel, 2 frame window (why?)
      HopVelavg = rollapply(HopVel, 2, mean)
      #insert 0 at the first row
      HopVelavg2 = insert(HopVelavg, 1, 0)
      #Calculate Velocity Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopVelarea = HopVelavg2*0.002
      
      #Caluclate Displacement as the cumsum of Velocity Area
      HopDisp = cumsum(HopVelarea)
      
      #Define length of the Displacement data
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacement close to zero e.g. if Final displacement (l) is >0.0001, repeat the calculations above with HopVelInit -0.002, and continue until Final displacement is within 0.0001 of zero.
      #Conversely, do the opposite if Final displacement is <0.0001
      if (HopDisp[l]>0){
        while (HopDisp[l] > 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit - 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)}
      }else (HopDisp[l]<0) 
      {
        while (HopDisp[l] < 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit + 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)
        }
      }
      
      
      #Calculate output variables
      
      ContactFrame <- 5 + which(Hop[5:length(Hop)] >10)[1]
      PeakHop[w] = max(Hop)
      PeakHopBW[w] = max(Hop)/BW
      ContactTimeHop[w] = (length(HopDisp) - ContactFrame)*0.002
      FlightTimeHop[w] = (length(HopDisp) - (length(HopDisp) - ContactFrame))*0.002
      MinHopDisp[w] = min(HopDisp)
      EccHopDisp[w] = (HopDisp[ContactFrame] - min(HopDisp))
      EccStiffHop[w] = ((max(Hop) - BW)/(HopDisp[ContactFrame] - min(HopDisp)))/input$Mass
      JumpFreqHop[w] = 1/(length(FullHops[[w]])*0.002)
      
    }
    
    #Convert Output variables to data.frame and round each value to 2 dp
    PeakHop_df = data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits=2)
    PeakHopBW_df = data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits=2)
    ContactTimeHop_df = data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    FlightTimeHop_df = data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    MinHopDisp_df = data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccHopDisp_df = data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccStiffHop_df = data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits=2)
    JumpFreqHop_df = data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Calculate average of middle 5 hops as the "most reliable"
    avgEccCOMDisp = round(mean(EccHopDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[4:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[4:9]),digits=2)
    
    #create a variable listing the hopnumbers as Hop1, Hop2 etc, looping to q (number of hops from above)
    hopnumber = c()
    for(n in 1:q){
      hopnumber[n] <- paste0("Hop", n, sep = "")
    }
    #add to that a col header "Average"
    hopnumber[q+1] = "Average"
    #convert to data.frame for rbind below
    hopnumber = data.frame(hopnumber)
    
    #Create reactive variable that stays alive for combined final summary table
    values$Right2.3Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
    #combine the data for each hop as rows and the average as the final row
    EccCOMDisp2 <-  rbind(EccHopDisp_df, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTimeHop_df, avgContactTime)
    FlightTime2 <-  rbind(FlightTimeHop_df, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffHop_df, avgEccStiffness)
    PeakForceBW2 <-  rbind(PeakHopBW_df, avgPeakForceBW)
    JumpFreq2 <-  rbind(JumpFreqHop_df, avgJumpFreq)
    
    #Create output data frame withe the hopnumber variable as the first column and data from above as columns
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForceBW2, JumpFreq2)
    
    #rename column names of output data frame (default is the variable name)
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    
    #define output file for Shiny
    OutputdfRight2.3 <-  as.data.frame(Output)
    
    ({OutputdfRight2.3})
    
  })
  outputOptions(output, "resultstableRight2.3", suspendWhenHidden = FALSE)
  
  output$resultstableRight2.6 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right2.6$datapath, stringsAsFactors = F, skip = 17))
    
    #remove NAs
    newdata <-  na.omit(data1)
    
    #Extract Fz column number 4 and convert character to numeric
    Fz2 <-  as.numeric(newdata[,4])
    
    #make that its own data frame
    Fz2 <-  as.data.frame(Fz2)
    
    #manual BW and mass
    #BW = 85*9.8
    #Mass = 85
    
    #Calculate BW from Mass that is entered on the Shiny - should give an error if no Mass is entered
    BW = (input$Mass)*9.8
    
    #define first flight instance
    Flight <- which(Fz2 <5)[1]
    #define first contact instance after first flight instance
    
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    
    #zero offset using average of values during flight - doesn't really work if there are negative values, and small enough to not worry
    #zerooffset = mean(Fz2$Fz2[Flight:Contact])
    #Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    #list first Flight instance - is this not the same as above??
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    #loop to detect and list all contact and flight instances after first flight - use 10N as the threshold for each - r 1:20 to cover max number of hops, shouldn't cause an error when it gets to the max
    for (r in 1:20){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <10)
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRight2.6 <- renderPlot({
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
        geom_vline(xintercept=Contact[16], col="red")
      
      geom_vline(xintercept=Flight[17], col="green") +
        geom_vline(xintercept=Contact[17], col="red")
      geom_vline(xintercept=Flight[18], col="green") +
        geom_vline(xintercept=Contact[18], col="red")
      geom_vline(xintercept=Flight[19], col="green") +
        geom_vline(xintercept=Contact[19], col="red")
      geom_vline(xintercept=Flight[20], col="green") +
        geom_vline(xintercept=Contact[20], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    #Split the full data into FullHops which is from contact to contact, using the final r value from the 1:20 loop above, which should be the last contact instance - subtract 1 to give the number of complete hops
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
    }
    
    #Split the data into Hops from flight to flight
    Hops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
    }
    
    #Define blank variables for the loops below
    PeakHop = c()
    PeakHopBW = c()
    ContactTimeHop = c()
    FlightTimeHop = c()
    MinHopDisp = c()
    EccHopDisp = c()
    EccStiffHop = c()
    JumpFreqHop = c()
    
    
    # the final q value after the loop is now the number of full hops, so it can be reused below
    
    for (w in 1:q){
      
      #Hop1
      
      #Calculate Acceleration using F=m*a
      
      Hop <-  Hops[[w]]
      #HopAcc <- (Hop-BW)/input$Mass
      HopAcc <- (Hop-BW)/input$Mass
      
      
      #Create a rolling average of Acc, 2 frame window (why?)
      HopAccavg = rollapply(HopAcc, 2, mean)
      #insert 0 at the first row
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Calculate Acceleration Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopAccarea = HopAccavg2*0.002
      
      #HopVelInit is a random starting velocity to apply the acceleration to, and will be adjusted later 
      HopVelInit = 1.3
      
      #Relabel HopAccarea as HopVela  add HopVelInit
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      
      #cumulative sum of the acceleration will give the velocity at each point
      HopVel = cumsum(HopVela)
      
      #Create a rolling average of Vel, 2 frame window (why?)
      HopVelavg = rollapply(HopVel, 2, mean)
      #insert 0 at the first row
      HopVelavg2 = insert(HopVelavg, 1, 0)
      #Calculate Velocity Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopVelarea = HopVelavg2*0.002
      
      #Caluclate Displacement as the cumsum of Velocity Area
      HopDisp = cumsum(HopVelarea)
      
      #Define length of the Displacement data
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacement close to zero e.g. if Final displacement (l) is >0.0001, repeat the calculations above with HopVelInit -0.002, and continue until Final displacement is within 0.0001 of zero.
      #Conversely, do the opposite if Final displacement is <0.0001
      if (HopDisp[l]>0){
        while (HopDisp[l] > 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit - 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)}
      }else (HopDisp[l]<0) 
      {
        while (HopDisp[l] < 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit + 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)
        }
      }
      
      
      #Calculate output variables
      
      ContactFrame <- 5 + which(Hop[5:length(Hop)] >10)[1]
      PeakHop[w] = max(Hop)
      PeakHopBW[w] = max(Hop)/BW
      ContactTimeHop[w] = (length(HopDisp) - ContactFrame)*0.002
      FlightTimeHop[w] = (length(HopDisp) - (length(HopDisp) - ContactFrame))*0.002
      MinHopDisp[w] = min(HopDisp)
      EccHopDisp[w] = (HopDisp[ContactFrame] - min(HopDisp))
      EccStiffHop[w] = ((max(Hop) - BW)/(HopDisp[ContactFrame] - min(HopDisp)))/input$Mass
      JumpFreqHop[w] = 1/(length(FullHops[[w]])*0.002)
      
    }
    
    #Convert Output variables to data.frame and round each value to 2 dp
    PeakHop_df = data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits=2)
    PeakHopBW_df = data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits=2)
    ContactTimeHop_df = data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    FlightTimeHop_df = data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    MinHopDisp_df = data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccHopDisp_df = data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccStiffHop_df = data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits=2)
    JumpFreqHop_df = data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Calculate average of middle 5 hops as the "most reliable"
    avgEccCOMDisp = round(mean(EccHopDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[4:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[4:9]),digits=2)
    
    #create a variable listing the hopnumbers as Hop1, Hop2 etc, looping to q (number of hops from above)
    hopnumber = c()
    for(n in 1:q){
      hopnumber[n] <- paste0("Hop", n, sep = "")
    }
    #add to that a col header "Average"
    hopnumber[q+1] = "Average"
    #convert to data.frame for rbind below
    hopnumber = data.frame(hopnumber)
    
    #Create reactive variable that stays alive for combined final summary table
    values$Right2.6Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
    #combine the data for each hop as rows and the average as the final row
    EccCOMDisp2 <-  rbind(EccHopDisp_df, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTimeHop_df, avgContactTime)
    FlightTime2 <-  rbind(FlightTimeHop_df, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffHop_df, avgEccStiffness)
    PeakForceBW2 <-  rbind(PeakHopBW_df, avgPeakForceBW)
    JumpFreq2 <-  rbind(JumpFreqHop_df, avgJumpFreq)
    
    #Create output data frame withe the hopnumber variable as the first column and data from above as columns
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForceBW2, JumpFreq2)
    
    #rename column names of output data frame (default is the variable name)
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    
    #define output file for Shiny
    OutputdfRight2.6 <-  as.data.frame(Output)
    
    ({OutputdfRight2.6})
    
  })
  outputOptions(output, "resultstableRight2.6", suspendWhenHidden = FALSE)
  
  output$resultstableRightSS <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$RightSS$datapath, stringsAsFactors = F, skip = 17))
    
    #remove NAs
    newdata <-  na.omit(data1)
    
    #Extract Fz column number 4 and convert character to numeric
    Fz2 <-  as.numeric(newdata[,4])
    
    #make that its own data frame
    Fz2 <-  as.data.frame(Fz2)
    
    #manual BW and mass
    #BW = 85*9.8
    #Mass = 85
    
    #Calculate BW from Mass that is entered on the Shiny - should give an error if no Mass is entered
    BW = (input$Mass)*9.8
    
    #define first flight instance
    Flight <- which(Fz2 <5)[1]
    #define first contact instance after first flight instance
    
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    
    #zero offset using average of values during flight - doesn't really work if there are negative values, and small enough to not worry
    #zerooffset = mean(Fz2$Fz2[Flight:Contact])
    #Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    #list first Flight instance - is this not the same as above??
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    #loop to detect and list all contact and flight instances after first flight - use 10N as the threshold for each - r 1:20 to cover max number of hops, shouldn't cause an error when it gets to the max
    for (r in 1:20){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <10)
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRightSS <- renderPlot({
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
        geom_vline(xintercept=Contact[16], col="red")
      
      geom_vline(xintercept=Flight[17], col="green") +
        geom_vline(xintercept=Contact[17], col="red")
      geom_vline(xintercept=Flight[18], col="green") +
        geom_vline(xintercept=Contact[18], col="red")
      geom_vline(xintercept=Flight[19], col="green") +
        geom_vline(xintercept=Contact[19], col="red")
      geom_vline(xintercept=Flight[20], col="green") +
        geom_vline(xintercept=Contact[20], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    #Split the full data into FullHops which is from contact to contact, using the final r value from the 1:20 loop above, which should be the last contact instance - subtract 1 to give the number of complete hops
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      FullHops[[q]] <- Fz2$Fz2[Contact[q]:Contact[t]]
    }
    
    #Split the data into Hops from flight to flight
    Hops <- list()
    for(q in 1:(r-1)){
      t <- q+1
      Hops[[q]] <- Fz2$Fz2[Flight[q]:Flight[t]]
    }
    
    #Define blank variables for the loops below
    PeakHop = c()
    PeakHopBW = c()
    ContactTimeHop = c()
    FlightTimeHop = c()
    MinHopDisp = c()
    EccHopDisp = c()
    EccStiffHop = c()
    JumpFreqHop = c()
    
    
    # the final q value after the loop is now the number of full hops, so it can be reused below
    
    for (w in 1:q){
      
      #Hop1
      
      #Calculate Acceleration using F=m*a
      
      Hop <-  Hops[[w]]
      #HopAcc <- (Hop-BW)/input$Mass
      HopAcc <- (Hop-BW)/input$Mass
      
      
      #Create a rolling average of Acc, 2 frame window (why?)
      HopAccavg = rollapply(HopAcc, 2, mean)
      #insert 0 at the first row
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Calculate Acceleration Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopAccarea = HopAccavg2*0.002
      
      #HopVelInit is a random starting velocity to apply the acceleration to, and will be adjusted later 
      HopVelInit = 1.3
      
      #Relabel HopAccarea as HopVela  add HopVelInit
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      
      #cumulative sum of the acceleration will give the velocity at each point
      HopVel = cumsum(HopVela)
      
      #Create a rolling average of Vel, 2 frame window (why?)
      HopVelavg = rollapply(HopVel, 2, mean)
      #insert 0 at the first row
      HopVelavg2 = insert(HopVelavg, 1, 0)
      #Calculate Velocity Area under the curve, using the time variable (500hz = 0.002 seconds)
      HopVelarea = HopVelavg2*0.002
      
      #Caluclate Displacement as the cumsum of Velocity Area
      HopDisp = cumsum(HopVelarea)
      
      #Define length of the Displacement data
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacement close to zero e.g. if Final displacement (l) is >0.0001, repeat the calculations above with HopVelInit -0.002, and continue until Final displacement is within 0.0001 of zero.
      #Conversely, do the opposite if Final displacement is <0.0001
      if (HopDisp[l]>0){
        while (HopDisp[l] > 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit - 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)}
      }else (HopDisp[l]<0) 
      {
        while (HopDisp[l] < 0.0001) {
          HopAccavg = rollapply(HopAcc, 2, mean)
          HopAccavg2 = insert(HopAccavg, 1, 0)
          HopAccarea = HopAccavg2*0.002
          HopVelInit = HopVelInit + 0.002
          HopVela = HopAccarea
          HopVela[1] = HopVelInit+HopAccarea[1]
          HopVel = cumsum(HopVela)
          HopVelavg = rollapply(HopVel, 2, mean)
          HopVelavg2 = insert(HopVelavg, 1, 0)
          HopVelarea = HopVelavg2*0.002
          HopDisp = cumsum(HopVelarea)
        }
      }
      
      
      #Calculate output variables
      
      ContactFrame <- 5 + which(Hop[5:length(Hop)] >10)[1]
      PeakHop[w] = max(Hop)
      PeakHopBW[w] = max(Hop)/BW
      ContactTimeHop[w] = (length(HopDisp) - ContactFrame)*0.002
      FlightTimeHop[w] = (length(HopDisp) - (length(HopDisp) - ContactFrame))*0.002
      MinHopDisp[w] = min(HopDisp)
      EccHopDisp[w] = (HopDisp[ContactFrame] - min(HopDisp))
      EccStiffHop[w] = ((max(Hop) - BW)/(HopDisp[ContactFrame] - min(HopDisp)))/input$Mass
      JumpFreqHop[w] = 1/(length(FullHops[[w]])*0.002)
      
    }
    
    #Convert Output variables to data.frame and round each value to 2 dp
    PeakHop_df = data.frame(PeakHop) %>% mutate_if(is.numeric, round, digits=2)
    PeakHopBW_df = data.frame(PeakHopBW) %>% mutate_if(is.numeric, round, digits=2)
    ContactTimeHop_df = data.frame(ContactTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    FlightTimeHop_df = data.frame(FlightTimeHop) %>% mutate_if(is.numeric, round, digits=2)
    MinHopDisp_df = data.frame(MinHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccHopDisp_df = data.frame(EccHopDisp) %>% mutate_if(is.numeric, round, digits=2)
    EccStiffHop_df = data.frame(EccStiffHop) %>% mutate_if(is.numeric, round, digits=2)
    JumpFreqHop_df = data.frame(JumpFreqHop) %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Calculate average of middle 5 hops as the "most reliable"
    avgEccCOMDisp = round(mean(EccHopDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[4:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[4:9]),digits=2)
    
    #create a variable listing the hopnumbers as Hop1, Hop2 etc, looping to q (number of hops from above)
    hopnumber = c()
    for(n in 1:q){
      hopnumber[n] <- paste0("Hop", n, sep = "")
    }
    #add to that a col header "Average"
    hopnumber[q+1] = "Average"
    #convert to data.frame for rbind below
    hopnumber = data.frame(hopnumber)
    
    #Create reactive variable that stays alive for combined final summary table
    values$RightSSAverages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
    #combine the data for each hop as rows and the average as the final row
    EccCOMDisp2 <-  rbind(EccHopDisp_df, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTimeHop_df, avgContactTime)
    FlightTime2 <-  rbind(FlightTimeHop_df, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffHop_df, avgEccStiffness)
    PeakForceBW2 <-  rbind(PeakHopBW_df, avgPeakForceBW)
    JumpFreq2 <-  rbind(JumpFreqHop_df, avgJumpFreq)
    
    #Create output data frame withe the hopnumber variable as the first column and data from above as columns
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForceBW2, JumpFreq2)
    
    #rename column names of output data frame (default is the variable name)
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    
    #define output file for Shiny
    OutputdfRightSS <-  as.data.frame(Output)
    
    ({OutputdfRightSS})
    
  })
  outputOptions(output, "resultstableRightSS", suspendWhenHidden = FALSE)
  
  output$resultstableSummary <- renderTable({
    rownames <- c("Left 1.7Hz", "Left 2.0Hz", "Left 2.3Hz", "Left 2.6Hz", "Left SSHz", "Right 1.7Hz", "Right 2.0Hz", "Right 2.3Hz", "Right 2.6Hz", "Right SSHz")
    OutputAverages <- rbind(values$Left1.7Averages, values$Left2.0Averages, values$Left2.3Averages, values$Left2.6Averages, values$LeftSSAverages, values$Right1.7Averages, values$Right2.0Averages, values$Right2.3Averages, values$Right2.6Averages, values$RightSSAverages)
    OutputSummarytable <-  cbind(rownames, OutputAverages)
    colnames(OutputSummarytable) <-  c("Trial", "EccCOMDisp","ContactTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    OutputdfAverages <- as.data.frame(OutputSummarytable)
    ({OutputdfAverages})
    
  })
  outputOptions(output, "resultstableSummary", suspendWhenHidden = TRUE)
}  