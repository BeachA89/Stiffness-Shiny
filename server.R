#Currently server500Hz


# changed freq to 0.002 from 0.002
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
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
        geom_vline(xintercept=Contact[12], col="red") +
        
        geom_vline(xintercept=Flight[13], col="green") +
        geom_vline(xintercept=Contact[13], col="red") #+
      
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
  
  output$resultstableLeft2 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left2$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotLeft2 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Left2Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfLeft2 <-  as.data.frame(Output)
    
    ({OutputdfLeft2})
    
    
  })
  outputOptions(output, "resultstableLeft2", suspendWhenHidden = FALSE)
  
  output$resultstableLeft3 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left3$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotLeft3 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Left3Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfLeft3 <-  as.data.frame(Output)
    
    ({OutputdfLeft3})
    
    
  })
  outputOptions(output, "resultstableLeft3", suspendWhenHidden = FALSE)
  
  output$resultstableLeft4 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left4$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotLeft4 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Left4Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfLeft4 <-  as.data.frame(Output)
    
    ({OutputdfLeft4})
    
    
  })
  outputOptions(output, "resultstableLeft4", suspendWhenHidden = FALSE)
  
  output$resultstableLeft5 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left5$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotLeft5 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Left5Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfLeft5 <-  as.data.frame(Output)
    
    ({OutputdfLeft5})
    
    
  })
  outputOptions(output, "resultstableLeft5", suspendWhenHidden = FALSE)
  
  output$resultstableLeft6 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left6$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotLeft6 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Left6Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfLeft6 <-  as.data.frame(Output)
    
    ({OutputdfLeft6})
    
    
  })
  outputOptions(output, "resultstableLeft6", suspendWhenHidden = FALSE)
  
  output$resultstableRight1 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right1$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRight1 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Right1Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfRight1 <-  as.data.frame(Output)
    
    ({OutputdfRight1})
    
    
  })
  outputOptions(output, "resultstableRight1", suspendWhenHidden = FALSE)
  
  output$resultstableRight2 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right2$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRight2 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Right2Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfRight2 <-  as.data.frame(Output)
    
    ({OutputdfRight2})
    
    
  })
  outputOptions(output, "resultstableRight2", suspendWhenHidden = FALSE)
  
  output$resultstableRight3 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right3$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRight3 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Right3Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfRight3 <-  as.data.frame(Output)
    
    ({OutputdfRight3})
    
    
  })
  outputOptions(output, "resultstableRight3", suspendWhenHidden = FALSE)
  
  output$resultstableRight4 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right4$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 10  # Number of frames that force must rise consecutively to confirm contact
    
    Contact <- Flight + which(
      sapply(Flight + 1:(length(Fz2$Fz2) - Flight), function(i) {
        segment <- Fz2$Fz2[i:(i + min_frames_rising - 1)]
        all(segment > 20) && all(diff(segment) > 0)
      })
    )[1]
    #zerooffset = mean(Fz2$Fz2[Flight:Contact])
    #Fz2$Fz2 = Fz2$Fz2 - zerooffset
    # Loop to detect and list all contact and flight instances
    Contact <- numeric(12)  # Preallocate for efficiency
    Flight <- numeric(12)   # Preallocate for efficiency
    
    # Initialize the first flight
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRight4 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Right4Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfRight4 <-  as.data.frame(Output)
    
    ({OutputdfRight4})
    
    
  })
  outputOptions(output, "resultstableRight4", suspendWhenHidden = FALSE)
  
  output$resultstableRight5 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right5$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRight5 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Right5Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfRight5 <-  as.data.frame(Output)
    
    ({OutputdfRight5})
    
    
  })
  outputOptions(output, "resultstableRight5", suspendWhenHidden = FALSE)
  
  output$resultstableRight6 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right6$datapath, stringsAsFactors = F))
    
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
    Flight <- which(Fz2$Fz2 < 20)[1]
    
    # Define the first contact instance after the first flight
    # Ensure the force surpasses 20 N, rises steadily, and stays above the threshold for several frames
    min_frames_rising <- 5  # Number of frames that force must rise consecutively to confirm contact
    
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
    Flight[1] <- which(Fz2$Fz2 < 20)[1]
    
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
      Flight[t] <- Contact[r] + which(Fz2$Fz2[Contact[r] + 1:length(Fz2$Fz2)] < 20)[1]
    }
    
    #this will create the overall plot, labeling each flight and contact instance - done 20 as above, shouldn't cause an error
    output$projectplotRight6 <- renderPlotly({
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
      
      ContactFrame <- 5 + which(
        sapply(5:(length(Hop) - min_frames_rising), function(i) {
          segment <- Hop[i:(i + min_frames_rising - 1)]
          all(segment > 20) && all(diff(segment) > 0)
        })
      )[1]
      
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
    avgEccCOMDisp = round(mean(EccHopDisp[5:9]),digits=2)
    avgContactTime = round(mean(ContactTimeHop[5:9]),digits=2)
    avgFlightTime = round(mean(FlightTimeHop[5:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffHop[5:9]),digits=2)
    avgPeakForceBW = round(mean(PeakHopBW[5:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreqHop[5:9]),digits=2)
    
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
    values$Right6Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForceBW, avgJumpFreq)
    
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
    OutputdfRight6 <-  as.data.frame(Output)
    
    ({OutputdfRight6})
    
    
  })
  outputOptions(output, "resultstableRight6", suspendWhenHidden = FALSE)
  
  output$resultstableSummary <- renderTable({
    rownames <- c("Left1", "Left2", "Left3", "Left4", "Left5", "Left6", "Right1", "Right2", "Right3", "Right4", "Right5", "Right6")
    OutputAverages <- rbind(values$Left1Averages, values$Left2Averages, values$Left3Averages, values$Left4Averages, values$Left5Averages, values$Left6Averages, values$Right1Averages, values$Right2Averages, values$Right3Averages, values$Right4Averages, values$Right5Averages, values$Right6Averages)
    OutputSummarytable <-  cbind(rownames, OutputAverages)
    colnames(OutputSummarytable) <-  c("Trial", "EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForceBW", "JumpFreq")
    OutputdfAverages <- as.data.frame(OutputSummarytable)
    ({OutputdfAverages})
    
  })
  outputOptions(output, "resultstableSummary", suspendWhenHidden = TRUE)
  
}  