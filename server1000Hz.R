# changed freq to 0.001 from 0.001
# changed flight threshold to 10N

# Define server logic to read selected file ----
server <- function(input, output) {
  values <- reactiveValues()
  output$Summaryhead <- renderText({
    "Averages"
  })
  output$resultstableLeft1.7 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left1.7$datapath, stringsAsFactors = F, skip = 17))
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <15)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >15)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <15) [1]
    
    for (r in 1:10){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >15)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <15)
    }
    
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
        geom_vline(xintercept=Contact[16], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(r in 1:9){
      t <- r+1
      FullHops[[r]] <- Fz2$Fz2[Contact[r]:Contact[t]]
    }
    
    Hops <- list()
    for(r in 1:9){
      t <- r+1
      Hops[[r]] <- Fz2$Fz2[Flight[r]:Flight[t]]
    }
    
    
    for (r in 1:9){
      
      #Hop1
      
      #Calculate Acceleration
      
      Hop <-  Hops[[r]]
      HopAcc <- (Hop-BW)/input$Mass
      
      
      
      
      HopAccavg = rollapply(HopAcc, 2, mean)
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Acceleration Area
      HopAccarea = HopAccavg2*0.001
      HopVelInit = 1.3
      
      #Velocity
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      HopVel = cumsum(HopVela)
      
      #Velocity Area
      HopVelavg = rollapply(HopVel, 2, mean)
      HopVelavg2 = insert(HopVelavg, 1, 0)
      HopVelarea = HopVelavg2*0.001
      
      #Displacement
      HopDisp = cumsum(HopVelarea)
      
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacment close to zero
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
      
      assign(paste0("PeakHop", r, sep = ""), max(Hop))
      assign(paste0("PeakHopPC", r, sep = ""), max(Hop)/BW)
      
      
      
      
      ContactHop <- 5 + which(Hop[5:length(Hop)] >15)[1]
      
      assign(paste0("ContactTimeHop", r, sep = ""), (length(HopDisp) - ContactHop)*0.001)
      
      assign(paste0("FlightTimeHop", r, sep = ""), (length(HopDisp) - (length(HopDisp) - ContactHop))*0.001)
      
      assign(paste0("MinHopDisp", r, sep = ""), min(HopDisp))
      
      assign(paste0("EccHopDisp", r, sep = ""), (HopDisp[ContactHop] - min(HopDisp)))
      
      assign(paste0("EccStiffHop", r, sep = ""), ((max(Hop) - BW)/(HopDisp[ContactHop] - min(HopDisp))/input$Mass))
      
      
      assign(paste0("JumpFreqHop", r, sep = ""), 1/(length(FullHops[[r]])*0.001))
      
      
    }
    
    
    
    #########################
    
    
    # hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14","Hop15", "Average")
    # EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2), round(EccHopDisp10,digits=2), round(EccHopDisp11,digits=2), round(EccHopDisp12,digits=2), round(EccHopDisp13,digits=2), round(EccHopDisp14,digits=2), round(EccHopDisp15,digits=2))
    # ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2), round(ContactTimeHop15,digits=2))
    # EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2), round(EccStiffHop15,digits=2))
    # PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2), round(PeakHopPC10,digits=2), round(PeakHopPC11,digits=2), round(PeakHopPC12,digits=2), round(PeakHopPC13,digits=2), round(PeakHopPC14,digits=2), round(PeakHopPC15,digits=2))
    # JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2), round(JumpFreqHop15,digits=2))
    # 
    # avgEccCOMDisp = round(mean(EccCOMDisp[4:15]),digits=2)
    # avgContactTime = round(mean(ContactTime[4:15]),digits=2)
    # avgEccStiffness = round(mean(EccStiffness[4:15]),digits=2)
    # avgPeakForcePC = round(mean(PeakForcePC[4:15]),digits=2)
    # avgJumpFreq = round(mean(JumpFreq[4:15]),digits=2)
    
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13", "Average")
    EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2))
    FlightTime <- rbind(round(FlightTimeHop1,digits=2), round(FlightTimeHop2,digits=2), round(FlightTimeHop3,digits=2), round(FlightTimeHop4,digits=2), round(FlightTimeHop5,digits=2), round(FlightTimeHop6,digits=2), round(FlightTimeHop7,digits=2), round(FlightTimeHop8,digits=2), round(FlightTimeHop9,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2))
    PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp[4:9]),digits=2)
    avgContactTime = round(mean(ContactTime[4:9]),digits=2)
    avgFlightTime = round(mean(FlightTime[4:9]),digits=2)
    avgEccStiffness = round(mean(EccStiffness[4:9]),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC[4:9]),digits=2)
    avgJumpFreq = round(mean(JumpFreq[4:9]),digits=2)
    
    values$Left1.7Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgFlightTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    FlightTime2 <-  rbind(FlightTime, avgFlightTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, FlightTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "FlightTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfLeft1.7 <-  as.data.frame(Output)
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    # 
    
    
    ({OutputdfLeft1.7})
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    #     
  })
  outputOptions(output, "resultstableLeft1.7", suspendWhenHidden = FALSE)
  
  
  output$resultstableLeft2.0 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left2.0$datapath, stringsAsFactors = F, skip = 17))
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <15)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >15)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <15) [1]
    
    for (r in 1:16){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >15)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <15)
    }
    
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
    })
    outputOptions(output, "projectplotLeft2.0", suspendWhenHidden = FALSE)
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(r in 1:15){
      t <- r+1
      FullHops[[r]] <- Fz2$Fz2[Contact[r]:Contact[t]]
    }
    
    Hops <- list()
    for(r in 1:15){
      t <- r+1
      Hops[[r]] <- Fz2$Fz2[Flight[r]:Flight[t]]
    }
    
    
    for (r in 1:15){
      
      #Hop1
      
      #Calculate Acceleration
      
      Hop <-  Hops[[r]]
      HopAcc <- (Hop-BW)/input$Mass
      
      
      
      
      HopAccavg = rollapply(HopAcc, 2, mean)
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Acceleration Area
      HopAccarea = HopAccavg2*0.001
      HopVelInit = 1.3
      
      #Velocity
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      HopVel = cumsum(HopVela)
      
      #Velocity Area
      HopVelavg = rollapply(HopVel, 2, mean)
      HopVelavg2 = insert(HopVelavg, 1, 0)
      HopVelarea = HopVelavg2*0.001
      
      #Displacement
      HopDisp = cumsum(HopVelarea)
      
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacment close to zero
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
      
      assign(paste0("PeakHop", r, sep = ""), max(Hop))
      assign(paste0("PeakHopPC", r, sep = ""), max(Hop)/BW)
      
      
      
      
      ContactHop <- 5 + which(Hop[5:length(Hop)] >15)[1]
      
      assign(paste0("ContactTimeHop", r, sep = ""), (length(HopDisp) - ContactHop)*0.001)
      
      assign(paste0("MinHopDisp", r, sep = ""), min(HopDisp))
      
      assign(paste0("EccHopDisp", r, sep = ""), (HopDisp[ContactHop] - min(HopDisp)))
      
      assign(paste0("EccStiffHop", r, sep = ""), ((max(Hop) - BW)/(HopDisp[ContactHop] - min(HopDisp))/input$Mass))
      
      
      assign(paste0("JumpFreqHop", r, sep = ""), 1/(length(FullHops[[r]])*0.001))
      
      
    }
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14","Hop15", "Average")
    EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2), round(EccHopDisp10,digits=2), round(EccHopDisp11,digits=2), round(EccHopDisp12,digits=2), round(EccHopDisp13,digits=2), round(EccHopDisp14,digits=2), round(EccHopDisp15,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2), round(ContactTimeHop15,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2), round(EccStiffHop15,digits=2))
    PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2), round(PeakHopPC10,digits=2), round(PeakHopPC11,digits=2), round(PeakHopPC12,digits=2), round(PeakHopPC13,digits=2), round(PeakHopPC14,digits=2), round(PeakHopPC15,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2), round(JumpFreqHop15,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    values$Left2.0Averages <- cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfLeft2.0 <-  as.data.frame(Output)
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    # 
    
    
    ({OutputdfLeft2.0})
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    #     
  })
  outputOptions(output, "resultstableLeft2.0", suspendWhenHidden = FALSE)
  output$resultstableLeft2.3 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left2.3$datapath, stringsAsFactors = F, skip = 17))
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <15)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >15)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <15) [1]
    
    for (r in 1:16){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >15)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <15)
    }
    
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
    })
    outputOptions(output, "projectplotLeft2.3", suspendWhenHidden = FALSE)    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(r in 1:15){
      t <- r+1
      FullHops[[r]] <- Fz2$Fz2[Contact[r]:Contact[t]]
    }
    
    Hops <- list()
    for(r in 1:15){
      t <- r+1
      Hops[[r]] <- Fz2$Fz2[Flight[r]:Flight[t]]
    }
    
    
    for (r in 1:15){
      
      #Hop1
      
      #Calculate Acceleration
      
      Hop <-  Hops[[r]]
      HopAcc <- (Hop-BW)/input$Mass
      
      
      
      
      HopAccavg = rollapply(HopAcc, 2, mean)
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Acceleration Area
      HopAccarea = HopAccavg2*0.001
      HopVelInit = 1.3
      
      #Velocity
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      HopVel = cumsum(HopVela)
      
      #Velocity Area
      HopVelavg = rollapply(HopVel, 2, mean)
      HopVelavg2 = insert(HopVelavg, 1, 0)
      HopVelarea = HopVelavg2*0.001
      
      #Displacement
      HopDisp = cumsum(HopVelarea)
      
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacment close to zero
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
      
      assign(paste0("PeakHop", r, sep = ""), max(Hop))
      assign(paste0("PeakHopPC", r, sep = ""), max(Hop)/BW)
      
      
      
      
      ContactHop <- 5 + which(Hop[5:length(Hop)] >15)[1]
      
      assign(paste0("ContactTimeHop", r, sep = ""), (length(HopDisp) - ContactHop)*0.001)
      
      assign(paste0("MinHopDisp", r, sep = ""), min(HopDisp))
      
      assign(paste0("EccHopDisp", r, sep = ""), (HopDisp[ContactHop] - min(HopDisp)))
      
      assign(paste0("EccStiffHop", r, sep = ""), ((max(Hop) - BW)/(HopDisp[ContactHop] - min(HopDisp))/input$Mass))
      
      
      assign(paste0("JumpFreqHop", r, sep = ""), 1/(length(FullHops[[r]])*0.001))
      
      
    }
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14","Hop15", "Average")
    EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2), round(EccHopDisp10,digits=2), round(EccHopDisp11,digits=2), round(EccHopDisp12,digits=2), round(EccHopDisp13,digits=2), round(EccHopDisp14,digits=2), round(EccHopDisp15,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2), round(ContactTimeHop15,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2), round(EccStiffHop15,digits=2))
    PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2), round(PeakHopPC10,digits=2), round(PeakHopPC11,digits=2), round(PeakHopPC12,digits=2), round(PeakHopPC13,digits=2), round(PeakHopPC14,digits=2), round(PeakHopPC15,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2), round(JumpFreqHop15,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    values$Left2.3Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfLeft2.3 <-  as.data.frame(Output)
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    # 
    
    
    ({OutputdfLeft2.3})
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    #     
  })
  outputOptions(output, "resultstableLeft2.3", suspendWhenHidden = FALSE)
  output$resultstableLeft2.6 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Left2.6$datapath, stringsAsFactors = F, skip = 17))
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <15)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >15)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <15) [1]
    
    for (r in 1:16){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >15)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <15)
    }
    
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
    })
    outputOptions(output, "projectplotLeft2.6", suspendWhenHidden = FALSE)    
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(r in 1:15){
      t <- r+1
      FullHops[[r]] <- Fz2$Fz2[Contact[r]:Contact[t]]
    }
    
    Hops <- list()
    for(r in 1:15){
      t <- r+1
      Hops[[r]] <- Fz2$Fz2[Flight[r]:Flight[t]]
    }
    
    
    for (r in 1:15){
      
      #Hop1
      
      #Calculate Acceleration
      
      Hop <-  Hops[[r]]
      HopAcc <- (Hop-BW)/input$Mass
      
      
      
      
      HopAccavg = rollapply(HopAcc, 2, mean)
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Acceleration Area
      HopAccarea = HopAccavg2*0.001
      HopVelInit = 1.3
      
      #Velocity
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      HopVel = cumsum(HopVela)
      
      #Velocity Area
      HopVelavg = rollapply(HopVel, 2, mean)
      HopVelavg2 = insert(HopVelavg, 1, 0)
      HopVelarea = HopVelavg2*0.001
      
      #Displacement
      HopDisp = cumsum(HopVelarea)
      
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacment close to zero
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
      
      assign(paste0("PeakHop", r, sep = ""), max(Hop))
      assign(paste0("PeakHopPC", r, sep = ""), max(Hop)/BW)
      
      
      
      
      ContactHop <- 5 + which(Hop[5:length(Hop)] >15)[1]
      
      assign(paste0("ContactTimeHop", r, sep = ""), (length(HopDisp) - ContactHop)*0.001)
      
      assign(paste0("MinHopDisp", r, sep = ""), min(HopDisp))
      
      assign(paste0("EccHopDisp", r, sep = ""), (HopDisp[ContactHop] - min(HopDisp)))
      
      assign(paste0("EccStiffHop", r, sep = ""), ((max(Hop) - BW)/(HopDisp[ContactHop] - min(HopDisp))/input$Mass))
      
      
      assign(paste0("JumpFreqHop", r, sep = ""), 1/(length(FullHops[[r]])*0.001))
      
      
    }
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14","Hop15", "Average")
    EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2), round(EccHopDisp10,digits=2), round(EccHopDisp11,digits=2), round(EccHopDisp12,digits=2), round(EccHopDisp13,digits=2), round(EccHopDisp14,digits=2), round(EccHopDisp15,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2), round(ContactTimeHop15,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2), round(EccStiffHop15,digits=2))
    PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2), round(PeakHopPC10,digits=2), round(PeakHopPC11,digits=2), round(PeakHopPC12,digits=2), round(PeakHopPC13,digits=2), round(PeakHopPC14,digits=2), round(PeakHopPC15,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2), round(JumpFreqHop15,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    values$Left2.6Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfLeft2.6 <-  as.data.frame(Output)
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    # 
    
    
    ({OutputdfLeft2.6})
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    #     
  })
  outputOptions(output, "resultstableLeft2.6", suspendWhenHidden = FALSE)
  output$resultstableLeftSS <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$LeftSS$datapath, stringsAsFactors = F, skip = 17))
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <15)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >15)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <15) [1]
    
    for (r in 1:16){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >15)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <15)
    }
    
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
    })
    outputOptions(output, "projectplotLeftSS", suspendWhenHidden = FALSE)    
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(r in 1:15){
      t <- r+1
      FullHops[[r]] <- Fz2$Fz2[Contact[r]:Contact[t]]
    }
    
    Hops <- list()
    for(r in 1:15){
      t <- r+1
      Hops[[r]] <- Fz2$Fz2[Flight[r]:Flight[t]]
    }
    
    
    for (r in 1:15){
      
      #Hop1
      
      #Calculate Acceleration
      
      Hop <-  Hops[[r]]
      HopAcc <- (Hop-BW)/input$Mass
      
      
      
      
      HopAccavg = rollapply(HopAcc, 2, mean)
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Acceleration Area
      HopAccarea = HopAccavg2*0.001
      HopVelInit = 1.3
      
      #Velocity
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      HopVel = cumsum(HopVela)
      
      #Velocity Area
      HopVelavg = rollapply(HopVel, 2, mean)
      HopVelavg2 = insert(HopVelavg, 1, 0)
      HopVelarea = HopVelavg2*0.001
      
      #Displacement
      HopDisp = cumsum(HopVelarea)
      
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacment close to zero
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
      
      assign(paste0("PeakHop", r, sep = ""), max(Hop))
      assign(paste0("PeakHopPC", r, sep = ""), max(Hop)/BW)
      
      
      
      
      ContactHop <- 5 + which(Hop[5:length(Hop)] >15)[1]
      
      assign(paste0("ContactTimeHop", r, sep = ""), (length(HopDisp) - ContactHop)*0.001)
      
      assign(paste0("MinHopDisp", r, sep = ""), min(HopDisp))
      
      assign(paste0("EccHopDisp", r, sep = ""), (HopDisp[ContactHop] - min(HopDisp)))
      
      assign(paste0("EccStiffHop", r, sep = ""), ((max(Hop) - BW)/(HopDisp[ContactHop] - min(HopDisp))/input$Mass))
      
      
      assign(paste0("JumpFreqHop", r, sep = ""), 1/(length(FullHops[[r]])*0.001))
      
      
    }
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14","Hop15", "Average")
    EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2), round(EccHopDisp10,digits=2), round(EccHopDisp11,digits=2), round(EccHopDisp12,digits=2), round(EccHopDisp13,digits=2), round(EccHopDisp14,digits=2), round(EccHopDisp15,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2), round(ContactTimeHop15,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2), round(EccStiffHop15,digits=2))
    PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2), round(PeakHopPC10,digits=2), round(PeakHopPC11,digits=2), round(PeakHopPC12,digits=2), round(PeakHopPC13,digits=2), round(PeakHopPC14,digits=2), round(PeakHopPC15,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2), round(JumpFreqHop15,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    values$LeftSSAverages <- cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfLeftSS <-  as.data.frame(Output)
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    # 
    
    
    ({OutputdfLeftSS})
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    #     
  })
  outputOptions(output, "resultstableLeftSS", suspendWhenHidden = FALSE)
  output$resultstableRight1.7 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right1.7$datapath, stringsAsFactors = F, skip = 17))
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <15)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >15)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <15) [1]
    
    for (r in 1:16){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >15)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <15)
    }
    
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
    })
    outputOptions(output, "projectplotRight1.7", suspendWhenHidden = FALSE)    
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(r in 1:15){
      t <- r+1
      FullHops[[r]] <- Fz2$Fz2[Contact[r]:Contact[t]]
    }
    
    Hops <- list()
    for(r in 1:15){
      t <- r+1
      Hops[[r]] <- Fz2$Fz2[Flight[r]:Flight[t]]
    }
    
    
    for (r in 1:15){
      
      #Hop1
      
      #Calculate Acceleration
      
      Hop <-  Hops[[r]]
      HopAcc <- (Hop-BW)/input$Mass
      
      
      
      
      HopAccavg = rollapply(HopAcc, 2, mean)
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Acceleration Area
      HopAccarea = HopAccavg2*0.001
      HopVelInit = 1.3
      
      #Velocity
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      HopVel = cumsum(HopVela)
      
      #Velocity Area
      HopVelavg = rollapply(HopVel, 2, mean)
      HopVelavg2 = insert(HopVelavg, 1, 0)
      HopVelarea = HopVelavg2*0.001
      
      #Displacement
      HopDisp = cumsum(HopVelarea)
      
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacment close to zero
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
      
      assign(paste0("PeakHop", r, sep = ""), max(Hop))
      assign(paste0("PeakHopPC", r, sep = ""), max(Hop)/BW)
      
      
      
      
      ContactHop <- 5 + which(Hop[5:length(Hop)] >15)[1]
      
      assign(paste0("ContactTimeHop", r, sep = ""), (length(HopDisp) - ContactHop)*0.001)
      
      assign(paste0("MinHopDisp", r, sep = ""), min(HopDisp))
      
      assign(paste0("EccHopDisp", r, sep = ""), (HopDisp[ContactHop] - min(HopDisp)))
      
      assign(paste0("EccStiffHop", r, sep = ""), ((max(Hop) - BW)/(HopDisp[ContactHop] - min(HopDisp))/input$Mass))
      
      
      assign(paste0("JumpFreqHop", r, sep = ""), 1/(length(FullHops[[r]])*0.001))
      
      
    }
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14","Hop15", "Average")
    EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2), round(EccHopDisp10,digits=2), round(EccHopDisp11,digits=2), round(EccHopDisp12,digits=2), round(EccHopDisp13,digits=2), round(EccHopDisp14,digits=2), round(EccHopDisp15,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2), round(ContactTimeHop15,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2), round(EccStiffHop15,digits=2))
    PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2), round(PeakHopPC10,digits=2), round(PeakHopPC11,digits=2), round(PeakHopPC12,digits=2), round(PeakHopPC13,digits=2), round(PeakHopPC14,digits=2), round(PeakHopPC15,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2), round(JumpFreqHop15,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    values$Right1.7Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfRight1.7 <-  as.data.frame(Output)
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    # 
    
    
    ({OutputdfRight1.7})
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    #     
  })
  outputOptions(output, "resultstableRight1.7", suspendWhenHidden = FALSE)
  output$resultstableRight2.0 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right2.0$datapath, stringsAsFactors = F, skip = 17))
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <15)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >15)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <15) [1]
    
    for (r in 1:16){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >15)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <15)
    }
    
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
    })
    outputOptions(output, "projectplotRight2.0", suspendWhenHidden = FALSE)    
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(r in 1:15){
      t <- r+1
      FullHops[[r]] <- Fz2$Fz2[Contact[r]:Contact[t]]
    }
    
    Hops <- list()
    for(r in 1:15){
      t <- r+1
      Hops[[r]] <- Fz2$Fz2[Flight[r]:Flight[t]]
    }
    
    
    for (r in 1:15){
      
      #Hop1
      
      #Calculate Acceleration
      
      Hop <-  Hops[[r]]
      HopAcc <- (Hop-BW)/input$Mass
      
      
      
      
      HopAccavg = rollapply(HopAcc, 2, mean)
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Acceleration Area
      HopAccarea = HopAccavg2*0.001
      HopVelInit = 1.3
      
      #Velocity
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      HopVel = cumsum(HopVela)
      
      #Velocity Area
      HopVelavg = rollapply(HopVel, 2, mean)
      HopVelavg2 = insert(HopVelavg, 1, 0)
      HopVelarea = HopVelavg2*0.001
      
      #Displacement
      HopDisp = cumsum(HopVelarea)
      
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacment close to zero
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
      
      assign(paste0("PeakHop", r, sep = ""), max(Hop))
      assign(paste0("PeakHopPC", r, sep = ""), max(Hop)/BW)
      
      
      
      
      ContactHop <- 5 + which(Hop[5:length(Hop)] >15)[1]
      
      assign(paste0("ContactTimeHop", r, sep = ""), (length(HopDisp) - ContactHop)*0.001)
      
      assign(paste0("MinHopDisp", r, sep = ""), min(HopDisp))
      
      assign(paste0("EccHopDisp", r, sep = ""), (HopDisp[ContactHop] - min(HopDisp)))
      
      assign(paste0("EccStiffHop", r, sep = ""), ((max(Hop) - BW)/(HopDisp[ContactHop] - min(HopDisp))/input$Mass))
      
      
      assign(paste0("JumpFreqHop", r, sep = ""), 1/(length(FullHops[[r]])*0.001))
      
      
    }
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14","Hop15", "Average")
    EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2), round(EccHopDisp10,digits=2), round(EccHopDisp11,digits=2), round(EccHopDisp12,digits=2), round(EccHopDisp13,digits=2), round(EccHopDisp14,digits=2), round(EccHopDisp15,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2), round(ContactTimeHop15,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2), round(EccStiffHop15,digits=2))
    PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2), round(PeakHopPC10,digits=2), round(PeakHopPC11,digits=2), round(PeakHopPC12,digits=2), round(PeakHopPC13,digits=2), round(PeakHopPC14,digits=2), round(PeakHopPC15,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2), round(JumpFreqHop15,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    values$Right2.0Averages <- cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfRight2.0 <-  as.data.frame(Output)
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    # 
    
    
    ({OutputdfRight2.0})
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    #     
  })
  outputOptions(output, "resultstableRight2.0", suspendWhenHidden = FALSE)
  output$resultstableRight2.3 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right2.3$datapath, stringsAsFactors = F, skip = 17))
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <15)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >15)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <15) [1]
    
    for (r in 1:16){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >15)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <15)
    }
    
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
    })
    outputOptions(output, "projectplotRight2.3", suspendWhenHidden = FALSE)    
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(r in 1:15){
      t <- r+1
      FullHops[[r]] <- Fz2$Fz2[Contact[r]:Contact[t]]
    }
    
    Hops <- list()
    for(r in 1:15){
      t <- r+1
      Hops[[r]] <- Fz2$Fz2[Flight[r]:Flight[t]]
    }
    
    
    for (r in 1:15){
      
      #Hop1
      
      #Calculate Acceleration
      
      Hop <-  Hops[[r]]
      HopAcc <- (Hop-BW)/input$Mass
      
      
      
      
      HopAccavg = rollapply(HopAcc, 2, mean)
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Acceleration Area
      HopAccarea = HopAccavg2*0.001
      HopVelInit = 1.3
      
      #Velocity
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      HopVel = cumsum(HopVela)
      
      #Velocity Area
      HopVelavg = rollapply(HopVel, 2, mean)
      HopVelavg2 = insert(HopVelavg, 1, 0)
      HopVelarea = HopVelavg2*0.001
      
      #Displacement
      HopDisp = cumsum(HopVelarea)
      
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacment close to zero
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
      
      assign(paste0("PeakHop", r, sep = ""), max(Hop))
      assign(paste0("PeakHopPC", r, sep = ""), max(Hop)/BW)
      
      
      
      
      ContactHop <- 5 + which(Hop[5:length(Hop)] >15)[1]
      
      assign(paste0("ContactTimeHop", r, sep = ""), (length(HopDisp) - ContactHop)*0.001)
      
      assign(paste0("MinHopDisp", r, sep = ""), min(HopDisp))
      
      assign(paste0("EccHopDisp", r, sep = ""), (HopDisp[ContactHop] - min(HopDisp)))
      
      assign(paste0("EccStiffHop", r, sep = ""), ((max(Hop) - BW)/(HopDisp[ContactHop] - min(HopDisp))/input$Mass))
      
      
      assign(paste0("JumpFreqHop", r, sep = ""), 1/(length(FullHops[[r]])*0.001))
      
      
    }
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14","Hop15", "Average")
    EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2), round(EccHopDisp10,digits=2), round(EccHopDisp11,digits=2), round(EccHopDisp12,digits=2), round(EccHopDisp13,digits=2), round(EccHopDisp14,digits=2), round(EccHopDisp15,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2), round(ContactTimeHop15,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2), round(EccStiffHop15,digits=2))
    PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2), round(PeakHopPC10,digits=2), round(PeakHopPC11,digits=2), round(PeakHopPC12,digits=2), round(PeakHopPC13,digits=2), round(PeakHopPC14,digits=2), round(PeakHopPC15,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2), round(JumpFreqHop15,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    values$Right2.3Averages <- cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfRight2.3 <-  as.data.frame(Output)
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    # 
    
    
    ({OutputdfRight2.3})
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    #     
  })
  outputOptions(output, "resultstableRight2.3", suspendWhenHidden = FALSE)
  output$resultstableRight2.6 <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$Right2.6$datapath, stringsAsFactors = F, skip = 17))
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <15)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >15)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <15) [1]
    
    for (r in 1:16){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >15)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <15)
    }
    
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
    })
    outputOptions(output, "projectplotRight2.6", suspendWhenHidden = FALSE)    
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(r in 1:15){
      t <- r+1
      FullHops[[r]] <- Fz2$Fz2[Contact[r]:Contact[t]]
    }
    
    Hops <- list()
    for(r in 1:15){
      t <- r+1
      Hops[[r]] <- Fz2$Fz2[Flight[r]:Flight[t]]
    }
    
    
    for (r in 1:15){
      
      #Hop1
      
      #Calculate Acceleration
      
      Hop <-  Hops[[r]]
      HopAcc <- (Hop-BW)/input$Mass
      
      
      
      
      HopAccavg = rollapply(HopAcc, 2, mean)
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Acceleration Area
      HopAccarea = HopAccavg2*0.001
      HopVelInit = 1.3
      
      #Velocity
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      HopVel = cumsum(HopVela)
      
      #Velocity Area
      HopVelavg = rollapply(HopVel, 2, mean)
      HopVelavg2 = insert(HopVelavg, 1, 0)
      HopVelarea = HopVelavg2*0.001
      
      #Displacement
      HopDisp = cumsum(HopVelarea)
      
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacment close to zero
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
      
      assign(paste0("PeakHop", r, sep = ""), max(Hop))
      assign(paste0("PeakHopPC", r, sep = ""), max(Hop)/BW)
      
      
      
      
      ContactHop <- 5 + which(Hop[5:length(Hop)] >15)[1]
      
      assign(paste0("ContactTimeHop", r, sep = ""), (length(HopDisp) - ContactHop)*0.001)
      
      assign(paste0("MinHopDisp", r, sep = ""), min(HopDisp))
      
      assign(paste0("EccHopDisp", r, sep = ""), (HopDisp[ContactHop] - min(HopDisp)))
      
      assign(paste0("EccStiffHop", r, sep = ""), ((max(Hop) - BW)/(HopDisp[ContactHop] - min(HopDisp))/input$Mass))
      
      
      assign(paste0("JumpFreqHop", r, sep = ""), 1/(length(FullHops[[r]])*0.001))
      
      
    }
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14","Hop15", "Average")
    EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2), round(EccHopDisp10,digits=2), round(EccHopDisp11,digits=2), round(EccHopDisp12,digits=2), round(EccHopDisp13,digits=2), round(EccHopDisp14,digits=2), round(EccHopDisp15,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2), round(ContactTimeHop15,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2), round(EccStiffHop15,digits=2))
    PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2), round(PeakHopPC10,digits=2), round(PeakHopPC11,digits=2), round(PeakHopPC12,digits=2), round(PeakHopPC13,digits=2), round(PeakHopPC14,digits=2), round(PeakHopPC15,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2), round(JumpFreqHop15,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    values$Right2.6Averages <-  cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfRight2.6 <-  as.data.frame(Output)
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    # 
    
    
    ({OutputdfRight2.6})
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    #     
  })
  outputOptions(output, "resultstableRight2.6", suspendWhenHidden = FALSE)
  output$resultstableRightSS <- renderTable({
    input$goButton
    
    data1 <- isolate(
      read.csv(input$RightSS$datapath, stringsAsFactors = F, skip = 17))
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <15)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >15)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <15) [1]
    
    for (r in 1:16){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >15)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <15)
    }
    
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
    })
    outputOptions(output, "projectplotRightSS", suspendWhenHidden = FALSE)    
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHops <- list()
    for(r in 1:15){
      t <- r+1
      FullHops[[r]] <- Fz2$Fz2[Contact[r]:Contact[t]]
    }
    
    Hops <- list()
    for(r in 1:15){
      t <- r+1
      Hops[[r]] <- Fz2$Fz2[Flight[r]:Flight[t]]
    }
    
    
    for (r in 1:15){
      
      #Hop1
      
      #Calculate Acceleration
      
      Hop <-  Hops[[r]]
      HopAcc <- (Hop-BW)/input$Mass
      
      
      
      
      HopAccavg = rollapply(HopAcc, 2, mean)
      HopAccavg2 = insert(HopAccavg, 1, 0)
      
      #Acceleration Area
      HopAccarea = HopAccavg2*0.001
      HopVelInit = 1.3
      
      #Velocity
      HopVela = HopAccarea
      HopVela[1] = HopVelInit+HopAccarea[1]
      HopVel = cumsum(HopVela)
      
      #Velocity Area
      HopVelavg = rollapply(HopVel, 2, mean)
      HopVelavg2 = insert(HopVelavg, 1, 0)
      HopVelarea = HopVelavg2*0.001
      
      #Displacement
      HopDisp = cumsum(HopVelarea)
      
      l = length(HopDisp)
      
      #Adjust Init Velocity to get Final Displacment close to zero
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
      
      assign(paste0("PeakHop", r, sep = ""), max(Hop))
      assign(paste0("PeakHopPC", r, sep = ""), max(Hop)/BW)
      
      
      
      
      ContactHop <- 5 + which(Hop[5:length(Hop)] >15)[1]
      
      assign(paste0("ContactTimeHop", r, sep = ""), (length(HopDisp) - ContactHop)*0.001)
      
      assign(paste0("MinHopDisp", r, sep = ""), min(HopDisp))
      
      assign(paste0("EccHopDisp", r, sep = ""), (HopDisp[ContactHop] - min(HopDisp)))
      
      assign(paste0("EccStiffHop", r, sep = ""), ((max(Hop) - BW)/(HopDisp[ContactHop] - min(HopDisp))/input$Mass))
      
      
      assign(paste0("JumpFreqHop", r, sep = ""), 1/(length(FullHops[[r]])*0.001))
      
      
    }
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14","Hop15", "Average")
    EccCOMDisp <- rbind(round(EccHopDisp1,digits=2), round(EccHopDisp2,digits=2), round(EccHopDisp3,digits=2), round(EccHopDisp4,digits=2), round(EccHopDisp5,digits=2), round(EccHopDisp6,digits=2), round(EccHopDisp7,digits=2), round(EccHopDisp8,digits=2), round(EccHopDisp9,digits=2), round(EccHopDisp10,digits=2), round(EccHopDisp11,digits=2), round(EccHopDisp12,digits=2), round(EccHopDisp13,digits=2), round(EccHopDisp14,digits=2), round(EccHopDisp15,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2), round(ContactTimeHop15,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2), round(EccStiffHop15,digits=2))
    PeakForcePC <- rbind(round(PeakHopPC1,digits=2), round(PeakHopPC2,digits=2), round(PeakHopPC3,digits=2), round(PeakHopPC4,digits=2), round(PeakHopPC5,digits=2), round(PeakHopPC6,digits=2), round(PeakHopPC7,digits=2), round(PeakHopPC8,digits=2), round(PeakHopPC9,digits=2), round(PeakHopPC10,digits=2), round(PeakHopPC11,digits=2), round(PeakHopPC12,digits=2), round(PeakHopPC13,digits=2), round(PeakHopPC14,digits=2), round(PeakHopPC15,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2), round(JumpFreqHop15,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    values$RightSSAverages <-  cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfRightSS <-  as.data.frame(Output)
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    # 
    
    
    ({OutputdfRightSS})
    # 
    # dir = choose.dir(default = "", caption = "Select folder")
    # file <- paste(dir, "/", Trial, ".xlsx", sep="")
    # 
    # write.xlsx(Output, file=file, sheetName=Trial, append=FALSE) 
    #     
  })
  outputOptions(output, "resultstableRightSS", suspendWhenHidden = FALSE)
  
  output$resultstableSummary <- renderTable({
    rownames <- c("Left 1.7Hz", "Left 2.0Hz", "Left 2.3Hz", "Left 2.6Hz", "Left SSHz", "Right 1.7Hz", "Right 2.0Hz", "Right 2.3Hz", "Right 2.6Hz", "Right SSHz")
    OutputAverages <- rbind(values$Left1.7Averages, values$Left2.0Averages, values$Left2.3Averages, values$Left2.6Averages, values$LeftSSAverages, values$Right1.7Averages, values$Right2.0Averages, values$Right2.3Averages, values$Right2.6Averages, values$RightSSAverages)
    OutputSummarytable <-  cbind(rownames, OutputAverages)
    colnames(OutputSummarytable) <-  c("Trial", "EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfAverages <- as.data.frame(OutputSummarytable)
    ({OutputdfAverages})
    
  })
  outputOptions(output, "resultstableSummary", suspendWhenHidden = TRUE)
}  