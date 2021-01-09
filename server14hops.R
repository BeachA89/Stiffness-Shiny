# Define server logic to read selected file ----
server <- function(input, output) {
  output$resultstableLeft1.7 <- renderTable({
    
    data1 <- 
      read.csv(input$Left1.7$datapath, stringsAsFactors = F, skip = 17)
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <5)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    for (r in 1:15){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <5)
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
        geom_vline(xintercept=Contact[15], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHop2 <- Fz2$Fz2[Contact[2]:Contact[3]]
    FullHop3 <- Fz2$Fz2[Contact[3]:Contact[4]]
    FullHop4 <- Fz2$Fz2[Contact[4]:Contact[5]]
    FullHop5 <- Fz2$Fz2[Contact[5]:Contact[6]]
    FullHop6 <- Fz2$Fz2[Contact[6]:Contact[7]]
    FullHop7 <- Fz2$Fz2[Contact[7]:Contact[8]]
    FullHop8 <- Fz2$Fz2[Contact[8]:Contact[9]]
    FullHop9 <- Fz2$Fz2[Contact[9]:Contact[10]]
    FullHop10 <- Fz2$Fz2[Contact[10]:Contact[11]]
    FullHop11 <- Fz2$Fz2[Contact[11]:Contact[12]]
    FullHop12 <- Fz2$Fz2[Contact[12]:Contact[13]]
    FullHop13 <- Fz2$Fz2[Contact[13]:Contact[14]]
    FullHop14 <- Fz2$Fz2[Contact[14]:Contact[15]]
    #    FullHop15 <- Fz2$Fz2[Contact[15]:Contact[16]]
    
    Hop1 <- Fz2$Fz2[Flight[1]:Flight[2]]
    Hop2 <- Fz2$Fz2[Flight[2]:Flight[3]]
    Hop3 <- Fz2$Fz2[Flight[3]:Flight[4]]
    Hop4 <- Fz2$Fz2[Flight[4]:Flight[5]]
    Hop5 <- Fz2$Fz2[Flight[5]:Flight[6]]
    Hop6 <- Fz2$Fz2[Flight[6]:Flight[7]]
    Hop7 <- Fz2$Fz2[Flight[7]:Flight[8]]
    Hop8 <- Fz2$Fz2[Flight[8]:Flight[9]]
    Hop9 <- Fz2$Fz2[Flight[9]:Flight[10]]
    Hop10 <- Fz2$Fz2[Flight[10]:Flight[11]]
    Hop11 <- Fz2$Fz2[Flight[11]:Flight[12]]
    Hop12 <- Fz2$Fz2[Flight[12]:Flight[13]]
    Hop13 <- Fz2$Fz2[Flight[13]:Flight[14]]
    Hop14 <- Fz2$Fz2[Flight[14]:Flight[15]]
    Hop15 <- Fz2$Fz2[Flight[15]:Flight[16]]
    #    Hop16 <- Fz2$Fz2[Flight[16]:Flight[17]]
    
    # windows()
    # plot(FullHop1)
    # windows()
    # plot(FullHop2)
    # windows()
    # plot(FullHop3)
    # windows()
    # plot(FullHop4)
    # windows()
    # plot(FullHop5)
    # windows()
    # plot(FullHop6)
    # windows()
    # plot(FullHop7)
    # windows()
    # plot(FullHop8)
    # windows()
    # plot(FullHop9)
    # windows()
    # plot(FullHop10)
    # windows()
    # plot(FullHop11)
    # windows()
    # plot(FullHop12)
    # windows()
    # plot(FullHop13)
    # windows()
    # plot(FullHop14)
    # windows()
    # plot(FullHop15)
    
    #Hop1
    
    #Calculate Acceleration
    
    Hop1Acc = (Hop1-BW)/input$Mass
    
    Hop1Accavg = rollapply(Hop1Acc, 2, mean)
    Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
    
    #Acceleration Area
    Hop1Accarea = Hop1Accavg2*0.001
    Hop1VelInit = 1.3
    
    #Velocity
    Hop1Vela = Hop1Accarea
    Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
    Hop1Vel = cumsum(Hop1Vela)
    
    #Velocity Area
    Hop1Velavg = rollapply(Hop1Vel, 2, mean)
    Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
    Hop1Velarea = Hop1Velavg2*0.001
    
    #Displacement
    Hop1Disp = cumsum(Hop1Velarea)
    
    l = length(Hop1Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop1Disp[l]>0){
      while (Hop1Disp[l] > 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit - 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)}
    }else (Hop1Disp[l]<0) 
    {
      while (Hop1Disp[l] < 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit + 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop1 = max(Hop1)
    PeakHop1PC = PeakHop1/BW
    
    
    ContactHop1 <- 5 + which(Hop1[5:length(Hop1)] >5)[1]
    ContactTimeHop1 = (length(Hop1Disp) - ContactHop1)*0.001
    MinHop1Disp = min(Hop1Disp)
    EccHop1Disp = (Hop1Disp[ContactHop1] - MinHop1Disp)
    EccStiffHop1 = ((PeakHop1 - BW)/EccHop1Disp)/input$Mass
    
    JumpFreqHop1 = 1/(length(FullHop1)*0.001)
    
    #####################
    
    #Hop2
    
    #Calculate Acceleration
    
    Hop2Acc = (Hop2-BW)/input$Mass
    
    Hop2Accavg = rollapply(Hop2Acc, 2, mean)
    Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
    
    #Acceleration Area
    Hop2Accarea = Hop2Accavg2*0.001
    Hop2VelInit = 1.3
    
    #Velocity
    Hop2Vela = Hop2Accarea
    Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
    Hop2Vel = cumsum(Hop2Vela)
    
    #Velocity Area
    Hop2Velavg = rollapply(Hop2Vel, 2, mean)
    Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
    Hop2Velarea = Hop2Velavg2*0.001
    
    #Displacement
    Hop2Disp = cumsum(Hop2Velarea)
    
    l = length(Hop2Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop2Disp[l]>0){
      while (Hop2Disp[l] > 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit - 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)}
    }else (Hop2Disp[l]<0) 
    {
      while (Hop2Disp[l] < 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit + 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop2 = max(Hop2)
    PeakHop2PC = PeakHop2/BW
    
    
    ContactHop2 <- 5 + which(Hop2[5:length(Hop2)] >5)[1]
    ContactTimeHop2 = (length(Hop2Disp) - ContactHop2)*0.001
    MinHop2Disp = min(Hop2Disp)
    EccHop2Disp = (Hop2Disp[ContactHop2] - MinHop2Disp)
    EccStiffHop2 = ((PeakHop2 - BW)/EccHop2Disp)/input$Mass
    
    JumpFreqHop2 = 1/(length(FullHop2)*0.001)
    
    #########################
    
    #Hop3
    
    #Calculate Acceleration
    
    Hop3Acc = (Hop3-BW)/input$Mass
    
    Hop3Accavg = rollapply(Hop3Acc, 2, mean)
    Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
    
    #Acceleration Area
    Hop3Accarea = Hop3Accavg2*0.001
    Hop3VelInit = 1.3
    
    #Velocity
    Hop3Vela = Hop3Accarea
    Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
    Hop3Vel = cumsum(Hop3Vela)
    
    #Velocity Area
    Hop3Velavg = rollapply(Hop3Vel, 2, mean)
    Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
    Hop3Velarea = Hop3Velavg2*0.001
    
    #Displacement
    Hop3Disp = cumsum(Hop3Velarea)
    
    l = length(Hop3Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop3Disp[l]>0){
      while (Hop3Disp[l] > 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit - 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)}
    }else (Hop3Disp[l]<0) 
    {
      while (Hop3Disp[l] < 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit + 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop3 = max(Hop3)
    PeakHop3PC = PeakHop3/BW
    
    
    ContactHop3 <- 5 + which(Hop3[5:length(Hop3)] >5)[1]
    ContactTimeHop3 = (length(Hop3Disp) - ContactHop3)*0.001
    MinHop3Disp = min(Hop3Disp)
    EccHop3Disp = (Hop3Disp[ContactHop3] - MinHop3Disp)
    EccStiffHop3 = ((PeakHop3 - BW)/EccHop3Disp)/input$Mass
    
    JumpFreqHop3 = 1/(length(FullHop3)*0.001)
    
    #########################
    
    #Hop4
    
    #Calculate Acceleration
    
    Hop4Acc = (Hop4-BW)/input$Mass
    
    Hop4Accavg = rollapply(Hop4Acc, 2, mean)
    Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
    
    #Acceleration Area
    Hop4Accarea = Hop4Accavg2*0.001
    Hop4VelInit = 1.3
    
    #Velocity
    Hop4Vela = Hop4Accarea
    Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
    Hop4Vel = cumsum(Hop4Vela)
    
    #Velocity Area
    Hop4Velavg = rollapply(Hop4Vel, 2, mean)
    Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
    Hop4Velarea = Hop4Velavg2*0.001
    
    #Displacement
    Hop4Disp = cumsum(Hop4Velarea)
    
    l = length(Hop4Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop4Disp[l]>0){
      while (Hop4Disp[l] > 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit - 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)}
    }else (Hop4Disp[l]<0) 
    {
      while (Hop4Disp[l] < 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit + 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop4 = max(Hop4)
    PeakHop4PC = PeakHop4/BW
    
    
    ContactHop4 <- 5 + which(Hop4[5:length(Hop4)] >5)[1]
    ContactTimeHop4 = (length(Hop4Disp) - ContactHop4)*0.001
    MinHop4Disp = min(Hop4Disp)
    EccHop4Disp = (Hop4Disp[ContactHop4] - MinHop4Disp)
    EccStiffHop4 = ((PeakHop4 - BW)/EccHop4Disp)/input$Mass
    
    JumpFreqHop4 = 1/(length(FullHop4)*0.001)
    
    #########################
    
    #Hop5
    
    #Calculate Acceleration
    
    Hop5Acc = (Hop5-BW)/input$Mass
    
    Hop5Accavg = rollapply(Hop5Acc, 2, mean)
    Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
    
    #Acceleration Area
    Hop5Accarea = Hop5Accavg2*0.001
    Hop5VelInit = 1.3
    
    #Velocity
    Hop5Vela = Hop5Accarea
    Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
    Hop5Vel = cumsum(Hop5Vela)
    
    #Velocity Area
    Hop5Velavg = rollapply(Hop5Vel, 2, mean)
    Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
    Hop5Velarea = Hop5Velavg2*0.001
    
    #Displacement
    Hop5Disp = cumsum(Hop5Velarea)
    
    l = length(Hop5Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop5Disp[l]>0){
      while (Hop5Disp[l] > 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit - 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)}
    }else (Hop5Disp[l]<0) 
    {
      while (Hop5Disp[l] < 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit + 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop5 = max(Hop5)
    PeakHop5PC = PeakHop5/BW
    
    
    ContactHop5 <- 5 + which(Hop5[5:length(Hop5)] >5)[1]
    ContactTimeHop5 = (length(Hop5Disp) - ContactHop5)*0.001
    MinHop5Disp = min(Hop5Disp)
    EccHop5Disp = (Hop5Disp[ContactHop5] - MinHop5Disp)
    EccStiffHop5 = ((PeakHop5 - BW)/EccHop5Disp)/input$Mass
    
    JumpFreqHop5 = 1/(length(FullHop5)*0.001)
    
    #########################
    
    
    #Hop6
    
    #Calculate Acceleration
    
    Hop6Acc = (Hop6-BW)/input$Mass
    
    Hop6Accavg = rollapply(Hop6Acc, 2, mean)
    Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
    
    #Acceleration Area
    Hop6Accarea = Hop6Accavg2*0.001
    Hop6VelInit = 1.3
    
    #Velocity
    Hop6Vela = Hop6Accarea
    Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
    Hop6Vel = cumsum(Hop6Vela)
    
    #Velocity Area
    Hop6Velavg = rollapply(Hop6Vel, 2, mean)
    Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
    Hop6Velarea = Hop6Velavg2*0.001
    
    #Displacement
    Hop6Disp = cumsum(Hop6Velarea)
    
    l = length(Hop6Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop6Disp[l]>0){
      while (Hop6Disp[l] > 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit - 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)}
    }else (Hop6Disp[l]<0) 
    {
      while (Hop6Disp[l] < 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit + 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop6 = max(Hop6)
    PeakHop6PC = PeakHop6/BW
    
    
    ContactHop6 <- 5 + which(Hop6[5:length(Hop6)] >5)[1]
    ContactTimeHop6 = (length(Hop6Disp) - ContactHop6)*0.001
    MinHop6Disp = min(Hop6Disp)
    EccHop6Disp = (Hop6Disp[ContactHop6] - MinHop6Disp)
    EccStiffHop6 = ((PeakHop6 - BW)/EccHop6Disp)/input$Mass
    
    JumpFreqHop6 = 1/(length(FullHop6)*0.001)
    
    #########################
    
    
    #Hop7
    
    #Calculate Acceleration
    
    Hop7Acc = (Hop7-BW)/input$Mass
    
    Hop7Accavg = rollapply(Hop7Acc, 2, mean)
    Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
    
    #Acceleration Area
    Hop7Accarea = Hop7Accavg2*0.001
    Hop7VelInit = 1.3
    
    #Velocity
    Hop7Vela = Hop7Accarea
    Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
    Hop7Vel = cumsum(Hop7Vela)
    
    #Velocity Area
    Hop7Velavg = rollapply(Hop7Vel, 2, mean)
    Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
    Hop7Velarea = Hop7Velavg2*0.001
    
    #Displacement
    Hop7Disp = cumsum(Hop7Velarea)
    
    l = length(Hop7Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop7Disp[l]>0){
      while (Hop7Disp[l] > 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit - 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)}
    }else (Hop7Disp[l]<0) 
    {
      while (Hop7Disp[l] < 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit + 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop7 = max(Hop7)
    PeakHop7PC = PeakHop7/BW
    
    
    ContactHop7 <- 5 + which(Hop7[5:length(Hop7)] >5)[1]
    ContactTimeHop7 = (length(Hop7Disp) - ContactHop7)*0.001
    MinHop7Disp = min(Hop7Disp)
    EccHop7Disp = (Hop7Disp[ContactHop7] - MinHop7Disp)
    EccStiffHop7 = ((PeakHop7 - BW)/EccHop7Disp)/input$Mass
    
    JumpFreqHop7 = 1/(length(FullHop7)*0.001)
    
    #########################
    
    #Hop8
    
    #Calculate Acceleration
    
    Hop8Acc = (Hop8-BW)/input$Mass
    
    Hop8Accavg = rollapply(Hop8Acc, 2, mean)
    Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
    
    #Acceleration Area
    Hop8Accarea = Hop8Accavg2*0.001
    Hop8VelInit = 1.3
    
    #Velocity
    Hop8Vela = Hop8Accarea
    Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
    Hop8Vel = cumsum(Hop8Vela)
    
    #Velocity Area
    Hop8Velavg = rollapply(Hop8Vel, 2, mean)
    Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
    Hop8Velarea = Hop8Velavg2*0.001
    
    #Displacement
    Hop8Disp = cumsum(Hop8Velarea)
    
    l = length(Hop8Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop8Disp[l]>0){
      while (Hop8Disp[l] > 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit - 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)}
    }else (Hop8Disp[l]<0) 
    {
      while (Hop8Disp[l] < 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit + 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop8 = max(Hop8)
    PeakHop8PC = PeakHop8/BW
    
    
    ContactHop8 <- 5 + which(Hop8[5:length(Hop8)] >5)[1]
    ContactTimeHop8 = (length(Hop8Disp) - ContactHop8)*0.001
    MinHop8Disp = min(Hop8Disp)
    EccHop8Disp = (Hop8Disp[ContactHop8] - MinHop8Disp)
    EccStiffHop8 = ((PeakHop8 - BW)/EccHop8Disp)/input$Mass
    
    JumpFreqHop8 = 1/(length(FullHop8)*0.001)
    
    #########################
    
    #Hop9
    
    #Calculate Acceleration
    
    Hop9Acc = (Hop9-BW)/input$Mass
    
    Hop9Accavg = rollapply(Hop9Acc, 2, mean)
    Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
    
    #Acceleration Area
    Hop9Accarea = Hop9Accavg2*0.001
    Hop9VelInit = 1.3
    
    #Velocity
    Hop9Vela = Hop9Accarea
    Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
    Hop9Vel = cumsum(Hop9Vela)
    
    #Velocity Area
    Hop9Velavg = rollapply(Hop9Vel, 2, mean)
    Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
    Hop9Velarea = Hop9Velavg2*0.001
    
    #Displacement
    Hop9Disp = cumsum(Hop9Velarea)
    
    l = length(Hop9Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop9Disp[l]>0){
      while (Hop9Disp[l] > 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit - 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)}
    }else (Hop9Disp[l]<0) 
    {
      while (Hop9Disp[l] < 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit + 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop9 = max(Hop9)
    PeakHop9PC = PeakHop9/BW
    
    
    ContactHop9 <- 5 + which(Hop9[5:length(Hop9)] >5)[1]
    ContactTimeHop9 = (length(Hop9Disp) - ContactHop9)*0.001
    MinHop9Disp = min(Hop9Disp)
    EccHop9Disp = (Hop9Disp[ContactHop9] - MinHop9Disp)
    EccStiffHop9 = ((PeakHop9 - BW)/EccHop9Disp)/input$Mass
    
    JumpFreqHop9 = 1/(length(FullHop9)*0.001)
    
    #########################
    
    #Hop10
    
    #Calculate Acceleration
    
    Hop10Acc = (Hop10-BW)/input$Mass
    
    Hop10Accavg = rollapply(Hop10Acc, 2, mean)
    Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
    
    #Acceleration Area
    Hop10Accarea = Hop10Accavg2*0.001
    Hop10VelInit = 1.3
    
    #Velocity
    Hop10Vela = Hop10Accarea
    Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
    Hop10Vel = cumsum(Hop10Vela)
    
    #Velocity Area
    Hop10Velavg = rollapply(Hop10Vel, 2, mean)
    Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
    Hop10Velarea = Hop10Velavg2*0.001
    
    #Displacement
    Hop10Disp = cumsum(Hop10Velarea)
    
    l = length(Hop10Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop10Disp[l]>0){
      while (Hop10Disp[l] > 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit - 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)}
    }else (Hop10Disp[l]<0) 
    {
      while (Hop10Disp[l] < 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit + 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop10 = max(Hop10)
    PeakHop10PC = PeakHop10/BW
    
    
    ContactHop10 <- 5 + which(Hop10[5:length(Hop10)] >5)[1]
    ContactTimeHop10 = (length(Hop10Disp) - ContactHop10)*0.001
    MinHop10Disp = min(Hop10Disp)
    EccHop10Disp = (Hop10Disp[ContactHop10] - MinHop10Disp)
    EccStiffHop10 = ((PeakHop10 - BW)/EccHop10Disp)/input$Mass
    
    JumpFreqHop10 = 1/(length(FullHop10)*0.001)
    
    #########################
    
    #Hop11
    
    #Calculate Acceleration
    
    Hop11Acc = (Hop11-BW)/input$Mass
    
    Hop11Accavg = rollapply(Hop11Acc, 2, mean)
    Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
    
    #Acceleration Area
    Hop11Accarea = Hop11Accavg2*0.001
    Hop11VelInit = 1.3
    
    #Velocity
    Hop11Vela = Hop11Accarea
    Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
    Hop11Vel = cumsum(Hop11Vela)
    
    #Velocity Area
    Hop11Velavg = rollapply(Hop11Vel, 2, mean)
    Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
    Hop11Velarea = Hop11Velavg2*0.001
    
    #Displacement
    Hop11Disp = cumsum(Hop11Velarea)
    
    l = length(Hop11Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop11Disp[l]>0){
      while (Hop11Disp[l] > 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit - 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)}
    }else (Hop11Disp[l]<0) 
    {
      while (Hop11Disp[l] < 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit + 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop11 = max(Hop11)
    PeakHop11PC = PeakHop11/BW
    
    
    ContactHop11 <- 5 + which(Hop11[5:length(Hop11)] >5)[1]
    ContactTimeHop11 = (length(Hop11Disp) - ContactHop11)*0.001
    MinHop11Disp = min(Hop11Disp)
    EccHop11Disp = (Hop11Disp[ContactHop11] - MinHop11Disp)
    EccStiffHop11 = ((PeakHop11 - BW)/EccHop11Disp)/input$Mass
    
    JumpFreqHop11 = 1/(length(FullHop11)*0.001)
    
    #########################
    
    #Hop12
    
    #Calculate Acceleration
    
    Hop12Acc = (Hop12-BW)/input$Mass
    
    Hop12Accavg = rollapply(Hop12Acc, 2, mean)
    Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
    
    #Acceleration Area
    Hop12Accarea = Hop12Accavg2*0.001
    Hop12VelInit = 1.3
    
    #Velocity
    Hop12Vela = Hop12Accarea
    Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
    Hop12Vel = cumsum(Hop12Vela)
    
    #Velocity Area
    Hop12Velavg = rollapply(Hop12Vel, 2, mean)
    Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
    Hop12Velarea = Hop12Velavg2*0.001
    
    #Displacement
    Hop12Disp = cumsum(Hop12Velarea)
    
    l = length(Hop12Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop12Disp[l]>0){
      while (Hop12Disp[l] > 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit - 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)}
    }else (Hop12Disp[l]<0) 
    {
      while (Hop12Disp[l] < 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit + 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop12 = max(Hop12)
    PeakHop12PC = PeakHop12/BW
    
    
    ContactHop12 <- 5 + which(Hop12[5:length(Hop12)] >5)[1]
    ContactTimeHop12 = (length(Hop12Disp) - ContactHop12)*0.001
    MinHop12Disp = min(Hop12Disp)
    EccHop12Disp = (Hop12Disp[ContactHop12] - MinHop12Disp)
    EccStiffHop12 = ((PeakHop12 - BW)/EccHop12Disp)/input$Mass
    
    JumpFreqHop12 = 1/(length(FullHop12)*0.001)
    
    #########################
    
    #Hop13
    
    #Calculate Acceleration
    
    Hop13Acc = (Hop13-BW)/input$Mass
    
    Hop13Accavg = rollapply(Hop13Acc, 2, mean)
    Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
    
    #Acceleration Area
    Hop13Accarea = Hop13Accavg2*0.001
    Hop13VelInit = 1.3
    
    #Velocity
    Hop13Vela = Hop13Accarea
    Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
    Hop13Vel = cumsum(Hop13Vela)
    
    #Velocity Area
    Hop13Velavg = rollapply(Hop13Vel, 2, mean)
    Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
    Hop13Velarea = Hop13Velavg2*0.001
    
    #Displacement
    Hop13Disp = cumsum(Hop13Velarea)
    
    l = length(Hop13Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop13Disp[l]>0){
      while (Hop13Disp[l] > 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit - 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)}
    }else (Hop13Disp[l]<0) 
    {
      while (Hop13Disp[l] < 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit + 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop13 = max(Hop13)
    PeakHop13PC = PeakHop13/BW
    
    
    ContactHop13 <- 5 + which(Hop13[5:length(Hop13)] >5)[1]
    ContactTimeHop13 = (length(Hop13Disp) - ContactHop13)*0.001
    MinHop13Disp = min(Hop13Disp)
    EccHop13Disp = (Hop13Disp[ContactHop13] - MinHop13Disp)
    EccStiffHop13 = ((PeakHop13 - BW)/EccHop13Disp)/input$Mass
    
    JumpFreqHop13 = 1/(length(FullHop13)*0.001)
    
    #########################
    
    #Hop14
    
    #Calculate Acceleration
    
    Hop14Acc = (Hop14-BW)/input$Mass
    
    Hop14Accavg = rollapply(Hop14Acc, 2, mean)
    Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
    
    #Acceleration Area
    Hop14Accarea = Hop14Accavg2*0.001
    Hop14VelInit = 1.3
    
    #Velocity
    Hop14Vela = Hop14Accarea
    Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
    Hop14Vel = cumsum(Hop14Vela)
    
    #Velocity Area
    Hop14Velavg = rollapply(Hop14Vel, 2, mean)
    Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
    Hop14Velarea = Hop14Velavg2*0.001
    
    #Displacement
    Hop14Disp = cumsum(Hop14Velarea)
    
    l = length(Hop14Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop14Disp[l]>0){
      while (Hop14Disp[l] > 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit - 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)}
    }else (Hop14Disp[l]<0) 
    {
      while (Hop14Disp[l] < 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit + 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop14 = max(Hop14)
    PeakHop14PC = PeakHop14/BW
    
    
    ContactHop14 <- 5 + which(Hop14[5:length(Hop14)] >5)[1]
    ContactTimeHop14 = (length(Hop14Disp) - ContactHop14)*0.001
    MinHop14Disp = min(Hop14Disp)
    EccHop14Disp = (Hop14Disp[ContactHop14] - MinHop14Disp)
    EccStiffHop14 = ((PeakHop14 - BW)/EccHop14Disp)/input$Mass
    
    JumpFreqHop14 = 1/(length(FullHop14)*0.001)
    # 
    # #########################
    # 
    # #Hop15
    # 
    # #Calculate Acceleration
    # 
    # Hop15Acc = (Hop15-BW)/input$Mass
    # 
    # Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    # Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    # 
    # #Acceleration Area
    # Hop15Accarea = Hop15Accavg2*0.001
    # Hop15VelInit = 1.3
    # 
    # #Velocity
    # Hop15Vela = Hop15Accarea
    # Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    # Hop15Vel = cumsum(Hop15Vela)
    # 
    # #Velocity Area
    # Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    # Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    # Hop15Velarea = Hop15Velavg2*0.001
    # 
    # #Displacement
    # Hop15Disp = cumsum(Hop15Velarea)
    # 
    # l = length(Hop15Disp)
    # 
    # #Adjust Init Velocity to get Final Displacment close to zero
    # if (Hop15Disp[l]>0){
    #   while (Hop15Disp[l] > 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit - 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)}
    # }else (Hop15Disp[l]<0) 
    # {
    #   while (Hop15Disp[l] < 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit + 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)
    #   }
    # }
    # 
    # 
    # #Calculate output variables
    # PeakHop15 = max(Hop15)
    # PeakHop15PC = PeakHop15/BW
    # 
    # 
    # ContactHop15 <- 5 + which(Hop15[5:length(Hop15)] >5)[1]
    # ContactTimeHop15 = (length(Hop15Disp) - ContactHop15)*0.001
    # MinHop15Disp = min(Hop15Disp)
    # EccHop15Disp = (Hop15Disp[ContactHop15] - MinHop15Disp)
    # EccStiffHop15 = ((PeakHop15 - BW)/EccHop15Disp)/input$Mass
    # 
    # JumpFreqHop15 = 1/(length(FullHop15)*0.001)
    
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14", "Average")
    EccCOMDisp <- rbind(round(EccHop1Disp,digits=2), round(EccHop2Disp,digits=2), round(EccHop3Disp,digits=2), round(EccHop4Disp,digits=2), round(EccHop5Disp,digits=2), round(EccHop6Disp,digits=2), round(EccHop7Disp,digits=2), round(EccHop8Disp,digits=2), round(EccHop9Disp,digits=2), round(EccHop10Disp,digits=2), round(EccHop11Disp,digits=2), round(EccHop12Disp,digits=2), round(EccHop13Disp,digits=2), round(EccHop14Disp,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2))
    PeakForcePC <- rbind(round(PeakHop1PC,digits=2), round(PeakHop2PC,digits=2), round(PeakHop3PC,digits=2), round(PeakHop4PC,digits=2), round(PeakHop5PC,digits=2), round(PeakHop6PC,digits=2), round(PeakHop7PC,digits=2), round(PeakHop8PC,digits=2), round(PeakHop9PC,digits=2), round(PeakHop10PC,digits=2), round(PeakHop11PC,digits=2), round(PeakHop12PC,digits=2), round(PeakHop13PC,digits=2), round(PeakHop14PC,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14
                                                                                                                                                                                                                                                                                                                                                                                                                                      ,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    Left1.7Averages <<-cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
    EccCOMDisp2 <-  rbind(EccCOMDisp, avgEccCOMDisp)
    ContactTime2 <-  rbind(ContactTime, avgContactTime)
    EccStiffness2 <-  rbind(EccStiffness, avgEccStiffness)
    PeakForcePC2 <-  rbind(PeakForcePC, avgPeakForcePC)
    JumpFreq2 <-  rbind(JumpFreq, avgJumpFreq)
    
    Output <- cbind(hopnumber, EccCOMDisp2, ContactTime2, EccStiffness2, PeakForcePC2, JumpFreq2)
    
    colnames(Output)  <-c("Hopnumber","EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
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
  output$resultstableLeft2.0 <- renderTable({
    
    data1 <- 
      read.csv(input$Left2.0$datapath, stringsAsFactors = F, skip = 17)
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <5)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    for (r in 1:15){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <5)
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
        geom_vline(xintercept=Contact[15], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHop2 <- Fz2$Fz2[Contact[2]:Contact[3]]
    FullHop3 <- Fz2$Fz2[Contact[3]:Contact[4]]
    FullHop4 <- Fz2$Fz2[Contact[4]:Contact[5]]
    FullHop5 <- Fz2$Fz2[Contact[5]:Contact[6]]
    FullHop6 <- Fz2$Fz2[Contact[6]:Contact[7]]
    FullHop7 <- Fz2$Fz2[Contact[7]:Contact[8]]
    FullHop8 <- Fz2$Fz2[Contact[8]:Contact[9]]
    FullHop9 <- Fz2$Fz2[Contact[9]:Contact[10]]
    FullHop10 <- Fz2$Fz2[Contact[10]:Contact[11]]
    FullHop11 <- Fz2$Fz2[Contact[11]:Contact[12]]
    FullHop12 <- Fz2$Fz2[Contact[12]:Contact[13]]
    FullHop13 <- Fz2$Fz2[Contact[13]:Contact[14]]
    FullHop14 <- Fz2$Fz2[Contact[14]:Contact[15]]
    #    FullHop15 <- Fz2$Fz2[Contact[15]:Contact[16]]
    
    Hop1 <- Fz2$Fz2[Flight[1]:Flight[2]]
    Hop2 <- Fz2$Fz2[Flight[2]:Flight[3]]
    Hop3 <- Fz2$Fz2[Flight[3]:Flight[4]]
    Hop4 <- Fz2$Fz2[Flight[4]:Flight[5]]
    Hop5 <- Fz2$Fz2[Flight[5]:Flight[6]]
    Hop6 <- Fz2$Fz2[Flight[6]:Flight[7]]
    Hop7 <- Fz2$Fz2[Flight[7]:Flight[8]]
    Hop8 <- Fz2$Fz2[Flight[8]:Flight[9]]
    Hop9 <- Fz2$Fz2[Flight[9]:Flight[10]]
    Hop10 <- Fz2$Fz2[Flight[10]:Flight[11]]
    Hop11 <- Fz2$Fz2[Flight[11]:Flight[12]]
    Hop12 <- Fz2$Fz2[Flight[12]:Flight[13]]
    Hop13 <- Fz2$Fz2[Flight[13]:Flight[14]]
    Hop14 <- Fz2$Fz2[Flight[14]:Flight[15]]
    Hop15 <- Fz2$Fz2[Flight[15]:Flight[16]]
    #    Hop16 <- Fz2$Fz2[Flight[16]:Flight[17]]
    
    # windows()
    # plot(FullHop1)
    # windows()
    # plot(FullHop2)
    # windows()
    # plot(FullHop3)
    # windows()
    # plot(FullHop4)
    # windows()
    # plot(FullHop5)
    # windows()
    # plot(FullHop6)
    # windows()
    # plot(FullHop7)
    # windows()
    # plot(FullHop8)
    # windows()
    # plot(FullHop9)
    # windows()
    # plot(FullHop10)
    # windows()
    # plot(FullHop11)
    # windows()
    # plot(FullHop12)
    # windows()
    # plot(FullHop13)
    # windows()
    # plot(FullHop14)
    # windows()
    # plot(FullHop15)
    
    #Hop1
    
    #Calculate Acceleration
    
    Hop1Acc = (Hop1-BW)/input$Mass
    
    Hop1Accavg = rollapply(Hop1Acc, 2, mean)
    Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
    
    #Acceleration Area
    Hop1Accarea = Hop1Accavg2*0.001
    Hop1VelInit = 1.3
    
    #Velocity
    Hop1Vela = Hop1Accarea
    Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
    Hop1Vel = cumsum(Hop1Vela)
    
    #Velocity Area
    Hop1Velavg = rollapply(Hop1Vel, 2, mean)
    Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
    Hop1Velarea = Hop1Velavg2*0.001
    
    #Displacement
    Hop1Disp = cumsum(Hop1Velarea)
    
    l = length(Hop1Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop1Disp[l]>0){
      while (Hop1Disp[l] > 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit - 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)}
    }else (Hop1Disp[l]<0) 
    {
      while (Hop1Disp[l] < 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit + 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop1 = max(Hop1)
    PeakHop1PC = PeakHop1/BW
    
    
    ContactHop1 <- 5 + which(Hop1[5:length(Hop1)] >5)[1]
    ContactTimeHop1 = (length(Hop1Disp) - ContactHop1)*0.001
    MinHop1Disp = min(Hop1Disp)
    EccHop1Disp = (Hop1Disp[ContactHop1] - MinHop1Disp)
    EccStiffHop1 = ((PeakHop1 - BW)/EccHop1Disp)/input$Mass
    
    JumpFreqHop1 = 1/(length(FullHop1)*0.001)
    
    #####################
    
    #Hop2
    
    #Calculate Acceleration
    
    Hop2Acc = (Hop2-BW)/input$Mass
    
    Hop2Accavg = rollapply(Hop2Acc, 2, mean)
    Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
    
    #Acceleration Area
    Hop2Accarea = Hop2Accavg2*0.001
    Hop2VelInit = 1.3
    
    #Velocity
    Hop2Vela = Hop2Accarea
    Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
    Hop2Vel = cumsum(Hop2Vela)
    
    #Velocity Area
    Hop2Velavg = rollapply(Hop2Vel, 2, mean)
    Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
    Hop2Velarea = Hop2Velavg2*0.001
    
    #Displacement
    Hop2Disp = cumsum(Hop2Velarea)
    
    l = length(Hop2Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop2Disp[l]>0){
      while (Hop2Disp[l] > 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit - 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)}
    }else (Hop2Disp[l]<0) 
    {
      while (Hop2Disp[l] < 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit + 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop2 = max(Hop2)
    PeakHop2PC = PeakHop2/BW
    
    
    ContactHop2 <- 5 + which(Hop2[5:length(Hop2)] >5)[1]
    ContactTimeHop2 = (length(Hop2Disp) - ContactHop2)*0.001
    MinHop2Disp = min(Hop2Disp)
    EccHop2Disp = (Hop2Disp[ContactHop2] - MinHop2Disp)
    EccStiffHop2 = ((PeakHop2 - BW)/EccHop2Disp)/input$Mass
    
    JumpFreqHop2 = 1/(length(FullHop2)*0.001)
    
    #########################
    
    #Hop3
    
    #Calculate Acceleration
    
    Hop3Acc = (Hop3-BW)/input$Mass
    
    Hop3Accavg = rollapply(Hop3Acc, 2, mean)
    Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
    
    #Acceleration Area
    Hop3Accarea = Hop3Accavg2*0.001
    Hop3VelInit = 1.3
    
    #Velocity
    Hop3Vela = Hop3Accarea
    Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
    Hop3Vel = cumsum(Hop3Vela)
    
    #Velocity Area
    Hop3Velavg = rollapply(Hop3Vel, 2, mean)
    Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
    Hop3Velarea = Hop3Velavg2*0.001
    
    #Displacement
    Hop3Disp = cumsum(Hop3Velarea)
    
    l = length(Hop3Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop3Disp[l]>0){
      while (Hop3Disp[l] > 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit - 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)}
    }else (Hop3Disp[l]<0) 
    {
      while (Hop3Disp[l] < 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit + 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop3 = max(Hop3)
    PeakHop3PC = PeakHop3/BW
    
    
    ContactHop3 <- 5 + which(Hop3[5:length(Hop3)] >5)[1]
    ContactTimeHop3 = (length(Hop3Disp) - ContactHop3)*0.001
    MinHop3Disp = min(Hop3Disp)
    EccHop3Disp = (Hop3Disp[ContactHop3] - MinHop3Disp)
    EccStiffHop3 = ((PeakHop3 - BW)/EccHop3Disp)/input$Mass
    
    JumpFreqHop3 = 1/(length(FullHop3)*0.001)
    
    #########################
    
    #Hop4
    
    #Calculate Acceleration
    
    Hop4Acc = (Hop4-BW)/input$Mass
    
    Hop4Accavg = rollapply(Hop4Acc, 2, mean)
    Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
    
    #Acceleration Area
    Hop4Accarea = Hop4Accavg2*0.001
    Hop4VelInit = 1.3
    
    #Velocity
    Hop4Vela = Hop4Accarea
    Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
    Hop4Vel = cumsum(Hop4Vela)
    
    #Velocity Area
    Hop4Velavg = rollapply(Hop4Vel, 2, mean)
    Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
    Hop4Velarea = Hop4Velavg2*0.001
    
    #Displacement
    Hop4Disp = cumsum(Hop4Velarea)
    
    l = length(Hop4Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop4Disp[l]>0){
      while (Hop4Disp[l] > 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit - 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)}
    }else (Hop4Disp[l]<0) 
    {
      while (Hop4Disp[l] < 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit + 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop4 = max(Hop4)
    PeakHop4PC = PeakHop4/BW
    
    
    ContactHop4 <- 5 + which(Hop4[5:length(Hop4)] >5)[1]
    ContactTimeHop4 = (length(Hop4Disp) - ContactHop4)*0.001
    MinHop4Disp = min(Hop4Disp)
    EccHop4Disp = (Hop4Disp[ContactHop4] - MinHop4Disp)
    EccStiffHop4 = ((PeakHop4 - BW)/EccHop4Disp)/input$Mass
    
    JumpFreqHop4 = 1/(length(FullHop4)*0.001)
    
    #########################
    
    #Hop5
    
    #Calculate Acceleration
    
    Hop5Acc = (Hop5-BW)/input$Mass
    
    Hop5Accavg = rollapply(Hop5Acc, 2, mean)
    Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
    
    #Acceleration Area
    Hop5Accarea = Hop5Accavg2*0.001
    Hop5VelInit = 1.3
    
    #Velocity
    Hop5Vela = Hop5Accarea
    Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
    Hop5Vel = cumsum(Hop5Vela)
    
    #Velocity Area
    Hop5Velavg = rollapply(Hop5Vel, 2, mean)
    Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
    Hop5Velarea = Hop5Velavg2*0.001
    
    #Displacement
    Hop5Disp = cumsum(Hop5Velarea)
    
    l = length(Hop5Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop5Disp[l]>0){
      while (Hop5Disp[l] > 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit - 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)}
    }else (Hop5Disp[l]<0) 
    {
      while (Hop5Disp[l] < 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit + 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop5 = max(Hop5)
    PeakHop5PC = PeakHop5/BW
    
    
    ContactHop5 <- 5 + which(Hop5[5:length(Hop5)] >5)[1]
    ContactTimeHop5 = (length(Hop5Disp) - ContactHop5)*0.001
    MinHop5Disp = min(Hop5Disp)
    EccHop5Disp = (Hop5Disp[ContactHop5] - MinHop5Disp)
    EccStiffHop5 = ((PeakHop5 - BW)/EccHop5Disp)/input$Mass
    
    JumpFreqHop5 = 1/(length(FullHop5)*0.001)
    
    #########################
    
    
    #Hop6
    
    #Calculate Acceleration
    
    Hop6Acc = (Hop6-BW)/input$Mass
    
    Hop6Accavg = rollapply(Hop6Acc, 2, mean)
    Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
    
    #Acceleration Area
    Hop6Accarea = Hop6Accavg2*0.001
    Hop6VelInit = 1.3
    
    #Velocity
    Hop6Vela = Hop6Accarea
    Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
    Hop6Vel = cumsum(Hop6Vela)
    
    #Velocity Area
    Hop6Velavg = rollapply(Hop6Vel, 2, mean)
    Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
    Hop6Velarea = Hop6Velavg2*0.001
    
    #Displacement
    Hop6Disp = cumsum(Hop6Velarea)
    
    l = length(Hop6Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop6Disp[l]>0){
      while (Hop6Disp[l] > 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit - 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)}
    }else (Hop6Disp[l]<0) 
    {
      while (Hop6Disp[l] < 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit + 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop6 = max(Hop6)
    PeakHop6PC = PeakHop6/BW
    
    
    ContactHop6 <- 5 + which(Hop6[5:length(Hop6)] >5)[1]
    ContactTimeHop6 = (length(Hop6Disp) - ContactHop6)*0.001
    MinHop6Disp = min(Hop6Disp)
    EccHop6Disp = (Hop6Disp[ContactHop6] - MinHop6Disp)
    EccStiffHop6 = ((PeakHop6 - BW)/EccHop6Disp)/input$Mass
    
    JumpFreqHop6 = 1/(length(FullHop6)*0.001)
    
    #########################
    
    
    #Hop7
    
    #Calculate Acceleration
    
    Hop7Acc = (Hop7-BW)/input$Mass
    
    Hop7Accavg = rollapply(Hop7Acc, 2, mean)
    Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
    
    #Acceleration Area
    Hop7Accarea = Hop7Accavg2*0.001
    Hop7VelInit = 1.3
    
    #Velocity
    Hop7Vela = Hop7Accarea
    Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
    Hop7Vel = cumsum(Hop7Vela)
    
    #Velocity Area
    Hop7Velavg = rollapply(Hop7Vel, 2, mean)
    Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
    Hop7Velarea = Hop7Velavg2*0.001
    
    #Displacement
    Hop7Disp = cumsum(Hop7Velarea)
    
    l = length(Hop7Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop7Disp[l]>0){
      while (Hop7Disp[l] > 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit - 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)}
    }else (Hop7Disp[l]<0) 
    {
      while (Hop7Disp[l] < 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit + 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop7 = max(Hop7)
    PeakHop7PC = PeakHop7/BW
    
    
    ContactHop7 <- 5 + which(Hop7[5:length(Hop7)] >5)[1]
    ContactTimeHop7 = (length(Hop7Disp) - ContactHop7)*0.001
    MinHop7Disp = min(Hop7Disp)
    EccHop7Disp = (Hop7Disp[ContactHop7] - MinHop7Disp)
    EccStiffHop7 = ((PeakHop7 - BW)/EccHop7Disp)/input$Mass
    
    JumpFreqHop7 = 1/(length(FullHop7)*0.001)
    
    #########################
    
    #Hop8
    
    #Calculate Acceleration
    
    Hop8Acc = (Hop8-BW)/input$Mass
    
    Hop8Accavg = rollapply(Hop8Acc, 2, mean)
    Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
    
    #Acceleration Area
    Hop8Accarea = Hop8Accavg2*0.001
    Hop8VelInit = 1.3
    
    #Velocity
    Hop8Vela = Hop8Accarea
    Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
    Hop8Vel = cumsum(Hop8Vela)
    
    #Velocity Area
    Hop8Velavg = rollapply(Hop8Vel, 2, mean)
    Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
    Hop8Velarea = Hop8Velavg2*0.001
    
    #Displacement
    Hop8Disp = cumsum(Hop8Velarea)
    
    l = length(Hop8Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop8Disp[l]>0){
      while (Hop8Disp[l] > 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit - 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)}
    }else (Hop8Disp[l]<0) 
    {
      while (Hop8Disp[l] < 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit + 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop8 = max(Hop8)
    PeakHop8PC = PeakHop8/BW
    
    
    ContactHop8 <- 5 + which(Hop8[5:length(Hop8)] >5)[1]
    ContactTimeHop8 = (length(Hop8Disp) - ContactHop8)*0.001
    MinHop8Disp = min(Hop8Disp)
    EccHop8Disp = (Hop8Disp[ContactHop8] - MinHop8Disp)
    EccStiffHop8 = ((PeakHop8 - BW)/EccHop8Disp)/input$Mass
    
    JumpFreqHop8 = 1/(length(FullHop8)*0.001)
    
    #########################
    
    #Hop9
    
    #Calculate Acceleration
    
    Hop9Acc = (Hop9-BW)/input$Mass
    
    Hop9Accavg = rollapply(Hop9Acc, 2, mean)
    Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
    
    #Acceleration Area
    Hop9Accarea = Hop9Accavg2*0.001
    Hop9VelInit = 1.3
    
    #Velocity
    Hop9Vela = Hop9Accarea
    Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
    Hop9Vel = cumsum(Hop9Vela)
    
    #Velocity Area
    Hop9Velavg = rollapply(Hop9Vel, 2, mean)
    Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
    Hop9Velarea = Hop9Velavg2*0.001
    
    #Displacement
    Hop9Disp = cumsum(Hop9Velarea)
    
    l = length(Hop9Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop9Disp[l]>0){
      while (Hop9Disp[l] > 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit - 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)}
    }else (Hop9Disp[l]<0) 
    {
      while (Hop9Disp[l] < 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit + 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop9 = max(Hop9)
    PeakHop9PC = PeakHop9/BW
    
    
    ContactHop9 <- 5 + which(Hop9[5:length(Hop9)] >5)[1]
    ContactTimeHop9 = (length(Hop9Disp) - ContactHop9)*0.001
    MinHop9Disp = min(Hop9Disp)
    EccHop9Disp = (Hop9Disp[ContactHop9] - MinHop9Disp)
    EccStiffHop9 = ((PeakHop9 - BW)/EccHop9Disp)/input$Mass
    
    JumpFreqHop9 = 1/(length(FullHop9)*0.001)
    
    #########################
    
    #Hop10
    
    #Calculate Acceleration
    
    Hop10Acc = (Hop10-BW)/input$Mass
    
    Hop10Accavg = rollapply(Hop10Acc, 2, mean)
    Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
    
    #Acceleration Area
    Hop10Accarea = Hop10Accavg2*0.001
    Hop10VelInit = 1.3
    
    #Velocity
    Hop10Vela = Hop10Accarea
    Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
    Hop10Vel = cumsum(Hop10Vela)
    
    #Velocity Area
    Hop10Velavg = rollapply(Hop10Vel, 2, mean)
    Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
    Hop10Velarea = Hop10Velavg2*0.001
    
    #Displacement
    Hop10Disp = cumsum(Hop10Velarea)
    
    l = length(Hop10Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop10Disp[l]>0){
      while (Hop10Disp[l] > 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit - 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)}
    }else (Hop10Disp[l]<0) 
    {
      while (Hop10Disp[l] < 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit + 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop10 = max(Hop10)
    PeakHop10PC = PeakHop10/BW
    
    
    ContactHop10 <- 5 + which(Hop10[5:length(Hop10)] >5)[1]
    ContactTimeHop10 = (length(Hop10Disp) - ContactHop10)*0.001
    MinHop10Disp = min(Hop10Disp)
    EccHop10Disp = (Hop10Disp[ContactHop10] - MinHop10Disp)
    EccStiffHop10 = ((PeakHop10 - BW)/EccHop10Disp)/input$Mass
    
    JumpFreqHop10 = 1/(length(FullHop10)*0.001)
    
    #########################
    
    #Hop11
    
    #Calculate Acceleration
    
    Hop11Acc = (Hop11-BW)/input$Mass
    
    Hop11Accavg = rollapply(Hop11Acc, 2, mean)
    Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
    
    #Acceleration Area
    Hop11Accarea = Hop11Accavg2*0.001
    Hop11VelInit = 1.3
    
    #Velocity
    Hop11Vela = Hop11Accarea
    Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
    Hop11Vel = cumsum(Hop11Vela)
    
    #Velocity Area
    Hop11Velavg = rollapply(Hop11Vel, 2, mean)
    Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
    Hop11Velarea = Hop11Velavg2*0.001
    
    #Displacement
    Hop11Disp = cumsum(Hop11Velarea)
    
    l = length(Hop11Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop11Disp[l]>0){
      while (Hop11Disp[l] > 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit - 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)}
    }else (Hop11Disp[l]<0) 
    {
      while (Hop11Disp[l] < 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit + 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop11 = max(Hop11)
    PeakHop11PC = PeakHop11/BW
    
    
    ContactHop11 <- 5 + which(Hop11[5:length(Hop11)] >5)[1]
    ContactTimeHop11 = (length(Hop11Disp) - ContactHop11)*0.001
    MinHop11Disp = min(Hop11Disp)
    EccHop11Disp = (Hop11Disp[ContactHop11] - MinHop11Disp)
    EccStiffHop11 = ((PeakHop11 - BW)/EccHop11Disp)/input$Mass
    
    JumpFreqHop11 = 1/(length(FullHop11)*0.001)
    
    #########################
    
    #Hop12
    
    #Calculate Acceleration
    
    Hop12Acc = (Hop12-BW)/input$Mass
    
    Hop12Accavg = rollapply(Hop12Acc, 2, mean)
    Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
    
    #Acceleration Area
    Hop12Accarea = Hop12Accavg2*0.001
    Hop12VelInit = 1.3
    
    #Velocity
    Hop12Vela = Hop12Accarea
    Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
    Hop12Vel = cumsum(Hop12Vela)
    
    #Velocity Area
    Hop12Velavg = rollapply(Hop12Vel, 2, mean)
    Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
    Hop12Velarea = Hop12Velavg2*0.001
    
    #Displacement
    Hop12Disp = cumsum(Hop12Velarea)
    
    l = length(Hop12Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop12Disp[l]>0){
      while (Hop12Disp[l] > 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit - 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)}
    }else (Hop12Disp[l]<0) 
    {
      while (Hop12Disp[l] < 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit + 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop12 = max(Hop12)
    PeakHop12PC = PeakHop12/BW
    
    
    ContactHop12 <- 5 + which(Hop12[5:length(Hop12)] >5)[1]
    ContactTimeHop12 = (length(Hop12Disp) - ContactHop12)*0.001
    MinHop12Disp = min(Hop12Disp)
    EccHop12Disp = (Hop12Disp[ContactHop12] - MinHop12Disp)
    EccStiffHop12 = ((PeakHop12 - BW)/EccHop12Disp)/input$Mass
    
    JumpFreqHop12 = 1/(length(FullHop12)*0.001)
    
    #########################
    
    #Hop13
    
    #Calculate Acceleration
    
    Hop13Acc = (Hop13-BW)/input$Mass
    
    Hop13Accavg = rollapply(Hop13Acc, 2, mean)
    Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
    
    #Acceleration Area
    Hop13Accarea = Hop13Accavg2*0.001
    Hop13VelInit = 1.3
    
    #Velocity
    Hop13Vela = Hop13Accarea
    Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
    Hop13Vel = cumsum(Hop13Vela)
    
    #Velocity Area
    Hop13Velavg = rollapply(Hop13Vel, 2, mean)
    Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
    Hop13Velarea = Hop13Velavg2*0.001
    
    #Displacement
    Hop13Disp = cumsum(Hop13Velarea)
    
    l = length(Hop13Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop13Disp[l]>0){
      while (Hop13Disp[l] > 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit - 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)}
    }else (Hop13Disp[l]<0) 
    {
      while (Hop13Disp[l] < 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit + 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop13 = max(Hop13)
    PeakHop13PC = PeakHop13/BW
    
    
    ContactHop13 <- 5 + which(Hop13[5:length(Hop13)] >5)[1]
    ContactTimeHop13 = (length(Hop13Disp) - ContactHop13)*0.001
    MinHop13Disp = min(Hop13Disp)
    EccHop13Disp = (Hop13Disp[ContactHop13] - MinHop13Disp)
    EccStiffHop13 = ((PeakHop13 - BW)/EccHop13Disp)/input$Mass
    
    JumpFreqHop13 = 1/(length(FullHop13)*0.001)
    
    #########################
    
    #Hop14
    
    #Calculate Acceleration
    
    Hop14Acc = (Hop14-BW)/input$Mass
    
    Hop14Accavg = rollapply(Hop14Acc, 2, mean)
    Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
    
    #Acceleration Area
    Hop14Accarea = Hop14Accavg2*0.001
    Hop14VelInit = 1.3
    
    #Velocity
    Hop14Vela = Hop14Accarea
    Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
    Hop14Vel = cumsum(Hop14Vela)
    
    #Velocity Area
    Hop14Velavg = rollapply(Hop14Vel, 2, mean)
    Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
    Hop14Velarea = Hop14Velavg2*0.001
    
    #Displacement
    Hop14Disp = cumsum(Hop14Velarea)
    
    l = length(Hop14Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop14Disp[l]>0){
      while (Hop14Disp[l] > 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit - 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)}
    }else (Hop14Disp[l]<0) 
    {
      while (Hop14Disp[l] < 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit + 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop14 = max(Hop14)
    PeakHop14PC = PeakHop14/BW
    
    
    ContactHop14 <- 5 + which(Hop14[5:length(Hop14)] >5)[1]
    ContactTimeHop14 = (length(Hop14Disp) - ContactHop14)*0.001
    MinHop14Disp = min(Hop14Disp)
    EccHop14Disp = (Hop14Disp[ContactHop14] - MinHop14Disp)
    EccStiffHop14 = ((PeakHop14 - BW)/EccHop14Disp)/input$Mass
    
    JumpFreqHop14 = 1/(length(FullHop14)*0.001)
    
    #########################
    # 
    # #Hop15
    # 
    # #Calculate Acceleration
    # 
    # Hop15Acc = (Hop15-BW)/input$Mass
    # 
    # Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    # Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    # 
    # #Acceleration Area
    # Hop15Accarea = Hop15Accavg2*0.001
    # Hop15VelInit = 1.3
    # 
    # #Velocity
    # Hop15Vela = Hop15Accarea
    # Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    # Hop15Vel = cumsum(Hop15Vela)
    # 
    # #Velocity Area
    # Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    # Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    # Hop15Velarea = Hop15Velavg2*0.001
    # 
    # #Displacement
    # Hop15Disp = cumsum(Hop15Velarea)
    # 
    # l = length(Hop15Disp)
    # 
    # #Adjust Init Velocity to get Final Displacment close to zero
    # if (Hop15Disp[l]>0){
    #   while (Hop15Disp[l] > 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit - 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)}
    # }else (Hop15Disp[l]<0) 
    # {
    #   while (Hop15Disp[l] < 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit + 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)
    #   }
    # }
    # 
    # 
    # #Calculate output variables
    # PeakHop15 = max(Hop15)
    # PeakHop15PC = PeakHop15/BW
    # 
    # 
    # ContactHop15 <- 5 + which(Hop15[5:length(Hop15)] >5)[1]
    # ContactTimeHop15 = (length(Hop15Disp) - ContactHop15)*0.001
    # MinHop15Disp = min(Hop15Disp)
    # EccHop15Disp = (Hop15Disp[ContactHop15] - MinHop15Disp)
    # EccStiffHop15 = ((PeakHop15 - BW)/EccHop15Disp)/input$Mass
    # 
    # JumpFreqHop15 = 1/(length(FullHop15)*0.001)
    # 
    # 
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14", "Average")
    EccCOMDisp <- rbind(round(EccHop1Disp,digits=2), round(EccHop2Disp,digits=2), round(EccHop3Disp,digits=2), round(EccHop4Disp,digits=2), round(EccHop5Disp,digits=2), round(EccHop6Disp,digits=2), round(EccHop7Disp,digits=2), round(EccHop8Disp,digits=2), round(EccHop9Disp,digits=2), round(EccHop10Disp,digits=2), round(EccHop11Disp,digits=2), round(EccHop12Disp,digits=2), round(EccHop13Disp,digits=2), round(EccHop14Disp,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2))
    PeakForcePC <- rbind(round(PeakHop1PC,digits=2), round(PeakHop2PC,digits=2), round(PeakHop3PC,digits=2), round(PeakHop4PC,digits=2), round(PeakHop5PC,digits=2), round(PeakHop6PC,digits=2), round(PeakHop7PC,digits=2), round(PeakHop8PC,digits=2), round(PeakHop9PC,digits=2), round(PeakHop10PC,digits=2), round(PeakHop11PC,digits=2), round(PeakHop12PC,digits=2), round(PeakHop13PC,digits=2), round(PeakHop14PC,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    Left2.0Averages <<-cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
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
    
  })
  output$resultstableLeft2.3 <- renderTable({
    
    data1 <- 
      read.csv(input$Left2.3$datapath, stringsAsFactors = F, skip = 17)
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <5)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    for (r in 1:15){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <5)
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
        geom_vline(xintercept=Contact[15], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHop2 <- Fz2$Fz2[Contact[2]:Contact[3]]
    FullHop3 <- Fz2$Fz2[Contact[3]:Contact[4]]
    FullHop4 <- Fz2$Fz2[Contact[4]:Contact[5]]
    FullHop5 <- Fz2$Fz2[Contact[5]:Contact[6]]
    FullHop6 <- Fz2$Fz2[Contact[6]:Contact[7]]
    FullHop7 <- Fz2$Fz2[Contact[7]:Contact[8]]
    FullHop8 <- Fz2$Fz2[Contact[8]:Contact[9]]
    FullHop9 <- Fz2$Fz2[Contact[9]:Contact[10]]
    FullHop10 <- Fz2$Fz2[Contact[10]:Contact[11]]
    FullHop11 <- Fz2$Fz2[Contact[11]:Contact[12]]
    FullHop12 <- Fz2$Fz2[Contact[12]:Contact[13]]
    FullHop13 <- Fz2$Fz2[Contact[13]:Contact[14]]
    FullHop14 <- Fz2$Fz2[Contact[14]:Contact[15]]
    #    FullHop15 <- Fz2$Fz2[Contact[15]:Contact[16]]
    
    Hop1 <- Fz2$Fz2[Flight[1]:Flight[2]]
    Hop2 <- Fz2$Fz2[Flight[2]:Flight[3]]
    Hop3 <- Fz2$Fz2[Flight[3]:Flight[4]]
    Hop4 <- Fz2$Fz2[Flight[4]:Flight[5]]
    Hop5 <- Fz2$Fz2[Flight[5]:Flight[6]]
    Hop6 <- Fz2$Fz2[Flight[6]:Flight[7]]
    Hop7 <- Fz2$Fz2[Flight[7]:Flight[8]]
    Hop8 <- Fz2$Fz2[Flight[8]:Flight[9]]
    Hop9 <- Fz2$Fz2[Flight[9]:Flight[10]]
    Hop10 <- Fz2$Fz2[Flight[10]:Flight[11]]
    Hop11 <- Fz2$Fz2[Flight[11]:Flight[12]]
    Hop12 <- Fz2$Fz2[Flight[12]:Flight[13]]
    Hop13 <- Fz2$Fz2[Flight[13]:Flight[14]]
    Hop14 <- Fz2$Fz2[Flight[14]:Flight[15]]
    Hop15 <- Fz2$Fz2[Flight[15]:Flight[16]]
    #    Hop16 <- Fz2$Fz2[Flight[16]:Flight[17]]
    
    # windows()
    # plot(FullHop1)
    # windows()
    # plot(FullHop2)
    # windows()
    # plot(FullHop3)
    # windows()
    # plot(FullHop4)
    # windows()
    # plot(FullHop5)
    # windows()
    # plot(FullHop6)
    # windows()
    # plot(FullHop7)
    # windows()
    # plot(FullHop8)
    # windows()
    # plot(FullHop9)
    # windows()
    # plot(FullHop10)
    # windows()
    # plot(FullHop11)
    # windows()
    # plot(FullHop12)
    # windows()
    # plot(FullHop13)
    # windows()
    # plot(FullHop14)
    # windows()
    # plot(FullHop15)
    
    #Hop1
    
    #Calculate Acceleration
    
    Hop1Acc = (Hop1-BW)/input$Mass
    
    Hop1Accavg = rollapply(Hop1Acc, 2, mean)
    Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
    
    #Acceleration Area
    Hop1Accarea = Hop1Accavg2*0.001
    Hop1VelInit = 1.3
    
    #Velocity
    Hop1Vela = Hop1Accarea
    Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
    Hop1Vel = cumsum(Hop1Vela)
    
    #Velocity Area
    Hop1Velavg = rollapply(Hop1Vel, 2, mean)
    Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
    Hop1Velarea = Hop1Velavg2*0.001
    
    #Displacement
    Hop1Disp = cumsum(Hop1Velarea)
    
    l = length(Hop1Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop1Disp[l]>0){
      while (Hop1Disp[l] > 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit - 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)}
    }else (Hop1Disp[l]<0) 
    {
      while (Hop1Disp[l] < 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit + 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop1 = max(Hop1)
    PeakHop1PC = PeakHop1/BW
    
    
    ContactHop1 <- 5 + which(Hop1[5:length(Hop1)] >5)[1]
    ContactTimeHop1 = (length(Hop1Disp) - ContactHop1)*0.001
    MinHop1Disp = min(Hop1Disp)
    EccHop1Disp = (Hop1Disp[ContactHop1] - MinHop1Disp)
    EccStiffHop1 = ((PeakHop1 - BW)/EccHop1Disp)/input$Mass
    
    JumpFreqHop1 = 1/(length(FullHop1)*0.001)
    
    #####################
    
    #Hop2
    
    #Calculate Acceleration
    
    Hop2Acc = (Hop2-BW)/input$Mass
    
    Hop2Accavg = rollapply(Hop2Acc, 2, mean)
    Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
    
    #Acceleration Area
    Hop2Accarea = Hop2Accavg2*0.001
    Hop2VelInit = 1.3
    
    #Velocity
    Hop2Vela = Hop2Accarea
    Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
    Hop2Vel = cumsum(Hop2Vela)
    
    #Velocity Area
    Hop2Velavg = rollapply(Hop2Vel, 2, mean)
    Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
    Hop2Velarea = Hop2Velavg2*0.001
    
    #Displacement
    Hop2Disp = cumsum(Hop2Velarea)
    
    l = length(Hop2Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop2Disp[l]>0){
      while (Hop2Disp[l] > 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit - 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)}
    }else (Hop2Disp[l]<0) 
    {
      while (Hop2Disp[l] < 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit + 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop2 = max(Hop2)
    PeakHop2PC = PeakHop2/BW
    
    
    ContactHop2 <- 5 + which(Hop2[5:length(Hop2)] >5)[1]
    ContactTimeHop2 = (length(Hop2Disp) - ContactHop2)*0.001
    MinHop2Disp = min(Hop2Disp)
    EccHop2Disp = (Hop2Disp[ContactHop2] - MinHop2Disp)
    EccStiffHop2 = ((PeakHop2 - BW)/EccHop2Disp)/input$Mass
    
    JumpFreqHop2 = 1/(length(FullHop2)*0.001)
    
    #########################
    
    #Hop3
    
    #Calculate Acceleration
    
    Hop3Acc = (Hop3-BW)/input$Mass
    
    Hop3Accavg = rollapply(Hop3Acc, 2, mean)
    Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
    
    #Acceleration Area
    Hop3Accarea = Hop3Accavg2*0.001
    Hop3VelInit = 1.3
    
    #Velocity
    Hop3Vela = Hop3Accarea
    Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
    Hop3Vel = cumsum(Hop3Vela)
    
    #Velocity Area
    Hop3Velavg = rollapply(Hop3Vel, 2, mean)
    Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
    Hop3Velarea = Hop3Velavg2*0.001
    
    #Displacement
    Hop3Disp = cumsum(Hop3Velarea)
    
    l = length(Hop3Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop3Disp[l]>0){
      while (Hop3Disp[l] > 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit - 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)}
    }else (Hop3Disp[l]<0) 
    {
      while (Hop3Disp[l] < 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit + 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop3 = max(Hop3)
    PeakHop3PC = PeakHop3/BW
    
    
    ContactHop3 <- 5 + which(Hop3[5:length(Hop3)] >5)[1]
    ContactTimeHop3 = (length(Hop3Disp) - ContactHop3)*0.001
    MinHop3Disp = min(Hop3Disp)
    EccHop3Disp = (Hop3Disp[ContactHop3] - MinHop3Disp)
    EccStiffHop3 = ((PeakHop3 - BW)/EccHop3Disp)/input$Mass
    
    JumpFreqHop3 = 1/(length(FullHop3)*0.001)
    
    #########################
    
    #Hop4
    
    #Calculate Acceleration
    
    Hop4Acc = (Hop4-BW)/input$Mass
    
    Hop4Accavg = rollapply(Hop4Acc, 2, mean)
    Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
    
    #Acceleration Area
    Hop4Accarea = Hop4Accavg2*0.001
    Hop4VelInit = 1.3
    
    #Velocity
    Hop4Vela = Hop4Accarea
    Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
    Hop4Vel = cumsum(Hop4Vela)
    
    #Velocity Area
    Hop4Velavg = rollapply(Hop4Vel, 2, mean)
    Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
    Hop4Velarea = Hop4Velavg2*0.001
    
    #Displacement
    Hop4Disp = cumsum(Hop4Velarea)
    
    l = length(Hop4Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop4Disp[l]>0){
      while (Hop4Disp[l] > 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit - 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)}
    }else (Hop4Disp[l]<0) 
    {
      while (Hop4Disp[l] < 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit + 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop4 = max(Hop4)
    PeakHop4PC = PeakHop4/BW
    
    
    ContactHop4 <- 5 + which(Hop4[5:length(Hop4)] >5)[1]
    ContactTimeHop4 = (length(Hop4Disp) - ContactHop4)*0.001
    MinHop4Disp = min(Hop4Disp)
    EccHop4Disp = (Hop4Disp[ContactHop4] - MinHop4Disp)
    EccStiffHop4 = ((PeakHop4 - BW)/EccHop4Disp)/input$Mass
    
    JumpFreqHop4 = 1/(length(FullHop4)*0.001)
    
    #########################
    
    #Hop5
    
    #Calculate Acceleration
    
    Hop5Acc = (Hop5-BW)/input$Mass
    
    Hop5Accavg = rollapply(Hop5Acc, 2, mean)
    Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
    
    #Acceleration Area
    Hop5Accarea = Hop5Accavg2*0.001
    Hop5VelInit = 1.3
    
    #Velocity
    Hop5Vela = Hop5Accarea
    Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
    Hop5Vel = cumsum(Hop5Vela)
    
    #Velocity Area
    Hop5Velavg = rollapply(Hop5Vel, 2, mean)
    Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
    Hop5Velarea = Hop5Velavg2*0.001
    
    #Displacement
    Hop5Disp = cumsum(Hop5Velarea)
    
    l = length(Hop5Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop5Disp[l]>0){
      while (Hop5Disp[l] > 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit - 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)}
    }else (Hop5Disp[l]<0) 
    {
      while (Hop5Disp[l] < 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit + 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop5 = max(Hop5)
    PeakHop5PC = PeakHop5/BW
    
    
    ContactHop5 <- 5 + which(Hop5[5:length(Hop5)] >5)[1]
    ContactTimeHop5 = (length(Hop5Disp) - ContactHop5)*0.001
    MinHop5Disp = min(Hop5Disp)
    EccHop5Disp = (Hop5Disp[ContactHop5] - MinHop5Disp)
    EccStiffHop5 = ((PeakHop5 - BW)/EccHop5Disp)/input$Mass
    
    JumpFreqHop5 = 1/(length(FullHop5)*0.001)
    
    #########################
    
    
    #Hop6
    
    #Calculate Acceleration
    
    Hop6Acc = (Hop6-BW)/input$Mass
    
    Hop6Accavg = rollapply(Hop6Acc, 2, mean)
    Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
    
    #Acceleration Area
    Hop6Accarea = Hop6Accavg2*0.001
    Hop6VelInit = 1.3
    
    #Velocity
    Hop6Vela = Hop6Accarea
    Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
    Hop6Vel = cumsum(Hop6Vela)
    
    #Velocity Area
    Hop6Velavg = rollapply(Hop6Vel, 2, mean)
    Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
    Hop6Velarea = Hop6Velavg2*0.001
    
    #Displacement
    Hop6Disp = cumsum(Hop6Velarea)
    
    l = length(Hop6Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop6Disp[l]>0){
      while (Hop6Disp[l] > 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit - 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)}
    }else (Hop6Disp[l]<0) 
    {
      while (Hop6Disp[l] < 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit + 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop6 = max(Hop6)
    PeakHop6PC = PeakHop6/BW
    
    
    ContactHop6 <- 5 + which(Hop6[5:length(Hop6)] >5)[1]
    ContactTimeHop6 = (length(Hop6Disp) - ContactHop6)*0.001
    MinHop6Disp = min(Hop6Disp)
    EccHop6Disp = (Hop6Disp[ContactHop6] - MinHop6Disp)
    EccStiffHop6 = ((PeakHop6 - BW)/EccHop6Disp)/input$Mass
    
    JumpFreqHop6 = 1/(length(FullHop6)*0.001)
    
    #########################
    
    
    #Hop7
    
    #Calculate Acceleration
    
    Hop7Acc = (Hop7-BW)/input$Mass
    
    Hop7Accavg = rollapply(Hop7Acc, 2, mean)
    Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
    
    #Acceleration Area
    Hop7Accarea = Hop7Accavg2*0.001
    Hop7VelInit = 1.3
    
    #Velocity
    Hop7Vela = Hop7Accarea
    Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
    Hop7Vel = cumsum(Hop7Vela)
    
    #Velocity Area
    Hop7Velavg = rollapply(Hop7Vel, 2, mean)
    Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
    Hop7Velarea = Hop7Velavg2*0.001
    
    #Displacement
    Hop7Disp = cumsum(Hop7Velarea)
    
    l = length(Hop7Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop7Disp[l]>0){
      while (Hop7Disp[l] > 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit - 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)}
    }else (Hop7Disp[l]<0) 
    {
      while (Hop7Disp[l] < 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit + 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop7 = max(Hop7)
    PeakHop7PC = PeakHop7/BW
    
    
    ContactHop7 <- 5 + which(Hop7[5:length(Hop7)] >5)[1]
    ContactTimeHop7 = (length(Hop7Disp) - ContactHop7)*0.001
    MinHop7Disp = min(Hop7Disp)
    EccHop7Disp = (Hop7Disp[ContactHop7] - MinHop7Disp)
    EccStiffHop7 = ((PeakHop7 - BW)/EccHop7Disp)/input$Mass
    
    JumpFreqHop7 = 1/(length(FullHop7)*0.001)
    
    #########################
    
    #Hop8
    
    #Calculate Acceleration
    
    Hop8Acc = (Hop8-BW)/input$Mass
    
    Hop8Accavg = rollapply(Hop8Acc, 2, mean)
    Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
    
    #Acceleration Area
    Hop8Accarea = Hop8Accavg2*0.001
    Hop8VelInit = 1.3
    
    #Velocity
    Hop8Vela = Hop8Accarea
    Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
    Hop8Vel = cumsum(Hop8Vela)
    
    #Velocity Area
    Hop8Velavg = rollapply(Hop8Vel, 2, mean)
    Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
    Hop8Velarea = Hop8Velavg2*0.001
    
    #Displacement
    Hop8Disp = cumsum(Hop8Velarea)
    
    l = length(Hop8Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop8Disp[l]>0){
      while (Hop8Disp[l] > 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit - 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)}
    }else (Hop8Disp[l]<0) 
    {
      while (Hop8Disp[l] < 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit + 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop8 = max(Hop8)
    PeakHop8PC = PeakHop8/BW
    
    
    ContactHop8 <- 5 + which(Hop8[5:length(Hop8)] >5)[1]
    ContactTimeHop8 = (length(Hop8Disp) - ContactHop8)*0.001
    MinHop8Disp = min(Hop8Disp)
    EccHop8Disp = (Hop8Disp[ContactHop8] - MinHop8Disp)
    EccStiffHop8 = ((PeakHop8 - BW)/EccHop8Disp)/input$Mass
    
    JumpFreqHop8 = 1/(length(FullHop8)*0.001)
    
    #########################
    
    #Hop9
    
    #Calculate Acceleration
    
    Hop9Acc = (Hop9-BW)/input$Mass
    
    Hop9Accavg = rollapply(Hop9Acc, 2, mean)
    Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
    
    #Acceleration Area
    Hop9Accarea = Hop9Accavg2*0.001
    Hop9VelInit = 1.3
    
    #Velocity
    Hop9Vela = Hop9Accarea
    Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
    Hop9Vel = cumsum(Hop9Vela)
    
    #Velocity Area
    Hop9Velavg = rollapply(Hop9Vel, 2, mean)
    Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
    Hop9Velarea = Hop9Velavg2*0.001
    
    #Displacement
    Hop9Disp = cumsum(Hop9Velarea)
    
    l = length(Hop9Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop9Disp[l]>0){
      while (Hop9Disp[l] > 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit - 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)}
    }else (Hop9Disp[l]<0) 
    {
      while (Hop9Disp[l] < 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit + 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop9 = max(Hop9)
    PeakHop9PC = PeakHop9/BW
    
    
    ContactHop9 <- 5 + which(Hop9[5:length(Hop9)] >5)[1]
    ContactTimeHop9 = (length(Hop9Disp) - ContactHop9)*0.001
    MinHop9Disp = min(Hop9Disp)
    EccHop9Disp = (Hop9Disp[ContactHop9] - MinHop9Disp)
    EccStiffHop9 = ((PeakHop9 - BW)/EccHop9Disp)/input$Mass
    
    JumpFreqHop9 = 1/(length(FullHop9)*0.001)
    
    #########################
    
    #Hop10
    
    #Calculate Acceleration
    
    Hop10Acc = (Hop10-BW)/input$Mass
    
    Hop10Accavg = rollapply(Hop10Acc, 2, mean)
    Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
    
    #Acceleration Area
    Hop10Accarea = Hop10Accavg2*0.001
    Hop10VelInit = 1.3
    
    #Velocity
    Hop10Vela = Hop10Accarea
    Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
    Hop10Vel = cumsum(Hop10Vela)
    
    #Velocity Area
    Hop10Velavg = rollapply(Hop10Vel, 2, mean)
    Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
    Hop10Velarea = Hop10Velavg2*0.001
    
    #Displacement
    Hop10Disp = cumsum(Hop10Velarea)
    
    l = length(Hop10Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop10Disp[l]>0){
      while (Hop10Disp[l] > 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit - 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)}
    }else (Hop10Disp[l]<0) 
    {
      while (Hop10Disp[l] < 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit + 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop10 = max(Hop10)
    PeakHop10PC = PeakHop10/BW
    
    
    ContactHop10 <- 5 + which(Hop10[5:length(Hop10)] >5)[1]
    ContactTimeHop10 = (length(Hop10Disp) - ContactHop10)*0.001
    MinHop10Disp = min(Hop10Disp)
    EccHop10Disp = (Hop10Disp[ContactHop10] - MinHop10Disp)
    EccStiffHop10 = ((PeakHop10 - BW)/EccHop10Disp)/input$Mass
    
    JumpFreqHop10 = 1/(length(FullHop10)*0.001)
    
    #########################
    
    #Hop11
    
    #Calculate Acceleration
    
    Hop11Acc = (Hop11-BW)/input$Mass
    
    Hop11Accavg = rollapply(Hop11Acc, 2, mean)
    Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
    
    #Acceleration Area
    Hop11Accarea = Hop11Accavg2*0.001
    Hop11VelInit = 1.3
    
    #Velocity
    Hop11Vela = Hop11Accarea
    Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
    Hop11Vel = cumsum(Hop11Vela)
    
    #Velocity Area
    Hop11Velavg = rollapply(Hop11Vel, 2, mean)
    Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
    Hop11Velarea = Hop11Velavg2*0.001
    
    #Displacement
    Hop11Disp = cumsum(Hop11Velarea)
    
    l = length(Hop11Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop11Disp[l]>0){
      while (Hop11Disp[l] > 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit - 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)}
    }else (Hop11Disp[l]<0) 
    {
      while (Hop11Disp[l] < 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit + 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop11 = max(Hop11)
    PeakHop11PC = PeakHop11/BW
    
    
    ContactHop11 <- 5 + which(Hop11[5:length(Hop11)] >5)[1]
    ContactTimeHop11 = (length(Hop11Disp) - ContactHop11)*0.001
    MinHop11Disp = min(Hop11Disp)
    EccHop11Disp = (Hop11Disp[ContactHop11] - MinHop11Disp)
    EccStiffHop11 = ((PeakHop11 - BW)/EccHop11Disp)/input$Mass
    
    JumpFreqHop11 = 1/(length(FullHop11)*0.001)
    
    #########################
    
    #Hop12
    
    #Calculate Acceleration
    
    Hop12Acc = (Hop12-BW)/input$Mass
    
    Hop12Accavg = rollapply(Hop12Acc, 2, mean)
    Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
    
    #Acceleration Area
    Hop12Accarea = Hop12Accavg2*0.001
    Hop12VelInit = 1.3
    
    #Velocity
    Hop12Vela = Hop12Accarea
    Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
    Hop12Vel = cumsum(Hop12Vela)
    
    #Velocity Area
    Hop12Velavg = rollapply(Hop12Vel, 2, mean)
    Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
    Hop12Velarea = Hop12Velavg2*0.001
    
    #Displacement
    Hop12Disp = cumsum(Hop12Velarea)
    
    l = length(Hop12Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop12Disp[l]>0){
      while (Hop12Disp[l] > 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit - 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)}
    }else (Hop12Disp[l]<0) 
    {
      while (Hop12Disp[l] < 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit + 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop12 = max(Hop12)
    PeakHop12PC = PeakHop12/BW
    
    
    ContactHop12 <- 5 + which(Hop12[5:length(Hop12)] >5)[1]
    ContactTimeHop12 = (length(Hop12Disp) - ContactHop12)*0.001
    MinHop12Disp = min(Hop12Disp)
    EccHop12Disp = (Hop12Disp[ContactHop12] - MinHop12Disp)
    EccStiffHop12 = ((PeakHop12 - BW)/EccHop12Disp)/input$Mass
    
    JumpFreqHop12 = 1/(length(FullHop12)*0.001)
    
    #########################
    
    #Hop13
    
    #Calculate Acceleration
    
    Hop13Acc = (Hop13-BW)/input$Mass
    
    Hop13Accavg = rollapply(Hop13Acc, 2, mean)
    Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
    
    #Acceleration Area
    Hop13Accarea = Hop13Accavg2*0.001
    Hop13VelInit = 1.3
    
    #Velocity
    Hop13Vela = Hop13Accarea
    Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
    Hop13Vel = cumsum(Hop13Vela)
    
    #Velocity Area
    Hop13Velavg = rollapply(Hop13Vel, 2, mean)
    Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
    Hop13Velarea = Hop13Velavg2*0.001
    
    #Displacement
    Hop13Disp = cumsum(Hop13Velarea)
    
    l = length(Hop13Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop13Disp[l]>0){
      while (Hop13Disp[l] > 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit - 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)}
    }else (Hop13Disp[l]<0) 
    {
      while (Hop13Disp[l] < 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit + 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop13 = max(Hop13)
    PeakHop13PC = PeakHop13/BW
    
    
    ContactHop13 <- 5 + which(Hop13[5:length(Hop13)] >5)[1]
    ContactTimeHop13 = (length(Hop13Disp) - ContactHop13)*0.001
    MinHop13Disp = min(Hop13Disp)
    EccHop13Disp = (Hop13Disp[ContactHop13] - MinHop13Disp)
    EccStiffHop13 = ((PeakHop13 - BW)/EccHop13Disp)/input$Mass
    
    JumpFreqHop13 = 1/(length(FullHop13)*0.001)
    
    #########################
    
    #Hop14
    
    #Calculate Acceleration
    
    Hop14Acc = (Hop14-BW)/input$Mass
    
    Hop14Accavg = rollapply(Hop14Acc, 2, mean)
    Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
    
    #Acceleration Area
    Hop14Accarea = Hop14Accavg2*0.001
    Hop14VelInit = 1.3
    
    #Velocity
    Hop14Vela = Hop14Accarea
    Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
    Hop14Vel = cumsum(Hop14Vela)
    
    #Velocity Area
    Hop14Velavg = rollapply(Hop14Vel, 2, mean)
    Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
    Hop14Velarea = Hop14Velavg2*0.001
    
    #Displacement
    Hop14Disp = cumsum(Hop14Velarea)
    
    l = length(Hop14Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop14Disp[l]>0){
      while (Hop14Disp[l] > 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit - 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)}
    }else (Hop14Disp[l]<0) 
    {
      while (Hop14Disp[l] < 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit + 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop14 = max(Hop14)
    PeakHop14PC = PeakHop14/BW
    
    
    ContactHop14 <- 5 + which(Hop14[5:length(Hop14)] >5)[1]
    ContactTimeHop14 = (length(Hop14Disp) - ContactHop14)*0.001
    MinHop14Disp = min(Hop14Disp)
    EccHop14Disp = (Hop14Disp[ContactHop14] - MinHop14Disp)
    EccStiffHop14 = ((PeakHop14 - BW)/EccHop14Disp)/input$Mass
    
    JumpFreqHop14 = 1/(length(FullHop14)*0.001)
    
    #########################
    
    # #Hop15
    # 
    # #Calculate Acceleration
    # 
    # Hop15Acc = (Hop15-BW)/input$Mass
    # 
    # Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    # Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    # 
    # #Acceleration Area
    # Hop15Accarea = Hop15Accavg2*0.001
    # Hop15VelInit = 1.3
    # 
    # #Velocity
    # Hop15Vela = Hop15Accarea
    # Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    # Hop15Vel = cumsum(Hop15Vela)
    # 
    # #Velocity Area
    # Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    # Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    # Hop15Velarea = Hop15Velavg2*0.001
    # 
    # #Displacement
    # Hop15Disp = cumsum(Hop15Velarea)
    # 
    # l = length(Hop15Disp)
    # 
    # #Adjust Init Velocity to get Final Displacment close to zero
    # if (Hop15Disp[l]>0){
    #   while (Hop15Disp[l] > 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit - 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)}
    # }else (Hop15Disp[l]<0) 
    # {
    #   while (Hop15Disp[l] < 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit + 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)
    #   }
    # }
    # 
    # 
    # #Calculate output variables
    # PeakHop15 = max(Hop15)
    # PeakHop15PC = PeakHop15/BW
    # 
    # 
    # ContactHop15 <- 5 + which(Hop15[5:length(Hop15)] >5)[1]
    # ContactTimeHop15 = (length(Hop15Disp) - ContactHop15)*0.001
    # MinHop15Disp = min(Hop15Disp)
    # EccHop15Disp = (Hop15Disp[ContactHop15] - MinHop15Disp)
    # EccStiffHop15 = ((PeakHop15 - BW)/EccHop15Disp)/input$Mass
    # 
    # JumpFreqHop15 = 1/(length(FullHop15)*0.001)
    # 
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14", "Average")
    EccCOMDisp <- rbind(round(EccHop1Disp,digits=2), round(EccHop2Disp,digits=2), round(EccHop3Disp,digits=2), round(EccHop4Disp,digits=2), round(EccHop5Disp,digits=2), round(EccHop6Disp,digits=2), round(EccHop7Disp,digits=2), round(EccHop8Disp,digits=2), round(EccHop9Disp,digits=2), round(EccHop10Disp,digits=2), round(EccHop11Disp,digits=2), round(EccHop12Disp,digits=2), round(EccHop13Disp,digits=2), round(EccHop14Disp,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2))
    PeakForcePC <- rbind(round(PeakHop1PC,digits=2), round(PeakHop2PC,digits=2), round(PeakHop3PC,digits=2), round(PeakHop4PC,digits=2), round(PeakHop5PC,digits=2), round(PeakHop6PC,digits=2), round(PeakHop7PC,digits=2), round(PeakHop8PC,digits=2), round(PeakHop9PC,digits=2), round(PeakHop10PC,digits=2), round(PeakHop11PC,digits=2), round(PeakHop12PC,digits=2), round(PeakHop13PC,digits=2), round(PeakHop14PC,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    Left2.3Averages <<-cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
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
    
  })
  output$resultstableLeft2.6 <- renderTable({
    
    data1 <- 
      read.csv(input$Left2.6$datapath, stringsAsFactors = F, skip = 17)
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <5)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    for (r in 1:15){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <5)
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
        geom_vline(xintercept=Contact[15], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHop2 <- Fz2$Fz2[Contact[2]:Contact[3]]
    FullHop3 <- Fz2$Fz2[Contact[3]:Contact[4]]
    FullHop4 <- Fz2$Fz2[Contact[4]:Contact[5]]
    FullHop5 <- Fz2$Fz2[Contact[5]:Contact[6]]
    FullHop6 <- Fz2$Fz2[Contact[6]:Contact[7]]
    FullHop7 <- Fz2$Fz2[Contact[7]:Contact[8]]
    FullHop8 <- Fz2$Fz2[Contact[8]:Contact[9]]
    FullHop9 <- Fz2$Fz2[Contact[9]:Contact[10]]
    FullHop10 <- Fz2$Fz2[Contact[10]:Contact[11]]
    FullHop11 <- Fz2$Fz2[Contact[11]:Contact[12]]
    FullHop12 <- Fz2$Fz2[Contact[12]:Contact[13]]
    FullHop13 <- Fz2$Fz2[Contact[13]:Contact[14]]
    FullHop14 <- Fz2$Fz2[Contact[14]:Contact[15]]
    #    FullHop15 <- Fz2$Fz2[Contact[15]:Contact[16]]
    
    Hop1 <- Fz2$Fz2[Flight[1]:Flight[2]]
    Hop2 <- Fz2$Fz2[Flight[2]:Flight[3]]
    Hop3 <- Fz2$Fz2[Flight[3]:Flight[4]]
    Hop4 <- Fz2$Fz2[Flight[4]:Flight[5]]
    Hop5 <- Fz2$Fz2[Flight[5]:Flight[6]]
    Hop6 <- Fz2$Fz2[Flight[6]:Flight[7]]
    Hop7 <- Fz2$Fz2[Flight[7]:Flight[8]]
    Hop8 <- Fz2$Fz2[Flight[8]:Flight[9]]
    Hop9 <- Fz2$Fz2[Flight[9]:Flight[10]]
    Hop10 <- Fz2$Fz2[Flight[10]:Flight[11]]
    Hop11 <- Fz2$Fz2[Flight[11]:Flight[12]]
    Hop12 <- Fz2$Fz2[Flight[12]:Flight[13]]
    Hop13 <- Fz2$Fz2[Flight[13]:Flight[14]]
    Hop14 <- Fz2$Fz2[Flight[14]:Flight[15]]
    Hop15 <- Fz2$Fz2[Flight[15]:Flight[16]]
    #    Hop16 <- Fz2$Fz2[Flight[16]:Flight[17]]
    
    # windows()
    # plot(FullHop1)
    # windows()
    # plot(FullHop2)
    # windows()
    # plot(FullHop3)
    # windows()
    # plot(FullHop4)
    # windows()
    # plot(FullHop5)
    # windows()
    # plot(FullHop6)
    # windows()
    # plot(FullHop7)
    # windows()
    # plot(FullHop8)
    # windows()
    # plot(FullHop9)
    # windows()
    # plot(FullHop10)
    # windows()
    # plot(FullHop11)
    # windows()
    # plot(FullHop12)
    # windows()
    # plot(FullHop13)
    # windows()
    # plot(FullHop14)
    # windows()
    # plot(FullHop15)
    
    #Hop1
    
    #Calculate Acceleration
    
    Hop1Acc = (Hop1-BW)/input$Mass
    
    Hop1Accavg = rollapply(Hop1Acc, 2, mean)
    Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
    
    #Acceleration Area
    Hop1Accarea = Hop1Accavg2*0.001
    Hop1VelInit = 1.3
    
    #Velocity
    Hop1Vela = Hop1Accarea
    Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
    Hop1Vel = cumsum(Hop1Vela)
    
    #Velocity Area
    Hop1Velavg = rollapply(Hop1Vel, 2, mean)
    Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
    Hop1Velarea = Hop1Velavg2*0.001
    
    #Displacement
    Hop1Disp = cumsum(Hop1Velarea)
    
    l = length(Hop1Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop1Disp[l]>0){
      while (Hop1Disp[l] > 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit - 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)}
    }else (Hop1Disp[l]<0) 
    {
      while (Hop1Disp[l] < 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit + 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop1 = max(Hop1)
    PeakHop1PC = PeakHop1/BW
    
    
    ContactHop1 <- 5 + which(Hop1[5:length(Hop1)] >5)[1]
    ContactTimeHop1 = (length(Hop1Disp) - ContactHop1)*0.001
    MinHop1Disp = min(Hop1Disp)
    EccHop1Disp = (Hop1Disp[ContactHop1] - MinHop1Disp)
    EccStiffHop1 = ((PeakHop1 - BW)/EccHop1Disp)/input$Mass
    
    JumpFreqHop1 = 1/(length(FullHop1)*0.001)
    
    #####################
    
    #Hop2
    
    #Calculate Acceleration
    
    Hop2Acc = (Hop2-BW)/input$Mass
    
    Hop2Accavg = rollapply(Hop2Acc, 2, mean)
    Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
    
    #Acceleration Area
    Hop2Accarea = Hop2Accavg2*0.001
    Hop2VelInit = 1.3
    
    #Velocity
    Hop2Vela = Hop2Accarea
    Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
    Hop2Vel = cumsum(Hop2Vela)
    
    #Velocity Area
    Hop2Velavg = rollapply(Hop2Vel, 2, mean)
    Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
    Hop2Velarea = Hop2Velavg2*0.001
    
    #Displacement
    Hop2Disp = cumsum(Hop2Velarea)
    
    l = length(Hop2Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop2Disp[l]>0){
      while (Hop2Disp[l] > 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit - 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)}
    }else (Hop2Disp[l]<0) 
    {
      while (Hop2Disp[l] < 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit + 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop2 = max(Hop2)
    PeakHop2PC = PeakHop2/BW
    
    
    ContactHop2 <- 5 + which(Hop2[5:length(Hop2)] >5)[1]
    ContactTimeHop2 = (length(Hop2Disp) - ContactHop2)*0.001
    MinHop2Disp = min(Hop2Disp)
    EccHop2Disp = (Hop2Disp[ContactHop2] - MinHop2Disp)
    EccStiffHop2 = ((PeakHop2 - BW)/EccHop2Disp)/input$Mass
    
    JumpFreqHop2 = 1/(length(FullHop2)*0.001)
    
    #########################
    
    #Hop3
    
    #Calculate Acceleration
    
    Hop3Acc = (Hop3-BW)/input$Mass
    
    Hop3Accavg = rollapply(Hop3Acc, 2, mean)
    Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
    
    #Acceleration Area
    Hop3Accarea = Hop3Accavg2*0.001
    Hop3VelInit = 1.3
    
    #Velocity
    Hop3Vela = Hop3Accarea
    Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
    Hop3Vel = cumsum(Hop3Vela)
    
    #Velocity Area
    Hop3Velavg = rollapply(Hop3Vel, 2, mean)
    Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
    Hop3Velarea = Hop3Velavg2*0.001
    
    #Displacement
    Hop3Disp = cumsum(Hop3Velarea)
    
    l = length(Hop3Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop3Disp[l]>0){
      while (Hop3Disp[l] > 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit - 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)}
    }else (Hop3Disp[l]<0) 
    {
      while (Hop3Disp[l] < 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit + 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop3 = max(Hop3)
    PeakHop3PC = PeakHop3/BW
    
    
    ContactHop3 <- 5 + which(Hop3[5:length(Hop3)] >5)[1]
    ContactTimeHop3 = (length(Hop3Disp) - ContactHop3)*0.001
    MinHop3Disp = min(Hop3Disp)
    EccHop3Disp = (Hop3Disp[ContactHop3] - MinHop3Disp)
    EccStiffHop3 = ((PeakHop3 - BW)/EccHop3Disp)/input$Mass
    
    JumpFreqHop3 = 1/(length(FullHop3)*0.001)
    
    #########################
    
    #Hop4
    
    #Calculate Acceleration
    
    Hop4Acc = (Hop4-BW)/input$Mass
    
    Hop4Accavg = rollapply(Hop4Acc, 2, mean)
    Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
    
    #Acceleration Area
    Hop4Accarea = Hop4Accavg2*0.001
    Hop4VelInit = 1.3
    
    #Velocity
    Hop4Vela = Hop4Accarea
    Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
    Hop4Vel = cumsum(Hop4Vela)
    
    #Velocity Area
    Hop4Velavg = rollapply(Hop4Vel, 2, mean)
    Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
    Hop4Velarea = Hop4Velavg2*0.001
    
    #Displacement
    Hop4Disp = cumsum(Hop4Velarea)
    
    l = length(Hop4Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop4Disp[l]>0){
      while (Hop4Disp[l] > 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit - 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)}
    }else (Hop4Disp[l]<0) 
    {
      while (Hop4Disp[l] < 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit + 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop4 = max(Hop4)
    PeakHop4PC = PeakHop4/BW
    
    
    ContactHop4 <- 5 + which(Hop4[5:length(Hop4)] >5)[1]
    ContactTimeHop4 = (length(Hop4Disp) - ContactHop4)*0.001
    MinHop4Disp = min(Hop4Disp)
    EccHop4Disp = (Hop4Disp[ContactHop4] - MinHop4Disp)
    EccStiffHop4 = ((PeakHop4 - BW)/EccHop4Disp)/input$Mass
    
    JumpFreqHop4 = 1/(length(FullHop4)*0.001)
    
    #########################
    
    #Hop5
    
    #Calculate Acceleration
    
    Hop5Acc = (Hop5-BW)/input$Mass
    
    Hop5Accavg = rollapply(Hop5Acc, 2, mean)
    Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
    
    #Acceleration Area
    Hop5Accarea = Hop5Accavg2*0.001
    Hop5VelInit = 1.3
    
    #Velocity
    Hop5Vela = Hop5Accarea
    Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
    Hop5Vel = cumsum(Hop5Vela)
    
    #Velocity Area
    Hop5Velavg = rollapply(Hop5Vel, 2, mean)
    Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
    Hop5Velarea = Hop5Velavg2*0.001
    
    #Displacement
    Hop5Disp = cumsum(Hop5Velarea)
    
    l = length(Hop5Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop5Disp[l]>0){
      while (Hop5Disp[l] > 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit - 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)}
    }else (Hop5Disp[l]<0) 
    {
      while (Hop5Disp[l] < 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit + 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop5 = max(Hop5)
    PeakHop5PC = PeakHop5/BW
    
    
    ContactHop5 <- 5 + which(Hop5[5:length(Hop5)] >5)[1]
    ContactTimeHop5 = (length(Hop5Disp) - ContactHop5)*0.001
    MinHop5Disp = min(Hop5Disp)
    EccHop5Disp = (Hop5Disp[ContactHop5] - MinHop5Disp)
    EccStiffHop5 = ((PeakHop5 - BW)/EccHop5Disp)/input$Mass
    
    JumpFreqHop5 = 1/(length(FullHop5)*0.001)
    
    #########################
    
    
    #Hop6
    
    #Calculate Acceleration
    
    Hop6Acc = (Hop6-BW)/input$Mass
    
    Hop6Accavg = rollapply(Hop6Acc, 2, mean)
    Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
    
    #Acceleration Area
    Hop6Accarea = Hop6Accavg2*0.001
    Hop6VelInit = 1.3
    
    #Velocity
    Hop6Vela = Hop6Accarea
    Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
    Hop6Vel = cumsum(Hop6Vela)
    
    #Velocity Area
    Hop6Velavg = rollapply(Hop6Vel, 2, mean)
    Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
    Hop6Velarea = Hop6Velavg2*0.001
    
    #Displacement
    Hop6Disp = cumsum(Hop6Velarea)
    
    l = length(Hop6Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop6Disp[l]>0){
      while (Hop6Disp[l] > 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit - 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)}
    }else (Hop6Disp[l]<0) 
    {
      while (Hop6Disp[l] < 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit + 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop6 = max(Hop6)
    PeakHop6PC = PeakHop6/BW
    
    
    ContactHop6 <- 5 + which(Hop6[5:length(Hop6)] >5)[1]
    ContactTimeHop6 = (length(Hop6Disp) - ContactHop6)*0.001
    MinHop6Disp = min(Hop6Disp)
    EccHop6Disp = (Hop6Disp[ContactHop6] - MinHop6Disp)
    EccStiffHop6 = ((PeakHop6 - BW)/EccHop6Disp)/input$Mass
    
    JumpFreqHop6 = 1/(length(FullHop6)*0.001)
    
    #########################
    
    
    #Hop7
    
    #Calculate Acceleration
    
    Hop7Acc = (Hop7-BW)/input$Mass
    
    Hop7Accavg = rollapply(Hop7Acc, 2, mean)
    Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
    
    #Acceleration Area
    Hop7Accarea = Hop7Accavg2*0.001
    Hop7VelInit = 1.3
    
    #Velocity
    Hop7Vela = Hop7Accarea
    Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
    Hop7Vel = cumsum(Hop7Vela)
    
    #Velocity Area
    Hop7Velavg = rollapply(Hop7Vel, 2, mean)
    Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
    Hop7Velarea = Hop7Velavg2*0.001
    
    #Displacement
    Hop7Disp = cumsum(Hop7Velarea)
    
    l = length(Hop7Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop7Disp[l]>0){
      while (Hop7Disp[l] > 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit - 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)}
    }else (Hop7Disp[l]<0) 
    {
      while (Hop7Disp[l] < 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit + 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop7 = max(Hop7)
    PeakHop7PC = PeakHop7/BW
    
    
    ContactHop7 <- 5 + which(Hop7[5:length(Hop7)] >5)[1]
    ContactTimeHop7 = (length(Hop7Disp) - ContactHop7)*0.001
    MinHop7Disp = min(Hop7Disp)
    EccHop7Disp = (Hop7Disp[ContactHop7] - MinHop7Disp)
    EccStiffHop7 = ((PeakHop7 - BW)/EccHop7Disp)/input$Mass
    
    JumpFreqHop7 = 1/(length(FullHop7)*0.001)
    
    #########################
    
    #Hop8
    
    #Calculate Acceleration
    
    Hop8Acc = (Hop8-BW)/input$Mass
    
    Hop8Accavg = rollapply(Hop8Acc, 2, mean)
    Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
    
    #Acceleration Area
    Hop8Accarea = Hop8Accavg2*0.001
    Hop8VelInit = 1.3
    
    #Velocity
    Hop8Vela = Hop8Accarea
    Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
    Hop8Vel = cumsum(Hop8Vela)
    
    #Velocity Area
    Hop8Velavg = rollapply(Hop8Vel, 2, mean)
    Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
    Hop8Velarea = Hop8Velavg2*0.001
    
    #Displacement
    Hop8Disp = cumsum(Hop8Velarea)
    
    l = length(Hop8Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop8Disp[l]>0){
      while (Hop8Disp[l] > 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit - 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)}
    }else (Hop8Disp[l]<0) 
    {
      while (Hop8Disp[l] < 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit + 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop8 = max(Hop8)
    PeakHop8PC = PeakHop8/BW
    
    
    ContactHop8 <- 5 + which(Hop8[5:length(Hop8)] >5)[1]
    ContactTimeHop8 = (length(Hop8Disp) - ContactHop8)*0.001
    MinHop8Disp = min(Hop8Disp)
    EccHop8Disp = (Hop8Disp[ContactHop8] - MinHop8Disp)
    EccStiffHop8 = ((PeakHop8 - BW)/EccHop8Disp)/input$Mass
    
    JumpFreqHop8 = 1/(length(FullHop8)*0.001)
    
    #########################
    
    #Hop9
    
    #Calculate Acceleration
    
    Hop9Acc = (Hop9-BW)/input$Mass
    
    Hop9Accavg = rollapply(Hop9Acc, 2, mean)
    Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
    
    #Acceleration Area
    Hop9Accarea = Hop9Accavg2*0.001
    Hop9VelInit = 1.3
    
    #Velocity
    Hop9Vela = Hop9Accarea
    Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
    Hop9Vel = cumsum(Hop9Vela)
    
    #Velocity Area
    Hop9Velavg = rollapply(Hop9Vel, 2, mean)
    Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
    Hop9Velarea = Hop9Velavg2*0.001
    
    #Displacement
    Hop9Disp = cumsum(Hop9Velarea)
    
    l = length(Hop9Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop9Disp[l]>0){
      while (Hop9Disp[l] > 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit - 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)}
    }else (Hop9Disp[l]<0) 
    {
      while (Hop9Disp[l] < 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit + 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop9 = max(Hop9)
    PeakHop9PC = PeakHop9/BW
    
    
    ContactHop9 <- 5 + which(Hop9[5:length(Hop9)] >5)[1]
    ContactTimeHop9 = (length(Hop9Disp) - ContactHop9)*0.001
    MinHop9Disp = min(Hop9Disp)
    EccHop9Disp = (Hop9Disp[ContactHop9] - MinHop9Disp)
    EccStiffHop9 = ((PeakHop9 - BW)/EccHop9Disp)/input$Mass
    
    JumpFreqHop9 = 1/(length(FullHop9)*0.001)
    
    #########################
    
    #Hop10
    
    #Calculate Acceleration
    
    Hop10Acc = (Hop10-BW)/input$Mass
    
    Hop10Accavg = rollapply(Hop10Acc, 2, mean)
    Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
    
    #Acceleration Area
    Hop10Accarea = Hop10Accavg2*0.001
    Hop10VelInit = 1.3
    
    #Velocity
    Hop10Vela = Hop10Accarea
    Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
    Hop10Vel = cumsum(Hop10Vela)
    
    #Velocity Area
    Hop10Velavg = rollapply(Hop10Vel, 2, mean)
    Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
    Hop10Velarea = Hop10Velavg2*0.001
    
    #Displacement
    Hop10Disp = cumsum(Hop10Velarea)
    
    l = length(Hop10Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop10Disp[l]>0){
      while (Hop10Disp[l] > 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit - 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)}
    }else (Hop10Disp[l]<0) 
    {
      while (Hop10Disp[l] < 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit + 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop10 = max(Hop10)
    PeakHop10PC = PeakHop10/BW
    
    
    ContactHop10 <- 5 + which(Hop10[5:length(Hop10)] >5)[1]
    ContactTimeHop10 = (length(Hop10Disp) - ContactHop10)*0.001
    MinHop10Disp = min(Hop10Disp)
    EccHop10Disp = (Hop10Disp[ContactHop10] - MinHop10Disp)
    EccStiffHop10 = ((PeakHop10 - BW)/EccHop10Disp)/input$Mass
    
    JumpFreqHop10 = 1/(length(FullHop10)*0.001)
    
    #########################
    
    #Hop11
    
    #Calculate Acceleration
    
    Hop11Acc = (Hop11-BW)/input$Mass
    
    Hop11Accavg = rollapply(Hop11Acc, 2, mean)
    Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
    
    #Acceleration Area
    Hop11Accarea = Hop11Accavg2*0.001
    Hop11VelInit = 1.3
    
    #Velocity
    Hop11Vela = Hop11Accarea
    Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
    Hop11Vel = cumsum(Hop11Vela)
    
    #Velocity Area
    Hop11Velavg = rollapply(Hop11Vel, 2, mean)
    Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
    Hop11Velarea = Hop11Velavg2*0.001
    
    #Displacement
    Hop11Disp = cumsum(Hop11Velarea)
    
    l = length(Hop11Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop11Disp[l]>0){
      while (Hop11Disp[l] > 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit - 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)}
    }else (Hop11Disp[l]<0) 
    {
      while (Hop11Disp[l] < 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit + 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop11 = max(Hop11)
    PeakHop11PC = PeakHop11/BW
    
    
    ContactHop11 <- 5 + which(Hop11[5:length(Hop11)] >5)[1]
    ContactTimeHop11 = (length(Hop11Disp) - ContactHop11)*0.001
    MinHop11Disp = min(Hop11Disp)
    EccHop11Disp = (Hop11Disp[ContactHop11] - MinHop11Disp)
    EccStiffHop11 = ((PeakHop11 - BW)/EccHop11Disp)/input$Mass
    
    JumpFreqHop11 = 1/(length(FullHop11)*0.001)
    
    #########################
    
    #Hop12
    
    #Calculate Acceleration
    
    Hop12Acc = (Hop12-BW)/input$Mass
    
    Hop12Accavg = rollapply(Hop12Acc, 2, mean)
    Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
    
    #Acceleration Area
    Hop12Accarea = Hop12Accavg2*0.001
    Hop12VelInit = 1.3
    
    #Velocity
    Hop12Vela = Hop12Accarea
    Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
    Hop12Vel = cumsum(Hop12Vela)
    
    #Velocity Area
    Hop12Velavg = rollapply(Hop12Vel, 2, mean)
    Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
    Hop12Velarea = Hop12Velavg2*0.001
    
    #Displacement
    Hop12Disp = cumsum(Hop12Velarea)
    
    l = length(Hop12Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop12Disp[l]>0){
      while (Hop12Disp[l] > 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit - 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)}
    }else (Hop12Disp[l]<0) 
    {
      while (Hop12Disp[l] < 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit + 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop12 = max(Hop12)
    PeakHop12PC = PeakHop12/BW
    
    
    ContactHop12 <- 5 + which(Hop12[5:length(Hop12)] >5)[1]
    ContactTimeHop12 = (length(Hop12Disp) - ContactHop12)*0.001
    MinHop12Disp = min(Hop12Disp)
    EccHop12Disp = (Hop12Disp[ContactHop12] - MinHop12Disp)
    EccStiffHop12 = ((PeakHop12 - BW)/EccHop12Disp)/input$Mass
    
    JumpFreqHop12 = 1/(length(FullHop12)*0.001)
    
    #########################
    
    #Hop13
    
    #Calculate Acceleration
    
    Hop13Acc = (Hop13-BW)/input$Mass
    
    Hop13Accavg = rollapply(Hop13Acc, 2, mean)
    Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
    
    #Acceleration Area
    Hop13Accarea = Hop13Accavg2*0.001
    Hop13VelInit = 1.3
    
    #Velocity
    Hop13Vela = Hop13Accarea
    Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
    Hop13Vel = cumsum(Hop13Vela)
    
    #Velocity Area
    Hop13Velavg = rollapply(Hop13Vel, 2, mean)
    Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
    Hop13Velarea = Hop13Velavg2*0.001
    
    #Displacement
    Hop13Disp = cumsum(Hop13Velarea)
    
    l = length(Hop13Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop13Disp[l]>0){
      while (Hop13Disp[l] > 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit - 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)}
    }else (Hop13Disp[l]<0) 
    {
      while (Hop13Disp[l] < 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit + 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop13 = max(Hop13)
    PeakHop13PC = PeakHop13/BW
    
    
    ContactHop13 <- 5 + which(Hop13[5:length(Hop13)] >5)[1]
    ContactTimeHop13 = (length(Hop13Disp) - ContactHop13)*0.001
    MinHop13Disp = min(Hop13Disp)
    EccHop13Disp = (Hop13Disp[ContactHop13] - MinHop13Disp)
    EccStiffHop13 = ((PeakHop13 - BW)/EccHop13Disp)/input$Mass
    
    JumpFreqHop13 = 1/(length(FullHop13)*0.001)
    
    #########################
    
    #Hop14
    
    #Calculate Acceleration
    
    Hop14Acc = (Hop14-BW)/input$Mass
    
    Hop14Accavg = rollapply(Hop14Acc, 2, mean)
    Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
    
    #Acceleration Area
    Hop14Accarea = Hop14Accavg2*0.001
    Hop14VelInit = 1.3
    
    #Velocity
    Hop14Vela = Hop14Accarea
    Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
    Hop14Vel = cumsum(Hop14Vela)
    
    #Velocity Area
    Hop14Velavg = rollapply(Hop14Vel, 2, mean)
    Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
    Hop14Velarea = Hop14Velavg2*0.001
    
    #Displacement
    Hop14Disp = cumsum(Hop14Velarea)
    
    l = length(Hop14Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop14Disp[l]>0){
      while (Hop14Disp[l] > 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit - 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)}
    }else (Hop14Disp[l]<0) 
    {
      while (Hop14Disp[l] < 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit + 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop14 = max(Hop14)
    PeakHop14PC = PeakHop14/BW
    
    
    ContactHop14 <- 5 + which(Hop14[5:length(Hop14)] >5)[1]
    ContactTimeHop14 = (length(Hop14Disp) - ContactHop14)*0.001
    MinHop14Disp = min(Hop14Disp)
    EccHop14Disp = (Hop14Disp[ContactHop14] - MinHop14Disp)
    EccStiffHop14 = ((PeakHop14 - BW)/EccHop14Disp)/input$Mass
    
    JumpFreqHop14 = 1/(length(FullHop14)*0.001)
    
    #########################
    
    # #Hop15
    # 
    # #Calculate Acceleration
    # 
    # Hop15Acc = (Hop15-BW)/input$Mass
    # 
    # Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    # Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    # 
    # #Acceleration Area
    # Hop15Accarea = Hop15Accavg2*0.001
    # Hop15VelInit = 1.3
    # 
    # #Velocity
    # Hop15Vela = Hop15Accarea
    # Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    # Hop15Vel = cumsum(Hop15Vela)
    # 
    # #Velocity Area
    # Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    # Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    # Hop15Velarea = Hop15Velavg2*0.001
    # 
    # #Displacement
    # Hop15Disp = cumsum(Hop15Velarea)
    # 
    # l = length(Hop15Disp)
    # 
    # #Adjust Init Velocity to get Final Displacment close to zero
    # if (Hop15Disp[l]>0){
    #   while (Hop15Disp[l] > 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit - 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)}
    # }else (Hop15Disp[l]<0) 
    # {
    #   while (Hop15Disp[l] < 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit + 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)
    #   }
    # }
    # 
    # 
    # #Calculate output variables
    # PeakHop15 = max(Hop15)
    # PeakHop15PC = PeakHop15/BW
    # 
    # 
    # ContactHop15 <- 5 + which(Hop15[5:length(Hop15)] >5)[1]
    # ContactTimeHop15 = (length(Hop15Disp) - ContactHop15)*0.001
    # MinHop15Disp = min(Hop15Disp)
    # EccHop15Disp = (Hop15Disp[ContactHop15] - MinHop15Disp)
    # EccStiffHop15 = ((PeakHop15 - BW)/EccHop15Disp)/input$Mass
    # 
    # JumpFreqHop15 = 1/(length(FullHop15)*0.001)
    
    
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14", "Average")
    EccCOMDisp <- rbind(round(EccHop1Disp,digits=2), round(EccHop2Disp,digits=2), round(EccHop3Disp,digits=2), round(EccHop4Disp,digits=2), round(EccHop5Disp,digits=2), round(EccHop6Disp,digits=2), round(EccHop7Disp,digits=2), round(EccHop8Disp,digits=2), round(EccHop9Disp,digits=2), round(EccHop10Disp,digits=2), round(EccHop11Disp,digits=2), round(EccHop12Disp,digits=2), round(EccHop13Disp,digits=2), round(EccHop14Disp,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2))
    PeakForcePC <- rbind(round(PeakHop1PC,digits=2), round(PeakHop2PC,digits=2), round(PeakHop3PC,digits=2), round(PeakHop4PC,digits=2), round(PeakHop5PC,digits=2), round(PeakHop6PC,digits=2), round(PeakHop7PC,digits=2), round(PeakHop8PC,digits=2), round(PeakHop9PC,digits=2), round(PeakHop10PC,digits=2), round(PeakHop11PC,digits=2), round(PeakHop12PC,digits=2), round(PeakHop13PC,digits=2), round(PeakHop14PC,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    Left2.6Averages <<-cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
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
    
  })
  output$resultstableLeftSS <- renderTable({
    
    data1 <- 
      read.csv(input$LeftSS$datapath, stringsAsFactors = F, skip = 17)
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <5)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    for (r in 1:15){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <5)
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
        geom_vline(xintercept=Contact[15], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHop2 <- Fz2$Fz2[Contact[2]:Contact[3]]
    FullHop3 <- Fz2$Fz2[Contact[3]:Contact[4]]
    FullHop4 <- Fz2$Fz2[Contact[4]:Contact[5]]
    FullHop5 <- Fz2$Fz2[Contact[5]:Contact[6]]
    FullHop6 <- Fz2$Fz2[Contact[6]:Contact[7]]
    FullHop7 <- Fz2$Fz2[Contact[7]:Contact[8]]
    FullHop8 <- Fz2$Fz2[Contact[8]:Contact[9]]
    FullHop9 <- Fz2$Fz2[Contact[9]:Contact[10]]
    FullHop10 <- Fz2$Fz2[Contact[10]:Contact[11]]
    FullHop11 <- Fz2$Fz2[Contact[11]:Contact[12]]
    FullHop12 <- Fz2$Fz2[Contact[12]:Contact[13]]
    FullHop13 <- Fz2$Fz2[Contact[13]:Contact[14]]
    FullHop14 <- Fz2$Fz2[Contact[14]:Contact[15]]
    #    FullHop15 <- Fz2$Fz2[Contact[15]:Contact[16]]
    
    Hop1 <- Fz2$Fz2[Flight[1]:Flight[2]]
    Hop2 <- Fz2$Fz2[Flight[2]:Flight[3]]
    Hop3 <- Fz2$Fz2[Flight[3]:Flight[4]]
    Hop4 <- Fz2$Fz2[Flight[4]:Flight[5]]
    Hop5 <- Fz2$Fz2[Flight[5]:Flight[6]]
    Hop6 <- Fz2$Fz2[Flight[6]:Flight[7]]
    Hop7 <- Fz2$Fz2[Flight[7]:Flight[8]]
    Hop8 <- Fz2$Fz2[Flight[8]:Flight[9]]
    Hop9 <- Fz2$Fz2[Flight[9]:Flight[10]]
    Hop10 <- Fz2$Fz2[Flight[10]:Flight[11]]
    Hop11 <- Fz2$Fz2[Flight[11]:Flight[12]]
    Hop12 <- Fz2$Fz2[Flight[12]:Flight[13]]
    Hop13 <- Fz2$Fz2[Flight[13]:Flight[14]]
    Hop14 <- Fz2$Fz2[Flight[14]:Flight[15]]
    Hop15 <- Fz2$Fz2[Flight[15]:Flight[16]]
    #    Hop16 <- Fz2$Fz2[Flight[16]:Flight[17]]
    
    # windows()
    # plot(FullHop1)
    # windows()
    # plot(FullHop2)
    # windows()
    # plot(FullHop3)
    # windows()
    # plot(FullHop4)
    # windows()
    # plot(FullHop5)
    # windows()
    # plot(FullHop6)
    # windows()
    # plot(FullHop7)
    # windows()
    # plot(FullHop8)
    # windows()
    # plot(FullHop9)
    # windows()
    # plot(FullHop10)
    # windows()
    # plot(FullHop11)
    # windows()
    # plot(FullHop12)
    # windows()
    # plot(FullHop13)
    # windows()
    # plot(FullHop14)
    # windows()
    # plot(FullHop15)
    
    #Hop1
    
    #Calculate Acceleration
    
    Hop1Acc = (Hop1-BW)/input$Mass
    
    Hop1Accavg = rollapply(Hop1Acc, 2, mean)
    Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
    
    #Acceleration Area
    Hop1Accarea = Hop1Accavg2*0.001
    Hop1VelInit = 1.3
    
    #Velocity
    Hop1Vela = Hop1Accarea
    Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
    Hop1Vel = cumsum(Hop1Vela)
    
    #Velocity Area
    Hop1Velavg = rollapply(Hop1Vel, 2, mean)
    Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
    Hop1Velarea = Hop1Velavg2*0.001
    
    #Displacement
    Hop1Disp = cumsum(Hop1Velarea)
    
    l = length(Hop1Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop1Disp[l]>0){
      while (Hop1Disp[l] > 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit - 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)}
    }else (Hop1Disp[l]<0) 
    {
      while (Hop1Disp[l] < 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit + 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop1 = max(Hop1)
    PeakHop1PC = PeakHop1/BW
    
    
    ContactHop1 <- 5 + which(Hop1[5:length(Hop1)] >5)[1]
    ContactTimeHop1 = (length(Hop1Disp) - ContactHop1)*0.001
    MinHop1Disp = min(Hop1Disp)
    EccHop1Disp = (Hop1Disp[ContactHop1] - MinHop1Disp)
    EccStiffHop1 = ((PeakHop1 - BW)/EccHop1Disp)/input$Mass
    
    JumpFreqHop1 = 1/(length(FullHop1)*0.001)
    
    #####################
    
    #Hop2
    
    #Calculate Acceleration
    
    Hop2Acc = (Hop2-BW)/input$Mass
    
    Hop2Accavg = rollapply(Hop2Acc, 2, mean)
    Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
    
    #Acceleration Area
    Hop2Accarea = Hop2Accavg2*0.001
    Hop2VelInit = 1.3
    
    #Velocity
    Hop2Vela = Hop2Accarea
    Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
    Hop2Vel = cumsum(Hop2Vela)
    
    #Velocity Area
    Hop2Velavg = rollapply(Hop2Vel, 2, mean)
    Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
    Hop2Velarea = Hop2Velavg2*0.001
    
    #Displacement
    Hop2Disp = cumsum(Hop2Velarea)
    
    l = length(Hop2Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop2Disp[l]>0){
      while (Hop2Disp[l] > 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit - 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)}
    }else (Hop2Disp[l]<0) 
    {
      while (Hop2Disp[l] < 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit + 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop2 = max(Hop2)
    PeakHop2PC = PeakHop2/BW
    
    
    ContactHop2 <- 5 + which(Hop2[5:length(Hop2)] >5)[1]
    ContactTimeHop2 = (length(Hop2Disp) - ContactHop2)*0.001
    MinHop2Disp = min(Hop2Disp)
    EccHop2Disp = (Hop2Disp[ContactHop2] - MinHop2Disp)
    EccStiffHop2 = ((PeakHop2 - BW)/EccHop2Disp)/input$Mass
    
    JumpFreqHop2 = 1/(length(FullHop2)*0.001)
    
    #########################
    
    #Hop3
    
    #Calculate Acceleration
    
    Hop3Acc = (Hop3-BW)/input$Mass
    
    Hop3Accavg = rollapply(Hop3Acc, 2, mean)
    Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
    
    #Acceleration Area
    Hop3Accarea = Hop3Accavg2*0.001
    Hop3VelInit = 1.3
    
    #Velocity
    Hop3Vela = Hop3Accarea
    Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
    Hop3Vel = cumsum(Hop3Vela)
    
    #Velocity Area
    Hop3Velavg = rollapply(Hop3Vel, 2, mean)
    Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
    Hop3Velarea = Hop3Velavg2*0.001
    
    #Displacement
    Hop3Disp = cumsum(Hop3Velarea)
    
    l = length(Hop3Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop3Disp[l]>0){
      while (Hop3Disp[l] > 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit - 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)}
    }else (Hop3Disp[l]<0) 
    {
      while (Hop3Disp[l] < 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit + 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop3 = max(Hop3)
    PeakHop3PC = PeakHop3/BW
    
    
    ContactHop3 <- 5 + which(Hop3[5:length(Hop3)] >5)[1]
    ContactTimeHop3 = (length(Hop3Disp) - ContactHop3)*0.001
    MinHop3Disp = min(Hop3Disp)
    EccHop3Disp = (Hop3Disp[ContactHop3] - MinHop3Disp)
    EccStiffHop3 = ((PeakHop3 - BW)/EccHop3Disp)/input$Mass
    
    JumpFreqHop3 = 1/(length(FullHop3)*0.001)
    
    #########################
    
    #Hop4
    
    #Calculate Acceleration
    
    Hop4Acc = (Hop4-BW)/input$Mass
    
    Hop4Accavg = rollapply(Hop4Acc, 2, mean)
    Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
    
    #Acceleration Area
    Hop4Accarea = Hop4Accavg2*0.001
    Hop4VelInit = 1.3
    
    #Velocity
    Hop4Vela = Hop4Accarea
    Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
    Hop4Vel = cumsum(Hop4Vela)
    
    #Velocity Area
    Hop4Velavg = rollapply(Hop4Vel, 2, mean)
    Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
    Hop4Velarea = Hop4Velavg2*0.001
    
    #Displacement
    Hop4Disp = cumsum(Hop4Velarea)
    
    l = length(Hop4Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop4Disp[l]>0){
      while (Hop4Disp[l] > 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit - 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)}
    }else (Hop4Disp[l]<0) 
    {
      while (Hop4Disp[l] < 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit + 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop4 = max(Hop4)
    PeakHop4PC = PeakHop4/BW
    
    
    ContactHop4 <- 5 + which(Hop4[5:length(Hop4)] >5)[1]
    ContactTimeHop4 = (length(Hop4Disp) - ContactHop4)*0.001
    MinHop4Disp = min(Hop4Disp)
    EccHop4Disp = (Hop4Disp[ContactHop4] - MinHop4Disp)
    EccStiffHop4 = ((PeakHop4 - BW)/EccHop4Disp)/input$Mass
    
    JumpFreqHop4 = 1/(length(FullHop4)*0.001)
    
    #########################
    
    #Hop5
    
    #Calculate Acceleration
    
    Hop5Acc = (Hop5-BW)/input$Mass
    
    Hop5Accavg = rollapply(Hop5Acc, 2, mean)
    Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
    
    #Acceleration Area
    Hop5Accarea = Hop5Accavg2*0.001
    Hop5VelInit = 1.3
    
    #Velocity
    Hop5Vela = Hop5Accarea
    Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
    Hop5Vel = cumsum(Hop5Vela)
    
    #Velocity Area
    Hop5Velavg = rollapply(Hop5Vel, 2, mean)
    Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
    Hop5Velarea = Hop5Velavg2*0.001
    
    #Displacement
    Hop5Disp = cumsum(Hop5Velarea)
    
    l = length(Hop5Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop5Disp[l]>0){
      while (Hop5Disp[l] > 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit - 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)}
    }else (Hop5Disp[l]<0) 
    {
      while (Hop5Disp[l] < 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit + 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop5 = max(Hop5)
    PeakHop5PC = PeakHop5/BW
    
    
    ContactHop5 <- 5 + which(Hop5[5:length(Hop5)] >5)[1]
    ContactTimeHop5 = (length(Hop5Disp) - ContactHop5)*0.001
    MinHop5Disp = min(Hop5Disp)
    EccHop5Disp = (Hop5Disp[ContactHop5] - MinHop5Disp)
    EccStiffHop5 = ((PeakHop5 - BW)/EccHop5Disp)/input$Mass
    
    JumpFreqHop5 = 1/(length(FullHop5)*0.001)
    
    #########################
    
    
    #Hop6
    
    #Calculate Acceleration
    
    Hop6Acc = (Hop6-BW)/input$Mass
    
    Hop6Accavg = rollapply(Hop6Acc, 2, mean)
    Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
    
    #Acceleration Area
    Hop6Accarea = Hop6Accavg2*0.001
    Hop6VelInit = 1.3
    
    #Velocity
    Hop6Vela = Hop6Accarea
    Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
    Hop6Vel = cumsum(Hop6Vela)
    
    #Velocity Area
    Hop6Velavg = rollapply(Hop6Vel, 2, mean)
    Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
    Hop6Velarea = Hop6Velavg2*0.001
    
    #Displacement
    Hop6Disp = cumsum(Hop6Velarea)
    
    l = length(Hop6Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop6Disp[l]>0){
      while (Hop6Disp[l] > 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit - 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)}
    }else (Hop6Disp[l]<0) 
    {
      while (Hop6Disp[l] < 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit + 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop6 = max(Hop6)
    PeakHop6PC = PeakHop6/BW
    
    
    ContactHop6 <- 5 + which(Hop6[5:length(Hop6)] >5)[1]
    ContactTimeHop6 = (length(Hop6Disp) - ContactHop6)*0.001
    MinHop6Disp = min(Hop6Disp)
    EccHop6Disp = (Hop6Disp[ContactHop6] - MinHop6Disp)
    EccStiffHop6 = ((PeakHop6 - BW)/EccHop6Disp)/input$Mass
    
    JumpFreqHop6 = 1/(length(FullHop6)*0.001)
    
    #########################
    
    
    #Hop7
    
    #Calculate Acceleration
    
    Hop7Acc = (Hop7-BW)/input$Mass
    
    Hop7Accavg = rollapply(Hop7Acc, 2, mean)
    Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
    
    #Acceleration Area
    Hop7Accarea = Hop7Accavg2*0.001
    Hop7VelInit = 1.3
    
    #Velocity
    Hop7Vela = Hop7Accarea
    Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
    Hop7Vel = cumsum(Hop7Vela)
    
    #Velocity Area
    Hop7Velavg = rollapply(Hop7Vel, 2, mean)
    Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
    Hop7Velarea = Hop7Velavg2*0.001
    
    #Displacement
    Hop7Disp = cumsum(Hop7Velarea)
    
    l = length(Hop7Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop7Disp[l]>0){
      while (Hop7Disp[l] > 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit - 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)}
    }else (Hop7Disp[l]<0) 
    {
      while (Hop7Disp[l] < 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit + 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop7 = max(Hop7)
    PeakHop7PC = PeakHop7/BW
    
    
    ContactHop7 <- 5 + which(Hop7[5:length(Hop7)] >5)[1]
    ContactTimeHop7 = (length(Hop7Disp) - ContactHop7)*0.001
    MinHop7Disp = min(Hop7Disp)
    EccHop7Disp = (Hop7Disp[ContactHop7] - MinHop7Disp)
    EccStiffHop7 = ((PeakHop7 - BW)/EccHop7Disp)/input$Mass
    
    JumpFreqHop7 = 1/(length(FullHop7)*0.001)
    
    #########################
    
    #Hop8
    
    #Calculate Acceleration
    
    Hop8Acc = (Hop8-BW)/input$Mass
    
    Hop8Accavg = rollapply(Hop8Acc, 2, mean)
    Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
    
    #Acceleration Area
    Hop8Accarea = Hop8Accavg2*0.001
    Hop8VelInit = 1.3
    
    #Velocity
    Hop8Vela = Hop8Accarea
    Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
    Hop8Vel = cumsum(Hop8Vela)
    
    #Velocity Area
    Hop8Velavg = rollapply(Hop8Vel, 2, mean)
    Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
    Hop8Velarea = Hop8Velavg2*0.001
    
    #Displacement
    Hop8Disp = cumsum(Hop8Velarea)
    
    l = length(Hop8Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop8Disp[l]>0){
      while (Hop8Disp[l] > 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit - 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)}
    }else (Hop8Disp[l]<0) 
    {
      while (Hop8Disp[l] < 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit + 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop8 = max(Hop8)
    PeakHop8PC = PeakHop8/BW
    
    
    ContactHop8 <- 5 + which(Hop8[5:length(Hop8)] >5)[1]
    ContactTimeHop8 = (length(Hop8Disp) - ContactHop8)*0.001
    MinHop8Disp = min(Hop8Disp)
    EccHop8Disp = (Hop8Disp[ContactHop8] - MinHop8Disp)
    EccStiffHop8 = ((PeakHop8 - BW)/EccHop8Disp)/input$Mass
    
    JumpFreqHop8 = 1/(length(FullHop8)*0.001)
    
    #########################
    
    #Hop9
    
    #Calculate Acceleration
    
    Hop9Acc = (Hop9-BW)/input$Mass
    
    Hop9Accavg = rollapply(Hop9Acc, 2, mean)
    Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
    
    #Acceleration Area
    Hop9Accarea = Hop9Accavg2*0.001
    Hop9VelInit = 1.3
    
    #Velocity
    Hop9Vela = Hop9Accarea
    Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
    Hop9Vel = cumsum(Hop9Vela)
    
    #Velocity Area
    Hop9Velavg = rollapply(Hop9Vel, 2, mean)
    Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
    Hop9Velarea = Hop9Velavg2*0.001
    
    #Displacement
    Hop9Disp = cumsum(Hop9Velarea)
    
    l = length(Hop9Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop9Disp[l]>0){
      while (Hop9Disp[l] > 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit - 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)}
    }else (Hop9Disp[l]<0) 
    {
      while (Hop9Disp[l] < 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit + 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop9 = max(Hop9)
    PeakHop9PC = PeakHop9/BW
    
    
    ContactHop9 <- 5 + which(Hop9[5:length(Hop9)] >5)[1]
    ContactTimeHop9 = (length(Hop9Disp) - ContactHop9)*0.001
    MinHop9Disp = min(Hop9Disp)
    EccHop9Disp = (Hop9Disp[ContactHop9] - MinHop9Disp)
    EccStiffHop9 = ((PeakHop9 - BW)/EccHop9Disp)/input$Mass
    
    JumpFreqHop9 = 1/(length(FullHop9)*0.001)
    
    #########################
    
    #Hop10
    
    #Calculate Acceleration
    
    Hop10Acc = (Hop10-BW)/input$Mass
    
    Hop10Accavg = rollapply(Hop10Acc, 2, mean)
    Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
    
    #Acceleration Area
    Hop10Accarea = Hop10Accavg2*0.001
    Hop10VelInit = 1.3
    
    #Velocity
    Hop10Vela = Hop10Accarea
    Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
    Hop10Vel = cumsum(Hop10Vela)
    
    #Velocity Area
    Hop10Velavg = rollapply(Hop10Vel, 2, mean)
    Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
    Hop10Velarea = Hop10Velavg2*0.001
    
    #Displacement
    Hop10Disp = cumsum(Hop10Velarea)
    
    l = length(Hop10Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop10Disp[l]>0){
      while (Hop10Disp[l] > 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit - 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)}
    }else (Hop10Disp[l]<0) 
    {
      while (Hop10Disp[l] < 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit + 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop10 = max(Hop10)
    PeakHop10PC = PeakHop10/BW
    
    
    ContactHop10 <- 5 + which(Hop10[5:length(Hop10)] >5)[1]
    ContactTimeHop10 = (length(Hop10Disp) - ContactHop10)*0.001
    MinHop10Disp = min(Hop10Disp)
    EccHop10Disp = (Hop10Disp[ContactHop10] - MinHop10Disp)
    EccStiffHop10 = ((PeakHop10 - BW)/EccHop10Disp)/input$Mass
    
    JumpFreqHop10 = 1/(length(FullHop10)*0.001)
    
    #########################
    
    #Hop11
    
    #Calculate Acceleration
    
    Hop11Acc = (Hop11-BW)/input$Mass
    
    Hop11Accavg = rollapply(Hop11Acc, 2, mean)
    Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
    
    #Acceleration Area
    Hop11Accarea = Hop11Accavg2*0.001
    Hop11VelInit = 1.3
    
    #Velocity
    Hop11Vela = Hop11Accarea
    Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
    Hop11Vel = cumsum(Hop11Vela)
    
    #Velocity Area
    Hop11Velavg = rollapply(Hop11Vel, 2, mean)
    Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
    Hop11Velarea = Hop11Velavg2*0.001
    
    #Displacement
    Hop11Disp = cumsum(Hop11Velarea)
    
    l = length(Hop11Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop11Disp[l]>0){
      while (Hop11Disp[l] > 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit - 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)}
    }else (Hop11Disp[l]<0) 
    {
      while (Hop11Disp[l] < 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit + 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop11 = max(Hop11)
    PeakHop11PC = PeakHop11/BW
    
    
    ContactHop11 <- 5 + which(Hop11[5:length(Hop11)] >5)[1]
    ContactTimeHop11 = (length(Hop11Disp) - ContactHop11)*0.001
    MinHop11Disp = min(Hop11Disp)
    EccHop11Disp = (Hop11Disp[ContactHop11] - MinHop11Disp)
    EccStiffHop11 = ((PeakHop11 - BW)/EccHop11Disp)/input$Mass
    
    JumpFreqHop11 = 1/(length(FullHop11)*0.001)
    
    #########################
    
    #Hop12
    
    #Calculate Acceleration
    
    Hop12Acc = (Hop12-BW)/input$Mass
    
    Hop12Accavg = rollapply(Hop12Acc, 2, mean)
    Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
    
    #Acceleration Area
    Hop12Accarea = Hop12Accavg2*0.001
    Hop12VelInit = 1.3
    
    #Velocity
    Hop12Vela = Hop12Accarea
    Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
    Hop12Vel = cumsum(Hop12Vela)
    
    #Velocity Area
    Hop12Velavg = rollapply(Hop12Vel, 2, mean)
    Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
    Hop12Velarea = Hop12Velavg2*0.001
    
    #Displacement
    Hop12Disp = cumsum(Hop12Velarea)
    
    l = length(Hop12Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop12Disp[l]>0){
      while (Hop12Disp[l] > 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit - 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)}
    }else (Hop12Disp[l]<0) 
    {
      while (Hop12Disp[l] < 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit + 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop12 = max(Hop12)
    PeakHop12PC = PeakHop12/BW
    
    
    ContactHop12 <- 5 + which(Hop12[5:length(Hop12)] >5)[1]
    ContactTimeHop12 = (length(Hop12Disp) - ContactHop12)*0.001
    MinHop12Disp = min(Hop12Disp)
    EccHop12Disp = (Hop12Disp[ContactHop12] - MinHop12Disp)
    EccStiffHop12 = ((PeakHop12 - BW)/EccHop12Disp)/input$Mass
    
    JumpFreqHop12 = 1/(length(FullHop12)*0.001)
    
    #########################
    
    #Hop13
    
    #Calculate Acceleration
    
    Hop13Acc = (Hop13-BW)/input$Mass
    
    Hop13Accavg = rollapply(Hop13Acc, 2, mean)
    Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
    
    #Acceleration Area
    Hop13Accarea = Hop13Accavg2*0.001
    Hop13VelInit = 1.3
    
    #Velocity
    Hop13Vela = Hop13Accarea
    Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
    Hop13Vel = cumsum(Hop13Vela)
    
    #Velocity Area
    Hop13Velavg = rollapply(Hop13Vel, 2, mean)
    Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
    Hop13Velarea = Hop13Velavg2*0.001
    
    #Displacement
    Hop13Disp = cumsum(Hop13Velarea)
    
    l = length(Hop13Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop13Disp[l]>0){
      while (Hop13Disp[l] > 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit - 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)}
    }else (Hop13Disp[l]<0) 
    {
      while (Hop13Disp[l] < 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit + 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop13 = max(Hop13)
    PeakHop13PC = PeakHop13/BW
    
    
    ContactHop13 <- 5 + which(Hop13[5:length(Hop13)] >5)[1]
    ContactTimeHop13 = (length(Hop13Disp) - ContactHop13)*0.001
    MinHop13Disp = min(Hop13Disp)
    EccHop13Disp = (Hop13Disp[ContactHop13] - MinHop13Disp)
    EccStiffHop13 = ((PeakHop13 - BW)/EccHop13Disp)/input$Mass
    
    JumpFreqHop13 = 1/(length(FullHop13)*0.001)
    
    #########################
    
    #Hop14
    
    #Calculate Acceleration
    
    Hop14Acc = (Hop14-BW)/input$Mass
    
    Hop14Accavg = rollapply(Hop14Acc, 2, mean)
    Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
    
    #Acceleration Area
    Hop14Accarea = Hop14Accavg2*0.001
    Hop14VelInit = 1.3
    
    #Velocity
    Hop14Vela = Hop14Accarea
    Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
    Hop14Vel = cumsum(Hop14Vela)
    
    #Velocity Area
    Hop14Velavg = rollapply(Hop14Vel, 2, mean)
    Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
    Hop14Velarea = Hop14Velavg2*0.001
    
    #Displacement
    Hop14Disp = cumsum(Hop14Velarea)
    
    l = length(Hop14Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop14Disp[l]>0){
      while (Hop14Disp[l] > 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit - 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)}
    }else (Hop14Disp[l]<0) 
    {
      while (Hop14Disp[l] < 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit + 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop14 = max(Hop14)
    PeakHop14PC = PeakHop14/BW
    
    
    ContactHop14 <- 5 + which(Hop14[5:length(Hop14)] >5)[1]
    ContactTimeHop14 = (length(Hop14Disp) - ContactHop14)*0.001
    MinHop14Disp = min(Hop14Disp)
    EccHop14Disp = (Hop14Disp[ContactHop14] - MinHop14Disp)
    EccStiffHop14 = ((PeakHop14 - BW)/EccHop14Disp)/input$Mass
    
    JumpFreqHop14 = 1/(length(FullHop14)*0.001)
    
    #########################
    
    # #Hop15
    # 
    # #Calculate Acceleration
    # 
    # Hop15Acc = (Hop15-BW)/input$Mass
    # 
    # Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    # Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    # 
    # #Acceleration Area
    # Hop15Accarea = Hop15Accavg2*0.001
    # Hop15VelInit = 1.3
    # 
    # #Velocity
    # Hop15Vela = Hop15Accarea
    # Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    # Hop15Vel = cumsum(Hop15Vela)
    # 
    # #Velocity Area
    # Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    # Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    # Hop15Velarea = Hop15Velavg2*0.001
    # 
    # #Displacement
    # Hop15Disp = cumsum(Hop15Velarea)
    # 
    # l = length(Hop15Disp)
    # 
    # #Adjust Init Velocity to get Final Displacment close to zero
    # if (Hop15Disp[l]>0){
    #   while (Hop15Disp[l] > 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit - 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)}
    # }else (Hop15Disp[l]<0) 
    # {
    #   while (Hop15Disp[l] < 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit + 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)
    #   }
    # }
    # 
    # 
    # #Calculate output variables
    # PeakHop15 = max(Hop15)
    # PeakHop15PC = PeakHop15/BW
    # 
    # 
    # ContactHop15 <- 5 + which(Hop15[5:length(Hop15)] >5)[1]
    # ContactTimeHop15 = (length(Hop15Disp) - ContactHop15)*0.001
    # MinHop15Disp = min(Hop15Disp)
    # EccHop15Disp = (Hop15Disp[ContactHop15] - MinHop15Disp)
    # EccStiffHop15 = ((PeakHop15 - BW)/EccHop15Disp)/input$Mass
    # 
    # JumpFreqHop15 = 1/(length(FullHop15)*0.001)
    # 
    # 
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14", "Average")
    EccCOMDisp <- rbind(round(EccHop1Disp,digits=2), round(EccHop2Disp,digits=2), round(EccHop3Disp,digits=2), round(EccHop4Disp,digits=2), round(EccHop5Disp,digits=2), round(EccHop6Disp,digits=2), round(EccHop7Disp,digits=2), round(EccHop8Disp,digits=2), round(EccHop9Disp,digits=2), round(EccHop10Disp,digits=2), round(EccHop11Disp,digits=2), round(EccHop12Disp,digits=2), round(EccHop13Disp,digits=2), round(EccHop14Disp,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2))
    PeakForcePC <- rbind(round(PeakHop1PC,digits=2), round(PeakHop2PC,digits=2), round(PeakHop3PC,digits=2), round(PeakHop4PC,digits=2), round(PeakHop5PC,digits=2), round(PeakHop6PC,digits=2), round(PeakHop7PC,digits=2), round(PeakHop8PC,digits=2), round(PeakHop9PC,digits=2), round(PeakHop10PC,digits=2), round(PeakHop11PC,digits=2), round(PeakHop12PC,digits=2), round(PeakHop13PC,digits=2), round(PeakHop14PC,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    LeftSSAverages <<-cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
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
    
  })
  output$resultstableRight1.7 <- renderTable({
    
    data1 <- 
      read.csv(input$Right1.7$datapath, stringsAsFactors = F, skip = 17)
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <5)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    for (r in 1:15){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <5)
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
        geom_vline(xintercept=Contact[15], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHop2 <- Fz2$Fz2[Contact[2]:Contact[3]]
    FullHop3 <- Fz2$Fz2[Contact[3]:Contact[4]]
    FullHop4 <- Fz2$Fz2[Contact[4]:Contact[5]]
    FullHop5 <- Fz2$Fz2[Contact[5]:Contact[6]]
    FullHop6 <- Fz2$Fz2[Contact[6]:Contact[7]]
    FullHop7 <- Fz2$Fz2[Contact[7]:Contact[8]]
    FullHop8 <- Fz2$Fz2[Contact[8]:Contact[9]]
    FullHop9 <- Fz2$Fz2[Contact[9]:Contact[10]]
    FullHop10 <- Fz2$Fz2[Contact[10]:Contact[11]]
    FullHop11 <- Fz2$Fz2[Contact[11]:Contact[12]]
    FullHop12 <- Fz2$Fz2[Contact[12]:Contact[13]]
    FullHop13 <- Fz2$Fz2[Contact[13]:Contact[14]]
    FullHop14 <- Fz2$Fz2[Contact[14]:Contact[15]]
    #    FullHop15 <- Fz2$Fz2[Contact[15]:Contact[16]]
    
    Hop1 <- Fz2$Fz2[Flight[1]:Flight[2]]
    Hop2 <- Fz2$Fz2[Flight[2]:Flight[3]]
    Hop3 <- Fz2$Fz2[Flight[3]:Flight[4]]
    Hop4 <- Fz2$Fz2[Flight[4]:Flight[5]]
    Hop5 <- Fz2$Fz2[Flight[5]:Flight[6]]
    Hop6 <- Fz2$Fz2[Flight[6]:Flight[7]]
    Hop7 <- Fz2$Fz2[Flight[7]:Flight[8]]
    Hop8 <- Fz2$Fz2[Flight[8]:Flight[9]]
    Hop9 <- Fz2$Fz2[Flight[9]:Flight[10]]
    Hop10 <- Fz2$Fz2[Flight[10]:Flight[11]]
    Hop11 <- Fz2$Fz2[Flight[11]:Flight[12]]
    Hop12 <- Fz2$Fz2[Flight[12]:Flight[13]]
    Hop13 <- Fz2$Fz2[Flight[13]:Flight[14]]
    Hop14 <- Fz2$Fz2[Flight[14]:Flight[15]]
    Hop15 <- Fz2$Fz2[Flight[15]:Flight[16]]
    #    Hop16 <- Fz2$Fz2[Flight[16]:Flight[17]]
    
    # windows()
    # plot(FullHop1)
    # windows()
    # plot(FullHop2)
    # windows()
    # plot(FullHop3)
    # windows()
    # plot(FullHop4)
    # windows()
    # plot(FullHop5)
    # windows()
    # plot(FullHop6)
    # windows()
    # plot(FullHop7)
    # windows()
    # plot(FullHop8)
    # windows()
    # plot(FullHop9)
    # windows()
    # plot(FullHop10)
    # windows()
    # plot(FullHop11)
    # windows()
    # plot(FullHop12)
    # windows()
    # plot(FullHop13)
    # windows()
    # plot(FullHop14)
    # windows()
    # plot(FullHop15)
    
    #Hop1
    
    #Calculate Acceleration
    
    Hop1Acc = (Hop1-BW)/input$Mass
    
    Hop1Accavg = rollapply(Hop1Acc, 2, mean)
    Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
    
    #Acceleration Area
    Hop1Accarea = Hop1Accavg2*0.001
    Hop1VelInit = 1.3
    
    #Velocity
    Hop1Vela = Hop1Accarea
    Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
    Hop1Vel = cumsum(Hop1Vela)
    
    #Velocity Area
    Hop1Velavg = rollapply(Hop1Vel, 2, mean)
    Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
    Hop1Velarea = Hop1Velavg2*0.001
    
    #Displacement
    Hop1Disp = cumsum(Hop1Velarea)
    
    l = length(Hop1Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop1Disp[l]>0){
      while (Hop1Disp[l] > 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit - 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)}
    }else (Hop1Disp[l]<0) 
    {
      while (Hop1Disp[l] < 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit + 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop1 = max(Hop1)
    PeakHop1PC = PeakHop1/BW
    
    
    ContactHop1 <- 5 + which(Hop1[5:length(Hop1)] >5)[1]
    ContactTimeHop1 = (length(Hop1Disp) - ContactHop1)*0.001
    MinHop1Disp = min(Hop1Disp)
    EccHop1Disp = (Hop1Disp[ContactHop1] - MinHop1Disp)
    EccStiffHop1 = ((PeakHop1 - BW)/EccHop1Disp)/input$Mass
    
    JumpFreqHop1 = 1/(length(FullHop1)*0.001)
    
    #####################
    
    #Hop2
    
    #Calculate Acceleration
    
    Hop2Acc = (Hop2-BW)/input$Mass
    
    Hop2Accavg = rollapply(Hop2Acc, 2, mean)
    Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
    
    #Acceleration Area
    Hop2Accarea = Hop2Accavg2*0.001
    Hop2VelInit = 1.3
    
    #Velocity
    Hop2Vela = Hop2Accarea
    Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
    Hop2Vel = cumsum(Hop2Vela)
    
    #Velocity Area
    Hop2Velavg = rollapply(Hop2Vel, 2, mean)
    Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
    Hop2Velarea = Hop2Velavg2*0.001
    
    #Displacement
    Hop2Disp = cumsum(Hop2Velarea)
    
    l = length(Hop2Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop2Disp[l]>0){
      while (Hop2Disp[l] > 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit - 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)}
    }else (Hop2Disp[l]<0) 
    {
      while (Hop2Disp[l] < 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit + 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop2 = max(Hop2)
    PeakHop2PC = PeakHop2/BW
    
    
    ContactHop2 <- 5 + which(Hop2[5:length(Hop2)] >5)[1]
    ContactTimeHop2 = (length(Hop2Disp) - ContactHop2)*0.001
    MinHop2Disp = min(Hop2Disp)
    EccHop2Disp = (Hop2Disp[ContactHop2] - MinHop2Disp)
    EccStiffHop2 = ((PeakHop2 - BW)/EccHop2Disp)/input$Mass
    
    JumpFreqHop2 = 1/(length(FullHop2)*0.001)
    
    #########################
    
    #Hop3
    
    #Calculate Acceleration
    
    Hop3Acc = (Hop3-BW)/input$Mass
    
    Hop3Accavg = rollapply(Hop3Acc, 2, mean)
    Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
    
    #Acceleration Area
    Hop3Accarea = Hop3Accavg2*0.001
    Hop3VelInit = 1.3
    
    #Velocity
    Hop3Vela = Hop3Accarea
    Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
    Hop3Vel = cumsum(Hop3Vela)
    
    #Velocity Area
    Hop3Velavg = rollapply(Hop3Vel, 2, mean)
    Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
    Hop3Velarea = Hop3Velavg2*0.001
    
    #Displacement
    Hop3Disp = cumsum(Hop3Velarea)
    
    l = length(Hop3Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop3Disp[l]>0){
      while (Hop3Disp[l] > 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit - 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)}
    }else (Hop3Disp[l]<0) 
    {
      while (Hop3Disp[l] < 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit + 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop3 = max(Hop3)
    PeakHop3PC = PeakHop3/BW
    
    
    ContactHop3 <- 5 + which(Hop3[5:length(Hop3)] >5)[1]
    ContactTimeHop3 = (length(Hop3Disp) - ContactHop3)*0.001
    MinHop3Disp = min(Hop3Disp)
    EccHop3Disp = (Hop3Disp[ContactHop3] - MinHop3Disp)
    EccStiffHop3 = ((PeakHop3 - BW)/EccHop3Disp)/input$Mass
    
    JumpFreqHop3 = 1/(length(FullHop3)*0.001)
    
    #########################
    
    #Hop4
    
    #Calculate Acceleration
    
    Hop4Acc = (Hop4-BW)/input$Mass
    
    Hop4Accavg = rollapply(Hop4Acc, 2, mean)
    Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
    
    #Acceleration Area
    Hop4Accarea = Hop4Accavg2*0.001
    Hop4VelInit = 1.3
    
    #Velocity
    Hop4Vela = Hop4Accarea
    Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
    Hop4Vel = cumsum(Hop4Vela)
    
    #Velocity Area
    Hop4Velavg = rollapply(Hop4Vel, 2, mean)
    Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
    Hop4Velarea = Hop4Velavg2*0.001
    
    #Displacement
    Hop4Disp = cumsum(Hop4Velarea)
    
    l = length(Hop4Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop4Disp[l]>0){
      while (Hop4Disp[l] > 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit - 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)}
    }else (Hop4Disp[l]<0) 
    {
      while (Hop4Disp[l] < 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit + 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop4 = max(Hop4)
    PeakHop4PC = PeakHop4/BW
    
    
    ContactHop4 <- 5 + which(Hop4[5:length(Hop4)] >5)[1]
    ContactTimeHop4 = (length(Hop4Disp) - ContactHop4)*0.001
    MinHop4Disp = min(Hop4Disp)
    EccHop4Disp = (Hop4Disp[ContactHop4] - MinHop4Disp)
    EccStiffHop4 = ((PeakHop4 - BW)/EccHop4Disp)/input$Mass
    
    JumpFreqHop4 = 1/(length(FullHop4)*0.001)
    
    #########################
    
    #Hop5
    
    #Calculate Acceleration
    
    Hop5Acc = (Hop5-BW)/input$Mass
    
    Hop5Accavg = rollapply(Hop5Acc, 2, mean)
    Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
    
    #Acceleration Area
    Hop5Accarea = Hop5Accavg2*0.001
    Hop5VelInit = 1.3
    
    #Velocity
    Hop5Vela = Hop5Accarea
    Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
    Hop5Vel = cumsum(Hop5Vela)
    
    #Velocity Area
    Hop5Velavg = rollapply(Hop5Vel, 2, mean)
    Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
    Hop5Velarea = Hop5Velavg2*0.001
    
    #Displacement
    Hop5Disp = cumsum(Hop5Velarea)
    
    l = length(Hop5Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop5Disp[l]>0){
      while (Hop5Disp[l] > 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit - 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)}
    }else (Hop5Disp[l]<0) 
    {
      while (Hop5Disp[l] < 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit + 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop5 = max(Hop5)
    PeakHop5PC = PeakHop5/BW
    
    
    ContactHop5 <- 5 + which(Hop5[5:length(Hop5)] >5)[1]
    ContactTimeHop5 = (length(Hop5Disp) - ContactHop5)*0.001
    MinHop5Disp = min(Hop5Disp)
    EccHop5Disp = (Hop5Disp[ContactHop5] - MinHop5Disp)
    EccStiffHop5 = ((PeakHop5 - BW)/EccHop5Disp)/input$Mass
    
    JumpFreqHop5 = 1/(length(FullHop5)*0.001)
    
    #########################
    
    
    #Hop6
    
    #Calculate Acceleration
    
    Hop6Acc = (Hop6-BW)/input$Mass
    
    Hop6Accavg = rollapply(Hop6Acc, 2, mean)
    Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
    
    #Acceleration Area
    Hop6Accarea = Hop6Accavg2*0.001
    Hop6VelInit = 1.3
    
    #Velocity
    Hop6Vela = Hop6Accarea
    Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
    Hop6Vel = cumsum(Hop6Vela)
    
    #Velocity Area
    Hop6Velavg = rollapply(Hop6Vel, 2, mean)
    Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
    Hop6Velarea = Hop6Velavg2*0.001
    
    #Displacement
    Hop6Disp = cumsum(Hop6Velarea)
    
    l = length(Hop6Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop6Disp[l]>0){
      while (Hop6Disp[l] > 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit - 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)}
    }else (Hop6Disp[l]<0) 
    {
      while (Hop6Disp[l] < 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit + 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop6 = max(Hop6)
    PeakHop6PC = PeakHop6/BW
    
    
    ContactHop6 <- 5 + which(Hop6[5:length(Hop6)] >5)[1]
    ContactTimeHop6 = (length(Hop6Disp) - ContactHop6)*0.001
    MinHop6Disp = min(Hop6Disp)
    EccHop6Disp = (Hop6Disp[ContactHop6] - MinHop6Disp)
    EccStiffHop6 = ((PeakHop6 - BW)/EccHop6Disp)/input$Mass
    
    JumpFreqHop6 = 1/(length(FullHop6)*0.001)
    
    #########################
    
    
    #Hop7
    
    #Calculate Acceleration
    
    Hop7Acc = (Hop7-BW)/input$Mass
    
    Hop7Accavg = rollapply(Hop7Acc, 2, mean)
    Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
    
    #Acceleration Area
    Hop7Accarea = Hop7Accavg2*0.001
    Hop7VelInit = 1.3
    
    #Velocity
    Hop7Vela = Hop7Accarea
    Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
    Hop7Vel = cumsum(Hop7Vela)
    
    #Velocity Area
    Hop7Velavg = rollapply(Hop7Vel, 2, mean)
    Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
    Hop7Velarea = Hop7Velavg2*0.001
    
    #Displacement
    Hop7Disp = cumsum(Hop7Velarea)
    
    l = length(Hop7Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop7Disp[l]>0){
      while (Hop7Disp[l] > 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit - 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)}
    }else (Hop7Disp[l]<0) 
    {
      while (Hop7Disp[l] < 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit + 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop7 = max(Hop7)
    PeakHop7PC = PeakHop7/BW
    
    
    ContactHop7 <- 5 + which(Hop7[5:length(Hop7)] >5)[1]
    ContactTimeHop7 = (length(Hop7Disp) - ContactHop7)*0.001
    MinHop7Disp = min(Hop7Disp)
    EccHop7Disp = (Hop7Disp[ContactHop7] - MinHop7Disp)
    EccStiffHop7 = ((PeakHop7 - BW)/EccHop7Disp)/input$Mass
    
    JumpFreqHop7 = 1/(length(FullHop7)*0.001)
    
    #########################
    
    #Hop8
    
    #Calculate Acceleration
    
    Hop8Acc = (Hop8-BW)/input$Mass
    
    Hop8Accavg = rollapply(Hop8Acc, 2, mean)
    Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
    
    #Acceleration Area
    Hop8Accarea = Hop8Accavg2*0.001
    Hop8VelInit = 1.3
    
    #Velocity
    Hop8Vela = Hop8Accarea
    Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
    Hop8Vel = cumsum(Hop8Vela)
    
    #Velocity Area
    Hop8Velavg = rollapply(Hop8Vel, 2, mean)
    Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
    Hop8Velarea = Hop8Velavg2*0.001
    
    #Displacement
    Hop8Disp = cumsum(Hop8Velarea)
    
    l = length(Hop8Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop8Disp[l]>0){
      while (Hop8Disp[l] > 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit - 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)}
    }else (Hop8Disp[l]<0) 
    {
      while (Hop8Disp[l] < 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit + 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop8 = max(Hop8)
    PeakHop8PC = PeakHop8/BW
    
    
    ContactHop8 <- 5 + which(Hop8[5:length(Hop8)] >5)[1]
    ContactTimeHop8 = (length(Hop8Disp) - ContactHop8)*0.001
    MinHop8Disp = min(Hop8Disp)
    EccHop8Disp = (Hop8Disp[ContactHop8] - MinHop8Disp)
    EccStiffHop8 = ((PeakHop8 - BW)/EccHop8Disp)/input$Mass
    
    JumpFreqHop8 = 1/(length(FullHop8)*0.001)
    
    #########################
    
    #Hop9
    
    #Calculate Acceleration
    
    Hop9Acc = (Hop9-BW)/input$Mass
    
    Hop9Accavg = rollapply(Hop9Acc, 2, mean)
    Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
    
    #Acceleration Area
    Hop9Accarea = Hop9Accavg2*0.001
    Hop9VelInit = 1.3
    
    #Velocity
    Hop9Vela = Hop9Accarea
    Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
    Hop9Vel = cumsum(Hop9Vela)
    
    #Velocity Area
    Hop9Velavg = rollapply(Hop9Vel, 2, mean)
    Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
    Hop9Velarea = Hop9Velavg2*0.001
    
    #Displacement
    Hop9Disp = cumsum(Hop9Velarea)
    
    l = length(Hop9Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop9Disp[l]>0){
      while (Hop9Disp[l] > 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit - 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)}
    }else (Hop9Disp[l]<0) 
    {
      while (Hop9Disp[l] < 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit + 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop9 = max(Hop9)
    PeakHop9PC = PeakHop9/BW
    
    
    ContactHop9 <- 5 + which(Hop9[5:length(Hop9)] >5)[1]
    ContactTimeHop9 = (length(Hop9Disp) - ContactHop9)*0.001
    MinHop9Disp = min(Hop9Disp)
    EccHop9Disp = (Hop9Disp[ContactHop9] - MinHop9Disp)
    EccStiffHop9 = ((PeakHop9 - BW)/EccHop9Disp)/input$Mass
    
    JumpFreqHop9 = 1/(length(FullHop9)*0.001)
    
    #########################
    
    #Hop10
    
    #Calculate Acceleration
    
    Hop10Acc = (Hop10-BW)/input$Mass
    
    Hop10Accavg = rollapply(Hop10Acc, 2, mean)
    Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
    
    #Acceleration Area
    Hop10Accarea = Hop10Accavg2*0.001
    Hop10VelInit = 1.3
    
    #Velocity
    Hop10Vela = Hop10Accarea
    Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
    Hop10Vel = cumsum(Hop10Vela)
    
    #Velocity Area
    Hop10Velavg = rollapply(Hop10Vel, 2, mean)
    Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
    Hop10Velarea = Hop10Velavg2*0.001
    
    #Displacement
    Hop10Disp = cumsum(Hop10Velarea)
    
    l = length(Hop10Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop10Disp[l]>0){
      while (Hop10Disp[l] > 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit - 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)}
    }else (Hop10Disp[l]<0) 
    {
      while (Hop10Disp[l] < 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit + 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop10 = max(Hop10)
    PeakHop10PC = PeakHop10/BW
    
    
    ContactHop10 <- 5 + which(Hop10[5:length(Hop10)] >5)[1]
    ContactTimeHop10 = (length(Hop10Disp) - ContactHop10)*0.001
    MinHop10Disp = min(Hop10Disp)
    EccHop10Disp = (Hop10Disp[ContactHop10] - MinHop10Disp)
    EccStiffHop10 = ((PeakHop10 - BW)/EccHop10Disp)/input$Mass
    
    JumpFreqHop10 = 1/(length(FullHop10)*0.001)
    
    #########################
    
    #Hop11
    
    #Calculate Acceleration
    
    Hop11Acc = (Hop11-BW)/input$Mass
    
    Hop11Accavg = rollapply(Hop11Acc, 2, mean)
    Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
    
    #Acceleration Area
    Hop11Accarea = Hop11Accavg2*0.001
    Hop11VelInit = 1.3
    
    #Velocity
    Hop11Vela = Hop11Accarea
    Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
    Hop11Vel = cumsum(Hop11Vela)
    
    #Velocity Area
    Hop11Velavg = rollapply(Hop11Vel, 2, mean)
    Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
    Hop11Velarea = Hop11Velavg2*0.001
    
    #Displacement
    Hop11Disp = cumsum(Hop11Velarea)
    
    l = length(Hop11Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop11Disp[l]>0){
      while (Hop11Disp[l] > 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit - 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)}
    }else (Hop11Disp[l]<0) 
    {
      while (Hop11Disp[l] < 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit + 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop11 = max(Hop11)
    PeakHop11PC = PeakHop11/BW
    
    
    ContactHop11 <- 5 + which(Hop11[5:length(Hop11)] >5)[1]
    ContactTimeHop11 = (length(Hop11Disp) - ContactHop11)*0.001
    MinHop11Disp = min(Hop11Disp)
    EccHop11Disp = (Hop11Disp[ContactHop11] - MinHop11Disp)
    EccStiffHop11 = ((PeakHop11 - BW)/EccHop11Disp)/input$Mass
    
    JumpFreqHop11 = 1/(length(FullHop11)*0.001)
    
    #########################
    
    #Hop12
    
    #Calculate Acceleration
    
    Hop12Acc = (Hop12-BW)/input$Mass
    
    Hop12Accavg = rollapply(Hop12Acc, 2, mean)
    Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
    
    #Acceleration Area
    Hop12Accarea = Hop12Accavg2*0.001
    Hop12VelInit = 1.3
    
    #Velocity
    Hop12Vela = Hop12Accarea
    Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
    Hop12Vel = cumsum(Hop12Vela)
    
    #Velocity Area
    Hop12Velavg = rollapply(Hop12Vel, 2, mean)
    Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
    Hop12Velarea = Hop12Velavg2*0.001
    
    #Displacement
    Hop12Disp = cumsum(Hop12Velarea)
    
    l = length(Hop12Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop12Disp[l]>0){
      while (Hop12Disp[l] > 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit - 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)}
    }else (Hop12Disp[l]<0) 
    {
      while (Hop12Disp[l] < 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit + 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop12 = max(Hop12)
    PeakHop12PC = PeakHop12/BW
    
    
    ContactHop12 <- 5 + which(Hop12[5:length(Hop12)] >5)[1]
    ContactTimeHop12 = (length(Hop12Disp) - ContactHop12)*0.001
    MinHop12Disp = min(Hop12Disp)
    EccHop12Disp = (Hop12Disp[ContactHop12] - MinHop12Disp)
    EccStiffHop12 = ((PeakHop12 - BW)/EccHop12Disp)/input$Mass
    
    JumpFreqHop12 = 1/(length(FullHop12)*0.001)
    
    #########################
    
    #Hop13
    
    #Calculate Acceleration
    
    Hop13Acc = (Hop13-BW)/input$Mass
    
    Hop13Accavg = rollapply(Hop13Acc, 2, mean)
    Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
    
    #Acceleration Area
    Hop13Accarea = Hop13Accavg2*0.001
    Hop13VelInit = 1.3
    
    #Velocity
    Hop13Vela = Hop13Accarea
    Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
    Hop13Vel = cumsum(Hop13Vela)
    
    #Velocity Area
    Hop13Velavg = rollapply(Hop13Vel, 2, mean)
    Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
    Hop13Velarea = Hop13Velavg2*0.001
    
    #Displacement
    Hop13Disp = cumsum(Hop13Velarea)
    
    l = length(Hop13Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop13Disp[l]>0){
      while (Hop13Disp[l] > 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit - 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)}
    }else (Hop13Disp[l]<0) 
    {
      while (Hop13Disp[l] < 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit + 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop13 = max(Hop13)
    PeakHop13PC = PeakHop13/BW
    
    
    ContactHop13 <- 5 + which(Hop13[5:length(Hop13)] >5)[1]
    ContactTimeHop13 = (length(Hop13Disp) - ContactHop13)*0.001
    MinHop13Disp = min(Hop13Disp)
    EccHop13Disp = (Hop13Disp[ContactHop13] - MinHop13Disp)
    EccStiffHop13 = ((PeakHop13 - BW)/EccHop13Disp)/input$Mass
    
    JumpFreqHop13 = 1/(length(FullHop13)*0.001)
    
    #########################
    
    #Hop14
    
    #Calculate Acceleration
    
    Hop14Acc = (Hop14-BW)/input$Mass
    
    Hop14Accavg = rollapply(Hop14Acc, 2, mean)
    Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
    
    #Acceleration Area
    Hop14Accarea = Hop14Accavg2*0.001
    Hop14VelInit = 1.3
    
    #Velocity
    Hop14Vela = Hop14Accarea
    Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
    Hop14Vel = cumsum(Hop14Vela)
    
    #Velocity Area
    Hop14Velavg = rollapply(Hop14Vel, 2, mean)
    Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
    Hop14Velarea = Hop14Velavg2*0.001
    
    #Displacement
    Hop14Disp = cumsum(Hop14Velarea)
    
    l = length(Hop14Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop14Disp[l]>0){
      while (Hop14Disp[l] > 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit - 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)}
    }else (Hop14Disp[l]<0) 
    {
      while (Hop14Disp[l] < 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit + 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop14 = max(Hop14)
    PeakHop14PC = PeakHop14/BW
    
    
    ContactHop14 <- 5 + which(Hop14[5:length(Hop14)] >5)[1]
    ContactTimeHop14 = (length(Hop14Disp) - ContactHop14)*0.001
    MinHop14Disp = min(Hop14Disp)
    EccHop14Disp = (Hop14Disp[ContactHop14] - MinHop14Disp)
    EccStiffHop14 = ((PeakHop14 - BW)/EccHop14Disp)/input$Mass
    
    JumpFreqHop14 = 1/(length(FullHop14)*0.001)
    
    #########################
    
    # #Hop15
    # 
    # #Calculate Acceleration
    # 
    # Hop15Acc = (Hop15-BW)/input$Mass
    # 
    # Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    # Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    # 
    # #Acceleration Area
    # Hop15Accarea = Hop15Accavg2*0.001
    # Hop15VelInit = 1.3
    # 
    # #Velocity
    # Hop15Vela = Hop15Accarea
    # Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    # Hop15Vel = cumsum(Hop15Vela)
    # 
    # #Velocity Area
    # Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    # Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    # Hop15Velarea = Hop15Velavg2*0.001
    # 
    # #Displacement
    # Hop15Disp = cumsum(Hop15Velarea)
    # 
    # l = length(Hop15Disp)
    # 
    # #Adjust Init Velocity to get Final Displacment close to zero
    # if (Hop15Disp[l]>0){
    #   while (Hop15Disp[l] > 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit - 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)}
    # }else (Hop15Disp[l]<0) 
    # {
    #   while (Hop15Disp[l] < 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit + 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)
    #   }
    # }
    # 
    # 
    # #Calculate output variables
    # PeakHop15 = max(Hop15)
    # PeakHop15PC = PeakHop15/BW
    # 
    # 
    # ContactHop15 <- 5 + which(Hop15[5:length(Hop15)] >5)[1]
    # ContactTimeHop15 = (length(Hop15Disp) - ContactHop15)*0.001
    # MinHop15Disp = min(Hop15Disp)
    # EccHop15Disp = (Hop15Disp[ContactHop15] - MinHop15Disp)
    # EccStiffHop15 = ((PeakHop15 - BW)/EccHop15Disp)/input$Mass
    # 
    # JumpFreqHop15 = 1/(length(FullHop15)*0.001)
    # 
    # 
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14", "Average")
    EccCOMDisp <- rbind(round(EccHop1Disp,digits=2), round(EccHop2Disp,digits=2), round(EccHop3Disp,digits=2), round(EccHop4Disp,digits=2), round(EccHop5Disp,digits=2), round(EccHop6Disp,digits=2), round(EccHop7Disp,digits=2), round(EccHop8Disp,digits=2), round(EccHop9Disp,digits=2), round(EccHop10Disp,digits=2), round(EccHop11Disp,digits=2), round(EccHop12Disp,digits=2), round(EccHop13Disp,digits=2), round(EccHop14Disp,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2))
    PeakForcePC <- rbind(round(PeakHop1PC,digits=2), round(PeakHop2PC,digits=2), round(PeakHop3PC,digits=2), round(PeakHop4PC,digits=2), round(PeakHop5PC,digits=2), round(PeakHop6PC,digits=2), round(PeakHop7PC,digits=2), round(PeakHop8PC,digits=2), round(PeakHop9PC,digits=2), round(PeakHop10PC,digits=2), round(PeakHop11PC,digits=2), round(PeakHop12PC,digits=2), round(PeakHop13PC,digits=2), round(PeakHop14PC,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    Right1.7Averages <<-cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
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
    
  })
  output$resultstableRight2.0 <- renderTable({
    
    data1 <- 
      read.csv(input$Right2.0$datapath, stringsAsFactors = F, skip = 17)
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <5)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    for (r in 1:15){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <5)
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
        geom_vline(xintercept=Contact[15], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHop2 <- Fz2$Fz2[Contact[2]:Contact[3]]
    FullHop3 <- Fz2$Fz2[Contact[3]:Contact[4]]
    FullHop4 <- Fz2$Fz2[Contact[4]:Contact[5]]
    FullHop5 <- Fz2$Fz2[Contact[5]:Contact[6]]
    FullHop6 <- Fz2$Fz2[Contact[6]:Contact[7]]
    FullHop7 <- Fz2$Fz2[Contact[7]:Contact[8]]
    FullHop8 <- Fz2$Fz2[Contact[8]:Contact[9]]
    FullHop9 <- Fz2$Fz2[Contact[9]:Contact[10]]
    FullHop10 <- Fz2$Fz2[Contact[10]:Contact[11]]
    FullHop11 <- Fz2$Fz2[Contact[11]:Contact[12]]
    FullHop12 <- Fz2$Fz2[Contact[12]:Contact[13]]
    FullHop13 <- Fz2$Fz2[Contact[13]:Contact[14]]
    FullHop14 <- Fz2$Fz2[Contact[14]:Contact[15]]
    #    FullHop15 <- Fz2$Fz2[Contact[15]:Contact[16]]
    
    Hop1 <- Fz2$Fz2[Flight[1]:Flight[2]]
    Hop2 <- Fz2$Fz2[Flight[2]:Flight[3]]
    Hop3 <- Fz2$Fz2[Flight[3]:Flight[4]]
    Hop4 <- Fz2$Fz2[Flight[4]:Flight[5]]
    Hop5 <- Fz2$Fz2[Flight[5]:Flight[6]]
    Hop6 <- Fz2$Fz2[Flight[6]:Flight[7]]
    Hop7 <- Fz2$Fz2[Flight[7]:Flight[8]]
    Hop8 <- Fz2$Fz2[Flight[8]:Flight[9]]
    Hop9 <- Fz2$Fz2[Flight[9]:Flight[10]]
    Hop10 <- Fz2$Fz2[Flight[10]:Flight[11]]
    Hop11 <- Fz2$Fz2[Flight[11]:Flight[12]]
    Hop12 <- Fz2$Fz2[Flight[12]:Flight[13]]
    Hop13 <- Fz2$Fz2[Flight[13]:Flight[14]]
    Hop14 <- Fz2$Fz2[Flight[14]:Flight[15]]
    Hop15 <- Fz2$Fz2[Flight[15]:Flight[16]]
    #    Hop16 <- Fz2$Fz2[Flight[16]:Flight[17]]
    
    # windows()
    # plot(FullHop1)
    # windows()
    # plot(FullHop2)
    # windows()
    # plot(FullHop3)
    # windows()
    # plot(FullHop4)
    # windows()
    # plot(FullHop5)
    # windows()
    # plot(FullHop6)
    # windows()
    # plot(FullHop7)
    # windows()
    # plot(FullHop8)
    # windows()
    # plot(FullHop9)
    # windows()
    # plot(FullHop10)
    # windows()
    # plot(FullHop11)
    # windows()
    # plot(FullHop12)
    # windows()
    # plot(FullHop13)
    # windows()
    # plot(FullHop14)
    # windows()
    # plot(FullHop15)
    
    #Hop1
    
    #Calculate Acceleration
    
    Hop1Acc = (Hop1-BW)/input$Mass
    
    Hop1Accavg = rollapply(Hop1Acc, 2, mean)
    Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
    
    #Acceleration Area
    Hop1Accarea = Hop1Accavg2*0.001
    Hop1VelInit = 1.3
    
    #Velocity
    Hop1Vela = Hop1Accarea
    Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
    Hop1Vel = cumsum(Hop1Vela)
    
    #Velocity Area
    Hop1Velavg = rollapply(Hop1Vel, 2, mean)
    Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
    Hop1Velarea = Hop1Velavg2*0.001
    
    #Displacement
    Hop1Disp = cumsum(Hop1Velarea)
    
    l = length(Hop1Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop1Disp[l]>0){
      while (Hop1Disp[l] > 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit - 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)}
    }else (Hop1Disp[l]<0) 
    {
      while (Hop1Disp[l] < 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit + 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop1 = max(Hop1)
    PeakHop1PC = PeakHop1/BW
    
    
    ContactHop1 <- 5 + which(Hop1[5:length(Hop1)] >5)[1]
    ContactTimeHop1 = (length(Hop1Disp) - ContactHop1)*0.001
    MinHop1Disp = min(Hop1Disp)
    EccHop1Disp = (Hop1Disp[ContactHop1] - MinHop1Disp)
    EccStiffHop1 = ((PeakHop1 - BW)/EccHop1Disp)/input$Mass
    
    JumpFreqHop1 = 1/(length(FullHop1)*0.001)
    
    #####################
    
    #Hop2
    
    #Calculate Acceleration
    
    Hop2Acc = (Hop2-BW)/input$Mass
    
    Hop2Accavg = rollapply(Hop2Acc, 2, mean)
    Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
    
    #Acceleration Area
    Hop2Accarea = Hop2Accavg2*0.001
    Hop2VelInit = 1.3
    
    #Velocity
    Hop2Vela = Hop2Accarea
    Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
    Hop2Vel = cumsum(Hop2Vela)
    
    #Velocity Area
    Hop2Velavg = rollapply(Hop2Vel, 2, mean)
    Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
    Hop2Velarea = Hop2Velavg2*0.001
    
    #Displacement
    Hop2Disp = cumsum(Hop2Velarea)
    
    l = length(Hop2Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop2Disp[l]>0){
      while (Hop2Disp[l] > 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit - 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)}
    }else (Hop2Disp[l]<0) 
    {
      while (Hop2Disp[l] < 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit + 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop2 = max(Hop2)
    PeakHop2PC = PeakHop2/BW
    
    
    ContactHop2 <- 5 + which(Hop2[5:length(Hop2)] >5)[1]
    ContactTimeHop2 = (length(Hop2Disp) - ContactHop2)*0.001
    MinHop2Disp = min(Hop2Disp)
    EccHop2Disp = (Hop2Disp[ContactHop2] - MinHop2Disp)
    EccStiffHop2 = ((PeakHop2 - BW)/EccHop2Disp)/input$Mass
    
    JumpFreqHop2 = 1/(length(FullHop2)*0.001)
    
    #########################
    
    #Hop3
    
    #Calculate Acceleration
    
    Hop3Acc = (Hop3-BW)/input$Mass
    
    Hop3Accavg = rollapply(Hop3Acc, 2, mean)
    Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
    
    #Acceleration Area
    Hop3Accarea = Hop3Accavg2*0.001
    Hop3VelInit = 1.3
    
    #Velocity
    Hop3Vela = Hop3Accarea
    Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
    Hop3Vel = cumsum(Hop3Vela)
    
    #Velocity Area
    Hop3Velavg = rollapply(Hop3Vel, 2, mean)
    Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
    Hop3Velarea = Hop3Velavg2*0.001
    
    #Displacement
    Hop3Disp = cumsum(Hop3Velarea)
    
    l = length(Hop3Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop3Disp[l]>0){
      while (Hop3Disp[l] > 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit - 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)}
    }else (Hop3Disp[l]<0) 
    {
      while (Hop3Disp[l] < 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit + 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop3 = max(Hop3)
    PeakHop3PC = PeakHop3/BW
    
    
    ContactHop3 <- 5 + which(Hop3[5:length(Hop3)] >5)[1]
    ContactTimeHop3 = (length(Hop3Disp) - ContactHop3)*0.001
    MinHop3Disp = min(Hop3Disp)
    EccHop3Disp = (Hop3Disp[ContactHop3] - MinHop3Disp)
    EccStiffHop3 = ((PeakHop3 - BW)/EccHop3Disp)/input$Mass
    
    JumpFreqHop3 = 1/(length(FullHop3)*0.001)
    
    #########################
    
    #Hop4
    
    #Calculate Acceleration
    
    Hop4Acc = (Hop4-BW)/input$Mass
    
    Hop4Accavg = rollapply(Hop4Acc, 2, mean)
    Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
    
    #Acceleration Area
    Hop4Accarea = Hop4Accavg2*0.001
    Hop4VelInit = 1.3
    
    #Velocity
    Hop4Vela = Hop4Accarea
    Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
    Hop4Vel = cumsum(Hop4Vela)
    
    #Velocity Area
    Hop4Velavg = rollapply(Hop4Vel, 2, mean)
    Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
    Hop4Velarea = Hop4Velavg2*0.001
    
    #Displacement
    Hop4Disp = cumsum(Hop4Velarea)
    
    l = length(Hop4Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop4Disp[l]>0){
      while (Hop4Disp[l] > 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit - 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)}
    }else (Hop4Disp[l]<0) 
    {
      while (Hop4Disp[l] < 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit + 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop4 = max(Hop4)
    PeakHop4PC = PeakHop4/BW
    
    
    ContactHop4 <- 5 + which(Hop4[5:length(Hop4)] >5)[1]
    ContactTimeHop4 = (length(Hop4Disp) - ContactHop4)*0.001
    MinHop4Disp = min(Hop4Disp)
    EccHop4Disp = (Hop4Disp[ContactHop4] - MinHop4Disp)
    EccStiffHop4 = ((PeakHop4 - BW)/EccHop4Disp)/input$Mass
    
    JumpFreqHop4 = 1/(length(FullHop4)*0.001)
    
    #########################
    
    #Hop5
    
    #Calculate Acceleration
    
    Hop5Acc = (Hop5-BW)/input$Mass
    
    Hop5Accavg = rollapply(Hop5Acc, 2, mean)
    Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
    
    #Acceleration Area
    Hop5Accarea = Hop5Accavg2*0.001
    Hop5VelInit = 1.3
    
    #Velocity
    Hop5Vela = Hop5Accarea
    Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
    Hop5Vel = cumsum(Hop5Vela)
    
    #Velocity Area
    Hop5Velavg = rollapply(Hop5Vel, 2, mean)
    Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
    Hop5Velarea = Hop5Velavg2*0.001
    
    #Displacement
    Hop5Disp = cumsum(Hop5Velarea)
    
    l = length(Hop5Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop5Disp[l]>0){
      while (Hop5Disp[l] > 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit - 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)}
    }else (Hop5Disp[l]<0) 
    {
      while (Hop5Disp[l] < 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit + 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop5 = max(Hop5)
    PeakHop5PC = PeakHop5/BW
    
    
    ContactHop5 <- 5 + which(Hop5[5:length(Hop5)] >5)[1]
    ContactTimeHop5 = (length(Hop5Disp) - ContactHop5)*0.001
    MinHop5Disp = min(Hop5Disp)
    EccHop5Disp = (Hop5Disp[ContactHop5] - MinHop5Disp)
    EccStiffHop5 = ((PeakHop5 - BW)/EccHop5Disp)/input$Mass
    
    JumpFreqHop5 = 1/(length(FullHop5)*0.001)
    
    #########################
    
    
    #Hop6
    
    #Calculate Acceleration
    
    Hop6Acc = (Hop6-BW)/input$Mass
    
    Hop6Accavg = rollapply(Hop6Acc, 2, mean)
    Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
    
    #Acceleration Area
    Hop6Accarea = Hop6Accavg2*0.001
    Hop6VelInit = 1.3
    
    #Velocity
    Hop6Vela = Hop6Accarea
    Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
    Hop6Vel = cumsum(Hop6Vela)
    
    #Velocity Area
    Hop6Velavg = rollapply(Hop6Vel, 2, mean)
    Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
    Hop6Velarea = Hop6Velavg2*0.001
    
    #Displacement
    Hop6Disp = cumsum(Hop6Velarea)
    
    l = length(Hop6Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop6Disp[l]>0){
      while (Hop6Disp[l] > 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit - 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)}
    }else (Hop6Disp[l]<0) 
    {
      while (Hop6Disp[l] < 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit + 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop6 = max(Hop6)
    PeakHop6PC = PeakHop6/BW
    
    
    ContactHop6 <- 5 + which(Hop6[5:length(Hop6)] >5)[1]
    ContactTimeHop6 = (length(Hop6Disp) - ContactHop6)*0.001
    MinHop6Disp = min(Hop6Disp)
    EccHop6Disp = (Hop6Disp[ContactHop6] - MinHop6Disp)
    EccStiffHop6 = ((PeakHop6 - BW)/EccHop6Disp)/input$Mass
    
    JumpFreqHop6 = 1/(length(FullHop6)*0.001)
    
    #########################
    
    
    #Hop7
    
    #Calculate Acceleration
    
    Hop7Acc = (Hop7-BW)/input$Mass
    
    Hop7Accavg = rollapply(Hop7Acc, 2, mean)
    Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
    
    #Acceleration Area
    Hop7Accarea = Hop7Accavg2*0.001
    Hop7VelInit = 1.3
    
    #Velocity
    Hop7Vela = Hop7Accarea
    Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
    Hop7Vel = cumsum(Hop7Vela)
    
    #Velocity Area
    Hop7Velavg = rollapply(Hop7Vel, 2, mean)
    Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
    Hop7Velarea = Hop7Velavg2*0.001
    
    #Displacement
    Hop7Disp = cumsum(Hop7Velarea)
    
    l = length(Hop7Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop7Disp[l]>0){
      while (Hop7Disp[l] > 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit - 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)}
    }else (Hop7Disp[l]<0) 
    {
      while (Hop7Disp[l] < 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit + 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop7 = max(Hop7)
    PeakHop7PC = PeakHop7/BW
    
    
    ContactHop7 <- 5 + which(Hop7[5:length(Hop7)] >5)[1]
    ContactTimeHop7 = (length(Hop7Disp) - ContactHop7)*0.001
    MinHop7Disp = min(Hop7Disp)
    EccHop7Disp = (Hop7Disp[ContactHop7] - MinHop7Disp)
    EccStiffHop7 = ((PeakHop7 - BW)/EccHop7Disp)/input$Mass
    
    JumpFreqHop7 = 1/(length(FullHop7)*0.001)
    
    #########################
    
    #Hop8
    
    #Calculate Acceleration
    
    Hop8Acc = (Hop8-BW)/input$Mass
    
    Hop8Accavg = rollapply(Hop8Acc, 2, mean)
    Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
    
    #Acceleration Area
    Hop8Accarea = Hop8Accavg2*0.001
    Hop8VelInit = 1.3
    
    #Velocity
    Hop8Vela = Hop8Accarea
    Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
    Hop8Vel = cumsum(Hop8Vela)
    
    #Velocity Area
    Hop8Velavg = rollapply(Hop8Vel, 2, mean)
    Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
    Hop8Velarea = Hop8Velavg2*0.001
    
    #Displacement
    Hop8Disp = cumsum(Hop8Velarea)
    
    l = length(Hop8Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop8Disp[l]>0){
      while (Hop8Disp[l] > 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit - 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)}
    }else (Hop8Disp[l]<0) 
    {
      while (Hop8Disp[l] < 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit + 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop8 = max(Hop8)
    PeakHop8PC = PeakHop8/BW
    
    
    ContactHop8 <- 5 + which(Hop8[5:length(Hop8)] >5)[1]
    ContactTimeHop8 = (length(Hop8Disp) - ContactHop8)*0.001
    MinHop8Disp = min(Hop8Disp)
    EccHop8Disp = (Hop8Disp[ContactHop8] - MinHop8Disp)
    EccStiffHop8 = ((PeakHop8 - BW)/EccHop8Disp)/input$Mass
    
    JumpFreqHop8 = 1/(length(FullHop8)*0.001)
    
    #########################
    
    #Hop9
    
    #Calculate Acceleration
    
    Hop9Acc = (Hop9-BW)/input$Mass
    
    Hop9Accavg = rollapply(Hop9Acc, 2, mean)
    Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
    
    #Acceleration Area
    Hop9Accarea = Hop9Accavg2*0.001
    Hop9VelInit = 1.3
    
    #Velocity
    Hop9Vela = Hop9Accarea
    Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
    Hop9Vel = cumsum(Hop9Vela)
    
    #Velocity Area
    Hop9Velavg = rollapply(Hop9Vel, 2, mean)
    Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
    Hop9Velarea = Hop9Velavg2*0.001
    
    #Displacement
    Hop9Disp = cumsum(Hop9Velarea)
    
    l = length(Hop9Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop9Disp[l]>0){
      while (Hop9Disp[l] > 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit - 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)}
    }else (Hop9Disp[l]<0) 
    {
      while (Hop9Disp[l] < 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit + 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop9 = max(Hop9)
    PeakHop9PC = PeakHop9/BW
    
    
    ContactHop9 <- 5 + which(Hop9[5:length(Hop9)] >5)[1]
    ContactTimeHop9 = (length(Hop9Disp) - ContactHop9)*0.001
    MinHop9Disp = min(Hop9Disp)
    EccHop9Disp = (Hop9Disp[ContactHop9] - MinHop9Disp)
    EccStiffHop9 = ((PeakHop9 - BW)/EccHop9Disp)/input$Mass
    
    JumpFreqHop9 = 1/(length(FullHop9)*0.001)
    
    #########################
    
    #Hop10
    
    #Calculate Acceleration
    
    Hop10Acc = (Hop10-BW)/input$Mass
    
    Hop10Accavg = rollapply(Hop10Acc, 2, mean)
    Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
    
    #Acceleration Area
    Hop10Accarea = Hop10Accavg2*0.001
    Hop10VelInit = 1.3
    
    #Velocity
    Hop10Vela = Hop10Accarea
    Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
    Hop10Vel = cumsum(Hop10Vela)
    
    #Velocity Area
    Hop10Velavg = rollapply(Hop10Vel, 2, mean)
    Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
    Hop10Velarea = Hop10Velavg2*0.001
    
    #Displacement
    Hop10Disp = cumsum(Hop10Velarea)
    
    l = length(Hop10Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop10Disp[l]>0){
      while (Hop10Disp[l] > 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit - 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)}
    }else (Hop10Disp[l]<0) 
    {
      while (Hop10Disp[l] < 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit + 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop10 = max(Hop10)
    PeakHop10PC = PeakHop10/BW
    
    
    ContactHop10 <- 5 + which(Hop10[5:length(Hop10)] >5)[1]
    ContactTimeHop10 = (length(Hop10Disp) - ContactHop10)*0.001
    MinHop10Disp = min(Hop10Disp)
    EccHop10Disp = (Hop10Disp[ContactHop10] - MinHop10Disp)
    EccStiffHop10 = ((PeakHop10 - BW)/EccHop10Disp)/input$Mass
    
    JumpFreqHop10 = 1/(length(FullHop10)*0.001)
    
    #########################
    
    #Hop11
    
    #Calculate Acceleration
    
    Hop11Acc = (Hop11-BW)/input$Mass
    
    Hop11Accavg = rollapply(Hop11Acc, 2, mean)
    Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
    
    #Acceleration Area
    Hop11Accarea = Hop11Accavg2*0.001
    Hop11VelInit = 1.3
    
    #Velocity
    Hop11Vela = Hop11Accarea
    Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
    Hop11Vel = cumsum(Hop11Vela)
    
    #Velocity Area
    Hop11Velavg = rollapply(Hop11Vel, 2, mean)
    Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
    Hop11Velarea = Hop11Velavg2*0.001
    
    #Displacement
    Hop11Disp = cumsum(Hop11Velarea)
    
    l = length(Hop11Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop11Disp[l]>0){
      while (Hop11Disp[l] > 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit - 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)}
    }else (Hop11Disp[l]<0) 
    {
      while (Hop11Disp[l] < 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit + 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop11 = max(Hop11)
    PeakHop11PC = PeakHop11/BW
    
    
    ContactHop11 <- 5 + which(Hop11[5:length(Hop11)] >5)[1]
    ContactTimeHop11 = (length(Hop11Disp) - ContactHop11)*0.001
    MinHop11Disp = min(Hop11Disp)
    EccHop11Disp = (Hop11Disp[ContactHop11] - MinHop11Disp)
    EccStiffHop11 = ((PeakHop11 - BW)/EccHop11Disp)/input$Mass
    
    JumpFreqHop11 = 1/(length(FullHop11)*0.001)
    
    #########################
    
    #Hop12
    
    #Calculate Acceleration
    
    Hop12Acc = (Hop12-BW)/input$Mass
    
    Hop12Accavg = rollapply(Hop12Acc, 2, mean)
    Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
    
    #Acceleration Area
    Hop12Accarea = Hop12Accavg2*0.001
    Hop12VelInit = 1.3
    
    #Velocity
    Hop12Vela = Hop12Accarea
    Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
    Hop12Vel = cumsum(Hop12Vela)
    
    #Velocity Area
    Hop12Velavg = rollapply(Hop12Vel, 2, mean)
    Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
    Hop12Velarea = Hop12Velavg2*0.001
    
    #Displacement
    Hop12Disp = cumsum(Hop12Velarea)
    
    l = length(Hop12Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop12Disp[l]>0){
      while (Hop12Disp[l] > 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit - 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)}
    }else (Hop12Disp[l]<0) 
    {
      while (Hop12Disp[l] < 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit + 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop12 = max(Hop12)
    PeakHop12PC = PeakHop12/BW
    
    
    ContactHop12 <- 5 + which(Hop12[5:length(Hop12)] >5)[1]
    ContactTimeHop12 = (length(Hop12Disp) - ContactHop12)*0.001
    MinHop12Disp = min(Hop12Disp)
    EccHop12Disp = (Hop12Disp[ContactHop12] - MinHop12Disp)
    EccStiffHop12 = ((PeakHop12 - BW)/EccHop12Disp)/input$Mass
    
    JumpFreqHop12 = 1/(length(FullHop12)*0.001)
    
    #########################
    
    #Hop13
    
    #Calculate Acceleration
    
    Hop13Acc = (Hop13-BW)/input$Mass
    
    Hop13Accavg = rollapply(Hop13Acc, 2, mean)
    Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
    
    #Acceleration Area
    Hop13Accarea = Hop13Accavg2*0.001
    Hop13VelInit = 1.3
    
    #Velocity
    Hop13Vela = Hop13Accarea
    Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
    Hop13Vel = cumsum(Hop13Vela)
    
    #Velocity Area
    Hop13Velavg = rollapply(Hop13Vel, 2, mean)
    Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
    Hop13Velarea = Hop13Velavg2*0.001
    
    #Displacement
    Hop13Disp = cumsum(Hop13Velarea)
    
    l = length(Hop13Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop13Disp[l]>0){
      while (Hop13Disp[l] > 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit - 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)}
    }else (Hop13Disp[l]<0) 
    {
      while (Hop13Disp[l] < 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit + 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop13 = max(Hop13)
    PeakHop13PC = PeakHop13/BW
    
    
    ContactHop13 <- 5 + which(Hop13[5:length(Hop13)] >5)[1]
    ContactTimeHop13 = (length(Hop13Disp) - ContactHop13)*0.001
    MinHop13Disp = min(Hop13Disp)
    EccHop13Disp = (Hop13Disp[ContactHop13] - MinHop13Disp)
    EccStiffHop13 = ((PeakHop13 - BW)/EccHop13Disp)/input$Mass
    
    JumpFreqHop13 = 1/(length(FullHop13)*0.001)
    
    #########################
    
    #Hop14
    
    #Calculate Acceleration
    
    Hop14Acc = (Hop14-BW)/input$Mass
    
    Hop14Accavg = rollapply(Hop14Acc, 2, mean)
    Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
    
    #Acceleration Area
    Hop14Accarea = Hop14Accavg2*0.001
    Hop14VelInit = 1.3
    
    #Velocity
    Hop14Vela = Hop14Accarea
    Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
    Hop14Vel = cumsum(Hop14Vela)
    
    #Velocity Area
    Hop14Velavg = rollapply(Hop14Vel, 2, mean)
    Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
    Hop14Velarea = Hop14Velavg2*0.001
    
    #Displacement
    Hop14Disp = cumsum(Hop14Velarea)
    
    l = length(Hop14Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop14Disp[l]>0){
      while (Hop14Disp[l] > 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit - 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)}
    }else (Hop14Disp[l]<0) 
    {
      while (Hop14Disp[l] < 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit + 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop14 = max(Hop14)
    PeakHop14PC = PeakHop14/BW
    
    
    ContactHop14 <- 5 + which(Hop14[5:length(Hop14)] >5)[1]
    ContactTimeHop14 = (length(Hop14Disp) - ContactHop14)*0.001
    MinHop14Disp = min(Hop14Disp)
    EccHop14Disp = (Hop14Disp[ContactHop14] - MinHop14Disp)
    EccStiffHop14 = ((PeakHop14 - BW)/EccHop14Disp)/input$Mass
    
    JumpFreqHop14 = 1/(length(FullHop14)*0.001)
    
    #########################
    
    # #Hop15
    # 
    # #Calculate Acceleration
    # 
    # Hop15Acc = (Hop15-BW)/input$Mass
    # 
    # Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    # Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    # 
    # #Acceleration Area
    # Hop15Accarea = Hop15Accavg2*0.001
    # Hop15VelInit = 1.3
    # 
    # #Velocity
    # Hop15Vela = Hop15Accarea
    # Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    # Hop15Vel = cumsum(Hop15Vela)
    # 
    # #Velocity Area
    # Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    # Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    # Hop15Velarea = Hop15Velavg2*0.001
    # 
    # #Displacement
    # Hop15Disp = cumsum(Hop15Velarea)
    # 
    # l = length(Hop15Disp)
    # 
    # #Adjust Init Velocity to get Final Displacment close to zero
    # if (Hop15Disp[l]>0){
    #   while (Hop15Disp[l] > 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit - 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)}
    # }else (Hop15Disp[l]<0) 
    # {
    #   while (Hop15Disp[l] < 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit + 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)
    #   }
    # }
    # 
    # 
    # #Calculate output variables
    # PeakHop15 = max(Hop15)
    # PeakHop15PC = PeakHop15/BW
    # 
    # 
    # ContactHop15 <- 5 + which(Hop15[5:length(Hop15)] >5)[1]
    # ContactTimeHop15 = (length(Hop15Disp) - ContactHop15)*0.001
    # MinHop15Disp = min(Hop15Disp)
    # EccHop15Disp = (Hop15Disp[ContactHop15] - MinHop15Disp)
    # EccStiffHop15 = ((PeakHop15 - BW)/EccHop15Disp)/input$Mass
    # 
    # JumpFreqHop15 = 1/(length(FullHop15)*0.001)
    # 
    # 
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14", "Average")
    EccCOMDisp <- rbind(round(EccHop1Disp,digits=2), round(EccHop2Disp,digits=2), round(EccHop3Disp,digits=2), round(EccHop4Disp,digits=2), round(EccHop5Disp,digits=2), round(EccHop6Disp,digits=2), round(EccHop7Disp,digits=2), round(EccHop8Disp,digits=2), round(EccHop9Disp,digits=2), round(EccHop10Disp,digits=2), round(EccHop11Disp,digits=2), round(EccHop12Disp,digits=2), round(EccHop13Disp,digits=2), round(EccHop14Disp,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2))
    PeakForcePC <- rbind(round(PeakHop1PC,digits=2), round(PeakHop2PC,digits=2), round(PeakHop3PC,digits=2), round(PeakHop4PC,digits=2), round(PeakHop5PC,digits=2), round(PeakHop6PC,digits=2), round(PeakHop7PC,digits=2), round(PeakHop8PC,digits=2), round(PeakHop9PC,digits=2), round(PeakHop10PC,digits=2), round(PeakHop11PC,digits=2), round(PeakHop12PC,digits=2), round(PeakHop13PC,digits=2), round(PeakHop14PC,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    Right2.0Averages <<-cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
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
    
  })
  output$resultstableRight2.3 <- renderTable({
    
    data1 <- 
      read.csv(input$Right2.3$datapath, stringsAsFactors = F, skip = 17)
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <5)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    for (r in 1:15){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <5)
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
        geom_vline(xintercept=Contact[15], col="red") 
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHop2 <- Fz2$Fz2[Contact[2]:Contact[3]]
    FullHop3 <- Fz2$Fz2[Contact[3]:Contact[4]]
    FullHop4 <- Fz2$Fz2[Contact[4]:Contact[5]]
    FullHop5 <- Fz2$Fz2[Contact[5]:Contact[6]]
    FullHop6 <- Fz2$Fz2[Contact[6]:Contact[7]]
    FullHop7 <- Fz2$Fz2[Contact[7]:Contact[8]]
    FullHop8 <- Fz2$Fz2[Contact[8]:Contact[9]]
    FullHop9 <- Fz2$Fz2[Contact[9]:Contact[10]]
    FullHop10 <- Fz2$Fz2[Contact[10]:Contact[11]]
    FullHop11 <- Fz2$Fz2[Contact[11]:Contact[12]]
    FullHop12 <- Fz2$Fz2[Contact[12]:Contact[13]]
    FullHop13 <- Fz2$Fz2[Contact[13]:Contact[14]]
    FullHop14 <- Fz2$Fz2[Contact[14]:Contact[15]]
    #    FullHop15 <- Fz2$Fz2[Contact[15]:Contact[16]]
    
    Hop1 <- Fz2$Fz2[Flight[1]:Flight[2]]
    Hop2 <- Fz2$Fz2[Flight[2]:Flight[3]]
    Hop3 <- Fz2$Fz2[Flight[3]:Flight[4]]
    Hop4 <- Fz2$Fz2[Flight[4]:Flight[5]]
    Hop5 <- Fz2$Fz2[Flight[5]:Flight[6]]
    Hop6 <- Fz2$Fz2[Flight[6]:Flight[7]]
    Hop7 <- Fz2$Fz2[Flight[7]:Flight[8]]
    Hop8 <- Fz2$Fz2[Flight[8]:Flight[9]]
    Hop9 <- Fz2$Fz2[Flight[9]:Flight[10]]
    Hop10 <- Fz2$Fz2[Flight[10]:Flight[11]]
    Hop11 <- Fz2$Fz2[Flight[11]:Flight[12]]
    Hop12 <- Fz2$Fz2[Flight[12]:Flight[13]]
    Hop13 <- Fz2$Fz2[Flight[13]:Flight[14]]
    Hop14 <- Fz2$Fz2[Flight[14]:Flight[15]]
    Hop15 <- Fz2$Fz2[Flight[15]:Flight[16]]
    #    Hop16 <- Fz2$Fz2[Flight[16]:Flight[17]]
    
    # windows()
    # plot(FullHop1)
    # windows()
    # plot(FullHop2)
    # windows()
    # plot(FullHop3)
    # windows()
    # plot(FullHop4)
    # windows()
    # plot(FullHop5)
    # windows()
    # plot(FullHop6)
    # windows()
    # plot(FullHop7)
    # windows()
    # plot(FullHop8)
    # windows()
    # plot(FullHop9)
    # windows()
    # plot(FullHop10)
    # windows()
    # plot(FullHop11)
    # windows()
    # plot(FullHop12)
    # windows()
    # plot(FullHop13)
    # windows()
    # plot(FullHop14)
    # windows()
    # plot(FullHop15)
    
    #Hop1
    
    #Calculate Acceleration
    
    Hop1Acc = (Hop1-BW)/input$Mass
    
    Hop1Accavg = rollapply(Hop1Acc, 2, mean)
    Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
    
    #Acceleration Area
    Hop1Accarea = Hop1Accavg2*0.001
    Hop1VelInit = 1.3
    
    #Velocity
    Hop1Vela = Hop1Accarea
    Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
    Hop1Vel = cumsum(Hop1Vela)
    
    #Velocity Area
    Hop1Velavg = rollapply(Hop1Vel, 2, mean)
    Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
    Hop1Velarea = Hop1Velavg2*0.001
    
    #Displacement
    Hop1Disp = cumsum(Hop1Velarea)
    
    l = length(Hop1Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop1Disp[l]>0){
      while (Hop1Disp[l] > 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit - 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)}
    }else (Hop1Disp[l]<0) 
    {
      while (Hop1Disp[l] < 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit + 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop1 = max(Hop1)
    PeakHop1PC = PeakHop1/BW
    
    
    ContactHop1 <- 5 + which(Hop1[5:length(Hop1)] >5)[1]
    ContactTimeHop1 = (length(Hop1Disp) - ContactHop1)*0.001
    MinHop1Disp = min(Hop1Disp)
    EccHop1Disp = (Hop1Disp[ContactHop1] - MinHop1Disp)
    EccStiffHop1 = ((PeakHop1 - BW)/EccHop1Disp)/input$Mass
    
    JumpFreqHop1 = 1/(length(FullHop1)*0.001)
    
    #####################
    
    #Hop2
    
    #Calculate Acceleration
    
    Hop2Acc = (Hop2-BW)/input$Mass
    
    Hop2Accavg = rollapply(Hop2Acc, 2, mean)
    Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
    
    #Acceleration Area
    Hop2Accarea = Hop2Accavg2*0.001
    Hop2VelInit = 1.3
    
    #Velocity
    Hop2Vela = Hop2Accarea
    Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
    Hop2Vel = cumsum(Hop2Vela)
    
    #Velocity Area
    Hop2Velavg = rollapply(Hop2Vel, 2, mean)
    Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
    Hop2Velarea = Hop2Velavg2*0.001
    
    #Displacement
    Hop2Disp = cumsum(Hop2Velarea)
    
    l = length(Hop2Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop2Disp[l]>0){
      while (Hop2Disp[l] > 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit - 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)}
    }else (Hop2Disp[l]<0) 
    {
      while (Hop2Disp[l] < 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit + 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop2 = max(Hop2)
    PeakHop2PC = PeakHop2/BW
    
    
    ContactHop2 <- 5 + which(Hop2[5:length(Hop2)] >5)[1]
    ContactTimeHop2 = (length(Hop2Disp) - ContactHop2)*0.001
    MinHop2Disp = min(Hop2Disp)
    EccHop2Disp = (Hop2Disp[ContactHop2] - MinHop2Disp)
    EccStiffHop2 = ((PeakHop2 - BW)/EccHop2Disp)/input$Mass
    
    JumpFreqHop2 = 1/(length(FullHop2)*0.001)
    
    #########################
    
    #Hop3
    
    #Calculate Acceleration
    
    Hop3Acc = (Hop3-BW)/input$Mass
    
    Hop3Accavg = rollapply(Hop3Acc, 2, mean)
    Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
    
    #Acceleration Area
    Hop3Accarea = Hop3Accavg2*0.001
    Hop3VelInit = 1.3
    
    #Velocity
    Hop3Vela = Hop3Accarea
    Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
    Hop3Vel = cumsum(Hop3Vela)
    
    #Velocity Area
    Hop3Velavg = rollapply(Hop3Vel, 2, mean)
    Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
    Hop3Velarea = Hop3Velavg2*0.001
    
    #Displacement
    Hop3Disp = cumsum(Hop3Velarea)
    
    l = length(Hop3Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop3Disp[l]>0){
      while (Hop3Disp[l] > 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit - 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)}
    }else (Hop3Disp[l]<0) 
    {
      while (Hop3Disp[l] < 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit + 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop3 = max(Hop3)
    PeakHop3PC = PeakHop3/BW
    
    
    ContactHop3 <- 5 + which(Hop3[5:length(Hop3)] >5)[1]
    ContactTimeHop3 = (length(Hop3Disp) - ContactHop3)*0.001
    MinHop3Disp = min(Hop3Disp)
    EccHop3Disp = (Hop3Disp[ContactHop3] - MinHop3Disp)
    EccStiffHop3 = ((PeakHop3 - BW)/EccHop3Disp)/input$Mass
    
    JumpFreqHop3 = 1/(length(FullHop3)*0.001)
    
    #########################
    
    #Hop4
    
    #Calculate Acceleration
    
    Hop4Acc = (Hop4-BW)/input$Mass
    
    Hop4Accavg = rollapply(Hop4Acc, 2, mean)
    Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
    
    #Acceleration Area
    Hop4Accarea = Hop4Accavg2*0.001
    Hop4VelInit = 1.3
    
    #Velocity
    Hop4Vela = Hop4Accarea
    Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
    Hop4Vel = cumsum(Hop4Vela)
    
    #Velocity Area
    Hop4Velavg = rollapply(Hop4Vel, 2, mean)
    Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
    Hop4Velarea = Hop4Velavg2*0.001
    
    #Displacement
    Hop4Disp = cumsum(Hop4Velarea)
    
    l = length(Hop4Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop4Disp[l]>0){
      while (Hop4Disp[l] > 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit - 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)}
    }else (Hop4Disp[l]<0) 
    {
      while (Hop4Disp[l] < 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit + 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop4 = max(Hop4)
    PeakHop4PC = PeakHop4/BW
    
    
    ContactHop4 <- 5 + which(Hop4[5:length(Hop4)] >5)[1]
    ContactTimeHop4 = (length(Hop4Disp) - ContactHop4)*0.001
    MinHop4Disp = min(Hop4Disp)
    EccHop4Disp = (Hop4Disp[ContactHop4] - MinHop4Disp)
    EccStiffHop4 = ((PeakHop4 - BW)/EccHop4Disp)/input$Mass
    
    JumpFreqHop4 = 1/(length(FullHop4)*0.001)
    
    #########################
    
    #Hop5
    
    #Calculate Acceleration
    
    Hop5Acc = (Hop5-BW)/input$Mass
    
    Hop5Accavg = rollapply(Hop5Acc, 2, mean)
    Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
    
    #Acceleration Area
    Hop5Accarea = Hop5Accavg2*0.001
    Hop5VelInit = 1.3
    
    #Velocity
    Hop5Vela = Hop5Accarea
    Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
    Hop5Vel = cumsum(Hop5Vela)
    
    #Velocity Area
    Hop5Velavg = rollapply(Hop5Vel, 2, mean)
    Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
    Hop5Velarea = Hop5Velavg2*0.001
    
    #Displacement
    Hop5Disp = cumsum(Hop5Velarea)
    
    l = length(Hop5Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop5Disp[l]>0){
      while (Hop5Disp[l] > 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit - 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)}
    }else (Hop5Disp[l]<0) 
    {
      while (Hop5Disp[l] < 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit + 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop5 = max(Hop5)
    PeakHop5PC = PeakHop5/BW
    
    
    ContactHop5 <- 5 + which(Hop5[5:length(Hop5)] >5)[1]
    ContactTimeHop5 = (length(Hop5Disp) - ContactHop5)*0.001
    MinHop5Disp = min(Hop5Disp)
    EccHop5Disp = (Hop5Disp[ContactHop5] - MinHop5Disp)
    EccStiffHop5 = ((PeakHop5 - BW)/EccHop5Disp)/input$Mass
    
    JumpFreqHop5 = 1/(length(FullHop5)*0.001)
    
    #########################
    
    
    #Hop6
    
    #Calculate Acceleration
    
    Hop6Acc = (Hop6-BW)/input$Mass
    
    Hop6Accavg = rollapply(Hop6Acc, 2, mean)
    Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
    
    #Acceleration Area
    Hop6Accarea = Hop6Accavg2*0.001
    Hop6VelInit = 1.3
    
    #Velocity
    Hop6Vela = Hop6Accarea
    Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
    Hop6Vel = cumsum(Hop6Vela)
    
    #Velocity Area
    Hop6Velavg = rollapply(Hop6Vel, 2, mean)
    Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
    Hop6Velarea = Hop6Velavg2*0.001
    
    #Displacement
    Hop6Disp = cumsum(Hop6Velarea)
    
    l = length(Hop6Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop6Disp[l]>0){
      while (Hop6Disp[l] > 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit - 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)}
    }else (Hop6Disp[l]<0) 
    {
      while (Hop6Disp[l] < 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit + 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop6 = max(Hop6)
    PeakHop6PC = PeakHop6/BW
    
    
    ContactHop6 <- 5 + which(Hop6[5:length(Hop6)] >5)[1]
    ContactTimeHop6 = (length(Hop6Disp) - ContactHop6)*0.001
    MinHop6Disp = min(Hop6Disp)
    EccHop6Disp = (Hop6Disp[ContactHop6] - MinHop6Disp)
    EccStiffHop6 = ((PeakHop6 - BW)/EccHop6Disp)/input$Mass
    
    JumpFreqHop6 = 1/(length(FullHop6)*0.001)
    
    #########################
    
    
    #Hop7
    
    #Calculate Acceleration
    
    Hop7Acc = (Hop7-BW)/input$Mass
    
    Hop7Accavg = rollapply(Hop7Acc, 2, mean)
    Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
    
    #Acceleration Area
    Hop7Accarea = Hop7Accavg2*0.001
    Hop7VelInit = 1.3
    
    #Velocity
    Hop7Vela = Hop7Accarea
    Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
    Hop7Vel = cumsum(Hop7Vela)
    
    #Velocity Area
    Hop7Velavg = rollapply(Hop7Vel, 2, mean)
    Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
    Hop7Velarea = Hop7Velavg2*0.001
    
    #Displacement
    Hop7Disp = cumsum(Hop7Velarea)
    
    l = length(Hop7Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop7Disp[l]>0){
      while (Hop7Disp[l] > 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit - 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)}
    }else (Hop7Disp[l]<0) 
    {
      while (Hop7Disp[l] < 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit + 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop7 = max(Hop7)
    PeakHop7PC = PeakHop7/BW
    
    
    ContactHop7 <- 5 + which(Hop7[5:length(Hop7)] >5)[1]
    ContactTimeHop7 = (length(Hop7Disp) - ContactHop7)*0.001
    MinHop7Disp = min(Hop7Disp)
    EccHop7Disp = (Hop7Disp[ContactHop7] - MinHop7Disp)
    EccStiffHop7 = ((PeakHop7 - BW)/EccHop7Disp)/input$Mass
    
    JumpFreqHop7 = 1/(length(FullHop7)*0.001)
    
    #########################
    
    #Hop8
    
    #Calculate Acceleration
    
    Hop8Acc = (Hop8-BW)/input$Mass
    
    Hop8Accavg = rollapply(Hop8Acc, 2, mean)
    Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
    
    #Acceleration Area
    Hop8Accarea = Hop8Accavg2*0.001
    Hop8VelInit = 1.3
    
    #Velocity
    Hop8Vela = Hop8Accarea
    Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
    Hop8Vel = cumsum(Hop8Vela)
    
    #Velocity Area
    Hop8Velavg = rollapply(Hop8Vel, 2, mean)
    Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
    Hop8Velarea = Hop8Velavg2*0.001
    
    #Displacement
    Hop8Disp = cumsum(Hop8Velarea)
    
    l = length(Hop8Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop8Disp[l]>0){
      while (Hop8Disp[l] > 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit - 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)}
    }else (Hop8Disp[l]<0) 
    {
      while (Hop8Disp[l] < 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit + 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop8 = max(Hop8)
    PeakHop8PC = PeakHop8/BW
    
    
    ContactHop8 <- 5 + which(Hop8[5:length(Hop8)] >5)[1]
    ContactTimeHop8 = (length(Hop8Disp) - ContactHop8)*0.001
    MinHop8Disp = min(Hop8Disp)
    EccHop8Disp = (Hop8Disp[ContactHop8] - MinHop8Disp)
    EccStiffHop8 = ((PeakHop8 - BW)/EccHop8Disp)/input$Mass
    
    JumpFreqHop8 = 1/(length(FullHop8)*0.001)
    
    #########################
    
    #Hop9
    
    #Calculate Acceleration
    
    Hop9Acc = (Hop9-BW)/input$Mass
    
    Hop9Accavg = rollapply(Hop9Acc, 2, mean)
    Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
    
    #Acceleration Area
    Hop9Accarea = Hop9Accavg2*0.001
    Hop9VelInit = 1.3
    
    #Velocity
    Hop9Vela = Hop9Accarea
    Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
    Hop9Vel = cumsum(Hop9Vela)
    
    #Velocity Area
    Hop9Velavg = rollapply(Hop9Vel, 2, mean)
    Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
    Hop9Velarea = Hop9Velavg2*0.001
    
    #Displacement
    Hop9Disp = cumsum(Hop9Velarea)
    
    l = length(Hop9Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop9Disp[l]>0){
      while (Hop9Disp[l] > 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit - 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)}
    }else (Hop9Disp[l]<0) 
    {
      while (Hop9Disp[l] < 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit + 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop9 = max(Hop9)
    PeakHop9PC = PeakHop9/BW
    
    
    ContactHop9 <- 5 + which(Hop9[5:length(Hop9)] >5)[1]
    ContactTimeHop9 = (length(Hop9Disp) - ContactHop9)*0.001
    MinHop9Disp = min(Hop9Disp)
    EccHop9Disp = (Hop9Disp[ContactHop9] - MinHop9Disp)
    EccStiffHop9 = ((PeakHop9 - BW)/EccHop9Disp)/input$Mass
    
    JumpFreqHop9 = 1/(length(FullHop9)*0.001)
    
    #########################
    
    #Hop10
    
    #Calculate Acceleration
    
    Hop10Acc = (Hop10-BW)/input$Mass
    
    Hop10Accavg = rollapply(Hop10Acc, 2, mean)
    Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
    
    #Acceleration Area
    Hop10Accarea = Hop10Accavg2*0.001
    Hop10VelInit = 1.3
    
    #Velocity
    Hop10Vela = Hop10Accarea
    Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
    Hop10Vel = cumsum(Hop10Vela)
    
    #Velocity Area
    Hop10Velavg = rollapply(Hop10Vel, 2, mean)
    Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
    Hop10Velarea = Hop10Velavg2*0.001
    
    #Displacement
    Hop10Disp = cumsum(Hop10Velarea)
    
    l = length(Hop10Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop10Disp[l]>0){
      while (Hop10Disp[l] > 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit - 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)}
    }else (Hop10Disp[l]<0) 
    {
      while (Hop10Disp[l] < 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit + 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop10 = max(Hop10)
    PeakHop10PC = PeakHop10/BW
    
    
    ContactHop10 <- 5 + which(Hop10[5:length(Hop10)] >5)[1]
    ContactTimeHop10 = (length(Hop10Disp) - ContactHop10)*0.001
    MinHop10Disp = min(Hop10Disp)
    EccHop10Disp = (Hop10Disp[ContactHop10] - MinHop10Disp)
    EccStiffHop10 = ((PeakHop10 - BW)/EccHop10Disp)/input$Mass
    
    JumpFreqHop10 = 1/(length(FullHop10)*0.001)
    
    #########################
    
    #Hop11
    
    #Calculate Acceleration
    
    Hop11Acc = (Hop11-BW)/input$Mass
    
    Hop11Accavg = rollapply(Hop11Acc, 2, mean)
    Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
    
    #Acceleration Area
    Hop11Accarea = Hop11Accavg2*0.001
    Hop11VelInit = 1.3
    
    #Velocity
    Hop11Vela = Hop11Accarea
    Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
    Hop11Vel = cumsum(Hop11Vela)
    
    #Velocity Area
    Hop11Velavg = rollapply(Hop11Vel, 2, mean)
    Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
    Hop11Velarea = Hop11Velavg2*0.001
    
    #Displacement
    Hop11Disp = cumsum(Hop11Velarea)
    
    l = length(Hop11Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop11Disp[l]>0){
      while (Hop11Disp[l] > 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit - 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)}
    }else (Hop11Disp[l]<0) 
    {
      while (Hop11Disp[l] < 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit + 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop11 = max(Hop11)
    PeakHop11PC = PeakHop11/BW
    
    
    ContactHop11 <- 5 + which(Hop11[5:length(Hop11)] >5)[1]
    ContactTimeHop11 = (length(Hop11Disp) - ContactHop11)*0.001
    MinHop11Disp = min(Hop11Disp)
    EccHop11Disp = (Hop11Disp[ContactHop11] - MinHop11Disp)
    EccStiffHop11 = ((PeakHop11 - BW)/EccHop11Disp)/input$Mass
    
    JumpFreqHop11 = 1/(length(FullHop11)*0.001)
    
    #########################
    
    #Hop12
    
    #Calculate Acceleration
    
    Hop12Acc = (Hop12-BW)/input$Mass
    
    Hop12Accavg = rollapply(Hop12Acc, 2, mean)
    Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
    
    #Acceleration Area
    Hop12Accarea = Hop12Accavg2*0.001
    Hop12VelInit = 1.3
    
    #Velocity
    Hop12Vela = Hop12Accarea
    Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
    Hop12Vel = cumsum(Hop12Vela)
    
    #Velocity Area
    Hop12Velavg = rollapply(Hop12Vel, 2, mean)
    Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
    Hop12Velarea = Hop12Velavg2*0.001
    
    #Displacement
    Hop12Disp = cumsum(Hop12Velarea)
    
    l = length(Hop12Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop12Disp[l]>0){
      while (Hop12Disp[l] > 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit - 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)}
    }else (Hop12Disp[l]<0) 
    {
      while (Hop12Disp[l] < 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit + 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop12 = max(Hop12)
    PeakHop12PC = PeakHop12/BW
    
    
    ContactHop12 <- 5 + which(Hop12[5:length(Hop12)] >5)[1]
    ContactTimeHop12 = (length(Hop12Disp) - ContactHop12)*0.001
    MinHop12Disp = min(Hop12Disp)
    EccHop12Disp = (Hop12Disp[ContactHop12] - MinHop12Disp)
    EccStiffHop12 = ((PeakHop12 - BW)/EccHop12Disp)/input$Mass
    
    JumpFreqHop12 = 1/(length(FullHop12)*0.001)
    
    #########################
    
    #Hop13
    
    #Calculate Acceleration
    
    Hop13Acc = (Hop13-BW)/input$Mass
    
    Hop13Accavg = rollapply(Hop13Acc, 2, mean)
    Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
    
    #Acceleration Area
    Hop13Accarea = Hop13Accavg2*0.001
    Hop13VelInit = 1.3
    
    #Velocity
    Hop13Vela = Hop13Accarea
    Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
    Hop13Vel = cumsum(Hop13Vela)
    
    #Velocity Area
    Hop13Velavg = rollapply(Hop13Vel, 2, mean)
    Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
    Hop13Velarea = Hop13Velavg2*0.001
    
    #Displacement
    Hop13Disp = cumsum(Hop13Velarea)
    
    l = length(Hop13Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop13Disp[l]>0){
      while (Hop13Disp[l] > 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit - 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)}
    }else (Hop13Disp[l]<0) 
    {
      while (Hop13Disp[l] < 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit + 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop13 = max(Hop13)
    PeakHop13PC = PeakHop13/BW
    
    
    ContactHop13 <- 5 + which(Hop13[5:length(Hop13)] >5)[1]
    ContactTimeHop13 = (length(Hop13Disp) - ContactHop13)*0.001
    MinHop13Disp = min(Hop13Disp)
    EccHop13Disp = (Hop13Disp[ContactHop13] - MinHop13Disp)
    EccStiffHop13 = ((PeakHop13 - BW)/EccHop13Disp)/input$Mass
    
    JumpFreqHop13 = 1/(length(FullHop13)*0.001)
    
    #########################
    
    #Hop14
    
    #Calculate Acceleration
    
    Hop14Acc = (Hop14-BW)/input$Mass
    
    Hop14Accavg = rollapply(Hop14Acc, 2, mean)
    Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
    
    #Acceleration Area
    Hop14Accarea = Hop14Accavg2*0.001
    Hop14VelInit = 1.3
    
    #Velocity
    Hop14Vela = Hop14Accarea
    Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
    Hop14Vel = cumsum(Hop14Vela)
    
    #Velocity Area
    Hop14Velavg = rollapply(Hop14Vel, 2, mean)
    Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
    Hop14Velarea = Hop14Velavg2*0.001
    
    #Displacement
    Hop14Disp = cumsum(Hop14Velarea)
    
    l = length(Hop14Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop14Disp[l]>0){
      while (Hop14Disp[l] > 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit - 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)}
    }else (Hop14Disp[l]<0) 
    {
      while (Hop14Disp[l] < 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit + 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop14 = max(Hop14)
    PeakHop14PC = PeakHop14/BW
    
    
    ContactHop14 <- 5 + which(Hop14[5:length(Hop14)] >5)[1]
    ContactTimeHop14 = (length(Hop14Disp) - ContactHop14)*0.001
    MinHop14Disp = min(Hop14Disp)
    EccHop14Disp = (Hop14Disp[ContactHop14] - MinHop14Disp)
    EccStiffHop14 = ((PeakHop14 - BW)/EccHop14Disp)/input$Mass
    
    JumpFreqHop14 = 1/(length(FullHop14)*0.001)
    
    #########################
    
    # #Hop15
    # 
    # #Calculate Acceleration
    # 
    # Hop15Acc = (Hop15-BW)/input$Mass
    # 
    # Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    # Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    # 
    # #Acceleration Area
    # Hop15Accarea = Hop15Accavg2*0.001
    # Hop15VelInit = 1.3
    # 
    # #Velocity
    # Hop15Vela = Hop15Accarea
    # Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    # Hop15Vel = cumsum(Hop15Vela)
    # 
    # #Velocity Area
    # Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    # Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    # Hop15Velarea = Hop15Velavg2*0.001
    # 
    # #Displacement
    # Hop15Disp = cumsum(Hop15Velarea)
    # 
    # l = length(Hop15Disp)
    # 
    # #Adjust Init Velocity to get Final Displacment close to zero
    # if (Hop15Disp[l]>0){
    #   while (Hop15Disp[l] > 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit - 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)}
    # }else (Hop15Disp[l]<0) 
    # {
    #   while (Hop15Disp[l] < 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit + 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)
    #   }
    # }
    # 
    # 
    # #Calculate output variables
    # PeakHop15 = max(Hop15)
    # PeakHop15PC = PeakHop15/BW
    # 
    # 
    # ContactHop15 <- 5 + which(Hop15[5:length(Hop15)] >5)[1]
    # ContactTimeHop15 = (length(Hop15Disp) - ContactHop15)*0.001
    # MinHop15Disp = min(Hop15Disp)
    # EccHop15Disp = (Hop15Disp[ContactHop15] - MinHop15Disp)
    # EccStiffHop15 = ((PeakHop15 - BW)/EccHop15Disp)/input$Mass
    # 
    # JumpFreqHop15 = 1/(length(FullHop15)*0.001)
    # 
    # 
    # 
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14", "Average")
    EccCOMDisp <- rbind(round(EccHop1Disp,digits=2), round(EccHop2Disp,digits=2), round(EccHop3Disp,digits=2), round(EccHop4Disp,digits=2), round(EccHop5Disp,digits=2), round(EccHop6Disp,digits=2), round(EccHop7Disp,digits=2), round(EccHop8Disp,digits=2), round(EccHop9Disp,digits=2), round(EccHop10Disp,digits=2), round(EccHop11Disp,digits=2), round(EccHop12Disp,digits=2), round(EccHop13Disp,digits=2), round(EccHop14Disp,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2))
    PeakForcePC <- rbind(round(PeakHop1PC,digits=2), round(PeakHop2PC,digits=2), round(PeakHop3PC,digits=2), round(PeakHop4PC,digits=2), round(PeakHop5PC,digits=2), round(PeakHop6PC,digits=2), round(PeakHop7PC,digits=2), round(PeakHop8PC,digits=2), round(PeakHop9PC,digits=2), round(PeakHop10PC,digits=2), round(PeakHop11PC,digits=2), round(PeakHop12PC,digits=2), round(PeakHop13PC,digits=2), round(PeakHop14PC,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    Right2.3Averages <<-cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
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
    
  })
  output$resultstableRight2.6 <- renderTable({
    
    data1 <- 
      read.csv(input$Right2.6$datapath, stringsAsFactors = F, skip = 17)
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <5)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    for (r in 1:15){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <5)
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
        geom_vline(xintercept=Contact[15], col="red") 
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHop2 <- Fz2$Fz2[Contact[2]:Contact[3]]
    FullHop3 <- Fz2$Fz2[Contact[3]:Contact[4]]
    FullHop4 <- Fz2$Fz2[Contact[4]:Contact[5]]
    FullHop5 <- Fz2$Fz2[Contact[5]:Contact[6]]
    FullHop6 <- Fz2$Fz2[Contact[6]:Contact[7]]
    FullHop7 <- Fz2$Fz2[Contact[7]:Contact[8]]
    FullHop8 <- Fz2$Fz2[Contact[8]:Contact[9]]
    FullHop9 <- Fz2$Fz2[Contact[9]:Contact[10]]
    FullHop10 <- Fz2$Fz2[Contact[10]:Contact[11]]
    FullHop11 <- Fz2$Fz2[Contact[11]:Contact[12]]
    FullHop12 <- Fz2$Fz2[Contact[12]:Contact[13]]
    FullHop13 <- Fz2$Fz2[Contact[13]:Contact[14]]
    FullHop14 <- Fz2$Fz2[Contact[14]:Contact[15]]
    #    FullHop15 <- Fz2$Fz2[Contact[15]:Contact[16]]
    
    Hop1 <- Fz2$Fz2[Flight[1]:Flight[2]]
    Hop2 <- Fz2$Fz2[Flight[2]:Flight[3]]
    Hop3 <- Fz2$Fz2[Flight[3]:Flight[4]]
    Hop4 <- Fz2$Fz2[Flight[4]:Flight[5]]
    Hop5 <- Fz2$Fz2[Flight[5]:Flight[6]]
    Hop6 <- Fz2$Fz2[Flight[6]:Flight[7]]
    Hop7 <- Fz2$Fz2[Flight[7]:Flight[8]]
    Hop8 <- Fz2$Fz2[Flight[8]:Flight[9]]
    Hop9 <- Fz2$Fz2[Flight[9]:Flight[10]]
    Hop10 <- Fz2$Fz2[Flight[10]:Flight[11]]
    Hop11 <- Fz2$Fz2[Flight[11]:Flight[12]]
    Hop12 <- Fz2$Fz2[Flight[12]:Flight[13]]
    Hop13 <- Fz2$Fz2[Flight[13]:Flight[14]]
    Hop14 <- Fz2$Fz2[Flight[14]:Flight[15]]
    Hop15 <- Fz2$Fz2[Flight[15]:Flight[16]]
    #    Hop16 <- Fz2$Fz2[Flight[16]:Flight[17]]
    
    # windows()
    # plot(FullHop1)
    # windows()
    # plot(FullHop2)
    # windows()
    # plot(FullHop3)
    # windows()
    # plot(FullHop4)
    # windows()
    # plot(FullHop5)
    # windows()
    # plot(FullHop6)
    # windows()
    # plot(FullHop7)
    # windows()
    # plot(FullHop8)
    # windows()
    # plot(FullHop9)
    # windows()
    # plot(FullHop10)
    # windows()
    # plot(FullHop11)
    # windows()
    # plot(FullHop12)
    # windows()
    # plot(FullHop13)
    # windows()
    # plot(FullHop14)
    # windows()
    # plot(FullHop15)
    
    #Hop1
    
    #Calculate Acceleration
    
    Hop1Acc = (Hop1-BW)/input$Mass
    
    Hop1Accavg = rollapply(Hop1Acc, 2, mean)
    Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
    
    #Acceleration Area
    Hop1Accarea = Hop1Accavg2*0.001
    Hop1VelInit = 1.3
    
    #Velocity
    Hop1Vela = Hop1Accarea
    Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
    Hop1Vel = cumsum(Hop1Vela)
    
    #Velocity Area
    Hop1Velavg = rollapply(Hop1Vel, 2, mean)
    Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
    Hop1Velarea = Hop1Velavg2*0.001
    
    #Displacement
    Hop1Disp = cumsum(Hop1Velarea)
    
    l = length(Hop1Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop1Disp[l]>0){
      while (Hop1Disp[l] > 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit - 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)}
    }else (Hop1Disp[l]<0) 
    {
      while (Hop1Disp[l] < 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit + 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop1 = max(Hop1)
    PeakHop1PC = PeakHop1/BW
    
    
    ContactHop1 <- 5 + which(Hop1[5:length(Hop1)] >5)[1]
    ContactTimeHop1 = (length(Hop1Disp) - ContactHop1)*0.001
    MinHop1Disp = min(Hop1Disp)
    EccHop1Disp = (Hop1Disp[ContactHop1] - MinHop1Disp)
    EccStiffHop1 = ((PeakHop1 - BW)/EccHop1Disp)/input$Mass
    
    JumpFreqHop1 = 1/(length(FullHop1)*0.001)
    
    #####################
    
    #Hop2
    
    #Calculate Acceleration
    
    Hop2Acc = (Hop2-BW)/input$Mass
    
    Hop2Accavg = rollapply(Hop2Acc, 2, mean)
    Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
    
    #Acceleration Area
    Hop2Accarea = Hop2Accavg2*0.001
    Hop2VelInit = 1.3
    
    #Velocity
    Hop2Vela = Hop2Accarea
    Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
    Hop2Vel = cumsum(Hop2Vela)
    
    #Velocity Area
    Hop2Velavg = rollapply(Hop2Vel, 2, mean)
    Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
    Hop2Velarea = Hop2Velavg2*0.001
    
    #Displacement
    Hop2Disp = cumsum(Hop2Velarea)
    
    l = length(Hop2Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop2Disp[l]>0){
      while (Hop2Disp[l] > 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit - 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)}
    }else (Hop2Disp[l]<0) 
    {
      while (Hop2Disp[l] < 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit + 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop2 = max(Hop2)
    PeakHop2PC = PeakHop2/BW
    
    
    ContactHop2 <- 5 + which(Hop2[5:length(Hop2)] >5)[1]
    ContactTimeHop2 = (length(Hop2Disp) - ContactHop2)*0.001
    MinHop2Disp = min(Hop2Disp)
    EccHop2Disp = (Hop2Disp[ContactHop2] - MinHop2Disp)
    EccStiffHop2 = ((PeakHop2 - BW)/EccHop2Disp)/input$Mass
    
    JumpFreqHop2 = 1/(length(FullHop2)*0.001)
    
    #########################
    
    #Hop3
    
    #Calculate Acceleration
    
    Hop3Acc = (Hop3-BW)/input$Mass
    
    Hop3Accavg = rollapply(Hop3Acc, 2, mean)
    Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
    
    #Acceleration Area
    Hop3Accarea = Hop3Accavg2*0.001
    Hop3VelInit = 1.3
    
    #Velocity
    Hop3Vela = Hop3Accarea
    Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
    Hop3Vel = cumsum(Hop3Vela)
    
    #Velocity Area
    Hop3Velavg = rollapply(Hop3Vel, 2, mean)
    Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
    Hop3Velarea = Hop3Velavg2*0.001
    
    #Displacement
    Hop3Disp = cumsum(Hop3Velarea)
    
    l = length(Hop3Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop3Disp[l]>0){
      while (Hop3Disp[l] > 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit - 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)}
    }else (Hop3Disp[l]<0) 
    {
      while (Hop3Disp[l] < 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit + 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop3 = max(Hop3)
    PeakHop3PC = PeakHop3/BW
    
    
    ContactHop3 <- 5 + which(Hop3[5:length(Hop3)] >5)[1]
    ContactTimeHop3 = (length(Hop3Disp) - ContactHop3)*0.001
    MinHop3Disp = min(Hop3Disp)
    EccHop3Disp = (Hop3Disp[ContactHop3] - MinHop3Disp)
    EccStiffHop3 = ((PeakHop3 - BW)/EccHop3Disp)/input$Mass
    
    JumpFreqHop3 = 1/(length(FullHop3)*0.001)
    
    #########################
    
    #Hop4
    
    #Calculate Acceleration
    
    Hop4Acc = (Hop4-BW)/input$Mass
    
    Hop4Accavg = rollapply(Hop4Acc, 2, mean)
    Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
    
    #Acceleration Area
    Hop4Accarea = Hop4Accavg2*0.001
    Hop4VelInit = 1.3
    
    #Velocity
    Hop4Vela = Hop4Accarea
    Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
    Hop4Vel = cumsum(Hop4Vela)
    
    #Velocity Area
    Hop4Velavg = rollapply(Hop4Vel, 2, mean)
    Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
    Hop4Velarea = Hop4Velavg2*0.001
    
    #Displacement
    Hop4Disp = cumsum(Hop4Velarea)
    
    l = length(Hop4Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop4Disp[l]>0){
      while (Hop4Disp[l] > 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit - 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)}
    }else (Hop4Disp[l]<0) 
    {
      while (Hop4Disp[l] < 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit + 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop4 = max(Hop4)
    PeakHop4PC = PeakHop4/BW
    
    
    ContactHop4 <- 5 + which(Hop4[5:length(Hop4)] >5)[1]
    ContactTimeHop4 = (length(Hop4Disp) - ContactHop4)*0.001
    MinHop4Disp = min(Hop4Disp)
    EccHop4Disp = (Hop4Disp[ContactHop4] - MinHop4Disp)
    EccStiffHop4 = ((PeakHop4 - BW)/EccHop4Disp)/input$Mass
    
    JumpFreqHop4 = 1/(length(FullHop4)*0.001)
    
    #########################
    
    #Hop5
    
    #Calculate Acceleration
    
    Hop5Acc = (Hop5-BW)/input$Mass
    
    Hop5Accavg = rollapply(Hop5Acc, 2, mean)
    Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
    
    #Acceleration Area
    Hop5Accarea = Hop5Accavg2*0.001
    Hop5VelInit = 1.3
    
    #Velocity
    Hop5Vela = Hop5Accarea
    Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
    Hop5Vel = cumsum(Hop5Vela)
    
    #Velocity Area
    Hop5Velavg = rollapply(Hop5Vel, 2, mean)
    Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
    Hop5Velarea = Hop5Velavg2*0.001
    
    #Displacement
    Hop5Disp = cumsum(Hop5Velarea)
    
    l = length(Hop5Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop5Disp[l]>0){
      while (Hop5Disp[l] > 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit - 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)}
    }else (Hop5Disp[l]<0) 
    {
      while (Hop5Disp[l] < 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit + 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop5 = max(Hop5)
    PeakHop5PC = PeakHop5/BW
    
    
    ContactHop5 <- 5 + which(Hop5[5:length(Hop5)] >5)[1]
    ContactTimeHop5 = (length(Hop5Disp) - ContactHop5)*0.001
    MinHop5Disp = min(Hop5Disp)
    EccHop5Disp = (Hop5Disp[ContactHop5] - MinHop5Disp)
    EccStiffHop5 = ((PeakHop5 - BW)/EccHop5Disp)/input$Mass
    
    JumpFreqHop5 = 1/(length(FullHop5)*0.001)
    
    #########################
    
    
    #Hop6
    
    #Calculate Acceleration
    
    Hop6Acc = (Hop6-BW)/input$Mass
    
    Hop6Accavg = rollapply(Hop6Acc, 2, mean)
    Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
    
    #Acceleration Area
    Hop6Accarea = Hop6Accavg2*0.001
    Hop6VelInit = 1.3
    
    #Velocity
    Hop6Vela = Hop6Accarea
    Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
    Hop6Vel = cumsum(Hop6Vela)
    
    #Velocity Area
    Hop6Velavg = rollapply(Hop6Vel, 2, mean)
    Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
    Hop6Velarea = Hop6Velavg2*0.001
    
    #Displacement
    Hop6Disp = cumsum(Hop6Velarea)
    
    l = length(Hop6Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop6Disp[l]>0){
      while (Hop6Disp[l] > 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit - 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)}
    }else (Hop6Disp[l]<0) 
    {
      while (Hop6Disp[l] < 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit + 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop6 = max(Hop6)
    PeakHop6PC = PeakHop6/BW
    
    
    ContactHop6 <- 5 + which(Hop6[5:length(Hop6)] >5)[1]
    ContactTimeHop6 = (length(Hop6Disp) - ContactHop6)*0.001
    MinHop6Disp = min(Hop6Disp)
    EccHop6Disp = (Hop6Disp[ContactHop6] - MinHop6Disp)
    EccStiffHop6 = ((PeakHop6 - BW)/EccHop6Disp)/input$Mass
    
    JumpFreqHop6 = 1/(length(FullHop6)*0.001)
    
    #########################
    
    
    #Hop7
    
    #Calculate Acceleration
    
    Hop7Acc = (Hop7-BW)/input$Mass
    
    Hop7Accavg = rollapply(Hop7Acc, 2, mean)
    Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
    
    #Acceleration Area
    Hop7Accarea = Hop7Accavg2*0.001
    Hop7VelInit = 1.3
    
    #Velocity
    Hop7Vela = Hop7Accarea
    Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
    Hop7Vel = cumsum(Hop7Vela)
    
    #Velocity Area
    Hop7Velavg = rollapply(Hop7Vel, 2, mean)
    Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
    Hop7Velarea = Hop7Velavg2*0.001
    
    #Displacement
    Hop7Disp = cumsum(Hop7Velarea)
    
    l = length(Hop7Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop7Disp[l]>0){
      while (Hop7Disp[l] > 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit - 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)}
    }else (Hop7Disp[l]<0) 
    {
      while (Hop7Disp[l] < 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit + 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop7 = max(Hop7)
    PeakHop7PC = PeakHop7/BW
    
    
    ContactHop7 <- 5 + which(Hop7[5:length(Hop7)] >5)[1]
    ContactTimeHop7 = (length(Hop7Disp) - ContactHop7)*0.001
    MinHop7Disp = min(Hop7Disp)
    EccHop7Disp = (Hop7Disp[ContactHop7] - MinHop7Disp)
    EccStiffHop7 = ((PeakHop7 - BW)/EccHop7Disp)/input$Mass
    
    JumpFreqHop7 = 1/(length(FullHop7)*0.001)
    
    #########################
    
    #Hop8
    
    #Calculate Acceleration
    
    Hop8Acc = (Hop8-BW)/input$Mass
    
    Hop8Accavg = rollapply(Hop8Acc, 2, mean)
    Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
    
    #Acceleration Area
    Hop8Accarea = Hop8Accavg2*0.001
    Hop8VelInit = 1.3
    
    #Velocity
    Hop8Vela = Hop8Accarea
    Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
    Hop8Vel = cumsum(Hop8Vela)
    
    #Velocity Area
    Hop8Velavg = rollapply(Hop8Vel, 2, mean)
    Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
    Hop8Velarea = Hop8Velavg2*0.001
    
    #Displacement
    Hop8Disp = cumsum(Hop8Velarea)
    
    l = length(Hop8Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop8Disp[l]>0){
      while (Hop8Disp[l] > 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit - 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)}
    }else (Hop8Disp[l]<0) 
    {
      while (Hop8Disp[l] < 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit + 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop8 = max(Hop8)
    PeakHop8PC = PeakHop8/BW
    
    
    ContactHop8 <- 5 + which(Hop8[5:length(Hop8)] >5)[1]
    ContactTimeHop8 = (length(Hop8Disp) - ContactHop8)*0.001
    MinHop8Disp = min(Hop8Disp)
    EccHop8Disp = (Hop8Disp[ContactHop8] - MinHop8Disp)
    EccStiffHop8 = ((PeakHop8 - BW)/EccHop8Disp)/input$Mass
    
    JumpFreqHop8 = 1/(length(FullHop8)*0.001)
    
    #########################
    
    #Hop9
    
    #Calculate Acceleration
    
    Hop9Acc = (Hop9-BW)/input$Mass
    
    Hop9Accavg = rollapply(Hop9Acc, 2, mean)
    Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
    
    #Acceleration Area
    Hop9Accarea = Hop9Accavg2*0.001
    Hop9VelInit = 1.3
    
    #Velocity
    Hop9Vela = Hop9Accarea
    Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
    Hop9Vel = cumsum(Hop9Vela)
    
    #Velocity Area
    Hop9Velavg = rollapply(Hop9Vel, 2, mean)
    Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
    Hop9Velarea = Hop9Velavg2*0.001
    
    #Displacement
    Hop9Disp = cumsum(Hop9Velarea)
    
    l = length(Hop9Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop9Disp[l]>0){
      while (Hop9Disp[l] > 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit - 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)}
    }else (Hop9Disp[l]<0) 
    {
      while (Hop9Disp[l] < 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit + 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop9 = max(Hop9)
    PeakHop9PC = PeakHop9/BW
    
    
    ContactHop9 <- 5 + which(Hop9[5:length(Hop9)] >5)[1]
    ContactTimeHop9 = (length(Hop9Disp) - ContactHop9)*0.001
    MinHop9Disp = min(Hop9Disp)
    EccHop9Disp = (Hop9Disp[ContactHop9] - MinHop9Disp)
    EccStiffHop9 = ((PeakHop9 - BW)/EccHop9Disp)/input$Mass
    
    JumpFreqHop9 = 1/(length(FullHop9)*0.001)
    
    #########################
    
    #Hop10
    
    #Calculate Acceleration
    
    Hop10Acc = (Hop10-BW)/input$Mass
    
    Hop10Accavg = rollapply(Hop10Acc, 2, mean)
    Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
    
    #Acceleration Area
    Hop10Accarea = Hop10Accavg2*0.001
    Hop10VelInit = 1.3
    
    #Velocity
    Hop10Vela = Hop10Accarea
    Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
    Hop10Vel = cumsum(Hop10Vela)
    
    #Velocity Area
    Hop10Velavg = rollapply(Hop10Vel, 2, mean)
    Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
    Hop10Velarea = Hop10Velavg2*0.001
    
    #Displacement
    Hop10Disp = cumsum(Hop10Velarea)
    
    l = length(Hop10Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop10Disp[l]>0){
      while (Hop10Disp[l] > 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit - 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)}
    }else (Hop10Disp[l]<0) 
    {
      while (Hop10Disp[l] < 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit + 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop10 = max(Hop10)
    PeakHop10PC = PeakHop10/BW
    
    
    ContactHop10 <- 5 + which(Hop10[5:length(Hop10)] >5)[1]
    ContactTimeHop10 = (length(Hop10Disp) - ContactHop10)*0.001
    MinHop10Disp = min(Hop10Disp)
    EccHop10Disp = (Hop10Disp[ContactHop10] - MinHop10Disp)
    EccStiffHop10 = ((PeakHop10 - BW)/EccHop10Disp)/input$Mass
    
    JumpFreqHop10 = 1/(length(FullHop10)*0.001)
    
    #########################
    
    #Hop11
    
    #Calculate Acceleration
    
    Hop11Acc = (Hop11-BW)/input$Mass
    
    Hop11Accavg = rollapply(Hop11Acc, 2, mean)
    Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
    
    #Acceleration Area
    Hop11Accarea = Hop11Accavg2*0.001
    Hop11VelInit = 1.3
    
    #Velocity
    Hop11Vela = Hop11Accarea
    Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
    Hop11Vel = cumsum(Hop11Vela)
    
    #Velocity Area
    Hop11Velavg = rollapply(Hop11Vel, 2, mean)
    Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
    Hop11Velarea = Hop11Velavg2*0.001
    
    #Displacement
    Hop11Disp = cumsum(Hop11Velarea)
    
    l = length(Hop11Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop11Disp[l]>0){
      while (Hop11Disp[l] > 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit - 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)}
    }else (Hop11Disp[l]<0) 
    {
      while (Hop11Disp[l] < 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit + 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop11 = max(Hop11)
    PeakHop11PC = PeakHop11/BW
    
    
    ContactHop11 <- 5 + which(Hop11[5:length(Hop11)] >5)[1]
    ContactTimeHop11 = (length(Hop11Disp) - ContactHop11)*0.001
    MinHop11Disp = min(Hop11Disp)
    EccHop11Disp = (Hop11Disp[ContactHop11] - MinHop11Disp)
    EccStiffHop11 = ((PeakHop11 - BW)/EccHop11Disp)/input$Mass
    
    JumpFreqHop11 = 1/(length(FullHop11)*0.001)
    
    #########################
    
    #Hop12
    
    #Calculate Acceleration
    
    Hop12Acc = (Hop12-BW)/input$Mass
    
    Hop12Accavg = rollapply(Hop12Acc, 2, mean)
    Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
    
    #Acceleration Area
    Hop12Accarea = Hop12Accavg2*0.001
    Hop12VelInit = 1.3
    
    #Velocity
    Hop12Vela = Hop12Accarea
    Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
    Hop12Vel = cumsum(Hop12Vela)
    
    #Velocity Area
    Hop12Velavg = rollapply(Hop12Vel, 2, mean)
    Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
    Hop12Velarea = Hop12Velavg2*0.001
    
    #Displacement
    Hop12Disp = cumsum(Hop12Velarea)
    
    l = length(Hop12Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop12Disp[l]>0){
      while (Hop12Disp[l] > 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit - 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)}
    }else (Hop12Disp[l]<0) 
    {
      while (Hop12Disp[l] < 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit + 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop12 = max(Hop12)
    PeakHop12PC = PeakHop12/BW
    
    
    ContactHop12 <- 5 + which(Hop12[5:length(Hop12)] >5)[1]
    ContactTimeHop12 = (length(Hop12Disp) - ContactHop12)*0.001
    MinHop12Disp = min(Hop12Disp)
    EccHop12Disp = (Hop12Disp[ContactHop12] - MinHop12Disp)
    EccStiffHop12 = ((PeakHop12 - BW)/EccHop12Disp)/input$Mass
    
    JumpFreqHop12 = 1/(length(FullHop12)*0.001)
    
    #########################
    
    #Hop13
    
    #Calculate Acceleration
    
    Hop13Acc = (Hop13-BW)/input$Mass
    
    Hop13Accavg = rollapply(Hop13Acc, 2, mean)
    Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
    
    #Acceleration Area
    Hop13Accarea = Hop13Accavg2*0.001
    Hop13VelInit = 1.3
    
    #Velocity
    Hop13Vela = Hop13Accarea
    Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
    Hop13Vel = cumsum(Hop13Vela)
    
    #Velocity Area
    Hop13Velavg = rollapply(Hop13Vel, 2, mean)
    Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
    Hop13Velarea = Hop13Velavg2*0.001
    
    #Displacement
    Hop13Disp = cumsum(Hop13Velarea)
    
    l = length(Hop13Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop13Disp[l]>0){
      while (Hop13Disp[l] > 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit - 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)}
    }else (Hop13Disp[l]<0) 
    {
      while (Hop13Disp[l] < 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit + 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop13 = max(Hop13)
    PeakHop13PC = PeakHop13/BW
    
    
    ContactHop13 <- 5 + which(Hop13[5:length(Hop13)] >5)[1]
    ContactTimeHop13 = (length(Hop13Disp) - ContactHop13)*0.001
    MinHop13Disp = min(Hop13Disp)
    EccHop13Disp = (Hop13Disp[ContactHop13] - MinHop13Disp)
    EccStiffHop13 = ((PeakHop13 - BW)/EccHop13Disp)/input$Mass
    
    JumpFreqHop13 = 1/(length(FullHop13)*0.001)
    
    #########################
    
    #Hop14
    
    #Calculate Acceleration
    
    Hop14Acc = (Hop14-BW)/input$Mass
    
    Hop14Accavg = rollapply(Hop14Acc, 2, mean)
    Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
    
    #Acceleration Area
    Hop14Accarea = Hop14Accavg2*0.001
    Hop14VelInit = 1.3
    
    #Velocity
    Hop14Vela = Hop14Accarea
    Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
    Hop14Vel = cumsum(Hop14Vela)
    
    #Velocity Area
    Hop14Velavg = rollapply(Hop14Vel, 2, mean)
    Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
    Hop14Velarea = Hop14Velavg2*0.001
    
    #Displacement
    Hop14Disp = cumsum(Hop14Velarea)
    
    l = length(Hop14Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop14Disp[l]>0){
      while (Hop14Disp[l] > 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit - 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)}
    }else (Hop14Disp[l]<0) 
    {
      while (Hop14Disp[l] < 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit + 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop14 = max(Hop14)
    PeakHop14PC = PeakHop14/BW
    
    
    ContactHop14 <- 5 + which(Hop14[5:length(Hop14)] >5)[1]
    ContactTimeHop14 = (length(Hop14Disp) - ContactHop14)*0.001
    MinHop14Disp = min(Hop14Disp)
    EccHop14Disp = (Hop14Disp[ContactHop14] - MinHop14Disp)
    EccStiffHop14 = ((PeakHop14 - BW)/EccHop14Disp)/input$Mass
    
    JumpFreqHop14 = 1/(length(FullHop14)*0.001)
    
    #########################
    
    # #Hop15
    # 
    # #Calculate Acceleration
    # 
    # Hop15Acc = (Hop15-BW)/input$Mass
    # 
    # Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    # Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    # 
    # #Acceleration Area
    # Hop15Accarea = Hop15Accavg2*0.001
    # Hop15VelInit = 1.3
    # 
    # #Velocity
    # Hop15Vela = Hop15Accarea
    # Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    # Hop15Vel = cumsum(Hop15Vela)
    # 
    # #Velocity Area
    # Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    # Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    # Hop15Velarea = Hop15Velavg2*0.001
    # 
    # #Displacement
    # Hop15Disp = cumsum(Hop15Velarea)
    # 
    # l = length(Hop15Disp)
    # 
    # #Adjust Init Velocity to get Final Displacment close to zero
    # if (Hop15Disp[l]>0){
    #   while (Hop15Disp[l] > 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit - 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)}
    # }else (Hop15Disp[l]<0) 
    # {
    #   while (Hop15Disp[l] < 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit + 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)
    #   }
    # }
    # 
    # 
    # #Calculate output variables
    # PeakHop15 = max(Hop15)
    # PeakHop15PC = PeakHop15/BW
    # 
    # 
    # ContactHop15 <- 5 + which(Hop15[5:length(Hop15)] >5)[1]
    # ContactTimeHop15 = (length(Hop15Disp) - ContactHop15)*0.001
    # MinHop15Disp = min(Hop15Disp)
    # EccHop15Disp = (Hop15Disp[ContactHop15] - MinHop15Disp)
    # EccStiffHop15 = ((PeakHop15 - BW)/EccHop15Disp)/input$Mass
    # 
    # JumpFreqHop15 = 1/(length(FullHop15)*0.001)
    # 
    # 
    # 
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14", "Average")
    EccCOMDisp <- rbind(round(EccHop1Disp,digits=2), round(EccHop2Disp,digits=2), round(EccHop3Disp,digits=2), round(EccHop4Disp,digits=2), round(EccHop5Disp,digits=2), round(EccHop6Disp,digits=2), round(EccHop7Disp,digits=2), round(EccHop8Disp,digits=2), round(EccHop9Disp,digits=2), round(EccHop10Disp,digits=2), round(EccHop11Disp,digits=2), round(EccHop12Disp,digits=2), round(EccHop13Disp,digits=2), round(EccHop14Disp,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2))
    PeakForcePC <- rbind(round(PeakHop1PC,digits=2), round(PeakHop2PC,digits=2), round(PeakHop3PC,digits=2), round(PeakHop4PC,digits=2), round(PeakHop5PC,digits=2), round(PeakHop6PC,digits=2), round(PeakHop7PC,digits=2), round(PeakHop8PC,digits=2), round(PeakHop9PC,digits=2), round(PeakHop10PC,digits=2), round(PeakHop11PC,digits=2), round(PeakHop12PC,digits=2), round(PeakHop13PC,digits=2), round(PeakHop14PC,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    Right2.6Averages <<-cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
    
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
    
  })
  output$resultstableRightSS <- renderTable({
    
    data1 <- 
      read.csv(input$RightSS$datapath, stringsAsFactors = F, skip = 17)
    newdata <-  na.omit(data1)
    
    Fz2 <-  as.numeric(newdata[,4])
    Fz2 <-  as.data.frame(Fz2)
    
    #req(input$Mass)
    BW = (input$Mass)*9.8
    Flight <- which(Fz2 <5)[1]
    Contact <- Flight[1] - 1 +5 + which(Fz2$Fz2[Flight[1]+5:length(Fz2$Fz2)] >10)[1]
    zerooffset = mean(Fz2$Fz2[Flight:Contact])
    Fz2$Fz2 = Fz2$Fz2 - zerooffset
    
    Flight[1] <- which(Fz2$Fz2 <5) [1]
    
    for (r in 1:15){
      t = r+1
      Contact[r] <- Flight[r] - 1 +5 + which(Fz2$Fz2[Flight[r]+5:length(Fz2$Fz2)] >10)
      Flight[t] <- Contact[r] - 1 +5 + which(Fz2$Fz2[Contact[r]+5:length(Fz2$Fz2)] <5)
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
        geom_vline(xintercept=Contact[15], col="red")
    })
    
    
    # dlg_message(c("Is the hop devision correct?", "Continue?"), "okcancel")$res
    
    FullHop1 <- Fz2$Fz2[Contact[1]:Contact[2]]
    FullHop2 <- Fz2$Fz2[Contact[2]:Contact[3]]
    FullHop3 <- Fz2$Fz2[Contact[3]:Contact[4]]
    FullHop4 <- Fz2$Fz2[Contact[4]:Contact[5]]
    FullHop5 <- Fz2$Fz2[Contact[5]:Contact[6]]
    FullHop6 <- Fz2$Fz2[Contact[6]:Contact[7]]
    FullHop7 <- Fz2$Fz2[Contact[7]:Contact[8]]
    FullHop8 <- Fz2$Fz2[Contact[8]:Contact[9]]
    FullHop9 <- Fz2$Fz2[Contact[9]:Contact[10]]
    FullHop10 <- Fz2$Fz2[Contact[10]:Contact[11]]
    FullHop11 <- Fz2$Fz2[Contact[11]:Contact[12]]
    FullHop12 <- Fz2$Fz2[Contact[12]:Contact[13]]
    FullHop13 <- Fz2$Fz2[Contact[13]:Contact[14]]
    FullHop14 <- Fz2$Fz2[Contact[14]:Contact[15]]
    #    FullHop15 <- Fz2$Fz2[Contact[15]:Contact[16]]
    
    Hop1 <- Fz2$Fz2[Flight[1]:Flight[2]]
    Hop2 <- Fz2$Fz2[Flight[2]:Flight[3]]
    Hop3 <- Fz2$Fz2[Flight[3]:Flight[4]]
    Hop4 <- Fz2$Fz2[Flight[4]:Flight[5]]
    Hop5 <- Fz2$Fz2[Flight[5]:Flight[6]]
    Hop6 <- Fz2$Fz2[Flight[6]:Flight[7]]
    Hop7 <- Fz2$Fz2[Flight[7]:Flight[8]]
    Hop8 <- Fz2$Fz2[Flight[8]:Flight[9]]
    Hop9 <- Fz2$Fz2[Flight[9]:Flight[10]]
    Hop10 <- Fz2$Fz2[Flight[10]:Flight[11]]
    Hop11 <- Fz2$Fz2[Flight[11]:Flight[12]]
    Hop12 <- Fz2$Fz2[Flight[12]:Flight[13]]
    Hop13 <- Fz2$Fz2[Flight[13]:Flight[14]]
    Hop14 <- Fz2$Fz2[Flight[14]:Flight[15]]
    Hop15 <- Fz2$Fz2[Flight[15]:Flight[16]]
    #    Hop16 <- Fz2$Fz2[Flight[16]:Flight[17]]
    
    # windows()
    # plot(FullHop1)
    # windows()
    # plot(FullHop2)
    # windows()
    # plot(FullHop3)
    # windows()
    # plot(FullHop4)
    # windows()
    # plot(FullHop5)
    # windows()
    # plot(FullHop6)
    # windows()
    # plot(FullHop7)
    # windows()
    # plot(FullHop8)
    # windows()
    # plot(FullHop9)
    # windows()
    # plot(FullHop10)
    # windows()
    # plot(FullHop11)
    # windows()
    # plot(FullHop12)
    # windows()
    # plot(FullHop13)
    # windows()
    # plot(FullHop14)
    # windows()
    # plot(FullHop15)
    
    #Hop1
    
    #Calculate Acceleration
    
    Hop1Acc = (Hop1-BW)/input$Mass
    
    Hop1Accavg = rollapply(Hop1Acc, 2, mean)
    Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
    
    #Acceleration Area
    Hop1Accarea = Hop1Accavg2*0.001
    Hop1VelInit = 1.3
    
    #Velocity
    Hop1Vela = Hop1Accarea
    Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
    Hop1Vel = cumsum(Hop1Vela)
    
    #Velocity Area
    Hop1Velavg = rollapply(Hop1Vel, 2, mean)
    Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
    Hop1Velarea = Hop1Velavg2*0.001
    
    #Displacement
    Hop1Disp = cumsum(Hop1Velarea)
    
    l = length(Hop1Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop1Disp[l]>0){
      while (Hop1Disp[l] > 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit - 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)}
    }else (Hop1Disp[l]<0) 
    {
      while (Hop1Disp[l] < 0.0001) {
        Hop1Accavg = rollapply(Hop1Acc, 2, mean)
        Hop1Accavg2 = insert(Hop1Accavg, 1, 0)
        Hop1Accarea = Hop1Accavg2*0.001
        Hop1VelInit = Hop1VelInit + 0.001
        Hop1Vela = Hop1Accarea
        Hop1Vela[1] = Hop1VelInit+Hop1Accarea[1]
        Hop1Vel = cumsum(Hop1Vela)
        Hop1Velavg = rollapply(Hop1Vel, 2, mean)
        Hop1Velavg2 = insert(Hop1Velavg, 1, 0)
        Hop1Velarea = Hop1Velavg2*0.001
        Hop1Disp = cumsum(Hop1Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop1 = max(Hop1)
    PeakHop1PC = PeakHop1/BW
    
    
    ContactHop1 <- 5 + which(Hop1[5:length(Hop1)] >5)[1]
    ContactTimeHop1 = (length(Hop1Disp) - ContactHop1)*0.001
    MinHop1Disp = min(Hop1Disp)
    EccHop1Disp = (Hop1Disp[ContactHop1] - MinHop1Disp)
    EccStiffHop1 = ((PeakHop1 - BW)/EccHop1Disp)/input$Mass
    
    JumpFreqHop1 = 1/(length(FullHop1)*0.001)
    
    #####################
    
    #Hop2
    
    #Calculate Acceleration
    
    Hop2Acc = (Hop2-BW)/input$Mass
    
    Hop2Accavg = rollapply(Hop2Acc, 2, mean)
    Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
    
    #Acceleration Area
    Hop2Accarea = Hop2Accavg2*0.001
    Hop2VelInit = 1.3
    
    #Velocity
    Hop2Vela = Hop2Accarea
    Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
    Hop2Vel = cumsum(Hop2Vela)
    
    #Velocity Area
    Hop2Velavg = rollapply(Hop2Vel, 2, mean)
    Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
    Hop2Velarea = Hop2Velavg2*0.001
    
    #Displacement
    Hop2Disp = cumsum(Hop2Velarea)
    
    l = length(Hop2Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop2Disp[l]>0){
      while (Hop2Disp[l] > 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit - 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)}
    }else (Hop2Disp[l]<0) 
    {
      while (Hop2Disp[l] < 0.0001) {
        Hop2Accavg = rollapply(Hop2Acc, 2, mean)
        Hop2Accavg2 = insert(Hop2Accavg, 1, 0)
        Hop2Accarea = Hop2Accavg2*0.001
        Hop2VelInit = Hop2VelInit + 0.001
        Hop2Vela = Hop2Accarea
        Hop2Vela[1] = Hop2VelInit+Hop2Accarea[1]
        Hop2Vel = cumsum(Hop2Vela)
        Hop2Velavg = rollapply(Hop2Vel, 2, mean)
        Hop2Velavg2 = insert(Hop2Velavg, 1, 0)
        Hop2Velarea = Hop2Velavg2*0.001
        Hop2Disp = cumsum(Hop2Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop2 = max(Hop2)
    PeakHop2PC = PeakHop2/BW
    
    
    ContactHop2 <- 5 + which(Hop2[5:length(Hop2)] >5)[1]
    ContactTimeHop2 = (length(Hop2Disp) - ContactHop2)*0.001
    MinHop2Disp = min(Hop2Disp)
    EccHop2Disp = (Hop2Disp[ContactHop2] - MinHop2Disp)
    EccStiffHop2 = ((PeakHop2 - BW)/EccHop2Disp)/input$Mass
    
    JumpFreqHop2 = 1/(length(FullHop2)*0.001)
    
    #########################
    
    #Hop3
    
    #Calculate Acceleration
    
    Hop3Acc = (Hop3-BW)/input$Mass
    
    Hop3Accavg = rollapply(Hop3Acc, 2, mean)
    Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
    
    #Acceleration Area
    Hop3Accarea = Hop3Accavg2*0.001
    Hop3VelInit = 1.3
    
    #Velocity
    Hop3Vela = Hop3Accarea
    Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
    Hop3Vel = cumsum(Hop3Vela)
    
    #Velocity Area
    Hop3Velavg = rollapply(Hop3Vel, 2, mean)
    Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
    Hop3Velarea = Hop3Velavg2*0.001
    
    #Displacement
    Hop3Disp = cumsum(Hop3Velarea)
    
    l = length(Hop3Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop3Disp[l]>0){
      while (Hop3Disp[l] > 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit - 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)}
    }else (Hop3Disp[l]<0) 
    {
      while (Hop3Disp[l] < 0.0001) {
        Hop3Accavg = rollapply(Hop3Acc, 2, mean)
        Hop3Accavg2 = insert(Hop3Accavg, 1, 0)
        Hop3Accarea = Hop3Accavg2*0.001
        Hop3VelInit = Hop3VelInit + 0.001
        Hop3Vela = Hop3Accarea
        Hop3Vela[1] = Hop3VelInit+Hop3Accarea[1]
        Hop3Vel = cumsum(Hop3Vela)
        Hop3Velavg = rollapply(Hop3Vel, 2, mean)
        Hop3Velavg2 = insert(Hop3Velavg, 1, 0)
        Hop3Velarea = Hop3Velavg2*0.001
        Hop3Disp = cumsum(Hop3Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop3 = max(Hop3)
    PeakHop3PC = PeakHop3/BW
    
    
    ContactHop3 <- 5 + which(Hop3[5:length(Hop3)] >5)[1]
    ContactTimeHop3 = (length(Hop3Disp) - ContactHop3)*0.001
    MinHop3Disp = min(Hop3Disp)
    EccHop3Disp = (Hop3Disp[ContactHop3] - MinHop3Disp)
    EccStiffHop3 = ((PeakHop3 - BW)/EccHop3Disp)/input$Mass
    
    JumpFreqHop3 = 1/(length(FullHop3)*0.001)
    
    #########################
    
    #Hop4
    
    #Calculate Acceleration
    
    Hop4Acc = (Hop4-BW)/input$Mass
    
    Hop4Accavg = rollapply(Hop4Acc, 2, mean)
    Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
    
    #Acceleration Area
    Hop4Accarea = Hop4Accavg2*0.001
    Hop4VelInit = 1.3
    
    #Velocity
    Hop4Vela = Hop4Accarea
    Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
    Hop4Vel = cumsum(Hop4Vela)
    
    #Velocity Area
    Hop4Velavg = rollapply(Hop4Vel, 2, mean)
    Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
    Hop4Velarea = Hop4Velavg2*0.001
    
    #Displacement
    Hop4Disp = cumsum(Hop4Velarea)
    
    l = length(Hop4Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop4Disp[l]>0){
      while (Hop4Disp[l] > 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit - 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)}
    }else (Hop4Disp[l]<0) 
    {
      while (Hop4Disp[l] < 0.0001) {
        Hop4Accavg = rollapply(Hop4Acc, 2, mean)
        Hop4Accavg2 = insert(Hop4Accavg, 1, 0)
        Hop4Accarea = Hop4Accavg2*0.001
        Hop4VelInit = Hop4VelInit + 0.001
        Hop4Vela = Hop4Accarea
        Hop4Vela[1] = Hop4VelInit+Hop4Accarea[1]
        Hop4Vel = cumsum(Hop4Vela)
        Hop4Velavg = rollapply(Hop4Vel, 2, mean)
        Hop4Velavg2 = insert(Hop4Velavg, 1, 0)
        Hop4Velarea = Hop4Velavg2*0.001
        Hop4Disp = cumsum(Hop4Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop4 = max(Hop4)
    PeakHop4PC = PeakHop4/BW
    
    
    ContactHop4 <- 5 + which(Hop4[5:length(Hop4)] >5)[1]
    ContactTimeHop4 = (length(Hop4Disp) - ContactHop4)*0.001
    MinHop4Disp = min(Hop4Disp)
    EccHop4Disp = (Hop4Disp[ContactHop4] - MinHop4Disp)
    EccStiffHop4 = ((PeakHop4 - BW)/EccHop4Disp)/input$Mass
    
    JumpFreqHop4 = 1/(length(FullHop4)*0.001)
    
    #########################
    
    #Hop5
    
    #Calculate Acceleration
    
    Hop5Acc = (Hop5-BW)/input$Mass
    
    Hop5Accavg = rollapply(Hop5Acc, 2, mean)
    Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
    
    #Acceleration Area
    Hop5Accarea = Hop5Accavg2*0.001
    Hop5VelInit = 1.3
    
    #Velocity
    Hop5Vela = Hop5Accarea
    Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
    Hop5Vel = cumsum(Hop5Vela)
    
    #Velocity Area
    Hop5Velavg = rollapply(Hop5Vel, 2, mean)
    Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
    Hop5Velarea = Hop5Velavg2*0.001
    
    #Displacement
    Hop5Disp = cumsum(Hop5Velarea)
    
    l = length(Hop5Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop5Disp[l]>0){
      while (Hop5Disp[l] > 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit - 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)}
    }else (Hop5Disp[l]<0) 
    {
      while (Hop5Disp[l] < 0.0001) {
        Hop5Accavg = rollapply(Hop5Acc, 2, mean)
        Hop5Accavg2 = insert(Hop5Accavg, 1, 0)
        Hop5Accarea = Hop5Accavg2*0.001
        Hop5VelInit = Hop5VelInit + 0.001
        Hop5Vela = Hop5Accarea
        Hop5Vela[1] = Hop5VelInit+Hop5Accarea[1]
        Hop5Vel = cumsum(Hop5Vela)
        Hop5Velavg = rollapply(Hop5Vel, 2, mean)
        Hop5Velavg2 = insert(Hop5Velavg, 1, 0)
        Hop5Velarea = Hop5Velavg2*0.001
        Hop5Disp = cumsum(Hop5Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop5 = max(Hop5)
    PeakHop5PC = PeakHop5/BW
    
    
    ContactHop5 <- 5 + which(Hop5[5:length(Hop5)] >5)[1]
    ContactTimeHop5 = (length(Hop5Disp) - ContactHop5)*0.001
    MinHop5Disp = min(Hop5Disp)
    EccHop5Disp = (Hop5Disp[ContactHop5] - MinHop5Disp)
    EccStiffHop5 = ((PeakHop5 - BW)/EccHop5Disp)/input$Mass
    
    JumpFreqHop5 = 1/(length(FullHop5)*0.001)
    
    #########################
    
    
    #Hop6
    
    #Calculate Acceleration
    
    Hop6Acc = (Hop6-BW)/input$Mass
    
    Hop6Accavg = rollapply(Hop6Acc, 2, mean)
    Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
    
    #Acceleration Area
    Hop6Accarea = Hop6Accavg2*0.001
    Hop6VelInit = 1.3
    
    #Velocity
    Hop6Vela = Hop6Accarea
    Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
    Hop6Vel = cumsum(Hop6Vela)
    
    #Velocity Area
    Hop6Velavg = rollapply(Hop6Vel, 2, mean)
    Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
    Hop6Velarea = Hop6Velavg2*0.001
    
    #Displacement
    Hop6Disp = cumsum(Hop6Velarea)
    
    l = length(Hop6Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop6Disp[l]>0){
      while (Hop6Disp[l] > 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit - 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)}
    }else (Hop6Disp[l]<0) 
    {
      while (Hop6Disp[l] < 0.0001) {
        Hop6Accavg = rollapply(Hop6Acc, 2, mean)
        Hop6Accavg2 = insert(Hop6Accavg, 1, 0)
        Hop6Accarea = Hop6Accavg2*0.001
        Hop6VelInit = Hop6VelInit + 0.001
        Hop6Vela = Hop6Accarea
        Hop6Vela[1] = Hop6VelInit+Hop6Accarea[1]
        Hop6Vel = cumsum(Hop6Vela)
        Hop6Velavg = rollapply(Hop6Vel, 2, mean)
        Hop6Velavg2 = insert(Hop6Velavg, 1, 0)
        Hop6Velarea = Hop6Velavg2*0.001
        Hop6Disp = cumsum(Hop6Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop6 = max(Hop6)
    PeakHop6PC = PeakHop6/BW
    
    
    ContactHop6 <- 5 + which(Hop6[5:length(Hop6)] >5)[1]
    ContactTimeHop6 = (length(Hop6Disp) - ContactHop6)*0.001
    MinHop6Disp = min(Hop6Disp)
    EccHop6Disp = (Hop6Disp[ContactHop6] - MinHop6Disp)
    EccStiffHop6 = ((PeakHop6 - BW)/EccHop6Disp)/input$Mass
    
    JumpFreqHop6 = 1/(length(FullHop6)*0.001)
    
    #########################
    
    
    #Hop7
    
    #Calculate Acceleration
    
    Hop7Acc = (Hop7-BW)/input$Mass
    
    Hop7Accavg = rollapply(Hop7Acc, 2, mean)
    Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
    
    #Acceleration Area
    Hop7Accarea = Hop7Accavg2*0.001
    Hop7VelInit = 1.3
    
    #Velocity
    Hop7Vela = Hop7Accarea
    Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
    Hop7Vel = cumsum(Hop7Vela)
    
    #Velocity Area
    Hop7Velavg = rollapply(Hop7Vel, 2, mean)
    Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
    Hop7Velarea = Hop7Velavg2*0.001
    
    #Displacement
    Hop7Disp = cumsum(Hop7Velarea)
    
    l = length(Hop7Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop7Disp[l]>0){
      while (Hop7Disp[l] > 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit - 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)}
    }else (Hop7Disp[l]<0) 
    {
      while (Hop7Disp[l] < 0.0001) {
        Hop7Accavg = rollapply(Hop7Acc, 2, mean)
        Hop7Accavg2 = insert(Hop7Accavg, 1, 0)
        Hop7Accarea = Hop7Accavg2*0.001
        Hop7VelInit = Hop7VelInit + 0.001
        Hop7Vela = Hop7Accarea
        Hop7Vela[1] = Hop7VelInit+Hop7Accarea[1]
        Hop7Vel = cumsum(Hop7Vela)
        Hop7Velavg = rollapply(Hop7Vel, 2, mean)
        Hop7Velavg2 = insert(Hop7Velavg, 1, 0)
        Hop7Velarea = Hop7Velavg2*0.001
        Hop7Disp = cumsum(Hop7Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop7 = max(Hop7)
    PeakHop7PC = PeakHop7/BW
    
    
    ContactHop7 <- 5 + which(Hop7[5:length(Hop7)] >5)[1]
    ContactTimeHop7 = (length(Hop7Disp) - ContactHop7)*0.001
    MinHop7Disp = min(Hop7Disp)
    EccHop7Disp = (Hop7Disp[ContactHop7] - MinHop7Disp)
    EccStiffHop7 = ((PeakHop7 - BW)/EccHop7Disp)/input$Mass
    
    JumpFreqHop7 = 1/(length(FullHop7)*0.001)
    
    #########################
    
    #Hop8
    
    #Calculate Acceleration
    
    Hop8Acc = (Hop8-BW)/input$Mass
    
    Hop8Accavg = rollapply(Hop8Acc, 2, mean)
    Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
    
    #Acceleration Area
    Hop8Accarea = Hop8Accavg2*0.001
    Hop8VelInit = 1.3
    
    #Velocity
    Hop8Vela = Hop8Accarea
    Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
    Hop8Vel = cumsum(Hop8Vela)
    
    #Velocity Area
    Hop8Velavg = rollapply(Hop8Vel, 2, mean)
    Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
    Hop8Velarea = Hop8Velavg2*0.001
    
    #Displacement
    Hop8Disp = cumsum(Hop8Velarea)
    
    l = length(Hop8Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop8Disp[l]>0){
      while (Hop8Disp[l] > 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit - 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)}
    }else (Hop8Disp[l]<0) 
    {
      while (Hop8Disp[l] < 0.0001) {
        Hop8Accavg = rollapply(Hop8Acc, 2, mean)
        Hop8Accavg2 = insert(Hop8Accavg, 1, 0)
        Hop8Accarea = Hop8Accavg2*0.001
        Hop8VelInit = Hop8VelInit + 0.001
        Hop8Vela = Hop8Accarea
        Hop8Vela[1] = Hop8VelInit+Hop8Accarea[1]
        Hop8Vel = cumsum(Hop8Vela)
        Hop8Velavg = rollapply(Hop8Vel, 2, mean)
        Hop8Velavg2 = insert(Hop8Velavg, 1, 0)
        Hop8Velarea = Hop8Velavg2*0.001
        Hop8Disp = cumsum(Hop8Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop8 = max(Hop8)
    PeakHop8PC = PeakHop8/BW
    
    
    ContactHop8 <- 5 + which(Hop8[5:length(Hop8)] >5)[1]
    ContactTimeHop8 = (length(Hop8Disp) - ContactHop8)*0.001
    MinHop8Disp = min(Hop8Disp)
    EccHop8Disp = (Hop8Disp[ContactHop8] - MinHop8Disp)
    EccStiffHop8 = ((PeakHop8 - BW)/EccHop8Disp)/input$Mass
    
    JumpFreqHop8 = 1/(length(FullHop8)*0.001)
    
    #########################
    
    #Hop9
    
    #Calculate Acceleration
    
    Hop9Acc = (Hop9-BW)/input$Mass
    
    Hop9Accavg = rollapply(Hop9Acc, 2, mean)
    Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
    
    #Acceleration Area
    Hop9Accarea = Hop9Accavg2*0.001
    Hop9VelInit = 1.3
    
    #Velocity
    Hop9Vela = Hop9Accarea
    Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
    Hop9Vel = cumsum(Hop9Vela)
    
    #Velocity Area
    Hop9Velavg = rollapply(Hop9Vel, 2, mean)
    Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
    Hop9Velarea = Hop9Velavg2*0.001
    
    #Displacement
    Hop9Disp = cumsum(Hop9Velarea)
    
    l = length(Hop9Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop9Disp[l]>0){
      while (Hop9Disp[l] > 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit - 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)}
    }else (Hop9Disp[l]<0) 
    {
      while (Hop9Disp[l] < 0.0001) {
        Hop9Accavg = rollapply(Hop9Acc, 2, mean)
        Hop9Accavg2 = insert(Hop9Accavg, 1, 0)
        Hop9Accarea = Hop9Accavg2*0.001
        Hop9VelInit = Hop9VelInit + 0.001
        Hop9Vela = Hop9Accarea
        Hop9Vela[1] = Hop9VelInit+Hop9Accarea[1]
        Hop9Vel = cumsum(Hop9Vela)
        Hop9Velavg = rollapply(Hop9Vel, 2, mean)
        Hop9Velavg2 = insert(Hop9Velavg, 1, 0)
        Hop9Velarea = Hop9Velavg2*0.001
        Hop9Disp = cumsum(Hop9Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop9 = max(Hop9)
    PeakHop9PC = PeakHop9/BW
    
    
    ContactHop9 <- 5 + which(Hop9[5:length(Hop9)] >5)[1]
    ContactTimeHop9 = (length(Hop9Disp) - ContactHop9)*0.001
    MinHop9Disp = min(Hop9Disp)
    EccHop9Disp = (Hop9Disp[ContactHop9] - MinHop9Disp)
    EccStiffHop9 = ((PeakHop9 - BW)/EccHop9Disp)/input$Mass
    
    JumpFreqHop9 = 1/(length(FullHop9)*0.001)
    
    #########################
    
    #Hop10
    
    #Calculate Acceleration
    
    Hop10Acc = (Hop10-BW)/input$Mass
    
    Hop10Accavg = rollapply(Hop10Acc, 2, mean)
    Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
    
    #Acceleration Area
    Hop10Accarea = Hop10Accavg2*0.001
    Hop10VelInit = 1.3
    
    #Velocity
    Hop10Vela = Hop10Accarea
    Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
    Hop10Vel = cumsum(Hop10Vela)
    
    #Velocity Area
    Hop10Velavg = rollapply(Hop10Vel, 2, mean)
    Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
    Hop10Velarea = Hop10Velavg2*0.001
    
    #Displacement
    Hop10Disp = cumsum(Hop10Velarea)
    
    l = length(Hop10Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop10Disp[l]>0){
      while (Hop10Disp[l] > 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit - 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)}
    }else (Hop10Disp[l]<0) 
    {
      while (Hop10Disp[l] < 0.0001) {
        Hop10Accavg = rollapply(Hop10Acc, 2, mean)
        Hop10Accavg2 = insert(Hop10Accavg, 1, 0)
        Hop10Accarea = Hop10Accavg2*0.001
        Hop10VelInit = Hop10VelInit + 0.001
        Hop10Vela = Hop10Accarea
        Hop10Vela[1] = Hop10VelInit+Hop10Accarea[1]
        Hop10Vel = cumsum(Hop10Vela)
        Hop10Velavg = rollapply(Hop10Vel, 2, mean)
        Hop10Velavg2 = insert(Hop10Velavg, 1, 0)
        Hop10Velarea = Hop10Velavg2*0.001
        Hop10Disp = cumsum(Hop10Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop10 = max(Hop10)
    PeakHop10PC = PeakHop10/BW
    
    
    ContactHop10 <- 5 + which(Hop10[5:length(Hop10)] >5)[1]
    ContactTimeHop10 = (length(Hop10Disp) - ContactHop10)*0.001
    MinHop10Disp = min(Hop10Disp)
    EccHop10Disp = (Hop10Disp[ContactHop10] - MinHop10Disp)
    EccStiffHop10 = ((PeakHop10 - BW)/EccHop10Disp)/input$Mass
    
    JumpFreqHop10 = 1/(length(FullHop10)*0.001)
    
    #########################
    
    #Hop11
    
    #Calculate Acceleration
    
    Hop11Acc = (Hop11-BW)/input$Mass
    
    Hop11Accavg = rollapply(Hop11Acc, 2, mean)
    Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
    
    #Acceleration Area
    Hop11Accarea = Hop11Accavg2*0.001
    Hop11VelInit = 1.3
    
    #Velocity
    Hop11Vela = Hop11Accarea
    Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
    Hop11Vel = cumsum(Hop11Vela)
    
    #Velocity Area
    Hop11Velavg = rollapply(Hop11Vel, 2, mean)
    Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
    Hop11Velarea = Hop11Velavg2*0.001
    
    #Displacement
    Hop11Disp = cumsum(Hop11Velarea)
    
    l = length(Hop11Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop11Disp[l]>0){
      while (Hop11Disp[l] > 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit - 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)}
    }else (Hop11Disp[l]<0) 
    {
      while (Hop11Disp[l] < 0.0001) {
        Hop11Accavg = rollapply(Hop11Acc, 2, mean)
        Hop11Accavg2 = insert(Hop11Accavg, 1, 0)
        Hop11Accarea = Hop11Accavg2*0.001
        Hop11VelInit = Hop11VelInit + 0.001
        Hop11Vela = Hop11Accarea
        Hop11Vela[1] = Hop11VelInit+Hop11Accarea[1]
        Hop11Vel = cumsum(Hop11Vela)
        Hop11Velavg = rollapply(Hop11Vel, 2, mean)
        Hop11Velavg2 = insert(Hop11Velavg, 1, 0)
        Hop11Velarea = Hop11Velavg2*0.001
        Hop11Disp = cumsum(Hop11Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop11 = max(Hop11)
    PeakHop11PC = PeakHop11/BW
    
    
    ContactHop11 <- 5 + which(Hop11[5:length(Hop11)] >5)[1]
    ContactTimeHop11 = (length(Hop11Disp) - ContactHop11)*0.001
    MinHop11Disp = min(Hop11Disp)
    EccHop11Disp = (Hop11Disp[ContactHop11] - MinHop11Disp)
    EccStiffHop11 = ((PeakHop11 - BW)/EccHop11Disp)/input$Mass
    
    JumpFreqHop11 = 1/(length(FullHop11)*0.001)
    
    #########################
    
    #Hop12
    
    #Calculate Acceleration
    
    Hop12Acc = (Hop12-BW)/input$Mass
    
    Hop12Accavg = rollapply(Hop12Acc, 2, mean)
    Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
    
    #Acceleration Area
    Hop12Accarea = Hop12Accavg2*0.001
    Hop12VelInit = 1.3
    
    #Velocity
    Hop12Vela = Hop12Accarea
    Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
    Hop12Vel = cumsum(Hop12Vela)
    
    #Velocity Area
    Hop12Velavg = rollapply(Hop12Vel, 2, mean)
    Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
    Hop12Velarea = Hop12Velavg2*0.001
    
    #Displacement
    Hop12Disp = cumsum(Hop12Velarea)
    
    l = length(Hop12Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop12Disp[l]>0){
      while (Hop12Disp[l] > 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit - 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)}
    }else (Hop12Disp[l]<0) 
    {
      while (Hop12Disp[l] < 0.0001) {
        Hop12Accavg = rollapply(Hop12Acc, 2, mean)
        Hop12Accavg2 = insert(Hop12Accavg, 1, 0)
        Hop12Accarea = Hop12Accavg2*0.001
        Hop12VelInit = Hop12VelInit + 0.001
        Hop12Vela = Hop12Accarea
        Hop12Vela[1] = Hop12VelInit+Hop12Accarea[1]
        Hop12Vel = cumsum(Hop12Vela)
        Hop12Velavg = rollapply(Hop12Vel, 2, mean)
        Hop12Velavg2 = insert(Hop12Velavg, 1, 0)
        Hop12Velarea = Hop12Velavg2*0.001
        Hop12Disp = cumsum(Hop12Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop12 = max(Hop12)
    PeakHop12PC = PeakHop12/BW
    
    
    ContactHop12 <- 5 + which(Hop12[5:length(Hop12)] >5)[1]
    ContactTimeHop12 = (length(Hop12Disp) - ContactHop12)*0.001
    MinHop12Disp = min(Hop12Disp)
    EccHop12Disp = (Hop12Disp[ContactHop12] - MinHop12Disp)
    EccStiffHop12 = ((PeakHop12 - BW)/EccHop12Disp)/input$Mass
    
    JumpFreqHop12 = 1/(length(FullHop12)*0.001)
    
    #########################
    
    #Hop13
    
    #Calculate Acceleration
    
    Hop13Acc = (Hop13-BW)/input$Mass
    
    Hop13Accavg = rollapply(Hop13Acc, 2, mean)
    Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
    
    #Acceleration Area
    Hop13Accarea = Hop13Accavg2*0.001
    Hop13VelInit = 1.3
    
    #Velocity
    Hop13Vela = Hop13Accarea
    Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
    Hop13Vel = cumsum(Hop13Vela)
    
    #Velocity Area
    Hop13Velavg = rollapply(Hop13Vel, 2, mean)
    Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
    Hop13Velarea = Hop13Velavg2*0.001
    
    #Displacement
    Hop13Disp = cumsum(Hop13Velarea)
    
    l = length(Hop13Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop13Disp[l]>0){
      while (Hop13Disp[l] > 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit - 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)}
    }else (Hop13Disp[l]<0) 
    {
      while (Hop13Disp[l] < 0.0001) {
        Hop13Accavg = rollapply(Hop13Acc, 2, mean)
        Hop13Accavg2 = insert(Hop13Accavg, 1, 0)
        Hop13Accarea = Hop13Accavg2*0.001
        Hop13VelInit = Hop13VelInit + 0.001
        Hop13Vela = Hop13Accarea
        Hop13Vela[1] = Hop13VelInit+Hop13Accarea[1]
        Hop13Vel = cumsum(Hop13Vela)
        Hop13Velavg = rollapply(Hop13Vel, 2, mean)
        Hop13Velavg2 = insert(Hop13Velavg, 1, 0)
        Hop13Velarea = Hop13Velavg2*0.001
        Hop13Disp = cumsum(Hop13Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop13 = max(Hop13)
    PeakHop13PC = PeakHop13/BW
    
    
    ContactHop13 <- 5 + which(Hop13[5:length(Hop13)] >5)[1]
    ContactTimeHop13 = (length(Hop13Disp) - ContactHop13)*0.001
    MinHop13Disp = min(Hop13Disp)
    EccHop13Disp = (Hop13Disp[ContactHop13] - MinHop13Disp)
    EccStiffHop13 = ((PeakHop13 - BW)/EccHop13Disp)/input$Mass
    
    JumpFreqHop13 = 1/(length(FullHop13)*0.001)
    
    #########################
    
    #Hop14
    
    #Calculate Acceleration
    
    Hop14Acc = (Hop14-BW)/input$Mass
    
    Hop14Accavg = rollapply(Hop14Acc, 2, mean)
    Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
    
    #Acceleration Area
    Hop14Accarea = Hop14Accavg2*0.001
    Hop14VelInit = 1.3
    
    #Velocity
    Hop14Vela = Hop14Accarea
    Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
    Hop14Vel = cumsum(Hop14Vela)
    
    #Velocity Area
    Hop14Velavg = rollapply(Hop14Vel, 2, mean)
    Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
    Hop14Velarea = Hop14Velavg2*0.001
    
    #Displacement
    Hop14Disp = cumsum(Hop14Velarea)
    
    l = length(Hop14Disp)
    
    #Adjust Init Velocity to get Final Displacment close to zero
    if (Hop14Disp[l]>0){
      while (Hop14Disp[l] > 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit - 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)}
    }else (Hop14Disp[l]<0) 
    {
      while (Hop14Disp[l] < 0.0001) {
        Hop14Accavg = rollapply(Hop14Acc, 2, mean)
        Hop14Accavg2 = insert(Hop14Accavg, 1, 0)
        Hop14Accarea = Hop14Accavg2*0.001
        Hop14VelInit = Hop14VelInit + 0.001
        Hop14Vela = Hop14Accarea
        Hop14Vela[1] = Hop14VelInit+Hop14Accarea[1]
        Hop14Vel = cumsum(Hop14Vela)
        Hop14Velavg = rollapply(Hop14Vel, 2, mean)
        Hop14Velavg2 = insert(Hop14Velavg, 1, 0)
        Hop14Velarea = Hop14Velavg2*0.001
        Hop14Disp = cumsum(Hop14Velarea)
      }
    }
    
    
    #Calculate output variables
    PeakHop14 = max(Hop14)
    PeakHop14PC = PeakHop14/BW
    
    
    ContactHop14 <- 5 + which(Hop14[5:length(Hop14)] >5)[1]
    ContactTimeHop14 = (length(Hop14Disp) - ContactHop14)*0.001
    MinHop14Disp = min(Hop14Disp)
    EccHop14Disp = (Hop14Disp[ContactHop14] - MinHop14Disp)
    EccStiffHop14 = ((PeakHop14 - BW)/EccHop14Disp)/input$Mass
    
    JumpFreqHop14 = 1/(length(FullHop14)*0.001)
    
    #########################
    
    # #Hop15
    # 
    # #Calculate Acceleration
    # 
    # Hop15Acc = (Hop15-BW)/input$Mass
    # 
    # Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    # Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    # 
    # #Acceleration Area
    # Hop15Accarea = Hop15Accavg2*0.001
    # Hop15VelInit = 1.3
    # 
    # #Velocity
    # Hop15Vela = Hop15Accarea
    # Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    # Hop15Vel = cumsum(Hop15Vela)
    # 
    # #Velocity Area
    # Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    # Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    # Hop15Velarea = Hop15Velavg2*0.001
    # 
    # #Displacement
    # Hop15Disp = cumsum(Hop15Velarea)
    # 
    # l = length(Hop15Disp)
    # 
    # #Adjust Init Velocity to get Final Displacment close to zero
    # if (Hop15Disp[l]>0){
    #   while (Hop15Disp[l] > 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit - 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)}
    # }else (Hop15Disp[l]<0) 
    # {
    #   while (Hop15Disp[l] < 0.0001) {
    #     Hop15Accavg = rollapply(Hop15Acc, 2, mean)
    #     Hop15Accavg2 = insert(Hop15Accavg, 1, 0)
    #     Hop15Accarea = Hop15Accavg2*0.001
    #     Hop15VelInit = Hop15VelInit + 0.001
    #     Hop15Vela = Hop15Accarea
    #     Hop15Vela[1] = Hop15VelInit+Hop15Accarea[1]
    #     Hop15Vel = cumsum(Hop15Vela)
    #     Hop15Velavg = rollapply(Hop15Vel, 2, mean)
    #     Hop15Velavg2 = insert(Hop15Velavg, 1, 0)
    #     Hop15Velarea = Hop15Velavg2*0.001
    #     Hop15Disp = cumsum(Hop15Velarea)
    #   }
    # }
    # 
    # 
    # #Calculate output variables
    # PeakHop15 = max(Hop15)
    # PeakHop15PC = PeakHop15/BW
    # 
    # 
    # ContactHop15 <- 5 + which(Hop15[5:length(Hop15)] >5)[1]
    # ContactTimeHop15 = (length(Hop15Disp) - ContactHop15)*0.001
    # MinHop15Disp = min(Hop15Disp)
    # EccHop15Disp = (Hop15Disp[ContactHop15] - MinHop15Disp)
    # EccStiffHop15 = ((PeakHop15 - BW)/EccHop15Disp)/input$Mass
    # 
    # JumpFreqHop15 = 1/(length(FullHop15)*0.001)
    # 
    # 
    
    
    #########################
    hopnumber <- c("Hop1","Hop2","Hop3","Hop4","Hop5","Hop6","Hop7","Hop8","Hop9","Hop10","Hop11","Hop12","Hop13","Hop14", "Average")
    EccCOMDisp <- rbind(round(EccHop1Disp,digits=2), round(EccHop2Disp,digits=2), round(EccHop3Disp,digits=2), round(EccHop4Disp,digits=2), round(EccHop5Disp,digits=2), round(EccHop6Disp,digits=2), round(EccHop7Disp,digits=2), round(EccHop8Disp,digits=2), round(EccHop9Disp,digits=2), round(EccHop10Disp,digits=2), round(EccHop11Disp,digits=2), round(EccHop12Disp,digits=2), round(EccHop13Disp,digits=2), round(EccHop14Disp,digits=2))
    ContactTime <- rbind(round(ContactTimeHop1,digits=2), round(ContactTimeHop2,digits=2), round(ContactTimeHop3,digits=2), round(ContactTimeHop4,digits=2), round(ContactTimeHop5,digits=2), round(ContactTimeHop6,digits=2), round(ContactTimeHop7,digits=2), round(ContactTimeHop8,digits=2), round(ContactTimeHop9,digits=2), round(ContactTimeHop10,digits=2), round(ContactTimeHop11,digits=2), round(ContactTimeHop12,digits=2), round(ContactTimeHop13,digits=2), round(ContactTimeHop14,digits=2))
    EccStiffness <- rbind(round(EccStiffHop1,digits=2), round(EccStiffHop2,digits=2), round(EccStiffHop3,digits=2), round(EccStiffHop4,digits=2), round(EccStiffHop5,digits=2), round(EccStiffHop6,digits=2), round(EccStiffHop7,digits=2), round(EccStiffHop8,digits=2), round(EccStiffHop9,digits=2), round(EccStiffHop10,digits=2), round(EccStiffHop11,digits=2), round(EccStiffHop12,digits=2), round(EccStiffHop13,digits=2), round(EccStiffHop14,digits=2))
    PeakForcePC <- rbind(round(PeakHop1PC,digits=2), round(PeakHop2PC,digits=2), round(PeakHop3PC,digits=2), round(PeakHop4PC,digits=2), round(PeakHop5PC,digits=2), round(PeakHop6PC,digits=2), round(PeakHop7PC,digits=2), round(PeakHop8PC,digits=2), round(PeakHop9PC,digits=2), round(PeakHop10PC,digits=2), round(PeakHop11PC,digits=2), round(PeakHop12PC,digits=2), round(PeakHop13PC,digits=2), round(PeakHop14PC,digits=2))
    JumpFreq <- rbind(round(JumpFreqHop1,digits=2), round(JumpFreqHop2,digits=2), round(JumpFreqHop3,digits=2), round(JumpFreqHop4,digits=2), round(JumpFreqHop5,digits=2), round(JumpFreqHop6,digits=2), round(JumpFreqHop7,digits=2), round(JumpFreqHop8,digits=2), round(JumpFreqHop9,digits=2), round(JumpFreqHop10,digits=2), round(JumpFreqHop11,digits=2), round(JumpFreqHop12,digits=2), round(JumpFreqHop13,digits=2), round(JumpFreqHop14,digits=2))
    
    avgEccCOMDisp = round(mean(EccCOMDisp),digits=2)
    avgContactTime = round(mean(ContactTime),digits=2)
    avgEccStiffness = round(mean(EccStiffness),digits=2)
    avgPeakForcePC = round(mean(PeakForcePC),digits=2)
    avgJumpFreq = round(mean(JumpFreq),digits=2)
    
    RightSSAverages <<-cbind(avgEccCOMDisp, avgContactTime, avgEccStiffness, avgPeakForcePC,avgJumpFreq)
    
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
    
  })
  output$resultstableSummary <- renderTable({
    OutputAverages <- rbind(Left1.7Averages, Left2.0Averages, Left2.3Averages, Left2.6Averages, LeftSSAverages, Right1.7Averages, Right2.0Averages, Right2.3Averages, Right2.6Averages, RightSSAverages)
    rownames(OutputAverages) <- c("Left 1.7Hz", "Left 2.0Hz", "Left 2.3Hz", "Left 2.6Hz", "Left SSHz", "Right 1.7Hz", "Right 2.0Hz", "Right 2.3Hz", "Right 2.6Hz", "Right SSHz")
    colnames(OutputAverages) <-  c("Trial", "EccCOMDisp","ContactTime", "EccStiffness", "PeakForcePC", "JumpFreq")
    OutputdfAverages <- as.data.frame(OutputAverages)
    ({OutputdfAverages})
    
  })
}  