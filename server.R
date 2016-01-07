library(cmprsk)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(tools)

shinyServer(function(input, output, session) {
  
  yourdata <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }else{
      read.csv(inFile$datapath,header=input$header, sep=input$sep, quote=input$quote)
    }
  })
  
  output$contents <- renderDataTable({
    prova<-input$file1
    if (is.null(prova)){
      return(NULL)
    }
    validate(need(file_ext(prova$name) %in% c('txt','csv'), "Wrong File Format, try again!"))
    try(yourdata(), silent=TRUE)
  }) 
  
  output$Drop.ftime <- renderUI({
    data <- yourdata()
    if (is.null(data)){
      return(NULL)
    }
    varnames <- c(Choose='',names(data))
    selectInput("dft", label = "This variable indicates the time at which the failure occurred. It must be numerical.", choices=as.list(varnames))
  })
  
  output$Drop.status<-renderUI({
    data <- yourdata()
    if (is.null(data)){
      return(NULL)
    }
    varnames <- c(Choose='',names(data))
    selectInput("dst", label = "This variable must be coded numerically with distinct codes (i.e. 1, 2, ...) for different causes of failure and code 0 for censored observations.", choices=as.list(varnames))
  })
  
  output$Drop.group <- renderUI({
    data<-yourdata()
    if (is.null(data)){
      return(NULL)
    }
    varnames <- c(Choose='',names(data))
    selectInput("dgr", label = " CIF estimates will be calculated within groups. The grouping variabile is categorical, it is preferable to indicate the groups with alphabetical labels.",choices=as.list(varnames))
  })
  
  source("CumIncidence.R") 
  
  mod1 <- reactive({
    data <- yourdata()
    nomiconf <- names(data)
    name1 <- nomiconf[nomiconf==input$dft]
    name2 <- nomiconf[nomiconf==input$dst]
    name3 <- nomiconf[nomiconf==input$dgr]
    varftime  <- data[, name1]
    varstatus <- data[, name2]
    vargroup  <- data[, name3]
    if (input$timepoints==""){
      with(data, CumIncidence(ftime=varftime, fstatus=varstatus, group=vargroup, digits = 3))
    }else{tpoints <- as.numeric(unlist(strsplit(input$timepoints, "[,]"))) 
          with(data, CumIncidence(ftime=varftime, fstatus=varstatus, group=vargroup, t=tpoints, digits = 3))
    }
  })
  
  output$cif <- renderPrint({
    if (input$goButton == 0){validate(need(input$goButton != 0, "Select variables and press the go button.")) 
    }else{ 
      fit2 <- isolate(try(invisible(mod1()), silent=TRUE))
      try(round(fit2$est,4), silent=TRUE)
    }
  })
  
  
  output$cifstder <- renderPrint({
    if (input$goButton == 0){validate(need(input$goButton != 0, "Select variables and press the go button.")) 
    }else{ 
      fit2 <- isolate(try(invisible(mod1()), silent=TRUE))
      try(round(fit2$se,4), silent=TRUE)
    }
  })
  
  output$ciftest <- renderPrint({
    if (input$goButton == 0){validate(need(input$goButton != 0, "Select variables and press the go button.")) 
    }else{ 
      fit2 <- isolate(try(invisible(mod1()), silent=TRUE))
      try(round(fit2$test,4), silent=TRUE)
    }
  })
  
  output$cifplot <- renderPlot({
    if (input$goButton == 0){validate(need(input$goButton != 0, "Select variables and press the go button.")) 
    }else{isolate(try({data <- yourdata()
                       nomiconf <- names(data)
                       name1 <- nomiconf[nomiconf==input$dft]
                       name2 <- nomiconf[nomiconf==input$dst]
                       name3 <- nomiconf[nomiconf==input$dgr]
                       varftime  <- data[, name1]
                       varstatus <- data[, name2]
                       vargroup  <- data[, name3]
                       
                       fit2 <- isolate(invisible(mod1()))
                       g <- max(1, length(unique(vargroup)))
                       s <- length(unique(varstatus))
                       estsalvo1 <- as.data.frame(t(round(fit2$est,4)))
                       estsalvo1$id <- as.numeric(dimnames(fit2$est)[[2]])
                       
                       plot_estsalvo1<- melt(estsalvo1,id.var="id")
                       plot_estsalvo1$lty <-  rep(rep(levels(factor(vargroup)), times = rep(length(estsalvo1$id), times=length(levels(factor(vargroup))))), times=(s-1))
                       plot_estsalvo1$col <-  rep(levels(factor(varstatus))[-1], times =rep(length(estsalvo1$id)*g, times=(s-1)))
                       
                       if(input$plotCIFbw == 1){ggplot(plot_estsalvo1, aes(x=id, y=value, vgroup=variable, colour=col, alpha=0.85))+ geom_step(aes(lty=lty), size=1.25)+  ylab("Probability") + xlab("Time")  + scale_colour_discrete(name=name2) + scale_linetype_discrete(name=name3)+ scale_alpha_continuous(guide=FALSE)
                       } else {ggplot(plot_estsalvo1, aes(x=id, y=value, vgroup=variable, colour=col, alpha=0.85))+ geom_step(aes(lty=lty), size=1.25)+  ylab("Probability") + xlab("Time") + scale_linetype_discrete(name=name3)+ scale_alpha_continuous(guide=FALSE) +scale_colour_grey(name=name2, start=0.1, end=0.6) + theme_bw() + scale_fill_grey(start = 0.2, end = 0.8,na.value = "red")
                       }}, silent=TRUE))
    } 
  })
  
  mod2 <- reactive({
    data <- yourdata()
    nomiconf <- names(data)
    name1 <- nomiconf[nomiconf==input$dft]
    name2 <- nomiconf[nomiconf==input$dst]
    name3 <- nomiconf[nomiconf==input$dgr]
    varftime    <-  data[, name1]
    varstatus   <- data[, name2]
    vargroup   <-  data[, name3]
    if (input$timepoints==""){
      with(data, CumIncidence(ftime=varftime, fstatus=varstatus, group=vargroup, level=as.numeric(input$conflev)))
    }else{tpoints <- as.numeric(unlist(strsplit(input$timepoints, "[,]"))) 
          with(data, CumIncidence(ftime=varftime, fstatus=varstatus, group=vargroup, t=tpoints, level=as.numeric(input$conflev)))
    }
  })
  
  output$confint <- renderPrint({
    if (input$goButton2 == 0){validate(need(input$goButton2 != 0, "Specify the confidence level and press the go button."))
    }else{
      fit3 <- isolate(try(mod2(), silent=TRUE))
      try(round(fit3$ci[,,],4), silent=TRUE)
    }
  })
  
  
  output$confintplot <- renderPlot({
    if (input$goButton2 == 0){validate(need(input$goButton2 != 0, "Specify the confidence level and press the go button."))
    }else{isolate(try({ data <- yourdata()
                        nomiconf <- names(data)
                        name1 <- nomiconf[nomiconf==input$dft]
                        name2 <- nomiconf[nomiconf==input$dst]
                        name3 <- nomiconf[nomiconf==input$dgr]
                        varftime  <- data[, name1]
                        varstatus <- data[, name2]
                        vargroup  <- data[, name3]
                        g <- max(1, length(unique(vargroup)))
                        s <- length(unique(varstatus))
                        
                        fit3 <- isolate(mod2())
                        timep <- as.numeric(dimnames(fit3$ci[,,1])[[2]])
                        restsalvocint1 <- data.frame(cbind(melt(t(round(fit3$est[,dimnames(fit3$ci[,,1])[[2]]], 4))), t(data.frame(fit3$ci)) ))
                        restsalvocint1$col <-  rep(levels(factor(varstatus))[-1], times= rep(length(timep)*g, times=s-1))
                        
                        isolate(if (input$plotConfbw == 1){isolate(ggplot(restsalvocint1, aes(x=Var1, y=value, colour=col))+   geom_step(size=1.25) +   facet_wrap(~Var2)+geom_step(aes(x=Var1, y=lower), size=0.25)+  geom_step(aes(x=Var1, y=upper, colour=col), size=0.25)+ylab("Probability") + xlab("Time")  +  scale_colour_discrete(name=name2)) 
                        }else{isolate(ggplot(restsalvocint1, aes(x=Var1, y=value, colour=col, alpha=0.9))+   
                                        geom_step(size=1.25) +   facet_wrap(~Var2)+geom_step(aes(x=Var1, y=lower), size=0.25)+  geom_step(aes(x=Var1, y=upper, colour=col), size=0.25)+ylab("Probability") + xlab("Time")  + scale_alpha_continuous(guide=FALSE) +scale_colour_grey(name=name2, start=0.1, end=0.6) + theme_bw() +
                                        scale_fill_grey(guide=FALSE, start = 0.2, end = 0.8,na.value = "red"))})
    }, silent=TRUE))}
  })  
  
  source("crr-addson.R")
  
  output$Drop.ftime2<-renderUI({
    data<-yourdata()
    if (is.null(data))
      return()
    varnames<-c(Choose='',names(data))
    selectInput("dft2", label = "This variable indicates the time at which the failure occurred. It must be numerical.", choices=as.list(varnames))
  })
  
  output$Drop.status2<-renderUI({
    data<-yourdata()
    if (is.null(data))
      return()
    varnames<-c(Choose='',names(data))
    selectInput("dst2", label = "This variable must be coded numerically with distinct codes (i.e. 1, 2, ...) for different causes of failure and code 0 for censored observations.", choices=as.list(varnames))
  })
  
  output$Drop.covnum<-renderUI({
    data<-yourdata()
    if (is.null(data))
      return()
    varnames<-names(data)
    selectInput("covnum", label = "Here you can choose one or more numerical covariates from the loaded dataset.",choices=as.list(varnames), multiple=TRUE)
  })
  
  output$Drop.covcat<-renderUI({
    data<-yourdata()
    if (is.null(data))
      return()
    varnames<-names(data)
    selectInput("covcat", label = "Here you can choose one or more categorical covariates from the loaded dataset.",choices=as.list(varnames), multiple=TRUE)
  })
  
  mod3<- reactive({  
    data <- yourdata()
    nomiconf <- names(data)
    name1 <- nomiconf[nomiconf == input$dft2]
    name2 <- nomiconf[nomiconf == input$dst2]
    name3 <- input$covnum
    name4 <- input$covcat
    varftime   <-  data[, name1]
    varstatus  <-  data[, name2]
    covarnum   <-  as.matrix(data[, name3])
    covarcat   <-  as.matrix(data[, name4])
    
    if (is.null(input$covcat)){
      covariates = covarnum
      dimnames(covariates) <- list(NULL,name3)
      mod <-isolate(crr(ftime=varftime, fstatus=varstatus, cov1=covariates))
      summary(mod)
    } else {
      designX <- list()
      for (j in 1:dim(covarcat)[2]){
        if (input$basecat==""){
          designX[[j]] <- factor2ind(covarcat[,j])
        }else{
          category <- unlist(strsplit(input$basecat, "[,]"))
          designX[[j]] <- factor2ind(covarcat[,j], as.character(category[j]))
        }
      }
      factorcovariates <- do.call(cbind, designX)
      covariates <- cbind(covarnum, factorcovariates)
      
      covlvl<-list()
      for (k in 1:dim(covarcat)[2]){
        if (input$basecat==""){
          covlvl[[k]]<-levels(as.factor(covarcat[,k]))[-1]
        }else{
          covlvl[[k]]<-levels(as.factor(covarcat[,k]))[-which(levels(as.factor(covarcat[,k]))==category[k])]
        }
      }
      namerep<- rep(0, times=length(covlvl))
      for(h in 1:length(covlvl)){
        namerep[h] <- length(covlvl[[h]])
      }
      dimnames(covariates)[[2]] <- c(name3, paste(rep(name4, times=namerep), unlist(covlvl), sep=":"))
      
      with(data, crr(ftime=varftime, fstatus=varstatus, cov1=covariates))
    }
  })
  
  output$regModel <- renderPrint({  
    try({data <- yourdata()
                 nomiconf <- names(data)
                 if (input$goButton3 == 0){validate(need(input$goButton3 != 0, "Select variables and press the go button."))}else{
                   fitcrr <- isolate(invisible(mod3()))
                   summary(fitcrr)}
    }, silent=TRUE)}) 
  
  output$regBIC <- renderPrint({  
    try({data <- yourdata()
                 nomiconf <- names(data)
                 if (input$goButton4 == 0){validate(need(input$goButton4 != 0, "Select variables and press the go button."))}else{
                   fitcrr <- isolate(invisible(mod3()))
                   modsel1<- modsel.crr(fitcrr)
                   modsel2<- modsel.crr(fitcrr, d=2)
                   data.frame(Value=c(round(modsel2$AIC[2], 2),round(modsel1$BIC[2], 2)),row.names=c("Akaike Information Criterion (AIC):","Bayesian Information Criterion (BIC):"))
                 }}, silent=TRUE)
   }) 
  
  output$regPlot <- renderPlot({
    try({data <- yourdata()
                 nomiconf <- names(data)
                 name1 <- nomiconf[nomiconf == input$dft2]
                 name2 <- nomiconf[nomiconf == input$dst2]
                 name3 <- intersect(nomiconf,input$covnum)
                 name4 <- intersect(nomiconf,input$covcat)
                 varftime   <-  data[, name1]
                 varstatus  <-  data[, name2]
                 covarnum   <-  as.matrix(data[, name3])
                 covarcat   <-  as.matrix(data[, name4])
                 if (input$goButton4 == 0){validate(need(input$goButton4 != 0, "Select variables and press the go button."))}else{
                   fitcrr <- isolate(invisible(mod3()))
                   bla<-data.frame(uft=fitcrr$uft, residsh=fitcrr$res)
                   dimnames(bla)[[2]]<-c("uft", names(fitcrr$coef))
                   plot_resid<- melt(bla,id.var="uft")
                   isolate(if (input$plotresbw == 1){isolate(ggplot(plot_resid, aes(x=uft, y=value))+ geom_point(size=1.25) +facet_wrap(~variable, scales = "free_y") +geom_smooth(method="loess") +  ylab("Shoenfeld residuals") + xlab("Failure Time"))}else{
                     isolate(ggplot(plot_resid, aes(x=uft, y=value))+ geom_point(size=1.25) +facet_wrap(~variable, scales = "free_y") +geom_smooth(col="black", method="loess")  + scale_colour_grey(start=0, end=0.8)+ theme_bw())
                   }) 
                 }
    }, silent=TRUE)}) 
})
