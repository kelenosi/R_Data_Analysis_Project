
function(input,output){
  
# Read in 'auto' table from the SQLite database
  
 auto <- reactive({
   df_auto
 })

 
output$plot_bar <- renderPlot(
 
  ggplot(df_auto, aes(x= clm,  group=get(input$cat))) + 
    geom_bar(aes(y = ..prop.., 
                 fill = factor(..x..,labels = c("0","1"))), stat="count") +
    geom_text(aes(label = scales::percent(..prop..),
                  y= ..prop.. ), stat= "count", 
              vjust = .20, size = 3.5, fontface = "bold")+
    labs(y = "Percent", fill="Claim Occurrence") +
    facet_wrap(get(input$cat)~.) +
    coord_cartesian(ylim = c(0,1)) +    
    scale_y_continuous(labels = scales::percent) +
    ggtitle(paste0("Occurrence of Claim Versus ",toupper(input$cat))) +
    theme(plot.title = element_text(hjust = .5)) 
  
  )

output$plot_box <- renderPlot(
  
ggplot(df_auto %>% filter(claimcst0 != 0) %>%
         mutate(log_claim = log(claimcst0)),
       aes(x=fct_reorder(factor(get(input$cat)),log_claim),y=log_claim)) +
  geom_boxplot() +
  xlab(input$cat) +
  ylab("Log Claim Amount") +
  ggtitle(paste0("Box Plot: ", toupper(input$cat)," vs Log claim") ) +
  theme(plot.title = element_text(hjust = .5))
 
) 

output$plot_point <- renderPlot(
  
  ggplot(auto() %>% filter(veh_value <= 7,
                        claimcst0 <= 10000), aes(x=veh_value, y= claimcst0))+
    geom_point(aes(colour = get(input$cat))) +
    facet_wrap(. ~ get(input$cat)) +
    xlab("Vehicle value in $10,000 units") +
    ylab("Claim Size") +
    ggtitle("Vehicle Insurance Data Scatterplot") +
    theme(plot.title = element_text(hjust = .5)) +
    guides(color=guide_legend(input$cat))
)


output$plot_hist <- renderPlot(
  ggplot(auto() %>% filter(veh_value <= 7) %>% 
           mutate(claimcst0 = log(claimcst0),
                  exposure = log(exposure),
                          veh_value = veh_value*10000),
    aes(x=get(input$num))) +  #clm_size)) +
    geom_histogram(bins = 100) +
    xlab(input$num) +   # "Claim size ($1000s)") +
    ggtitle(paste0("Histogram: ", ifelse(input$num %in% c("claimcst0","exposure"),
                   paste0("LOG_",toupper(input$num)),toupper(input$num)))) +
    theme(plot.title = element_text(hjust = .5))
  
  
)

# Generate a summary of the data ----
output$summary <- renderPrint({
  auto() %>% select(input$cat,input$num) %>% 
                map(summary)
})


# univariate table for categorical variables ----

output$table <- renderTable({
    auto() %>% 
    group_by(get(input$cat)) %>% 
    summarise(exposure=sum(exposure,na.rm = T),
              num_clm = sum(numclaims,na.rm =T),
              clm_amt = sum(claimcst0,na.rm = T)) %>% 
    mutate(freq = num_clm/exposure,
           severity = clm_amt/num_clm,
           pure_prem = clm_amt/exposure) %>% 
    rename(!!input$cat := "get(input$cat)") %>% 
    mutate(base_level = unique(get(input$cat))[which.max(exposure)]) 
  
})

#Generate underlying data ----
output$data <- renderDataTable({
  auto()
})

# Generate ROC graph ----

model <- reactive({

  model_insample <- predict(model_final,type = "response")
  model_insample <- model_insample > input$cutoff
  model_insample <- as.numeric(model_insample)

})

auc <- reactive({
  roc_0 <- pROC::roc(clm ~ model(), data=auto())
  round(as.numeric(roc_0$auc),3)
})



output$plot_roc <- renderPlot(

ROC(model(),auto()$clm,auc(),cutoff= input$cutoff)

)

# Generate Confusion Matrix ----

output$c_matrix <- function(){
  
tbl_cm <- table(auto()$clm, model(), dnn = c("Actual","Predicted"))

df_cm <- as.data.frame(tbl_cm,row.names = c(1,2,3,4)) %>%
  mutate(Actual = ifelse(Actual == 0,"No","Yes"),
         Predicted = ifelse(Predicted == 0,"No","Yes")) %>% 
  pivot_wider(names_from = "Predicted",values_from = "Freq") %>%
  rowwise() %>%
  mutate(Total = sum(No,Yes,na.rm = T))

df_total <- as.data.frame(t(sapply(df_cm[,-1],sum)))
df_total$Actual <- c("Total")

df_cm <- rbind(df_cm,df_total)

 cbind(name =rownames(df_cm),df_cm) %>%
  mutate(name = ifelse(name == 2,"Actual Claim","")) %>% 
  kbl(format = "html",format.args = list(big.mark = ","),
      col.names = c("","","No","Yes","Total")) %>%
  add_header_above(c(" "," ","Predicted Claim" = 3)) %>% 
  add_header_above(c("CLASSIFICATION TABLE " = 5)) %>% 
  collapse_rows(1, valign = "top") %>% 
  column_spec(1:4, width = "6em") %>% 
  column_spec(1:2,bold = TRUE) %>% 
  kable_styling(full_width = F, html_font = "Cambria")
  
}

# Generate prediction ----

val <- reactiveValues(result = NULL)

observeEvent(input$calc, {
  
  new_data <- data.frame(agecat = input$agecat,
                         area = input$area,
                         veh_age = input$veh_age,
                         veh_body = input$veh_body,
                         exposure = input$exposure)
  
  val$result <- round(predict(model_final,newdata = new_data,type = "response"),
                      digits = 3)

})


output$value <- renderText({
  paste(val$result)
})
  
}

