# Exposure functions 1 and 2
# By group
fun1<-function(group, var, name){
  M<<-Merged_survey %>%
    select({{group}}, surveydate, {{var}}) %>%
    rename(group = 1) %>% #column group is in
    rename(var = 3) %>% #column var is in
    group_by(group, surveydate, var) %>%
    drop_na(var) %>%
    summarise(N=n()) %>% #gets N by group survey date and response for exposure to myth
    mutate(Percentage=N/sum(N)*100) #calculates percentage that responded with a certain response for each group level and myth

  M<-M %>% mutate(y=case_when(var %in% c("No", "Not sure") ~ "No",
                              var %in% c("Yes") ~"Yes")) %>%
    group_by(group, surveydate, y) %>%
    drop_na(var) %>%
    summarise(Percentage = sum(Percentage)) %>% #redifnes percentage based on two categoires with "unsure" grouped with no
    mutate(Percentage=ifelse(y=="No" & Percentage=="100", 0, Percentage)) %>% #Sets percentage to 0 if the group level had no exposure or unsure exposure
    mutate(y=ifelse(y=="No" & Percentage==0,"Yes", y)) %>%  #If 0% of group reported no exposure, reset to Yes exposure so that % exposed can be 0%
    filter(y=="Yes") #this is the number for the dashboard.

  df2<-M %>% rename(Group = group,
                    Myth=y,
                    Percent_Exposure=Percentage) %>%
    mutate(Myth=name)
  exposure<<-rbind(exposure,df2) # This adds current myth to empty (first myth) or last dataframe generated
}


#All
fun2<-function(var, name){
  M<-Merged_survey %>%
    select(surveydate, {{var}}) %>%
    rename(var = 2) %>%
    group_by(surveydate, var) %>%
    drop_na(var) %>%
    summarise(Percentage=n()) %>%
    mutate(Percentage=Percentage/sum(Percentage)*100)
  M<-M %>% mutate(y=case_when(var %in% c("No", "Not sure") ~ "No",
                              var %in% c("Yes") ~"Yes"))
  M<-M%>% group_by(surveydate, y) %>%
    summarise(Percentage = sum(Percentage)) %>%
    mutate(Percentage=ifelse(y=="No" & Percentage=="100", 0, Percentage)) %>%
    mutate(y=ifelse(y=="No" & Percentage==0,"Yes", y)) %>%
    filter(y=="Yes")

  df2<-M %>%
    rename(Myth=y,
           Percent_Exposure=Percentage) %>%
    mutate(Myth=name)
  df2$Group<-"All"
  df2 <- df2[, c(4, 1, 2, 3)]
  exposure <<-  rbind(exposure, df2)
}

# Current believe function 3 and 4
#group
fun3<-function(group=NULL, filt, var, name){
  M<-Merged_survey%>%
    select({{filt}}, {{group}}, surveydate, {{var}}) %>%
    rename(filt=1) %>%
    rename(group = 2) %>%
    rename(var = 4) %>%
    filter(filt=="Yes") %>%
    filter(!is.na(var))%>%
    group_by(group, surveydate, var) %>% #groups by demographic variable*survey date*myth for summarizing
    summarise(Percentage=n()) %>%  #gives n in each group (demographic variable*survey date*myth)
    mutate(Percentage=Percentage/sum(Percentage)*100) #adds percentage as the number
  M<-M%>% mutate(x=case_when(var %in% c("Definitely true", "Seems like it could be true", "Not sure if it's true or untrue") ~ "Believe",
                             var %in% c("Definitely not true", "Seems like it's not true") ~ "Not Believe"))
  M<-M%>% group_by(group, surveydate, x) %>%
    summarise(Percentage = sum(Percentage))%>%
    mutate(Percentage=ifelse(x=="Not Believe" & Percentage=="100", 0, Percentage)) %>%
    mutate(x=ifelse(x=="Not Believe" & Percentage=="0","Believe", x))%>%
    filter(x=="Believe")
  df2<<-M %>% rename(Group = group,
                     Myth=x,
                     Percent_Believe=Percentage)%>%
    mutate(Myth=name)
  believe<<-rbind(believe,df2)
}

#all
fun4<-function(filt, var, name){
  M<-Merged_survey%>%
    select({{filt}}, surveydate, {{var}}) %>%
    rename(filt=1) %>%
    rename(var = 3) %>%
    filter(filt=="Yes") %>%
    filter(!is.na(var))%>%
    group_by(surveydate, var) %>% #groups by demographic variable*survey date*myth for summarizing
    summarise(Percentage=n()) %>%  #gives n in each group (demographic variable*survey date*myth)
    mutate(Percentage=Percentage/sum(Percentage)*100) #adds percentage as the number
  M<-M%>% mutate(x=case_when(var %in% c("Definitely true", "Seems like it could be true", "Not sure if it's true or untrue") ~ "Believe",
                             var %in% c("Definitely not true", "Seems like it's not true") ~ "Not Believe"))
  M<-M%>% group_by(surveydate, x) %>%
    summarise(Percentage = sum(Percentage))%>%
    mutate(Percentage=ifelse(x=="Not Believe" & Percentage=="100", 0, Percentage)) %>%
    mutate(x=ifelse(x=="Not Believe" & Percentage=="0","Believe", x))%>%
    filter(x=="Believe")
  M<-M %>% rename(Myth=x,
                  Percent_Believe=Percentage) %>%
    mutate(Myth=name)
  M$Group<-"All"
  M[, c(4, 1, 2, 3)]
  believe<<-rbind(believe, M)
}


# Ever Believe function  5 and 6
#all
fun5<-function(filt, var, name){
  dates<-as.character(unique(Merged_survey$surveydate))
  for(i in dates){
    M<-Merged_survey %>%
      select(ExternalReference, {{filt}}, surveydate, {{var}}) %>%
      rename(filt = 2) %>%
      rename(var = 4) %>%
      filter(filt=="Yes")%>%
      filter(surveydate<=i) %>%
      mutate(x=case_when(var %in% c("Definitely true", "Seems like it could be true", "Not sure if it's true or untrue") ~ 1,
                         var %in% c("Definitely not true", "Seems like it's not true") ~ 0)) %>%
      group_by(ExternalReference) %>%
      summarise(EverBel=sum(x, na.rm=TRUE)) %>%
      mutate(x=case_when(EverBel == 0 ~ 0,
                         EverBel>0 ~ 1)) %>%
      filter(!is.na(x))
    Group<-c("All")
    EverBel<-sum(M$x)
    n<-nrow(M)
    EB_Percent<-(sum(M$x)/nrow(M))*100
    Myth<-name
    surveydate<-i
    data<-data.frame(Group,  surveydate,  Myth, EverBel, n, EB_Percent)
    eb_all<<-rbind(eb_all, data)
  }
  return(eb_all)
}

#group
fun6<-function(group, filt, var, name, exception=""){

  M<-Merged_survey%>%
    select(ExternalReference, {{group}}, {{filt}}, surveydate, {{var}}) %>%
    #drop_na({{filt}})%>% #may need to change this to non-character
    rename(group = 2)  %>%
    rename(filt = 3) %>%
    rename(var = 5)
  dates<-as.character(unique(M$surveydate))
  dates<-dates[dates!=exception]

  for(i in dates){

    M0<-M  %>%
      filter(filt=="Yes") %>%
      filter(surveydate<=i) %>%
      mutate(x=case_when(var %in% c("Definitely true", "Seems like it could be true", "Not sure if it's true or untrue") ~ 1,
                         var %in% c("Definitely not true", "Seems like it's not true") ~ 0)) %>%
      group_by(group, ExternalReference) %>%
      summarise(EverBel=sum(x, na.rm = TRUE)) %>%
      mutate(x=case_when(EverBel == 0 ~ 0,
                         EverBel>0 ~ 1)) %>%
      summarise(EverBel=sum(x))

    M1<- M %>%
      filter(filt=="Yes") %>%
      filter(surveydate<=i) %>%
      mutate(x=case_when(var %in% c("Definitely true", "Seems like it could be true", "Not sure if it's true or untrue") ~ 1,
                         var %in% c("Definitely not true", "Seems like it's not true") ~ 0)) %>%
      group_by(group, ExternalReference) %>%
      summarise(EverBel=sum(x, na.rm = TRUE)) %>%
      mutate(x=case_when(EverBel == 0 ~ 0,
                         EverBel>0 ~ 1))%>%
      count(group)

    M2<-full_join(M0, M1)
    M2$EB_Percent<-(M2$EverBel/M2$n)*100
    M2$Myth<-name
    M2$surveydate<-i
    eb_all_group<<-rbind(eb_all_group, M2)
  }
}

fun7<-function (filt, var, name)
  {
  M_ind <<- Merged_survey %>%
    select(ExternalReference, {{filt}}, surveydate, {{var}}, gender_bl, race_bl, age_cat, partype) %>%
    rename(filt = 2) %>%
    rename(var = 4) %>%
    filter(!is.na(var)) %>%
    group_by(ExternalReference) %>%
  filter(surveydate == min(surveydate)) %>%
  mutate(FB_response = case_when(var %in% c("Definitely true", "Seems like it could be true") ~2,
                                 var %in% c("Not sure if it's true or untrue") ~ 1,
                                 var %in% c("Definitely not true", "Seems like it's not true") ~0)) %>%
  mutate(Myth = name) %>%
  rename(Reaction = var)
M_ind$FB_response <- factor(M_ind$FB_response, levels = c(0:2), labels = c("no", "unsure", "yes"))
M_ind <<- M_ind
df2 <<- M_ind[-c(2)]
first_believe <<- rbind(first_believe, df2)
}


fun8<-function (filt, var, name)
{
  M_react <<- Merged_survey %>% group_by(ExternalReference) %>%
    select(ExternalReference, {
      {
        filt
      }
    }, surveydate, {
      {
        var
      }
    }, gender_bl, race_bl, age_cat, partype) %>%
    rename(filt = 2) %>%
    rename(var = 4) %>%
    filter(!is.na(var)) %>% mutate(first_flag = case_when(filt =="Yes" & surveydate == min(surveydate) ~ 1, filt != "Yes" ~
                                                                                  0, filt == "Yes" & surveydate != min(surveydate) ~ 0)) %>%
    mutate(Baseline_response = case_when(first_flag = 1 & var %in% c("Definitely true", "Seems like it could be true")~2,
                                         first_flag = 1 & var %in% c("Not sure if it's true or untrue") ~1,
                                         first_flag = 1 & var %in% c("Definitely not true", "Seems like it's not true") ~ 0)) %>%
    mutate(Myth = name) %>%
    rename(Exposure = filt) %>% rename(Reaction = var)
  FB1 <<- rbind(FB1, M_react)
}

# First believe functions 9 and 10
fun9<-function (filt){
M_react2 <- FB1 %>% filter(Exposure == "Yes", Myth == {{filt}})
  dates <- as.character(unique(M_react2$surveydate))
  for (i in dates) {
    M_react3 <- M_react2 %>%
      filter(surveydate <= i) %>%
      group_by(Myth, ExternalReference) %>%
      summarise(N = n()) %>%
      mutate(surveydate = i) #M_react3 creates a dataset with c("surveydate",  "Myth", "n" )
    FB2 <- rbind(FB2, M_react3)
    FB3 <- FB2 %>%
      group_by(Myth, surveydate) %>%
      ungroup(ExternalReference) %>%
      summarise(N_denom = n())  #this creates the denominator for each survey date
  }
  M_react4 <- FB1 %>% #FB1 is the individual level data created from fun 8
    filter(Exposure == "Yes", Myth == {{filt}})
  dates <- as.character(unique(M_react4$surveydate))
  for (i in dates) {
    M_react5 <<- M_react4 %>% #below code classifies baseline response differently for accurate and inaccurate information items
      #(include unsure with yes (1,2) for inaccurate information items and with no (0,1) for accurate information items)
      mutate(First_Reaction_Believe = case_when(Baseline_response %in% c(1, 2) & Myth %in% Inaccurate ~ 1 ,
                                                Baseline_response %in% c(0) & Myth %in% Inaccurate ~ 0,
                                                Baseline_response %in% c(2) & Myth %in% Accurate ~ 1 ,
                                                Baseline_response %in% c(1,0) & Myth %in% Accurate ~ 0,)) %>%
      filter(First_Reaction_Believe == 1, first_flag == 1) %>%
      filter(surveydate <= i) %>%
      group_by(Myth) %>%
      summarise(N_baseline = n()) %>% mutate(surveydate = i) #creates numerator
    FB4 <<- rbind(FB4, M_react5)
  }
  FB5 <<- left_join(FB3, FB4, by = c("surveydate", "Myth")) %>%
    mutate(FB = (N_baseline/N_denom) * 100) %>% mutate(FB = if_else(is.na(FB),
                                                                    0, FB))
  first_believe_byweek <<- rbind(FB5, first_believe_byweek)
}

fun10<-function (name, group){
  M_react2 <- FB1 %>%
    select(ExternalReference, Exposure, surveydate, Myth, {{group}}, Baseline_response, first_flag)  %>%
    filter(Exposure == "Yes", Myth == {{name}})%>%
    rename(Group = 5) #This creates starting dataset of individual level data from FB1 created with function8

  #creates denominator dataset
  dates <- as.character(unique(M_react2$surveydate))
  for (i in dates) {
    M_react3 <- M_react2 %>%
      filter(surveydate <= i) %>%
      group_by(ExternalReference, Myth, Group) %>%
      summarise(N = n()) %>% #the number of surveys filled out through date for each person
      mutate(surveydate = i)
    FB2 <<- rbind(FB2, M_react3)  #creates dataset with columns (ExternalReference, Myth, Group, N, and surveydate)

    FB3 <<- FB2 %>%
      group_by(Myth, surveydate, Group) %>%
      ungroup(ExternalReference) %>%
      summarise(N_denom = n())  #this creates the denominator for each survey date
  }

  #creates numerator dataset
  for (i in dates) {
    M_react5 <- M_react2 %>% #M_react2 is individual level
      mutate(First_Reaction_Believe = case_when(Baseline_response %in% c(1, 2) & Myth %in% Inaccurate ~ 1 ,
                                                Baseline_response %in% c(0) & Myth %in% Inaccurate ~ 0,
                                                Baseline_response %in% c(2) & Myth %in% Accurate ~ 1 ,
                                                Baseline_response %in% c(1,0) & Myth %in% Accurate ~ 0,)) %>%
      filter(First_Reaction_Believe == 1, first_flag == 1) %>%
      filter(surveydate <= i) %>%
      group_by(Myth, Group) %>%
      summarise(N_baseline = n()) %>%
      mutate(surveydate = i)
    FB6 <<- rbind(FB6, M_react5)
  }
  FB5 <<- left_join(FB3, FB6, by = c("surveydate", "Myth", "Group")) %>%
    mutate(FB = (N_baseline/N_denom) * 100) %>% mutate(FB = if_else(is.na(FB), 0, FB))
}
