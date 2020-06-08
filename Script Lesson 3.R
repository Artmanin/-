library(tidyverse)

#Загрузите оба файла (cafe.xlsx и data-6783-2020-02-17.csv) в отдельные Data Frames
df_cafe <- readxl::read_excel("cafe.xlsx") %>% as.data.frame()
df_data <-
  read.csv2("data-6783-2020-02-17.csv", fileEncoding = "Windows-1251") %>% as.data.frame() %>% select(1:6)


#Проанализируйте, какие типы данных присвоились разным колонкам
str(df_cafe)
#Вижу много категориальных переменных, которые загрузились как chr - это плохо, трудно будет сортировать и группировать по уровням
#Вижу SeatsCount как chr - это плохо, нельзя будет делать необходимые расчеты 
name_factor <- c("IsNetObject",  "TypeObject", "District", "SocialPrivileges") #эти переменные явно будет полезно считать как фактор
df_cafe[,name_factor] <- lapply(df_cafe[,name_factor], as.factor)
df_cafe$SeatsCount <- as.numeric(df_cafe$SeatsCount)

str(df_data)
#Вижу ReportingPeriod как числовую переменную, но это год (фактор), могут не работать нужные для сортировки функции
#Вижу QuantityInThousandPeoples и MoscowRelativeShare как факторы - это плохо, нельзя считать 
df_data$ReportingPeriod <- as.factor(df_data$ReportingPeriod)
df_data$QuantityInThousandPeoples <- as.numeric(df_data$QuantityInThousandPeoples)
df_data$QuantityInThousandPeoples <- df_data$QuantityInThousandPeoples*1000 #не смог через pipe выше. хз как, все время ошибка классов
df_data$MoscowRelativeShare <- as.numeric(df_data$MoscowRelativeShare)
df_data$Territory <- as.character(df_data$Territory)
colnames(df_data)[2] <- "AdmArea"
#Беру 2016 год, потому что по нему есть все данные по численности
df_data <- filter(df_data, ReportingPeriod==2016)

#AdmAreaPop, содержащая численность населения каждого административного региона.
#Заметил проблему. Троицкий и Новомосковский административные округа просуммированы в DF с численностью. Предлагаю выровнять названия таким образом
df_cafe$AdmArea[str_detect(
  df_cafe$AdmArea,
  "Троицкий административный округ|Новомосковский административный округ"
)] <-
  "Троицкий и Новомосковский административные округа" #не понял как сделать проще замену текста, получилось только так

df_new <-
  left_join(
    select(
      df_cafe,
      Name,
      TypeObject,
      AdmArea,
      District,
      Address,
      SeatsCount
    ),
    select(df_data, AdmArea, QuantityInThousandPeoples),
    by = "AdmArea"
  )
colnames(df_new)[7] <- "AdmAreaPop"
str(df_new)

#Доп задание: Вычислите показатели: 
#var1 = количество кафе/кол-во людей, var2= количество людей/кол-во мест во всех кафе одного округа
df_var1 <-
  df_new %>% filter(TypeObject == "кафе") %>%
  group_by(AdmArea) %>%
  summarise(var1_0 =
              length(TypeObject)) %>% left_join(df_data, by = "AdmArea") %>% 
              mutate(var1 =
                            var1_0 / QuantityInThousandPeoples) %>% select(AdmArea, var1)
df_var2 <-
  df_new %>% filter(TypeObject == "кафе") %>% group_by(AdmArea, AdmAreaPop) %>% 
  summarise(var2 =
              unique(AdmAreaPop) / sum(SeatsCount)) %>% 
  select(AdmArea, var2)

df_new <- df_new %>% left_join(df_var1, by = "AdmArea") %>% left_join(df_var2, by = "AdmArea")
