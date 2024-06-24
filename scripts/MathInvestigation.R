library(googlesheets4)

Math <- read_sheet("https://docs.google.com/spreadsheets/d/1-U9ySxG3rk3aXMyeCzVywOfzwIg083EJ-XQGpBWDcv4/",
                   col_types="c") |> 
  filter(!is.na(Date))

MathMC <- Math |> 
  select(c(2:8)) |> 
  pivot_longer(cols=starts_with("Please rate your level"),
               names_to="question",
               values_to="value") |> 
  mutate(question=str_extract(question,"(?<=\\[).+?(?=\\])"),
         question=str_wrap(question,25))

MathDiscourse <- Math |> 
  select(c(2:3,9)) |> 
  rename(ProductiveLearningEnvironments=`The learning environment supported me to engage in meaningful discourse -- collaborate, share ideas, be curious, evaluate ideas, be vulnerable, participate equitably. Please select all that apply.`) |> 
  mutate(WholeGroup=str_detect(ProductiveLearningEnvironments,"In the whole group"),
         Partner=str_detect(ProductiveLearningEnvironments,"With a Partner"),
         SmallGroup=str_detect(ProductiveLearningEnvironments,"In small groups"),
         Facilitator=str_detect(ProductiveLearningEnvironments,"With the facilitator/coach")) |> 
  pivot_longer(WholeGroup:Facilitator,names_to="question",
               values_to="value")


MathMC |> 
  ggplot(mapping=aes(x=question,fill=value)) +
  geom_bar(position="dodge") + coord_flip()

MathDiscourse |> 
  filter(value==TRUE) |> 
  ggplot(mapping=aes(x=question)) +
  geom_bar()
