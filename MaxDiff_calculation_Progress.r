package.list <- c("bwsTools","readxl","tidyverse","tidyr", "stringr")
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library("bwsTools")
library("readxl")
library("tidyverse")
library("tidyr")
library("stringr")

setwd("C:/Users/DH/Documents/Projects/AnchoredMaxDiff") #Change this directory

#----- Empirical Bayes Method to Calculate Individual Best-Worst 
#----- Own case - experiment 475255 - Traditional approach -----#

## Read data
### List of alternatives
alt <- read_excel("20230621 Conjointly experiment 475255 export at 1906.xlsx",sheet="List of levels")
alt <- data.frame(alt["name"],alt["ALseqNum"])

### Experimental design with alternatives
exp_design <- read_excel("20230621 Conjointly experiment 475255 export at 1906.xlsx",sheet="Experimental design")

merge_data <- merge(exp_design, alt, by.x = "A1", by.y = "ALseqNum", all.x = TRUE)
merge_data <- merge_data[order(merge_data$BLOCK,merge_data$QES,merge_data$ALT),]
head(merge_data)

### Best & worst alternatives

best_data <- read_excel("20230621 Conjointly experiment 475255 export at 1906.xlsx",sheet="Raw responses")

BestT_data <- best_data %>%
  pivot_longer(cols = starts_with("q"), 
               names_to = "QES", 
               values_to = "BestChoice")%>%
  mutate(QES = str_remove(QES, "q"))

worst_data <- read_excel("20230621 Conjointly experiment 475255 export at 1906.xlsx",sheet="Worst raw responses")

worstT_data <- worst_data %>%
  pivot_longer(cols = starts_with("q"), 
               names_to = "QES", 
               values_to = "WorstChoice")%>%
  mutate(QES = str_remove(QES, "q"))

### Transform data structure to use bwsTools

shaped_data <- merge(merge_data, BestT_data, by = c("BLOCK", "QES"))
shaped_data <- merge(shaped_data, worstT_data, by = c("BLOCK", "QES"))

shaped_data <- shaped_data %>% mutate(value=if_else(ALT==BestChoice,1,if_else(ALT==WorstChoice,-1,0)))

shaped_data <- select(shaped_data, BLOCK, QES,name,value)
colnames(shaped_data) <- c("id", "block", "label", "value") 
head(shaped_data,20)

shaped_data <- as_tibble(shaped_data)

### compute scores using the traditional scope 

#Error: Each id must be rating the same set of items, unbalanced design 

### drop QA checks from e_bayescoring

e_bayescoringMod <- function (data, id, block, item, choice, E = 0.1, alpha = 1, 
    wide = FALSE) {
     agg_dat <- data %>% dplyr::group_by(!!sym(item)) %>% dplyr::summarise(bests = sum(!!sym(choice) ==
        1), worsts = sum(!!sym(choice) == -1), all = dplyr::n()) %>%
        dplyr::mutate(p_j = (all - worsts + bests)/(2 * all)) %>% 
        dplyr::select(!!sym(item), p_j)
    ind_dat <- data %>% dplyr::group_by(!!sym(id), !!sym(item)) %>%
        dplyr::summarise(bests = sum(!!sym(choice) == 1), worsts = sum(!!sym(choice) ==
            -1), all = dplyr::n()) %>% dplyr::mutate(p_ij = (all -
        worsts + bests)/(2 * all)) %>% dplyr::ungroup() %>% dplyr::mutate(p_ij = dplyr::case_when(p_ij ==
        0 ~ E, p_ij == 1 ~ (1 - E), TRUE ~ p_ij))
    out <- ind_dat %>% dplyr::left_join(agg_dat, by = item) %>%
        dplyr::mutate(p_ij = ((1/(1 + alpha)) * p_ij) + ((alpha/(1 +
            alpha)) * p_j)) %>% dplyr::mutate(b_ebayes = log(p_ij/(1 - 
        p_ij))) %>% dplyr::select(!!sym(id), !!sym(item), b_ebayes)
    if (wide) {
        out <- out %>% tidyr::spread(!!sym(item), b_ebayes)
    }
    return(out)
}

scores <- e_bayescoringMod(shaped_data, "id", "block", "label", "value")

grouped_scores <- scores%>%group_by(label)%>%summarize(score=mean(b_ebayes))

#write.csv(scores,"ind_scores_Trad.csv")
#write.csv(grouped_scores,"grouped_scores_Trad.csv")

#----- Own case - experiment 475255 - anchored design approach ---#

## recode the matrix grid answers

maxblock = max(shaped_data$block)+1

anchored <- read.csv("Anchored_answers.csv")
anchored[is.na(anchored)] <- 0

## collapse the all the columns to one 
anchored_collapsed <- pivot_longer(
  anchored,
  cols = -ID,
  names_to = "Choice",
  values_to = "Count"
)

## Keep only the selected options 

anchored_collapsed <- subset(anchored_collapsed, Count != 0)

## recode the values below the threshold (matrix grid)

anchored_collapsed$Count <- ifelse(grepl("Indifferent", anchored_collapsed$Choice), 0,
                       ifelse(grepl("avoid", anchored_collapsed$Choice), -1, anchored_collapsed$Count))

## Extract the names (alternatively this can be done in the csv file)

anchored_collapsed$Choice <- str_extract(anchored_collapsed$Choice, "(?<=\\.\\.)[[:alnum:]\\.]+(?=\\.Column)")

## add the "new" block and match the structure required by bwsTools

anchored_collapsed <- mutate(anchored_collapsed, block = maxblock)%>%
  select(ID, block, everything())

anchored_collapsed$Choice <- str_replace_all(anchored_collapsed$Choice, "\\.", " ")

colnames(anchored_collapsed) <- c("id", "block", "label", "value") 

anchored_final <- rbind(shaped_data,anchored_collapsed)
anchored_final <- anchored_final[order(anchored_final$id,anchored_final$block),]


scores_anchored <- e_bayescoringMod(anchored_final, "id", "block", "label", "value")
grouped_scores_ach <- scores_anchored%>%group_by(label)%>%summarize(score=mean(b_ebayes))

#write.csv(scores_anchored,"ind_scores_Anch.csv")
#write.csv(grouped_scores_ach,"grouped_scores_Anch.csv")