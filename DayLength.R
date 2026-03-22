#Required packages
library(tidyverse)
library(tesseract)
library(pdftools)
library(readr)
library(janitor)
library(zoo)
library(hms)
library(showtext)
library(ggthemes)

#Variable names were manually written in an excel file, it was imported as a vector
dt_names<- readxl::read_excel("Input/var_names_25.xlsx")[[1]]

#Aggregating the pdf files 
dt_files<- list.files(path= "Input", pattern = ".pdf$", full.names = TRUE)

#Importing the files, will take a bit of time due to OCR reading
dt_list<- dt_files %>% 
  set_names(make_clean_names(basename(.))) %>% 
  map(~read_lines(pdf_ocr_text(.x)))

#Basic text based cleaning by removing white spaces and separating columns
dt<- map(dt_list, \(x) str_split(string= str_squish(x), pattern = " "))

#Checking whether each rows is 25 column wide like in the pdf
dt %>% 
  map(~ map_int(.x, length)) %>%
  map(~ which(.x == 25)) %>% 
  map(length)

#So we found that 1st, 2nd, 3rd, and 4th files missed none, 2, 1, and 1 entries respectively
#OCR read 2 extra values at 24th and 43rd lines of 2nd file
dt$x202510_202512_pdf[[23]][11]<- "47.1"
dt$x202510_202512_pdf[[24]][11]<- "47.7"
dt$x202510_202512_pdf[[24]]<- dt$x202510_202512_pdf[[24]][-25]
dt$x202510_202512_pdf[[43]]<- dt$x202510_202512_pdf[[43]][-18]

#OCR read one extra value at 19th line of 3rd file
dt$x202601_202603_pdf[[19]]<- dt$x202601_202603_pdf[[19]][-3]

#OCR read 2 extra values at 44th line in 4th file
dt$x202604_202606_pdf[[44]]<- dt$x202604_202606_pdf[[44]][-c(10,23)]

#Aggregate all lists into one dataframe or tibble
dt<- dt %>%
  map(function(pdf_lines) {
    # 'pdf_lines' is the list of vectors for ONE pdf
    pdf_lines %>% keep(~ length(.x) == 25)
  }) %>% 
  map_dfr(function(one_pdf){
    # Inner map_dfr: Loops through each vector (line) inside that specific PDF
    one_pdf %>% map_dfr(~ as.data.frame(t(.x)))
  }, .id = "file_name"
  ) %>% 
  set_names(c("file_name", dt_names))

#Variable cleaning and formatting
dt_final<- dt %>% 
  group_by(file_name) %>% 
  mutate(day= row_number()) %>% 
  ungroup() %>% 
  mutate(across(ends_with("_m"), ~as.numeric(str_extract(.,"\\d{1,2}\\.\\d$")))) %>% 
  mutate(across(ends_with("_hr"), ~as.numeric(str_extract(., "\\d{1,2}")))) %>% 
  pivot_longer(cols = -c(file_name,day), 
               names_to = c("month", ".value"), 
               names_pattern = "(^m\\d{1})_(\\w+$)") %>% 
  mutate(start_period = str_extract(file_name, "\\d{6}"),
         start_month = as.numeric(str_sub(start_period, 5, 6)),
         month_offset = as.numeric(str_remove(month, "m")) - 1,
         month = start_month + month_offset,
         year= as.numeric(str_sub(file_name, 2,5)),
         date= make_date(year, month, day),
         #mt= morning twilight, sr=sunrise, ss=sunset, et=evening twilight
         mt= make_datetime(year, month, day, mt_hr, floor(mt_m), (as.numeric(mt_m) %% 1) * 60),
         sr= make_datetime(year, month, day, sr_hr, floor(sr_m), (as.numeric(sr_m) %% 1) * 60),
         ss= make_datetime(year, month, day, ss_hr, floor(ss_m), (as.numeric(ss_m) %% 1) * 60),
         et= make_datetime(year, month, day, et_hr, floor(et_m), (as.numeric(et_m) %% 1) * 60),
         ) %>% 
  select(-c(mt_hr:month_offset)) %>% 
  filter(!if_all(mt:et, is.na)) %>% 
  arrange(date)

#Checking again for missing values in each column
dt_final %>% mutate(across(everything(), ~is.na(.))) %>%  summarise(across(everything(), ~sum(.)))

#Performing spline based imputation as time value around the year is a curve
#Then spiked values from bad OCR reading is smoothed 
dt_final<- dt_final %>% 
  mutate(across(mt:et, 
                ~ifelse(
                  1438<time_length(.-lag(.), unit = "minute")& time_length(.-lag(.), unit = "minute")<1442, ., NA)
                )) %>%
  group_by(month) %>%
  mutate(across(mt:et, ~na.spline(as.numeric(.), na.rm = FALSE)
                )) %>% 
  mutate(across(mt:et, ~as.POSIXct(.,origin = "1970-01-01", tz="UTC")
                )) %>% 
  ungroup()


#Whoosh!! cleaning is done. Let's make some charts----
#Setting up the fonst
sysfonts::font_add_google("Roboto", "my_font")
showtext::showtext_auto()

#Setting up manual up colors
sky_colors <- c(
  "Morning Twilight" = "#4575b4", # Deep Sky Blue
  "Sunrise"          = "#fdae61", # Soft Orange
  "Sunset"           = "#f46d43", # Deep Orange/Red
  "Evening Twilight" = "#313695"  # Midnight Blue
)


#Creating chart
dt_final %>% 
  mutate(day_sec   = as_hms(ss) - as_hms(sr),
         night_sec = 24*3600- day_sec,
         diff_sec  = abs(day_sec - night_sec)) %>% 
  group_by(is_spring = month(date) < 7) %>% 
  mutate(is_equinox = (diff_sec == min(diff_sec))) %>% 
  ungroup() %>% 
  mutate(across(mt:et, ~as_hms(.)),
         is_astro_equinox = date %in% as.Date(c("2025-09-23", "2026-03-20"))) %>%
  pivot_longer(cols = c(mt,sr,ss,et), names_to = "category", values_to = "value") %>% 
  mutate(category= fct_recode(category, 
                              "Morning Twilight"="mt",
                              "Sunrise"="sr",
                              "Sunset"="ss",
                              "Evening Twilight"="et"),
         category = fct_relevel(category, "Morning Twilight", "Sunrise", "Sunset", "Evening Twilight")) %>% 
  ggplot(aes(x = as.Date(date), y = value, color=category)) +
  geom_line(linewidth = 0.8) +
  geom_vline(data = . %>% filter(is_equinox | is_astro_equinox), 
             aes(xintercept = date, linetype = is_astro_equinox), 
             color = "black", alpha = 0.6, show.legend = FALSE) +
  geom_text(data = . %>% filter((is_equinox | is_astro_equinox) & category == "Sunrise"),
            aes(x = as.Date(date)+2,
                y = ifelse(is_equinox, as_hms("15:00:00"), as_hms("09:00:00")), 
                label = format(date, "%d %b"),
                fontface = ifelse(is_astro_equinox, "italic", "bold")),
            position = "nudge",
            family = "my_font", angle = 90, vjust = -0.5, size = 10, color = "firebrick", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_y_continuous(labels = function(x) format(as_hms(x),"%H:%M")) +
  scale_color_manual(values = sky_colors) +
  labs(title = "Equilux at Dhaka: 5 Days Delta with The Equinox",
       subtitle= "Solid: Calculated Equilux | Dotted: Astronomical Equinox",
       caption = "Source: Dhaka Station, Bangladesh Meteorological Dept. (2025-2026)") +
  ggthemes::theme_wsj(base_family="my_font") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=40),
        plot.caption = element_text(size=20, face = "italic"),
        plot.subtitle = element_text(size=25),
        plot.title.position = "plot",
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 25),
        legend.box.margin = margin(-12,0,-12,0),
        legend.position = "bottom")

ggsave(filename= "Output/daylength.png", dpi = 300)



