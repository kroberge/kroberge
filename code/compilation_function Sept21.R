#This function handles LASSO downloads as of Spetember 2021.
no_covid_sept_21_compiler_fun <- function(excel_file){
  #excel_file <- "~/Documents/LA Postdoc stuff copy/RData/LASSO/Denver-Chico_collaboration/Data Compilation/January 2021/data/LASSO_2021-9-14 Su21.xlsx" # LASSO_2021-12-27 F21 this is here for trouble shooting the function
  courses_df <- read_excel(excel_file,sheet = "Course(s)")
  assessments_df <- read_excel(excel_file, sheet = "Assessment Sequence(s)")
  students_df <- read_excel(excel_file, sheet = "Students")
  pre_df <- read_excel(excel_file, sheet = "Responses (PRE)")
  post_df <- read_excel(excel_file, sheet = "Responses (POST)")
  #covid_df <- read_excel(excel_file, sheet = "COVID Responses")
  
  courses_df[courses_df == "null"] <- NA
  courses_df[courses_df == "-"] <- NA
  assessments_df[assessments_df == "null"] <- NA
  assessments_df[assessments_df == "-"] <- NA
  students_df[students_df == "null"] <- NA
  students_df[students_df == "-"] <- NA
  pre_df[pre_df == "null"] <- NA
  pre_df[pre_df == "-"] <- NA
  post_df[post_df == "null"] <- NA
  post_df[post_df == "-"] <- NA
  #covid_df[covid_df == "null"] <- NA
  #covid_df[covid_df == "-"] <- NA
  
  courses_df <- courses_df %>% select(course_id, course_number, course_name, course_strat_interactive_lecture, course_strat_integrated_lab_lecture, 
                                      course_strat_lecture, course_strat_discuss_small_groups, course_strat_design_activities, course_strat_req_work_together, course_strat_instr_solves_quant, 
                                      course_strat_instr_solves_qual, course_strat_students_solve_quant, course_strat_students_solve_qual, times_taught, course_learning_goals_prob_solving, 
                                      course_learning_goals_concept_understanding, course_learning_goals_attitudes_appreciation, course_context, course_major_requirement, course_comments, 
                                      course_use_las, la_primary_role, la_secondary_role, weekly_planning_time, times_taught_w_las, 
                                      course_uses_collab_learning, la_use_comments, participate_credit_pre, participate_credit_post, participate_email_reminders, 
                                      participate_inclass_reminders) %>%
    mutate(course_id = as.character(course_id)) %>%
    rename(used_collaborative_learning = course_uses_collab_learning)
  
  assessments_df <- assessments_df %>% select(assessment_sequence_id, course_id, assessment_code, ac_id, institution, term, year, dept_code, staff, has_multiple_versions, 
                                              filter_question, PRE_assessment_id, PRE_start_date, PRE_end_date, POST_assessment_id, POST_start_date, POST_end_date) %>%
    rename(instructor = staff,
           discipline = dept_code ) %>%
    mutate(POST_assessment_id = as.character(POST_assessment_id) ,
           PRE_assessment_id = as.character(PRE_assessment_id),
           course_id = as.character(course_id)) %>%
    rename(institution_name = institution)
  
  pre_df <- pre_df %>%
    rename_at(vars(-c('PRE_assessment_id','student_id')),function(x) paste0("pre_",x)) %>%
    mutate(PRE_assessment_id = as.character(PRE_assessment_id),
           student_id = as.character(student_id))
  
  post_df <- post_df %>%
    rename_at(vars(-c('POST_assessment_id','student_id')),function(x) paste0("post_",x)) %>%
    mutate(POST_assessment_id = as.character(POST_assessment_id),
           student_id = as.character(student_id))
  
  
  #colnames(covid_df)
  #covid_df <- covid_df %>%
  #            rename(student_id = PRE_student_id) %>%
  #            select(student_id, POST_assessment_id, POST_student_id, POST_engaged_more, POST_learned_more, POST_participated_more, POST_experienced_stability, POST_technology_difficult, POST_prefer_online, POST_what_changed, POST_impact_life, POST_tas_support, POST_las_support, POST_issues_completing, POST_no_responses)
  #colnames(students_df)
  
  
  if( !("parent_has_degree" %in% colnames(students_df))){ 
        students_df <- students_df %>% 
                       mutate(parent_has_degree = NA,
                              parent_has_degree = as.character(parent_has_degree))
  } 
  
  students_df <- students_df %>%
    mutate( time_with_la = as.character(time_with_la)) %>% #If it is all NA it comes back as a logical vector and then if_else fails trying to combing logical and character vectors
    mutate( gender =   if_else(is.na(gender)==FALSE,gender,      if_else(is.na(post_gender)==FALSE,post_gender,pre_gender)) ,
            ethnicity = if_else(is.na(ethnicity)==FALSE,ethnicity,      if_else(is.na(post_ethnicity)==FALSE,post_ethnicity,pre_ethnicity)) ,
            race = if_else(is.na(race)==FALSE,race,      if_else(is.na(post_race)==FALSE,post_race,pre_race)),
            year = if_else(is.na(year)==FALSE,year,      if_else(is.na(post_year)==FALSE,post_year,pre_year)),
            la_other_course = if_else(is.na(la_other_course)==FALSE,la_other_course,      if_else(is.na(post_la_other_course)==FALSE,post_la_other_course,pre_la_other_course)),
            status = if_else(is.na(status)==FALSE,status,      if_else(is.na(post_status)==FALSE,post_status,pre_status)) ,
            first_time_student = if_else(is.na(first_time_student)==FALSE,first_time_student,      if_else(is.na(post_first_time_student)==FALSE,post_first_time_student,pre_first_time_student)),
            first_time_la = if_else(is.na(first_time_la)==FALSE,first_time_la,      if_else(is.na(post_first_time_la)==FALSE,post_first_time_la,pre_first_time_la)) ,
            time_with_la = if_else(is.na(time_with_la)==FALSE,time_with_la,      if_else(is.na(post_time_with_la)==FALSE,post_time_with_la,pre_time_with_la)),
            parent_has_degree = if_else(is.na(parent_has_degree)==FALSE,parent_has_degree,      if_else(is.na(post_parent_has_degree)==FALSE,post_parent_has_degree,pre_parent_has_degree)) ) %>%
    select(student_id, gender, ethnicity, race, year, status, first_time_student, time_with_la, pre_assessment_id, pre_participate, pre_demographics_before, pre_version, pre_duration, pre_num_questions, pre_num_correct, pre_score_perc, pre_submitted, post_assessment_id, post_participate, post_demographics_before, post_version, post_duration, post_num_questions, post_num_correct, post_score_perc, post_submitted, is_matched, parent_has_degree) %>%
    rename(pre_agree_to_participate = pre_participate,
           post_agree_to_participate = post_participate,
           year_in_school = year,
           PRE_assessment_id = pre_assessment_id,
           POST_assessment_id = post_assessment_id,
           student_or_la = status,
           pre_score = pre_score_perc,
           post_score = post_score_perc)
  
  
  
  temp_df <- full_join(courses_df, assessments_df,by ="course_id")
  temp_df <- left_join(temp_df, students_df, by = c("PRE_assessment_id", "POST_assessment_id"))
  temp_df <- left_join(temp_df, pre_df, by = c("PRE_assessment_id", "student_id"))  
  temp_df <- left_join(temp_df, post_df, by = c("POST_assessment_id", "student_id"))  
  
  return(temp_df)
}

