if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_email_address <- Sys.getenv( "my_email_address" )
library(lodown)
# examine all available ANES microdata files
anes_cat <-
	get_catalog( "anes" ,
		output_dir = file.path( getwd() ) , 
		your_email = my_email_address )

# 2015 only
anes_cat <- subset( anes_cat , year == 2015 )
# download the microdata to your local computer
stopifnot( nrow( anes_cat ) > 0 )


library(survey)

anes_df <- readRDS( file.path( getwd() , "2015 main.rds" ) )

anes_design <-
	svydesign( 
		~psu.full , 
		strata = ~strata.full , 
		data = anes_df , 
		weights = ~weight.full , 
		nest = TRUE 
	)

anes_design <- 
	update( 
		anes_design , 
		q2 = q2 ,
		never_rarely_wore_bike_helmet = as.numeric( qn8 == 1 ) ,
		ever_smoked_marijuana = as.numeric( qn47 == 1 ) ,
		ever_tried_to_quit_cigarettes = as.numeric( q36 > 2 ) ,
		smoked_cigarettes_past_year = as.numeric( q36 > 1 )
	)
sum( weights( anes_design , "sampling" ) != 0 )

svyby( ~ one , ~ ever_smoked_marijuana , anes_design , unwtd.count )
svytotal( ~ one , anes_design )

svyby( ~ one , ~ ever_smoked_marijuana , anes_design , svytotal )
svymean( ~ bmipct , anes_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , anes_design , svymean , na.rm = TRUE )
svymean( ~ q2 , anes_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , anes_design , svymean , na.rm = TRUE )
svytotal( ~ bmipct , anes_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , anes_design , svytotal , na.rm = TRUE )
svytotal( ~ q2 , anes_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , anes_design , svytotal , na.rm = TRUE )
svyquantile( ~ bmipct , anes_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ ever_smoked_marijuana , 
	anes_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ever_tried_to_quit_cigarettes , 
	denominator = ~ smoked_cigarettes_past_year , 
	anes_design ,
	na.rm = TRUE
)
sub_anes_design <- subset( anes_design , qn41 == 1 )
svymean( ~ bmipct , sub_anes_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , anes_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ ever_smoked_marijuana , 
		anes_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( anes_design )
svyvar( ~ bmipct , anes_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , anes_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , anes_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ never_rarely_wore_bike_helmet , anes_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_bike_helmet , anes_design )
svychisq( 
	~ never_rarely_wore_bike_helmet + q2 , 
	anes_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_bike_helmet + q2 , 
		anes_design 
	)

summary( glm_result )
library(srvyr)
anes_srvyr_design <- as_survey( anes_design )
anes_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

anes_srvyr_design %>%
	group_by( ever_smoked_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

