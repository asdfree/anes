if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_email_address <- Sys.getenv( "my_email_address" )
library(lodown)
# examine all available ANES microdata files
anes_cat <-
	get_catalog( "anes" ,
		output_dir = file.path( getwd() ) , 
		your_email = my_email_address )

# 2016 only
anes_cat <- subset( anes_cat , directory == "2016 Time Series Study" )
# download the microdata to your local computer
stopifnot( nrow( anes_cat ) > 0 )


library(survey)

anes_df <- 
	readRDS( 
		file.path( getwd() , 
			"2016 Time Series Study/anes_timeseries_2016_.rds" )
	)

anes_design <-
	svydesign( 
		~v160202 , 
		strata = ~v160201 , 
		data = anes_df , 
		weights = ~v160102 , 
		nest = TRUE 
	)

anes_design <- 
	update( 
		anes_design , 
		
		one = 1 ,
		
		pope_francis_score = ifelse( v162094 %in% 0:100 , v162094 , NA ) ,

		christian_fundamentalist_score = ifelse( v162095 %in% 0:100 , v162095 , NA ) ,
		
		primary_voter = ifelse( v161021 %in% 1:2 , as.numeric( v161021 == 1 ) , NA ) ,

		think_gov_spend =
			factor( v161514 , levels = 1:4 , labels =
				c( 'foreign aid' , 'medicare' , 'national defense' , 'social security' )
			) ,
		
		undoc_kids =
			factor( v161195x , levels = 1:6 , labels =
				c( 'should sent back - favor a great deal' ,
					'should sent back - favor a moderate amount' ,
					'should sent back - favor a little' ,
					'should allow to stay - favor a little' ,
					'should allow to stay - favor a moderate amount' ,
					'should allow to stay - favor a great deal' )
			)

	)
sum( weights( anes_design , "sampling" ) != 0 )

svyby( ~ one , ~ undoc_kids , anes_design , unwtd.count )
svytotal( ~ one , anes_design )

svyby( ~ one , ~ undoc_kids , anes_design , svytotal )
svymean( ~ pope_francis_score , anes_design , na.rm = TRUE )

svyby( ~ pope_francis_score , ~ undoc_kids , anes_design , svymean , na.rm = TRUE )
svymean( ~ think_gov_spend , anes_design , na.rm = TRUE )

svyby( ~ think_gov_spend , ~ undoc_kids , anes_design , svymean , na.rm = TRUE )
svytotal( ~ pope_francis_score , anes_design , na.rm = TRUE )

svyby( ~ pope_francis_score , ~ undoc_kids , anes_design , svytotal , na.rm = TRUE )
svytotal( ~ think_gov_spend , anes_design , na.rm = TRUE )

svyby( ~ think_gov_spend , ~ undoc_kids , anes_design , svytotal , na.rm = TRUE )
svyquantile( ~ pope_francis_score , anes_design , 0.5 , na.rm = TRUE )

svyby( 
	~ pope_francis_score , 
	~ undoc_kids , 
	anes_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ christian_fundamentalist_score , 
	denominator = ~ pope_francis_score , 
	anes_design ,
	na.rm = TRUE
)
sub_anes_design <- subset( anes_design , v161158x == 4 )
svymean( ~ pope_francis_score , sub_anes_design , na.rm = TRUE )
this_result <- svymean( ~ pope_francis_score , anes_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ pope_francis_score , 
		~ undoc_kids , 
		anes_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( anes_design )
svyvar( ~ pope_francis_score , anes_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ pope_francis_score , anes_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ pope_francis_score , anes_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ primary_voter , anes_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( pope_francis_score ~ primary_voter , anes_design )
svychisq( 
	~ primary_voter + think_gov_spend , 
	anes_design 
)
glm_result <- 
	svyglm( 
		pope_francis_score ~ primary_voter + think_gov_spend , 
		anes_design 
	)

summary( glm_result )
library(srvyr)
anes_srvyr_design <- as_survey( anes_design )
anes_srvyr_design %>%
	summarize( mean = survey_mean( pope_francis_score , na.rm = TRUE ) )

anes_srvyr_design %>%
	group_by( undoc_kids ) %>%
	summarize( mean = survey_mean( pope_francis_score , na.rm = TRUE ) )

