if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_email_address <- Sys.getenv( "my_email_address" )
library(lodown)
lodown( "anes" , output_dir = file.path( getwd() ) , 
	your_email = my_email_address )
