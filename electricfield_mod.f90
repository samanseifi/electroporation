MODULE ELECTRICFIELD
USE VARIABLES
USE UTIL

IMPLICIT NONE

CONTAINS

FUNCTION V_m(t)
	real*8 :: t
	real*8 :: tau, V_m
	
	tau = Cm*L*(lambda_in + lambda_ex)/(lambda_ex*lambda_in)
	V_m = U0*(1 - exp(-t/tau))
	
END FUNCTION V_m

END MODULE ELECTRICFIELD
