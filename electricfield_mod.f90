MODULE ELECTRICFIELD
USE VARIABLES
USE UTIL

IMPLICIT NONE

CONTAINS

FUNCTION F(Vm)
	real*8 :: Vm
	real*8 :: tau, F
	
!	tau = Cm*L*(lambda_in + lambda_ex)/(lambda_ex*lambda_in)
!	V_m = U0*(1 - exp(-t/tau))
	
!	F = (exp(L)*U0 + Vm)/(1 - exp(2*L))
	F = -(1.0/L)*(U0/2.0 + Vm/2.0)
	
END FUNCTION F

END MODULE ELECTRICFIELD
