!! by Saman Seifi
!!Module contains routines for numerical integration of Allen-Cahn model for system of lipid/pore under applied electricfield 
!! using central difference (spherical laplacian) formula used for nabla^2
!!The phase-field derivation is from the book by Provatas and Elder and this code developed on top of Model A
! 
!
MODULE SOLVER
USE VARIABLES !!where variables are defined
USE UTIL      !!For printing and random number generation
USE ELECTRICFIELD
IMPLICIT NONE

CONTAINS !-----------

!This subroutine for time evolution of phase-feild equation
subroutine calculate
	
	!initialize to zero the average of the order parameter
	ave_psi=0.50d0 
  
	! Determining the initial condition (if initfile=yes load initial profile from file otherwise do the random seed)
	if (initfile == 'yes') then
		!reading the initial order parameter from file
		open(3,file=initname,status='unknown')
		do i=1,Nx
			do j=1,Ny
				read(3,*) PSI(i,j)
				ave_psi=ave_psi+PSI(i,j)
			end do
		end do
		close(3)
	elseif (initfile == 'no') then  ! default initial profile
		!set guassian ditributed initial conditions. NOTE: PSI-->order parameter 
		!initialize random seed for random number generation in gasdev()
		call srand(idum)
		do i=1,Nx
			do j=1,Ny
				! set PSI=0 + fluctuations 
				PSI(i,j)=0.5 + 0.001*gasdev2()
				!update order parameter sum
				ave_psi=ave_psi+PSI(i,j)
			end do  
		end do  	
	else		
		print *, 'Error: variable initfile has to be yes or no!'
		print *, ''
		print *, 'Running failure!'
		STOP
	endif
  
	!write out initial average value of the order parameter
	open(2,file='averga_psi',status='unknown')
	write(2,*) ave_psi/(Nx*Ny)

	!print intial PSI field
	open(1,file='out_0',status='unknown')
	call print_2Dfield(PSI, Nx, Ny, 1)
	close(1)

	!!!START TIME MARCHING
	do t_loop=1, tmax

		!!!!!!!!!!!!!!! order parameter equation update !!!!!!!!!!!!!!!!!!!!

		!1. periodic BC for PSI and compute grad^2(PSI) array
		call PERIODIC(PSI)
		!call ZEROFLUX(PSI)
		call NABLA2(PSI,grad2)
		
		!correction coeff for area
		do i=1,Nx
			do j=1,Ny
				PSI_0(i,j) = PSI(i,j)
				!PSI_P(i,j) = PSI(i,j)*PSI(i,j)*(3 - 2*PSI(i,j))
				PSI_P(i,j) = 0.5 + 0.5*tanh(5*(PSI(i,j) - 0.5))
			end do
		end do
		c0 = ((Nx*Ny) - SUM(PSI_0))/(Nx*Ny - SUM(PSI_P))
		c1 = SUM(PSI)/(Nx*Ny)
		
		!getting the transmembrane voltage
		Vm = V_m(dt*t_loop)
		
		sigma_elec = 0.5*Cm*Vm*Vm*c1*c1
		!print*, sigma_elec
	
		!2. calculate right hand side of model A
		do i=1, Nx
			do j=1,Ny
!				RHS(i,j)= W2*grad2(i,j)-PSI(i,j)*(1+2*PSI(i,j)*PSI(i,j)-3*PSI(i,j))-(sigma+sigma_elec)*c0*(6*PSI(i,j)-6*PSI(i,j)*PSI(i,j))
				RHS(i,j)= W2*grad2(i,j)-PSI(i,j)*(1+2*PSI(i,j)*PSI(i,j)-3*PSI(i,j))-(sigma+sigma_elec)*c0*(1.0 - tanh(PSI(i,j))*tanh(PSI(i,j)))
			end do 
		end do
		
		
		
		!initialize to zero the average of the order parameter for this time step
		ave_psi=0.0
	
		!4. take one time step forward
		do i=1, Nx
			do j=1,Ny
				PSI(i,j) = PSI(i,j) + dt*M*RHS(i,j)
				ave_psi=ave_psi+PSI(i,j)
			end do
		end do

		!!!!!!!!!!!!!!!!!!! I/O  (print out average & PSI)!!!!!!!!!!!!!!!!!!!!!!!!
		write(2,*) ave_psi/(Nx*Ny)
	
		if(mod(t_loop,file_skip)+1 == 1)then
			call chari(t_loop,cn,il)
			open(1,file='out_'//cn(1:il),status='unknown')
			call print_2Dfield( PSI, Nx, Ny, 1)
			close(1)
		end if
		
		!!!!!!!!!!!!!!!!!!! I/O  (print out stuff)!!!!!!!!!!!!!!!!!!!!!!!!		
		open(4, file='Vm', status='unknown')
		write(4,*) Vm
		
		open(5, file='c0', status='unknown')
		write(5, *) c0
		
		open(6, file='c1', status='unknown')
		write(6, *) c1

	end do !time loop

	!close file opened to store average of the order parameter
	close(2)
	close(4) !finish writing into Vm
	close(5)
	close(6)
	
end subroutine calculate 

!-------------------------------------------------------------------
!Finite difference calculation of Laplacian of A, stres answer in GR2. (See appendix Provatas, Elder book)
subroutine NABLA2(A,GR2)
	real*8, dimension(0:,0:) ::A,GR2
    GR2=0.0d0
    do i=1,Nx
		do j=1,Ny
			GR2(i,j) = ( (A(i+1,j) + A(i-1,j) + A(i,j+1) +A(i,j-1))/2.0  &
				& +   (A(i+1,j+1)+A(i-1,j+1)+A(i+1,j-1) +A(i-1,j-1))/4.0 &
				& -   3*A(i,j) )* dx2_in
		end do
	end do
end subroutine NABLA2

!-----------------------
!enfroces periodic boundary conditions (see Provatas, Elder book)
subroutine PERIODIC(A)
	real*8, dimension(0:,0:) ::A
    A(Nx+1,:)    = A(1,:)
    A(0,:)       = A(Nx,:)
    A(:,Ny+1)    = A(:,1)
    A(:,0)       = A(:,Ny)
end subroutine PERIODIC

!-----------------------
!enforce zero flux bondary conditions
subroutine ZEROFLUX(A)
	real*8, dimension(0:,0:) ::A
	A(0,:)		 = A(2,:)
	A(Nx+1,:)	 = A(Nx-1,:)
	A(:,0)		 = A(:,2)
	A(:,Ny+1)	 = A(:,Ny-1)
end subroutine ZEROFLUX
!!!!!!!!!!!!!!!!!!!!!!!

END MODULE SOLVER
