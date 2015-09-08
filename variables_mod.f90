MODULE VARIABLES
IMPLICIT NONE

!GLOBAL VARIABLES
integer               :: i,j, k, Nx, Ny , t_loop, il
integer               :: file_skip, tmax
integer, parameter    :: Nmax=2200,idum=92923932
real*8                :: dt, dx, epsilon, sigma, gamma, M, W2, dx2_in, ave_psi, c0
real*8                :: PSI(0:Nmax,0:Nmax), PSI_0(0:Nmax,0:Nmax), PSI_P(0:Nmax,0:Nmax)
real*8                :: grad2(0:Nmax,0:Nmax)
real*8                :: RHS(0:Nmax,0:NMax)
character(len=9)      :: cn
character(len=3)	  :: initfile
character(len=14)	  :: initname
character(len=3)	  :: elecfield
double complex 		  :: PSIt(0:Nmax,0:Nmax)
double complex 		  :: PSIc(0:Nmax,0:Nmax)

!ELECTRIC STUFF
real*8				  :: lambda_in, lambda_ex
real*8			      :: Cm, C_LW
real*8				  :: L, c1
real*8				  :: Vm, U0
real*8				  :: sigma_elec
real*8				  :: Kw, Km

CONTAINS

subroutine read_globals

	INTEGER 		  :: num_lines, ios, k
	character(len=1)  :: junk	
	
	!read initial parameters from file 
	open(1,file='input')
		read(1,*) dx          !space step
		read(1,*) dt          !time step
		read(1,*) epsilon     !small length scale
		read(1,*) sigma		  !surface tension
		read(1,*) gamma       !line tension coeff.
		read(1,*) file_skip   ! how often to print
		read(1,*) tmax        !how many total time steps to take
		read(1,*) Nx          !system size in the x direction
		read(1,*) Ny          !system size in the y direction 
		read(1,*) initfile	  !check if it reads the initial condition from a data file (= "yes" or "no")
		read(1,*) initname    !determining the initial condition
		read(1,*) elecfield	  !switch to turn on/off the electric field (= "on" or "off")
	close(1)
	
	if (elecfield == 'on') then
		open(2, file='input_elec')
			read(2,*) lambda_in 	!conductivity of interior fluid
			read(2,*) lambda_ex		!conductivity of exterior fluid
			read(2,*) Km
			read(2,*) Kw
			read(2,*) Cm			!capacitance of the lipid membrane
			read(2,*) L				!membrane to electrodes length
			read(2,*) U0			!applied potential
		close(2)
	endif
	
	!initialize specific parameters 
	PSI=0.0d0
	
	W2 = (epsilon*gamma)
	
	C_LW = (Kw/Km - 1)*Cm
	
	M=1.0d0
	
	dx2_in = 1 / dx**2
	
	!test that system sizes do not exceed declared dimensions (avoid this using dynamic allocation)
	if(Nx+1.gt.Nmax.or.Ny+1.gt.Nmax)then 
		print*, 'One of the system dimensions exceeds array dimensions'
		stop
	endif
	
	!teset if the system size given in initial file is the same as it defined in Nx and Ny
	num_lines = 0
	if (initfile =='yes') then
		open(0, file=initname, status='unknown')
		do k=1,1000000
			read(0,*, IOSTAT=ios) junk
			if (ios /= 0) exit
			if (k == 1000000) then
				print*, 'Error: Maximum number of records exceeded'
				print*, ''
				print*, 'Runing failure!'
				STOP
			endif
			num_lines = num_lines + 1 !number of lines in the file
		enddo
		rewind(0) 
		if (mod(num_lines, Nx*Ny) /= 0) then
			print*, 'Error: The system size of the initial file does not'
			print*, 'maatch with your domain size Nx and Ny'
			print*, ''
			print*, 'Runing failure!'
			STOP
		endif 
	endif
	
	print*,'Global data:'
	print*,'_________________________________'
	print 10, sigma, gamma, epsilon, dt, dx, tmax
10  FORMAT(' sigma = ',F8.4,/ ' gamma = ',F8.4,/ ' epsilon = ',F8.4,/ ' dt = ',F6.4,/' dx = ',F6.4,/ ' tmax = ',I6 )
	print 20, file_skip, Nx, Ny
20	FORMAT(' file_skip = ',I6,/ ' Nx = ',I6,/ ' Ny = ',I6)
	print*, 'Initil File = ', initfile
	if (initfile == 'yes') then
		print*, 'Filename = ', initname
	else
		print*, 'Initial = RANDOMSEED'
	endif
	print*, 'Electric field = ', elecfield
	print*,'_________________________________'
	
	if (elecfield == 'on') then
		print*, ''
		print*, 'Electric data:'
		print*, '_________________________________'
		print 30, lambda_in, lambda_ex, Cm
30		FORMAT(' Interior Cond. = ',F8.4,/ ' Exterior Cond. = ',F8.4,/ ' Lipid Capcitance = ',F8.4)
		print 40, L, U0
40		FORMAT(' Electrod Length = ', F8.4,/ ' Applied Potential = ',F8.4)
		print*, '_________________________________'
	endif
	
end subroutine read_globals


END MODULE VARIABLES
