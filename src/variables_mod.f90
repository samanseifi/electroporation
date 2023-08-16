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
real*8				  :: lambda_in, lambda_ex, lambda
real*8			      :: Cm, C_LW, C_0, Gm
real*8				  :: L, c1
real*8				  :: Vm, U0
real*8				  :: sigma_elec
real*8				  :: Kw, Km
real*8				  :: epsilon_0 		!permitivitty constant
real*8				  :: h      		!lipid bilayer thickness

CONTAINS

subroutine read_globals

	namelist /input_params/ dx, dt, epsilon, sigma, gamma, file_skip, tmax, Nx, Ny, initfile, initname, elecfield
	!space step
	!small length scale
	!surface tension
	!line tension coeff.
	!how often to print
	!how many total time steps to take
	!system size in the x direction
	!system size in the y direction
	!check if it reads the initial condition from a data file (= "yes" or "no")
	!determining the initial condition
	!switch to turn on/off the electric field (= "on" or "off")

	namelist /input_elec/ lambda_in, lambda_ex, Km, Kw, Cm, Gm, L, U0
	!conductivity of interior fluid
	!conductivity of exterior fluid
	!capacitance of the lipid membrane
	!conductance of the lipid membrane
	!membrane to electrodes length
	!applied potential

	INTEGER 		  :: num_lines, ios, k, u
	character(len=1)  :: junk

	character(1024) :: input_dir

	call get_command_argument(1, input_dir, status=u)
	if (u/=0) error stop "please specify input directory like:  ./inputs/"

	! read initial parameters from file
	open(newunit=u,file=trim(input_dir) // '/input.nml', status='old')
	read(u, nml=input_params)

	if (elecfield == 'on') then
		read(u, nml=input_elec)
	endif
	close(u)

	!initialize specific parameters
	PSI=0.0d0

	W2 = (epsilon*gamma)

	epsilon_0 = 8.8542e-12

	h = 77.99 ! Non-dimensionalized (actual value = 5.0e-9)

	C_0 = (Km*epsilon_0)/(5.0e-9) ! h = 5.e-9 m

	!C_LW = (Kw/Km - 1)*Cm
	C_LW = (Kw/Km - 1.0)*C_0

	lambda = lambda_in

	M=1.0d0

	dx2_in = 1 / dx**2

	!test that system sizes do not exceed declared dimensions (avoid this using dynamic allocation)
	if(Nx+1.gt.Nmax.or.Ny+1.gt.Nmax)then
		error stop 'One of the system dimensions exceeds array dimensions'
	endif

	!teset if the system size given in initial file is the same as it defined in Nx and Ny
	num_lines = 0
	if (initfile =='yes') then
		open(newunit=u, file=initname, status='unknown')
		do k=1,1000000
			read(u,*, IOSTAT=ios) junk
			if (ios /= 0) exit
			if (k == 1000000) error stop 'Error: Maximum number of records exceeded'
			num_lines = num_lines + 1 !number of lines in the file
		enddo
		close(u)
		if (mod(num_lines, Nx*Ny) /= 0) then
			error stop 'Error: The system size of the initial file does not maatch with your domain size Nx and Ny'
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
		print 40, Gm, L, U0
40		FORMAT(' Lipid Conductance = ', F8.4,/ ' Electrod Length = ', F8.4,/ ' Applied Potential = ',F8.4)
		print*, '_________________________________'
	endif

end subroutine read_globals


END MODULE VARIABLES
