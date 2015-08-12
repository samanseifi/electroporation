F90 = /usr/bin/gfortran-4.6
F77 = /usr/bin/f77
FFLAGS =  -lfftw3
########################################################################
MOD_OBJ =\
          variables_mod.o util_mod.o solver_mod.o electricfield_mod.o

F90_OBJS = manager.o
##################################################################### Libraries

ALL_OBJS = $(MOD_OBJ) $(F90_OBJS) 

NDS_3D: $(ALL_OBJS) 
	$(F90) $(ALL_OBJS)   -o modelA $(FFLAGS)

clean:  
	rm *.mod *.o *~ signal_* *.png *.jpg *.csv modelA

clean2:
	rm out_* 
##################################################################### Rules
variables_mod.o        : variables_mod.f90
		         $(F90) -c variables_mod.f90 $(FFLAGS)
util_mod.o             :variables_mod.o util_mod.f90
		         $(F90) -c util_mod.f90 $(FFLAGS)
electricfield_mod.o	   :variables_mod.o util_mod.o electricfield_mod.f90
		         $(F90) -c electricfield_mod.f90 $(FFLAGS)
solver_mod.o           :variables_mod.o util_mod.o electricfield_mod.o solver_mod.f90
		         $(F90) -c solver_mod.f90 $(FFLAGS)
manager.o 	       : manager.f90 variables_mod.o util_mod.o solver_mod.o electricfield_mod.o
		         $(F90) -c manager.f90 $(FFLAGS)

######################################################################cleanup 




