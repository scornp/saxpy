.SUFFIXES: .f90 .o
.SECONDARY: $(UTLOBJH)

STEM = utility #omp_sub
STEM+= saxpy_parametered
SRC = ${STEM:=.f90}
OBJ = ${STEM:=.o}
EXE = fred
F90 = gfortran

RPT = #-qopt-report=3 -qopt-report-file=$@.optrpt

OPT = -O3 -g -fopenmp #-xHost -fcode-asm -Fa$@.s #-check bounds

.f90.o:
	${F90} ${OPT} ${RPT} -c $< -o $@

${EXE} : ${OBJ}
	${F90} ${OPT} ${RPT} -o $@ ${OBJ}

default: ${EXE} 
	@echo default: ${EXE}

clean:
	rm -f ${EXE} *.o *.mod *.optrpt *.s


