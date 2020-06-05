.SUFFIXES: .f90 .o
.SECONDARY: $(OBJU)

.PHONY: all

STEMU = utility
SRCU  = ${STEMU:=.f90}
OBJU  = ${STEMU:=.o}

STEMP = saxpy_parametered_openmp 
SRCP  = ${STEMP:=.f90}
OBJP  = ${STEMP:=.o}
EXEP  = ${STEMP} # saxpy parametered_openmp

STEMV = saxpy_variabled_openmp 
SRCV  = ${STEMV:=.f90}
OBJV  = ${STEMV:=.o}
EXEV  = ${STEMV} # saxpy_variabled_openmp

F90 = gfortran

RPT = #-qopt-report=3 -qopt-report-file=$@.optrpt

OPT = -O3 -g -fopenmp #-xHost -fcode-asm -Fa$@.s #-check bounds

.f90.o:
	${F90} ${OPT} ${RPT} -c $< -o $@


all: ${EXEV} # ${EXEP}

${OBJP} : ${OBJU}

${OBJV} : ${OBJU}

${EXEP} : ${OBJP}
	@echo building ${EXEP}:
	${F90} ${OPT} ${RPT} -o $@ ${OBJP} ${OBJU}

${EXEV} : ${OBJV}
	@echo building ${EXEV}:
	${F90} ${OPT} ${RPT} -o $@ ${OBJV} ${OBJU}

clean:
	rm -f ${EXEV}  ${EXEP} *.o *.mod *.optrpt *.s


