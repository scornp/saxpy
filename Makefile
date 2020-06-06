.SUFFIXES: .f90 .o
.SECONDARY: $(OBJU)

.PHONY: all

STEMU = utility
SRCU  = ${STEMU:=.f90}
OBJU  = ${STEMU:=.o}

STEMB = saxpy_baseline
SRCB  = ${STEMB:=.f90}
OBJB  = ${STEMB:=.o}
EXEB  = ${STEMB} # saxpy_baseline_openmp

STEMV = saxpy_variabled_openmp 
SRCV  = ${STEMV:=.f90}
OBJV  = ${STEMV:=.o}
EXEV  = ${STEMV} # saxpy_variabled_openmp

#F90 = gfortran
F90 = tau_f90.sh

RPT = #-qopt-report=3 -qopt-report-file=$@.optrpt

OPT =  -O3 -fopenmp #-xHost -fcode-asm -Fa$@.s #-check bounds

.f90.o:
	${F90} ${OPT} ${RPT} -c $< -o $@


all: ${EXEV} ${EXEB}

${OBJB} : ${OBJU}

${OBJV} : ${OBJU}

${EXEB} : ${OBJB}
	@echo building ${EXEB}:
	${F90} ${OPT} ${RPT} -o $@ ${OBJB} ${OBJU}

${EXEV} : ${OBJV}
	@echo building ${EXEV}:
	${F90} ${OPT} ${RPT} -o $@ ${OBJV} ${OBJU}

clean:
	rm -f ${EXEV}  ${EXEB} *.o *.mod *.optrpt *.s


