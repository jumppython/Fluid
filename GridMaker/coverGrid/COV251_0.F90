!
!	GRID251.F90
!	
!
!	Created by allen tang on 12/12/10.
!	Copyright 2012 __MyCompanyName__. All rights reserved.
!
!   CAVITY 251X251
!
PROGRAM MAKE_GRID_WITH_251X251
!
  IMPLICIT NONE
!
  INTEGER, PARAMETER :: INX=255
!
  REAL, DIMENSION (INX,INX) :: X,Y,COV
!
  CHARACTER(LEN=32) :: ARG
!   OUTPUT TYPE : MTYPE = 1
  INTEGER :: MTYPE=1
!
  INTEGER :: MZAHYO=0
!
! COV_TYPE : 0 -- FOR RE
!            1 -- FOR EPSILON
!            2 -- FOR R
  INTEGER :: COV_TYPE=0
!
  INTEGER I,J
  INTEGER :: IMAX=251
  INTEGER :: JMAX=251
  INTEGER :: NCEN=252/2
  INTEGER :: MCEN=252/2
!
  REAL CNT
  REAL :: AA=0.1 / SQRT(100000.0)
  REAL :: BB=1.032052
  REAL :: R05=0.5
  REAL :: R10=1.0
  REAL :: CIMIN,CIMAX,CJMIN,CJMAX
!
  CALL GET_COMMAND_ARGUMENT(1, ARG)
  IF (LEN_TRIM(ARG)==0) THEN
    WRITE(6,*) "THERE IS NO ARGUMENT"
    STOP 40
  END IF
  IF(TRIM(ARG)=="A1") THEN
    CIMIN=0.0
	CIMAX=0.2
	CJMIN=0.0
	CJMAX=0.2
  ELSE IF(TRIM(ARG)=="A2") THEN
    CIMIN=0.2
	CIMAX=0.4
	CJMIN=0.0
	CJMAX=0.2
  ELSE IF(TRIM(ARG)=="A3") THEN
    CIMIN=0.4
	CIMAX=0.6
	CJMIN=0.0
	CJMAX=0.2
  ELSE IF(TRIM(ARG)=="A4") THEN
    CIMIN=0.6
	CIMAX=0.8
	CJMIN=0.0
	CJMAX=0.2
  ELSE IF(TRIM(ARG)=="A5") THEN
    CIMIN=0.8
	CIMAX=1.0
	CJMIN=0.0
	CJMAX=0.2
  ELSE IF(TRIM(ARG)=="B1") THEN
    CIMIN=0.0
	CIMAX=0.2
	CJMIN=0.2
	CJMAX=0.4
  ELSE IF(TRIM(ARG)=="B2") THEN
    CIMIN=0.2
	CIMAX=0.4
	CJMIN=0.2
	CJMAX=0.4
  ELSE IF(TRIM(ARG)=="B3") THEN
    CIMIN=0.4
	CIMAX=0.6
	CJMIN=0.2
	CJMAX=0.4
  ELSE IF(TRIM(ARG)=="B4") THEN
    CIMIN=0.6
	CIMAX=0.8
	CJMIN=0.2
	CJMAX=0.4
  ELSE IF(TRIM(ARG)=="B5") THEN
    CIMIN=0.8
	CIMAX=1.0
	CJMIN=0.2
	CJMAX=0.4
  ELSE IF(TRIM(ARG)=="C1") THEN
    CIMIN=0.0
	CIMAX=0.2
	CJMIN=0.4
	CJMAX=0.6
  ELSE IF(TRIM(ARG)=="C2") THEN
    CIMIN=0.2
	CIMAX=0.4
	CJMIN=0.4
	CJMAX=0.6
  ELSE IF(TRIM(ARG)=="C3") THEN
    CIMIN=0.4
	CIMAX=0.6
	CJMIN=0.4
	CJMAX=0.6
  ELSE IF(TRIM(ARG)=="C4") THEN
    CIMIN=0.6
	CIMAX=0.8
	CJMIN=0.4
	CJMAX=0.6
  ELSE IF(TRIM(ARG)=="C5") THEN
    CIMIN=0.8
	CIMAX=1.0
	CJMIN=0.4
	CJMAX=0.6
  ELSE IF(TRIM(ARG)=="D1") THEN
    CIMIN=0.0
	CIMAX=0.2
	CJMIN=0.6
	CJMAX=0.8
  ELSE IF(TRIM(ARG)=="D2") THEN
    CIMIN=0.2
	CIMAX=0.4
	CJMIN=0.6
	CJMAX=0.8
  ELSE IF(TRIM(ARG)=="D3") THEN
    CIMIN=0.4
	CIMAX=0.6
	CJMIN=0.6
	CJMAX=0.8
  ELSE IF(TRIM(ARG)=="D4") THEN
    CIMIN=0.6
	CIMAX=0.8
	CJMIN=0.6
	CJMAX=0.8
  ELSE IF(TRIM(ARG)=="D5") THEN
    CIMIN=0.8
	CIMAX=1.0
	CJMIN=0.6
	CJMAX=0.8
  ELSE IF(TRIM(ARG)=="E1") THEN
    CIMIN=0.0
	CIMAX=0.2
	CJMIN=0.8
	CJMAX=1.0
  ELSE IF(TRIM(ARG)=="E2") THEN
    CIMIN=0.2
	CIMAX=0.4
	CJMIN=0.8
	CJMAX=1.0
  ELSE IF(TRIM(ARG)=="E3") THEN
    CIMIN=0.4
	CIMAX=0.6
	CJMIN=0.8
	CJMAX=1.0
  ELSE IF(TRIM(ARG)=="E4") THEN
    CIMIN=0.6
	CIMAX=0.8
	CJMIN=0.8
	CJMAX=1.0
  ELSE IF(TRIM(ARG)=="E5") THEN
    CIMIN=0.8
	CIMAX=1.0
	CJMIN=0.8
	CJMAX=1.0
  ELSE
    WRITE(6,*) "ARGUMENT IS WRONG! CHECK IT AGAIN"
	STOP 41
  END IF  
!   GRID GENERATION
  DO I=1,IMAX
    DO J=1,JMAX
	  IF(I.EQ.1) THEN
	    X(I,J)=0.0
	  ELSE IF(I.LT.NCEN) THEN
	    X(I,J)=X(I-1,J)+AA*BB**REAL(I-2)
	  ELSE IF(I.EQ.NCEN) THEN
	    X(I,J)=R05
	  ELSE IF(I.LE.IMAX) THEN
	    X(I,J)=R10-X(NCEN*2-I,J)
	  END IF
	END DO
  END DO
  WRITE(6,*) "X END!"
  DO J=1,JMAX
    DO I=1,IMAX
	  IF(J.EQ.1) THEN
	    Y(I,J)=0.0
	  ELSE IF(J.LT.MCEN) THEN
	    Y(I,J)=Y(I,J-1)+AA*BB**REAL(J-2)
	  ELSE IF(J.EQ.MCEN) THEN
	    Y(I,J)=R05
	  ELSE IF(J.LE.JMAX) THEN
	    Y(I,J)=R10-Y(I,MCEN*2-J)
	  END IF
	END DO
  END DO
  WRITE(6,*) "Y END !"
!
  IF(COV_TYPE==0) THEN
    DO J=1,JMAX
      DO I=1,IMAX
	    IF(Y(I,J).GE.CJMIN.AND.Y(I,J).LT.CJMAX.AND.X(I,J).GE.CIMIN.AND.X(I,J).LT.CIMAX) THEN
	      COV(I,J)=98.0
	    ELSE
	      COV(I,J)=1.0
	    END IF
	  END DO
    END DO
  ELSE IF(COV_TYPE==1) THEN
    DO J=1,JMAX
      DO I=1,IMAX
	    IF(Y(I,J).GE.CJMIN.AND.Y(I,J).LT.CJMAX.AND.X(I,J).GE.CIMIN.AND.X(I,J).LT.CIMAX) THEN
	      COV(I,J)=0.6
	    ELSE
	      COV(I,J)=1.0
	    END IF
	  END DO
    END DO
  ELSE IF(COV_TYPE==2) THEN
    DO J=1,JMAX
      DO I=1,IMAX
	    IF(Y(I,J).GE.CJMIN.AND.Y(I,J).LT.CJMAX.AND.X(I,J).GE.CIMIN.AND.X(I,J).LT.CIMAX) THEN
	      COV(I,J)=1.2
	    ELSE
	      COV(I,J)=1.0
	    END IF
	  END DO
    END DO
  END IF
  OPEN(UNIT=800,FILE="COV.GR",FORM="UNFORMATTED")
  WRITE(800) ((COV(I,J),I=1,IMAX),J=1,JMAX)
  CLOSE(800)
  OPEN(UNIT=900,FILE="COV.GD")
  DO J=1,JMAX
    WRITE(900, " ")
    DO I=1,IMAX
	  WRITE(900,"(F18.8,' ',F18.8,' ',F18.8)") X(I,J),Y(I,J),COV(I,J)
	END DO
  END DO
  CLOSE(900)
  WRITE(6,*) "COV WAS DONE"
!
  STOP 1000
!
END PROGRAM MAKE_GRID_WITH_251X251



