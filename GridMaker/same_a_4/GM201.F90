!
!	GRID251.F90
!	
!
!	Created by allen tang on 12/12/10.
!	Copyright 2012 __MyCompanyName__. All rights reserved.
!
!   CAVITY 251X251
!
PROGRAM MAKE_GRID_WITH_201X201
!
  IMPLICIT NONE
!
  INTEGER, PARAMETER :: INX=205
!
  REAL, DIMENSION (INX,INX) :: X,Y
!
!   OUTPUT TYPE : MTYPE = 1
  INTEGER :: MTYPE=1
!
  INTEGER :: MZAHYO=0
!
  INTEGER I,J,MK,NK
  INTEGER :: IMAX=201
  INTEGER :: JMAX=201
  INTEGER :: NCEN=202/2
  INTEGER :: MCEN=202/2
!  
  INTEGER, DIMENSION (15,2) :: A1XY,A2XY,A3XY,A4XY
!
  REAL CNT
  REAL :: AA=0.0004
  REAL :: BB=1.040161
  REAL :: R05=0.5
  REAL :: R10=1.0
!
  A1XY(1,1)=78
  A1XY(1,2)=47
  A1XY(2,1)=95
  A1XY(2,2)=47
  A1XY(3,1)=107
  A1XY(3,2)=47
  A1XY(4,1)=124
  A1XY(4,2)=47
  A1XY(5,1)=78
  A1XY(5,2)=62
  A1XY(6,1)=95
  A1XY(6,2)=62
  A1XY(7,1)=107
  A1XY(7,2)=62
  A1XY(8,1)=124
  A1XY(8,2)=62
  A1XY(9,1)=78
  A1XY(9,2)=72
  A1XY(10,1)=95
  A1XY(10,2)=72
  A1XY(11,1)=107
  A1XY(11,2)=72
  A1XY(12,1)=124
  A1XY(12,2)=72
  A1XY(13,1)=78
  A1XY(13,2)=78
  A1XY(14,1)=95
  A1XY(14,2)=78
  A1XY(15,1)=107
  A1XY(15,2)=78
!
  A2XY(1,1)=47
  A2XY(1,2)=78
  A2XY(2,1)=62
  A2XY(2,2)=78
  A2XY(3,1)=72
  A2XY(3,2)=78
  A2XY(4,1)=47
  A2XY(4,2)=95
  A2XY(5,1)=62
  A2XY(5,2)=95
  A2XY(6,1)=72
  A2XY(6,2)=95
  A2XY(7,1)=78
  A2XY(7,2)=95
  A2XY(8,1)=47
  A2XY(8,2)=107
  A2XY(9,1)=62
  A2XY(9,2)=107
  A2XY(10,1)=72
  A2XY(10,2)=107
  A2XY(11,1)=78
  A2XY(11,2)=107
  A2XY(12,1)=47
  A2XY(12,2)=124
  A2XY(13,1)=62
  A2XY(13,2)=124
  A2XY(14,1)=72
  A2XY(14,2)=124
  A2XY(15,1)=78
  A2XY(15,2)=124
!
  A3XY(1,1)=95
  A3XY(1,2)=124
  A3XY(2,1)=107
  A3XY(2,2)=124
  A3XY(3,1)=124
  A3XY(3,2)=124
  A3XY(4,1)=78
  A3XY(4,2)=130
  A3XY(5,1)=95
  A3XY(5,2)=130
  A3XY(6,1)=107
  A3XY(6,2)=130
  A3XY(7,1)=124
  A3XY(7,2)=130
  A3XY(8,1)=78
  A3XY(8,2)=140
  A3XY(9,1)=95
  A3XY(9,2)=140
  A3XY(10,1)=107
  A3XY(10,2)=140
  A3XY(11,1)=124
  A3XY(11,2)=140
  A3XY(12,1)=78
  A3XY(12,2)=155
  A3XY(13,1)=95
  A3XY(13,2)=155
  A3XY(14,1)=107
  A3XY(14,2)=155
  A3XY(15,1)=124
  A3XY(15,2)=155
!
  A4XY(1,1)=124
  A4XY(1,2)=78
  A4XY(2,1)=130
  A4XY(2,2)=78
  A4XY(3,1)=140
  A4XY(3,2)=78
  A4XY(4,1)=155
  A4XY(4,2)=78
  A4XY(5,1)=124
  A4XY(5,2)=95
  A4XY(6,1)=130
  A4XY(6,2)=95
  A4XY(7,1)=140
  A4XY(7,2)=95
  A4XY(8,1)=155
  A4XY(8,2)=95
  A4XY(9,1)=124
  A4XY(9,2)=107
  A4XY(10,1)=130
  A4XY(10,2)=107
  A4XY(11,1)=140
  A4XY(11,2)=107
  A4XY(12,1)=155
  A4XY(12,2)=107
  A4XY(13,1)=130
  A4XY(13,2)=124
  A4XY(14,1)=140
  A4XY(14,2)=124
  A4XY(15,1)=155
  A4XY(15,2)=124
!
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
  IF(MTYPE.EQ.0) THEN
    OPEN(UNIT=100,FILE="YOKOJIKU.DAT")
	DO J=1,JMAX
	  DO I=1,IMAX
	    IF(MZAHYO.EQ.1) THEN
		  WRITE(100,*) "(",I,",",J,")=",X(I,J),Y(I,J)
		ELSE
		  WRITE(100,*) X(I,J),Y(I,J)
		END IF
	  END DO
	  WRITE(100,"(1H )")
	END DO
	CLOSE(100)
	OPEN(UNIT=200,FILE="TATEJIKU")
	DO I=1,IMAX
	  DO J=1,JMAX
	    IF(MZAHYO.EQ.1) THEN
		  WRITE(200,*) "(",I,",",J,")=",X(I,J),Y(I,J)
	    ELSE
		  WRITE(200,*) X(I,J),Y(I,J)
		END IF
	  END DO
	  WRITE(200,"(1H )")
	END DO
	CLOSE(200)
	WRITE(6,*) "GRID DATA IS DEFINED."
  ELSE IF(MTYPE.EQ.1) THEN
    OPEN(UNIT=500,FILE="FORT.GR",FORM="UNFORMATTED")
	WRITE(500) IMAX,JMAX
	WRITE(500) ((X(I,J),I=1,IMAX),J=1,JMAX)
	WRITE(500) ((Y(I,J),I=1,IMAX),J=1,JMAX)
	CLOSE(500)
	OPEN(UNIT=700,FILE="FORT.GD")
	DO I=1,IMAX
	  CNT=0.0
	  WRITE(700," ")
	  DO J=1,JMAX
	    CNT=CNT+1.0
		WRITE(700,"(F18.8,' ',F18.8,' ',F18.8)") CNT,X(I,J),Y(I,J)
	  END DO
	END DO
	CLOSE(700)
	WRITE(6,*) "GRID DATA WAS DONE."
  END IF
!
  OPEN(UNIT=800,FILE="SP.DAT")
  DO I=1,15
    MK=A1XY(I,1)
	NK=A1XY(I,2)
	WRITE(800,"(F18.8,' ',F18.8)") X(MK,NK),Y(MK,NK)
	WRITE(800," ")
    MK=A2XY(I,1)
	NK=A2XY(I,2)
	WRITE(800,"(F18.8,' ',F18.8)") X(MK,NK),Y(MK,NK)
	WRITE(800," ")
    MK=A3XY(I,1)
	NK=A3XY(I,2)
	WRITE(800,"(F18.8,' ',F18.8)") X(MK,NK),Y(MK,NK)
	WRITE(800," ")
    MK=A4XY(I,1)
	NK=A4XY(I,2)
	WRITE(800,"(F18.8,' ',F18.8)") X(MK,NK),Y(MK,NK)
	WRITE(800," ")
  END DO
  CLOSE(800)
!
  STOP 1000
!
END PROGRAM MAKE_GRID_WITH_201X201



