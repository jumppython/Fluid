PROGRAM CAVITY_1X1_MAC_METHOD_CDF
!  
  IMPLICIT NONE
!  
  INTEGER, PARAMETER :: INX=255
  INTEGER, PARAMETER :: INXX=INX*INX
!
  CHARACTER (LEN=12) :: inte
  CHARACTER (LEN=8) :: str
!
  INTEGER :: NN,I,J,K,KK
  INTEGER, DIMENSION (15,2) :: A1XY,A2XY,A3XY,A4XY
!
  REAL :: RDM,NSUM,CNT,AJJ,DUDY,DVDX,TPMAX,TPMIN,PMAX,PMIN,TIME
  REAL :: ZANSA,UXI,VXI,UETA,VETA,WXI,AJAC,XXI,YXI,XETA,YETA,ALPH,BETA,GAMA,DDD,QIJ
  REAL :: XXX,XXE,XEE,YXX,YXE,YEE,WXJ,WEJ
  REAL :: GOSA,P0,SS,U0,V0,BJ1,BJ2,B1,B2,TTT,B3
  REAL :: GOSAQ,P1,P2,P3,P4,U1,U2,U3,U4,U5,U6,U7,U8,V1,V2,V3,V4,V5,V6,V7,V8,QETA,B4,S1,SS1,S2,SS2
  REAL :: PL,PLD,PLU,PF,PR,PRD,PRU
  REAL :: AVGU, AVGV,AVGP,AVG1U,AVG1V,AVG1P,AVG2U,AVG2V,AVG2P,AVG3U,AVG3V,AVG3P,AVG4U,AVG4V,AVG4P
  REAL, DIMENSION (INX) :: TB,TO,TX,TY,WORK
  REAL, DIMENSION (INX, INX) :: X, Y, X0,Y0,WX,WE,AL,BE,GA,AJ,XX,YX,XE,YE,D,Q,Z,BB1,BB2,BB3
  REAL, DIMENSION (0:INX, 0:INX) :: P,U,V,PDIFF
  REAL, DIMENSION (3, 15) :: AREA1,AREA2,AREA3,AREA4
  REAL :: OMGZ(300,300),AA(2,2),DZ(2,2)
!	 
  CHARACTER (LEN=4) :: NUM
  CHARACTER (LEN=10) :: INDX
! grid data reading
  INTEGER :: JGR = 1
  INTEGER :: MPRINT = 1000
  INTEGER :: MWRITE = 100
  INTEGER :: VWRITE = 100
  INTEGER :: IPVOUT = 0
!  
  INTEGER :: MSLICE=1000
  INTEGER :: NWRITED=0
  REAL :: RT2I=0.707107
!  
  INTEGER :: IOPEN=2000
  INTEGER :: ICOUNT=1
  INTEGER :: LOOP=0
!
  INTEGER :: N1999=6249999
  INTEGER :: N2000=6250000
  INTEGER :: N2001=6312500
  INTEGER :: N2050=7500000
  INTEGER :: N2051=7562500
  INTEGER :: N2100=8750000
  INTEGER :: N2101=8812500
  INTEGER :: N2150=10000000
  INTEGER :: N2151=10062500
  INTEGER :: N2200=11250000
  INTEGER :: N2201=11312500
  INTEGER :: N2250=12500000
  INTEGER :: N2251=12562500
  INTEGER :: N2300=13750000
  INTEGER :: N2301=13812500
  INTEGER :: N2350=15000000
  INTEGER :: N2351=15062500
  INTEGER :: N2480=15312500
  INTEGER :: N2490=15624990
  INTEGER :: N2500=15625000
  INTEGER :: NN0=1
!  
  REAL :: RTIME=0.0
  REAL :: PRMT=0.0
  REAL :: PFMT=0.0
  REAL :: PLMT=0.0
!	  
  REAL :: PI=4.*ATAN(1.)
!
  REAL :: RE = 8000.
  INTEGER :: MX = 251
  INTEGER :: MY = 251
!
! case of Re=1.0x10^3
! DT=0.05 -> NG!
! DT=0.04   : OK(till 374 step)
! case of Re=3.0x10^4
! DT=0.01   NG!
! DT=0.0075 NG!
! DT=0.007  NG!
! DT=0.0065 NG!
! DT=0.006 OK! (till 334step)
! DT=0.005 OK!(till ?step)
! DT=0.003  izen saiyou
! DT=0.001 OK!(till 122step)
! case of Re=1.0x10^5
! DT=0.01:INF!
! DT=0.003:NG!
! DT=0.002:OK(till 35 step)
! DT=0.001000

!
  REAL :: DT = 0.0008 
  INTEGER :: NLAST = 25000000
  INTEGER :: NJ = 1
  INTEGER :: ILAST3 = 1000
  INTEGER :: ILAST4 = 1000 
  REAL :: EPS2 = 1.0E-6                                                   
  REAL :: EPS3 = 1.0E-7
  REAL :: CONST2 = 1.0
  REAL :: CONST3 = 1.0
  REAL :: REI,DTI,UYT
!
  REI = 1.0 / RE
  DTI = 1.0 / DT
  UYT = 1.0
!
  AVGU=0.0
  AVGV=0.0
  AVGP=0.0
  AVG1U=0.0
  AVG1V=0.0
  AVG1P=0.0
  AVG2U=0.0
  AVG2V=0.0
  AVG2P=0.0
  AVG3U=0.0
  AVG3V=0.0
  AVG3P=0.0
  AVG4U=0.0
  AVG4V=0.0
  AVG4P=0.0
!
  A1XY(1,1)=98
  A1XY(1,2)=58
  A1XY(2,1)=119
  A1XY(2,2)=58
  A1XY(3,1)=133
  A1XY(3,2)=58
  A1XY(4,1)=154
  A1XY(4,2)=58
  A1XY(5,1)=98
  A1XY(5,2)=77
  A1XY(6,1)=119
  A1XY(6,2)=77
  A1XY(7,1)=133
  A1XY(7,2)=77
  A1XY(8,1)=154
  A1XY(8,2)=77
  A1XY(9,1)=98
  A1XY(9,2)=89
  A1XY(10,1)=119
  A1XY(10,2)=89
  A1XY(11,1)=133
  A1XY(11,2)=89
  A1XY(12,1)=154
  A1XY(12,2)=89
  A1XY(13,1)=98
  A1XY(13,2)=98
  A1XY(14,1)=119
  A1XY(14,2)=98
  A1XY(15,1)=133
  A1XY(15,2)=98
!
  A2XY(1,1)=58
  A2XY(1,2)=98
  A2XY(2,1)=77
  A2XY(2,2)=98
  A2XY(3,1)=89
  A2XY(3,2)=98
  A2XY(4,1)=58
  A2XY(4,2)=119
  A2XY(5,1)=77
  A2XY(5,2)=119
  A2XY(6,1)=89
  A2XY(6,2)=119
  A2XY(7,1)=98
  A2XY(7,2)=119
  A2XY(8,1)=58
  A2XY(8,2)=133
  A2XY(9,1)=77
  A2XY(9,2)=133
  A2XY(10,1)=89
  A2XY(10,2)=133
  A2XY(11,1)=98
  A2XY(11,2)=133
  A2XY(12,1)=58
  A2XY(12,2)=154
  A2XY(13,1)=77
  A2XY(13,2)=154
  A2XY(14,1)=89
  A2XY(14,2)=154
  A2XY(15,1)=98
  A2XY(15,2)=154
!
  A3XY(1,1)=119
  A3XY(1,2)=154
  A3XY(2,1)=133
  A3XY(2,2)=154
  A3XY(3,1)=154
  A3XY(3,2)=154
  A3XY(4,1)=98
  A3XY(4,2)=163
  A3XY(5,1)=119
  A3XY(5,2)=163
  A3XY(6,1)=133
  A3XY(6,2)=163
  A3XY(7,1)=154
  A3XY(7,2)=163
  A3XY(8,1)=98
  A3XY(8,2)=175
  A3XY(9,1)=119
  A3XY(9,2)=175
  A3XY(10,1)=133
  A3XY(10,2)=175
  A3XY(11,1)=154
  A3XY(11,2)=175
  A3XY(12,1)=98
  A3XY(12,2)=194
  A3XY(13,1)=119
  A3XY(13,2)=194
  A3XY(14,1)=133
  A3XY(14,2)=194
  A3XY(15,1)=154
  A3XY(15,2)=194
!
  A4XY(1,1)=154
  A4XY(1,2)=98
  A4XY(2,1)=163
  A4XY(2,2)=98
  A4XY(3,1)=175
  A4XY(3,2)=98
  A4XY(4,1)=194
  A4XY(4,2)=98
  A4XY(5,1)=154
  A4XY(5,2)=119
  A4XY(6,1)=163
  A4XY(6,2)=119
  A4XY(7,1)=175
  A4XY(7,2)=119
  A4XY(8,1)=194
  A4XY(8,2)=119
  A4XY(9,1)=154
  A4XY(9,2)=133
  A4XY(10,1)=163
  A4XY(10,2)=133
  A4XY(11,1)=175
  A4XY(11,2)=133
  A4XY(12,1)=194
  A4XY(12,2)=133
  A4XY(13,1)=163
  A4XY(13,2)=154
  A4XY(14,1)=175
  A4XY(14,2)=154
  A4XY(15,1)=194
  A4XY(15,2)=154
!
  OPEN (UNIT=8,FILE='kekka.dat')
  OPEN (UNIT=9,FILE='input.dat')
  IF (IPVOUT .EQ. 1) THEN
    OPEN (UNIT=12,FILE='fort.pv',FORM='unformatted')
  END IF
!
  WRITE (9, *) 'NLAST,DT,RE,LM,KM,MM,meshpoints'
  WRITE (6, *) 'NLAST,DT,RE,LM,KM,MM,meshpoints'
  WRITE (9, *) NLAST,DT,RE,NLAST,ILAST3,ILAST4,MX,' x',MY
  WRITE (6, *) NLAST,DT,RE,NLAST,ILAST3,ILAST4,MX,' x',MY
  CLOSE (9)
  IF (JGR .EQ. 1) THEN
	OPEN (UNIT=11,FILE='FORT.GR',FORM='unformatted')
    READ (11) MX,MY
    READ (11) ((X(J,K),J=1,MX),K=1,MY)
    READ (11) ((Y(J,K),J=1,MX),K=1,MY)
    CLOSE (11)
  ELSE
    WRITE(6, *) 'READ fort.gr!.'
    STOP 10
  END IF
  WRITE(8, *)  'NN,K,GOSA,KK,GOSAQ'
  WRITE(6, *) 'NN,K,GOSA'
!
  DO J=1,MX
	DO K=1,MY
	  X0(J,K) = X(J,K)
	  Y0(J,K) = Y(J,K)
    END DO
  END DO
!
!                                                                       
!*************************************************************          
!*                  SOLVING FLOW EQUATION                    *          
!*************************************************************          
!                                                                       
!******CALCULATION OF COEFFICIENTS OF TRANSFORMED EQUATION              
!                                                                       
!*** for wall-pdiff dx,dy
  DO I = NJ, MX
    IF(I .EQ. MX) THEN
      TX(I) = 0.5 * ABS (X(I-1,1) - X(I,1))
    ELSE
      TX(I) = 0.5 * (X(I+1,1) - X(I,1))
    END IF
  END DO
  DO J=1, MY
    IF(J .EQ. MY) THEN
	  TY(J) = 0.5 * ABS (Y(NJ,J-1) - Y(NJ,J))
    ELSE
      TY(J) = 0.5 * (Y(NJ,J+1)-Y(NJ,J))
    END IF
  END DO
  DO J=2, MY-1
    DO I=2, MX-1
	  XX(I,J)=0.5*(X(I+1,J)-X(I-1,J))
	  YX(I,J)=0.5*(Y(I+1,J)-Y(I-1,J))
	END DO
  END DO                                                          
  DO J=2, MY-1                                                   
    DO I=2, MX-1                                                     
	  XE(I,J)=0.5*(X(I,J+1)-X(I,J-1))
      YE(I,J)=0.5*(Y(I,J+1)-Y(I,J-1))
    END DO
  END DO
!
  DO J=2, MY-1
	DO I=2, MX-1
	  AL(I,J)=XE(I,J)*XE(I,J)+YE(I,J)*YE(I,J)                         
      BE(I,J)=XX(I,J)*XE(I,J)+YX(I,J)*YE(I,J)                         
      GA(I,J)=XX(I,J)*XX(I,J)+YX(I,J)*YX(I,J)                         
      AJ(I,J)=XX(I,J)*YE(I,J)-XE(I,J)*YX(I,J)                         
    END DO
  END DO                                                          
  DO J=2, MY-1                                                   
    DO I=2, MX-1                                                   
      XXX=X(I+1,J)-2.0*X(I,J)+X(I-1,J)                                  
      XXE=0.25*(X(I+1,J+1)-X(I-1,J+1)-X(I+1,J-1)+X(I-1,J-1))            
      XEE=X(I,J+1)-2.0*X(I,J)+X(I,J-1)                                  
      YXX=Y(I+1,J)-2.0*Y(I,J)+Y(I-1,J)                                  
      YXE=0.25*(Y(I+1,J+1)-Y(I-1,J+1)-Y(I+1,J-1)+Y(I-1,J-1))            
      YEE=Y(I,J+1)-2.0*Y(I,J)+Y(I,J-1)                                  
      WXJ=AL(I,J)*YXX-2.0*BE(I,J)*YXE+GA(I,J)*YEE                       
      WEJ=AL(I,J)*XXX-2.0*BE(I,J)*XXE+GA(I,J)*XEE                       
      WX(I,J)=(WXJ*XE(I,J)-WEJ*YE(I,J))/AJ(I,J)**3                      
      WE(I,J)=(WEJ*YX(I,J)-WXJ*XX(I,J))/AJ(I,J)**3                      
    END DO
  END DO                                                          
  DO J=2, MY-1                                                     
    DO I=2, MX-1                                                     
	  Z(I,J)=1.0/(2.0*(AL(I,J)+GA(I,J)))                              
    END DO
  END DO
  DO J=2, MY-1
    DO I=2, MX-1
      X(I,J)=(AL(I,J)+.5*AJ(I,J)**2*WX(I,J))*Z(I,J)                     
      Y(I,J)=(GA(I,J)+.5*AJ(I,J)**2*WE(I,J))*Z(I,J)                     
	END DO
  END DO
  DO J=2, MY-1                                                   
    DO I=2, MX-1                                                     
      BB1(I,J)=-(X(I,J)-2.0*Z(I,J)*AL(I,J))                             
      BB2(I,J)=-(Y(I,J)-2.0*Z(I,J)*GA(I,J))
    END DO
  END DO
  DO J=2, MY-1                                                   
    DO I=2, MX-1                                                   
      D(I,J)=.5*BE(I,J)*Z(I,J)                                          
    END DO
  END DO
!
!*************************************************************                                                                       
!*                   INITIAL CONDITION                       *
!*************************************************************
!
  DO J=1, MY                                                     
    DO I=1, MX                                                     
      U(I,J) = 0.0
      V(I,J) = 0.0
      P(I,J) = 0.0
    END DO
  END DO
!
!*************************************************************
!*                  READ P549 AND UV549                      *
!*************************************************************
!	CALL READ_PUV(P, U, V, MX)
!
!*************************************************************
!*    CALCULATION OF RIGHT HAND SIDE OF POISSON EQUATION     *
!*************************************************************
!
  OPEN (UNIT=600,FILE='UVP2000.DAT')
  OPEN (UNIT=601,FILE='UVP2050.DAT')
  OPEN (UNIT=602,FILE='UVP2100.DAT')
  OPEN (UNIT=603,FILE='UVP2150.DAT')
  OPEN (UNIT=604,FILE='UVP2200.DAT')
  OPEN (UNIT=605,FILE='UVP2250.DAT')
  OPEN (UNIT=606,FILE='UVP2300.DAT')
  OPEN (UNIT=607,FILE='UVP2350.DAT')
!
  OPEN (UNIT=610,FILE='AREA1.DAT')
  OPEN (UNIT=620,FILE='AREA2.DAT')
  OPEN (UNIT=630,FILE='AREA3.DAT')
  OPEN (UNIT=640,FILE='AREA4.DAT')
!
  TPMAX = -1000.0
  TPMIN = 1000.0
!
  TIME = RTIME
!
  CALL RANDOM_SEED(PUT=(/1/))
!
!*************************************************************
!*                       LOOP 1 START                        *
!*************************************************************
!
  DO NN = NN0, NLAST                                                 
    TIME = TIME + DT
!
!	call random_seed()
!	call random_number(RDM)
!
!*************************************************************
!*                  BOUNDARY CONDITION  U, V                 *
!*************************************************************
!                                                       
    DO J=2, MY-1
	  U(1,J) = 0.
	  U(0,J) = 2.0*U(1,J)-U(2,J)
      V(1,J) = 0.
      V(0,J) = 2.0*V(1,J)-V(2,J)
      U(MX,J) = 0.
      U(MX+1,J) = 2.0*U(MX,J)-U(MX-1,J)
      V(MX,J) = 0.
      V(MX+1,J) = 2.0*V(MX,J)-V(MX-1,J)
	END DO
!
    DO J=2, MX-1
      U(J,1) = 0.
      U(J,0) = 2.0*U(J,1)-U(J,2)
      V(J,1) = 0.
      V(J,0) = 2.0*V(J,1)-V(J,2)
	  U(J,MY) = 1.
      U(J,MY+1) = 2.0*U(J,MY)-U(J,MY-1)
      V(J,MY) = 0.
      V(J,MY+1) = 2.0*V(J,MY)-V(J,MY-1)
	END DO
!
    U(1,1) = 0.
    V(1,1) = 0.
    U(1,0) = U(1,1)
    V(1,0) = V(1,1)
    U(0,1) = U(1,1)
    V(0,1) = V(1,1)
!
    U(1,MY) = 1.
    V(1,MY) = 0.
    U(1,MY+1) = U(1,MY)
    V(1,MY+1) = V(1,MY)
    U(0,MY) = U(1,MY)
    V(0,MY) = V(1,MY)
!
    U(MX,1) = 0.
    V(MX,1) = 0.
    U(MX+1,1) = U(MX,1)
	V(MX+1,1) = V(MX,1)
    U(MX,0) = U(MX,1)
    V(MX,0) = V(MX,1)
!
    U(MX,MY) = 1.
    V(MX,MY) = 0.
    U(MX+1,MY) = U(MX,MY)
    V(MX+1,MY) = V(MX,MY)
    U(MX,MY+1) = U(MX,MY)
    V(MX,MY+1) = V(MX,MY)
!
    ZANSA=0.0
    DO J=2, MY-1
      DO I=2, MX-1
        UXI=0.5*(U(I+1,J)-U(I-1,J))
        VXI=0.5*(V(I+1,J)-V(I-1,J))
        UETA=0.5*(U(I,J+1)-U(I,J-1))
        VETA=0.5*(V(I,J+1)-V(I,J-1))
        WXI=WX(I,J)
        AJAC=AJ(I,J)
        XXI=XX(I,J)
        YXI=YX(I,J)
        XETA=XE(I,J)
        YETA=YE(I,J)
        ALPH=AL(I,J)
        BETA=BE(I,J)
        GAMA=GA(I,J)
        DDD=AJAC*(YETA*UXI-YXI*UETA+XXI*VETA-XETA*VXI)
        ZANSA=ZANSA+DDD/AJAC**2
        QIJ=-(YETA*UXI-YXI*UETA)**2-2.0*(XXI*UETA-XETA*UXI)*(YETA*VXI-YXI*VETA)-(XXI*VETA-XETA*VXI)**2+DTI*DDD
        Q(I,J)=QIJ*Z(I,J)
      END DO
	END DO
!
!*************************************************************
!*            SOR ITERATION---POISSON EQUATION               *
!*************************************************************
!
!*************************************************************
!                         LOOP 2 START                       *
!*************************************************************
!
    DO K=1,ILAST4
      GOSA=0.0
!
!*************************************************************
!*                   BOUNDARY CONDITION P                    *
!*************************************************************
!
      DO J=2, MX-1                                                     
        P(J,MY) = P(J,MY-1)
        P(J,1)  = P(J,2)
      END DO
      DO J=2, MY-1                                                     
        P(1,J)=P(2,J)
        P(MX,J)=P(MX-1,J)
      END DO
      P(1,1) = P(1,2)
      P(1,MY) = P(2,MY)
      P(MX,1) = P(MX,2)
      P(MX,MY) = P(MX-1,MY)
!
      DO J=2,MY-1                                                 
        DO I=2,MX-1                                                   
          P0=P(I,J)                                                         
          SS=X(I,J)*P(I+1,J)+BB1(I,J)*P(I-1,J)+Y(I,J)*P(I,J+1)+BB2(I,J)*P(I,J-1)-D(I,J)*(P(I+1,J+1)-P(I-1,J+1)-P(I+1,J-1)+P(I-1,J-1))-Q(I,J)-P0                              
          GOSA=GOSA+SS*SS
          WORK(I)=P0+CONST2*SS
	    END DO
        DO I=2,MX-1                                       
          P(I,J)=WORK(I)                                               
        END DO                                                          
      END DO
!
      IF(GOSA.NE.GOSA) THEN
	    STOP 20
	  END IF
	  IF(GOSA.LT.EPS2) THEN
	    EXIT
      END IF
    END DO
!
!*************************************************************
!*                          LOOP 2 END                       *
!*************************************************************
!
	IF(MOD (NN,MPRINT) .EQ. 0) THEN
	  WRITE(6, *) 'time>',TIME
      WRITE(8, *) 'time>',TIME
      WRITE(6, *) 'out of gs>',NN,K,GOSA
	END IF
!*************************************************************
!*                     TIME MARCHING                         *
!*                    IMPLICIT METHOD                        *
!*************************************************************                                                                       
!
	DO J=2,MY-1                                                   
      DO I=2,MX-1                                                   
        Q(I,J)=U(I,J)                                                     
        D(I,J)=V(I,J)                                                     
	  END DO
	END DO
    DO J=2,MY-1                                                   
      DO I=2,MX-1                                                   
        U0=U(I,J)                                                         
        V0=V(I,J)                                                         
        BJ1=1.0/AJ(I,J)                                                   
        BJ2=BJ1*BJ1                                                       
        B1=ABS((U0*YE(I,J)-V0*XE(I,J))*BJ1)                               
        B2=ABS((V0*XX(I,J)-U0*YX(I,J))*BJ1)                               
        TTT=1.0                                            
        B3=-6.0*.25*UYT*(B1+B2)-2.0*TTT*REI*BJ2*(AL(I,J)+GA(I,J))         
        BB3(I,J)=1.0/(1.0-DT*B3)                                          
	  END DO
	END DO
!
!*************************************************************
!*                          LOOP 3 START                     *
!*************************************************************
!
	DO KK=1,ILAST3                                                
      GOSAQ=0.0                                                         
      DO J=2,MY-1                                                    
        DO I=2,MX-1                                                   
          P1=P(I+1,J)                                                       
          P2=P(I,J+1)                                                       
          P3=P(I-1,J)                                                       
          P4=P(I,J-1)                                                       
          WXI=WX(I,J)                                                       
          U1=U(I+1,J)                                                       
          U2=U(I,J+1)                                                       
          U3=U(I-1,J)                                                       
          U4=U(I,J-1)                                                       
          U0=Q(I,J)                                                         
          V1=V(I+1,J)                                                       
          V2=V(I,J+1)                                                       
          V3=V(I-1,J)                                                       
          V4=V(I,J-1)                                                       
          V0=D(I,J)                                                         
          XXI=XX(I,J)                                                       
          YXI=YX(I,J)                                                       
          XETA=XE(I,J)                                                      
          YETA=YE(I,J)                                                      
          QETA=WE(I,J)                                                      
          ALPH=AL(I,J)                                                      
          BETA=BE(I,J)                                                      
          GAMA=GA(I,J)                                                      
          BJ1=1.0/AJ(I,J)                                                   
          BJ2=BJ1*BJ1                                                       
          U5=U(I+2,J)                                                       
          U6=U(I,J+2)                                                       
          U7=U(I-2,J)                                                       
          U8=U(I,J-2)                                                       
          V5=V(I+2,J)                                                       
          V6=V(I,J+2)                                                       
          V7=V(I-2,J)                                                       
          V8=V(I,J-2)                                                       
          B1=(U0*YETA-V0*XETA)*BJ1                                          
          B2=(V0*XXI-U0*YXI)*BJ1                                            
          U0=0.0                                                            
          V0=0.0                                                            
          TTT=1.0                                            
          B3=B1*(-U5+8.0*(U1-U3)+U7)/12.+ABS(B1)*(U5+U7-4.0*(U1+U3)+6.0*U0)*.25*UYT+B2*(-U6+8.0*(U2-U4)+U8)/12.+ABS(B2)*(U6+U8-4.0*(U2+U4)+6.0*U0)*0.25*UYT                                  
          B4=B1*(-V5+8.0*(V1-V3)+V7)/12.+ABS(B1)*(V5+V7-4.0*(V1+V3)+6.0*V0)*.25*UYT+B2*(-V6+8.0*(V2-V4)+V8)/12.+ABS(B2)*(V6+V8-4.0*(V2+V4)+6.0*V0)*0.25*UYT                                  
          S1=-B3-0.5*(YETA*(P1-P3)-YXI*(P2-P4))*BJ1+TTT*REI*BJ2*(ALPH*(U1+U3)-0.5*BETA*(U(I+1,J+1)-U(I+1,J-1)-U(I-1,J+1)+U(I-1,J-1))+GAMA*(U2+U4)-2.0*U0*(ALPH+GAMA))+TTT*REI*(QETA*(U2-U4)+WXI*(U1-U3))*.5                           
          S2=-B4-0.5*(XXI*(P2-P4)-XETA*(P1-P3))*BJ1+TTT*REI*BJ2*(ALPH*(V1+V3)-0.5*BETA*(V(I+1,J+1)-V(I+1,J-1)-V(I-1,J+1)+V(I-1,J-1))+GAMA*(V2+V4)-2.0*V0*(ALPH+GAMA))+TTT*REI*(QETA*(V2-V4)+WXI*(V1-V3))*.5                           
          SS1=(Q(I,J)+DT*S1)*BB3(I,J)-U(I,J)                                
          SS2=(D(I,J)+DT*S2)*BB3(I,J)-V(I,J)                                
          GOSAQ=GOSAQ+SS1*SS1+SS2*SS2                                       
          TO(I)=U(I,J)+SS1*CONST3                                           
          TB(I)=V(I,J)+SS2*CONST3                                           
        END DO
        DO I=2,MX-1                                                    
          U(I,J)=TO(I)
          V(I,J)=TB(I)
        END DO
	  END DO
!
      IF(GOSAQ .NE. GOSAQ) THEN
        STOP 30
	  END IF
      IF(GOSAQ.LT.EPS3) THEN
        EXIT
	  END IF
    END DO
!
!Here is the Random
!	DO J=2,MY-1
!	  DO I=2,MX-1
!	    IF(I.GE.X2A.AND.I.LE.X3A.AND.J.GE.Y2A.AND.J.LE.Y3A) THEN
!	      CALL RANDOM_NUMBER(RDM)
!	      U(I,J)=U(I,J)+(RDM-.5)*6.0E-6
!		  CALL RANDOM_NUMBER(RDM)
!		  V(I,J)=V(I,J)+(RDM-.5)*6.0E-6
!	    END IF
!	  END DO
!	END DO
!
!*************************************************************
!*                          LOOP 3 END                       *
!*************************************************************
!
    DO J=2,MY-1                                                   
      DO I=2,MX-1                                                   
        D(I,J)=.5*BE(I,J)*Z(I,J)                                          
      END DO
    END DO
!
!*************************************************************
!*                          PDIFF ROUTINE                    *
!*************************************************************
! 
	IF(NN.GE.N2000) THEN
      PMAX = -1000.0
      PMIN = 1000.0
      DO J=1,MY
        DO I=1,MX
          PDIFF(I,J) = P(I,J) - P(1,MY-1)
          IF(PDIFF(I,J) .GT. PMAX) THEN
            PMAX = PDIFF(I,J)
          ENDIF
          IF(PDIFF(I,J) .LT. PMIN) THEN
            PMIN = PDIFF(I,J)
          ENDIF
        END DO
	  END DO
!      
	  IF(PMAX .GT. TPMAX) THEN
	    TPMAX = PMAX
	  END IF
	  IF(PMIN .LT. TPMIN) THEN
	    TPMIN = PMIN
	  END IF
	END IF
!
!*************************************************************
!*                       MONITERING DATA                     *
!*************************************************************
!
    IF(MOD(NN,MPRINT) .EQ. 0) THEN
      WRITE(6, *)  'out of im>', NN,KK,GOSAQ
      WRITE(8, *) NN,K,GOSA,KK,GOSAQ 
	END IF
!
!*************************************************************
!*              FOR DATA FILE OPEN/CLOSE                     *
!*************************************************************
!
!    IF((((NN-NN0)/MWRITE .GE. MSLICE) .AND. (ICOUNT .GE. IOPEN) .AND. (MOD((NN-NN0)/MWRITE,MSLICE) .EQ. 0)) .OR. ((TIME.GT.A1599) .AND. (TIME.LE.I1600))) THEN
!      CLOSE(12)
!      WRITE(6, *)"...",((NN-NN0)/MWRITE)/MSLICE,"!"
!	  WRITE(inte, *)((NN-NN0)/MWRITE)/MSLICE
!	  str(1:7)='fort.pv'
!	  str(8:8)=inte(2:2)
!      WRITE(6, *)'open ',str,'!'
!	  OPEN(UNIT=12,FILE=str,FORM='unformatted')
!	  ICOUNT=0
!	ENDIF
!
!*************************************************************
!*         initilize for integration of wall-pdiff           *
!*************************************************************
!
    IF(NN .GE. N2000) THEN
      PL=0.0
      PLD=0.0
      PLU=0.0
      PF=0.0
      PR=0.0
      PRD=0.0
      PRU=0.0
!
!*************************************************************
!*           for integration of wall-pdiff (begin)           *
!*************************************************************
!
      PLD=0.5*(PDIFF(NJ,2)+PDIFF(NJ,1))*TY(1)*RT2I+0.5*(PDIFF(NJ+1,1)+PDIFF(NJ,1))*TX(NJ)*RT2I
      PLU=0.5*(PDIFF(NJ,MY-1)+PDIFF(NJ,MY))*TY(MY)*RT2I+0.5*(PDIFF(NJ+1,MY)+PDIFF(NJ,MY))*TX(NJ)*RT2I
      PRD=0.5*(PDIFF(MX,2)+PDIFF(MX,1))*TY(1)*RT2I+0.5*(PDIFF(MX-1,1)+PDIFF(MX,1))*TX(MX)*RT2I
      PRU=0.5*(PDIFF(MX,MY-1)+PDIFF(MX,MY))*TY(MY)*RT2I+0.5*(PDIFF(MX-1,MY)+PDIFF(MX,MY))*TX(MX)*RT2I
      PL=PLD+PLU
      PR=PRD+PRU
      PF=PLD+PRD
      DO j=1,MY-1
        PL=PL+0.5*(PDIFF(NJ,j+1)+PDIFF(NJ,j))*TY(j)
        PR=PR+0.5*(PDIFF(MX,j+1)+PDIFF(MX,j))*TY(j)
      END DO
      DO i=1,MX-1
        PF=PF+0.5*(PDIFF(i+1,1)+PDIFF(i,1))*TX(i)
      END DO
      PRMT=PRMT+PR
      PFMT=PFMT+PF
      PLMT=PLMT+PL
    END IF
!
!*************************************************************
!*           for integration of wall-pdiff (end)             *
!*************************************************************
!
    IF((NN.GT.N1999).AND.(NN.LT.N2000)) THEN
      OPEN(UNIT=14,FILE='UVP1999.DAT')
	  DO J=1,MX
	    WRITE(14,"")
	    DO K=1,MY
	      WRITE(14,"(F18.8,' ',F18.8,' ',F18.8)") U(J,K),V(J,K),P(J,K)
	    END DO
	  END DO
      CLOSE(14)
    END IF
!
!*************************************************************
!*                   WRITE DATA START                        *
!*************************************************************
!                                                   
    IF(NN .GE. N2000 .AND. NN .LE. N2001 .AND. MOD(NN, MWRITE) .EQ. 0) THEN
	  DO J=1,15
		AREA1(1,J)=U(A1XY(J,1),A1XY(J,2))
		AREA1(2,J)=V(A1XY(J,1),A1XY(J,2))
		AREA1(3,J)=PDIFF(A1XY(J,1),A1XY(J,2))
	  END DO
!
      WRITE(600,"(F18.8)",ADVANCE="NO") TIME
      DO J=1,15
	    IF(MOD(J,15).EQ.0) THEN
		  WRITE(600,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		ELSE
		  WRITE(600,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		END IF
	  END DO                                                   
    ELSE IF(NN .GE. N2050 .AND. NN .LE. N2051 .AND. MOD(NN, MWRITE) .EQ. 0) THEN
	  DO J=1,15
		AREA1(1,J)=U(A1XY(J,1),A1XY(J,2))
		AREA1(2,J)=V(A1XY(J,1),A1XY(J,2))
		AREA1(3,J)=PDIFF(A1XY(J,1),A1XY(J,2))
	  END DO
!
      WRITE(601,"(F18.8)",ADVANCE="NO") TIME
      DO J=1,15
	    IF(MOD(J,15).EQ.0) THEN
		  WRITE(601,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		ELSE
		  WRITE(601,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		END IF
	  END DO!                                                   
    ELSE IF(NN .GE. N2100 .AND. NN .LE. N2101 .AND. MOD(NN, MWRITE) .EQ. 0) THEN
	  DO J=1,15
		AREA1(1,J)=U(A1XY(J,1),A1XY(J,2))
		AREA1(2,J)=V(A1XY(J,1),A1XY(J,2))
		AREA1(3,J)=PDIFF(A1XY(J,1),A1XY(J,2))
	  END DO
!
      WRITE(602,"(F18.8)",ADVANCE="NO") TIME
      DO J=1,15
	    IF(MOD(J,15).EQ.0) THEN
		  WRITE(602,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		ELSE
		  WRITE(602,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		END IF
	  END DO!                                                   
    ELSE IF(NN .GE. N2150 .AND. NN .LE. N2151 .AND. MOD(NN, MWRITE) .EQ. 0) THEN
	  DO J=1,15
		AREA1(1,J)=U(A1XY(J,1),A1XY(J,2))
		AREA1(2,J)=V(A1XY(J,1),A1XY(J,2))
		AREA1(3,J)=PDIFF(A1XY(J,1),A1XY(J,2))
	  END DO
!
      WRITE(603,"(F18.8)",ADVANCE="NO") TIME
      DO J=1,15
	    IF(MOD(J,15).EQ.0) THEN
		  WRITE(603,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		ELSE
		  WRITE(603,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		END IF
	  END DO!                                                   
    ELSE IF(NN .GE. N2200 .AND. NN .LE. N2201 .AND. MOD(NN, MWRITE) .EQ. 0) THEN
	  DO J=1,15
		AREA1(1,J)=U(A1XY(J,1),A1XY(J,2))
		AREA1(2,J)=V(A1XY(J,1),A1XY(J,2))
		AREA1(3,J)=PDIFF(A1XY(J,1),A1XY(J,2))
	  END DO
!
      WRITE(604,"(F18.8)",ADVANCE="NO") TIME
      DO J=1,15
	    IF(MOD(J,15).EQ.0) THEN
		  WRITE(604,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		ELSE
		  WRITE(604,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		END IF
	  END DO!                                                   
    ELSE IF(NN .GE. N2250 .AND. NN .LE. N2251 .AND. MOD(NN, MWRITE) .EQ. 0) THEN
	  DO J=1,15
		AREA1(1,J)=U(A1XY(J,1),A1XY(J,2))
		AREA1(2,J)=V(A1XY(J,1),A1XY(J,2))
		AREA1(3,J)=PDIFF(A1XY(J,1),A1XY(J,2))
	  END DO
!
      WRITE(605,"(F18.8)",ADVANCE="NO") TIME
      DO J=1,15
	    IF(MOD(J,15).EQ.0) THEN
		  WRITE(605,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		ELSE
		  WRITE(605,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		END IF
	  END DO!                                                   
    ELSE IF(NN .GE. N2300 .AND. NN .LE. N2301 .AND. MOD(NN, MWRITE) .EQ. 0) THEN
	  DO J=1,15
		AREA1(1,J)=U(A1XY(J,1),A1XY(J,2))
		AREA1(2,J)=V(A1XY(J,1),A1XY(J,2))
		AREA1(3,J)=PDIFF(A1XY(J,1),A1XY(J,2))
	  END DO
!
      WRITE(606,"(F18.8)",ADVANCE="NO") TIME
      DO J=1,15
	    IF(MOD(J,15).EQ.0) THEN
		  WRITE(606,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		ELSE
		  WRITE(606,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		END IF
	  END DO!                                                   
    ELSE IF(NN .GE. N2350 .AND. NN .LE. N2351 .AND. MOD(NN, MWRITE) .EQ. 0) THEN
	  DO J=1,15
		AREA1(1,J)=U(A1XY(J,1),A1XY(J,2))
		AREA1(2,J)=V(A1XY(J,1),A1XY(J,2))
		AREA1(3,J)=PDIFF(A1XY(J,1),A1XY(J,2))
	  END DO
!
      WRITE(607,"(F18.8)",ADVANCE="NO") TIME
      DO J=1,15
	    IF(MOD(J,15).EQ.0) THEN
		  WRITE(607,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		ELSE
		  WRITE(607,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		END IF
	  END DO
	ELSE IF(NN .GE. N2480 .AND. MOD(NN, MWRITE) .EQ. 0) THEN
	  NWRITED=NWRITED+1
      WRITE(6, *) 'writing at', NN,NWRITED
      WRITE(8, *) 'writing at', NN,NWRITED
!
!*************************************************************
!*           mean time pdiffwall routine (begin)             *
!*************************************************************
!
      OPEN(UNIT=18,FILE='meantimepdiff.dat')
      NSUM = NN-NN0+1
      CNT = 1.0/NSUM
      WRITE(18, *) RTIME,TIME,NSUM
      WRITE(18, *) PRMT*CNT,PFMT*CNT,PLMT*CNT
      WRITE(18, '(1H )')
	  CLOSE(18)
!
!*************************************************************
!*           mean time pdiffwall routine (end)               *
!*************************************************************
!
      IF(IPVOUT .EQ. 1) THEN
		ICOUNT=ICOUNT+1
        WRITE(12) NN,TIME
        WRITE(12) ((U(J,K),J=1,MX),K=1,MY)
        WRITE(12) ((V(J,K),J=1,MX),K=1,MY)
        WRITE(12) ((PDIFF(J,K),J=1,MX),K=1,MY)
	  END IF
!
      DO J=1,15
		AREA1(1,J)=U(A1XY(J,1),A1XY(J,2))
		AREA1(2,J)=V(A1XY(J,1),A1XY(J,2))
		AREA1(3,J)=PDIFF(A1XY(J,1),A1XY(J,2))
!
		AREA2(1,J)=U(A2XY(J,1),A2XY(J,2))
		AREA2(2,J)=V(A2XY(J,1),A2XY(J,2))
		AREA2(3,J)=PDIFF(A2XY(J,1),A2XY(J,2))
!
		AREA3(1,J)=U(A3XY(J,1),A3XY(J,2))
		AREA3(2,J)=V(A3XY(J,1),A3XY(J,2))
		AREA3(3,J)=PDIFF(A3XY(J,1),A3XY(J,2))
!
        AREA4(1,J)=U(A4XY(J,1),A4XY(J,2))
		AREA4(2,J)=V(A4XY(J,1),A4XY(J,2))
		AREA4(3,J)=PDIFF(A4XY(J,1),A4XY(J,2))
	  END DO
!
      WRITE(610,"(F18.8)",ADVANCE="NO") TIME
      WRITE(620,"(F18.8)",ADVANCE="NO") TIME
	  WRITE(630,"(F18.8)",ADVANCE="NO") TIME
	  WRITE(640,"(F18.8)",ADVANCE="NO") TIME
      DO J=1,15
	    IF(MOD(J,15).EQ.0) THEN
		  WRITE(610,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		  WRITE(620,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA2(1,J),AREA2(2,J),AREA2(3,J)
		  WRITE(630,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA3(1,J),AREA3(2,J),AREA3(3,J)
		  WRITE(640,"(' ',F18.8,' ',F18.8,' ',F18.8/)",ADVANCE="NO") AREA4(1,J),AREA4(2,J),AREA4(3,J)
		ELSE
		  WRITE(610,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA1(1,J),AREA1(2,J),AREA1(3,J)
		  WRITE(620,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA2(1,J),AREA2(2,J),AREA2(3,J)
		  WRITE(630,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA3(1,J),AREA3(2,J),AREA3(3,J)
		  WRITE(640,"(' ',F18.8,' ',F18.8,' ',F18.8)",ADVANCE="NO") AREA4(1,J),AREA4(2,J),AREA4(3,J)
		END IF
	  END DO
!
	END IF
!
!*************************************************************
!*                     WRITE DATA END                        *
!*************************************************************
!
!*************************************************************
!*                    WRITE VORT & U,V START                       *
!*************************************************************
!
    IF(NN .GE. N2490 .AND. MOD(NN,VWRITE) .EQ. 0) THEN
	  WRITE(INDX, "(I8)") NN
	  INDX=ADJUSTL(INDX)
!
	  OPEN(UNIT=22,FILE='vort'//TRIM(INDX)//'.txt')
!****** WRITE HEAD FOR TECPLOT ******
      WRITE(22,*) 'TITLE = "VORT"'
	  WRITE(22,*) 'VARIABLES = "X","Y","VORT"'
	  WRITE(22,*) 'ZONE T = "ZONE '//TRIM(INDX)//'"'
	  WRITE(22,*) 'I=151, J=151, K=1, ZONETYPE=Ordered'
	  WRITE(22,*) 'DATAPACKING=POINT'
	  WRITE(22,*) 'DT=(SINGLE SINGLE SINGLE)'
!****** WRITE HEAD FOR TECPLOT ******





	  DO J=2,MX-1
	    DO K=2,MY-1
	      DZ(1,1)=.5*(X0(J+1,K)-X0(J-1,K))
	      DZ(2,1)=.5*(Y0(J+1,K)-Y0(J-1,K))
	      DZ(1,2)=.5*(X0(J,K+1)-X0(J,K-1))
	      DZ(2,2)=.5*(Y0(J,K+1)-Y0(J,K-1))
		  AJJ=DZ(1,1)*DZ(2,2)-DZ(1,2)*DZ(2,1)
          AA(1,1)=DZ(2,2)/AJJ
          AA(2,2)=DZ(1,1)/AJJ
          AA(1,2)=-DZ(1,2)/AJJ
          AA(2,1)=-DZ(2,1)/AJJ
          UXI=.5*(U(J+1,K)-U(J-1,K))
          UETA=.5*(U(J,K+1)-U(J,K-1))
          VXI=.5*(V(J+1,K)-V(J-1,K))
          VETA=.5*(V(J,K+1)-V(J,K-1))
		  DUDY=AA(2,1)*UXI+AA(2,2)*UETA
          DVDX=AA(1,1)*VXI+AA(1,2)*VETA
          OMGZ(J,K)=DVDX-DUDY
        END DO
	  END DO
	  DO J=1,MX
        OMGZ(J,1)=OMGZ(J,2)
		OMGZ(J,MY)=OMGZ(J,MY-1)
	  END DO
      DO K=1,MY
        OMGZ(1,K)=OMGZ(MX-1,K)
        OMGZ(MX,K)=OMGZ(2,K)
      END DO
	  DO J=1,MX
		DO K=1,MY
          IF(OMGZ(J,K) .GT. 4.) THEN
		    OMGZ(J,K)=4.
          ELSE IF(OMGZ(J,K) .LT. -4.) THEN
		    OMGZ(J,K)=-4.
		  END IF
          IF(K .EQ. 1) THEN
		    WRITE(22,"(/,3F18.8)") X0(J,K), Y0(J,K), OMGZ(J,K)
          ELSE
            WRITE(22,"(3F18.8)") X0(J,K), Y0(J,K), OMGZ(J,K)
          END IF
		END DO
	  END DO
	  CLOSE(unit=22)
!
      OPEN(UNIT=24,FILE='uv'//TRIM(INDX)//'.txt')
	  DO J=2,MX-1
	    WRITE(24," ")
		DO K=2,MY-1
		  WRITE(24,"(4F18.8)") X0(J,K), Y0(J,K), U(J,K), V(J,K)
		END DO
	  END DO
	END IF
!
!*************************************************************
!*                     WRITE VORT & U,V END                        *
!*************************************************************
!
    IF(NN .GE. N2000) THEN
      MWRITE=100
	END IF
    IF(NN .GT. N2500) THEN
      EXIT
    END IF
!
  END DO
!
!*************************************************************
!*                          LOOP 1 END                       *
!*************************************************************
!
  WRITE(6, *) 'TIME IS OVER 2000!'
  WRITE(8, *) 'TIME IS OVER 2000'
  WRITE(8, *)' pdiff LOOP =',LOOP
  WRITE(6, *)' pdiff LOOP =',LOOP
  CLOSE(8)
  IF(IPVOUT .EQ. 1) THEN
	CLOSE(12)
  END IF
  STOP 40
END PROGRAM CAVITY_1X1_MAC_METHOD_CDF
!
!
!
SUBROUTINE READ_PUV (DP,DU,DV,N)
!
! READ THE DATA FROM UVP1999.DAT
!
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: I,J,IMAX,JMAX,JSTEP
  REAL :: P,U,V
  REAL, INTENT (OUT), DIMENSION (N, N) :: DP,DU,DV
!
  IMAX = 251
  JMAX = 252
!
  OPEN (UNIT=80,FILE="UVP10200.DAT",STATUS="OLD")
!
  DO I = 1, IMAX
    JSTEP = 1
    DO J = 1, JMAX
	  READ (80, "(F18.8,' ',F18.8,' ',F18.8)") U, V, P
	  IF (P .EQ. 0.0 .AND. U .EQ. 0.0 .AND. V .EQ. 0.0) THEN
		JSTEP = JSTEP - 1
	  ELSE
	    DP(I, JSTEP) = P
		DU(I, JSTEP) = U
		DV(I, JSTEP) = V
	  END IF
	  JSTEP = JSTEP + 1
	END DO
  END DO
END SUBROUTINE READ_PUV
!