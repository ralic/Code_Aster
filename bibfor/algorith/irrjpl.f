      SUBROUTINE  IRRJPL(FAMI,KPG,KSP,MOD,NMAT,MATER,NR,NVI,SIGF,VIND,
     &                   VINF,SIGD,DSDE)

      IMPLICIT NONE
      CHARACTER*(*)     FAMI
      CHARACTER*8       MOD
      INTEGER           NMAT,NMOD,KPG,KSP,NVI,NR
      REAL*8            MATER(NMAT,2),DSDE(6,6),SIGF(6),VIND(*)
      REAL*8            VINF(*),SIGD(6)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/02/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================

C       ----------------------------------------------------------------
C       IRRAD3M   :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
C                     ELASTO_PLASTIQUE EN VITESSE A T OU T+DT
C       ----------------------------------------------------------------
C       IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C           KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATER  :  COEFFICIENTS MATERIAU
C           NR     :  TAILLE DE LA MATRICE JACOBIENNE
C           SIGF   :  CONTRAINTES A T+DT
C           VIND   :  VARIABLES INTERNES A T
C           VINF   :  VARIABLES INTERNES A T+DT
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C       ----------------------------------------------------------------
      INTEGER NDT,NDI
C     ------------------------------------------------------------------
      COMMON /TDIM/ NDT,NDI

      REAL*8 YD(NR),YF(NR),DY(NR),DRDY(NR,NR),L,DET
      REAL*8 Y1(NDT,NDT),Y2(NDT),Y3(NDT),Y5(NDT),MAT(6,6),I4(6,6)
      INTEGER I,J,IRET

C     ------------------------------------------------------------------
       DATA  I4        /1.D0   , 0.D0  , 0.D0  , 0.D0  ,0.D0  ,0.D0,
     1                  0.D0   , 1.D0  , 0.D0  , 0.D0  ,0.D0  ,0.D0,
     2                  0.D0   , 0.D0  , 1.D0  , 0.D0  ,0.D0  ,0.D0,
     3                  0.D0   , 0.D0  , 0.D0  , 1.D0  ,0.D0  ,0.D0,
     4                  0.D0   , 0.D0  , 0.D0  , 0.D0  ,1.D0  ,0.D0,
     5                  0.D0   , 0.D0  , 0.D0  , 0.D0  ,0.D0  ,1.D0/
C     ------------------------------------------------------------------

      CALL LCEQVN(NDT,SIGF,YF)
      CALL LCEQVN(NDT,SIGD,YD)

      CALL LCEQVN ( NVI-1,  VIND , YD(NDT+1) )
      CALL LCEQVN ( NVI-1,  VINF , YF(NDT+1) )

      IF ( MOD.EQ.'C_PLAN') THEN
        CALL LCEQVN ( (NR-1),  YF , DY )
        CALL DAXPY ( (NR-1), -1.D0, YD, 1,DY, 1)
      ELSE
        CALL LCEQVN ( NR,  YF , DY )
        CALL DAXPY ( NR, -1.D0, YD, 1,DY, 1)
      ENDIF
C     RECALCUL DE LA MATRICE JACOBIENNE
      CALL IRRJAC (FAMI,KPG,KSP,MOD,NMAT,MATER,YF,DY,NR,DRDY)

      DO 10 I=1,NDT
        DO 20 J=1,NDT
          Y1(I,J)=DRDY(I,J)
20      CONTINUE
10    CONTINUE

      DO 30 I=1,NDT
        Y2(I)=DRDY(I,NDT+1)
30    CONTINUE

      DO 40 I=1,NDT
        Y3(I)=DRDY(NDT+1,I)
40    CONTINUE

      L=DRDY(NDT+1,NDT+1)

      DO 50 I=1,NDT
        Y5(I)=DRDY(NDT+3,I)
50    CONTINUE

      DO 60 I=1,NDT
        DO 70 J=1,NDT
          IF (L.EQ.0.D0) THEN
            MAT(I,J)=Y1(I,J)-Y2(I)*Y5(J)
          ELSE
            MAT(I,J)=Y1(I,J)-(1.D0/L)*Y2(I)*Y3(J)-Y2(I)*Y5(J)
          ENDIF
70      CONTINUE
60    CONTINUE

      CALL LCEQMA ( I4, DSDE)
      CALL MGAUSS('NFVP',MAT,DSDE,6,NDT,NDT,DET,IRET)
      
      END
