      SUBROUTINE LCMMJA (TYPMOD,  NMAT,MATERF, TIMED, TIMEF,
     &                    ITMAX, TOLER,NBCOMM,CPMONO,   PGL,
     &                   TOUTMS,   HSR,    NR,  VIND,    YF,
     &                       DY,  DRDY,  IRET               )
      IMPLICIT NONE
C ----------------------------------------------------------------------
C MODIF ALGORITH  DATE 14/06/2011   AUTEUR PROIX J-M.PROIX 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J.M.PROIX
C TOLE CRS_1404
C       ----------------------------------------------------------------
C       MONOCRISTAL : CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
C                    DY    = ( DSIG + DGAMMA PAR SYST )
C                    Y     = ( SIG   GAMMA P par syst. gliss)
C       IN  TYPMOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TIMED  :  ISTANT PRECEDENT
C           TIMEF  :  INSTANT ACTUEL
C           ITMAX  :  ITER_INTE_MAXI
C           TOLER  :  RESI_INTE_RELA
C           COMP   :  NOM COMPORTEMENT
C           NBCOMM :  INCIDES DES COEF MATERIAU
C           CPMONO :  NOM DES COMPORTEMENTS
C           PGL    :  MATRICE DE PASSAGE
C           TOUTMS :  TENSEURS D'ORIENTATION
C           HSR    :  MATRICE D'INTERACTION
C           NVI    :  NOMBRE DE VARIABLES INTERNES
C           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
C           YF     :  VARIABLES A T + DT =  ( SIGF X1F X2F PF (EPS3F) )
C           DY     :  SOLUTION           =  ( DSIG DX1 DX2 DP (DEPS3) )
C           NR     :  DIMENSION DECLAREE DRDY
C       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C       OUT IRET   :  CODE RETOUR
C       ----------------------------------------------------------------
      INTEGER         NMAT , NR, NBFSYS
      INTEGER         NBCOMM(NMAT,3),IFA,I,IRET
      INTEGER         IFL,ITMAX
      INTEGER         NUECOU,KR(5),IMIN,IMAX
      REAL*8            VIND(*)
      REAL*8          TOUTMS(5,24,6),HSR(5,24,24)
C     ALLOCATION DYNAMIQUE
      REAL*8          PGL(3,3),TIMED, TIMEF
      REAL*8          YF(*),DY(*),DRDY(NR,NR)
      REAL*8          MATERF(NMAT*2)
      REAL*8          TOLER
      CHARACTER*16    CPMONO(5*NMAT+1)
      CHARACTER*8     TYPMOD
C     ----------------------------------------------------------------

      IRET=0
      NBFSYS=NBCOMM(NMAT,2)
C     test pour verifier que KOCKS_RAUCH n'est pas mélangé avec d'autres

      DO 6 IFA=1,NBFSYS
         IFL=NBCOMM(IFA,1)
         NUECOU=NINT(MATERF(NMAT+IFL))
         KR(IFA)=NUECOU
    6 CONTINUE
      IMAX=0
      IMIN=9
      DO 7 I=1,NBFSYS
        IF (KR(I).GT.IMAX) IMAX=KR(I)
        IF (KR(I).LT.IMIN) IMIN=KR(I)
    7 CONTINUE
      IF (IMAX.EQ.4) THEN
C        KOCKS-RAUCH
         CALL ASSERT(IMIN.EQ.4)
         CALL LCMMJ2 (TYPMOD, NMAT, MATERF,TIMED, TIMEF,
     &                NBCOMM,CPMONO,PGL,TOUTMS,HSR,
     &                NR,VIND,YF,DY,DRDY,IRET)
      ELSEIF (IMAX.EQ.5) THEN
C        DD-CFC
         CALL ASSERT(IMIN.EQ.5)
         CALL LCMMJ3 (TYPMOD, NMAT, MATERF,TIMED, TIMEF,
     &                NBCOMM,CPMONO,PGL,TOUTMS,HSR,
     &                NR,VIND,YF,DY,DRDY,IRET)
      ELSE
C        AUTRES COMPORTEMENTS
         CALL LCMMJ1 (TYPMOD, NMAT, MATERF,TIMED, TIMEF,
     &                ITMAX,TOLER,NBCOMM,CPMONO,PGL,TOUTMS,HSR,
     &                 NR,VIND,YF,DY,DRDY,IRET)
      ENDIF
      END
