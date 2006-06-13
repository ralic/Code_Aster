      SUBROUTINE FGDOBA(NOMMAT,NBCYCL,SIGMIN,SIGMAX,LKE,RKE,
     +                  LHAIGH,RCORR,DOM)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8       NOMMAT
      REAL*8                          SIGMIN(*),SIGMAX(*)
      REAL*8                   RCORR(*),DOM(*),         RKE(*)
      INTEGER                  NBCYCL
      LOGICAL           LHAIGH,                     LKE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 25/07/2001   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C     -----------------------------------------------------------------
C     CALCUL DU DOMMAGE ELEMENTAIRE PAR INTERPOLATION SUR
C     UNE COURBE DE WOHLER DONNEE PAR 1/N = A (DSIGMA)**BETA
C     FORMULE DE BASKIN
C     ------------------------------------------------------------------
C IN  NOMMAT : K8  : NOM DU MATERIAU
C IN  NBCYCL : I   : NOMBRE DE CYCLES
C IN  SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
C IN  SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
C IN  LKE    : L   : PRISE EN COMPTE DU COEFFICIENT KE
C IN  RKE    : R   : VALEURS DU COEFFICIENT KE
C IN  LHAIGH : L   : PRISE EN COMPTE CORRECTION DE HAIGH
C IN  RCORR  : R   : VALEURS DE LA CORRECTION DE HAIGH
C OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      REAL*8         DELTA,VAL(2),RBID
      CHARACTER*2    CODRET(2)
      CHARACTER*8    NOMRES(2),NOMPAR
C
      NOMPAR = ' '
      NBPAR  = 0
      NOMRES(1) = 'A_BASQUI'
      NOMRES(2) = 'BETA_BAS'
      CALL RCVALE(NOMMAT,'FATIGUE',NBPAR,NOMPAR,RBID,2,NOMRES,VAL,
     +            CODRET,'F ')
      DO 10 I=1,NBCYCL
        DELTA = (1.D0/2.D0)*ABS(SIGMAX(I)-SIGMIN(I))
        IF (LKE) DELTA = DELTA * RKE(I)
        IF (LHAIGH) DELTA = DELTA / RCORR(I)
        DOM(I) = VAL(1)* DELTA**VAL(2)
 10   CONTINUE
C
      END
