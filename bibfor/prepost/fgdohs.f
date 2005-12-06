      SUBROUTINE FGDOHS(NOMMAT,NBCYCL,SIGMIN,SIGMAX,
     +                  LKE,RKE,LHAIGH,RCORR,DOM)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8       NOMMAT
      REAL*8                          SIGMIN(*),SIGMAX(*)
      REAL*8                RKE(*),    RCORR(*),DOM(*)
      INTEGER                  NBCYCL
      LOGICAL           LKE,    LHAIGH
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
C     UNE COURBE DE WOHLER DONNEE HORS ZONE SINGULIERE
C     ------------------------------------------------------------------
C IN  NOMMAT : K8  : NOM DU MATERIAU
C IN  NBCYCL : I   : NOMBRE DE CYCLES
C IN  SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
C IN  SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
C IN  LKE    : L   : PRISE EN COMPTE DU COEFFICIENT KE
C IN  RKE    : R   : VALEURS DES COEFFICIENTS KE
C IN  LHAIGH : L   : PRISE EN COMPTE DE LA CORRECTION DE HAIGH
C IN  RCORR  : R   : VALEURS DES CORRECTIONS DE HAIGH
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
      CHARACTER*2  CODRET(6)
      CHARACTER*8  NOMPAR
      CHARACTER*8  NOMRES(6)

      REAL*8       DELTA,SALT,X,Y,NRUPT,SLMODI,VAL(6),RBID,RE
C
      NOMRES(1) = 'E_REFE'
      NOMRES(2) = 'A0'
      NOMRES(3) = 'A1'
      NOMRES(4) = 'A2'
      NOMRES(5) = 'A3'
      NOMRES(6) = 'SL'
      NBPAR     = 0
      NOMPAR    = ' '
      CALL RCVALE(NOMMAT,'FATIGUE',NBPAR,NOMPAR,RBID,6,NOMRES,VAL,
     +            CODRET,'F ')
      NOMRES(1) = 'E'
      CALL RCVALE(NOMMAT,'ELAS',NBPAR,NOMPAR,RBID,1,NOMRES,RE,
     +            CODRET,'F ')
      DO 10 I=1,NBCYCL
        DELTA = ABS(SIGMAX(I)-SIGMIN(I))
        IF(LKE) DELTA = DELTA * RKE(I)
        IF(LHAIGH) THEN
          DELTA = DELTA / RCORR(I)
          SLMODI  = VAL(6) / RCORR(I)
        ELSE
          SLMODI  = VAL(6)
        ENDIF
        SALT  = 1.D0/2.D0*(VAL(1)/RE)*DELTA
        X  = LOG10 (SALT)
        IF (SALT.GE.SLMODI) THEN
          Y = VAL(2) + VAL(3)*X + VAL(4)*X**2 + VAL(5)*X**3
          NRUPT = 10**Y
          DOM(I) = 1.D0 / NRUPT
        ELSE
          DOM(I) = 0.D0
        ENDIF
 10   CONTINUE
C
      END
