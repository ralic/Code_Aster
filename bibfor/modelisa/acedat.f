      SUBROUTINE ACEDAT(MOTFAC,IN,NPARA,SEC,EXP,TAB,CAR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                  IN,NPARA(*)
      CHARACTER*(*)     MOTFAC,         SEC(*),EXP(*),TAB(*),CAR(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/05/2005   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     INITIALISATION DES PARAMETRES ET DES DATAS
C ----------------------------------------------------------------------
C
C     --- MOT CLE FACTEUR "POUTRE"-------------------------------------
      PARAMETER ( NGENPO = 15 , NRECPO = 6  , NCERPO = 2  )
      PARAMETER ( NSECPO = 2  , NTSEPO = 3  , NBOPOU = 44 )
      CHARACTER*8  EXPPOU(NBOPOU), TABPOU(NBOPOU)
      CHARACTER*8  CARPOU(NGENPO*(NSECPO+1),NTSEPO)
      CHARACTER*16 SECPOU(NTSEPO)
C
C     --- MOT CLE FACTEUR "BARRE" -------------------------------------
      PARAMETER ( NGENBA = 1  , NRECBA = 6  , NCERBA = 2  )
      PARAMETER ( NSECBA = 1  , NTSEBA = 3  , NBOBAR = 8  )
      CHARACTER*8  EXPBAR(NBOBAR), TABBAR(NBOBAR), CARBAR(NRECBA,NTSEBA)
      CHARACTER*16 SECBAR(NTSEBA)
C
C     --- POUTRE -------------------------------------------------------
      DATA TABPOU /'A1      ','IY1     ','IZ1     ','AY1     ',
     +             'AZ1     ','EY1     ','EZ1     ','JX1     ',
     +             'RY1     ','RZ1     ','RT1     ',
     +             'A2      ','IY2     ','IZ2     ','AY2     ',
     +             'AZ2     ','EY2     ','EZ2     ','JX2     ',
     +             'RY2     ','RZ2     ','RT2     ','TVAR    ',
     +             'HY1     ','HZ1     ','EPY1    ','EPZ1    ',
     +             'HY2     ','HZ2     ','EPY2    ','EPZ2    ',
     +             'R1      ','EP1     ','R2      ','EP2     ',
     +             'TSEC    ','AI1     ','AI2     ','JG1     ',
     +             'JG2     ','IYR21   ','IYR22   ','IZR21   ',
     +             'IZR22   '/
      DATA EXPPOU /'A       ','IY      ','IZ      ','AY      ',
     +             'AZ      ','EY      ','EZ      ','JX      ',
     +             'RY      ','RZ      ','RT      ',
     +             'A       ','IY      ','IZ      ','AY      ',
     +             'AZ      ','EY      ','EZ      ','JX      ',
     +             'RY      ','RZ      ','RT      ','TVAR    ',
     +             'HY      ','HZ      ','EPY     ','EPZ     ',
     +             'HY      ','HZ      ','EPY     ','EPZ     ',
     +             'R       ','EP      ','R       ','EP      ',
     +             'TSEC    ','AI      ','AI      ','JG      ',
     +             'JG      ','IYR2    ','IYR2    ','IZR2    ',
     +             'IZR2    '/
      DATA SECPOU(1)                     /'GENERALE  '/
      DATA (CARPOU(I,1),I=1,NGENPO*(NSECPO+1))/
     +             'A       ','IY      ','IZ      ','AY      ',
     +             'AZ      ','EY      ','EZ      ','JX      ',
     +             'RY      ','RZ      ','RT      ','AI      ',
     +             'JG      ','IYR2    ','IZR2    ',
     +             'A1      ','IY1     ','IZ1     ','AY1     ',
     +             'AZ1     ','EY1     ','EZ1     ','JX1     ',
     +             'RY1     ','RZ1     ','RT1     ','AI1     ',
     +             'JG1     ','IYR21   ','IZR21   ',
     +             'A2      ','IY2     ','IZ2     ','AY2     ',
     +             'AZ2     ','EY2     ','EZ2     ','JX2     ',
     +             'RY2     ','RZ2     ','RT2     ','AI2     ',
     +             'JG2     ','IYR22   ','IZR22   '/
      DATA SECPOU(2)                     /'RECTANGLE'/
      DATA (CARPOU(I,2),I=1,NRECPO*(NSECPO+1))/
     +             'H       ','HY      ','HZ      ',
     +             'EP      ','EPY     ','EPZ     ',
     +             'H1      ','HY1     ','HZ1     ',
     +             'EP1     ','EPY1    ','EPZ1    ',
     +             'H2      ','HY2     ','HZ2     ',
     +             'EP2     ','EPY2    ','EPZ2    '/
      DATA SECPOU(3)                     /'CERCLE    '/
      DATA (CARPOU(I,3),I=1,NCERPO*(NSECPO+1))/
     +             'R       ','EP      ',
     +             'R1      ','EP1     ',
     +             'R2      ','EP2     '/
C
C     --- BARRE --------------------------------------------------------
      DATA  EXPBAR /'A' ,'HY' ,'HZ' ,'EPY' ,'EPZ' ,'R' ,'EP' ,'TSEC'/
      DATA  TABBAR /'A1','HY1','HZ1','EPY1','EPZ1','R1','EP1','TSEC'/
      DATA  SECBAR(1)               /'GENERALE'/
      DATA (CARBAR(I,1),I=1,NGENBA) /'A'/
      DATA  SECBAR(2)               /'RECTANGLE'/
      DATA (CARBAR(I,2),I=1,NRECBA) /'H','HY','HZ','EP','EPY','EPZ'/
      DATA  SECBAR(3)               /'CERCLE '/
      DATA (CARBAR(I,3),I=1,NCERBA) /'R','EP'/
C     ------------------------------------------------------------------
      IF ( MOTFAC.EQ. 'POUTRE' ) THEN
         NPARA(1) = NSECPO
         NPARA(2) = NTSEPO
         NPARA(3) = NBOPOU
         NPARA(4) = NSECPO * NGENPO
         NPARA(5) = NSECPO * NGENPO
         NPARA(6) = NGENPO
         NPARA(7) = NRECPO
         NPARA(8) = NCERPO
         IF (IN.EQ.0) GOTO 9999
         DO 10 I = 1,NTSEPO
            SEC(I) = SECPOU(I)
 10      CONTINUE
         DO 12 I = 1,NBOPOU
            EXP(I) = EXPPOU(I)
            TAB(I) = TABPOU(I)
 12      CONTINUE
         K = 0
         DO 14 I = 1,NGENPO*(NSECPO+1)
            K = K + 1
            CAR(K) = CARPOU(I,1)
 14      CONTINUE
         DO 16 I = 1,NRECPO*(NSECPO+1)
            K = K + 1
            CAR(K) = CARPOU(I,2)
 16      CONTINUE
         K = 2*NGENPO*(NSECPO+1)
         DO 18 I = 1,NCERPO*(NSECPO+1)
            K = K + 1
            CAR(K) = CARPOU(I,3)
 18      CONTINUE
      ELSEIF ( MOTFAC.EQ. 'BARRE' ) THEN
         NPARA(1) = NSECBA
         NPARA(2) = NTSEBA
         NPARA(3) = NBOBAR
         NPARA(4) = NSECBA * NRECBA
         NPARA(5) = NSECBA * NRECBA
         NPARA(6) = NGENBA
         NPARA(7) = NRECBA
         NPARA(8) = NCERBA
         IF (IN.EQ.0) GOTO 9999
         DO 20 I = 1,NTSEBA
            SEC(I) = SECBAR(I)
 20      CONTINUE
         DO 22 I = 1,NBOBAR
            EXP(I) = EXPBAR(I)
            TAB(I) = TABBAR(I)
 22      CONTINUE
         K = 0
         DO 24 I = 1,NGENBA
            K = K + 1
            CAR(K) = CARBAR(I,1)
 24      CONTINUE
         K = NRECBA
         DO 26 I = 1,NRECBA
            K = K + 1
            CAR(K) = CARBAR(I,2)
 26      CONTINUE
         K = 2*NRECBA
         DO 28 I = 1,NCERBA
            K = K + 1
            CAR(K) = CARBAR(I,3)
 28      CONTINUE
      ENDIF
C
 9999 CONTINUE
      END
