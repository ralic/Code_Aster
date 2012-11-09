      SUBROUTINE DPVPMA ( MOD, IMAT, NBMAT, TEMPD, MATERD,
     &                      MATERF, MATCST, NDT, NDI, NVI, INDAL)
C ====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
        IMPLICIT     NONE
        INTEGER      NDT, NDI, NVI, IMAT, NBMAT
        REAL*8       MATERD(NBMAT,2), MATERF(NBMAT,2), TEMPD
        CHARACTER*8  MOD
        CHARACTER*3  MATCST
C ====================================================================
C --- RECUPERATION DONNEES MATERIAU POUR DRUCKER PRAGER VISCOPLASTIQUE
C --- VISC_DRUC_PRAG -------------------------------------------------
C ====================================================================
C IN  : MOD    : TYPE DE MODELISATION -----------------------------
C --- : IMAT   : ADRESSE DU MATERIAU CODE -------------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : TEMPD  : TEMPERATURE BIDON --------------------------------
C OUT : MATERD : COEFFICIENTS MATERIAU A T ------------------------
C --- : MATERF : COEFFICIENTS MATERIAU A T+DT ---------------------
C ------------ : MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ------------ : MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C --- : MATCST : 'OUI' --------------------------------------------
C --- : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR -----------
C --- : NDI    : NOMBRE DE COMPOSANTES DIRECTES DU TENSEUR --------
C --- : NVI    : NB DE VARIABLES INTERNES -------------------------
C --- : INDAL  : INDICATEUR SUR ALPHA
C =================================================================
        INTEGER         II,INDAL,I,J
        REAL*8          E, NU, MU, K
        REAL*8          UN, DEUX, TROIS
      INTEGER CERR(17)
        CHARACTER*8     NOMC(17)
C =================================================================
        PARAMETER ( TROIS  =  3.0D0 )
        PARAMETER ( DEUX   =  2.0D0 )
        PARAMETER ( UN     =  1.0D0 )
C =================================================================
C --- DEFINITION PARAMETRES MATERIAU ELAS -------------------------
C =================================================================
        NOMC(1)  =  'E        '
        NOMC(2)  =  'NU       '
        NOMC(3)  =  'ALPHA    '
        NOMC(4)  =  'PREF     '
        NOMC(5)  =  'A        '
        NOMC(6)  =  'N        '
        NOMC(7)  =  'P_PIC    '
        NOMC(8)  =  'P_ULT    '
        NOMC(9)  =  'ALPHA_0  '
        NOMC(10) =  'ALPHA_PIC'
        NOMC(11) =  'ALPHA_ULT'
        NOMC(12) =  'R_0      '
        NOMC(13) =  'R_PIC    '
        NOMC(14) =  'R_ULT    '
        NOMC(15) =  'BETA_0   '
        NOMC(16) =  'BETA_PIC '
        NOMC(17) =  'BETA_ULT '

      DO 101 I=1,NBMAT
         DO 102 J=1,2
            MATERD(I,J) = 0.D0
 102     CONTINUE
 101  CONTINUE

C =================================================================
C --- RECUPERATION DES PARAMETRES MATERIAU ------------------------
C =================================================================
      CALL RCVALA(IMAT,' ', 'ELAS', 1, 'TEMP', TEMPD, 3,
     &               NOMC(1),  MATERD(1,1),  CERR(1), 0)
      INDAL=1
      IF (CERR(3).NE.0) INDAL=0

      CALL RCVALA(IMAT,' ','VISC_DRUC_PRAG', 1, 'TEMP', TEMPD, 14,
     &               NOMC(4),  MATERD(1,2),  CERR(4), 0)
C =================================================================
C - CALCUL DES MODULES DE CISAILLEMENT ET DE DEFORMATION VOLUMIQUE-
C =================================================================
      E     = MATERD(1,1)
      NU    = MATERD(2,1)
      MU    = E / (DEUX*(UN+NU))
      K     = E / (TROIS*(UN-DEUX*NU))
C =================================================================
C --- STOCKAGE DES MODULES CALCULES COMME PARAMETRES MATERIAU -----
C =================================================================
      MATERD(4,1) = MU
      MATERD(5,1) = K
C =================================================================
C --- DEFINITION D'UN MATERIAU FINAL ------------------------------
C =================================================================
      DO 10 II=1,NBMAT
         MATERF(II,1) = MATERD(II,1)
         MATERF(II,2) = MATERD(II,2)
 10   CONTINUE
      MATCST = 'OUI'
C =================================================================
C --- NOMBRE DE COMPOSANTES ---------------------------------------
C =================================================================
      IF (MOD(1:2).EQ.'3D') THEN
         NDT = 6
         NDI = 3
      ELSE IF ((MOD(1:6).EQ.'D_PLAN') .OR.
     &         (MOD(1:4).EQ.'AXIS')        ) THEN
         NDT = 4
         NDI = 3
      ENDIF
C =================================================================
C --- NOMBRE DE VARIABLES INTERNES --------------------------------
C =================================================================
      NVI = 4
C =================================================================
      END
