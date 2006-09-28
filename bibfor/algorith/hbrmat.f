      SUBROUTINE HBRMAT(MOD, IMAT, NBMAT, TEMPD, MATERD, MATERF, MATCST,
     &   NDT, NDI, NR, NVI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
        IMPLICIT     NONE
        INTEGER      NDT, NDI, NVI, NR,IMAT,NBMAT
        REAL*8       MATERF(NBMAT,2),MATERD(NBMAT,2),TEMPD
        CHARACTER*3  MATCST
        CHARACTER*8  MOD
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU POUR LA LOI DE HOEK BROWN ------
C |--------------------------------------------------------------------|
C |----- NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES -----------|
C |----- MATER(*,1) = E, NU, ALPHA, MU, K -----------------------------|
C |----- MATER(*,2) = GRUP, GRES, SEND --------------------------------|
C |---------------- : SRUP, MEND, MRUP, BETA, ALPHAHB,  ---------------|
C |---------------- : PPHI1, BRES, AP, DP, CP, SIGBD, PPHI2, PPHI0 ----|
C |----- VARIABLE INTERNE : GAMMAP-, EPSP-  ---------------------------|
C |--------------------------------------------------------------------|
C ======================================================================
C IN  IMAT   :  ADRESSE DU MATERIAU CODE -------------------------------
C     NBMAT  :  NOMBRE DE PARAMETRES MATERIAU --------------------------
C     TEMPD  :  TEMPERATURE BIDON --------------------------------------
C     MOD    :  TYPE DE MODELISATION -----------------------------------
C OUT MATERD :  COEFFICIENTS MATERIAU A T ------------------------------
C     MATERF :  COEFFICIENTS MATERIAU A T+DT ---------------------------
C               MATER(*,1) = CARACTERISTIQUES   ELASTIQUES -------------
C               MATER(*,2) = CARACTERISTIQUES   PLASTIQUES -------------
C     MATCST : 'OUI' ---------------------------------------------------
C     NDT    :  NB TOTAL DE COMPOSANTES TENSEURS -----------------------
C     NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS -------------------
C     NR     :  NOMBRE D'EQUATION DU SYSTEME NL  -----------------------
C     NVI    :  NB DE VARIABLES INTERNES -------------------------------
C ======================================================================
      REAL*8          E, NU, MU, K, MRUP, SRUP, ALPHA
      REAL*8          AP, DP, CP, SIGBD, BRES, GRES, GRUP
      REAL*8          UN, DEUX, EPS
      REAL*8          COHERE
      CHARACTER*2     CERR(14)
      CHARACTER*8     NOMC(14)
      INTEGER         II
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( EPS    =  1.0D-6  )
C =================================================================
      DO 12 II = 1 , NBMAT
         MATERD(II,1) = 0.D0
         MATERD(II,2) = 0.D0
         MATERF(II,1) = 0.D0
         MATERF(II,2) = 0.D0
 12   CONTINUE
C =================================================================
C --- DEFINITION DES CHAMPS ---------------------------------------
C =================================================================
      NOMC(1)  = 'E         '
      NOMC(2)  = 'NU        '
      NOMC(3)  = 'ALPHA     '
      NOMC(4)  = 'GAMMA_RUP '
      NOMC(5)  = 'GAMMA_RES '
      NOMC(6)  = 'S_END     '
      NOMC(7)  = 'S_RUP     '
      NOMC(8) =  'M_END     '
      NOMC(9) =  'M_RUP     '
      NOMC(10) = 'BETA      '
      NOMC(11) = 'ALPHAHB   '
      NOMC(12) = 'PHI_RUP   '
      NOMC(13) = 'PHI_RES   '
      NOMC(14) = 'PHI_END   '
C =================================================================
C --- RECUPERATION DES PARAMETRES MATERIAU ------------------------
C =================================================================
      MATERF(3,1) = 0.0D0
      MATERF(11,2) = 0.0D0
      CALL RCVALA(IMAT,' ', 'ELAS', 0, ' ', 0.D0, 2,
     &     NOMC(1),  MATERF(1,1),  CERR(1), 'FM')
      CALL RCVALA(IMAT,' ', 'ELAS', 0, ' ', 0.D0, 1,
     &     NOMC(3),  MATERF(3,1),  CERR(3), ' ')
      CALL RCVALA(IMAT,' ', 'HOEK_BROWN', 0, ' ', 0.D0, 10,
     &     NOMC(4),  MATERF(1,2),  CERR(4), 'FM' )
      CALL RCVALA(IMAT,' ', 'HOEK_BROWN', 0, ' ', 0.D0, 1,
     &     NOMC(14),  MATERF(11,2),  CERR(14), ' ' )
C =================================================================
C - CALCUL DES MODULES DE CISAILLEMENT ET DE DEFORMATION VOLUMIQUE-
C =================================================================
      E     = MATERF(1,1)
      NU    = MATERF(2,1)
      MU    = E / (DEUX*(UN+NU))
      K     = E / (3.0D0*(UN-DEUX*NU))
C =================================================================
C --- STOCKAGE DES PARAMETRES ELASTIQUES CALCULES -----------------
C =================================================================
      MATERF(4,1) = MU
      MATERF(5,1) = K
C =================================================================
C - CALCUL DES COEFFICIENTS PARABOLIQUES ET SIGMABD --------------
C =================================================================
      MATERF(15,2) = MATERF(10,2)
      MATERF(16,2) = MATERF(11,2)
      GRUP    = MATERF(1,2)
      GRES    = MATERF(2,2)
      MRUP    = MATERF(6,2)
      SRUP    = MATERF(4,2)
      ALPHA   = MATERF(8,2)
      BRES    = MATERF(7,2) - SQRT(SRUP)
      AP      = -BRES / (GRUP - GRES)**2
      DP      = DEUX*BRES*GRES / (GRUP - GRES)**2
      CP      = BRES*GRUP*(GRUP-DEUX*GRES) / (GRUP - GRES)**2
      COHERE  = AP*GRUP**2+DP*GRUP+CP
      IF (ABS(COHERE).GT.EPS) THEN
         CALL U2MESS('F','ALGORITH3_90')
      ENDIF
      COHERE  = AP*GRES**2+DP*GRES+CP
      IF (ABS(COHERE-BRES).GT.EPS) THEN
         CALL U2MESS('F','ALGORITH3_91')
      ENDIF
      SIGBD  = ((MRUP) +SQRT((MRUP)**2 +
     &    4.0D0*((UN-ALPHA)**2)*SRUP)) /
     &     (DEUX*(UN-ALPHA)**2)
C  =================================================================
C --- STOCKAGE DES PARAMETRES PLASTIQUES CALCULES -----------------
C =================================================================
      MATERF(10,2) = BRES
      MATERF(11,2) = AP
      MATERF(12,2) = DP
      MATERF(13,2) = CP
      MATERF(14,2) = SIGBD
      DO 10 II=1,NBMAT
         MATERD(II,1) = MATERF(II,1)
         MATERD(II,2) = MATERF(II,2)
 10   CONTINUE
      MATCST = 'OUI'
C ======================================================================
C --- NOMBRE DE COMPOSANTES --------------------------------------------
C ======================================================================
      IF (MOD(1:2).EQ.'3D') THEN
         NDT = 6
         NDI = 3
      ELSE IF ((MOD(1:6).EQ.'D_PLAN') .OR.
     &        (MOD(1:4).EQ.'AXIS')        ) THEN
         NDT = 4
         NDI = 3
      ELSE IF ((MOD(1:6).EQ.'C_PLAN') .OR.
     &        (MOD(1:2).EQ.'1D')        ) THEN
         CALL U2MESS('F','ALGORITH3_92')
      ELSE
         CALL U2MESS('F','ALGORITH2_20')
      ENDIF
C ======================================================================
C --- NOMBRE DE VARIABLES INTERNES -------------------------------------
C ======================================================================
        NVI = 3
C =================================================================
C - NOMBRE DE CONDITIONS NON-LINEAIRES ----------------------------
C =================================================================
        NR  = NDT + 3
C ======================================================================
        END
