        SUBROUTINE LGLMAT ( MOD, IMAT, NBMAT, TEMPD, TEMPF, MATERD,
     &                      MATERF, MATCST, NDT, NDI, NR, NVI)
C
        IMPLICIT     NONE
        INTEGER      NDT, NDI, NR, NVI, IMAT, NBMAT
        REAL*8       MATERD(NBMAT,2), MATERF(NBMAT,2), TEMPD, TEMPF
        CHARACTER*3  MATCST
        CHARACTER*8  MOD
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C =================================================================
C |---------------------------------------------------------------|
C |-- BUT : RECUPERATION DES DONNEES MATERIAU POUR LA LOI DE -----|
C |------ : COMPORTEMENT DE LAIGLE (MECANIQUE DES ROCHES) --------|
C |---------------------------------------------------------------|
C |----- NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES ------|
C |----- MATER(*,1) = E, NU, MU, K -------------------------------|
C |----- MATER(*,2) = GAMMA_ULT, GAMMA_E, M_ULT, M_E, A_E, -------|
C |---------------- : M_PIC, A_PIC, ETA, SIGMA_C, GAMMA, ---------|
C |---------------- : KSI, GAMMA_CJS, SIGMA_P1, SIGMA_P2, PA -----|
C |----- VARIABLE INTERNE : GAMMA_P, EPS_P-  ---------------------|
C |---------------------------------------------------------------|
C =================================================================
C IN  : MOD    : TYPE DE MODELISATION -----------------------------
C --- : IMAT   : ADRESSE DU MATERIAU CODE -------------------------
C --- : NBMAT  : NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : TEMPD  : TEMPERATURE BIDON --------------------------------
C --- : TEMPF  : TEMPERATURE BIDON --------------------------------
C OUT : MATERD : COEFFICIENTS MATERIAU A T ------------------------
C --- : MATERF : COEFFICIENTS MATERIAU A T+DT ---------------------
C ------------ : MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ------------ : MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C --- : MATCST : 'OUI' --------------------------------------------
C --- : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR -----------
C --- : NDI    : NOMBRE DE COMPOSANTES DIRECTES DU TENSEUR --------
C --- : NR     : NOMBRE D'EQUATION DU SYSTEME NL ------------------
C --- : NVI    : NB DE VARIABLES INTERNES -------------------------
C =================================================================
      INTEGER         II
      REAL*8          E, NU, MU, K, GAMMA, KSI
      REAL*8          UN, DEUX, TROIS
      CHARACTER*2     CERR(18)
      CHARACTER*8     NOMC(18)
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
C =================================================================
      CALL JEMARQ ()
C =================================================================
C --- NB DE COMPOSANTES / VARIABLES INTERNES ----------------------
C =================================================================
      CALL LGLNVI(MOD, NDT, NDI, NVI)
C =================================================================
C - NOMBRE DE CONDITIONS NON-LINEAIRES ----------------------------
C =================================================================
      NR  = NDT + 3
C =================================================================
C --- DEFINITION DES CHAMPS ---------------------------------------
C =================================================================
      NOMC(1)  = 'E        '
      NOMC(2)  = 'NU       '
      NOMC(3)  = 'ALPHA    '
      NOMC(4)  = 'GAMMA_ULT'
      NOMC(5)  = 'GAMMA_E  '
      NOMC(6)  = 'M_ULT    '
      NOMC(7)  = 'M_E      '
      NOMC(8)  = 'A_E      '
      NOMC(9)  = 'M_PIC    '
      NOMC(10) = 'A_PIC    '
      NOMC(11) = 'ETA      '
      NOMC(12) = 'SIGMA_C  '
      NOMC(13) = 'GAMMA    '
      NOMC(14) = 'KSI      '
      NOMC(15) = 'GAMMA_CJS'
      NOMC(16) = 'SIGMA_P1 '
      NOMC(17) = 'SIGMA_P2 '
      NOMC(18) = 'PA       '
C =================================================================
C --- RECUPERATION DES PARAMETRES MATERIAU ------------------------
C =================================================================
      CALL RCVALA ( IMAT, 'ELAS', 1, 'TEMP', TEMPD, 3,
     +               NOMC(1),  MATERD(1,1),  CERR(1), ' ')
      CALL RCVALA ( IMAT, 'LAIGLE', 1, 'TEMP', TEMPD, 15,
     +               NOMC(4),  MATERD(1,2),  CERR(4), ' ' )
C =================================================================
C - CALCUL DES MODULES DE CISAILLEMENT ET DE DEFORMATION VOLUMIQUE-
C =================================================================
      E     = MATERD(1,1)
      NU    = MATERD(2,1)
      MU    = E / (DEUX*(UN+NU))
      K     = E / (TROIS*(UN-DEUX*NU))
C =================================================================
C - VERIFICATIONS -------------------------------------------------
C =================================================================
      GAMMA = MATERD(13,2)
      KSI   = MATERD(14,2)
      IF ((GAMMA/KSI).GT.UN) THEN
         CALL UTMESS('F','LGLMAT','LA CONDITION GAMMA/KSI <= 1'//
     +               ' N EST PAS RESPECTEE')
      ENDIF
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
      CALL JEDEMA ()
C =================================================================
      END
