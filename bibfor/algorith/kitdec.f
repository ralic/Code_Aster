       SUBROUTINE KITDEC(MOD, COMPOR, NBVARI, YAMEC, YATE, YAP1, YAP2,
     +            MECA, THMC, THER, HYDR, IMATE, DEFGEM, DEFGEP, ADDEME,
     +            ADDEP1, ADDEP2, ADDETE, NDIM, T0, P10, P20, PHI0,
     +            PVP0, DEPSV, EPSV, DEPS, T, P1, P2, DT, DP1, DP2,
     +            GRAT, GRAP1, GRAP2, NVIMEC, NVITH, ADVIME, ADVITH)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 15/03/2004   AUTEUR JOUMANA J.EL-GHARIB 
C RESPONSABLE UFBHHLL C.CHAVANT
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
C TOLE CRP_21
C ======================================================================
      IMPLICIT      NONE
      INTEGER       NBVARI, YAMEC, YATE, YAP1, YAP2, IMATE
      INTEGER       ADDEME, ADDEP1, ADDEP2, ADDETE, NDIM
      INTEGER       NVIMEC, NVITH, ADVIME, ADVITH
      REAL*8        T0, P10, P20, PHI0, PVP0, DEPSV, EPSV, DEPS(6), T
      REAL*8        P1, P2, GRAT(3), GRAP1(3), GRAP2(3), DP1, DP2, DT
      REAL*8        DEFGEM(*), DEFGEP(*) 
      CHARACTER*8   MOD(*)
      CHARACTER*16  COMPOR(*), MECA, THMC, THER, HYDR
C ======================================================================
C --- RECUPERATION DU NOMBRE DE VARIABLES INTERNES ---------------------
C --- ET DES DONNEES INITIALES -----------------------------------------
C ======================================================================
C OUT ENTIER : NVIMEC : NOMBRE DE VARIABLES INTERNES MECANIQUES
C OUT ENTIER : NVITH  = NBVARI - NVIMEC
C OUT ENTIER : ADVIME = 1 
C OUT ENTIER : ADVITH = NVIMEC+1
C ======================================================================
      INTEGER       IBID1, IBID2
      REAL*8        RBID1, RBID2, RBID3, RBID4, RBID5, RBID6, RBID7
      REAL*8        RBID8, RBID9, RBID10, RBID11, RBID12, RBID13, RBID14
      REAL*8        RBID15, RBID16, RBID17, RBID18, RBID19, RBID20
      REAL*8        RBID21, RBID22, RBID23, RBID24, RBID25, RBID26
      REAL*8        RBID27, RBID28, RBID29, RBID30, RBID31, RBID32
      REAL*8        RBID33, RBID34, RBID35, RBID36, RBID37, RBID38
      REAL*8        RBID39, RBID40, RBID41, RBID42, RBID43, RBID44
      REAL*8        RBID45,RBID46,RBID47,RBID48,RBID49,RBID50,RBID51
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      NVIMEC = 0
      NVITH  = 0
      THMC   = COMPOR( 8)
      THER   = COMPOR( 9)
      HYDR   = COMPOR(10)
      MECA   = COMPOR(11)
C ======================================================================
      CALL THMRCP( 'INITIALI', IMATE, THMC, MECA, HYDR, THER,
     +           T0, P10, P20, PHI0, PVP0, RBID1, RBID2, 
     +           RBID3, RBID4,RBID5, RBID6,
     +           RBID7, RBID8, RBID9, RBID10, RBID11,RBID12, 
     +           RBID13, RBID14,
     +           RBID15, RBID16, RBID17,RBID18, RBID19, RBID20,
     +           RBID21, RBID22, RBID23,RBID24, RBID25, RBID26,
     +           RBID27, RBID28, RBID29,RBID30, RBID31, RBID32,
     +           RBID33, RBID34, RBID35,RBID36, RBID37, RBID38,
     +           RBID39, RBID40, RBID41,RBID42, RBID43, RBID44,RBID45,
     +           RBID46,RBID47,RBID48,RBID49,RBID50,RBID51,RBID5) 
C ======================================================================
      IF (YAMEC.EQ.1) THEN
         IF(MECA.EQ.'ELAS') THEN
            NVIMEC = 0
         ELSE IF(MECA.EQ.'CJS') THEN
            CALL CJSNVI ( MOD, IBID1, IBID2, NVIMEC )
         ELSE IF(MECA.EQ.'CAM_CLAY ') THEN
            NVIMEC = 2
         ELSE IF(MECA.EQ.'BARCELONE') THEN
            NVIMEC = 5
         ELSE IF(MECA.EQ.'LAIGLE') THEN
            CALL LGLNVI ( MOD, IBID1, IBID2, NVIMEC )
         ELSE IF(MECA.EQ.'MAZARS') THEN
            NVIMEC = 3
         ELSE IF(MECA.EQ.'ENDO_ISOT_BETON') THEN
            NVIMEC = 2
         ELSE IF(MECA.EQ.'ELAS_THM') THEN
            NVIMEC = 0
         ELSE IF(MECA.EQ.'SURF_ETAT_SATU') THEN
            NVIMEC = 0
         ELSE IF(MECA.EQ.'SURF_ETAT_NSAT') THEN
            NVIMEC = 0
         ELSE IF(MECA.EQ.'CAM_CLAY_THM') THEN
            NVIMEC = 6
         ELSE IF(MECA.EQ.'DRUCKER_PRAGER') THEN
            NVIMEC = 3
         ELSE
            CALL UTMESS('F','KITDEC','COMPOR MECA NON VALIDE')
         ENDIF
      ENDIF
C ======================================================================
      NVITH = NBVARI - NVIMEC
      ADVIME = 1
      ADVITH = 1 + NVIMEC
C ======================================================================
C --- CALCUL DES VARIABLES ---------------------------------------------
C ======================================================================
      CALL CALCVA(  YAMEC, YATE, YAP1, YAP2, DEFGEM, DEFGEP,
     +              ADDEME, ADDEP1, ADDEP2, ADDETE, NDIM,
     +              T0, P10, P20, DEPSV, EPSV, DEPS, T, P1, P2,
     +              GRAT, GRAP1, GRAP2, DP1, DP2, DT )
C ======================================================================
      END
