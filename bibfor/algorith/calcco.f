       SUBROUTINE CALCCO (OPTION,MECA,THMC,THER,HYDR,IMATE,
     +                    NDIM,DIMDEF,DIMCON,NBVARI,YAMEC,YAP1,
     +                    NBPHA1,YAP2,NBPHA2,YATE,ADDEME,ADCOME,ADVIHY,
     +                    ADVICO,ADDEP1,ADCP11,ADCP12,ADDEP2,ADCP21,
     +                    ADCP22,ADDETE,ADCOTE,CONGEM,CONGEP,VINTM,
     +                    VINTP,DSDE,DEPS,EPSV,DEPSV,P1,P2,DP1,DP2,
     +                    T,DT,PHI,PVP,PAD,H11,H12,H21,H22,KH,RHO11,
     +                    PHI0,PVP0,P10,P20,T0,SAT,RETCOM,CRIT,BIOT,
     +                    VIHRHO,VICPHI,VICPVP,VICSAT)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 17/05/2004   AUTEUR ROMEO R.FERNANDES 
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
C **********************************************************************
C ROUTINE CALCCO : CETTE ROUTINE CALCULE LES CONTRAINTES GENERALISEES
C   ET LA MATRICE TANGENTE DES GRANDEURS COUPLEES, A SAVOIR CELLES QUI
C   NE SONT PAS DES GRANDEURS DE MECANIQUE PURE OU DES FLUX PURS
C   ELLES RENVOIT POUR CELA A DIFFERENTES ROUTINES SUIVANT 
C   LA VALEUR DE THMC
C **********************************************************************
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C COMMENTAIRE DE NMCONV : 
C                       = 0 OK
C                       = 1 ECHEC DANS L'INTEGRATION : PAS DE RESULTATS
C                       = 3 SIZZ NON NUL (DEBORST) ON CONTINUE A ITERER
C ======================================================================
      IMPLICIT      NONE
      INTEGER       NDIM,DIMDEF,DIMCON,NBVARI,IMATE
      INTEGER       YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE
      INTEGER       ADCOME,ADCP11,ADCP12,ADCP21,ADCP22,ADCOTE
      INTEGER       ADDEME,ADDEP1,ADDEP2,ADDETE,RETCOM
      INTEGER       ADVIHY,ADVICO,VIHRHO,VICPHI,VICPVP,VICSAT
      REAL*8        CONGEM(DIMCON),CONGEP(DIMCON)
      REAL*8        VINTM(NBVARI),VINTP(NBVARI)
      REAL*8        DSDE(DIMCON,DIMDEF),EPSV,DEPSV,P1,DP1,P2,DP2,T,DT
      REAL*8        PHI,PVP,PAD,H11,H12,H21,H22,KH,RHO11,PHI0
      REAL*8        PVP0,P10,P20,T0,SAT
      CHARACTER*16  OPTION,MECA,THMC,THER,HYDR
C ======================================================================
C --- VARIABLES LOCALES POUR BARCELONE-------------------------------
C ======================================================================
      REAL*8       DEPS(6),BIOT,CRIT(*)  
      REAL*8       INSTAM,INSTAP
      CHARACTER*8  TYPMOD(2)
      CHARACTER*16  COMPOR
C
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_SATU ----------------------
C ======================================================================
      IF (THMC.EQ.'LIQU_SATU') THEN
          CALL HMLISA(OPTION,MECA,THMC,THER,HYDR,IMATE,NDIM,DIMDEF,
     +                  DIMCON,NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,
     +                  ADVICO,VIHRHO,VICPHI,ADDEP1,ADCP11,ADDETE,
     +                  ADCOTE,CONGEM,CONGEP,VINTM,VINTP,DSDE,EPSV,
     +                  DEPSV,P1,DP1,T,DT,PHI,RHO11,PHI0,SAT,RETCOM)
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE GAZ ----------------------------
C ======================================================================
      ELSE IF (THMC.EQ.'GAZ') THEN
          CALL HMGAZP(OPTION,MECA,THMC,THER,HYDR,IMATE,NDIM,DIMDEF,
     +                  DIMCON,NBVARI,YAMEC,YATE,ADDEME,ADCOME,
     +                  ADVICO,VICPHI,ADDEP1,ADCP11,ADDETE,ADCOTE,
     +                  CONGEM,CONGEP,VINTM,VINTP,DSDE,EPSV,DEPSV,P1,
     +                  DP1,T,DT,PHI,RHO11,PHI0,SAT,RETCOM)
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_VAPE ----------------------
C ======================================================================
      ELSE IF (THMC.EQ.'LIQU_VAPE') THEN
          CALL HMLIVA(OPTION,MECA,THER,HYDR,IMATE,NDIM,DIMDEF,DIMCON,
     +                  NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,ADVICO,
     +                  VIHRHO,VICPHI,VICPVP,VICSAT,ADDEP1,ADCP11,
     +                  ADCP12,ADDETE,ADCOTE,CONGEM,CONGEP,VINTM,VINTP,
     +                  DSDE,EPSV,DEPSV,P1,DP1,T,DT,PHI,PVP,H11,H12,
     +                  RHO11,PHI0,PVP0,SAT,RETCOM,THMC)
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_VAPE_GAZ ------------------
C ======================================================================
      ELSE IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
          CALL HMLVAG(OPTION,MECA,THER,HYDR,IMATE,NDIM,DIMDEF,DIMCON,
     +                  NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,ADVICO,
     +                  VIHRHO,VICPHI,VICPVP,VICSAT,ADDEP1,ADCP11,
     +                  ADCP12,ADDEP2,ADCP21,ADDETE,ADCOTE,CONGEM,
     +                  CONGEP,VINTM,VINTP,DSDE,DEPS,EPSV,DEPSV,P1,P2,
     +                  DP1,DP2,T,DT,PHI,PVP,H11,H12,H21,RHO11,PHI0,
     +                  PVP0,SAT,RETCOM,THMC,CRIT,BIOT)
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_GAZ -----------------------
C ======================================================================
      ELSE IF (THMC.EQ.'LIQU_GAZ') THEN
          CALL HMLIGA(OPTION,MECA,THER,HYDR,IMATE,NDIM,DIMDEF,DIMCON,
     +                  NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,ADVICO,
     +                  VIHRHO,VICPHI,VICSAT,ADDEP1,ADCP11,ADDEP2,
     +                  ADCP21,ADDETE,ADCOTE,CONGEM,CONGEP,VINTM,VINTP,
     +                  DSDE,DEPS,EPSV,DEPSV,P1,P2,DP1,DP2,T,DT,PHI,
     +                  RHO11,PHI0,SAT,RETCOM,THMC,CRIT,BIOT)
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_GAZ_ATM -------------------
C ======================================================================
      ELSE IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
          CALL HMLGAT(OPTION,MECA,THER,HYDR,IMATE,NDIM,DIMDEF,DIMCON,
     +                  NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,ADVICO,
     +                  VIHRHO,VICPHI,VICSAT,ADDEP1,ADCP11,ADDETE,
     +                  ADCOTE,CONGEM,CONGEP,VINTM,VINTP,DSDE,EPSV,
     +                 DEPSV,P1,DP1,T,DT,PHI,RHO11,PHI0,SAT,RETCOM,THMC)
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ_VAPE ---------------
C ======================================================================
      ELSE IF (THMC.EQ.'LIQU_AD_GAZ_VAPE') THEN
          CALL HMLVGA(OPTION,MECA,THER,HYDR,IMATE,NDIM,DIMDEF,DIMCON,
     +                  NBVARI,YAMEC,YATE,ADDEME,ADCOME,ADVIHY,ADVICO,
     +                  VIHRHO,VICPHI,VICPVP,VICSAT,ADDEP1,ADCP11,
     +                  ADCP12,ADDEP2,ADCP21,ADCP22,ADDETE,ADCOTE,
     +                  CONGEM,CONGEP,VINTM,VINTP,DSDE,EPSV,DEPSV,P1,P2,
     +                  DP1,DP2,T,DT,PHI,PAD,PVP,H11,H12,H21,H22,KH,
     +                  RHO11,PHI0,PVP0,SAT,RETCOM,THMC)
C
      ENDIF
 9001  FORMAT(A10,2X,A20,2X,A20,2X,A8)
C
      END
