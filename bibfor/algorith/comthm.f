        SUBROUTINE COMTHM(OPTION,IMATE,TYPMOD,COMPOR,CRIT,INSTAM,INSTAP,
     +                    NDIM,DIMDEF,DIMCON,NBVARI,YAMEC,YAP1,NBPHA1,
     +                    YAP2,NBPHA2,YATE,ADDEME,ADCOME,ADDEP1,ADCP11,
     +                    ADCP12,ADDEP2,ADCP21,ADCP22,ADDETE,ADCOTE,
     +                    DEFGEM,DEFGEP,CONGEM,CONGEP,VINTM,VINTP,
     +                    DSDE,PESA,RETCOM)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 28/09/2004   AUTEUR GRANET S.GRANET 
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
C
C VERSION DU 07/06/99  ECRITE PAR PASCAL CHARLES
C ROUTINE COMTHM
C CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE AU POINT
C DE GAUSS SUIVANT LES OPTIONS DEFINIES
C
C **********************************************************************
C               CRIT    CRITERES  LOCAUX
C                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
C                                 (ITER_INTE_MAXI == ITECREL)
C                       CRIT(2) = TYPE DE JACOBIEN A T+DT
C                                 (TYPE_MATR_COMP == MACOMP)
C                                 0 = EN VITESSE     > SYMETRIQUE
C                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
C                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
C                                 (RESI_INTE_RELA == RESCREL)
C                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
C                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
C                                 (RESI_INTE_PAS == ITEDEC )
C                                 0 = PAS DE REDECOUPAGE
C                                 N = NOMBRE DE PALIERS
C ======================================================================
C IN OPTION : OPTION DE CALCUL
C IN COMPOR : COMPORTEMENT
C IN IMATE  : MATERIAU CODE
C IN NDIM   : DIMENSION DE L'ESPACE
C IN DIMDEF : DIMENSION DU TABLEAU DES DEFORMATIONS GENERALISEES
C             AU POINT DE GAUSS CONSIDERE
C IN DIMCON : DIMENSION DU TABLEAU DES CONTRAINTES GENERALISEES
C             AU POINT DE GAUSS CONSIDERE 
C IN NBVARI : NOMBRE TOTAL DE VARIABLES INTERNES AU POINT DE GAUSS
C IN YAMEC  : =1 S'IL Y A UNE EQUATION DE DEFORMATION MECANIQUE
C IN YAP1   : =1 S'IL Y A UNE EQUATION DE PRESSION DE FLUIDE
C IN YAP2   : =1 S'IL Y A UNE DEUXIEME EQUATION DE PRESSION DE FLUIDE
C IN YATE   : =1 S'IL YA UNE EQUATION THERMIQUE
C IN NBPHA1 : NOMBRE DE PHASES DU FLUIDE 1
C IN NBPHA2 : NOMBRE DE PHASES DU FLUIDE 2
C IN ADDEME : ADRESSE DES DEFORMATIONS MECANIQUES
C IN ADDEP1 : ADRESSE DES DEFORMATIONS CORRESPONDANT A LA PRESSION 1
C IN ADDEP2 : ADRESSE DES DEFORMATIONS CORRESPONDANT A LA PRESSION 2
C IN ADDETE : ADRESSE DES DEFORMATIONS THERMIQUES
C IN ADCOME : ADRESSE DES CONTRAINTES MECANIQUES
C IN ADCP11 : ADRESSE DES CONTRAINTES FLUIDE 1 PHASE 1
C IN ADCP11 : ADRESSE DES CONTRAINTES FLUIDE 1 PHASE 2
C IN ADCP11 : ADRESSE DES CONTRAINTES FLUIDE 2 PHASE 1
C IN ADCP11 : ADRESSE DES CONTRAINTES FLUIDE 2 PHASE 2
C IN ADCOTE : ADRESSE DES CONTRAINTES THERMIQUES
C IN DEFGEM : DEFORMATIONS GENERALISEES A L'INSTANT MOINS
C IN DEFGEP : DEFORMATIONS GENERALISEES A L'INSTANT PLUS
C IN CONGEM : CONTRAINTES GENERALISEES A L'INSTANT MOINS
C IN VINTM  : VARIABLES INTERNES A L'INSTANT MOINS
C IN TYPMOD
C
C OUT CONGEP: CONTRAINTES GENERALISEES A L'INSTANT PLUS
C OUT VINTP : VARIABLES INTERNES A L'INSTANT PLUS
C OUT DSDE  : MATRICE TANGENTE CONTRAINTES DEFORMATIONS
C
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C ======================================================================
C VARIABLES IN / OUT
C ======================================================================
      IMPLICIT      NONE
      INTEGER       RETCOM
      INTEGER       NDIM,DIMDEF,DIMCON,NBVARI,IMATE,YAMEC,YAP1,NBPHA1
      INTEGER       YAP2,NBPHA2,YATE,ADDEME,ADDEP1,ADDEP2,ADDETE
      INTEGER       ADCOME,ADCP11,ADCP12,ADCP21,ADCP22,ADCOTE
      REAL*8        DEFGEM(1:DIMDEF),DEFGEP(1:DIMDEF),CONGEP(1:DIMCON)
      REAL*8        CONGEM(1:DIMCON),VINTM(1:NBVARI),VINTP(1:NBVARI)
      REAL*8        DSDE(1:DIMCON,1:DIMDEF),CRIT(*),INSTAM,INSTAP
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  COMPOR(*),OPTION
C ======================================================================
C VARIABLES LOCALES
C ======================================================================
      LOGICAL       CERMES 
      INTEGER       I,NVIM,NVIT,NVIH,NVIC,ADVIME,ADVITH,ADVIHY,ADVICO
      INTEGER       VIHRHO,VICPHI,VICPVP,VICSAT,NVITH
      REAL*8        P1,DP1,GRAP1(3),P2,DP2,GRAP2(3),T,DT,GRAT(3)
      REAL*8        PHI,PVP,PAD,H11,H12,H21,H22,RHO11,EPSV,DEPS(6),DEPSV
      REAL*8        T0,P10,P20,PHI0,PVP0,SAT,RV0
      REAL*8        G1D,G1F,G1C,J1D,J1F,J1C,J2,J3,G2,G3,MAMOVG
      REAL*8        RGAZ, RHOD, CPD, BIOT, SATM, SATUR,DSATUR, PESA(3)
      REAL*8        PERMFH, PERMLI, DPERML, PERMGZ,DPERMS, DPERMP, FICK
      REAL*8        DFICKT, DFICKG, LAMBP,DLAMBP, RHOL, UNSURK
      REAL*8        CPL, LAMBS,DLAMBS, VISCL, DVISCL, CPG, LAMBT
      REAL*8        DLAMBT,VISCG, DVISCG, MAMOLG, CPVG, VISCVG, DVISVG
      REAL*8        FICKAD,DFADT,KH,LAMBCT, ALPHA,ISOT(3)
      CHARACTER*16  MECA,THMC,THER,HYDR
C ======================================================================
C --- MISE AU POINT POUR LES VARIABLES INTERNES ------------------------
C --- DEFINITION DES POINTEURS POUR LES DIFFERENTES RELATIONS DE -------
C --- COMPORTEMENTS ET POUR LES DIFFERENTES COMPOSANTES ----------------
C ======================================================================
      CALL NVITHM(COMPOR, MECA, THMC, THER, HYDR, NVIM, NVIT,
     +                   NVIH, NVIC, ADVIME, ADVITH, ADVIHY, ADVICO,
     +                   VIHRHO, VICPHI, VICPVP, VICSAT)
C ======================================================================
C --- RECUPERATION DES DONNEES INITIALES -------------------------------
C ======================================================================
      CALL KITDEC(YAMEC, YATE, YAP1, YAP2, MECA, THMC, THER,
     +                   HYDR, IMATE, DEFGEM, DEFGEP, ADDEME, ADDEP1,
     +                   ADDEP2, ADDETE, NDIM, T0, P10, P20, PHI0, PVP0,
     +                   DEPSV, EPSV, DEPS, T, P1, P2, DT, DP1, DP2,
     +                   GRAT, GRAP1, GRAP2)
C ======================================================================
C --- INITIALISATION DE CERMES -----------------------------------------
C ======================================================================
      CERMES =  (  (MECA.EQ.'ELAS_THM')       .OR.
     +             (MECA.EQ.'CAM_CLAY_THM')   .OR.
     +             (MECA.EQ.'SURF_ETAT_SATU') .OR.
     +             (MECA.EQ.'SURF_ETAT_NSAT') .OR.
     +             (THMC.EQ.'LIQU_SATU_GAT')  .OR.
     +             (THMC.EQ.'LIQU_NSAT_GAT')       ) 
      IF (CERMES) THEN
         NVITH = NVIT + NVIH + NVIC
      ENDIF
C ======================================================================
C --- CALCUL DES RESIDUS ET DES MATRICES TANGENTES ---------------------
C ======================================================================
      IF (CERMES) THEN
         CALL CACOGT(OPTION,MECA,THMC,HYDR,IMATE,NDIM,DIMDEF,DIMCON,
     +               NVIM,NVITH,YAMEC,YATE,ADDEME,ADCOME,ADVITH,
     +               ADDEP1,ADCP11,ADCP12,ADDEP2,ADCP21,ADDETE,ADCOTE,
     +               CONGEM,CONGEP,VINTM,VINTP,DSDE,EPSV,DEPSV,P1,P2,
     +               DP1,DP2,T,DT,PHI,PVP,H11,H12,H21,RHO11,PHI0,PVP0,
     +               P10,P20,T0,SAT,RV0,G1D,G1F,G1C,J1D,J1F,J1C,J2,J3,
     +               G2,G3)
      ELSE    
         CALL CALCCO(OPTION,MECA,THMC,THER,HYDR,IMATE,
     +                    NDIM,DIMDEF,DIMCON,NBVARI,YAMEC,YAP1,
     +                    NBPHA1,YAP2,NBPHA2,YATE,ADDEME,ADCOME,ADVIHY,
     +                    ADVICO,ADDEP1,ADCP11,ADCP12,ADDEP2,ADCP21,
     +                    ADCP22,ADDETE,ADCOTE,CONGEM,CONGEP,VINTM,
     +                    VINTP,DSDE,DEPS,EPSV,DEPSV,P1,P2,DP1,DP2,
     +                    T,DT,PHI,
     +                    PVP,PAD,H11,H12,H21,H22,KH,RHO11,PHI0,PVP0,
     +                    P10,P20,T0,SAT,RETCOM,CRIT,BIOT,
     +                    VIHRHO,VICPHI,VICPVP,VICSAT)

         IF (RETCOM.NE.0) THEN
            GOTO 9000
         ENDIF
      ENDIF
C ======================================================================
C --- CALCUL DES GRANDEURS MECANIQUES PURES UNIQUEMENT SI YAMEC = 1 ----
C ======================================================================
      IF (YAMEC.EQ.1) THEN 
         CALL CALCME(OPTION,COMPOR,MECA,IMATE,TYPMOD,CRIT,INSTAM,INSTAP,
     +               T0,NDIM,DIMDEF,DIMCON,NVIM,NVITH,YATE,ADDEME,
     +               ADCOME,ADDETE,DEFGEM,CONGEM,CONGEP,VINTM,VINTP,
     +               ADVIME,ADDEP1,ADDEP2,DSDE,DEPS,DEPSV,PHI,
     +               P1,P2,T,DT,PHI0,RETCOM,DP1,DP2,SAT,BIOT)
         IF ( RETCOM.NE.0) THEN
            GOTO 9000
         ENDIF
      ENDIF
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU FINALES ------------------------
C ======================================================================
      IF (.NOT.CERMES) THEN
         CALL THMLEC(IMATE, THMC, MECA, HYDR, THER, T, P1, P2,
     +               PHI, VINTP(1), PVP, PAD, RGAZ, BIOT, SATUR,
     +               DSATUR, PESA, PERMFH, PERMLI, DPERML, PERMGZ,
     +               DPERMS, DPERMP, FICK, DFICKT, DFICKG, LAMBP,
     +               DLAMBP, UNSURK, ALPHA, LAMBS, DLAMBS, VISCL,
     +               DVISCL, MAMOLG, LAMBT, DLAMBT, VISCG, DVISCG,
     +               MAMOVG, FICKAD, DFADT, LAMBCT,ISOT)
      ENDIF
C ======================================================================
C --- CALCUL DES FLUX HYDRAULIQUES UNIQUEMENT SI YAP1 = 1 --------------
C ======================================================================
      IF (YAP1.EQ.1) THEN
         IF (CERMES) THEN
            CALL CAFHGT(OPTION,THMC,HYDR,IMATE,NDIM,DIMDEF,
     +                  DIMCON,NVIM,NVITH,YAMEC,YATE,ADDEP1,ADDEP2,
     +                  ADCP11,ADCP12,ADCP21,ADDEME,ADDETE,CONGEP,
     +                  DSDE,P1,P2,GRAP1,GRAP2,T,GRAT,PHI,PVP,RHO11,
     +                  H11,H12,T0,SAT,RV0,G1F,J1D,J1F,J1C,
     +                  J2,J3,G2,PESA)
         ELSE
             CALL CALCFH(OPTION,MECA,THMC,THER,HYDR,IMATE,NDIM,DIMDEF, 
     +                    DIMCON,YAMEC,YATE,ADDEP1,ADDEP2, 
     +                    ADCP11,ADCP12,ADCP21,ADCP22,ADDEME,ADDETE,
     +                    CONGEM,CONGEP,DSDE,P1,P2,
     +                    GRAP1,GRAP2,T,GRAT,PHI,PVP,PAD,RHO11,H11,H12,
     +                    H21,H22,RGAZ, RHOD, CPD, BIOT, SATUR, DSATUR,
     +                    PESA, PERMFH, PERMLI, DPERML, PERMGZ, DPERMS,
     +                    DPERMP, FICK, DFICKT, DFICKG,FICKAD,DFADT,
     +                    LAMBP, DLAMBP,KH,
     +                    RHOL, UNSURK, ALPHA,  CPL, LAMBS,
     +                    DLAMBS,
     +                    VISCL, DVISCL, MAMOLG,CPG, LAMBT, DLAMBT,
     +                    VISCG,DVISCG, MAMOVG,
     +                    CPVG, VISCVG, DVISVG, RETCOM,ISOT)
            IF ( RETCOM.NE.0) THEN
               GOTO 9000
            ENDIF
         ENDIF
      ENDIF
C ======================================================================
C --- CALCUL DU FLUX THERMIQUE UNIQUEMENT SI YATE = 1 ------------------
C ======================================================================
      IF (YATE.EQ.1) THEN
         IF (CERMES) THEN
            CALL CAFTGT(OPTION,THMC,HYDR,IMATE,NDIM,DIMDEF,
     +                  DIMCON,NVIM,NVITH,YAMEC,YAP1,YAP2,
     +                  ADDETE,ADDEME,ADDEP1,ADDEP2,ADCOTE,
     +                  CONGEP,DSDE,P1,T,GRAT,SAT)
         ELSE

            CALL CALCFT(OPTION,MECA,THMC,HYDR,IMATE,NDIM,DIMDEF,
     +                 DIMCON,YAMEC,YAP1,NBPHA1,YAP2,
     +                 NBPHA2,YATE,ADDETE,ADDEME,ADDEP1,ADDEP2,
     +                 ADCOTE,CONGEM,CONGEP,
     +                 DSDE,EPSV,P1,P2,T,GRAT,PHI,SAT,PVP,
     +                 RGAZ, RHOD, CPD, BIOT, SATM, SATUR,
     +                 DSATUR, PESA, PERMFH, PERMLI, DPERML, PERMGZ,
     +                 DPERMS, DPERMP, FICK, DFICKT, DFICKG, LAMBP,
     +                 DLAMBP, RHOL, UNSURK, ALPHA, CPL, LAMBS,
     +                 DLAMBS, VISCL, DVISCL, MAMOLG, CPG, LAMBT,
     +                 DLAMBT,
     +                 VISCG, DVISCG, MAMOVG, CPVG, VISCVG, DVISVG,
     +                 RETCOM,LAMBCT,RHO11,H11,H12)
            IF ( RETCOM.NE.0) THEN
               GOTO 9000
            ENDIF
         ENDIF 
      ENDIF
C ======================================================================
 9000 CONTINUE
C ======================================================================
      END
