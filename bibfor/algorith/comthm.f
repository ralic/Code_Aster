        SUBROUTINE COMTHM(OPTION,PERMAN,VF,IFA,VALFAC,VALCEN,
     &                    IMATE,TYPMOD,COMPOR,
     &                    CRIT,INSTAM,INSTAP,
     &                    NDIM,DIMDEF,DIMCON,NBVARI,YAMEC,YAP1,
     &                    YAP2,YATE,ADDEME,ADCOME,ADDEP1,ADCP11,
     &                    ADCP12,ADDEP2,ADCP21,ADCP22,ADDETE,ADCOTE,
     &                    DEFGEM,DEFGEP,CONGEM,CONGEP,VINTM,VINTP,
     &                    DSDE,PESA,RETCOM,KPI,NPG,P10,P20)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 08/02/2011   AUTEUR GRANET S.GRANET 
C RESPONSABLE GRANET S.GRANET
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
C IN PERMAN : TRUE SI PERMANENT
C IN VF : TRUE SI VOLUMES FINIS
C IN IFA : UTILISE EN VF ET POUR LES VALEURS AUX ARETES
C      -> NUMERO DE LA FACE. LES INFORMATIONS SONT STOCKES
C       DS VALFAC(1:6,1:4,1:NBFACE)
C VALFAC : SOCKAGE DES VALEURS CALSULEES AUX ARETES EN VF IFA!=0
C DES VALEURS AU CENTRE
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
C IN TYPMOD : MODELISATION (D_PLAN, AXI, 3D ?)
C
C OUT CONGEP : CONTRAINTES GENERALISEES A L'INSTANT PLUS
C OUT VINTP  : VARIABLES INTERNES A L'INSTANT PLUS
C OUT DSDE   : MATRICE TANGENTE CONTRAINTES DEFORMATIONS
C
C OUT RETCOM : RETOUR LOI DE COMPORTEMENT
C ======================================================================
C VARIABLES IN / OUT
C ======================================================================
      IMPLICIT NONE

      REAL*8        VALCEN(14,6)
      INTEGER       MAXFA
      PARAMETER     (MAXFA=6)
      REAL*8        VALFAC(MAXFA,14,6)
      INTEGER       MASSE,DMASP1,DMASP2
      INTEGER       EAU,AIR
      INTEGER       VKINT,KXX,KYY,KZZ,KXY,KYZ,KZX
C      PARAMETER(CON=1,DCONP1=2,DCONP2=3,DIFFU=4,DDIFP1=5,DDIFP2=6)
C      PARAMETER(MOB=7,DMOBP1=8,DMOBP2=9,MASSE=10,DMASP1=11,DMASP2=12)
C      PARAMETER(RHOGA=1,RHOLQ=2,RHOGA1=3,RHOGA2=4,RHOLQ1=5,RHOLQ2=6)
      PARAMETER     (MASSE=10,DMASP1=11,DMASP2=12)
      PARAMETER     (VKINT=13)
C      PARAMETER(DENSIT=14)
      PARAMETER     (KXX=1,KYY=2,KZZ=3,KXY=4,KYZ=5,KZX=6)
      PARAMETER     (EAU=1,AIR=2)
      INTEGER       RETCOM,KPI,NPG
      INTEGER       NDIM,DIMDEF,DIMCON,NBVARI,IMATE,YAMEC,YAP1
      INTEGER       YAP2,YATE,ADDEME,ADDEP1,ADDEP2,ADDETE
      INTEGER       ADCOME,ADCP11,ADCP12,ADCP21,ADCP22,ADCOTE
      REAL*8        DEFGEM(1:DIMDEF),DEFGEP(1:DIMDEF),CONGEP(1:DIMCON)
      REAL*8        CONGEM(1:DIMCON),VINTM(1:NBVARI),VINTP(1:NBVARI)
      REAL*8        DSDE(1:DIMCON,1:DIMDEF),CRIT(*),INSTAM,INSTAP
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  COMPOR(*),OPTION
      LOGICAL       PERMAN,VF
      INTEGER       IFA
      INTEGER       VICPR1,VICPR2
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       NVIM,ADVIME,ADVITH,ADVIHY,ADVICO
      INTEGER       VIHRHO,VICPHI,VICPVP,VICSAT,NVIH,NVIC,NVIT
      REAL*8        P1,DP1,GRAP1(3),P2,DP2,GRAP2(3),T,DT,GRAT(3)
      REAL*8        PHI,PVP,PAD,H11,H12,RHO11,EPSV,DEPS(6),DEPSV
      REAL*8        T0,P10,P20,PHI0,PVP0,SAT,MAMOVG
      REAL*8        RGAZ, BIOT, SATUR,DSATUR, PESA(3)
      REAL*8        PERMFH, PERMLI, DPERML, PERMGZ,DPERMS, DPERMP, FICK
      REAL*8        DFICKT, DFICKG, LAMBP,DLAMBP, UNSURK
      REAL*8        LAMBS,DLAMBS, VISCL, DVISCL, LAMBT
      REAL*8        DLAMBT,VISCG, DVISCG, MAMOLG
      REAL*8        FICKAD,DFADT,KH,LAMBCT, ALPHA,ISOT(6)
      REAL*8        DFICKS
      REAL*8        DELTAT
      CHARACTER*16  MECA,THMC,THER,HYDR
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      RETCOM = 0
C ======================================================================
C --- MISE AU POINT POUR LES VARIABLES INTERNES ------------------------
C --- DEFINITION DES POINTEURS POUR LES DIFFERENTES RELATIONS DE -------
C --- COMPORTEMENTS ET POUR LES DIFFERENTES COMPOSANTES ----------------
C ======================================================================
      CALL NVITHM(COMPOR, MECA, THMC, THER, HYDR, NVIM, NVIT,
     +            NVIH, NVIC, ADVIME, ADVITH, ADVIHY, ADVICO,
     +            VIHRHO, VICPHI, VICPVP, VICSAT,VICPR1,VICPR2)
C ======================================================================
C --- RECUPERATION DES DONNEES INITIALES -------------------------------
C ======================================================================
      CALL KITDEC(YAMEC, YATE, YAP1, YAP2, MECA, THMC, THER, HYDR,
     +            IMATE, DEFGEM, DEFGEP, ADDEME, ADDEP1, ADDEP2,
     +            ADDETE, NDIM, T0, P10, P20, PHI0, PVP0,
     +            DEPSV, EPSV, DEPS, T, P1, P2, DT, DP1, DP2,
     +            GRAT, GRAP1, GRAP2, RETCOM,INSTAP)
      IF (RETCOM.NE.0) THEN
         GOTO 9000
      ENDIF
C ======================================================================
C --- CALCUL DES RESIDUS ET DES MATRICES TANGENTES ---------------------
C ======================================================================

      CALL CALCCO(OPTION,PERMAN,MECA,THMC,THER,HYDR,IMATE,
     +            NDIM,DIMDEF,DIMCON,NBVARI,YAMEC,
     +            YATE,ADDEME,ADCOME,ADVIHY,
     +            ADVICO,ADDEP1,ADCP11,ADCP12,ADDEP2,ADCP21,
     +            ADCP22,ADDETE,ADCOTE,CONGEM,CONGEP,VINTM,
     +            VINTP,DSDE,DEPS,EPSV,DEPSV,P1,P2,DP1,DP2,
     +            T,DT,PHI,
     +            PVP,PAD,H11,H12,KH,RHO11,PHI0,PVP0,
     +            SAT,RETCOM,CRIT,BIOT,
     +            VIHRHO,VICPHI,VICPVP,VICSAT,INSTAP)

      IF (RETCOM.NE.0) THEN
         GOTO 9000
      ENDIF
C
C VOLUMES FINIS
C
      IF(VF.AND.(IFA.EQ.0)) THEN
       DELTAT=INSTAP-INSTAM
       IF((OPTION(1:9).EQ.'FULL_MECA').OR.
     & (OPTION(1:9).EQ.'RAPH_MECA')) THEN

        VALCEN(MASSE ,EAU)=( CONGEP(ADCP11)+CONGEP(ADCP12)
     >              -CONGEM(ADCP11)-CONGEM(ADCP12))/DELTAT
        VALCEN(MASSE ,AIR)=( CONGEP(ADCP21)+CONGEP(ADCP22)
     >              -CONGEM(ADCP21)-CONGEM(ADCP22))/DELTAT
C
       ENDIF
       IF ( (OPTION(1:9) .EQ. 'RIGI_MECA') .OR.
     & (OPTION(1:9) .EQ. 'FULL_MECA') ) THEN
                   VALCEN(DMASP1,EAU)=
     > (DSDE(ADCP11,ADDEP1)+ DSDE(ADCP12,ADDEP1))/DELTAT
                   VALCEN(DMASP2,EAU)=
     > (DSDE(ADCP11,ADDEP2)+ DSDE(ADCP12,ADDEP2))/DELTAT
                   VALCEN(DMASP1,AIR)=
     > (DSDE(ADCP22,ADDEP1)+ DSDE(ADCP21,ADDEP1))/DELTAT
                   VALCEN(DMASP2,AIR)=
     > (DSDE(ADCP22,ADDEP2)+ DSDE(ADCP21,ADDEP2))/DELTAT
       ENDIF

      ENDIF

C ======================================================================
C --- CALCUL DES GRANDEURS MECANIQUES PURES UNIQUEMENT SI YAMEC = 1 -
C ET SI ON EST SUR UN POINT DE GAUSS (POUR L'INTEGRATION REDUITE)
C  C'EST A DIRE SI KPI<NPG
C ======================================================================
      IF (YAMEC.EQ.1 .AND. KPI .LE. NPG) THEN
         CALL CALCME(OPTION,COMPOR,THMC,MECA,IMATE,TYPMOD,CRIT,
     +               INSTAM,INSTAP,
     +               T0,NDIM,DIMDEF,DIMCON,NVIM,YATE,ADDEME,
     +               ADCOME,ADDETE,DEFGEM,CONGEM,CONGEP,VINTM,VINTP,
     +               ADDEP1,ADDEP2,DSDE,DEPS,DEPSV,
     +               P1,P2,T,DT,RETCOM,DP1,DP2,SAT,BIOT)
         IF ( RETCOM.NE.0) THEN
            GOTO 9000
         ENDIF
      ENDIF
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU FINALES ------------------------
C ======================================================================
      CALL THMLEC(IMATE, THMC, MECA, HYDR, THER, T, P1, P2,
     +            PHI, VINTP(1), PVP, PAD, RGAZ, BIOT, SATUR,
     +            DSATUR, PESA, PERMFH, PERMLI, DPERML, PERMGZ,
     +            DPERMS, DPERMP, FICK, DFICKT, DFICKG, LAMBP,
     +            DLAMBP, UNSURK, ALPHA, LAMBS, DLAMBS, VISCL,
     +            DVISCL, MAMOLG, LAMBT, DLAMBT, VISCG, DVISCG,
     +            MAMOVG, FICKAD, DFADT, LAMBCT,ISOT,
     >            DFICKS,INSTAP)


C CONDUCTIVITES EN VF
C
      IF(VF.AND.(IFA.EQ.0)) THEN
       VALCEN(VKINT ,KXX)=PERMFH*ISOT(1)
       VALCEN(VKINT ,KYY)=PERMFH*ISOT(2)
       VALCEN(VKINT ,KZZ)=PERMFH*ISOT(3)
       VALCEN(VKINT ,KXY)=PERMFH*ISOT(4)
       VALCEN(VKINT ,KYZ)=PERMFH*ISOT(5)
       VALCEN(VKINT ,KZX)=PERMFH*ISOT(6)
      ENDIF
C ======================================================================
C --- CALCUL DES FLUX HYDRAULIQUES UNIQUEMENT SI YAP1 = 1 --------------
C ======================================================================
      IF (YAP1.EQ.1) THEN
          CALL CALCFH(OPTION,PERMAN,THMC,NDIM,DIMDEF,DIMCON,YAMEC,
     &                YATE,ADDEP1,ADDEP2,ADCP11,ADCP12,ADCP21,
     &                ADCP22,ADDEME,ADDETE,CONGEP,DSDE,P1,P2,GRAP1,
     &                GRAP2,T,GRAT,PVP,PAD,RHO11,H11,H12,RGAZ,DSATUR,
     &                PESA,PERMFH,PERMLI,DPERML,PERMGZ,DPERMS,DPERMP,
     &                FICK,DFICKT,DFICKG,FICKAD,DFADT,KH,UNSURK,
     &                ALPHA,VISCL,DVISCL,MAMOLG,VISCG,DVISCG,
     &                MAMOVG,ISOT,
     &                DFICKS,VF,IFA,VALFAC,VALCEN)
          IF ( RETCOM.NE.0) THEN
             GOTO 9000
          ENDIF
      ENDIF
C ======================================================================
C --- CALCUL DU FLUX THERMIQUE UNIQUEMENT SI YATE = 1 ------------------
C ======================================================================
      IF (YATE.EQ.1) THEN
         CALL CALCFT(OPTION,THMC  ,IMATE ,NDIM  ,DIMDEF,
     +               DIMCON,YAMEC ,YAP1  ,YAP2  ,
     +               ADDETE,ADDEME,ADDEP1,ADDEP2,
     +               ADCOTE,CONGEP,
     +               DSDE  ,T     ,GRAT  ,PHI   ,PVP   ,
     +               RGAZ  ,BIOT  ,SATUR ,DSATUR,
     +               LAMBP ,DLAMBP,LAMBS ,DLAMBS,LAMBT ,
     +               DLAMBT,MAMOVG,
     +               LAMBCT,RHO11,H11,H12)
         IF ( RETCOM.NE.0) THEN
            GOTO 9000
         ENDIF
      ENDIF
C ======================================================================
 9000 CONTINUE
C ======================================================================
      END
