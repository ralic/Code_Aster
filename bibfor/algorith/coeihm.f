      SUBROUTINE  COEIHM(OPTION,PERMAN,RESI,RIGI,IMATE,COMPOR,
     &                    CRIT,INSTAM,INSTAP,NOMAIL,
     &                    NDIM,DIMDEF,DIMCON,NBVARI,YAMEC,YAP1,YAP2,
     &                    YATE,NBPHA1,NBPHA2,ADDEME,ADCOME,
     &                    ADDEP1,ADCP11,ADCP12,ADDLH1,ADCOP1,
     &                    ADDEP2,ADCP21,ADCP22,ADCOP2,ADDETE,ADCOTE,
     &                    DEFGEM,DEFGEP,KPI,NPG,NPI,SIGM,SIGP,VARIM,
     &                    VARIP,RES,DRDE,RETCOM)

      IMPLICIT NONE


C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/05/2010   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
C ======================================================================

C - VARIABLES ENTREE  
      INTEGER DIMDEF,DIMCON,NPG,KPI,NPI,NDIM,NBPHA1,NBPHA2
      INTEGER NBVARI,YAMEC,YATE,YAP1,YAP2,IMATE
      INTEGER ADDEME,ADDEP1,ADDEP2,ADDETE,ADCOP1,ADCOP2,ADDLH1
      INTEGER ADCOME,ADCP11,ADCP12,ADCP21,ADCP22,ADCOTE

      REAL*8 DEFGEM(1:DIMDEF),DEFGEP(1:DIMDEF)
      REAL*8 VARIM(NBVARI),INSTAM,INSTAP,CRIT(*)
      REAL*8 SIGM(DIMCON)
      CHARACTER*8   NOMAIL
      CHARACTER*16  OPTION,COMPOR(*)
      LOGICAL PERMAN,RESI,RIGI

C - VARIABLES SORTIE
      INTEGER RETCOM
      REAL*8 SIGP(DIMCON),VARIP(NBVARI)
      REAL*8 RES(DIMDEF),DRDE(DIMDEF,DIMDEF)

C - VARIABLES LOCALES
      INTEGER NVIM,NVIT,NVIH,NVIC,ADVIME,ADVITH,ADVIHY,ADVICO
      INTEGER I,J,F
      INTEGER VIHRHO,VICPHI,VICPVP,VICSAT
      INTEGER IFA,VICPR1,VICPR2    
      REAL*8 T0,P10,P20,PHI0,PVP0,DEPSV,EPSV,DEPS(6)
      REAL*8 T,P1,P2,DT,DP1,DP2,GRAT(3),GRAP1(3),GRAP2(3)
      REAL*8 PVP,PAD,H11,H12,KH,RHO11,PHI
      REAL*8 SAT,BIOT,RGAZ,SATUR,DSATUR,PESA(3)    
      REAL*8 PERMFH, PERMLI, DPERML, PERMGZ,DPERMS,DPERMP,FICK,DFICKT
      REAL*8 LAMBP,DLAMBP, UNSURK, ALPHA, LAMBS, DLAMBS, VISCL,DFICKG
      REAL*8 DVISCL, MAMOLG, LAMBT, DLAMBT, VISCG, DVISCG
      REAL*8 MAMOVG, FICKAD, DFADT, LAMBCT,ISOT(6)
      REAL*8 DFICKS
      REAL*8 DSDE(DIMCON,DIMDEF)
      REAL*8 TLINT,OUVH,DELTAT
      REAL*8 VALCEN(14,6)
      INTEGER MAXFA
      PARAMETER (MAXFA=6)
      REAL*8 VALFAC(MAXFA,14,6)
      CHARACTER*16 MECA,THMC,THER,HYDR
      LOGICAL VF

C
C =====================================================================
C.......................................................................
C
C     BUT:  INTEGRATION DES LOIS DE COMPORTEMENT
C
C     L'INTEGRATION DES LOIS DE COMPORTEMENT THM ET D'INTERFACE
C     PERMET DE CALCULER LES CONTRAINTES GENERALISES ET LES VARIABLES 
C     INTERNES EN CHAQUE POINT D'INTEGRATION
C     LA ROUTINE RENVOIE EGALEMENT LES RESIDUS ET L'OPERATEUR TANGENT EN
C     FONCTION DU POINT D'INTEGRATION
C.......................................................................
C =====================================================================
C IN OPTION : OPTION DE CALCUL
C IN PERMAN : PERMANENT ?
C IN RESI   : FULL_MECA OU RAPH_MECA ?
C IN RIGI   : FULL_MECA OU RIGI_MECA ?
C IN IMATE  : MATERIAU CODE
C IN COMPOR : COMPORTEMENT
C IN CRIT   : CRITERES DE CONVERGENCE LOCAUX
C IN INSTAM : TEMPS MOINS
C IN INSTAP : TEMPS PLUS
C IN NOMAIL : NUMERO DE MAILLE
C IN NDIM   : DIMENSION DE L'ESPACE
C IN DIMDEF : DIMENSION DU TABLEAU DES DEFORMATIONS GENERALISEES
C             AU POINT DE GAUSS CONSIDERE
C IN DIMCON : DIMENSION DU TABLEAU DES CONTRAINTES GENERALISEES
C             AU POINT DE GAUSS CONSIDERE
C IN NBVARI :
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
C IN ADCP12 : ADRESSE DES CONTRAINTES FLUIDE 1 PHASE 2
C IN ADCOP1 : ADRESSE DES CONTRAINTES CORRESPONDANT AU SAUT DE PRE1
C IN ADCP21 : ADRESSE DES CONTRAINTES FLUIDE 2 PHASE 1
C IN ADCP22 : ADRESSE DES CONTRAINTES FLUIDE 2 PHASE 2
C IN ADCOP2 : ADRESSE DES CONTRAINTES CORRESPONDANT AU SAUT DE PRE2
C IN ADCOTE : ADRESSE DES CONTRAINTES THERMIQUES
C IN DEFGEM : DEFORMATIONS GENERALISEES A L'INSTANT MOINS
C IN DEFGEP : DEFORMATIONS GENERALISEES A L'INSTANT PLUS
C IN SIGM   : CONTRAINTES GENERALISEES A L'INSTANT MOINS
C IN VINTM  : VARIABLES INTERNES A L'INSTANT MOINS
C IN KPI    : POINT D'INTEGRATION
C IN NPG    : NOMBRE DE POINTS DE GAUSS
C IN NPI    : NOMBRE DE POINTS D'INTEGRATION
C ====================================================================
C OUT SIGP  : CONTRAINTES GENERALISES
C OUT VARIP : VARIABLES INTERNES : 
C --- VARIABLES 1 A NVIM : VAR. INT MECANIQUES (VOIR LOI DE 
C                          COMPORTEMENT MECANIQUE) 
C --- VARIABLES NVIM+1 A NVIM+NVIH : VAR. INT HYDRAULIQUES
C                                    V1 RHO_LIQUIDE - RHO_0 
C --- VARIABLES NVIM+NVIH+1 A NVIM+NVIH+NVIC : VAR. INT COUPLAGES
C                        : V1 : OUVERTURE DE FISSURE
C                        : V2 : PVP - PVP_0 SI VAPEUR 
C                        : V3 : SATURATION SI LOI NON SATUREE 
C OUT RES   : RESIDU AU POINT D'INTEGRATION
C OUT DRDE  : OPERATEUR TANGENT AU POINT D'INTEGRATION
C OUT RETCOM: RETOUR LOI DE COPORTEMENT
C =====================================================================

C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      RETCOM = 0
      DELTAT = INSTAP-INSTAM
      VF = .FALSE.

      IF (RESI) THEN
        DO 1 I=1,NBVARI
          VARIP(I)=0.D0
 1      CONTINUE
        DO 2 I=1,DIMCON
          SIGP(I)=0.D0
 2      CONTINUE
      END IF

        IF (RIGI) THEN
          DO 811 I=1,DIMDEF
            DO 812 J=1,DIMCON
              DSDE(J,I)=0.D0
 812        CONTINUE
 811      CONTINUE

          DO 10 I=1,DIMDEF
            DO 11 J=1,DIMDEF
              DRDE(I,J)=0.D0
 11         CONTINUE
 10       CONTINUE
        END IF


C ======================================================================
C --- MISE AU POINT POUR LES VARIABLES INTERNES ------------------------
C --- DEFINITION DES POINTEURS POUR LES DIFFERENTES RELATIONS DE -------
C --- COMPORTEMENTS ET POUR LES DIFFERENTES COMPOSANTES ----------------
C ======================================================================
      CALL NVITHM(COMPOR, MECA, THMC, THER, HYDR, NVIM, NVIT,
     +            NVIH, NVIC, ADVIME, ADVITH, ADVIHY, ADVICO,
     +            VIHRHO, VICPHI, VICPVP, VICSAT,VICPR1,VICPR2)
     
C - TEST LOI DE COMPORTEMENT

        IF (MECA.NE.'JOINT_BANDIS') THEN
          CALL U2MESK('F','ALGORITH17_10',1,MECA)
        END IF     

C ======================================================================
C --- CALCULS MECA -----------------------------------------------------
C ======================================================================
      CALL COEIME(MECA,IMATE,NOMAIL,RESI,RIGI,NDIM,DIMDEF,DIMCON,
     &                  YAP1,YAP2,YATE,ADDEME,ADDEP1,ADDEP2,
     &                  NBVARI,ADVIME,ADVICO,NPG,DEFGEP,DEFGEM,
     &                  SIGM,SIGP,VARIM,VARIP,OUVH,TLINT,
     &                  DRDE,KPI,RETCOM)



      IF (RETCOM.NE.0) THEN
         GOTO 9000
      END IF
C ======================================================================
C --- RECUPERATION DES DONNEES INITIALES -------------------------------
C ======================================================================
      CALL KITDEC(0, YATE, YAP1, YAP2, MECA, THMC, THER, HYDR,
     +                   IMATE,DEFGEM,DEFGEP, ADDEME, ADDEP1, ADDEP2,
     +                   ADDETE, NDIM-1, T0, P10, P20, PHI0, PVP0,
     +                   DEPSV, EPSV, DEPS, T, P1, P2, DT, DP1, DP2,
     +                   GRAT, GRAP1, GRAP2, RETCOM,INSTAP)

      EPSV = 0.D0
      DEPSV = 0.D0
      PHI0=0.D0

C ======================================================================
C --- CALCUL DES RESIDUS ET DES MATRICES TANGENTES ---------------------
C ======================================================================

      CALL CALCCO(OPTION,PERMAN,MECA,THMC,THER,HYDR,IMATE,
     +                    NDIM-1,DIMDEF,DIMCON,NBVARI,2,
     +                    YATE,ADDEME,ADCOME,ADVIHY,
     +                    ADVICO,ADDEP1,ADCP11,ADCP12,ADDEP2,ADCP21,
     +                    ADCP22,ADDETE,ADCOTE,SIGM,SIGP,VARIM,
     +                    VARIP,DSDE,DEPS,EPSV,DEPSV,P1,P2,DP1,DP2,
     +                    T,DT,PHI,
     +                    PVP,PAD,H11,H12,KH,RHO11,PHI0,PVP0,
     +                    SAT,RETCOM,CRIT,BIOT,
     +                    VIHRHO,VICPHI,VICPVP,VICSAT,INSTAP)

      IF (RETCOM.NE.0) THEN
         GOTO 9000
      END IF
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU FINALES ------------------------
C ======================================================================
      CALL THMLEC(IMATE, THMC, MECA, HYDR, THER, T, P1, P2,
     +               PHI, VARIP(1), PVP, PAD, RGAZ, BIOT, SATUR,
     +               DSATUR, PESA, PERMFH, PERMLI, DPERML, PERMGZ,
     +               DPERMS, DPERMP, FICK, DFICKT, DFICKG, LAMBP,
     +               DLAMBP, UNSURK, ALPHA, LAMBS, DLAMBS, VISCL,
     +               DVISCL, MAMOLG, LAMBT, DLAMBT, VISCG, DVISCG,
     +               MAMOVG, FICKAD, DFADT, LAMBCT,ISOT,
     +               DFICKS,INSTAP)

C ======================================================================
C --- CALCUL DES FLUX HYDRAULIQUES -------------------------------------
C ======================================================================

      FICK = 0.D0
      DFICKT = 0.D0  
      DFICKG = 0.D0 
      DPERML = 0.D0

      IF (YAP1.EQ.1) THEN
          CALL CALCFH(OPTION,PERMAN,THMC,NDIM-1,DIMDEF,DIMCON,YAMEC,
     &                YATE,ADDEP1,ADDEP2,ADCP11,ADCP12,ADCP21,
     &                ADCP22,ADDEME,ADDETE,SIGP,DSDE,P1,P2,GRAP1,
     &                GRAP2,T,GRAT,PVP,PAD,RHO11,H11,H12,RGAZ,DSATUR,
     &                PESA,TLINT,PERMLI,DPERML,PERMGZ,DPERMS,DPERMP,
     &                FICK,DFICKT,DFICKG,FICKAD,DFADT,KH,UNSURK,
     &                ALPHA,VISCL,DVISCL,MAMOLG,VISCG,DVISCG,
     &                MAMOVG,ISOT,
     &                DFICKS,VF,IFA,VALFAC,VALCEN)
          IF ( RETCOM.NE.0) THEN
             GOTO 9000
          ENDIF
      ENDIF

C ======================================================================
C --- CONTRAINTES GENERALISEES -----------------------------------------
C ======================================================================

         IF (RESI) THEN
C - COMPOSANTES CONSTITUANT 1
           IF (YAP1 .EQ. 1) THEN  
             SIGP(ADCP11+1) = OUVH*SIGP(ADCP11+1)  
             DO 305 F = 1,2
               SIGP(ADCOP1+F-1) = DEFGEP(ADDLH1+1+F)
               SIGP(ADCOP1+F+1)= DEFGEP(ADDLH1-1+F)-DEFGEP(ADDEP1)
 305         CONTINUE 
           END IF

C ======================================================================
C --- CALCUL DU VECTEUR FORCE INTERNE AUX POINTS DE GAUSS --------------
C ======================================================================
           DO 410 I=1,DIMDEF
             RES(I)=0.D0
 410       CONTINUE

         IF (KPI .LE. NPG) THEN
C - COMPOSANTES MECANIQUES
           DO 420 I = 1,NDIM
             RES(I) = SIGP(I)
 420       CONTINUE
              RES(1) = RES(1)+SIGP(NDIM+1)

C - COMPOSANTES CONSTITUANT 1
           IF (YAP1 .EQ. 1) THEN  
             RES(ADDEP1) = DELTAT*(SIGP(ADCOP1)+SIGP(ADCOP1+1))
             DO 421 J = 1,NDIM-1 
               RES(ADDEP1+J) = DELTAT*SIGP(ADCP11+J)
 421         CONTINUE    
             DO 422 F = 1,2
               RES(ADDLH1+F-1) = -DELTAT*SIGP(ADCOP1+F-1)
               RES(ADDLH1+F+1) = SIGP(ADCOP1+F+1)
 422         CONTINUE 
           END IF      
         END IF
C ======================================================================
C --- CALCUL DU VECTEUR FORCE INTERNE AUX SOMMETS --------------
C ======================================================================
         IF ((KPI .GT. NPG) .OR. (NPI .EQ. NPG)) THEN

C - COMPOSANTES CONSTITUANT 1
           IF (YAP1 .EQ. 1) THEN  
             RES(ADDEP1) = RES(ADDEP1) - SIGP(ADCP11)
             RES(ADDEP1) = RES(ADDEP1) + SIGM(ADCP11)
           END IF
         END IF
      END IF
C - FIN DE L'OPTION RESI

C ======================================================================
C --- CALCUL DE L'OPERATEUR TANGENT ------------------------------------
C ======================================================================
      IF (RIGI) THEN

C ======================================================================
C --- D(RESIDU)/D(DEFORMATIONS GENERALISES)------------ 
C --- POUR MATRICE DE RIGIDITE------------------------------------------
C ======================================================================
        IF (KPI .LE. NPG) THEN
  
C - LIGNES CORRESPONDANT AU SAUT DE U ET A LAMBDA

         IF (YAP1 .EQ. 1) THEN 
           DRDE(ADDEME,ADDEP1)=-1.D0
         END IF

C - LIGNES CORRESPONDANT AUX TERMES HYDRAULIQUES

        IF (YAP1 .EQ. 1) THEN
          DO 510 F=1,2
            DRDE(ADDEP1,ADDLH1+1+F)= DELTAT

            DRDE(ADDLH1+F-1,ADDLH1+1+F)=-DELTAT

            DRDE(ADDLH1+F+1,ADDEP1)=-1.D0
            DRDE(ADDLH1+F+1,ADDLH1+F-1)=1.D0
 510      CONTINUE
          DO 511 I=1,NDIM-1          
            DO 512 J=1,NDIM-1
              IF (THMC.EQ.'GAZ') THEN
                DRDE(ADDEP1+I,1) = DRDE(ADDEP1+I,1)
     &      +DELTAT*3.D0*PERMFH*RHO11/VISCG*(-GRAP1(I)+RHO11*PESA(I))
              END IF
              IF (THMC.EQ.'LIQU_SATU') THEN
                DRDE(ADDEP1+I,1) = DRDE(ADDEP1+I,1)
     &      +DELTAT*3.D0*PERMFH*RHO11/VISCL*(-GRAP1(I)+RHO11*PESA(I))
              END IF
                DRDE(ADDEP1+I,ADDEP1)= DRDE(ADDEP1+I,ADDEP1)
     &                +DELTAT*OUVH*DSDE(ADCP11+J,ADDEP1)
              DRDE(ADDEP1+I,ADDEP1+J) = DRDE(ADDEP1+I,ADDEP1+J)
     &           +  DELTAT*OUVH*DSDE(ADCP11+I,ADDEP1+J)
 512        CONTINUE
 511      CONTINUE
        END IF
      END IF
C ======================================================================
C --- D(RESIDU)/D(DEFORMATIONS GENERALISES)------------ 
C --- POUR MATRICE DE MASSE---------------------------------------------
C ======================================================================
        IF ((KPI .GT. NPG) .OR. (NPI .EQ. NPG)) THEN  

          IF (YAP1 .EQ. 1) THEN
            DRDE(ADDEP1,ADDEME) = DRDE(ADDEP1,ADDEME) - RHO11 
            DRDE(ADDEP1,ADDEP1) = DRDE(ADDEP1,ADDEP1)
     &                          + DSDE(ADCP11,ADDEP1) 
          END IF     
        END IF
      END IF

C ======================================================================
 9000 CONTINUE
C ======================================================================

      END
