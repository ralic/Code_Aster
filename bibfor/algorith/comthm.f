        SUBROUTINE COMTHM(OPTION,IMATE,TYPMOD,COMPOR,
     >                     CRIT,INSTAM, INSTAP,
     &                     NDIM,DIMDEF,DIMCON,NBVARI,        
     &                     YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                     ADDEME,ADCOME,
     &                     ADDEP1,ADCP11,ADCP12,
     &                     ADDEP2,ADCP21,ADCP22,
     &                     ADDETE,ADCOTE,
     &                     DEFGEM,DEFGEP,CONGEM,CONGEP,
     &                     VINTM,VINTP,
     &                     DSDE,RETCOM
     &                    )
C
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/09/2002   AUTEUR UFBHHLL C.CHAVANT 
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
C TOLE CRP_20
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
C
C
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

C VARIABLES IN / OUT
C
      IMPLICIT NONE
      INTEGER NDIM,DIMDEF,DIMCON,NBVARI,IMATE
      INTEGER YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE
      INTEGER ADDEME,ADDEP1,ADDEP2,ADDETE
      INTEGER ADCOME,ADCP11,ADCP12,ADCP21,ADCP22,ADCOTE
      CHARACTER*16 COMPOR(*),OPTION
      REAL*8 DEFGEM(1:DIMDEF),DEFGEP(1:DIMDEF)
      REAL*8 CONGEM(1:DIMCON),CONGEP(1:DIMCON)
      REAL*8 VINTM(1:NBVARI),VINTP(1:NBVARI)
      REAL*8 DSDE(1:DIMCON,1:DIMDEF)
      CHARACTER*8  TYPMOD(2)
      REAL*8          CRIT(*),INSTAM,INSTAP
      INTEGER RETCOM
C
C VARIABLES LOCALES
C 
C NVIMEC : NOMBRE DE VARIABLES INTERNES MECANIQUES
C NVITH  : NOMBRE DE VARIABLES INTERNES THERMO_HYDRIQUES
C ADVIME : ADRESSE DES VARIABLES INTERNES MECANIQUES
C ADVITH : ADRESSE DES VARIABLES INTERNES THERMO-HYDRIQUES
C
      INTEGER NVIMEC,NVITH,ADVIME,ADVITH
      CHARACTER*16 MECA,THMC,THER,HYDR
      INTEGER INMECA,INTHMC,INTHER,INHYDR
      INTEGER ELA,CJS,ENLGAT,ESUGAT
C   
      REAL*8 EPSV,DEPS(6),DEPSV
      REAL*8 P1,DP1,GRAP1(3)
      REAL*8 P2,DP2,GRAP2(3)
      REAL*8 T,DT,GRAT(3)      
      REAL*8 PHI,PVP,H11,H12,H21,RHO11
      REAL*8 T0,P10,P20,PHI0,PVP0,SAT,RV0
      REAL*8 G1D,G1F,G1C,J1D,J1F,J1C,J2,J3,G2,G3
      INTEGER I
      LOGICAL CERMES 

C
C DECLARATION POUR LA RECUPERATION DES COEFFICIENTS MATERIAU
C
      INTEGER NBRES
      PARAMETER   (   NBRES = 5  )
      REAL*8 INIT(NBRES)
      CHARACTER*8 NCRA(NBRES)
      CHARACTER*2 CODRET(NBRES)      
      DATA NCRA / 'TEMP','PRE1','PRE2','PORO','PRES_VAP' /
C      
C  MODIFS CC SET BUT NOT USED
C 
C      DATA ELA    /1/
C      DATA CJS    /2/ 
C      DATA ENLGAT /3/
C      DATA ESUGAT /4/
C
C    DECODAGE DE COMPOR ET DES SOUS COMPORTEMENTS EVENTUYELS
C
      CALL KITDEC(TYPMOD(1),NBVARI,YAMEC,YAP1,YAP2,YATE,COMPOR,
     >            MECA,THMC,THER,HYDR,
     >            INMECA,INTHMC,INTHER,INHYDR,
     >            NVIMEC,NVITH,ADVIME,ADVITH)
C
C  RECUPERATION DES CONDITIONS INITIALES
C
      CALL RCVALA(IMATE,'THM_INIT',0,' ',0.D0,
     &            NBRES,NCRA,INIT,CODRET,'FM')
      T0=INIT(1)
      P10=INIT(2)
      P20=0.D0
      PHI0=INIT(4)
      IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
        P20=INIT(3)
        PVP0=INIT(5)
      ENDIF
      IF (THMC.EQ.'LIQU_VAPE') THEN
        PVP0=INIT(5)
      ENDIF
      IF (THMC.EQ.'LIQU_NSAT_GAT') THEN
        P20=INIT(3)
      ENDIF
      IF (THMC.EQ.'LIQU_GAZ') THEN
        P20=INIT(3)
      ENDIF     
C     
C
C **********************************************************************
C  CALCUL DES VARIABLES  QUELLE QUE SOIT L'OPTION
C **********************************************************************
C  VARIABLES MECANIQUES
C
      DEPSV=0.D0
      EPSV=0.D0
      IF (YAMEC.EQ.1) THEN
         DO 100 I=1,6
            DEPS(I)=DEFGEP(ADDEME+NDIM-1+I)-DEFGEM(ADDEME+NDIM-1+I)
 100     CONTINUE
         DO 101 I=1,3
            DEPSV=DEPSV+DEFGEP(ADDEME+NDIM-1+I)-DEFGEM(ADDEME+NDIM-1+I)
 101     CONTINUE
         DO 102 I=1,3
            EPSV=EPSV+DEFGEP(ADDEME+NDIM-1+I)
 102    CONTINUE
      ENDIF
C
C **********************************************************************
C   VARIABLES HYDRAULIQUES
      P1=P10
      P2=P20
      IF (YAP1.EQ.1) THEN
         P1=DEFGEP(ADDEP1)+P10
         DP1=DEFGEP(ADDEP1)-DEFGEM(ADDEP1)
         DO 103 I=1,NDIM
            GRAP1(I)=DEFGEP(ADDEP1+I)
 103     CONTINUE
         IF (YAP2.EQ.1) THEN
            P2=DEFGEP(ADDEP2)+P20
            DP2=DEFGEP(ADDEP2)-DEFGEM(ADDEP2)
            DO 104 I=1,NDIM
               GRAP2(I)=DEFGEP(ADDEP2+I)
 104        CONTINUE
         ENDIF
      ENDIF
C
C **********************************************************************
C   VARIABLES THERMIQUES
C
      T=T0
      IF (YATE.EQ.1) THEN
         DT=DEFGEP(ADDETE)-DEFGEM(ADDETE)
         T=DEFGEP(ADDETE)+T0
         DO 105 I=1,NDIM
            GRAT(I)=DEFGEP(ADDETE+I)
 105     CONTINUE
      ELSE
         T = T0
         DT = 0.D0
      ENDIF
      
       CERMES=
     >     ((MECA.EQ.'ELAS_THM').OR.
     >     (MECA.EQ.'CAM_CLAY_THM').OR.
     >     (MECA.EQ.'SURF_ETAT_SATU').OR.
     >     (MECA.EQ.'SURF_ETAT_NSAT').OR.
     >     (THMC.EQ.'LIQU_SATU_GAT').OR.
     >     (THMC.EQ.'LIQU_NSAT_GAT')) 

C
C **********************************************************************
C  CALCUL DES RESIDUS ET DES MATRICES TANGENTES
C
C **********************************************************************
C  CALCUL DES GRANDEURS DE COUPLAGE
C      
C 

       IF (CERMES) THEN      
         CALL CACOGT(OPTION,COMPOR,MECA,THMC,THER,HYDR,
     &                      INMECA,INTHMC,INTHER,INHYDR,
     &                      IMATE,
     &                      NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,        
     &                      YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                      ADDEME,ADCOME,ADVIME,ADVITH,
     &                      ADDEP1,ADCP11,ADCP12,
     &                      ADDEP2,ADCP21,ADCP22,
     &                      ADDETE,ADCOTE,
     &                      CONGEM,CONGEP,
     &                      VINTM,VINTP,
     &                      DSDE,
     &                      EPSV,DEPSV,P1,P2,DP1,DP2,T,DT,
     &                      PHI,PVP,H11,H12,H21,RHO11,PHI0,PVP0,
     &                      P10,P20,T0,SAT,RV0,
     &                      G1D,G1F,G1C,J1D,J1F,J1C,J2,J3,G2,G3)
       ELSE    
         CALL CALCCO(OPTION,COMPOR,MECA,THMC,THER,HYDR,
     &                      INMECA,INTHMC,INTHER,INHYDR,
     &                      IMATE,
     &                      NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,        
     &                      YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                      ADDEME,ADCOME,ADVIME,ADVITH,
     &                      ADDEP1,ADCP11,ADCP12,
     &                      ADDEP2,ADCP21,ADCP22,
     &                      ADDETE,ADCOTE,
     &                      CONGEM,CONGEP,
     &                      VINTM,VINTP,
     &                      DSDE,
     &                      EPSV,DEPSV,P1,P2,DP1,DP2,T,DT,
     &                      PHI,PVP,H11,H12,H21,RHO11,PHI0,PVP0,
     &                      P10,P20,T0,SAT,RETCOM)
         IF ( RETCOM.NE.0) THEN
          GOTO 9000
         ENDIF
       ENDIF
C
C **********************************************************************
C  CALCUL DES GRANDEURS MECANIQUES PURES UNIQUEMENT SI YAMEC=1
C

      IF (YAMEC.EQ.1) THEN 
         CALL CALCME(OPTION,COMPOR,MECA,INMECA,IMATE,TYPMOD,
     >                  CRIT,INSTAM, INSTAP, T0,
     &                  NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,        
     &                  YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                  ADDEME,ADCOME,ADDETE,
     &                  DEFGEM,CONGEM,CONGEP,
     &                  VINTM,VINTP,ADVIME,ADVITH,
     &                  ADDEP1,ADDEP2,
     &                  DSDE,
     &                  DEPS,DEPSV,PHI,P1,P2,T,DT,PHI0,RETCOM
     &                 )
         IF ( RETCOM.NE.0) THEN
          GOTO 9000
         ENDIF
      ENDIF
C         
C **********************************************************************
C  CALCUL DES FLUX HYDRAULIQUES UNIQUEMENT SI YAP1=1
C
      IF (YAP1.EQ.1) THEN
        IF ( CERMES) THEN
         CALL CAFHGT(OPTION,MECA,THMC,THER,HYDR,
     >                      INMECA,INTHMC,INTHER,INHYDR,
     >                        IMATE,
     &                        NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,        
     &                        YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                        ADDEP1,ADDEP2,ADCP11,ADCP12,
     &                        ADCP21,ADCP22,ADDEME,ADDETE,
     &                        CONGEM,CONGEP,
     &                        VINTM,VINTP,ADVIME,ADVITH,
     &                        DSDE,
     &                        EPSV,P1,P2,GRAP1,GRAP2,T,GRAT,
     &                       PHI,PVP,RHO11,H11,H12,H21,T0,SAT,RV0,
     &                        G1D,G1F,G1C,J1D,J1F,J1C,J2,J3,G2,G3)
        ELSE
         CALL CALCFH(OPTION,MECA,THMC,THER,HYDR,
     >                      INMECA,INTHMC,INTHER,INHYDR,
     >                        IMATE,
     &                        NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,        
     &                        YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                        ADDEP1,ADDEP2,ADCP11,ADCP12,
     &                        ADCP21,ADCP22,ADDEME,ADDETE,
     &                        CONGEM,CONGEP,
     &                        VINTM,VINTP,ADVIME,ADVITH,
     &                        DSDE,
     &                        EPSV,P1,P2,GRAP1,GRAP2,T,GRAT,
     &                       PHI,PVP,RHO11,H11,H12,H21,T0,SAT,RETCOM)
         IF ( RETCOM.NE.0) THEN
          GOTO 9000
         ENDIF
        ENDIF
      ENDIF
C
C **********************************************************************
C  CALCUL DU FLUX THERMIQUE UNIQUEMENT SI YATE
C
      IF (YATE.EQ.1) THEN
      IF (CERMES) THEN
        CALL CAFTGT(OPTION,MECA,THMC,THER,HYDR,
     >                     INMECA,INTHMC,INTHER,INHYDR,
     >                       IMATE,
     &                       NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,        
     &                       YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                       ADDETE,ADDEME,ADDEP1,ADDEP2,ADCOTE,
     &                       CONGEM,CONGEP,
     &                       VINTM,VINTP,ADVIME,ADVITH,
     &                       DSDE,
     &                       EPSV,P1,P2,T,GRAT,PHI,SAT,RV0,
     &                       G1D,G1F,G1C,J1D,J1F,J1C,J2,J3,G2,G3)
        ELSE
          CALL CALCFT(OPTION,MECA,THMC,THER,HYDR,
     >                     INMECA,INTHMC,INTHER,INHYDR,
     >                       IMATE,
     &                       NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,        
     &                       YAMEC,YAP1,NBPHA1,YAP2,NBPHA2,YATE,
     &                       ADDETE,ADDEME,ADDEP1,ADDEP2,ADCOTE,
     &                       CONGEM,CONGEP,
     &                       VINTM,VINTP,ADVIME,ADVITH,
     &                       DSDE,
     &                       EPSV,P1,P2,T,GRAT,PHI,SAT,PVP,RETCOM)
         IF ( RETCOM.NE.0) THEN
          GOTO 9000
         ENDIF
        ENDIF 
      ENDIF
 9000   CONTINUE
      END
