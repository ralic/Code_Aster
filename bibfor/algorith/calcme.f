        SUBROUTINE CALCME(OPTION,COMPOR,MECA,IMATE,TYPMOD,CRIT,INSTAM,
     +                    INSTAP,TREF,NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,
     +                    YATE,ADDEME,ADCOME,ADDETE,DEFGEM,CONGEM,
     +                    CONGEP,VINTM,VINTP,ADVIME,ADDEP1,ADDEP2,DSDE,
     +                    DEPS,DEPSV,PHI,P1,P2,T,DT,PHI0,RETCOM,DP1,DP2,
     +                    SAT,BIOT)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 08/06/2004   AUTEUR ROMEO R.FERNANDES 
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
C TOLE CRP_20
C TOLE CRP_21    
C **********************************************************************
C ROUTINE CALC_MECA
C CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE MECANIQUES
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
C                OUT RETCOM
C ======================================================================
      IMPLICIT      NONE
      LOGICAL       MECTRU
      INTEGER       NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,ADDEME,ADDETE,ADDEP1
      INTEGER       ADDEP2,ADCOME,ADVIME,IMATE,YATE,RETCOM
      REAL*8        DEFGEM(1:DIMDEF),CONGEM(1:DIMCON),CONGEP(1:DIMCON)
      REAL*8        VINTM(1:NVIMEC+NVITH),VINTP(1:NVIMEC+NVITH)
      REAL*8        DSDE(1:DIMCON,1:DIMDEF)
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  OPTION,COMPOR(*),MECA
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       I,J,NELAS,NRESMA
      REAL*8        DEPS(6),DEPSV,T,DT,TF,P1,P2
      REAL*8        PHI,YOUNG,NU,ALPHA0,PHI0,CRIT(*),INSTAM,INSTAP,TREF
      PARAMETER (NELAS = 4  )
      PARAMETER (NRESMA = 18)
      REAL*8       ELAS(NELAS)
      CHARACTER*8  NCRA1(NELAS)
      CHARACTER*2 CODRET(NRESMA)
C BG
      REAL*8  XKL,XKB,BTK,ALFAD,RKB,RLA,R2G,REE,RHO
      REAL*8  RDLADT,RD2GDT,RDEEDP,RDEEDT,RDKBDP,RDKBDT
C
      REAL*8  ELOAD,EUNLD,XN,RF,TM1,SUCM
      REAL*8  ANPHI,COHES,SUCC,SUCP1,SUCP2,TEN,TENS
      REAL*8  S1,S2,S3,SMEAN,ATMP,SUC
      REAL*8  AP1,SR,SMAX,SM1,TRAC,AA
      REAL*8  ET,BT,BTMAX,BTMIN,RDEEP1,RDEEP2
      REAL*8  RDBTDT,RDBTP1,RDBTP2
      REAL*8  RD2GP1,RD2GP2,RDLAP1,RDLAP2
      REAL*8  DC,DF,J1,J2,J3,EV   
      REAL*8  DSDEME(6,6)  
      REAL*8  ALPH0,ALPH1,ALPH2,ALPH3,XMT,RDDCDT,RDXMDT
      REAL*8  ALPHAT,ALPHBT,ALPHCT,SY,BB,XM,TM
      REAL*8  PP,R8BID
      INTEGER      NNELA,NBPAR,NSURM,NINIG,NSATM
      PARAMETER    (NNELA=7, NSURM=14,NINIG=2,NSATM=15)
      REAL*8       VALRES(NNELA),VALPAR,VALSUR(NSURM),INIGAT(NINIG)
      REAL*8       VSATSU(NSATM)
      CHARACTER*8  BGCR1(NNELA),NOMPAR,BGCR2(NSURM),BGCR3(NINIG)
      CHARACTER*8  BGCR7(NSATM)
      CHARACTER*16 COMPLG
C ======================================================================
C    VARIABLES LOCALES POUR L'APPEL AU MODELE DE BARCELONE
      REAL*8  DSIDP1(6),DP1,DP2,SAT,BIOT
C ======================================================================
      INTEGER NDT,NDI
      COMMON /TDIM/   NDT  , NDI
C
      DATA NCRA1 / 'E','NU','RHO','ALPHA' /
C BG
      DATA BGCR1 
     > /'RHO_S','UN_SUR_KS','E','KB','D_KB_T','ALPHA_S','ALPHA_D'/
      DATA BGCR2 /'E_CHAR','E_DECHAR','XN','RF','EV_KB','EV_XM'
     &            ,'D_E_T','D_E_SUCC','ANG_FRT','COHE'
     &            ,'D_COEH_SUCC','ANG_FRT_ULT','SUCC_ULTM','RESI_TRAC'/
      DATA BGCR3 /'DEGR_SATU','PRES_ATMO'/
      DATA BGCR7 /'E_CHAR','E_DECHAR','XN','RF','EV_KB'
     &            ,'EV_XM','D_E_T','ALPHA0','ALPHA1','ALPHA2','ALPHA3'
     &            ,'ALPHA_S','ANG_FRT','COHE','RESI_TRAC'/
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU DANS DEFI_MATERIAU -------------
C ======================================================================
      IF (  (MECA.EQ.'ELAS')            .OR.
     +      (MECA.EQ.'CJS')             .OR.
     +      (MECA.EQ.'CAM_CLAY')        .OR.
     +      (MECA.EQ.'BARCELONE')       .OR.
     +      (MECA.EQ.'LAIGLE')          .OR.
     +      (MECA.EQ.'MAZARS')          .OR.
     +      (MECA.EQ.'ENDO_ISOT_BETON') ) THEN
         IF ( OPTION(10:14).EQ.'_ELAS' ) THEN
            CALL UTMESS('F','CALCME','OPTION SECANTE NON VALIDE ' )
         ENDIF
         CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,NELAS,
     +                                           NCRA1,ELAS,CODRET,'FM')
         YOUNG  = ELAS(1)
         NU     = ELAS(2)
         ALPHA0 = ELAS(4)
      ENDIF
      IF ( MECA.EQ.'DRUCKER_PRAGER' ) THEN
         CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,NELAS,
     +                                           NCRA1,ELAS,CODRET,'FM')
         YOUNG  = ELAS(1)
         NU     = ELAS(2)
         ALPHA0 = ELAS(4)
      ENDIF
C ======================================================================
C --- CALCUL DES CONTRAINTES -------------------------------------------
C ======================================================================
C --- LOI ELASTIQUE ----------------------------------------------------
C ======================================================================
      IF (MECA.EQ.'ELAS') THEN
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
           DO 101 I=1,3
             DO 301 J=1,3         
                DSDE(ADCOME-1+I,ADDEME+NDIM-1+J)=
     &                  DSDE(ADCOME-1+I,ADDEME+NDIM-1+J)
     &                  +YOUNG*NU/(1.D0+NU)/(1.D0-2.D0*NU)
 301         CONTINUE
 101       CONTINUE
           ENDIF
C
           IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN     
             DO 121 I=1,3
               CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &              +YOUNG*NU/(1.D0+NU)/(1.D0-2.D0*NU)*DEPSV
 121        CONTINUE
            ENDIF
C
          DO 102 I=1,6
            IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN      
               DSDE(ADCOME-1+I,ADDEME+NDIM-1+I)= 
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+I)+YOUNG/(1.D0+NU)
            ENDIF
            IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN
               CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &              +YOUNG/(1.D0+NU)*DEPS(I)
            ENDIF
 102     CONTINUE
         IF (YATE.EQ.1) THEN
            DO 103 I=1,3
               IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDETE)=DSDE(ADCOME-1+I,ADDETE)
     &                           -YOUNG*ALPHA0/(1.D0-2.D0*NU)
               ENDIF
               IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA'))  THEN
                  CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &                         -YOUNG*ALPHA0/(1.D0-2.D0*NU)*DT
               ENDIF
 103        CONTINUE
         ENDIF
      ENDIF
C ======================================================================
C --- LOI CJS, LOI LAIGLE OU LOI DRUCKER_PRAGER ------------------------
C ======================================================================
      MECTRU = .FALSE.
      IF (MECA.EQ.'CJS') THEN
        MECTRU = .TRUE.
        TF = T + DT
        CALL NMCJS(  TYPMOD,  IMATE, COMPOR, CRIT,
     &                      INSTAM, INSTAP, 
     >                      T,TF, TREF, 
     >                      DEFGEM(ADDEME+NDIM),DEPS, 
     >                      CONGEM(ADCOME), VINTM, OPTION, 
     >                      CONGEP(ADCOME), VINTP, 
     >                      DSDEME)
      ENDIF
      IF (MECA.EQ.'LAIGLE') THEN
        COMPLG = 'LAIGLE'
        MECTRU = .TRUE.
        TF = T + DT
        CALL REDECE(NDIM,TYPMOD,IMATE,COMPLG,CRIT,INSTAM, INSTAP, 
     >              T,TF,TREF,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,
     >              DEFGEM(ADDEME+NDIM),DEPS,CONGEM(ADCOME),
     >              VINTM,OPTION,R8BID,CONGEP(ADCOME),VINTP, 
     >              DSDEME)
      ENDIF
      IF (MECA.EQ.'DRUCKER_PRAGER') THEN
         MECTRU = .TRUE.
         TF = T + DT
         CALL LCDRPR(TYPMOD,OPTION,IMATE,CONGEM(ADCOME),
     +               T,TF,TREF,DEPS,VINTM,VINTP,
     +               CONGEP(ADCOME),DSDEME,RETCOM)
      ENDIF
      IF (MECTRU) THEN
         IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.
     >       (OPTION(1:9).EQ.'FULL_MECA')) THEN
            DO 200 I = 1 , NDT
               DO 201 J = 1 , NDT
                  DSDE(ADCOME+I-1,ADDEME+NDIM+J-1)=DSDEME(I,J)
 201           CONTINUE
 200        CONTINUE
C ======================================================================
C --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
C --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
C ======================================================================
            IF (YATE.EQ.1) THEN
               DO 206 I=1,3 
                  DSDE(ADCOME-1+I,ADDETE)=-ALPHA0*
     >                           (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     >                            DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     >                            DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 206           CONTINUE
            ENDIF
         ENDIF
      ENDIF
C ======================================================================
C --- LOI CAM_CLAY -----------------------------------------------------
C ======================================================================
      IF (MECA.EQ.'CAM_CLAY') THEN
        TF = T + DT
        CALL NMCCAM(  NDIM, TYPMOD,  IMATE, COMPOR, CRIT,
     &                      INSTAM, INSTAP, 
     >                      T,TF, TREF, 
     >                      DEPS, 
     >                      CONGEM(ADCOME), VINTM, OPTION, 
     >                      CONGEP(ADCOME), VINTP, 
     >                      DSDEME)
        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     >            (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DO 402 I = 1 , 2*NDIM
           DO 401 J = 1 , 2*NDIM
            DSDE(ADCOME+I-1,ADDEME+NDIM+J-1)=DSDEME(I,J)
  401     CONTINUE
  402     CONTINUE
C ======================================================================
C --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
C --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
C ======================================================================
         IF (YATE.EQ.1) THEN
            DO 406 I=1,3 
                  DSDE(ADCOME-1+I,ADDETE)=-ALPHA0*
     >            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 406        CONTINUE
         ENDIF
        ENDIF
      ENDIF
C ======================================================================
C --- LOI BARCELONE ----------------------------------------------------
C ======================================================================
      IF (MECA.EQ.'BARCELONE') THEN
        TF = T + DT
        CALL NMBARC(  NDIM, IMATE, CRIT, SAT, BIOT,
     &                      T,TF, 
     >                      DEPS, 
     >                      CONGEM(ADCOME), VINTM, OPTION, 
     >                      CONGEP(ADCOME), VINTP, 
     >                      DSDEME,P1,P2,DP1,DP2,
     &                      DSIDP1)
        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     >            (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DO 412 I = 1 , 2*NDIM
           DO 411 J = 1 , 2*NDIM
            DSDE(ADCOME+I-1,ADDEME+NDIM+J-1)=DSDEME(I,J)
  411     CONTINUE
  412     CONTINUE
C ======================================================================
C --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
C --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
C ======================================================================
         IF (YATE.EQ.1) THEN
            DO 416 I=1,3 
                  DSDE(ADCOME-1+I,ADDETE)=-ALPHA0*
     >            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 416        CONTINUE
         ENDIF
        ENDIF
      ENDIF
C ======================================================================
C --- LOI MAZARS -------------------------------------------------------
C ======================================================================
      IF (MECA.EQ.'MAZARS') THEN
        TF = T + DT
        CALL LCMAZA(  NDIM, TYPMOD,  IMATE, COMPOR,DEFGEM(ADDEME+NDIM),
     >                      DEPS, VINTM, T, TF, TREF, 
     >                      0.D0,0.D0,0.D0,0.D0,0.D0,
     >                      OPTION, CONGEP(ADCOME), VINTP, DSDEME)
     
        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     >            (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DO 502 I = 1 , 2*NDIM
           DO 501 J = 1 , 2*NDIM
            DSDE(ADCOME+I-1,ADDEME+NDIM+J-1)=DSDEME(I,J)
  501     CONTINUE
  502     CONTINUE
C ======================================================================
C --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
C --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
C ======================================================================
         IF (YATE.EQ.1) THEN
            DO 506 I=1,3 
                  DSDE(ADCOME-1+I,ADDETE)=-ALPHA0*
     >            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 506        CONTINUE
         ENDIF
        ENDIF
      ENDIF
C ======================================================================
C --- LOI ENDO_ISOT_BETON ----------------------------------------------
C ======================================================================
      IF (MECA.EQ.'ENDO_ISOT_BETON') THEN
        TF = T + DT
        CALL LCLDSB(  NDIM, TYPMOD,  IMATE, COMPOR,
     &                      DEFGEM(ADDEME+NDIM), 
     >                      DEPS, 
     >                      VINTM, T, TF, TREF,
     >                      OPTION, CONGEP(ADCOME), VINTP, 
     >                      DSDEME)
        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     >            (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DO 602 I = 1 , 2*NDIM
           DO 601 J = 1 , 2*NDIM
            DSDE(ADCOME+I-1,ADDEME+NDIM+J-1)=DSDEME(I,J)
  601     CONTINUE
  602     CONTINUE
C ======================================================================
C --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
C --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
C ======================================================================
         IF (YATE.EQ.1) THEN
            DO 606 I=1,3 
                  DSDE(ADCOME-1+I,ADDETE)=-ALPHA0*
     >            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 606        CONTINUE
         ENDIF
        ENDIF
      ENDIF
C ======================================================================
C --- LOI ELASTIQUE NON LINEAIRE POUR LES SOLS SATURE ------------------
C ======================================================================
      IF (MECA.EQ.'ELAS_THM') THEN
C
C  RECUPERATION DES COEFFICIENTS DU MATERIAU PORO_THM_GAT 
C
      NBPAR  = 0
      VALPAR = 0.D0
      NOMPAR = ' '
      CALL RCVALA(IMATE,' ','ELAS_THM',NBPAR,NOMPAR,VALPAR,
     &              NNELA,BGCR1,VALRES,CODRET, 'FM' )
       XKL    = VALRES(3)
       XKB    = VALRES(4)
       BTK    = VALRES(5)
       ALFAD  = VALRES(7)

      ENDIF
C
C CALCUL DES MODULES  
C
      IF (MECA.EQ.'ELAS_THM') THEN
       RKB=XKB+BTK*T
       RDKBDT=BTK
       NU=(3.D0*XKB-XKL)/(6.D0*XKB)
       REE    = XKL+3.D0*(1.D0-2.D0*NU)*BTK*T
       RDEEDT = 3.D0*(1.D0-2.D0*NU)*RDKBDT
C
C     FORM D MATRIX
C 
      RLA=3.D0*RKB*(3.D0*RKB-REE)/(9.D0*RKB-REE)
      R2G=6.D0*RKB*REE/(9.D0*RKB-REE)
C
      RDLADT=RDEEDT/(1.D0+NU)*NU/(1.D0-2.D0*NU)
      RD2GDT=RDEEDT/(1.D0+NU)
C      
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
           DO 111 I=1,3
             DO 311 J=1,3         
                DSDE(ADCOME-1+I,ADDEME+NDIM-1+J)=
     &                  DSDE(ADCOME-1+I,ADDEME+NDIM-1+J)
     &                  +RLA
 311         CONTINUE
 111       CONTINUE
           ENDIF
C
           IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN     
             DO 221 I=1,3
               CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &              +RLA*DEPSV
 221        CONTINUE
            ENDIF
C
C
         DO 112 I=1,6
            IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN      
               DSDE(ADCOME-1+I,ADDEME+NDIM-1+I)= 
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+I)+R2G
            ENDIF
            IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN
               CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &              +R2G*DEPS(I)
            ENDIF
 112     CONTINUE
         IF (YATE.EQ.1) THEN
          DO 113 I=1,3
               IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDETE)=DSDE(ADCOME-1+I,ADDETE)
     &                          -3.D0*RKB*ALFAD-6.D0*BTK*ALFAD*DT+RDLADT
               ENDIF
               IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA'))  THEN
                  CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &                         -3.D0*RKB*ALFAD*DT
               ENDIF
 113      CONTINUE
          DO 114 I=1,6
             IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDETE)=DSDE(ADCOME-1+I,ADDETE)
     &                           +RD2GDT
             ENDIF
 114      CONTINUE
         ENDIF
      ENDIF
C ======================================================================
C --- LOI ELASTIQUE NON LINEAIRE POUR LES SOLS SATURE ------------------
C ======================================================================
        IF (MECA.EQ.'SURF_ETAT_SATU') THEN
C
C  RECUPERATION DES COEFFICIENTS DU MATERIAU SURF_SAT__GAT 
C
            NBPAR  = 0
            VALPAR = 0.D0
            NOMPAR = ' '
            CALL RCVALA(IMATE,' ', 'SURF_ETAT_SATU' ,NBPAR,NOMPAR,
     &              VALPAR,NSATM,BGCR7,VSATSU,CODRET, 'FM' )
            ELOAD   = VSATSU(1)
            EUNLD   = VSATSU(2)
            XN      = VSATSU(3)
            RF      = VSATSU(4)
            XKB     = VSATSU(5)
            XM      = VSATSU(6)
            TM1     = VSATSU(7)
            ALPH0   = VSATSU(8)
            ALPH1   = VSATSU(9)
            ALPH2   = VSATSU(10)
            ALPH3   = VSATSU(11)
            ANPHI   = VSATSU(13)
            COHES   = VSATSU(14)
            TEN     = VSATSU(15)
C
            CALL RCVALA(IMATE,' ','THM_INIT',0,' ',0.D0,NINIG,BGCR3,
     &               INIGAT,CODRET,'FM')
            ATMP = INIGAT(2)
            CALL PRCIPE(CONGEP,DIMCON,S1,S2,S3,NDIM)
      SY=0.D0
         DO 20 I=1,3
            SY=SY+CONGEP(ADCOME-1+I)
 20     CONTINUE
      SY=SY/3.D0
      SMEAN=S3
      ALPHAT=ALPH0+ALPH2*DT
      ALPHBT=ALPH1+ALPH3*DT
      ALPHCT=ALPH1+2.D0*ALPH3*DT
C
C     CHECK FOR TENSILE FAILURE
C
      TENS=S3+TEN
      AP1=0.1D0*ATMP
      IF(S3.GT.0.D0) GO TO 40
      IF(TENS.GT.0.D0) GO TO 30
      SMEAN=AP1
      BB=1.D0
      IF(XM.GT.0.000001D0) BB=(SMEAN/ATMP)**XM
      BT=XKB*ATMP*BB*(1.D0+ALPHBT*DT)
      IF (XM.EQ.1.D0)THEN
      XMT= ALPHAT
      RDXMDT=ALPH2
      ELSE
      XMT= ALPHAT+ALPHCT*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      RDXMDT=ALPH2+2.D0*ALPH3*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      ENDIF
      RDBTDT =XKB*ATMP*BB*ALPHCT
      RDBTP1 =0.D0
      ET=0.01D0*BT
      SR=1.0D0
      GO TO 100
30    CONTINUE
      SMEAN=TENS
40    CONTINUE
C
C     CHECK FOR SHEAR FAILURE
C
      PP=ANPHI
      SR=(1.D0-SIN(PP))*(S1-SMEAN)/(2.D0*COHES*COS(PP)
     &    +2.D0*SMEAN*SIN(PP))
      IF(SR.LT.0.95D0) GO TO 50
      SR=0.95D0
C
C     CALCULATE LOAD/UNLOAD MODULUS
C
50    CONTINUE   
      TRAC=ABS(S1-S3)/2.D0
C       DEFINITION DE SM1
      SM1=MAX(ABS(CONGEP(ADCOME+2)-CONGEP(ADCOME)),
     &        ABS(CONGEP(ADCOME+1)-CONGEP(ADCOME)),
     &        ABS(CONGEP(ADCOME+2)-CONGEP(ADCOME+1)))
      SM1=SM1/2.D0
      AA=1.D0
      BB=1.D0
      IF(SMEAN.LT.AP1) SMEAN=AP1
      IF(XN.GT.0.000001D0) AA=(SMEAN/ATMP)**XN
      IF(XM.GT.0.000001D0) BB=(SMEAN/ATMP)**XM
      IF(TRAC.GE.0.99D0*SM1) GO TO 60
      ET=EUNLD*AA*ATMP+T*TM1
      BT=XKB*ATMP*BB*(1.D0+ALPHBT*DT)
      RDBTDT =XKB*ATMP*BB*ALPHCT
      RDBTP1 =0.D0
      IF (XM.EQ.1.D0)THEN
      XMT= ALPHAT
      RDXMDT=ALPH2
      ELSE
      XMT= ALPHAT+ALPHCT*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      RDXMDT=ALPH2+2.D0*ALPH3*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      ENDIF
      RDEEDT = TM1
      RDEEP1 =0.D0
      GO TO  90
60    CONTINUE   
      ET=(ELOAD*AA*ATMP+T*TM1)*(1.D0-RF*SR)**2.D0
      BT=XKB*ATMP*BB*(1.D0+ALPHBT*DT)
      RDBTDT =XKB*ATMP*BB*ALPHCT
      RDBTP1 =0.D0
      IF (XM.EQ.1.D0)THEN
      XMT= ALPHAT
      RDXMDT=ALPH2
      ELSE
      XMT= ALPHAT+ALPHCT*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      RDXMDT=ALPH2+2.D0*ALPH3*(SMEAN/ATMP)**(1.D0-XM)/XKB/(1.D0-XM)
      ENDIF
      RDEEDT = TM1*(1D0-RF*SR)**2.D0
      RDEEP1 =0.D0
90    CONTINUE   
      BTMAX=17.0D0*ET
      BTMIN=0.33D0*ET
      IF(BT.LT.BTMIN) BT=BTMIN
      IF(BT.GT.BTMAX) BT=BTMAX
100   CONTINUE   
C
      DC=3.D0*BT*XMT
      RDDCDT=3.D0*(RDBTDT*XMT+BT*RDXMDT)
C
C     FORM D MATRIX
C
      RLA=3.D0*BT*(3.D0*BT-ET)/(9.D0*BT-ET)
      R2G=6.D0*BT*ET/(9.D0*BT-ET)
C
      RD2GDT=((6.D0*RDBTDT*ET+6.D0*RDEEDT*BT)*(9.D0*BT-ET)
     &       -6.D0*BT*ET*(9.D0*RDBTDT-RDEEDT))/(9.D0*BT-ET)/(9.D0*BT-ET)
      RDLADT=RDBTDT-RD2GDT/3.D0
C
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
           DO 205 I=1,3
             DO 321 J=1,3         
                DSDE(ADCOME-1+I,ADDEME+NDIM-1+J)=
     &                  DSDE(ADCOME-1+I,ADDEME+NDIM-1+J)+RLA
 321         CONTINUE
 205      CONTINUE
           ENDIF
C
           IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN     
             DO 222 I=1,3
               CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &              +RLA*DEPSV
 222        CONTINUE
            ENDIF
C
C 
         DO 202 I=1,6
            IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN      
               DSDE(ADCOME-1+I,ADDEME+NDIM-1+I)= 
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+I)+R2G
            ENDIF
            IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN
               CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &              +R2G*DEPS(I)
            ENDIF
 202     CONTINUE
C
       IF (YATE.EQ.1) THEN
          DO 203 I=1,3
               IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDETE)=DSDE(ADCOME-1+I,ADDETE)
     &                                    -DC-RDDCDT*DT+RDLADT
               ENDIF
               IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA'))  THEN
                  CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &                                -DC*DT
               ENDIF
 203      CONTINUE
          DO 204 I=1,6
             IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDETE)=DSDE(ADCOME-1+I,ADDETE)
     &                                      +RD2GDT
             ENDIF
 204      CONTINUE
         ENDIF
        ENDIF
C ======================================================================
C --- LOI ELASTIQUE NON LINEAIRE POUR LES SOLS NON SATURE --------------
C ======================================================================
        IF (MECA.EQ.'SURF_ETAT_NSAT') THEN
C
C  RECUPERATION DES COEFFICIENTS DU MATERIAU PORO_THM_NONSAT 
C
            NBPAR  = 0
            VALPAR = 0.D0
            NOMPAR = ' '
            CALL RCVALA(IMATE,' ','SURF_ETAT_NSAT',NBPAR,NOMPAR,
     &              VALPAR,NSURM,BGCR2,VALSUR,CODRET, 'FM' )
            ELOAD   = VALSUR(1)
            EUNLD   = VALSUR(2)        
            XN      = VALSUR(3)
            RF      = VALSUR(4)
            TM1     = VALSUR(7)
            SUCM    = VALSUR(8)
            ANPHI   = VALSUR(9) 
            COHES   = VALSUR(10)
            SUCC    = VALSUR(11)
            SUCP1   = VALSUR(12)
            SUCP2   = VALSUR(13)
            TEN     = VALSUR(14)
C
            CALL RCVALA(IMATE,' ','THM_INIT',0,' ',0.D0,NINIG,BGCR3,
     &               INIGAT,CODRET,'FM')
            ATMP = INIGAT(2)
            CALL PRCIPE(CONGEP,DIMCON,S1,S2,S3,NDIM)
            CALL STATVT(IMATE,MECA,DIMCON,ADCOME,CONGEP,ATMP,S3,P1,P2,
     &                  J1,J2,J3, BT,DF,DC,RDBTDT,RDBTP1,RDBTP2,PHI)
C
C     CHECK FOR TENSILE FAILURE
C
      SMEAN=S3
      TENS=S3+TEN
      AP1=0.1D0*ATMP
      IF(S3.GT.0.D0) GO TO 400
      IF(TENS.GT.0.D0) GO TO 300
      SMEAN=AP1
      ET=0.01D0*BT
      SR=1.00D0
      RDEEDT = 0.01D0*RDBTDT
      RDEEP1 = 0.01D0*RDBTP1
      RDEEP2 = 0.01D0*RDBTP2
      GO TO 1000
300   CONTINUE
      SMEAN=TENS
C
C     CHECK FOR SHEAR FAILURE
C
400   CONTINUE
      SUC=P2-P1
      CALL STRENT(COHES,ANPHI,S1,SMEAN,SUC,
     &            SUCC,SUCP1,SUCP2,SMAX,T)
      SR=SMAX
      IF(SR.LT.0.95D0) GO TO 500
      SR=0.95D0
C
C     CALCULATE LOAD/UNLOAD MODULUS
C
500   CONTINUE   
      TRAC=ABS(S1-S3)/2.D0
C       DEFINITION DE SM1
      SM1=MAX(ABS(CONGEP(ADCOME+2)-CONGEP(ADCOME)),
     &        ABS(CONGEP(ADCOME+1)-CONGEP(ADCOME)),
     &        ABS(CONGEP(ADCOME+2)-CONGEP(ADCOME+1)))
      SM1=SM1/2.D0
      AA=1.D0
      IF(SMEAN.LT.AP1) SMEAN=AP1
      IF(XN.GT.1.D-3) AA=(SMEAN/ATMP)**XN
      IF(TRAC.GE.0.99D0*SM1) GO TO 600
      ET=EUNLD*AA*ATMP+SUC*SUCM+T*TM1
      RDEEDT = TM1
      RDEEP1 =-SUCM
      RDEEP2 = SUCM
      GO TO  900
600   CONTINUE   
      ET=(ELOAD*AA*ATMP+SUC*SUCM+T*TM1)*(1.D0-RF*SR)*(1.D0-RF*SR)
      RDEEDT = TM1*(1.D0-RF*SR)**2.D0
      RDEEP1 =-SUCM*(1.D0-RF*SR)**2.D0
      RDEEP2 = SUCM*(1.D0-RF*SR)**2.D0
900   CONTINUE   
      BTMAX=17.0D0*ET
      BTMIN=0.33D0*ET
      IF(BT.LT.BTMIN) BT=BTMIN
      IF(BT.GT.BTMAX) BT=BTMAX
1000  CONTINUE   
C
C
C     FORM D MATRIX
C
      RLA=3.D0*BT*(3.D0*BT-ET)/(9.D0*BT-ET)
      R2G=6.D0*BT*ET/(9.D0*BT-ET)
C
      RD2GDT=((6.D0*RDBTDT*ET+6.D0*RDEEDT*BT)*(9.D0*BT-ET)
     &       -6.D0*BT*ET*(9.D0*RDBTDT-RDEEDT))/(9.D0*BT-ET)/(9.D0*BT-ET)
      RDLADT=RDBTDT-RD2GDT/3.D0
C
      RD2GP1=((6.D0*RDBTP1*ET+6.D0*RDEEP1*BT)*(9.D0*BT-ET)
     &       -6.D0*BT*ET*(9.D0*RDBTP1-RDEEP1))/(9.D0*BT-ET)/(9.D0*BT-ET)
      RDLAP1=RDBTP1-RD2GP1/3.D0
C
      RD2GP2=((6.D0*RDBTP2*ET+6.D0*RDEEP2*BT)*(9.D0*BT-ET)
     &       -6.D0*BT*ET*(9.D0*RDBTP2-RDEEP2))/(9.D0*BT-ET)/(9.D0*BT-ET)
      RDLAP2=RDBTP2-RD2GP2/3.D0
C
         IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
           DO 211 I=1,3
             DO 331 J=1,3         
                DSDE(ADCOME-1+I,ADDEME+NDIM-1+J)=
     &                  DSDE(ADCOME-1+I,ADDEME+NDIM-1+J)+RLA
 331         CONTINUE
 211       CONTINUE
           ENDIF
C
           IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN     
             DO 421 I=1,3
               CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &              +RLA*DEPSV
 421        CONTINUE
            ENDIF
C
C 
         DO 212 I=1,6
            IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN      
               DSDE(ADCOME-1+I,ADDEME+NDIM-1+I)= 
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+I)+R2G
            ENDIF
            IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &         (OPTION(1:9).EQ.'FULL_MECA')) THEN
               CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &              +R2G*DEPS(I)
            ENDIF
 212     CONTINUE
C
C
          DO 213 I=1,3
               IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDEP1)=DSDE(ADCOME-1+I,ADDEP1)
     &                                    +RDLAP1
               ENDIF
 213      CONTINUE
          DO 214 I=1,6
             IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDEP1)=DSDE(ADCOME-1+I,ADDEP1)
     &                                      +RD2GP1
             ENDIF
 214      CONTINUE
C
C
          DO 215 I=1,3
               IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDEP2)=DSDE(ADCOME-1+I,ADDEP2)
     &                                    +RDLAP2
               ENDIF
 215      CONTINUE
          DO 216 I=1,6
               IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDEP2)=DSDE(ADCOME-1+I,ADDEP2)
     &                                      +RD2GP2
               ENDIF
 216      CONTINUE
C
         IF (YATE.EQ.1) THEN
          DO 217 I=1,3
               IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDETE)=DSDE(ADCOME-1+I,ADDETE)
     &                                    -DC+RDLADT
               ENDIF
               IF ((OPTION(1:9).EQ.'RAPH_MECA').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA'))  THEN
                  CONGEP(ADCOME+I-1)=CONGEP(ADCOME+I-1)
     &                                -DC*DT
               ENDIF
 217      CONTINUE
          DO 218 I=1,6
             IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN 
                  DSDE(ADCOME-1+I,ADDETE)=DSDE(ADCOME-1+I,ADDETE)
     &                                      +RD2GDT
             ENDIF
 218      CONTINUE
         ENDIF
        ENDIF
C ======================================================================
C --- LOI ELASTOPLASTIQUE SATURE THM -----------------------------------
C ======================================================================
      IF (MECA.EQ.'CAM_CLAY_THM') THEN
       CALL  NMPGAT(OPTION,IMATE,T,DT, TREF,DEPS,PHI0,
     &              NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,        
     &              ADDEME,ADCOME,ADDETE,CONGEM,CONGEP,
     &              VINTM,VINTP,ADVIME,DSDE)
       ENDIF
C ======================================================================
      END
