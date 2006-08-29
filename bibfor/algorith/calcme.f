        SUBROUTINE CALCME(OPTION,COMPOR,THMC,MECA,IMATE,TYPMOD,CRIT,
     +                    INSTAM,INSTAP,TREF,NDIM,DIMDEF,DIMCON,NVIMEC,
     +                    NVITH,YATE,ADDEME,ADCOME,ADDETE,DEFGEM,CONGEM,
     +                    CONGEP,VINTM,VINTP,ADVIME,ADDEP1,ADDEP2,DSDE,
     +                    DEPS,DEPSV,PHI,P1,P2,T,DT,PHI0,RETCOM,DP1,DP2,
     +                    SAT,BIOT)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 28/08/2006   AUTEUR CIBHHPD L.SALMONA 
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
      LOGICAL       MECTRU,PRE2TR
      INTEGER       NDIM,DIMDEF,DIMCON,NVIMEC,NVITH,ADDEME,ADDETE,ADDEP1
      INTEGER       ADDEP2,ADCOME,ADVIME,IMATE,YATE,RETCOM
      REAL*8        DEFGEM(1:DIMDEF),CONGEM(1:DIMCON),CONGEP(1:DIMCON)
      REAL*8        VINTM(1:NVIMEC+NVITH),VINTP(1:NVIMEC+NVITH)
      REAL*8        DSDE(1:DIMCON,1:DIMDEF)
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  OPTION,COMPOR(*),MECA,THMC
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       I,J,NELAS,NRESMA,IRET
      REAL*8        DEPS(6),DEPSV,T,DT,TINI,P1,P2
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
      REAL*8  PP,R8BID,ANGMAS(3)
      CHARACTER*16 COMPLG(2)
C ======================================================================
C    VARIABLES LOCALES POUR L'APPEL AU MODELE DE BARCELONE
      REAL*8  DSIDP1(6),DP1,DP2,SAT,BIOT
CCCC    SIP NECESSAIRE POUR CALCULER LES CONTRAINTES TOTALES
CCCC    ET ENSUITE CONTRAINTES NETTES DANS LE MODELE DE BARCELONE
      REAL*8  SIPM,SIPP      
C ======================================================================
C    VARIABLES LOCALES POUR L'APPEL AU MODELE DE HOEK_BROWN_TOT
      REAL*8  DSIDP2(6),DSPDP1,DSPDP2
C ======================================================================
      INTEGER NDT,NDI
      COMMON /TDIM/   NDT  , NDI
C
      DATA NCRA1 / 'E','NU','ALPHA','RHO' /
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU DANS DEFI_MATERIAU -------------
C ======================================================================
      IF (  (MECA.EQ.'ELAS')            .OR.
     +      (MECA.EQ.'CJS')             .OR.
     +      (MECA.EQ.'CAM_CLAY')        .OR.
     +      (MECA.EQ.'BARCELONE')       .OR.
     +      (MECA.EQ.'LAIGLE')          .OR.
     +      (MECA.EQ.'HOEK_BROWN_EFF')  .OR.
     +      (MECA.EQ.'HOEK_BROWN_TOT')  .OR.
     +      (MECA.EQ.'MAZARS')          .OR.
     +      (MECA.EQ.'ENDO_ISOT_BETON') ) THEN
         IF ( OPTION(10:14).EQ.'_ELAS' ) THEN
            CALL UTMESS('F','CALCME','OPTION SECANTE NON VALIDE ' )
         ENDIF
         CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,NELAS,
     +                                           NCRA1,ELAS,CODRET,'FM')
         YOUNG  = ELAS(1)
         NU     = ELAS(2)
         ALPHA0 = ELAS(3)
      ENDIF
      IF ( MECA.EQ.'DRUCKER_PRAGER' ) THEN
         CALL RCVALA(IMATE,' ','ELAS',0,' ',0.D0,NELAS,
     +                                           NCRA1,ELAS,CODRET,'FM')
         YOUNG  = ELAS(1)
         NU     = ELAS(2)
         ALPHA0 = ELAS(3)
      ENDIF
      IF (MECA.EQ.'ELAS_THER')  THEN
         IF ( OPTION(10:14).EQ.'_ELAS' ) THEN
            CALL UTMESS('F','CALCME','OPTION SECANTE NON VALIDE ' )
         ENDIF
         CALL RCVALA(IMATE,' ','ELAS',1,'TEMP', T,3,
     +                                 NCRA1(1),ELAS(1),CODRET,'FM')
         YOUNG  = ELAS(1)
         NU     = ELAS(2)
         ALPHA0 = ELAS(3)
      ENDIF
C ======================================================================
C --- CALCUL DES CONTRAINTES -------------------------------------------
C ======================================================================
C --- LOI ELASTIQUE ----------------------------------------------------
C ======================================================================
      IF ((MECA.EQ.'ELAS').OR.(MECA.EQ.'ELAS_THER')) THEN
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
C --- LOI CJS, LOI LAIGLE, LOI HOEK-BROWN OU LOI DRUCKER_PRAGER -------
C ======================================================================
      MECTRU = .FALSE.
      IF (MECA.EQ.'CJS') THEN
        MECTRU = .TRUE.
        TINI = T - DT
        CALL NMCJS(  TYPMOD,  IMATE, COMPOR, CRIT,
     &                      INSTAM, INSTAP, 
     >                      TINI,T, TREF, 
     >                      DEFGEM(ADDEME+NDIM),DEPS, 
     >                      CONGEM(ADCOME), VINTM, OPTION, 
     >                      CONGEP(ADCOME), VINTP, 
     >                      DSDEME,IRET)
      ENDIF
      IF (MECA.EQ.'LAIGLE') THEN
        COMPLG(1) = 'LAIGLE'
        WRITE (COMPLG(2),'(I16)') NVIMEC
        MECTRU = .TRUE.
        TINI = T - DT
        CALL REDECE('RIGI',1,1,NDIM,TYPMOD,IMATE,COMPLG,CRIT,INSTAM, 
     >              INSTAP,TINI,T,TREF,DEFGEM(ADDEME+NDIM),DEPS,
     >              CONGEM(ADCOME),VINTM,OPTION,R8BID,ANGMAS,
     >              CONGEP(ADCOME),VINTP,DSDEME,RETCOM)
      ENDIF
      IF (MECA.EQ.'HOEK_BROWN_EFF') THEN
        COMPLG(1) = 'HOEK_BROWN_EFF'
        WRITE (COMPLG(2),'(I16)') NVIMEC
        MECTRU = .TRUE.
        TINI = T - DT
        CALL REDECE('RIGI',1,1,NDIM,TYPMOD,IMATE,COMPLG,CRIT,INSTAM, 
     >              INSTAP,TINI,T,TREF,DEFGEM(ADDEME+NDIM),DEPS,
     >              CONGEM(ADCOME),VINTM,OPTION,R8BID,ANGMAS,
     >              CONGEP(ADCOME),VINTP,DSDEME,RETCOM)
      ENDIF
      IF (MECA.EQ.'DRUCKER_PRAGER') THEN
         MECTRU = .TRUE.
         TINI = T - DT
         CALL LCDRPR(TYPMOD,OPTION,IMATE,CONGEM(ADCOME),
     +               TINI,T,TREF,DEPS,VINTM,VINTP,
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
        TINI = T - DT
        CALL NMCCAM(  NDIM, TYPMOD,  IMATE, COMPOR, CRIT,
     &                      INSTAM, INSTAP, 
     >                      TINI,T, TREF, 
     >                      DEPS, 
     >                      CONGEM(ADCOME), VINTM, OPTION, 
     >                      CONGEP(ADCOME), VINTP, 
     >                      DSDEME,RETCOM)
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
        TINI = T - DT
        SIPM=CONGEM(ADCOME+6)
        SIPP=CONGEP(ADCOME+6)
         CALL NMBARC(  NDIM, IMATE, CRIT, SAT, BIOT,
     &                      TINI,T,
     >                      DEPS,
     >                      CONGEM(ADCOME), VINTM, OPTION,
     >                      CONGEP(ADCOME), VINTP,
     >                      DSDEME,P1,P2,DP1,DP2,
     &                      DSIDP1,SIPM,SIPP,RETCOM)
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
C --- LOI HOEK_BROWN_TOT -----------------------------------------------
C ======================================================================
      IF (MECA.EQ.'HOEK_BROWN_TOT') THEN
        TINI = T - DT
        SIPM=CONGEM(ADCOME+6)
        SIPP=CONGEP(ADCOME+6)
        DSPDP1 = 0.0D0
        DSPDP2 = 0.0D0         
        CALL DSIPDP(THMC,ADCOME,ADDEP1,ADDEP2,
     1              DIMCON,DIMDEF,DSDE,DSPDP1,DSPDP2,PRE2TR)
        
        CALL LCHBR2( TYPMOD,OPTION,IMATE,CRIT,CONGEM(ADCOME),
     &   DEFGEM(ADDEME+NDIM),TINI,T,TREF,DEPS,VINTM,VINTP,DSPDP1,DSPDP2,
     &    SIPM,SIPP,CONGEP(ADCOME),DSDEME,DSIDP1,DSIDP2,RETCOM)
        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     >            (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DO 413 I = 1 , 2*NDIM
        IF (ADDEP1.GE.1) THEN
            DSDE(ADCOME+I-1,ADDEP1) = DSIDP1(I)
        ENDIF
C
        IF (PRE2TR) THEN
            DSDE(ADCOME+I-1,ADDEP2) = DSIDP2(I)
        ENDIF
            DO 414 J = 1 , 2*NDIM
                DSDE(ADCOME+I-1,ADDEME+NDIM+J-1)=DSDEME(I,J)
  414       CONTINUE
  413     CONTINUE        
C ======================================================================
C --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
C --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
C ======================================================================
         IF (YATE.EQ.1) THEN
            DO 417 I=1,3 
                  DSDE(ADCOME-1+I,ADDETE)=-ALPHA0*
     >            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 417        CONTINUE
         ENDIF
        ENDIF
      ENDIF
C ======================================================================
C --- LOI MAZARS -------------------------------------------------------
C ======================================================================
      IF (MECA.EQ.'MAZARS') THEN
        TINI = T - DT
        CALL LCMAZA('RIGI',1,1,NDIM,TYPMOD,IMATE,COMPOR,
     >              DEFGEM(ADDEME+NDIM),DEPS, VINTM, TINI, T, TREF, 
     >              OPTION,CONGEP(ADCOME),VINTP,DSDEME)
     
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
        TINI = T - DT
        CALL LCLDSB('RIGI',1,1,NDIM, TYPMOD,  IMATE, COMPOR,
     &                      DEFGEM(ADDEME+NDIM), 
     >                      DEPS, 
     >                      VINTM,TINI,T,TREF,OPTION, 
     >                      CONGEP(ADCOME), VINTP,DSDEME)
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
      END
