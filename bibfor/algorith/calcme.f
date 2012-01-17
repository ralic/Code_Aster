        SUBROUTINE CALCME(OPTION,COMPOR,THMC,MECA,IMATE,TYPMOD,CRIT,
     &                    INSTAM,INSTAP,
     &                    TREF,NDIM,DIMDEF,DIMCON,NVIMEC,
     &                    YATE,ADDEME,ADCOME,ADDETE,DEFGEM,CONGEM,
     &                    CONGEP,VINTM,VINTP,ADDEP1,ADDEP2,DSDE,
     &                    DEPS,DEPSV,P1,P2,T,DT,RETCOM,DP1,DP2,
     &                    SAT,BIOT)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 16/01/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GRANET S.GRANET
C TOLE CRP_20 CRP_21
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
      INTEGER       NDIM,DIMDEF,DIMCON,NVIMEC,ADDEME,ADDETE,ADDEP1
      INTEGER       ADDEP2,ADCOME,IMATE,YATE,RETCOM
      REAL*8        DEFGEM(DIMDEF),CONGEM(DIMCON),CONGEP(DIMCON)
      REAL*8        VINTM(NVIMEC),VINTP(NVIMEC)
      REAL*8        DSDE(DIMCON,DIMDEF)
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  OPTION,COMPOR(*),MECA,THMC
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       I,J,NELAS,NRESMA,NUMLC
      REAL*8        DEPS(6),DEPSV,T,DT,TINI,P1,P2
      REAL*8        YOUNG,NU,ALPHA0,CRIT(*),INSTAM,INSTAP,TREF
      PARAMETER (NELAS = 4  )
      PARAMETER (NRESMA = 18)
      REAL*8       ELAS(NELAS)
      CHARACTER*8  NCRA1(NELAS),FAMI,POUM
      INTEGER ICODRE(NRESMA)
      REAL*8  DSDEME(6,6)
      REAL*8  R8BID,ANGMA1(3),ANGMAS(7)
      CHARACTER*16 COMPLG(2)
      LOGICAL         CP , YAPRE2
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
      INTEGER NDT,NDI,KPG,SPT
      COMMON /TDIM/   NDT  , NDI
C
      DATA NCRA1 / 'E','NU','ALPHA','RHO' /
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU DANS DEFI_MATERIAU -------------
C ======================================================================
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'
      IF (  (MECA.EQ.'CJS')             .OR.
     &      (MECA.EQ.'CAM_CLAY')        .OR.
     &      (MECA.EQ.'BARCELONE')       .OR.
     &      (MECA.EQ.'LAIGLE')          .OR.
     &      (MECA.EQ.'HOEK_BROWN_EFF')  .OR.
     &      (MECA.EQ.'HOEK_BROWN_TOT')  .OR.
     &      (MECA.EQ.'MAZARS')          .OR.
     &      (MECA.EQ.'ENDO_ISOT_BETON') ) THEN
         IF ( OPTION(10:14).EQ.'_ELAS' ) THEN
            CALL U2MESS('F','ALGORITH_67')
         ENDIF
      ENDIF
      CALL RCVALB(FAMI,KPG,SPT,POUM,IMATE,' ','ELAS',1,'TEMP', T,3,
     &            NCRA1(1),ELAS(1),ICODRE,1)
      YOUNG  = ELAS(1)
      NU     = ELAS(2)
      ALPHA0 = ELAS(3)
C ======================================================================
C --- RECUPERATION OU NON DE LA PRESSION DE GAZ
C ======================================================================
      YAPRE2 = .FALSE.
      IF ((THMC.EQ.'GAZ').OR.(THMC.EQ.'LIQU_VAPE_GAZ').OR.
     & (THMC.EQ.'LIQU_AD_GAZ').OR.
     &    (THMC.EQ.'LIQU_GAZ').OR.(THMC.EQ.'LIQU_AD_GAZ_VAPE'))THEN
                YAPRE2 = .TRUE.
      ENDIF
C ======================================================================
C --- CALCUL DES CONTRAINTES -------------------------------------------
C ======================================================================
C --- LOI ELASTIQUE ----------------------------------------------------
C ======================================================================
      IF ((MECA.EQ.'ELAS')) THEN
         IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.
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
            IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.
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
               IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.
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
     &                      TINI,T, TREF,
     &                      DEFGEM(ADDEME+NDIM),DEPS,
     &                      CONGEM(ADCOME), VINTM, OPTION,
     &                      CONGEP(ADCOME), VINTP,
     &                      DSDEME,RETCOM)
      ENDIF

C ======================================================================
C ------                       LOI DE HUJEUX                      ------
C ======================================================================
      IF (MECA.EQ.'HUJEUX') THEN

        MECTRU = .TRUE.
        TINI = T - DT

        DO 150 I =1, 7
          ANGMAS(I)=0.D0
 150      CONTINUE

        COMPLG(1) = 'HUJEUX'
        WRITE (COMPLG(2),'(I16)') NVIMEC
        NUMLC=34
        CP=.FALSE.
        CALL REDECE('RIGI',1,1,NDIM,TYPMOD,IMATE,COMPLG,CRIT,INSTAM,
     &          INSTAP,6,DEFGEM(ADDEME+NDIM),DEPS,
     &          6,CONGEM(ADCOME),VINTM,OPTION,ANGMAS,1,R8BID,
     &          CP,NUMLC,TINI,T,TREF,
     &             CONGEP(ADCOME),VINTP,36,DSDEME,1,R8BID,RETCOM)

      ENDIF
C --- End

      IF (MECA.EQ.'LAIGLE') THEN
        COMPLG(1) = 'LAIGLE'
        WRITE (COMPLG(2),'(I16)') NVIMEC
        MECTRU = .TRUE.
        TINI = T - DT
        NUMLC=33
        CP=.FALSE.
        CALL REDECE('RIGI',1,1,NDIM,TYPMOD,IMATE,COMPLG,CRIT,INSTAM,
     &          INSTAP,6,DEFGEM(ADDEME+NDIM),DEPS,
     &          6,CONGEM(ADCOME),VINTM,OPTION,ANGMA1,1,R8BID,
     &          CP,NUMLC,TINI,T,TREF,
     &              CONGEP(ADCOME),VINTP,36,DSDEME,1,R8BID,RETCOM)
      ENDIF
      IF (MECA.EQ.'HOEK_BROWN_EFF') THEN
        COMPLG(1) = 'HOEK_BROWN_EFF'
        WRITE (COMPLG(2),'(I16)') NVIMEC
        MECTRU = .TRUE.
        TINI = T - DT
        NUMLC=33
        CP=.FALSE.
        CALL REDECE('RIGI',1,1,NDIM,TYPMOD,IMATE,COMPLG,CRIT,INSTAM,
     &          INSTAP,6,DEFGEM(ADDEME+NDIM),DEPS,
     &          6,CONGEM(ADCOME),VINTM,OPTION,ANGMA1,1,R8BID,
     &          CP,NUMLC,TINI,T,TREF,
     &              CONGEP(ADCOME),VINTP,36,DSDEME,1,R8BID,RETCOM)
      ENDIF
      IF (MECA.EQ.'DRUCK_PRAGER' .OR. MECA.EQ.'DRUCK_PRAG_N_A' ) THEN
         MECTRU = .TRUE.
         TINI = T - DT
         CALL LCDRPR(TYPMOD,OPTION,IMATE,MECA,CONGEM(ADCOME),
     &               TINI,T,TREF,DEPS,VINTM,VINTP,
     &               CONGEP(ADCOME),DSDEME,RETCOM)
      ENDIF
      IF (MECTRU) THEN
         IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.
     &       (OPTION(1:9).EQ.'FULL_MECA')) THEN
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
     &                           (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     &                            DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     &                            DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 206           CONTINUE
            ENDIF
         ENDIF
      ENDIF
C ======================================================================
C --- LOI L&K -----------------------------------------------------
C ======================================================================
      IF (MECA.EQ.'LETK') THEN
        TINI = T - DT
        CALL LKCOMP(TYPMOD,  IMATE,
     &              INSTAM, INSTAP,
     &              TINI,T, TREF,
     &              DEPS,
     &              CONGEM(ADCOME), VINTM, OPTION,
     &              CONGEP(ADCOME), VINTP,
     &              DSDEME,RETCOM)
        IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DO 302 I = 1 , 2*NDIM
           DO 303 J = 1 , 2*NDIM
            DSDE(ADCOME+I-1,ADDEME+NDIM+J-1)=DSDEME(I,J)
  303     CONTINUE
  302     CONTINUE
C ======================================================================
C --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
C --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
C ======================================================================
         IF (YATE.EQ.1) THEN
            DO 306 I=1,3
                  DSDE(ADCOME-1+I,ADDETE)=-ALPHA0*
     &            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 306        CONTINUE
         ENDIF
        ENDIF
      ENDIF
C ======================================================================
C --- LOI VISC_DRUC_PRAG -----------------------------------------------
C ======================================================================
      IF (MECA.EQ.'VISC_DRUC_PRAG') THEN
        TINI = T - DT
        CALL DPVPLC( TYPMOD, OPTION,  IMATE,  CRIT,
     &                      INSTAM, INSTAP,
     &                      TINI,T, TREF,
     &                      DEPS,
     &                      CONGEM(ADCOME), VINTM,
     &                      CONGEP(ADCOME), VINTP,
     &                      DSDEME,RETCOM)
        IF ((OPTION(1:9).EQ.'RIGI_MECA').OR.
     &      (OPTION(1:9).EQ.'FULL_MECA')) THEN
          DO 252 I = 1 , 2*NDIM
           DO 253 J = 1 , 2*NDIM
            DSDE(ADCOME+I-1,ADDEME+NDIM+J-1)=DSDEME(I,J)
  253     CONTINUE
  252     CONTINUE
C ======================================================================
C --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
C --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
C ======================================================================
         IF (YATE.EQ.1) THEN
            DO 256 I=1,3
                  DSDE(ADCOME-1+I,ADDETE)=-ALPHA0*
     &            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 256        CONTINUE
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
     &                      TINI,T, TREF,
     &                      DEPS,
     &                      CONGEM(ADCOME), VINTM, OPTION,
     &                      CONGEP(ADCOME), VINTP,
     &                      DSDEME,RETCOM)
        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN
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
     &            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
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
     &                      DEPS,
     &                      CONGEM(ADCOME), VINTM, OPTION,
     &                      CONGEP(ADCOME), VINTP,
     &                      DSDEME,P1,P2,DP1,DP2,
     &                      DSIDP1,SIPM,SIPP,RETCOM)
        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN
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
     &            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 416        CONTINUE
         ENDIF
        ENDIF
      ENDIF
C ======================================================================
C --- LOI ELAS_GONF ----------------------------------------------------
C ======================================================================
      IF (MECA.EQ.'ELAS_GONF') THEN
        TINI = T - DT
        SIPM=CONGEM(ADCOME+6)
        SIPP=CONGEP(ADCOME+6)
C
        CALL ELAGON(NDIM,IMATE,CRIT,SAT,BIOT,
     &              TINI,T,ALPHA0,
     &              DEPS,YOUNG,NU,
     &              CONGEM(ADCOME),OPTION,
     &              CONGEP(ADCOME),
     &              DSDEME,P1,P2,DP1,
     &              DSIDP1,DSIDP2)

        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN

            DO 522 I = 1 , 2*NDIM
              DSDE(ADCOME+I-1,ADDEP1) = DSDE(ADCOME+I-1,ADDEP1)
     &                      +DSIDP1(I)
              DO 521 J = 1 , 2*NDIM
                DSDE(ADCOME+I-1,ADDEME+NDIM+J-1)=DSDEME(I,J)
  521         CONTINUE
  522       CONTINUE

            IF (YAPRE2)THEN
              DO 523 I = 1 , 2*NDIM
                 DSDE(ADCOME+I-1,ADDEP2) = DSDE(ADCOME+I-1,ADDEP2)
     &                      +DSIDP2(I)
  523         CONTINUE
            ENDIF

C ======================================================================
C --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
C --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
C ======================================================================
          IF (YATE.EQ.1) THEN
            DO 4116 I=1,3
                  DSDE(ADCOME-1+I,ADDETE)=-ALPHA0*
     >            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     >             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 4116       CONTINUE
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
     &              DIMCON,DIMDEF,DSDE,DSPDP1,DSPDP2,PRE2TR)

        CALL LCHBR2( TYPMOD,OPTION,IMATE,CRIT,CONGEM(ADCOME),
     &   DEFGEM(ADDEME+NDIM),TINI,T,TREF,DEPS,VINTM,VINTP,DSPDP1,DSPDP2,
     &    SIPP,CONGEP(ADCOME),DSDEME,DSIDP1,DSIDP2,RETCOM)
        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN
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
     &            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
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
     &              DEFGEM(ADDEME+NDIM),DEPS, VINTM, TINI, T, TREF,
     &              OPTION,CONGEP(ADCOME),VINTP,DSDEME)
        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN
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
     &            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
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
     &                      DEPS,
     &                      VINTM,TINI,T,TREF,OPTION,
     &                      CONGEP(ADCOME), VINTP,DSDEME,CRIT)
        IF ((OPTION(1:16).EQ.'RIGI_MECA_TANG').OR.
     &            (OPTION(1:9).EQ.'FULL_MECA')) THEN
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
     &            (DSDE(ADCOME-1+I,ADDEME+NDIM-1+1)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+2)+
     &             DSDE(ADCOME-1+I,ADDEME+NDIM-1+3))/3.D0
 606        CONTINUE
         ENDIF
        ENDIF
      ENDIF
C ======================================================================
      END
