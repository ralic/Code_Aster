      SUBROUTINE DKTSNL ( NOMTE, OPT, XYZL, IMATE, IMATSE, CARCRI,
     &                    TEMPS, COMPOR,
     &                    CACOQ, NBCOU, LGPG, UL, DUL, SIGMOS, VARMOS,
     &                    SIGMOI, VARMOI, STYPSE, SIGPAS,
     &                    SIGPLU, VARPLU, US, DUS,
     &                    VECTU, SIGPLS, VARPLS )
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'
      INTEGER         NBCOU, LGPG, IMATE, IMATSE
      REAL*8          XYZL(3,*), UL(6,*), DUL(6,*), CACOQ(6), CARCRI(*)
      REAL*8          SIGMOS(6,*), VARMOS(LGPG,*), US(6,*), DUS(6,*)
      REAL*8          SIGMOI(6,*), VARMOI(LGPG,*), INSTM, INSTP
      REAL*8          SIGPLU(6,*), VARPLU(LGPG,*), SIGPAS(6,*), TEMPS(3)
      REAL*8          VECTU(6,*), SIGPLS(6,*), VARPLS(LGPG,*)
      CHARACTER*16    NOMTE, OPT, COMPOR(6)
      CHARACTER*24    STYPSE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRP_20
C TOLE CRP_21
C     ------------------------------------------------------------------
C     CALCULS DE SENSIBILITE POUR L'ELEMENT DE PLAQUE DKT
C     TOUTES LES ENTREES ET LES SORTIES SONT DANS LE REPERE LOCAL.
C     (MEME SI LES TABLEAUX SONT DIMENSIONNES EN 3D)
C     ------------------------------------------------------------------
C IN  NOMTE    : NOM DU TYPE ELEMENT
C IN  OPT      : OPTION A CALCULER :
C         'MECA_SENS_MATE'  SENSIBILITE PAR RAPPORT AU MATERIAU
C         'MECA_SENS_CHAR'  SENSIBILITE PAR RAPPORT AU CHARGEMENT
C         'MECA_SENS_EPAI'  SENSIBILITE PAR RAPPORT A L EPAISSEUR
C         'MECA_SENS_RAPH'  CONTRAINTES ET VARIABLES INTERNES SENSIBLES
C         'MECA_SENS_RAPHEP' CONTRAINTES ET VARIABLES INTERNES SENSIBLES
C                            PAR RAPPORT A L EPAISSEUR
C IN  XYZL    : COORDONNEES DES NOEUDS EN LOCAL
C IN  IMATE   : MATERIAU CODE
C IN  IMATSE  : MATERIAU CODE SENSIBLE
C IN  CARCRI  : PARAMETRES DE CONVERGENCE
C IN  INSTM   : INSTANT -
C IN  INSTP   : INSTANT +
C IN  COMPOR  : COMPORTEMENT
C IN  CACOQ   : CARAC COQUES
C IN  NBCOU   : NOMBRE DE COUCHES (INTEGRATION DE LA PLASTICITE)
C IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
C               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
C IN  UL      : DEPLACEMENT A L'INSTANT -
C IN  DUL     : INCREMENT DE DEPLACEMENT
C IN  SIGMOS  : CONTRAINTES SENSIBLE A L'INSTANT -
C IN  VARMOS  : V. INTERNES SENSIBLE A L'INSTANT -
C IN  SIGMOI  : CONTRAINTES A L'INSTANT PRECEDENT
C IN  VARMOI  : V. INTERNES A L'INSTANT -
C IN  STYPSE  : SOUS-TYPE DE SENSIBILITE
C IN  SIGPAS  : CONTRAINTES SENSIBLES PARTIELLES A L'INSTANT +
C IN  SIGPLU  : CONTRAINTES A L'INSTANT +
C IN  VARPLU  : V. INTERNES A L'INSTANT +
C IN  US      : DEPLACEMENT SENSIBLE A L'INSTANT -
C IN  DUS     : INCR DE DEPLACEMENT SENSIBLE
C OUT VECTU   : FORCES NODALES (MECA_SENS_MATE ET MECA_SENS_CHAR)
C OUT SIGPLS  : CONTRAINTES SENSIBLES A L'INSTANT +
C OUT VARPLS  : V. INTERNES SENSIBLES A L'INSTANT +
C     ------------------------------------------------------------------
C     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      REAL*8 R8VIDE
C
C --------- VARIABLES LOCALES :
C  -- GENERALITES :
C  ----------------
C  CMPS DE DEPLACEMENT :
C   - MEMBRANE : DX(N1), DY(N1), DX(N2), ..., DY(NNO)
C   - FLEXION  : DZ(N1), BETAX(N1), BETAY(N1), DZ(N2), ..., BETAY(NNO)
C  CMPS DE DEFORMATION ET CONTRAINTE PLANE (DANS UNE COUCHE) :
C   -            EPSIXX,EPSIYY,2*EPSIXY
C   -            SIGMXX,SIGMYY,SIGMXY
C  CMPS DE DEFORMATION ET CONTRAINTE PLANE POUR APPEL NSCOMP :
C   -            EPSIXX,EPSIYY,EPSIZZ,SQRT(2)*EPSIXY
C   -            SIGMXX,SIGMYY,SIGMZZ,SQRT(2)*SIGMXY
C  CMPS DE DEFORMATION COQUE :
C   - MEMBRANE : EPSIXX,EPSIYY,2*EPSIXY
C   - FLEXION  : KHIXX,KHIYY,2*KHIXY
C  CMPS D' EFFORTS COQUE :
C   - MEMBRANE : NXX,NYY,NXY
C   - FLEXION  : MXX,MYY,MXY
C --------------------------------------------------------------------
CCC      PARAMETER (NNO=3)  POUR LES DKT
CCC      PARAMETER (NNO=4)  POUR LES DKQ
      INTEGER    NNO
      PARAMETER (NNO=4)
      INTEGER  NPGH
C            NPG:    NOMBRE DE POINTS DE GAUSS PAR ELEMENT
C            NC :    NOMBRE DE COTES DE L'ELEMENT
C            NPGH:   NOMBRE DE POINT D'INTEGRATION PAR COUCHE
      REAL*8 POIDS, HIC, H, ZIC, DZIC, ZMIN, COEF
C            POIDS:  POIDS DE GAUSS (Y COMPRIS LE JACOBIEN)
C            HIC:    EPAISSEUR D'UNE COUCHE
C            H :     EPAISSEUR TOTALE DE LA PLAQUE
C            ZIC:    COTE DU POINT D'INTEGRATION DANS L'EPAISSEUR
C            DZIC:   DERIVEE DE LA COTE DU POINT D'INTEGRATION
C            ZMIN:   COTE DU POINT D'INTEGRATION LE PLUS "BAS"
C            COEF:   POIDS D'INTEGRATION PAR COUCHE
      REAL*8 UM(2,NNO), UF(3,NNO), DUM(2,NNO), DUF(3,NNO)
C            UM:     DEPLACEMENT (MEMBRANE) "-"
C            UF:     DEPLACEMENT (FLEXION)  "-"
C           DUM:     INCREMENT DEPLACEMENT (MEMBRANE)
C           DUF:     INCREMENT DEPLACEMENT (FLEXION)
      REAL*8 USM(2,NNO), USF(3,NNO), DUSM(2,NNO), DUSF(3,NNO)
C            USM:     DEPLACEMENT SENSIBLE (MEMBRANE) "-"
C            USF:     DEPLACEMENT SENSIBLE (FLEXION)  "-"
C           DUSM:     INCREMENT DEPLACEMENT SENSIBLE (MEMBRANE)
C           DUSF:     INCREMENT DEPLACEMENT SENSIBLE (FLEXION)
      REAL*8 EPS2D(6), DEPS2D(6), DSIDEP(6,6)
C            EPS2D:  DEFORMATION DANS UNE COUCHE (2D C_PLAN)
C           DEPS2D:  INCREMENT DEFORMATION DANS UNE COUCHE (2D C_PLAN)
C           DSIDEP:  MATRICE D(SIG2D)/D(EPS2D)
      REAL*8 EPS(3), KHI(3), DEPS(3), DKHI(3), N(3), M(3)
C            EPS:    DEFORMATION DE MEMBRANE "-"
C            DEPS:   INCREMENT DE DEFORMATION DE MEMBRANE
C            KHI:    DEFORMATION DE FLEXION  "-"
C            DKHI:   INCREMENT DE DEFORMATION DE FLEXION
C            N  :    EFFORT NORMAL "+"
C            M  :    MOMENT FLECHISSANT "+"
      REAL*8 BF(3,3*NNO), BM(3,2*NNO)
C            BF :    MATRICE "B" (FLEXION)
C            BM :    MATRICE "B" (MEMBRANE)
      REAL*8 DF(9), DM(9), DMF(9), D2D(9)
C            D2D:    MATRICE DE RIGIDITE TANGENTE MATERIELLE (2D)
C            DF :    MATRICE DE RIGIDITE TANGENTE MATERIELLE (FLEXION)
C            DM :    MATRICE DE RIGIDITE TANGENTE MATERIELLE (MEMBRANE)
C            DMF:    MATRICE DE RIGIDITE TANGENTE MATERIELLE (COUPLAGE)
      REAL*8 FLEX(3*NNO*3*NNO), MEMB(2*NNO*2*NNO)
      REAL*8 MEFL(2*NNO*3*NNO), WORK(3*NNO*3*NNO)
C           MEMB:    MATRICE DE RIGIDITE DE MEMBRANE
C           FLEX:    MATRICE DE RIGIDITE DE FLEXION
C           WORK:    TABLEAU DE TRAVAIL
C           MEFL:    MATRICE DE COUPLAGE MEMBRANE-FLEXION
C             LE MATERIAU EST SUPPOSE HOMOGENE
C             IL PEUT NEANMOINS Y AVOIR COUPLAGE PAR LA PLASTICITE
C     ------------------ PARAMETRAGE ELEMENT ---------------------------
      INTEGER  NDIM, NNOEL, NNOS, NPG, IAD, I1
      INTEGER  IPOIDS, ICOOPG, IVF, IDFDX, IDFD2, JGANO, KL
      INTEGER  JTAB(7), COD, I, IADZI, IAZK24, KSP,ICOU, ICPG, IGAUH
      INTEGER  INO, IPG, IRET,IVSP, J, K, NBSP, NBVAR
      REAL*8   DEUX, RAC2, QSI, ETA, CARA(25), JACOB(5), DISTN, LC, CTOR
      REAL*8   DEDT(6),DEDTM(6),DEDTF(6),ANGMAS(3), KTAN(300)
      REAL*8   SIGM(6),SIGP(6),SIGMS(6),SIGPS(6),SIPAS(6),KPRIMU(24)
      REAL*8   SIGBID(360),VARBID(120),WK
      REAL*8   COE, COE1, COE2, KTANG(24,24), DULN(24)
      LOGICAL  DKT,DKQ
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16 OPTI
      CHARACTER*24 NOMELE
C     ------------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNOEL,NNOS,NPG,IPOIDS,ICOOPG,
     &                                         IVF,IDFDX,IDFD2,JGANO)
C
      DEUX   = 2.D0
      RAC2   = SQRT(DEUX)
      DKT    = .FALSE.
      DKQ    = .FALSE.

      IF (NOMTE.EQ.'MEDKTR3 ') THEN
        DKT = .TRUE.
      ELSEIF (NOMTE.EQ.'MEDKQU4 ') THEN
        DKQ = .TRUE.
      ELSE
        CALL U2MESK('F','ELEMENTS_34',1,NOMTE)
      END IF

      CALL TECACH('OON','PCONTMR',7,JTAB,IRET)
      NBSP=JTAB(7)
      CALL ASSERT(NPG.EQ.JTAB(3))
C
C     -- GRANDEURS GEOMETRIQUES :
C     ---------------------------
      H = CACOQ(1)
C     EXCENTREMENT NON TRAITE AVEC STAT_NON_LINE
      DISTN = CACOQ(5)
      IF (DISTN.NE.0.D0) THEN
         CALL TECAEL(IADZI, IAZK24)
         NOMELE=ZK24(IAZK24-1+3)
         CALL U2MESK('F','ELEMENTS_35',1,NOMELE)
      ENDIF
      IF ( DKT ) THEN
         CALL GTRIA3(XYZL,CARA)
         CTOR = CACOQ(4)
      ELSEIF ( DKQ ) THEN
         CALL GQUAD4(XYZL,CARA)
         CTOR = CACOQ(4)
      END IF
C
C     -- MISE A ZERO :
C     ---------------
      IF(OPT.EQ.'MECA_SENS_MATE'.OR.
     &   OPT.EQ.'MECA_SENS_CHAR'.OR.
     &   OPT.EQ.'MECA_SENS_EPAI') THEN
        CALL R8INIR(6*NNOEL,0.D0,VECTU,1)
      END IF
      IF(OPT.EQ.'MECA_SENS_EPAI') THEN
        CALL R8INIR((3*NNOEL)*(3*NNOEL),0.D0,FLEX,1)
        CALL R8INIR((2*NNOEL)*(2*NNOEL),0.D0,MEMB,1)
        CALL R8INIR((2*NNOEL)*(3*NNOEL),0.D0,MEFL,1)
      END IF

C     -- PARTITION DU DEPLACEMENT EN MEMBRANE/FLEXION :
C     -------------------------------------------------
      DO 30 INO = 1,NNOEL
        UM(1,INO) =  UL(1,INO)
        UM(2,INO) =  UL(2,INO)
        UF(1,INO) =  UL(3,INO)
        UF(2,INO) =  UL(5,INO)
        UF(3,INO) = -UL(4,INO)
        DUM(1,INO) =  DUL(1,INO)
        DUM(2,INO) =  DUL(2,INO)
        DUF(1,INO) =  DUL(3,INO)
        DUF(2,INO) =  DUL(5,INO)
        DUF(3,INO) = -DUL(4,INO)
   30 CONTINUE
      IF(OPT(1:14).EQ.'MECA_SENS_RAPH') THEN
        DO 40 INO = 1,NNOEL
          USM(1,INO) =  US(1,INO)
          USM(2,INO) =  US(2,INO)
          USF(1,INO) =  US(3,INO)
          USF(2,INO) =  US(5,INO)
          USF(3,INO) = -US(4,INO)
          DUSM(1,INO) =  DUS(1,INO)
          DUSM(2,INO) =  DUS(2,INO)
          DUSF(1,INO) =  DUS(3,INO)
          DUSF(2,INO) =  DUS(5,INO)
          DUSF(3,INO) = -DUS(4,INO)
   40   CONTINUE
      END IF
C     -- INTEGRATIONS SUR LA SURFACE DE L'ELEMENT:
C     --------------------------------------------
C     -- POUR POUVOIR UTILISER NMCOMP
C     -- POUR POUVOIR UTILISER NSCOMP (MODIFIER DPMATE)
      TYPMOD(1) = 'C_PLAN  '
      TYPMOD(2) = '        '
      NPGH = 3

C     NBVAR: NOMBRE DE VARIABLES INTERNES (2D) LOI COMPORT
      READ (COMPOR(2),'(I16)') NBVAR
      IF (NBCOU.LE.0) CALL U2MESK('F','ELEMENTS_36',1,COMPOR(6))

      HIC = H/NBCOU
      ZMIN = -H/DEUX + DISTN
C===============================================================

C     -- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
C     -------------------------------------------------
      DO 130,IPG = 1,NPG
        CALL R8INIR(3,0.D0,N,1)
        CALL R8INIR(3,0.D0,M,1)
        CALL R8INIR(9,0.D0,DF,1)
        CALL R8INIR(9,0.D0,DM,1)
        CALL R8INIR(9,0.D0,DMF,1)
        QSI = ZR(ICOOPG-1+NDIM*(IPG-1)+1)
        ETA = ZR(ICOOPG-1+NDIM*(IPG-1)+2)
        IF ( DKQ ) THEN
          CALL JQUAD4 ( XYZL, QSI, ETA, JACOB )
          POIDS = ZR(IPOIDS+IPG-1)*JACOB(1)
          CALL DXQBM ( QSI, ETA, JACOB(2), BM )
          CALL DKQBF ( QSI, ETA, JACOB(2), CARA, BF )
        ELSE
          POIDS = ZR(IPOIDS+IPG-1)*CARA(7)
          CALL DXTBM ( CARA(9), BM )
          CALL DKTBF ( QSI, ETA, CARA, BF )
        ENDIF

C       -- CALCUL DE EPS, DEPS, KHI, DKHI :
C       -----------------------------------
      IF(OPT.EQ.'MECA_SENS_MATE'.OR.
     &   OPT.EQ.'MECA_SENS_CHAR'.OR.
     &   OPT.EQ.'MECA_SENS_EPAI') THEN
          CALL PMRVEC('ZERO',3,2*NNOEL,BM, UM, EPS)
          CALL PMRVEC('ZERO',3,2*NNOEL,BM,DUM,DEPS)
          CALL PMRVEC('ZERO',3,3*NNOEL,BF, UF, KHI)
          CALL PMRVEC('ZERO',3,3*NNOEL,BF,DUF,DKHI)
      ELSE IF(OPT(1:14).EQ.'MECA_SENS_RAPH') THEN
          CALL PMRVEC('ZERO',3,2*NNOEL,BM, DUM, DEDTM)
          CALL PMRVEC('ZERO',3,3*NNOEL,BF, DUF, DEDTF)
          CALL PMRVEC('ZERO',3,2*NNOEL,BM, USM, EPS)
          CALL PMRVEC('ZERO',3,3*NNOEL,BF, USF, KHI)
          CALL PMRVEC('ZERO',3,2*NNOEL,BM, DUSM, DEPS)
          CALL PMRVEC('ZERO',3,3*NNOEL,BF, DUSF, DKHI)
      ENDIF

C       -- CALCUL DE L'ECOULEMENT PLASTIQUE SUR CHAQUE COUCHE:
C          PAR INTEGRATION EN TROIS POINTS
C       ------------------------------------------------------
        DO 80,ICOU = 1,NBCOU
          DO 70,IGAUH = 1,NPGH
            KSP=(ICOU-1)*NPGH+IGAUH
            IVSP = (ICOU-1)*NPGH*NBVAR + NBVAR*(IGAUH-1)+1
            ICPG = (IPG-1)*NBSP + KSP

C       -- COTE DES POINTS D'INTEGRATION
C       --------------------------------
            IF (IGAUH.EQ.1) THEN
              ZIC = ZMIN + (ICOU-1)*HIC
              COEF = 1.D0/3.D0
            ELSE IF (IGAUH.EQ.2) THEN
              ZIC = ZMIN + HIC/DEUX + (ICOU-1)*HIC
              COEF = 4.D0/3.D0
            ELSE
              ZIC = ZMIN + HIC + (ICOU-1)*HIC
              COEF = 1.D0/3.D0
            END IF

C         -- CALCUL DE EPS2D ET DEPS2D :
C         --------------------------
            EPS2D(1) = EPS(1) + ZIC*KHI(1)
            EPS2D(2) = EPS(2) + ZIC*KHI(2)
            EPS2D(3) = 0.0D0
            EPS2D(4) = (EPS(3)+ZIC*KHI(3))/RAC2
            EPS2D(5) = 0.D0
            EPS2D(6) = 0.D0
            DEPS2D(1) = DEPS(1) + ZIC*DKHI(1)
            DEPS2D(2) = DEPS(2) + ZIC*DKHI(2)
            DEPS2D(3) = 0.0D0
            DEPS2D(4) = (DEPS(3)+ZIC*DKHI(3))/RAC2
            DEPS2D(5) = 0.D0
            DEPS2D(6) = 0.D0
            IF(OPT(1:14).EQ.'MECA_SENS_RAPH') THEN
              DEDT(1) = DEDTM(1) + ZIC*DEDTF(1)
              DEDT(2) = DEDTM(2) + ZIC*DEDTF(2)
              DEDT(3) = 0.0D0
              DEDT(4) = (DEDTM(3)+ZIC*DEDTF(3))/RAC2
              DEDT(5) = 0.D0
              DEDT(6) = 0.D0
              IF(OPT.EQ.'MECA_SENS_RAPHEP') THEN
C   TERME SUPPLEMENTAIRE DANS LE CAS DERIVEE EPAISSEUR
                DZIC = ZIC/H
                DEPS2D(1) = DEPS2D(1) + DZIC*DEDTF(1)
                DEPS2D(2) = DEPS2D(2) + DZIC*DEDTF(2)
                DEPS2D(4) = DEPS2D(4) + DZIC*DEDTF(3)/RAC2
              END IF
            END IF

            DO 44 I = 1,3
              SIGMS(I) = SIGMOS(I,ICPG)
   44       CONTINUE
            DO 50 I = 4,6
              SIGMS(I) = SIGMOS(I,ICPG)*RAC2
   50       CONTINUE
C
            DO 41 I = 1,3
              SIGM(I) = SIGMOI(I,ICPG)
   41       CONTINUE
            DO 51 I = 4,6
              SIGM(I) = SIGMOI(I,ICPG)*RAC2
   51       CONTINUE
C
            DO 42 I = 1,3
              SIGP(I) = SIGPLU(I,ICPG)
   42       CONTINUE
            DO 52 I = 4,6
              SIGP(I) = SIGPLU(I,ICPG)*RAC2
   52       CONTINUE

            IF (OPT(1:14).EQ.'MECA_SENS_RAPH') THEN
              DO 54 I = 1,3
               SIPAS(I) = SIGPAS(I,ICPG)
   54         CONTINUE
              DO 56 I = 4,6
                SIPAS(I) = SIGPAS(I,ICPG)*RAC2
   56         CONTINUE
            ELSE
              DO 57 I = 1,6
                SIPAS(I) = 0.D0
   57         CONTINUE
            ENDIF

            IF(OPT.EQ.'MECA_SENS_EPAI') THEN
C         -- APPEL A NMCOMP POUR CALCULER LA MATRICE TANGENTE :
C         -----------------------------------------------------
C --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C --- INITIALISE A R8VIDE (ON NE S'EN SERT PAS)
               CALL R8INIR(3,  R8VIDE(), ANGMAS ,1)
               INSTP = TEMPS(1)
               INSTM = TEMPS(1) - TEMPS(2)
               OPTI= 'RIGI_MECA_TANG'
               CALL NMCOMP('RIGI',IPG,KSP,2,TYPMOD,IMATE,COMPOR,
     &                  CARCRI,INSTM,INSTP,6,EPS2D,DEPS2D,6,SIGM,
     &                  VARMOI(IVSP,IPG),OPTI,ANGMAS,1,LC,
     &                  SIGBID,VARBID,36,DSIDEP,1,WK,COD)
C           COD=1 : ECHEC INTEGRATION LOI DE COMPORTEMENT
               IF (COD.NE.0) THEN
                 CALL U2MESS('F','UTILITAI3_8')
               ENDIF
            ENDIF
C   -- APPEL A NSCOMP POUR RESOUDRE LE PB DERIVE SUR LA COUCHE :
C   ------------------------------------------------------------
           CALL NSCOMP(OPT,TYPMOD,COMPOR,3,IMATE,IMATSE,
     &                 EPS2D,DEPS2D,DEDT,SIGMS,VARMOS(IVSP,IPG),
     &                 VARMOI(IVSP,IPG),SIGM,VARPLU(IVSP,IPG),SIPAS,
     &                 SIGP,SIGPS,VARPLS(IVSP,IPG),STYPSE)

C            DIVISION DE LA CONTRAINTE DE CISAILLEMENT PAR SQRT(2)
C            POUR STOCKER LA VALEUR REELLE
           DO 90 KL = 1,3
             SIGPLS(KL,ICPG) = SIGPS(KL)
   90      CONTINUE
           DO 95 KL = 4,6
             SIGPLS(KL,ICPG) = SIGPS(KL)/RAC2
   95      CONTINUE

            IF(OPT.EQ.'MECA_SENS_MATE'.OR.
     &         OPT.EQ.'MECA_SENS_CHAR') THEN
C         -- CALCUL DES EFFORTS RESULTANTS DANS L'EPAISSEUR (N ET M) :
C         ------------------------------------------------------------
              COE1 = COEF*HIC/DEUX
              COE2 = COE1*ZIC
              N(1) = N(1) + COE1*SIGPLS(1,ICPG)
              N(2) = N(2) + COE1*SIGPLS(2,ICPG)
              N(3) = N(3) + COE1*SIGPLS(4,ICPG)
              M(1) = M(1) + COE2*SIGPLS(1,ICPG)
              M(2) = M(2) + COE2*SIGPLS(2,ICPG)
              M(3) = M(3) + COE2*SIGPLS(4,ICPG)
            ELSE IF(OPT.EQ.'MECA_SENS_EPAI') THEN
C           -- ON EXTRAIT DE DSIDEP LA SOUS-MATRICE INTERESSANTE D2D:
              D2D(1) = DSIDEP(1,1)
              D2D(2) = DSIDEP(1,2)
              D2D(3) = DSIDEP(1,4)/RAC2
              D2D(4) = DSIDEP(2,1)
              D2D(5) = DSIDEP(2,2)
              D2D(6) = DSIDEP(2,4)/RAC2
              D2D(7) = DSIDEP(4,1)/RAC2
              D2D(8) = DSIDEP(4,2)/RAC2
              D2D(9) = DSIDEP(4,4)/DEUX
C           -- ON DERIVE LES MATRICES PAR RAPPORT A H :
C           -------------------------------------------
              COE =  COEF*HIC/H/DEUX*POIDS
              DO 60 K = 1,9
                 DM(K) =  DM(K)  + COE*D2D(K)
                 DMF(K) = DMF(K) + COE*ZIC*D2D(K)
                 DF(K) =  DF(K)  + 3.0D0*COE*ZIC*ZIC*D2D(K)
   60         CONTINUE
            ENDIF
   70     CONTINUE
   80   CONTINUE
C
      IF(OPT.EQ.'MECA_SENS_MATE'.OR.
     &   OPT.EQ.'MECA_SENS_CHAR') THEN

C       -- CALCUL DU SECOND MEMBRE DR/DPS:
C       ---------------------------------
C       VECTU = VECTU - BFT*M - BMT*N
            DO 120,INO = 1,NNOEL
            DO 110,K = 1,3
              VECTU(1,INO) = VECTU(1,INO) -
     &                       BM(K,2* (INO-1)+1)*N(K)*POIDS
              VECTU(2,INO) = VECTU(2,INO) -
     &                       BM(K,2* (INO-1)+2)*N(K)*POIDS
              VECTU(3,INO) = VECTU(3,INO) -
     &                       BF(K,3* (INO-1)+1)*M(K)*POIDS
              VECTU(5,INO) = VECTU(5,INO) -
     &                       BF(K,3* (INO-1)+2)*M(K)*POIDS
              VECTU(4,INO) = VECTU(4,INO) +
     &                       BF(K,3* (INO-1)+3)*M(K)*POIDS
  110       CONTINUE
  120       CONTINUE
      ELSE IF(OPT.EQ.'MECA_SENS_EPAI') THEN
C       -- CALCUL DE LA DERIVEE DE LA MATRICE TANGENTE :
C       ----------------------------------------------
C       KTANG = KTANG + BFT*DF*BF + BMT*DM*BM + BMT*DMF*BF
C         -- MEMBRANE :
C         -------------
              CALL UTBTAB('CUMU',3,2*NNOEL,DM,BM,WORK,MEMB)
C         -- FLEXION :
C         ------------
              CALL UTBTAB('CUMU',3,3*NNOEL,DF,BF,WORK,FLEX)
C         -- COUPLAGE:
C         ------------
              CALL UTCTAB('CUMU',3,3*NNOEL,2*NNOEL,DMF,BF,BM,WORK,MEFL)
      ENDIF
C       -- FIN BOUCLE SUR LES POINTS DE GAUSS
  130 CONTINUE

      IF(OPT.EQ.'MECA_SENS_EPAI') THEN
C
C     -- ACCUMULATION DES SOUS MATRICES DANS KTAN :
C     -------------------------------------------
        IF (DKT) THEN
          CALL DXTLOC(FLEX,MEMB,MEFL,CTOR,KTAN)
        ELSEIF ( DKQ ) THEN
          CALL DXQLOC(FLEX,MEMB,MEFL,CTOR,KTAN)
        END IF
C PASSAGE VECTEUR - MATRICE POUR KTAN
          I1 = 0
          DO 190 I = 1 , 6*NNOEL
            DO 191 J = 1 , 6*NNOEL
              I1 = I1 + 1
              KTANG(I,J) = KTAN(I1)
              KTANG(J,I) = KTAN(I1)
              IF(J.EQ.I)GOTO 190
191         CONTINUE
190       CONTINUE
C PASSAGE MATRICE - VECTEUR POUR DUL
          I1 = 0
          DO 192 I = 1 , NNOEL
            DO 193 J = 1 , 6
              I1 = I1 + 1
              DULN(I1) = DUL(J,I)
193         CONTINUE
192       CONTINUE
C
C  PRODUIT KTAN * U ET STOCKAGE DE L OPPOSE DANS VECTU
C
        DO 210 I = 1,6*NNOEL
          KPRIMU(I) = 0.0D0
          DO 210 J = 1,6*NNOEL
             KPRIMU(I) = KPRIMU(I) + KTANG(I,J) * DULN(J)
  210   CONTINUE
        DO 220 INO = 1,NNOEL
          DO 230 K = 1,6
              IAD = (INO-1)*6 + K
              VECTU(K,INO) =  - KPRIMU(IAD)
  230     CONTINUE
  220   CONTINUE
      END IF
      END
