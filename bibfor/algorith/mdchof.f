      SUBROUTINE MDCHOF(NP1,NP2,NP3,NBM,IMPR,TC,
     &                  NBNL,TYPCH,NBSEG,PHII,NOMCH,
     &                  CHOC,ALPHA,BETA,GAMMA,ORIG,RC,THETA,
     &                  VITG,DEPG,VITG0,DEPG0,
     &                  OLD,OLDIA,FMRES,FMOD,FTMP,
     &                  TESTC,ITFORN,TOLN)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/01/2010   AUTEUR MACOCCO K.MACOCCO 
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
C TOLE  CRP_21
C-----------------------------------------------------------------------
C DESCRIPTION : ESTIMATION ET "LINEARISATION" DE LA FORCE NON-LINEAIRE
C -----------   (NON-LINEARITE DE TYPE CHOC)
C               CALCUL DE LA FORCE EXTERIEURE APRES PASSAGE DE
C               L'ALGORITHME TEMPOREL
C
C               APPELANT : MDITM2
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER      NP1, NP2, NP3, NBM, IMPR
      REAL*8       TC
      INTEGER      NBNL, TYPCH(*), NBSEG(*)
      REAL*8       PHII(NP2,NP1,*)
      CHARACTER*8  NOMCH(*)
      REAL*8       CHOC(6,*), ALPHA(2,*), BETA(2,*), GAMMA(2,*),
     &             ORIG(6,*), RC(NP3,*), THETA(NP3,*),
     &             VITG(*), DEPG(*), VITG0(*), DEPG0(*), OLD(9,*)
      INTEGER      OLDIA(*)
      REAL*8       FMRES(*), FMOD(*), FTMP(*)
      INTEGER      TESTC, ITFORN(*)
      REAL*8       TOLN
C
C VARIABLES LOCALES
C -----------------
      INTEGER      IC, J, IFORN, TYPOBS, NBS, ITESTC, ITEST0, TESTCV,
     &             IFR, IFM, LATEST, NBCHSI
      REAL*8       XGLO0(3), XXGLO0(3), XLOC0(3), VGLO0(3), VLOC0(3),
     &             XGLO(3), XXGLO(3), XLOC(3), VGLO(3), VLOC(3),
     &             X00(3), EXCLOC(3), XORIG(3), VORIG(3),
     &             SINA, COSA, SINB, COSB, SING, COSG,
     &             XJEU, COST, SINT, DNORM,
     &             FGLO(3), FLOC(3), FGLRES(3), FLRES(3),
     &             JACOBC(3,3), JACOBK(3,3), CHOCKC(2), TETAJ
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC    ABS
C
C FONCTIONS EXTERNES
C ------------------
      INTEGER      IUNIFI
C     EXTERNAL     IUNIFI
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL     CALJAC, CALRES, DEIMPJ, DISBUT, FTEST1, GLOLOC,
C    &             LCINVN, LOCGLO, PROJGM, PROJMG
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C  0. INITIALISATIONS
C     ---------------
C
      IF ( IMPR.GE.1 ) THEN
         IFM = IUNIFI('MESSAGE')
         IF ( IMPR.GE.2 ) IFR = IUNIFI('RESULTAT')
      ENDIF
C
      CALL VECINI(NP1,0.D0,FMOD)
      CALL VECINI(NP1,0.D0,FMRES)
      CALL VECINI(3,0.D0,FLOC)
C
      LATEST = TESTC
      TESTC = 0
C
      NBCHSI = 0
C
C  1. BOUCLE SUR LES NON-LINEARITES
C     -----------------------------
C
      DO 10 IC = 1, NBNL
C
         TETAJ = 1.0D0
C
         TYPOBS = TYPCH(IC)
         NBS    = NBSEG(IC)
C
         IF ( (TYPOBS.EQ.0).OR.(TYPOBS.EQ.1).OR.(TYPOBS.EQ.2) )
     &      XJEU = RC(1,IC)
C
C  1.1.  CONVERSION DDLS GENERALISES -> DDLS PHYSIQUES
C        ---------------------------------------------
C        (DEPLACEMENTS)
         CALL PROJMG(NP1,NP2,IC,NBM,PHII,DEPG,XGLO)
         CALL PROJMG(NP1,NP2,IC,NBM,PHII,DEPG0,XGLO0)
C        (VITESSES)
         CALL PROJMG(NP1,NP2,IC,NBM,PHII,VITG,VGLO)
         CALL PROJMG(NP1,NP2,IC,NBM,PHII,VITG0,VGLO0)
C
C  1.2.  PASSAGE REPERE GLOBAL -> LOCAL
C        ------------------------------
         XORIG(1) = ORIG(1,IC)
         XORIG(2) = ORIG(2,IC)
         XORIG(3) = ORIG(3,IC)
         VORIG(1) = 0.0D0
         VORIG(2) = 0.0D0
         VORIG(3) = 0.0D0
C
         X00(1) = ORIG(4,IC)
         X00(2) = ORIG(5,IC)
         X00(3) = ORIG(6,IC)
C
         XXGLO(1)  = XGLO(1)  + X00(1)
         XXGLO(2)  = XGLO(2)  + X00(2)
         XXGLO(3)  = XGLO(3)  + X00(3)
         XXGLO0(1) = XGLO0(1) + X00(1)
         XXGLO0(2) = XGLO0(2) + X00(2)
         XXGLO0(3) = XGLO0(3) + X00(3)
C
         SINA = ALPHA(1,IC)
         COSA = ALPHA(2,IC)
         SINB = BETA(1,IC)
         COSB = BETA(2,IC)
         SING = GAMMA(1,IC)
         COSG = GAMMA(2,IC)
C        (DEPLACEMENTS)
         CALL GLOLOC(XXGLO,XORIG,SINA,COSA,SINB,COSB,SING,COSG,XLOC)
         CALL GLOLOC(XXGLO0,XORIG,SINA,COSA,SINB,COSB,SING,COSG,XLOC0)
C        (VITESSES)
         CALL GLOLOC(VGLO,VORIG,SINA,COSA,SINB,COSB,SING,COSG,VLOC)
         CALL GLOLOC(VGLO0,VORIG,SINA,COSA,SINB,COSB,SING,COSG,VLOC0)
C        (EXCENTREMENT)
         CALL GLOLOC(X00,XORIG,SINA,COSA,SINB,COSB,SING,COSG,EXCLOC)
C
C  1.3.  TEST CHOC SUR BUTEE IC
C        ----------------------
         CALL DISBUT(NP3,IC,XLOC,TYPOBS,XJEU,RC,THETA,NBS,COST,SINT,
     &               DNORM)
         CALL FTEST1(NP3,RC,THETA,TYPCH,NBSEG,XLOC,IC,ITESTC,TOLN)
         CALL FTEST1(NP3,RC,THETA,TYPCH,NBSEG,XLOC0,IC,ITEST0,TOLN)
         TESTCV = 0
         CALL DEIMPJ(ITESTC,ITEST0,TETAJ,TESTCV)
C
C  1.4.  CALCULS REALISES EN CAS DE CHOC SUR LA BUTEE IC
C        -----------------------------------------------
         IF ( TESTCV.EQ.1 ) THEN
C
            TESTC = 1
C
C  1.4.1.   CALCUL DES MATRICES JACOBIENNES LIEES A LA FORCE NON-LIN
C           --------------------------------------------------------
            IFORN = ITFORN(IC)
            IF ( IFORN.EQ.0 ) THEN
               CHOCKC(1) = CHOC(1,IC)
               CHOCKC(2) = CHOC(2,IC)
            ELSE
               CHOCKC(1) = CHOC(1,IC)
               CHOCKC(2) = 0.0D0
            ENDIF
            CALL CALJAC(NP3,IC,TYPCH,NBSEG,CHOCKC,RC,THETA,
     &                  VLOC,XLOC,VLOC0,XLOC0,TETAJ,JACOBC,JACOBK)
C
C  1.4.2.   CALCUL DES FORCES NON-LINEAIRES ET RESIDUELLES
C           ----------------------------------------------
            CALL CALRES(NP3,IC,TYPCH,NBSEG,CHOC,RC,THETA,
     &                  VLOC,XLOC,VLOC0,XLOC0,EXCLOC,TETAJ,
     &                  JACOBC,JACOBK,FLOC,FLRES,OLD,OLDIA,IFORN,TOLN)
C
C  1.4.3.   PASSAGE REPERE LOCAL -> GLOBAL (FORCES RESIDUELLES)
C           ---------------------------------------------------
            CALL LOCGLO(FLOC,SINA,COSA,SINB,COSB,SING,COSG,FGLO)
            CALL LOCGLO(FLRES,SINA,COSA,SINB,COSB,SING,COSG,FGLRES)
C
C  1.4.4.   CONVERSION DDLS PHYSIQUES -> DDLS GENERALISES
C           (FORCES MODALES)
C           ----------------
            CALL VECINI(NP1,0.D0,FTMP)
            CALL PROJGM(NP1,NP2,IC,NBM,PHII,FGLO,FTMP)
            DO 20 J = 1, NBM
               FMOD(J) = FMOD(J) + FTMP(J)
  20        CONTINUE
C
C  1.4.5.   CONVERSION DDLS PHYSIQUES -> DDLS GENERALISES
C           (FORCES RESIDUELLES)
C           --------------------
            CALL VECINI(NP1,0.D0,FTMP)
            CALL PROJGM(NP1,NP2,IC,NBM,PHII,FGLRES,FTMP)
            DO 30 J = 1, NBM
               FMRES(J) = FMRES(J) + FTMP(J)
  30        CONTINUE
C
C  1.4.6.   IMPRESSIONS LE CAS ECHEANT
C           --------------------------
C.......... ITEST0.LT.1
C.......... <=> VOL OU CONTACT EXACT A L'INSTANT PRECEDENT
            IF ( (ITEST0.LT.1).AND.(IMPR.GE.1) ) THEN
               NBCHSI = NBCHSI + 1
               WRITE(IFM,1001) NOMCH(IC), TC
               IF ( IMPR.GE.2 ) WRITE(IFR,1001) NOMCH(IC), TC
            ENDIF
C
C  1.5.  REMPLISSAGE DU TABLEAU OLD LORSQU'IL N'Y A PAS DE CHOC
C        SUR LA BUTEE IC
C        ---------------
         ELSE
C
C  1.5.1.   REMPLISSAGE DU TABLEAU OLD
C           --------------------------
            OLD(1,IC) = -SINT*VLOC(2) + COST*VLOC(3)
            OLD(2,IC) = VLOC(1)
            OLD(3,IC) = 0.0D0
            OLD(4,IC) = 0.0D0
            OLD(5,IC) = XLOC(1)
            OLD(6,IC) = XLOC(2)
            OLD(7,IC) = XLOC(3)
            OLD(8,IC) = 0.0D0
            OLD(9,IC) = COST*VLOC(2) + SINT*VLOC(3)
C
C  1.5.2.   IMPRESSIONS LE CAS ECHEANT
C           --------------------------
C.......... ABS(ITEST0).EQ.1
C.......... <=> CONTACT EXACT OU CHOC A L'INSTANT PRECEDENT
            IF ( (ABS(ITEST0).EQ.1).AND.(IMPR.GE.1) ) THEN
               WRITE(IFM,1002) NOMCH(IC), TC
               IF ( IMPR.GE.2 ) WRITE(IFR,1002) NOMCH(IC), TC
            ENDIF
C
         ENDIF
C
  10  CONTINUE
C
C  2. IMPRESSIONS COMPLEMENTAIRES LE CAS ECHEANT
C     ------------------------------------------
C
      IF ( (LATEST.NE.TESTC).AND.(IMPR.GE.1) ) THEN
         IF ( (TESTC.EQ.1).AND.(NBCHSI.GT.1) ) THEN
            WRITE(IFM,1003) NBCHSI, TC
            IF ( IMPR.GE.2 ) WRITE(IFR,1003) NBCHSI, TC
         ELSE IF ( TESTC.EQ.0 ) THEN
            WRITE(IFM,1004) TC
            IF ( IMPR.GE.2 ) WRITE(IFR,1004) TC
         ENDIF
      ENDIF
C
C  3. FORMATS
C     -------
C
 1001 FORMAT(1X,'PASSAGE EN PHASE DE CHOC AU NOEUD ',A8,7X,
     &' - TEMPS COURANT: ',1PE12.5E2/)
 1002 FORMAT(1X,'PASSAGE EN PHASE DE VOL  AU NOEUD ',A8,7X,
     &' - TEMPS COURANT: ',1PE12.5E2/)
 1003 FORMAT(1X,'PASSAGE EN PHASE DE CHOCS SIMULTANES EN ',I2,' NOEUDS',
     &' - TEMPS COURANT: ',1PE12.5E2/)
 1004 FORMAT(1X,'PASSAGE EN PHASE DE VOL',26X,
     &' - TEMPS COURANT: ',1PE12.5E2/)
C
C --- FIN DE MDCHOF.
      END
