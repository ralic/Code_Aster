      SUBROUTINE DKTNLI (NOMTE,OPT,XYZL,UL,DUL,BTSIG,KTAN,CODRET)
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'
      INTEGER         CODRET
      REAL*8          XYZL(3,*), UL(6,*), DUL(6,*)
      REAL*8          KTAN(*), BTSIG(6,*)
      CHARACTER*16    NOMTE, OPT

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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     ------------------------------------------------------------------
C     CALCUL DES OPTIONS NON LINEAIRES POUR L'ELEMENT DE PLAQUE DKT
C     TOUTES LES ENTREES ET LES SORTIES SONT DANS LE REPERE LOCAL.
C     (MEME SI LES TABLEAUX SONT DIMMENSIONNES EN 3D)
C     ------------------------------------------------------------------
C     IN  OPT  : OPTION NON LINEAIRE A CALCULER
C                  'RAPH_MECA'
C                  'FULL_MECA'      OU 'FULL_MECA_ELAS'
C                  'RIGI_MECA_TANG' OU 'RIGI_MECA_ELAS'
C     IN  XYZL : COORDONNEES DES NOEUDS
C     IN  UL   : DEPLACEMENT A L'INSTANT T "-"
C     IN  DUL  : INCREMENT DE DEPLACEMENT
C     IN  PGL  : MATRICE DE PASSAGE GLOBAL - LOCAL ELEMENT
C     OUT KTAN : MATRICE DE RIGIDITE TANGENTE
C                    SI 'FULL_MECA' OU 'RIGI_MECA_TANG'
C     OUT BTSIG: DIV (SIGMA)
C                    SI 'FULL_MECA' OU 'RAPH_MECA'
C     ------------------------------------------------------------------
C     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      REAL*8 R8VIDE
C
      CHARACTER*8   TYPMOD(2)
CCC      PARAMETER (NNO=3)  POUR LES DKT
CCC      PARAMETER (NNO=4)  POUR LES DKQ
      INTEGER    NNO
      PARAMETER (NNO=4)
C            NNO:    NOMBRE DE NOEUDS DE L'ELEMENT
      REAL*8   DISTN, ANGMAS(3),WK
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
C  CMPS DE DEFORMATION ET CONTRAINTE PLANE POUR APPEL NMCOMP :
C   -            EPSIXX,EPSIYY,EPSIZZ,SQRT(2)*EPSIXY
C   -            SIGMXX,SIGMYY,SIGMZZ,SQRT(2)*SIGMXY
C  CMPS DE DEFORMATION COQUE :
C   - MEMBRANE : EPSIXX,EPSIYY,2*EPSIXY
C   - FLEXION  : KHIXX,KHIYY,2*KHIXY
C  CMPS D' EFFORTS COQUE :
C   - MEMBRANE : NXX,NYY,NXY
C   - FLEXION  : MXX,MYY,MXY
C --------------------------------------------------------------------
      INTEGER NBCOU, NPGH,  JNBSPI
C            NBCOU:  NOMBRE DE COUCHES (INTEGRATION DE LA PLASTICITE)
C            NPG:    NOMBRE DE POINTS DE GAUSS PAR ELEMENT
C            NC :    NOMBRE DE COTES DE L'ELEMENT
C            NPGH:   NOMBRE DE POINT D'INTEGRATION PAR COUCHE
      REAL*8 POIDS, HIC, H, ZIC, ZMIN, INSTM, INSTP,
     &       COEF
C            POIDS:  POIDS DE GAUSS (Y COMPRIS LE JACOBIEN)
C            AIRE:   SURFACE DE L'ELEMENT
C            HIC:    EPAISSEUR D'UNE COUCHE
C            H :     EPAISSEUR TOTALE DE LA PLAQUE
C            ZIC:    COTE DU POINT D'INTEGRATION DANS L'EPAISSEUR
C            ZMIN:   COTE DU POINT D'INTEGRATION LE PLUS "BAS"
C            INSTM:  INSTANT "-"
C            INSTP:  INSTANT "+"
C            COEF:   POIDS D'INTEGRATION PAR COUCHE
      REAL*8 UM(2,NNO), UF(3,NNO), DUM(2,NNO), DUF(3,NNO)
C            UM:     DEPLACEMENT (MEMBRANE) "-"
C            UF:     DEPLACEMENT (FLEXION)  "-"
C           DUM:     INCREMENT DEPLACEMENT (MEMBRANE)
C           DUF:     INCREMENT DEPLACEMENT (FLEXION)
      REAL*8 EPS2D(6), DEPS2D(6), DSIDEP(6,6)
C            EPS2D:  DEFORMATION DANS UNE COUCHE (2D C_PLAN)
C           DEPS2D:  INCREMENT DEFORMATION DANS UNE COUCHE (2D C_PLAN)
C            SIG2D:  CONTRAINTE DANS UNE COUCHE (2D C_PLAN)
C           DSIDEP:  MATRICE D(SIG2D)/D(EPS2D)
      REAL*8 EPS(3), KHI(3), DEPS(3), DKHI(3), N(3), M(3),SIGM(4)
C            EPS:    DEFORMATION DE MEMBRANE "-"
C            DEPS:   INCREMENT DE DEFORMATION DE MEMBRANE
C            KHI:    DEFORMATION DE FLEXION  "-"
C            DKHI:   INCREMENT DE DEFORMATION DE FLEXION
C            N  :    EFFORT NORMAL "+"
C            M  :    MOMENT FLECHISSANT "+"
C            SIGM : CONTRAINTE "-"
      REAL*8 DF(9), DM(9), DMF(9), D2D(9)
C            D2D:    MATRICE DE RIGIDITE TANGENTE MATERIELLE (2D)
C            DF :    MATRICE DE RIGIDITE TANGENTE MATERIELLE (FLEXION)
C            DM :    MATRICE DE RIGIDITE TANGENTE MATERIELLE (MEMBRANE)
C            DMF:    MATRICE DE RIGIDITE TANGENTE MATERIELLE (COUPLAGE)
      REAL*8 BF(3,3*NNO), BM(3,2*NNO), BMQ(2,3)
C            BF :    MATRICE "B" (FLEXION)
C            BM :    MATRICE "B" (MEMBRANE)
      REAL*8 FLEX(3*NNO*3*NNO), MEMB(2*NNO*2*NNO)
      REAL*8 MEFL(2*NNO*3*NNO), WORK(3*NNO*3*NNO)
C           MEMB:    MATRICE DE RIGIDITE DE MEMBRANE
C           FLEX:    MATRICE DE RIGIDITE DE FLEXION
C           WORK:    TABLEAU DE TRAVAIL
C           MEFL:    MATRICE DE COUPLAGE MEMBRANE-FLEXION
C             LE MATERIAU EST SUPPOSE HOMOGENE
C             IL PEUT NEANMOINS Y AVOIR COUPLAGE PAR LA PLASTICITE
C     ------------------ PARAMETRAGE ELEMENT ---------------------------
      INTEGER   NDIM,NNOEL,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER   JTAB(7), COD, I, KSP
      INTEGER   ICACOQ, ICARCR, ICOMPO, ICONTM, ICONTP, ICOU, ICPG,
     &           IGAUH, IINSTM, IINSTP, IMATE, INO, IPG, IRET, ISP,
     &           IVARIM, IVARIP, IVARIX,
     &          IVPG, J, K, NBCON, NBSP, NBVAR, NDIMV
      REAL*8     DEUX, RAC2, QSI, ETA, CARA(25), JACOB(5)
      REAL*8    CTOR,COEHSD
      REAL*8    LC
      LOGICAL   VECTEU,MATRIC,DKT,DKQ,LEUL
C     ------------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNOEL,NNOS,NPG,IPOIDS,ICOOPG,
     &                                         IVF,IDFDX,IDFD2,JGANO)

      DEUX   = 2.D0
      RAC2   = SQRT(DEUX)
      CODRET = 0

C     2 BOOLEENS COMMODES :
C     ---------------------
      VECTEU = ((OPT(1:9).EQ.'FULL_MECA').OR.(OPT.EQ.'RAPH_MECA'))
      MATRIC = ((OPT(1:9).EQ.'FULL_MECA').OR.(OPT(1:9).EQ.'RIGI_MECA'))
C     RECUPERATION DES OBJETS &INEL ET DES CHAMPS PARAMETRES :
C     --------------------------------------------------------
      DKT    = .FALSE.
      DKQ    = .FALSE.

      IF (NOMTE.EQ.'MEDKTR3 ') THEN
        DKT = .TRUE.
      ELSEIF (NOMTE.EQ.'MEDKQU4 ') THEN
        DKQ = .TRUE.
      ELSE
        CALL U2MESK('F','ELEMENTS_34',1,NOMTE)
      END IF

      CALL JEVECH('PMATERC','L',IMATE)

      CALL TECACH('OON','PCONTMR',7,JTAB,IRET)
      NBSP=JTAB(7)
      ICONTM=JTAB(1)
      CALL ASSERT(NPG.EQ.JTAB(3))

      CALL JEVECH('PVARIMR','L',IVARIM)
      CALL JEVECH('PINSTMR','L',IINSTM)
      CALL JEVECH('PINSTPR','L',IINSTP)
      INSTM = ZR(IINSTM)
      INSTP = ZR(IINSTP)

      CALL JEVECH('PCOMPOR','L',ICOMPO)
      CALL JEVECH('PCARCRI','L',ICARCR)
      CALL JEVECH('PCACOQU','L',ICACOQ)

      LEUL = ZK16(ICOMPO+2).EQ.'GROT_GDEP'
      IF(LEUL .AND. ZK16(ICOMPO)(1:4).NE.'ELAS') THEN
       CALL U2MESS('F','ELEMENTS2_73')
      ENDIF

      IF (VECTEU) THEN
        CALL JEVECH('PCONTPR','E',ICONTP)
        CALL JEVECH('PVARIPR','E',IVARIP)
      ELSE
C       -- POUR AVOIR UN TABLEAU BIDON A DONNER A NMCOMP :
        IVARIP = IVARIM
        ICONTP = ICONTM
      END IF

C     -- GRANDEURS GEOMETRIQUES :
C     ---------------------------
      H = ZR(ICACOQ)
      DISTN = ZR(ICACOQ+4)
      IF ( DKT ) THEN
         CALL GTRIA3(XYZL,CARA)
         CTOR = ZR(ICACOQ+3)
      ELSEIF ( DKQ ) THEN
         CALL GQUAD4(XYZL,CARA)
         CTOR = ZR(ICACOQ+3)
      END IF
C
C     -- MISES A ZERO :
C     ------------------
      IF (MATRIC) THEN
        CALL R8INIR((3*NNOEL)* (3*NNOEL),0.D0,FLEX,1)
        CALL R8INIR((2*NNOEL)* (2*NNOEL),0.D0,MEMB,1)
        CALL R8INIR((2*NNOEL)* (3*NNOEL),0.D0,MEFL,1)
      END IF
      IF (VECTEU) THEN
        CALL R8INIR(6*NNOEL,0.D0,BTSIG,1)
      END IF

C     -- PARTITION DU DEPLACEMENT EN MEMBRANE/FLEXION :
C     -------------------------------------------------
      DO 30,INO = 1,NNOEL
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

C     -- INTEGRATIONS SUR LA SURFACE DE L'ELEMENT:
C     --------------------------------------------
C     -- POUR POUVOIR UTILISER NMCOMP
      TYPMOD(1) = 'C_PLAN  '
      TYPMOD(2) = '        '
      NPGH = 3

C     CONTRAINTE 2D : SIXX,SIYY,SIZZ,SQRT(2)*SIXY
      NBCON = 6
C     NBVAR: NOMBRE DE VARIABLES INTERNES (2D) LOI COMPORT
C     NBCOU : NOMBRE DE COUCHES
      READ (ZK16(ICOMPO-1+2),'(I16)') NBVAR
      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU=ZI(JNBSPI-1+1)
      CALL TECACH('OON','PVARIMR',7,JTAB,IRET)
      IF (NBCOU.LE.0) CALL U2MESK('F','ELEMENTS_36',1,ZK16(ICOMPO-1+6))

      HIC = H/NBCOU
      ZMIN = -H/DEUX + DISTN
      IF (VECTEU) THEN
        NDIMV=NPG*NBSP*NBVAR
        CALL JEVECH('PVARIMP','L',IVARIX)
        CALL DCOPY(NDIMV,ZR(IVARIX),1,ZR(IVARIP),1)
      END IF

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
        CALL PMRVEC('ZERO',3,2*NNOEL,BM, UM, EPS)
        CALL PMRVEC('ZERO',3,2*NNOEL,BM,DUM,DEPS)
        CALL PMRVEC('ZERO',3,3*NNOEL,BF, UF, KHI)
        CALL PMRVEC('ZERO',3,3*NNOEL,BF,DUF,DKHI)

C     -- EULER_ALMANSI - TERMES QUADRATIQUES
        IF(LEUL) THEN
          CALL R8INIR(6,0.D0,BMQ,1)
          DO 145,I = 1,2
            DO 146,K = 1,NNOEL
              DO 142,J = 1,2
                BMQ(I,J) = BMQ(I,J) + BM(I,2*(K-1)+I)*DUM(J,K)
 142          CONTINUE
              BMQ(I,3) = BMQ(I,3) + BM(I,2*(K-1)+I)*DUF(1,K)
 146        CONTINUE
 145      CONTINUE

          DO 150, K = 1,3
            DO 155, I = 1,2
              DEPS(I) = DEPS(I) - 0.5D0*BMQ(I,K)*BMQ(I,K)
 155        CONTINUE
            DEPS(3) = DEPS(3) - BMQ(1,K)*BMQ(2,K)
 150      CONTINUE
        ENDIF

C       -- CALCUL DE L'ECOULEMENT PLASTIQUE SUR CHAQUE COUCHE:
C          PAR INTEGRATION EN TROIS POINTS
C       ------------------------------------------------------
        DO 80,ICOU = 1,NBCOU
          DO 70,IGAUH = 1,NPGH
            KSP=(ICOU-1)*NPGH+IGAUH
            ISP=(ICOU-1)*NPGH+IGAUH
            IVPG = ((IPG-1)*NBSP + ISP-1)*NBVAR
            ICPG = ((IPG-1)*NBSP + ISP-1)*NBCON

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


C         -- APPEL A NMCOMP POUR RESOUDRE LE PB SUR LA COUCHE :
C         -----------------------------------------------------
            DO 1 J=1,4
               SIGM(J)=ZR(ICONTM+ICPG-1+J)
  1         CONTINUE
            SIGM(4)=SIGM(4)*RAC2
C --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C --- INITIALISE A R8VIDE (ON NE S'EN SERT PAS)
            CALL R8INIR(3,  R8VIDE(), ANGMAS ,1)
            CALL NMCOMP('RIGI',IPG,KSP,2,TYPMOD,ZI(IMATE),
     &                  ZK16(ICOMPO),ZR(ICARCR),INSTM,INSTP,
     &                  6,EPS2D,DEPS2D,
     &                  6,SIGM,ZR(IVARIM+IVPG),
     &                  OPT,ANGMAS,1,LC,
     &                  ZR(ICONTP+ICPG),ZR(IVARIP+IVPG),
     &                  36,DSIDEP,1,WK,COD)

C            DIVISION DE LA CONTRAINTE DE CISAILLEMENT PAR SQRT(2)
C            POUR STOCKER LA VALEUR REELLE

            ZR(ICONTP+ICPG+3)=ZR(ICONTP+ICPG+3)/RAC2

C           COD=1 : ECHEC INTEGRATION LOI DE COMPORTEMENT
C           COD=3 : C_PLAN DEBORST SIGZZ NON NUL
            IF (COD.NE.0) THEN
               IF (CODRET.NE.1) THEN
                  CODRET=COD
               ENDIF
            ENDIF

C         -- CALCUL DES EFFORTS RESULTANTS DANS L'EPAISSEUR (N ET M) :
C         ------------------------------------------------------------
            IF (VECTEU) THEN
              COEHSD = COEF*HIC/DEUX
              N(1) = N(1) + COEHSD*ZR(ICONTP+ICPG-1+1)
              N(2) = N(2) + COEHSD*ZR(ICONTP+ICPG-1+2)
              N(3) = N(3) + COEHSD*ZR(ICONTP+ICPG-1+4)
              M(1) = M(1) + COEHSD*ZIC*ZR(ICONTP+ICPG-1+1)
              M(2) = M(2) + COEHSD*ZIC*ZR(ICONTP+ICPG-1+2)
              M(3) = M(3) + COEHSD*ZIC*ZR(ICONTP+ICPG-1+4)
            END IF

C         -- CALCUL DES MATRICES TANGENTES MATERIELLES (DM,DF,DMF):
C         ---------------------------------------------------------
            IF (MATRIC) THEN
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
              DO 60,K = 1,9
                DM(K) =  DM(K) + COEF*HIC/DEUX*POIDS*D2D(K)
                DMF(K) = DMF(K) + COEF*HIC/DEUX*POIDS*ZIC*D2D(K)
                DF(K) =  DF(K) + COEF*HIC/DEUX*POIDS*ZIC*ZIC*D2D(K)
   60         CONTINUE
            END IF
   70     CONTINUE
   80   CONTINUE
C
C       -- CALCUL DE DIV(SIGMA) ET RECOPIE DE N ET M DANS 'PCONTPR':
C       ----------------------------------------------------------
C       BTSIG = BTSIG + BFT*M + BMT*N
        IF (VECTEU) THEN
            DO 120,INO = 1,NNOEL
            DO 110,K = 1,3
              BTSIG(1,INO) = BTSIG(1,INO) +
     &                       BM(K,2* (INO-1)+1)*N(K)*POIDS
              BTSIG(2,INO) = BTSIG(2,INO) +
     &                       BM(K,2* (INO-1)+2)*N(K)*POIDS
              BTSIG(3,INO) = BTSIG(3,INO) +
     &                       BF(K,3* (INO-1)+1)*M(K)*POIDS
              BTSIG(5,INO) = BTSIG(5,INO) +
     &                       BF(K,3* (INO-1)+2)*M(K)*POIDS
              BTSIG(4,INO) = BTSIG(4,INO) -
     &                       BF(K,3* (INO-1)+3)*M(K)*POIDS
  110       CONTINUE
  120       CONTINUE
        END IF


C       -- CALCUL DE LA MATRICE TANGENTE :
C       ----------------------------------
C       KTANG = KTANG + BFT*DF*BF + BMT*DM*BM + BMT*DMF*BF
        IF (MATRIC) THEN
C         -- MEMBRANE :
C         -------------
          CALL UTBTAB('CUMU',3,2*NNOEL,DM,BM,WORK,MEMB)

C         -- FLEXION :
C         ------------
          CALL UTBTAB('CUMU',3,3*NNOEL,DF,BF,WORK,FLEX)

C         -- COUPLAGE:
C         ------------
          CALL UTCTAB('CUMU',3,3*NNOEL,2*NNOEL,DMF,BF,BM,WORK,MEFL)
        END IF

C       -- FIN BOUCLE SUR LES POINTS DE GAUSS
  130 CONTINUE

C     -- ACCUMULATION DES SOUS MATRICES DANS KTAN :
C     -----------------------------------------------
      IF (MATRIC) THEN
        IF (DKT) THEN
          CALL DXTLOC(FLEX,MEMB,MEFL,CTOR,KTAN)
        ELSE
          CALL DXQLOC(FLEX,MEMB,MEFL,CTOR,KTAN)
        END IF
      END IF
      END
