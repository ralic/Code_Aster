      SUBROUTINE DKTNLI ( NOMTE, OPT, XYZL, UL, DUL, BTSIG, KTAN,
     &                    PGL, CODRET )
      IMPLICIT  NONE
      INTEGER         CODRET
      REAL*8          XYZL(3,*), UL(6,*), DUL(6,*), PGL(3,3)
      REAL*8          KTAN(*), BTSIG(6,*)
      CHARACTER*16    NOMTE, OPT

C MODIF ELEMENTS  DATE 25/04/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C     IN  GRILLE : .TRUE. => ELEMENT DE GRILLE (MEGRDKT)
C     OUT KTAN : MATRICE DE RIGIDITE TANGENTE
C                    SI 'FULL_MECA' OU 'RIGI_MECA_TANG'
C     OUT BTSIG: DIV (SIGMA)
C                    SI 'FULL_MECA' OU 'RAPH_MECA'
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR,R8VIDE
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*8   TYPMOD(2), NOMPU(2)
CCC      PARAMETER (NNO=3)  POUR LES DKT
CCC      PARAMETER (NNO=4)  POUR LES DKQ
      INTEGER    NNO
      PARAMETER (NNO=4)
C            NNO:    NOMBRE DE NOEUDS DE L'ELEMENT
      REAL*8   DISTN, POIDS2, ROT(9), DH(9), ANGMAS(3)
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
      INTEGER NBCOU, NC, NPGH, NZ, JNBSPI, ITABP(8), ITABM(8)
C            NBCOU:  NOMBRE DE COUCHES (INTEGRATION DE LA PLASTICITE)
C            NPG:    NOMBRE DE POINTS DE GAUSS PAR ELEMENT
C            NC :    NOMBRE DE COTES DE L'ELEMENT
C            NPGH:   NOMBRE DE POINT D'INTEGRATION PAR COUCHE
      REAL*8 POIDS, HIC, H, ZIC, ZMIN, INSTM, INSTP, VALPU(2),
     &       TMC, TMINF, TMMOY, TMSUP, TPC, TPINF, TPMOY, TPSUP, COEF
C            POIDS:  POIDS DE GAUSS (Y COMPRIS LE JACOBIEN)
C            AIRE:   SURFACE DE L'ELEMENT
C            HIC:    EPAISSEUR D'UNE COUCHE
C            H :     EPAISSEUR TOTALE DE LA PLAQUE
C            ZIC:    COTE DU POINT D'INTEGRATION DANS L'EPAISSEUR
C            ZMIN:   COTE DU POINT D'INTEGRATION LE PLUS "BAS"
C            INSTM:  INSTANT "-"
C            INSTP:  INSTANT "+"
C            TMC:    TEMPERATURE A L'INSTANT INSTM
C            TPC:    TEMPERATURE A L'INSTANT INSTP
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
      REAL*8 DF(9), DM(9), DMF(9), D2D(9), DM2(9)
C            D2D:    MATRICE DE RIGIDITE TANGENTE MATERIELLE (2D)
C            DF :    MATRICE DE RIGIDITE TANGENTE MATERIELLE (FLEXION)
C            DM :    MATRICE DE RIGIDITE TANGENTE MATERIELLE (MEMBRANE)
C            DMF:    MATRICE DE RIGIDITE TANGENTE MATERIELLE (COUPLAGE)
      REAL*8 BF(3,3*NNO), BM(3,2*NNO)
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
      INTEGER   JTAB(7), COD, I, IADZI, IAZK24, KSP
      INTEGER   ICACOQ, ICARCR, ICOMPO, ICONTM, ICONTP, ICOU, ICPG,
     &          IER, IGAUH, IINSTM, IINSTP, IMATE, INO, IPG, IRET, ISP,
     &          ITEMP, ITEMPM, ITEMPP, ITREF, IVARIM, IVARIP, IVARIX,
     &          IVPG, J, K, NBCON, NBSP, NBVAR, NDIMV
      REAL*8     DEUX, RAC2, QSI, ETA, CARA(25), JACOB(5)
      REAL*8    CTOR,EPSANP(4),EPSANM(4),PHASM(7),PHASP(7)
      REAL*8    SECHGM,SECHGP,SREF,LC,JACGAU,BMAT(6,18)
      REAL*8    CDF, CM1, CM2, CM3, CP1, CP2, CP3
      LOGICAL   VECTEU,MATRIC,TEMPNO,GRILLE,DKT,DKQ
      CHARACTER*24 NOMELE
C     ------------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNOEL,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
C
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
      GRILLE = .FALSE.
      IF (NOMTE(1:8).EQ.'MEGRDKT ') THEN
        GRILLE = .TRUE.
      ELSEIF (NOMTE(1:8).EQ.'MEDKTR3 ') THEN
        DKT = .TRUE.
      ELSEIF (NOMTE(1:8).EQ.'MEDKQU4 ') THEN
        DKQ = .TRUE.
      ELSE
        CALL UTMESS('F','DKTNLI','ELEMENT NON TRAITE '//NOMTE)
      END IF

      CALL JEVECH('PMATERC','L',IMATE)

      CALL TECACH('OON','PCONTMR',7,JTAB,IRET)
      NBSP=JTAB(7)
      ICONTM=JTAB(1)
      IF (NPG.NE.JTAB(3)) CALL UTMESS('F','DKTNLI','STOP')

      CALL JEVECH('PVARIMR','L',IVARIM)
      CALL JEVECH('PTEREF' ,'L',ITREF)
      CALL JEVECH('PINSTMR','L',IINSTM)
      CALL JEVECH('PINSTPR','L',IINSTP)
      INSTM = ZR(IINSTM)
      INSTP = ZR(IINSTP)

      CALL JEVECH('PCOMPOR','L',ICOMPO)
      CALL JEVECH('PCARCRI','L',ICARCR)
      CALL JEVECH('PCACOQU','L',ICACOQ)

      IF (VECTEU) THEN
        CALL JEVECH('PCONTPR','E',ICONTP)
        CALL JEVECH('PVARIPR','E',IVARIP)
      ELSE
C       -- POUR AVOIR UN TABLEAU BIDON A DONNER A NMCOMP :
        IVARIP = IVARIM
        ICONTP = ICONTM
      END IF

C       -- POUR AVOIR UN TABLEAU BIDON A DONNER A NMCOMP :
      DO 10 J = 1,4
        EPSANM(J) = 0.D0
        EPSANP(J) = 0.D0
   10 CONTINUE
      NZ=0
      DO 20 J = 1,7
        PHASM(J) = 0.D0
        PHASP(J) = 0.D0
   20 CONTINUE

C     -- GRANDEURS GEOMETRIQUES :
C     ---------------------------
      H = ZR(ICACOQ)
      DISTN = 0.D0
      IF (.NOT.GRILLE) THEN
         DISTN = ZR(ICACOQ+4)
         IF (DISTN.NE.0.D0) THEN
            CALL TECAEL(IADZI, IAZK24)
            NOMELE=ZK24(IAZK24-1+3)
            CALL UTMESS('F','DKTNLI',
     &    'PAS D EXCENTREMENT AVEC STAT_NON_LINE MAILLE '//NOMELE)
         ENDIF
      ENDIF
      IF ( GRILLE ) THEN
         DISTN = ZR(ICACOQ+3)
         CALL GTRIA3(XYZL,CARA)
         CTOR  = ZR(ICACOQ+4)
      ELSEIF ( DKT ) THEN
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
      IF ( GRILLE ) THEN
        TYPMOD(2) = 'MEGRDKT '
        NPGH = 1
        COEF = DEUX
      ELSE
        NPGH = 3
      END IF

C     CONTRAINTE 2D : SIXX,SIYY,SIZZ,SQRT(2)*SIXY
      NBCON = 6
C     NBVAR: NOMBRE DE VARIABLES INTERNES (2D) LOI COMPORT
C     NBCOU : NOMBRE DE COUCHES
      READ (ZK16(ICOMPO-1+2),'(I16)') NBVAR
      CALL JEVECH('PNBSP_I','L',JNBSPI)
      NBCOU=ZI(JNBSPI-1+1)
      CALL TECACH('OON','PVARIMR',7,JTAB,IRET)
      IF (NBCOU.LE.0) CALL UTMESS('F','DKTNLI','NOMBRE DE COUCHES '//
     &                         ' NEGATIF OU NUL : '//ZK16(ICOMPO-1+6))

      HIC = H/NBCOU
      IF (GRILLE) THEN
        ZMIN = -H/DEUX + HIC/DEUX + DISTN
      ELSE
        ZMIN = -H/DEUX + DISTN
      END IF
      IF (VECTEU) THEN
        NDIMV=NPG*NBSP*NBVAR
        CALL JEVECH('PVARIMP','L',IVARIX)
        CALL DCOPY(NDIMV,ZR(IVARIX),1,ZR(IVARIP),1)
      END IF

C===============================================================
C     -- RECUPERATION DE LA TEMPERATURE :
C     1- SI LA TEMPERATURE EST CONNUE AUX NOEUDS :
C        -----------------------------------------
      CALL TECACH ('ONN','PTEMPMR',8,ITABM,IRET)
      ITEMPM=ITABM(1)
      IF (IRET.EQ.0.OR.IRET.EQ.3) THEN
        TEMPNO = .TRUE.
        CALL TECACH ('OON','PTEMPPR',8,ITABP,IRET)
        ITEMPP=ITABP(1)
C       -- CALCUL DES TEMPERATURES INF, SUP ET MOY
C          (MOYENNE DES NNO NOEUDS) ET DES COEF. DES POLY. DE DEGRE 2 :
C          ------------------------------------------------------------
        TMINF = 0.D0
        TMMOY = 0.D0
        TMSUP = 0.D0
        TPINF = 0.D0
        TPMOY = 0.D0
        TPSUP = 0.D0
        DO 40,INO = 1,NNOEL
          CALL DXTPIF(ZR(ITEMPM+3*(INO-1)),ZL(ITABM(8)+3*(INO-1)))
          TMMOY = TMMOY + ZR(ITEMPM-1+3*(INO-1)+1)/DBLE(NNOEL)
          TMINF = TMINF + ZR(ITEMPM-1+3*(INO-1)+2)/DBLE(NNOEL)
          TMSUP = TMSUP + ZR(ITEMPM-1+3*(INO-1)+3)/DBLE(NNOEL)

          CALL DXTPIF(ZR(ITEMPP+3*(INO-1)),ZL(ITABP(8)+3*(INO-1)))
          TPMOY = TPMOY + ZR(ITEMPP-1+3*(INO-1)+1)/DBLE(NNOEL)
          TPINF = TPINF + ZR(ITEMPP-1+3*(INO-1)+2)/DBLE(NNOEL)
          TPSUP = TPSUP + ZR(ITEMPP-1+3*(INO-1)+3)/DBLE(NNOEL)
   40   CONTINUE
        CM1 = TMMOY
        CM2 = (TMSUP-TMINF)/H
        CM3 = DEUX* (TMINF+TMSUP-DEUX*TMMOY)/ (H*H)
        CP1 = TPMOY
        CP2 = (TPSUP-TPINF)/H
        CP3 = DEUX* (TPINF+TPSUP-DEUX*TPMOY)/ (H*H)
      ELSE
C     2- SI LA TEMPERATURE EST UNE FONCTION DE 'INST' ET 'EPAIS'
C        -------------------------------------------------------
        CALL TECACH('ONN','PTEMPEF',1,ITEMP,IRET)
        IF (IRET.EQ.0) THEN
          TEMPNO = .FALSE.
          NOMPU(1) = 'INST'
          NOMPU(2) = 'EPAIS'
        ELSE
          CALL UTMESS('F','DKTNLI','TEMPERATURE NON TROUVEE.')
        END IF
      END IF

C ---  VARIABLE D HYDRATATION ET DE SECHAGE
      SECHGM = 0.D0
      SECHGP = 0.D0
      SREF   = 0.D0

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
              IF (GRILLE) THEN
                ZIC = ZMIN + (ICOU-1)*HIC
              ELSE
                ZIC = ZMIN + (ICOU-1)*HIC
                COEF = 1.D0/3.D0
              END IF
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

C         -- CALCUL DES TEMPERATURES TPC ET TMC SUR LA COUCHE :
C         ---------------------------------------------------
            IF (TEMPNO) THEN
              TMC = CM3*ZIC*ZIC + CM2*ZIC + CM1
              TPC = CP3*ZIC*ZIC + CP2*ZIC + CP1
            ELSE
              VALPU(2) = ZIC
              VALPU(1) = INSTM
              CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TMC,IER)
              VALPU(1) = INSTP
              CALL FOINTE('FM',ZK8(ITEMP),2,NOMPU,VALPU,TPC,IER)
            END IF

C         -- APPEL A NMCOMP POUR RESOUDRE LE PB SUR LA COUCHE :
C         -----------------------------------------------------
            IF ( GRILLE ) THEN
C --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C --- INITIALISE A R8VIDE (ON NE S'EN SERT PAS)
               CALL R8INIR(3,  R8VIDE(), ANGMAS ,1)
               CALL NMGRIL('RIGI',IPG,KSP,ZI(IMATE),TYPMOD,
     &                     ZK16(ICOMPO),OPT,EPS2D,DEPS2D,
     &                     ANGMAS,
     &                     ZR(ICONTM+ICPG),ZR(IVARIM+IVPG),
     &                     TMC,TPC,ZR(ITREF),
     &               ZR(ICONTP+ICPG),ZR(IVARIP+IVPG),DSIDEP,COD)
            ELSE
               DO 1 J=1,4
               SIGM(J)=ZR(ICONTM+ICPG-1+J)
  1            CONTINUE
               SIGM(4)=SIGM(4)*RAC2
C --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C --- INITIALISE A R8VIDE (ON NE S'EN SERT PAS)
               CALL R8INIR(3,  R8VIDE(), ANGMAS ,1)
               CALL NMCOMP('RIGI',IPG,KSP,2,TYPMOD,ZI(IMATE),
     &                ZK16(ICOMPO),ZR(ICARCR),ZR(IINSTM),ZR(IINSTP),
     &                  TMC,TPC,ZR(ITREF),
     &                  SECHGM,SECHGP,SREF,
     &                  EPS2D,DEPS2D,
     &                  SIGM,ZR(IVARIM+IVPG),
     &                  OPT,
     &                  EPSANM,EPSANP,
     &                  NZ,PHASM,PHASP,
     &                  ANGMAS,
     &                  LC,
     &                  ZR(ICONTP+ICPG),ZR(IVARIP+IVPG),DSIDEP,COD)
            ENDIF

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
              N(1) = N(1) + COEF*HIC/DEUX*ZR(ICONTP+ICPG-1+1)
              N(2) = N(2) + COEF*HIC/DEUX*ZR(ICONTP+ICPG-1+2)
              N(3) = N(3) + COEF*HIC/DEUX*ZR(ICONTP+ICPG-1+4)
            IF (GRILLE) THEN
C             LES SEULS MOMENTS SONT DUS A L'EXCENTREMENT
C             PAS DE RIGIDITE DE FLEXION PROPRE
              M(1) = M(1) + ZIC*HIC*ZR(ICONTP+ICPG-1+1)
              M(2) = M(2) + ZIC*HIC*ZR(ICONTP+ICPG-1+2)
              M(3) = M(3) + ZIC*HIC*ZR(ICONTP+ICPG-1+4)
            ELSE
              M(1) = M(1) + COEF*HIC/DEUX*ZIC*ZR(ICONTP+ICPG-1+1)
              M(2) = M(2) + COEF*HIC/DEUX*ZIC*ZR(ICONTP+ICPG-1+2)
              M(3) = M(3) + COEF*HIC/DEUX*ZIC*ZR(ICONTP+ICPG-1+4)
            END IF
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
              IF (GRILLE) THEN
                DO 50,K = 1,9
                  DM(K) = DM(K) + HIC*POIDS*D2D(K)
C                 SANS EXCENTREMENT, PAS DE FLEXION PROPRE
                  DF(K) = 0.D0
   50           CONTINUE
              ELSE
                DO 60,K = 1,9
                   DM(K) =  DM(K) + COEF*HIC/DEUX*POIDS*D2D(K)
                  DMF(K) = DMF(K) + COEF*HIC/DEUX*POIDS*ZIC*D2D(K)
                   DF(K) =  DF(K) + COEF*HIC/DEUX*POIDS*ZIC*ZIC*D2D(K)
   60           CONTINUE
              END IF
            END IF
   70     CONTINUE
   80   CONTINUE

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
          IF (GRILLE) THEN
            POIDS2 = DISTN*DISTN
            CALL DCOPY(9,DM,1,DM2,1)
            CALL DSCAL(9,POIDS2,DM2,1)
            CALL UTBTAB('CUMU',3,3*NNOEL,DM2,BF,WORK,FLEX)
            CALL DCOPY(9,DM,1,DMF,1)
            CALL DSCAL(9,DISTN,DMF,1)
          END IF
          CALL UTCTAB('CUMU',3,3*NNOEL,2*NNOEL,DMF,BF,BM,WORK,MEFL)
        END IF

C       -- FIN BOUCLE SUR LES POINTS DE GAUSS
  130 CONTINUE

C     -- ACCUMULATION DES SOUS MATRICES DANS KTAN :
C     -----------------------------------------------
      IF (MATRIC) THEN
        IF (DKT .OR. GRILLE) THEN
          CALL DXTLOC(FLEX,MEMB,MEFL,CTOR,KTAN)
        ELSE
          CALL DXQLOC(FLEX,MEMB,MEFL,CTOR,KTAN)
        END IF
      END IF
  140 CONTINUE
      END
