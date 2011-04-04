      SUBROUTINE DSQMAS ( XYZL, OPTION, PGL, MAS, ENER )
      IMPLICIT NONE
      REAL*8         XYZL(3,*), PGL(*), MAS(*), ENER(*)
      CHARACTER*16   OPTION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2011   AUTEUR DESOZA T.DESOZA 
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
C     ------------------------------------------------------------------
C     MATRICE MASSE DE L'ELEMENT DE PLAQUE DSQ
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
C     OUT MAS    : MATRICE DE RIGIDITE
C     OUT ENER   : TERMES POUR ENER_CIN (ECIN_ELEM)
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
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
      INTEGER   NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER    I, J, K, I1, I2, INT, II(8), JJ(8), LL(16)
      INTEGER    MULTIC, P, JDEPG, JCOQU, J1, J2
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8     HFT2(2,6), HMFT2(2,6), FLEX(12,12)
      REAL*8     BCB(2,12),BCA(2,4),AN(4,12),AM(4,8),BCM(2,8)
      REAL*8     NFX(12), NFY(12), NMX(8), NMY(8), NMI(4)
      REAL*8     MEMB(8,8), MEFL(8,12), AMEMB(64)
      REAL*8     WSQ(12), WMESQ(8), DEPL(24)
      REAL*8     MASLOC(300), MASGLO(300), ROF, WGTMF
      REAL*8     ZERO, UNQUAR, UNDEMI, UN, NEUF, DOUZE, EXCENT, XINERT
      REAL*8     R8GAEM, COEFM, WGTF, WGTM, DETJ, WGT, ROE,RHO,EPAIS
      REAL*8     QSI,ETA,JACOB(5),CARAQ4(25),T2EV(4),T2VE(4),T1VE(9)
      LOGICAL    COUPMF, EXCE, INER
C     ------------------------------------------------------------------
      REAL*8 CTOR
      DATA (II(K),K=1,8)/1,10,19,28,37,46,55,64/
      DATA (JJ(K),K=1,8)/5,14,23,32,33,42,51,60/
      DATA (LL(K),K=1,16)/3,7,12,16,17,21,26,30,35,39,44,48,49,53,58,62/
C     ------------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
C
      ZERO   = 0.0D0
      UNQUAR = 0.25D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      NEUF   = 9.0D0
      DOUZE = 12.0D0
C
      EXCENT = ZERO

      CALL DXROEP(RHO,EPAIS)
      ROE = RHO*EPAIS
      ROF = RHO*EPAIS*EPAIS*EPAIS/DOUZE

      CALL JEVECH('PCACOQU','L',JCOQU)
      CTOR   = ZR(JCOQU+3)
      EXCENT = ZR(JCOQU+4)
      XINERT = ZR(JCOQU+5)

      EXCE = .FALSE.
      INER = .FALSE.
      IF (ABS(EXCENT).GT.UN/R8GAEM()) EXCE = .TRUE.
      IF (ABS(XINERT).GT.UN/R8GAEM()) INER = .TRUE.
      IF ( .NOT. INER )  ROF = 0.0D0
C
C --- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE :
C     ---------------------------------------------------
      CALL GQUAD4 ( XYZL, CARAQ4 )
C
C --- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C --- MEMBRANE ET CISAILLEMENT INVERSEE :
C     ---------------------------------
      CALL DXMATE('RIGI',DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     +                               COUPMF,T2EV,T2VE,T1VE)
C
C --- INITIALISATIONS :
C     ---------------
      CALL R8INIR(144,ZERO,FLEX,1)
      CALL R8INIR( 96,ZERO,MEFL,1)
      CALL R8INIR( 32,ZERO,AM  ,1)
C
C --- CAS AVEC EXCENTREMENT
C     ---------------------
C --- CALCUL DES MATRICES AN ET AM QUI SONT TELLES QUE
C --- ALPHA = AN*UN + AM*UM
C ---  UN DESIGNANT LES DEPLACEMENTS DE FLEXION  UN = (W,BETA_X,BETA_Y)
C ---  UM DESIGNANT LES DEPLACEMENTS DE MEMBRANE UM = (U,V)
C
C --- CAS SANS EXCENTREMENT
C     ---------------------
C --- ON CALCULE SEULEMENT AN QUI EST TEL QUE ALPHA = AN*UN :
C      ----------------------------------------------------
      IF (EXCE) THEN
        CALL DSQDI2(XYZL,DF,DCI,DMF,DFC,DMC,AN,AM)
      ELSE
        CALL DSQDIS(XYZL,CARAQ4,DF,DCI,AN)
      ENDIF
C
C===========================================================
C ---  CALCUL DE LA PARTIE MEMBRANE DE LA MATRICE DE MASSE =
C===========================================================
C
C --- PRISE EN COMPTE DES TERMES DE MEMBRANE CLASSIQUES
C --- EN U*U ET V*V :
C     -------------
      COEFM = CARAQ4(21)*ROE/NEUF
      DO 30 K = 1,64
        AMEMB(K) = ZERO
   30 CONTINUE
      DO 40 K = 1,8
        AMEMB(II(K)) = UN
        AMEMB(JJ(K)) = UNQUAR
   40 CONTINUE
      DO 50 K = 1,16
        AMEMB(LL(K)) = UNDEMI
   50 CONTINUE
      DO 60 K = 1,64
        MEMB(K,1) = COEFM*AMEMB(K)
   60 CONTINUE
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     ===================================
      DO 70 INT = 1,NPG
        QSI = ZR(ICOOPG-1+NDIM*(INT-1)+1)
        ETA = ZR(ICOOPG-1+NDIM*(INT-1)+2)
C
C ---   CALCUL DU JACOBIEN SUR LE QUADRANGLE :
C       ------------------------------------
        CALL JQUAD4 ( XYZL, QSI, ETA, JACOB )
C
C ---   CALCUL DU PRODUIT HF.T2 :
C       -----------------------
        CALL DSXHFT ( DF, JACOB(2), HFT2 )
C
C ---   CALCUL DU PRODUIT HMF.T2 :
C       ------------------------
        CALL DXHMFT ( DMF, JACOB(2), HMFT2 )
C
C ---   CALCUL DES MATRICES BCB, BCA ET BCM :
C       -----------------------------------
        CALL DSQCIS ( QSI, ETA, CARAQ4, HMFT2, HFT2, BCM, BCB, BCA )
C
C ---   CALCUL DES FONCTIONS D'INTERPOLATION DE LA FLECHE :
C       -------------------------------------------------
        CALL DSQNIW(QSI,ETA,CARAQ4,DCI,BCM,BCB,BCA,AN,AM,WSQ,WMESQ)
C
C ---   CALCUL DES FONCTIONS D'INTERPOLATION DES ROTATIONS :
C       --------------------------------------------------
        CALL DSQNIB(QSI,ETA,CARAQ4,AN,AM,NFX,NFY,NMX,NMY)
C
C ---   CALCUL DES FONCTIONS DE FORME DE MEMBRANE :
C       -----------------------------------------
        CALL DXQNIM ( QSI, ETA, NMI )
C
C==========================================================
C ---  CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE =
C==========================================================
C
        DETJ = JACOB(1)
C
C ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION W
C ---   EST EGALE A RHO_F = RHO*EPAIS :
C       -----------------------------
        WGT = ZR(IPOIDS+INT-1)*DETJ*ROE
C
C ---   CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE
C ---   DUE AUX SEULS TERMES DE LA FLECHE W :
C       -----------------------------------
        DO 80 I = 1,12
          DO 90 J = 1,12
            FLEX(I,J) = FLEX(I,J) + WSQ(I)*WSQ(J)*WGT
   90     CONTINUE
   80   CONTINUE
C
C ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION BETA
C ---   EST EGALE A RHO_F = RHO*EPAIS**3/12 + D**2*EPAIS*RHO :
C       ----------------------------------------------------
          WGTF = ZR(IPOIDS+INT-1)*DETJ*(ROF+EXCENT*EXCENT*ROE)
C
C ---   PRISE EN COMPTE DES TERMES DE FLEXION DUS AUX ROTATIONS :
C       -------------------------------------------------------
          DO 100 I = 1, 12
            DO 110 J = 1, 12
              FLEX(I,J) = FLEX(I,J)+(NFX(I)*NFX(J)+NFY(I)*NFY(J))*WGTF
  110     CONTINUE
  100   CONTINUE
C==============================================================
C ---   CAS D'UN ELEMENT EXCENTRE : IL APPARAIT DE TERMES DE  =
C ---   COUPLAGE MEMBRANE-FLEXION ET DE NOUVEAUX TERMES POUR  =
C ---   PARTIE MEMBRANE DE LA MATRICE DE MASSE :              =
C==============================================================
C
        IF (EXCE) THEN
C
C===================================================================
C ---  CALCUL DE LA PARTIE MEMBRANE-FLEXION DE LA MATRICE DE MASSE =
C===================================================================
C
C ---     POUR LE COUPLAGE MEMBRANE-FLEXION, ON DOIT TENIR COMPTE
C ---     DE 3 MASSES VOLUMIQUES
C ---     RHO_M  = EPAIS*RHO
C ---     RHO_MF = D*EPAIS*RHO
C ---     RHO_F  = RHO*EPAIS**3/12 + D**2*EPAIS*RHO
C         -------------------------------------------------
          WGTM  = ZR(IPOIDS+INT-1)*DETJ*ROE
          WGTMF = ZR(IPOIDS+INT-1)*DETJ*EXCENT*ROE
          WGTF  = ZR(IPOIDS+INT-1)*DETJ*(ROF+EXCENT*EXCENT*ROE)
C
C ---     PRISE EN COMPTE DES TERMES DE COUPLAGE MEMBRANE-FLEXION
C ---     ON A 3 TYPES DE TERMES DONT IL FAUT TENIR COMPTE
C ---     1) LES TERMES U*BETA     -> NMI*NFX ET NMI*NFY (RHO_MF)
C ---     2) LES TERMES W*W        -> WMESQ*WST          (RHO_M)
C ---     3) LES TERMES BETA*BETA  -> NMX*NFX            (RHO_F)
C         ------------------------------------------------------
C
C ---      1) TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA :
C             ------------------------------------------
          DO 120 K = 1, 4
            I1 = 2*(K-1)+1
            I2 = I1     +1
            DO 130 J = 1, 12
                MEFL(I1,J) = MEFL(I1,J)+NMI(K)*NFX(J)*WGTMF
                MEFL(I2,J) = MEFL(I2,J)+NMI(K)*NFY(J)*WGTMF
  130       CONTINUE
  120     CONTINUE
C ---      2) TERMES DE COUPLAGE MEMBRANE-FLEXION W*W ET BETA*BETA :
C             ----------------------------------------------------
            DO 140 I = 1, 8
            DO 150 J = 1, 12
                MEFL(I,J) = MEFL(I,J) + WMESQ(I)*WSQ(J)*WGTM
     +                + (NMX(I)*NFX(J) + NMY(I)*NFY(J))*WGTF
  150       CONTINUE
  140     CONTINUE
C
C===========================================================
C ---  AJOUT DE NOUVEAUX TERMES A LA PARTIE MEMBRANE       =
C ---  DE LA MATRICE DE MASSE DUS A L'EXCENTREMENT         =
C===========================================================
C
C ---     PRISE EN COMPTE DES TERMES DE MEMBRANE
C ---     ON A 3 TYPES DE TERMES DONT IL FAUT TENIR COMPTE
C ---     1) LES TERMES U*BETA     -> NMI*NMX ET NMI*NMY (RHO_MF)
C ---     2) LES TERMES W*W        -> WMESQ*WMESQ        (RHO_M)
C ---     3) LES TERMES BETA*BETA  -> NMX*NMX            (RHO_F)
C         ------------------------------------------------------
C
C ---      1) TERMES DE MEMBRANE U*BETA :
C             -------------------------
          DO 160 K = 1, 4
            I1 = 2*(K-1)+1
            I2 = I1     +1
            DO 170 P = 1, 4
              J1 = 2*(P-1)+1
              J2 = J1     +1
              MEMB(I1,J1) = MEMB(I1,J1)+
     +                      (NMI(K)*NMX(J1)+NMI(P)*NMX(I1))*WGTMF
              MEMB(I1,J2) = MEMB(I1,J2)+
     +                      (NMI(K)*NMX(J2)+NMI(P)*NMY(I1))*WGTMF
              MEMB(I2,J1) = MEMB(I2,J1)+
     +                      (NMI(K)*NMY(J1)+NMI(P)*NMX(I2))*WGTMF
              MEMB(I2,J2) = MEMB(I2,J2)+
     +                      (NMI(K)*NMY(J2)+NMI(P)*NMY(I2))*WGTMF
  170       CONTINUE
  160     CONTINUE
C ---      2) TERMES DE MEMBRANE WMESQ*WMESQ ET BETA*BETA :
C           -------------------------------------------
          DO 180 I = 1, 8
            DO 190 J = 1, 8
                MEMB(I,J) = MEMB(I,J) + WMESQ(I)*WMESQ(J)*WGTM
     +                + (NMX(I)*NMX(J) + NMY(I)*NMY(J))*WGTF
  190       CONTINUE
  180     CONTINUE
C
        ENDIF
C ---   FIN DU TRAITEMENT DU CAS D'UN ELEMENT EXCENTRE
C       ----------------------------------------------
   70 CONTINUE
C --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
C     ---------------------------------------------
C
C --- INSERTION DES DIFFERENTES PARTIES CALCULEES DE LA MATRICE
C --- DE MASSE A LA MATRICE ELLE MEME :
C     ===============================
      IF (( OPTION .EQ. 'MASS_MECA' ).OR.(OPTION.EQ.'M_GAMMA')) THEN
        CALL DXQLOC(FLEX,MEMB,MEFL,CTOR,MAS)
C
      ELSE IF (OPTION.EQ.'MASS_MECA_DIAG' .OR.
     &         OPTION.EQ.'MASS_MECA_EXPLI' ) THEN
        CALL DXQLOC(FLEX,MEMB,MEFL,CTOR,MASLOC)
        WGT = CARAQ4(21)*ROE
        CALL UTPSLG(4,6,PGL,MASLOC,MASGLO)
        CALL DIALUM(4,6,24,WGT,MASGLO,MAS)
C
      ELSE IF (OPTION.EQ.'ECIN_ELEM') THEN
        CALL JEVECH('PDEPLAR','L',JDEPG)
        CALL UTPVGL(4,6,PGL,ZR(JDEPG),DEPL)
        CALL DXQLOE(FLEX,MEMB,MEFL,CTOR,.FALSE.,DEPL,ENER)
      END IF
C
      END
