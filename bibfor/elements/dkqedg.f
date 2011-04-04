      SUBROUTINE DKQEDG ( XYZL, OPTION, PGL, DEPL, EDGL )
      IMPLICIT  NONE
      REAL*8        XYZL(3,*),PGL(3,*), DEPL(*),EDGL(*)
      CHARACTER*16  OPTION
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
C     EFFORTS ET DEFORMATIONS GENERALISES DE L'ELEMENT DE PLAQUE DKQ
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  OPTION : NOM DE L'OPTION DE CALCUL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  DEPL   : DEPLACEMENTS
C     OUT EDGL   : EFFORTS OU DEFORMATIONS GENERALISES AUX NOEUDS DANS
C                  LE REPERE INTRINSEQUE A L'ELEMENT
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
      INTEGER  NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER MULTIC,NE,K,J,I,IE
      REAL*8 DEPF(12),DEPM(8)
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 HFT2(2,6)
      REAL*8 BF(3,12),BM(3,8)
      REAL*8 BDF(3),BDM(3),DCIS(2)
      REAL*8 VF(3),VM(3),VT(2),QSI,ETA,CARAQ4(25),JACOB(5)
      REAL*8 VFM(3),VMF(3),T2EV(4),T2VE(4),T1VE(9)
      LOGICAL COUPMF
      CHARACTER*4 FAMI
C     ------------------------------------------------------------------
C
      IF (OPTION(6:9).EQ.'ELGA') THEN
        CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
        NE  = NPG
        FAMI='RIGI'
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
        CALL ELREF5(' ','NOEU',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
        NE  = NNO
        FAMI='NOEU'
      END IF
C
C     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES ------------------------
C
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
      CALL GQUAD4 ( XYZL, CARAQ4 )
C     ----- CARACTERISTIQUES DES MATERIAUX --------
      CALL DXMATE(FAMI,DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     +                                         COUPMF,T2EV,T2VE,T1VE)
C     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
      DO 20 J = 1,4
        DO 10 I = 1,2
          DEPM(I+2* (J-1)) = DEPL(I+6* (J-1))
   10   CONTINUE
        DEPF(1+3* (J-1)) =  DEPL(1+2+6* (J-1))
        DEPF(2+3* (J-1)) =  DEPL(3+2+6* (J-1))
        DEPF(3+3* (J-1)) = -DEPL(2+2+6* (J-1))
   20 CONTINUE
C
      IF (OPTION(1:4).EQ.'DEGE') THEN
        DO 80 IE = 1,NE
          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4 ( XYZL, QSI, ETA, JACOB )
C           ------- CALCUL DU PRODUIT HF.T2 ---------------------------
          CALL DSXHFT ( DF, JACOB(2), HFT2 )
C           ----- CALCUL DES MATRICES BM ET BF AU POINT QSI ETA -------
          CALL DXQBM( QSI, ETA, JACOB(2), BM )
          CALL DKQBF( QSI, ETA, JACOB(2), CARAQ4, BF )
          DO 30 K = 1,3
            BDF(K) = 0.D0
            BDM(K) = 0.D0
   30     CONTINUE
          DO 60 I = 1,3
            DO 40 J = 1,12
              BDF(I) = BDF(I) + BF(I,J)*DEPF(J)
   40       CONTINUE
            DO 50 J = 1,8
              BDM(I) = BDM(I) + BM(I,J)*DEPM(J)
   50       CONTINUE
   60     CONTINUE
C           ------ VT = HFT2.TKQ.DEPF ---------------------------------
          CALL DKQTXY ( QSI, ETA, HFT2, DEPF, CARAQ4(13),
     +                                            CARAQ4(9), VT )
C           ------ DCIS = DCI.VT --------------------------------------
          DCIS(1) = DCI(1,1)*VT(1) + DCI(1,2)*VT(2)
          DCIS(2) = DCI(2,1)*VT(1) + DCI(2,2)*VT(2)
          DO 70 I = 1,3
            EDGL(I+8* (IE-1)) = BDM(I)
            EDGL(I+3+8* (IE-1)) = BDF(I)
   70     CONTINUE
C           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
          EDGL(3+8* (IE-1)) = EDGL(3+8* (IE-1))/2.D0
          EDGL(6+8* (IE-1)) = EDGL(6+8* (IE-1))/2.D0
          EDGL(7+8* (IE-1)) = DCIS(1)/2.D0
          EDGL(8+8* (IE-1)) = DCIS(2)/2.D0
   80   CONTINUE
      ELSE
        DO 160 IE = 1,NE
          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)
C           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
          CALL JQUAD4 ( XYZL, QSI, ETA, JACOB )
C           ------- CALCUL DU PRODUIT HF.T2 ----------------------------
          CALL DSXHFT ( DF, JACOB(2), HFT2 )
C           ----- CALCUL DES MATRICES BM ET BF AU POINT QSI ETA --------
          CALL DXQBM( QSI, ETA, JACOB(2), BM )
          CALL DKQBF( QSI, ETA, JACOB(2), CARAQ4, BF )
          DO 90 K = 1,3
            BDF(K) = 0.D0
            BDM(K) = 0.D0
            VF(K) = 0.D0
            VM(K) = 0.D0
            VFM(K) = 0.D0
            VMF(K) = 0.D0
   90     CONTINUE
C           ------ VF = DF.BF.DEPF , VFM = DMF.BM.DEPM ----------------
C           ------ VM = DM.BM.DEPM , VMF = DMF.BF.DEPF ----------------
          DO 120 I = 1,3
            DO 100 J = 1,12
              BDF(I) = BDF(I) + BF(I,J)*DEPF(J)
  100       CONTINUE
            DO 110 J = 1,8
              BDM(I) = BDM(I) + BM(I,J)*DEPM(J)
  110       CONTINUE
  120     CONTINUE
          DO 140 I = 1,3
            DO 130 J = 1,3
              VF(I) = VF(I) + DF(I,J)*BDF(J)
              VFM(I) = VFM(I) + DMF(I,J)*BDM(J)
              VM(I) = VM(I) + DM(I,J)*BDM(J)
              VMF(I) = VMF(I) + DMF(I,J)*BDF(J)
  130       CONTINUE
  140     CONTINUE
C           ------ VT = HFT2.TKQ.DEPF ---------------------------------
          CALL DKQTXY ( QSI, ETA, HFT2, DEPF, CARAQ4(13),
     +                                            CARAQ4(9), VT )
          DO 150 I = 1,3
            EDGL(I+8* (IE-1)) = VM(I) + VMF(I)
            EDGL(I+3+8* (IE-1)) = VF(I) + VFM(I)
  150     CONTINUE
          EDGL(7+8* (IE-1)) = VT(1)
          EDGL(8+8* (IE-1)) = VT(2)
  160   CONTINUE
      END IF
C
      END
