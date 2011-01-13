      SUBROUTINE DKTSIE ( FAMI, XYZL, OPTION, PGL, DEPL, CDL )
      IMPLICIT  NONE
      REAL*8        XYZL(3,*),PGL(3,*), DEPL(*), CDL(*)
      CHARACTER*16  OPTION
      CHARACTER*4   FAMI
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/01/2011   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C     ------------------------------------------------------------------
C      RELATION ELAS_COQUE
C     CONTRAINTES SIEF_ELGA DE L'ELEMENT DE PLAQUE DKT
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
C     IN  OPTION : NOM DE L'OPTION DE CALCUL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  DEPL   : DEPLACEMENTS
C     OUT CDL    : CONTRAINTES AUX POINTS DE GAUSS DANS LE REPERE LOCAL
C                  LE CALCUL EST FAIT SUR UNE SEULE COUCHE (ELAS_COQUE)
C                  SUR 3 NIVEAUX : 3 PTS D INTEGRATION DANS L EPAISSEUR
C                  CORRESPONDANT AUX NIVEAUX INF, MOY, SUP
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER     NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER     JCACO,I,J,K,IE,JMATE,ICPG,IG,IBID
      REAL*8      ZIC,EPAIS,EXCEN
      REAL*8      DEPF(9),DEPM(6),VT(2),LAMBDA(4)
      REAL*8      DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2)
      REAL*8      H(3,3),D1I(2,2),D2I(2,4),BF(3,9),BM(3,6)
      REAL*8      SM(3),SF(3),HFT2(2,6),HLT2(4,6)
      REAL*8      EPS(3),SIG(3),DCIS(2),CIST(2)
      REAL*8      QSI, ETA, CARAT3(21), T2EV(4), T2VE(4), T1VE(9)
C     ------------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
C
C     ----- RAPPEL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
      CALL GTRIA3 ( XYZL, CARAT3 )

C     ----- CARACTERISTIQUES DES MATERIAUX --------
      CALL DMATEL(FAMI,DF,DM,DMF,DC,DCI,NNO,PGL,IBID,1,T2EV,
     +                                                 T2VE,T1VE,NPG)

C     -------- CALCUL DE D1I ET D2I ------------------------------------
        CALL JEVECH('PCACOQU','L',JCACO)
        EPAIS = ZR(JCACO)
        EXCEN = ZR(JCACO+4)
        DO 10 K = 1,9
          H(K,1) = DM(K,1)/EPAIS
   10   CONTINUE
C     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
      DO 30 J = 1,3
        DO 20 I = 1,2
          DEPM(I+2* (J-1)) = DEPL(I+6* (J-1))
   20   CONTINUE
        DEPF(1+3* (J-1)) = DEPL(1+2+6* (J-1))
        DEPF(2+3* (J-1)) = DEPL(3+2+6* (J-1))
        DEPF(3+3* (J-1)) = -DEPL(2+2+6* (J-1))
   30 CONTINUE
C     ------ CALCUL DE LA MATRICE BM -----------------------------------
      CALL DXTBM ( CARAT3(9), BM )
C     ------ SM = BM.DEPM ----------------------------------------------
      DO 40 I = 1,3
        SM(I) = 0.D0
   40 CONTINUE
      DO 60 I = 1,3
        DO 50 J = 1,6
          SM(I) = SM(I) + BM(I,J)*DEPM(J)
   50   CONTINUE
   60 CONTINUE

C     ------- CALCUL DU PRODUIT HF.T2 ----------------------------------
      CALL DSXHFT ( DF, CARAT3(9), HFT2 )
C     ------ VT = HFT2.TKT.DEPF ---------------------------------------
      CALL DKTTXY ( CARAT3(16), CARAT3(13), HFT2, DEPF, VT )
C              ---------------------
C
C  BOUCLE SUR LES POINTS D INTEGRATION 
C
        DO 310 IE = 1,NPG
          QSI = ZR(ICOOPG-1+NDIM*(IE-1)+1)
          ETA = ZR(ICOOPG-1+NDIM*(IE-1)+2)
C           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
          CALL DKTBF ( QSI, ETA, CARAT3, BF )
          DO 312 I = 1,3
            SF(I) = 0.D0
 312      CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 314 I = 1,3
            DO 316 J = 1,9
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
 316        CONTINUE
 314     CONTINUE

C
C  BOUCLE SUR LES POINTS D INTEGRATION DANS L EPAISSEUR
C
            DO 320, IG = 1 , 3
              ICPG = 6*3*(IE-1) + 6*(IG-1)
C             -- COTE DES POINTS D'INTEGRATION
C             --------------------------------
              IF (IG.EQ.1) THEN
                ZIC = -EPAIS/2.D0  + EXCEN
              ELSE IF (IG.EQ.2) THEN
                ZIC = EXCEN
              ELSE
                ZIC = EPAIS/2.D0 + EXCEN
              END IF
C
              DO 332 I = 1,3
                EPS(I) = SM(I) + ZIC*SF(I)
                SIG(I) = 0.D0
 332          CONTINUE
              DO 334 I = 1,3
                DO 336 J = 1,3
                  SIG(I) = SIG(I) + H(I,J)*EPS(J)
 336            CONTINUE
 334          CONTINUE
              D1I(1,1) = 3.D0/ (2.D0*EPAIS) -
     +                   ZIC*ZIC*6.D0/ (EPAIS*EPAIS*EPAIS)
              D1I(2,2) = D1I(1,1)
              D1I(1,2) = 0.D0
              D1I(2,1) = 0.D0
C        ------ CIST = D1I.VT ---------
              CIST(1) = D1I(1,1)*VT(1) + D1I(1,2)*VT(2)
              CIST(2) = D1I(2,1)*VT(1) + D1I(2,2)*VT(2)
C
              CDL(ICPG+1) = SIG(1)
              CDL(ICPG+2) = SIG(2)
              CDL(ICPG+3) = 0.D0
              CDL(ICPG+4) = SIG(3)
              CDL(ICPG+5) = CIST(1)
              CDL(ICPG+6) = CIST(2)
 320        CONTINUE
 310      CONTINUE
C
      END
