      SUBROUTINE DSQRIG ( NOMTE, XYZL, OPTION, PGL, RIG, ENER )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      REAL*8        XYZL(3,*), PGL(*), RIG(*), ENER(*)
      CHARACTER*16  OPTION , NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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
C     MATRICE DE RIGIDITE DE L'ELEMENT DE PLAQUE DSQ (AVEC CISAILLEMENT)
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
C     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
C     OUT RIG    : MATRICE DE RIGIDITE
C     OUT ENER   : TERMES POUR ENER_POT (EPOT_ELEM)
C     ------------------------------------------------------------------
      INTEGER I, INT, J, JCOQU, JDEPG, K, MULTIC
      REAL*8 WGT,DEPL(24)
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 BFB(3,12),BFA(3,4),HFT2(2,6),HMFT2(2,6)
      REAL*8 BCB(2,12),BCA(2,4),BCM(2,8),PB(4,12),PM(4,8)
      REAL*8 BM(3,8)
      REAL*8 XAB1(3,12),XAB2(3,4),XAB3(2,12),XAB4(2,4)
      REAL*8 XAB5(4,12),XAB6(3,8),XAB7(3,2)
      REAL*8 XAB8(12,2),XAB9(4,2),XAB10(8,2),XAB11(4,8),XAB12(2,8)
      REAL*8 KBA(12,4),KF12(12,4),KFC12(12,4)
      REAL*8 KMF11(8,12),KMF12(8,4),KMF(8,12)
C                   -----(12,12) -----(4,4)
      REAL*8 KF11(12,12),KF22(4,4)
      REAL*8 KBB(12,12),KAA(4,4)
      REAL*8 KFC11(12,12),KFC22(4,4)
C                   -----(12,12) -----(12,12)
      REAL*8 KFC(12,12),KFB(12,12)
C                   -----(12,12) ----(12,12)
      REAL*8 FLEXI(12,12),FLEX(12,12)
C                   -----(8,8)   -----(8,8)
      REAL*8 MEMBI(8,8),MEMB(8,8),MEMEXC(8,8)
C                   -----(8,12)  -----(8,12)
      REAL*8 MEFLI(8,12),MEFL(8,12)
      REAL*8 KFCG11(12,4), KFC21(4,4), KMC(8,4), KMAPB(8,12)
      REAL*8 BCMBCB(8,12), KMA(8,4), KMB(8,12)
      REAL*8 KMPMT(8,8), KMPM(8,8), MEMBCF(8,8), BCAPM(2,8), R8GAEM
      REAL*8 BSIGTH(24), ENERTH, CTOR, UN, ZERO, ETA, EXCENT, QSI
      REAL*8 JACOB(5),CARAQ4(25),T2EV(4),T2VE(4),T1VE(9)
      LOGICAL COUPMF, EXCE, INDITH
      INTEGER   NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
C     ------------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
C
      ZERO   = 0.0D0
      UN     = 1.0D0
      ENERTH = ZERO
C
      CALL R8INIR(48,ZERO,KFCG11,1)
      CALL R8INIR(16,ZERO,KFC21,1)
      CALL R8INIR(32,ZERO,KMC  ,1)
      CALL R8INIR(32,ZERO,PM  ,1)
      CALL R8INIR(64,ZERO,MEMEXC,1)
      CALL R8INIR(32,ZERO,KMF12,1)
      CALL R8INIR(16,ZERO,BCM,1)
      CALL R8INIR(32,ZERO,KMA,1)
      CALL R8INIR(64,ZERO,MEMBCF,1)
      CALL R8INIR(96,ZERO,BCMBCB,1)
      CALL R8INIR(96,ZERO,KMF11,1)
      CALL R8INIR(64,ZERO,MEMB,1)
      CALL R8INIR(144,ZERO,FLEX,1)
      CALL R8INIR(96,ZERO,KMB,1)
      CALL R8INIR(96,ZERO,MEFL,1)
      CALL R8INIR(64,ZERO,KMPMT,1)
      CALL R8INIR(64,ZERO,KMPM,1)
      CALL R8INIR(96,ZERO,KMAPB,1)
      CALL R8INIR(16,ZERO,BCAPM,1)
C
      CALL JEVECH('PCACOQU','L',JCOQU)
      CTOR   = ZR(JCOQU+3)
      EXCENT = ZR(JCOQU+4)
C
      EXCE = .FALSE.
      IF (ABS(EXCENT).GT.UN/R8GAEM()) EXCE = .TRUE.
C
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
      CALL GQUAD4 ( XYZL, CARAQ4 )

C     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEE --------------------------
      CALL DXMATE('RIGI',DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     +                                    COUPMF,T2EV,T2VE,T1VE)

C     ---- CALCUL DE LA MATRICE PB -------------------------------------
      IF (EXCE) THEN
        CALL DSQDI2(XYZL,DF,DCI,DMF,DFC,DMC,PB,PM)
      ELSE
        CALL DSQDIS(XYZL,CARAQ4,DF,DCI,PB)
      ENDIF
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
      DO 10 INT = 1,NPG
C
C============================================================
C --- CALCUL DE LA MATRICE DE RIGIDITE DE L'ELEMENT POUR    =
C --- LA FLEXION ET LE CISAILLEMENT                         =
C============================================================
C
C ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
C       ------------------------------------------
        QSI = ZR(ICOOPG-1+NDIM*(INT-1)+1)
        ETA = ZR(ICOOPG-1+NDIM*(INT-1)+2)
C
C ---   CALCUL DU JACOBIEN SUR LE QUADRANGLE :
C       ------------------------------------
        CALL JQUAD4 ( XYZL, QSI, ETA, JACOB )
C
C ---   CALCUL DE LA MATRICE BM :
C       -----------------------
        CALL DXQBM ( QSI, ETA, JACOB(2), BM )
C
C ---   CALCUL DE LA MATRICE BFB  :
C       ------------------------
        CALL DSQBFB ( QSI, ETA, JACOB(2), BFB )
C
C ---   CALCUL DE LA MATRICE BFA  :
C       ------------------------
        CALL DSQBFA ( QSI, ETA , JACOB(2) , CARAQ4 , BFA )
C
C ---   CALCUL DU PRODUIT BFBT.DF.BFB :
C       -----------------------------
        CALL UTBTAB('ZERO',3,12,DF,BFB,XAB1,KF11)
C
C ---   CALCUL DU PRODUIT BFAT.DF.BFA :
C       -----------------------------
        CALL UTBTAB('ZERO',3,4,DF,BFA,XAB2,KF22)
C
C ---   CALCUL DU PRODUIT BFBT.DF.BFA :
C       -----------------------------
        CALL UTCTAB('ZERO',3,4,12,DF,BFA,BFB,XAB2,KF12)
C
C ---   CALCUL DU PRODUIT HF.T2 :
C       -----------------------
        CALL DSXHFT ( DF, JACOB(2), HFT2 )
C
C ---   CALCUL DU PRODUIT HMF.T2 :
C       ------------------------
        CALL DXHMFT ( DMF, JACOB(2), HMFT2 )
C
C ---   CALCUL DES MATRICES BCB, BCA ET BCM:
C       -----------------------------------
        CALL DSQCIS ( QSI, ETA, CARAQ4, HMFT2, HFT2, BCM, BCB, BCA )
C
C ---   CALCUL DES MATRICES BCBT.DCI.BCB  :
C       --------------------------------
        CALL UTBTAB('ZERO',2,12,DCI,BCB,XAB3,KBB)
C
C ---   CALCUL DU PRODUIT BCAT.DCI.BCA :
C       -----------------------------
        CALL UTBTAB('ZERO',2,4,DCI,BCA,XAB4,KAA)
C
C ---   CALCUL DU PRODUIT BCBT.DCI.BCA :
C       ------------------------------
        CALL UTCTAB('ZERO',2,4,12,DCI,BCA,BCB,XAB4,KBA)
C
C ---   CALCUL DU MATERIAU ELAS_COQUE :
C       =============================
C
C ---   CALCUL DU PRODUIT BFBT.DFC.DCI.BCA :
C       ----------------------------------
        CALL UTDTAB('ZERO',3,2,2,12,DFC,DCI,BFB,XAB7,XAB8)
        CALL PROMAT(XAB8,12,12,2,BCA,2,2,4,KFCG11)
C
C ---   CALCUL DU PRODUIT BFAT.DFC.DCI.BCA :
C       ----------------------------------
        CALL UTDTAB('ZERO',3,2,2,4,DFC,DCI,BFA,XAB7,XAB9)
        CALL PROMAT(XAB9,4,4,2,BCA,2,2,4,KFC21)
C
C ---   CALCUL DU PRODUIT BMT.DMC.DCI.BCA :
C       ----------------------------------
        CALL UTDTAB('ZERO',3,2,2,8,DMC,DCI,BM,XAB7,XAB10)
        CALL PROMAT(XAB10,8,8,2,BCA,2,2,4,KMC)
C
C ---   CALCUL DES SOMMES KF + KC = KFC :
C       -------------------------------
        DO 20 I = 1,12
          DO 30 J = 1,12
            KFC11(I,J) = KF11(I,J) + KBB(I,J)
   30     CONTINUE
   20   CONTINUE
        DO 40 I = 1,12
          DO 50 J = 1, 4
            KFC12(I,J) = KF12(I,J) + KBA(I,J) + KFCG11(I,J)
   50     CONTINUE
   40   CONTINUE
        DO 60 I = 1,4
          DO 70 J = 1,4
            KFC22(I,J) = KF22(I,J) + KAA(I,J) + KFC21(I,J) +
     +                   KFC21(J,I)
   70     CONTINUE
   60   CONTINUE
C
        IF (COUPMF.OR.EXCE) THEN
C
C ---     CALCUL DU PRODUIT BMT.DMF.BFB :
C         -----------------------------
          CALL UTCTAB('ZERO',3,12,8,DMF,BFB,BM,XAB1,KMF11)
C
C ---     CALCUL DU PRODUIT BMT.DMF.BFA :
C         -----------------------------
          CALL UTCTAB('ZERO',3,4,8,DMF,BFA,BM,XAB2,KMF12)
C
        ENDIF

C===============================================
C ---   PREPARATION DU CAS DE L'EXCENTREMENT   =
C===============================================
        IF (EXCE) THEN
C
          CALL R8INIR(64,ZERO,KMPMT,1)
          CALL R8INIR(64,ZERO,KMPM,1)
          CALL R8INIR(96,ZERO,KMAPB,1)
C
C ---     AFFECTATION DE LA MATRICE [MEMBCF] EGALE A [BCM]T*[DCI]*[BCM]:
C         -------------------------------------------------------------
          CALL UTBTAB('ZERO',2,8,DCI,BCM,XAB12,MEMBCF)

C
C ---     AFFECTATION DE LA MATRICE [KMA] EGALE A [BCM]T*[DCI]*[BCA] :
C         ---------------------------------------------------------
          CALL UTCTAB('ZERO',2,4,8,DCI,BCA,BCM,XAB4,KMA)
C
C ---     AFFECTATION DE LA MATRICE [KMB] EGALE A [BCM]T*[DCI]*[BCB] :
C         ---------------------------------------------------------
          CALL UTCTAB('ZERO',2,12,8,DCI,BCB,BCM,XAB3,KMB)
C
C ---     DETERMINATION DU TERME [PM]T*([KMA]*T+[KMF12]T) :
C         -----------------------------------------------
          DO 80 I = 1, 8
            DO 90 J = 1, 8
              DO 100 K = 1, 4
                 KMPMT(I,J) = KMPMT(I,J) +
     +                        PM(K,I)*(KMA(J,K)+KMF12(J,K))
 100          CONTINUE
  90        CONTINUE
  80      CONTINUE
C
C ---     DETERMINATION DU TERME ([KMA]+[KMF12])*[PM] :
C         --------------------------------------------
          DO 110 I = 1, 8
            DO 120 J = 1, 8
              DO 130 K = 1, 4
                 KMPM(I,J) = KMPM(I,J) + (KMA(I,K)+KMF12(I,K))*PM(K,J)
 130          CONTINUE
 120        CONTINUE
 110      CONTINUE
C
C ---     DETERMINATION DU TERME [KMA]*[PB] :
C         ---------------------------------
          DO 140 I = 1, 8
            DO 150 J = 1, 12
              DO 160 K = 1, 4
                 KMAPB(I,J) = KMAPB(I,J) + KMA(I,K)*PB(K,J)
 160          CONTINUE
 150        CONTINUE
 140      CONTINUE
C
        ENDIF
C=======================================================================
C --- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION                      =
C ---   FLEXI = KF11 + KBB + KFC12*PB + PB_T*KFC12_T + PB_T*KFC22*PB   =
C=======================================================================
C
C ---   FLEXI = KFC11 + KFC12*PB + PB_T*KFC12_T + PB_T*KFC22*PB  :
C       =======================================================
C
C ---   CALCUL DU PRODUIT PBT.KFC22.PB :
C       ------------------------------
        CALL UTBTAB('ZERO',4,12,KFC22,PB,XAB5,FLEXI)
C
       DO 170 I = 1,12
         DO 170 J = 1,12
          KFB(I,J) = ZERO
  170   CONTINUE
C
        DO 180 I = 1,12
          DO 190 J = 1,12
            DO 200 K = 1,4
              KFB(I,J) = KFB(I,J) + KFC12(I,K)*PB(K,J)
  200       CONTINUE
            KFC(J,I) = KFB(I,J)
  190     CONTINUE
  180   CONTINUE
C
        DO 210 I = 1,12
          DO 220 J = 1,12
            FLEXI(I,J) = FLEXI(I,J) + KFC11(I,J) + KFB(I,J) + KFC(I,J)
  220     CONTINUE
  210   CONTINUE
C
        WGT = ZR(IPOIDS+INT-1)*JACOB(1)
        DO 230 I = 1,12
        DO 230 J = 1,12
          FLEX(I,J) = FLEX(I,J) + FLEXI(I,J)*WGT
  230   CONTINUE
C
C============================================================
C --- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE          =
C --- K_MEMBRANE =   MEMBI + KMF12*PM + PM_T*KMF12_T        =
C ---              + PM_T*(KF22+KAA)*PM + BCM_T*DCI*BCM     =
C ---              + KMA*PM + PM_T*KMA_T                    =
C============================================================
C
C ---   CALCUL DU PRODUIT BMT.DM.BM :
C       ---------------------------
        CALL UTBTAB('ZERO',3,8,DM,BM,XAB6,MEMBI)
C
C ---    CALCUL DE [PM]T*([KF22] + [KAA])*[PM]
C       --------------------------------------
        IF (EXCE) THEN
          CALL UTBTAB('ZERO',4,8,KFC22,PM,XAB11,MEMEXC)
        ENDIF
C
C ---   CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE :
C*****************************************************************
C ATTENTION POUR LE MOMENT ON DESACTIVE LES TERMES SUPPLEMENTAIRES
C DUS A L'EXCENTREMENT ET INTERVENANT EN MEMBRANE :
C    MEMEXC -> PM_T*(KF22+KAA)*PM
C    KMPM   -> (KMF12+KMA)*PM
C    KMPMT  -> PM_T*(KMF12_T+KMA_T)
C    MEMBCF -> BCM_T*DCI*BCM
C FINALEMENT ON DECIDE DE REACTIVER CES TERMES EN ATTENDANT
C DES ANOMALIES TROP IMPORTANTES
C ---------------------------------------------------------------
        DO 240 I = 1,8
          DO 250 J = 1,8
          MEMB(I,J) = MEMB(I,J) +
     +         (MEMEXC(I,J)+MEMBI(I,J)+KMPM(I,J)+KMPMT(I,J)
     +         +MEMBCF(I,J))*WGT
C     +                            MEMBI(I,J)*WGT
C*****************************************************************
  250     CONTINUE
  240   CONTINUE

C====================================================================
C --- CALCUL DE LA MATRICE DE RIGIDITE DE COUPLAGE MEMBRANE-FLEXION =
C --- K_MEMBRANE-FLEXION =   KMF11 + KMB + PM_T*(KF22+KAA)*PB       =
C ---                      +  PM_T*(KF12_T+KBA_T) + KMA*PB          =
C ---                      +  KMF12*PB                              =
C====================================================================
C
         IF (COUPMF.OR.EXCE) THEN
C
          DO 260 I = 1,8
            DO 270 J = 1,12
              KMF(I,J) = ZERO
  270       CONTINUE
  260     CONTINUE
C
C ---     CALCUL DU TERME  [PM]T*([KF22] + [KAA])*[PB]
C         --------------------------------------------
          CALL UTCTAB('ZERO',4,12,8,KFC22,PB,PM,XAB5,KMF)
C
          DO 280 I = 1,8
            DO 290 J = 1,12
              DO 300 K = 1,4
                KMF(I,J) = KMF(I,J) + (KMF12(I,K)+KMC(I,K))*PB(K,J)
     +                              + PM(K,I)*KFC12(J,K)
  300         CONTINUE
              MEFLI(I,J) =   KMF11(I,J)  + KMF(I,J) + KMB(I,J)
     +                   +   KMAPB(I,J)
  290       CONTINUE
  280     CONTINUE
C
          DO 310 I = 1,8
            DO 320 J = 1,12
              MEFL(I,J) = MEFL(I,J) + MEFLI(I,J)*WGT
  320     CONTINUE
  310     CONTINUE
C
         END IF
C
  10  CONTINUE
C
      IF ( OPTION.EQ.'RIGI_MECA' ) THEN
        CALL DXQLOC(FLEX,MEMB,MEFL,CTOR,RIG)
C
      ELSE IF (OPTION.EQ.'EPOT_ELEM') THEN
        CALL JEVECH('PDEPLAR','L',JDEPG)
        CALL UTPVGL(4,6,PGL,ZR(JDEPG),DEPL)
        CALL DXQLOE(FLEX,MEMB,MEFL,CTOR,COUPMF,DEPL,ENER)
        CALL BSTHPL(NOMTE(1:8),BSIGTH,INDITH)
        IF (INDITH) THEN
          DO 330 I = 1, 24
            ENERTH = ENERTH + DEPL(I)*BSIGTH(I)
 330      CONTINUE
          ENER(1) = ENER(1) - ENERTH
        ENDIF
      END IF
C
      END
