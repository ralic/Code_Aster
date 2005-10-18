      SUBROUTINE DSTRIG ( NOMTE, XYZL, OPTION, PGL, RIG, ENER )
      IMPLICIT  NONE
      REAL*8        XYZL(3,*), PGL(*), RIG(*), ENER(*)
      CHARACTER*16  OPTION , NOMTE
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
C     ------------------------------------------------------------------
C MODIF ELEMENTS  DATE 14/10/2005   AUTEUR CIBHHLV L.VIVAN 
C
C     MATRICE DE RIGIDITE DE L'ELEMENT DE PLAQUE DST (AVEC CISAILLEMENT)
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
C     IN  OPTION : OPTION RIGI_MECA, RIGI_MECA_SENS* OU EPOT_ELEM_DEPL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
C     OUT RIG    : MATRICE DE RIGIDITE
C     OUT ENER   : TERMES POUR ENER_POT (EPOT_ELEM_DEPL)
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER  NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER INT, MULTIC, PERM(9), PERM2(36)
      INTEGER I, J, JCOQU, JDEPG, K, K1, K2
      REAL*8 WGT,AIRE
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 BFB(3,9),BFA(3,3),HFT2(2,6)
      REAL*8 BCA(2,3),PB(3,9),BM(3,6),PM(3,6)
      REAL*8 XAB1(3,6),XAB2(3,9),XAB3(2,3),XAB4(3,3),XAB5(3,2),XAB6(9,2)
      REAL*8 XAB7(3,2),XAB8(6,2),XAB9(3,6)
C                   ----(9,9)  ---(9,9)  ---(9,9)
      REAL*8 KF11(81),KFC(81),KFB(81),KF12(9,3)
      REAL*8 KMF11(6,9),KMF(6,9)
C                   ----(3,3) --(3,3) ---(3,3)
      REAL*8 KF22(9),KA(9),KAA(9)
C                   ----(9,9)  -----(9,9)  ----(6,6)
      REAL*8 FLEX(81),FLEXI(81),MEMB(36),MEMEXC(36)
C                   ----(6,9)  -----(6,9)

      REAL*8 MEFL(6,9),MEFLI(6,9),DEPL(18)
      REAL*8 KFC11(9,3), KFC21(9), KMC(6,3)
      REAL*8 KMF12(6,3), KMF12A(36)
      REAL*8 BSIGTH(24), ENERTH, EXCENT, UN, R8GAEM, ZERO
      REAL*8 QSI, ETA, CARAT3(21), T2EV(4), T2VE(4), T1VE(9)
      LOGICAL      ELASCO, EXCE, INDITH
C     ------------------------------------------------------------------
      REAL*8 CTOR
      DATA PERM  / 1, 4,   7,  2,  5,  8, 3, 6, 9 /
      DATA PERM2 / 1, 7,  13, 19, 25, 31,
     +             2, 8,  14, 20, 26, 32,
     +             3, 9,  15, 21, 27, 33,
     +             4, 10, 16, 22, 28, 34,
     +             5, 11, 17, 23, 29, 35,
     +             6, 12, 18, 24, 30, 36 /
C     ----------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     +                                         IVF,IDFDX,IDFD2,JGANO)
C
      ZERO = 0.0D0
      UN   = 1.0D0
      CALL R8INIR(27,ZERO,KFC11 ,1)
      CALL R8INIR( 9,ZERO,KFC21 ,1)
      CALL R8INIR(18,ZERO,KMC   ,1)
      CALL R8INIR(18,ZERO,PM    ,1)
      CALL R8INIR(18,ZERO,KMF12, 1)
      CALL R8INIR(36,ZERO,KMF12A,1)
      ENERTH = ZERO
C
      CALL JEVECH('PCACOQU','L',JCOQU)
      CTOR = ZR(JCOQU+3)
      EXCENT = ZR(JCOQU+4)
      EXCE = .FALSE.
      IF (ABS(EXCENT).GT.UN/R8GAEM()) EXCE = .TRUE.

C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE --------
      CALL GTRIA3 ( XYZL, CARAT3 )

C     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEE -------------------------
      CALL DXMATE(DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,.FALSE.,
     +                                         ELASCO,T2EV,T2VE,T1VE)
C     ------------------------------------------------------------------
C     CALCUL DE LA MATRICE DE RIGIDITE DE L'ELEMENT EN MEMBRANE
C     ------------------------------------------------------------------

C     ------ CALCUL DE LA MATRICE BM -----------------------------------
      CALL DXTBM ( CARAT3(9), BM )
C     ------ CALCUL DU PRODUIT BMT.DM.BM -------------------------------
      CALL UTBTAB('ZERO',3,6,DM,BM,XAB1,MEMB)
      AIRE = CARAT3(8)
      DO 10 K = 1,36
        MEMB(K) = MEMB(K)*AIRE
   10 CONTINUE

C     ------------------------------------------------------------------
C     CALCUL DES MATRICES DE RIGIDITE DE L'ELEMENT EN FLEXION ET
C     COUPLAGE MEMBRANE/FLEXION
C     ------------------------------------------------------------------

C     ------- CALCUL DE LA MATRICE BFB -------------------------------
      CALL DSTBFB ( CARAT3(9) , BFB )

C     ------- CALCUL DU PRODUIT BFBT.DF.BFB --------------------------
      CALL UTBTAB('ZERO',3,9,DF,BFB,XAB2,KF11)

      IF (MULTIC.EQ.2) THEN
C        ----- CALCUL DU PRODUIT BMT.DMF.BFB ------------------------
        CALL UTCTAB('ZERO',3,9,6,DMF,BFB,BM,XAB2,KMF11)
      END IF

C     ------- CALCUL DU PRODUIT HF.T2 ----------------------------------
      CALL DSXHFT ( DF, CARAT3(9), HFT2 )

      IF (EXCE) THEN
C
C ---   CALCUL DES MATRICES BCA ,PB ET PM DANS LE CAS DE L'EXCENTREMENT:
C       ---------------------------------------------------------------
        CALL DSTCI2 ( DCI, CARAT3, HFT2, DFC, DMC, BCA, PB, PM )
      ELSE
C
C ---   CALCUL DES MATRICES BCA ET PB DANS LE CAS NON EXCENTRE :
C       ------------------------------------------------------
        CALL DSTCIS ( DCI , CARAT3 , HFT2 , BCA , PB )
C
      ENDIF

C     ------- CALCUL DU PRODUIT BCAT.DCI.BCA ---------------------------
      CALL UTBTAB('ZERO',2,3,DCI,BCA,XAB3,KAA)

      DO 20 K = 1,81
        FLEX(K) = 0.D0
   20 CONTINUE
      DO 30 I = 1,6
      DO 30 J = 1,9
        MEFL(I,J) = 0.D0
   30 CONTINUE

      DO 170 INT = 1,NPG
C
C ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
C       ------------------------------------------
        QSI = ZR(ICOOPG-1+NDIM*(INT-1)+1)
        ETA = ZR(ICOOPG-1+NDIM*(INT-1)+2)
C
C ---   CALCUL DE LA MATRICE BFA AU POINT QSI ETA :
C       -----------------------------------------
        CALL DSTBFA ( QSI, ETA , CARAT3 , BFA )
C
C ---   CALCUL DU PRODUIT BFBT.DF.BFA :
C       -----------------------------
        CALL UTCTAB('ZERO',3,3,9,DF,BFA,BFB,XAB4,KF12)
C
C ---   CALCUL DU PRODUIT BFAT.DF.BFA :
C       -----------------------------
        CALL UTBTAB('ZERO',3,3,DF,BFA,XAB4,KF22)
C
C=========================================
C ---   CAS DU COMPORTEMENT ELAS_COQUE   =
C=========================================
        IF (ELASCO) THEN
C
C ---     CALCUL DU PRODUIT BFBT.DFC.DCI.BCA :
C         ----------------------------------
          CALL UTDTAB('ZERO',3,2,2,9,DFC,DCI,BFB,XAB5,XAB6)
          CALL PROMAT(XAB6,9,9,2,BCA,2,2,3,KFC11)
C
C ---     CALCUL DU PRODUIT BFAT.DFC.DCI.BCA :
C         ----------------------------------
          CALL UTDTAB('ZERO',3,2,2,3,DFC,DCI,BFA,XAB5,XAB7)
          CALL PROMAT(XAB7,3,3,2,BCA,2,2,3,KFC21)
C
C ---     CALCUL DU PRODUIT BMT.DMC.DCI.BCA :
C         ---------------------------------
          CALL UTDTAB('ZERO',3,2,2,6,DMC,DCI,BM,XAB5,XAB8)
          CALL PROMAT(XAB8,6,6,2,BCA,2,2,3,KMC)
C
        ENDIF
C
C==============================================================
C ---   CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE RIGIDITE =
C ---   FLEXI =  KF11 + KF12 * PB + PB T * KF12 T             =
C ---          + PB T * ( KF22 + KAA ) * PB                   =
C==============================================================
C
C ---   CALCUL DE LA SOMME KF22 + KAA :
C       -----------------------------
        DO 40 K = 1,9
          KA(K) = KF22(K) + KAA(K) + KFC21(K) + KFC21(PERM(K))
   40   CONTINUE
C
C ---   CALCUL DU PRODUIT PBT.KA.PB :
C       ---------------------------                          
        CALL UTBTAB('ZERO',3,9,KA,PB,XAB2,FLEXI)
        DO 50 K1 = 1,81
          KFB(K1) = 0.D0
   50   CONTINUE
        DO 80 I = 1,9
          DO 70 J = 1,9
            K1 = 9* (J-1) + I
            K2 = 9* (I-1) + J
            DO 60 K = 1,3
              KFB(K1) = KFB(K1) + (KF12(I,K)+KFC11(I,K))*PB(K,J)
   60       CONTINUE
            KFC(K2) = KFB(K1)
   70     CONTINUE
   80   CONTINUE
        DO 90 K = 1,81
          FLEXI(K) = FLEXI(K) + KF11(K) + KFB(K) + KFC(K)
   90   CONTINUE

        WGT = ZR(IPOIDS+INT-1)*CARAT3(7)
        DO 100 K = 1,81
          FLEX(K) = FLEX(K) + FLEXI(K)*WGT
  100   CONTINUE
C
        IF (MULTIC.EQ.2.OR.EXCE) THEN
C
C ---     DETERMINATION DE LA MATRICE [KMF12] = [BM]T*[DMF]*[BFA]
C ---     CETTE MATRICE INTERVIENT DANS LA MATRICE DE RIGIDITE
C ---     DE MEMBRANE ET DANS LA PARTIE COUPLAGE MEMBRANE-FLEXION
C ---     DE LA RIGIDITE :
C         --------------
          CALL UTCTAB('ZERO',3,3,6,DMF,BFA,BM,XAB4,KMF12)
C
        ENDIF
C
        IF (EXCE) THEN
C=====================================================================
C ---     L'EXCENTREMENT FAIT QUE L'ON DOIT AJOUTER A LA RIGIDITE    =
C ---     DE MEMBRANE CLASSIQUE, LES TERMES                          =
C ---     [PM]T*([KF22]+[KAA])*[PM] + [KMF12]*[PM] + [PM]T*[KMF12]T  =
C=====================================================================
C
C ---     CALCUL DU TERME [PM]T*([KF22] + [KAA])*[PM] :
C         ------------------------------------------
          CALL UTBTAB('ZERO',3,6,KA,PM,XAB9,MEMEXC)
C
C ---     CALCUL DU TERME [KMF12]*[PM] :
C         ----------------------------
          CALL PROMAT(KMF12,6,6,3,PM,3,3,6,KMF12A)
C
C ---     AJOUT DE CES NOUVELLES RIGIDITES DE MEMBRANE A LA 
C ---     RIGIDITE DE MEMBRANE CLASSIQUE :
C         -----------------------------
          DO 110 I = 1, 36
             MEMB(I) = MEMB(I) + 
     +             (MEMEXC(I)+KMF12A(I)+KMF12A(PERM2(I)))*WGT
 110      CONTINUE
C
        ENDIF
C
C=====================================================================
C ---     TRAITEMENT DU COUPLAGE MEMBRANE-FLEXION                    =
C ---     L'EXCENTREMENT FAIT QUE L'ON DOIT AJOUTER A LA RIGIDITE    =
C ---     DE COUPLAGE MEMBRANE-FLEXION LES TERMES                    =
C ---     [PM]T*([KF22]+[KAA])*[PB] + [PM]T*[KF12]T + [KMF12]*[PB]   =
C=====================================================================
C
        IF (MULTIC.EQ.2.OR.EXCE) THEN
C
          DO 120 K = 1,54
            KMF(K,1) = 0.D0
  120     CONTINUE
C
          IF (EXCE) THEN
C
C ---       AJOUT DANS LA RIGIDITE DE COUPLAGE MEMBRANE-FLEXION
C ---       DU TERME  [PM]T*([KF22]+[KAA])*[PB] :
C           ----------------------------------
            CALL UTCTAB('ZERO',3,9,6,KA,PB,PM,XAB2,KMF)
C
          ENDIF
C
C ---     AJOUT DANS LA RIGIDITE DE COUPLAGE MEMBRANE-FLEXION
C ---     DES TERMES [PM]T*[KF12]T + [KMF12]*[PB]   :
C         --------------------------------------
          DO 130 I = 1,9
            DO 140 J = 1,6
              DO 150 K = 1,3
                KMF(J,I) = KMF(J,I) + (KMF12(J,K)+KMC(J,K))*PB(K,I)
     +                              + PM(K,J)*KF12(I,K)
  150         CONTINUE
              K = 6* (I-1) + J
              MEFLI(J,I) = KMF11(J,I) + KMF(J,I)
  140       CONTINUE
  130     CONTINUE
          DO 160 I = 1,6
          DO 160 J = 1,9
            MEFL(I,J) = MEFL(I,J) + MEFLI(I,J)*WGT
  160     CONTINUE
        END IF

  170 CONTINUE

      IF ( OPTION.EQ.'RIGI_MECA'      .OR.
     +     OPTION.EQ.'RIGI_MECA_SENSI' .OR.
     +     OPTION.EQ.'RIGI_MECA_SENS_C' ) THEN
        CALL DXTLOC(FLEX,MEMB,MEFL,CTOR,RIG)
C
      ELSE IF (OPTION.EQ.'EPOT_ELEM_DEPL') THEN
        CALL JEVECH('PDEPLAR','L',JDEPG)
        CALL UTPVGL(3,6,PGL,ZR(JDEPG),DEPL)
        CALL DXTLOE(FLEX,MEMB,MEFL,CTOR,MULTIC,DEPL,ENER)
         CALL BSTHPL(NOMTE(1:8),BSIGTH,INDITH)
         IF (INDITH) THEN
           DO 180 I = 1, 18
            ENERTH = ENERTH + DEPL(I)*BSIGTH(I)
 180       CONTINUE
           ENER(1) = ENER(1) - ENERTH
         ENDIF
      END IF
C
      END
