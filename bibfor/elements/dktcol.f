      SUBROUTINE DKTCOL ( NOMTE, XYZL, OPTION, PGL, ICOU, INIV, DEPL,
     +                    CDL, MULTIC, GRILLE )
      IMPLICIT  NONE
      INTEGER             ICOU, INIV, MULTIC
      REAL*8              XYZL(3,*),PGL(3,*), DEPL(*), CDL(*)
      LOGICAL             GRILLE
      CHARACTER*16        NOMTE, OPTION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHPD S.VANDENBERGHE 
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
C     ------------------------------------------------------------------
C     CONTRAINTES ET DEFORMATIONS DE L'ELEMENT DE PLAQUE DKT
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
C     IN  OPTION : NOM DE L'OPTION DE CALCUL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  ICOU   : NUMERO DE LA COUCHE
C     IN  INIV   : NIVEAU DANS LA COUCHE (-1:INF , 0:MOY , 1:SUP)
C     IN  DEPL   : DEPLACEMENTS
C     IN  GRILLE : .TRUE. => ELEMENT DE GRILLE (MEGRDKT)
C          3 POUR UN MATERIAU ORTHOTROPE (MEGRDKT / MEGRDKQ)
C     OUT CDL    : CONTRAINTES OU DEFORMATIONS AUX NOEUDS DANS LE REPERE
C                  INTRINSEQUE A L'ELEMENT
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
      INTEGER       LZR,NE,INE,JCACO,I,J,K,IE,JMATE,IC,ICPG,IG
      REAL*8        R8BID,ZIC,HIC,ZMIN,DEUX,X3I,EPAIS,DISTN
      REAL*8        DEPF(9),DEPM(6),VT(2),LAMBDA(4)
      REAL*8        DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2)
      REAL*8        H(3,3),D1I(2,2),D2I(2,4),BF(3,9),BM(3,6)
      REAL*8        SM(3),SF(3),HFT2(2,6),HLT2(4,6)
      REAL*8        EPS(3),SIG(3),DCIS(2),CIST(2)
      CHARACTER*2   VAL, CODRET
      CHARACTER*3   NUM
      CHARACTER*8   NOMRES
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER NPG,NC,NNO
      INTEGER LJACO,LTOR,LQSI,LETA,LWGT,LXYC,LCOTE,LCOS,LSIN
      INTEGER LAIRE,LT1VE,LT2VE
      PARAMETER (NPG=3)
      PARAMETER (NNO=3)
      PARAMETER (NC=3)
      PARAMETER (LJACO=2)
      PARAMETER (LTOR=LJACO+4)
      PARAMETER (LQSI=LTOR+1)
      PARAMETER (LETA=LQSI+NPG+NNO)
      PARAMETER (LWGT=LETA+NPG+NNO)
      PARAMETER (LXYC=LWGT+NPG)
      PARAMETER (LCOTE=LXYC+2*NC)
      PARAMETER (LCOS=LCOTE+NC)
      PARAMETER (LSIN=LCOS+NC)
      PARAMETER (LAIRE=LSIN+NC)
      PARAMETER (LT1VE=LAIRE+1)
      PARAMETER (LT2VE=LT1VE+9)
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      DEUX = 2.D0
C
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ',LZR)
      IF (OPTION(6:9).EQ.'ELGA') THEN
        NE  = NPG
        INE = 0
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
        NE  = NNO
        INE = NPG
      END IF
C     ----- RAPPEL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
      CALL GTRIA3(XYZL,ZR(LZR))

C     ----- CARACTERISTIQUES DES MATERIAUX --------
      CALL DMATEL(DF,DM,DMF,DC,DCI,NNO,PGL,ZR(LZR),MULTIC,ICOU,GRILLE)

C     -------- CALCUL DE D1I ET D2I ------------------------------------
      IF (MULTIC.EQ.0 .OR. GRILLE) THEN

        CALL JEVECH('PCACOQU','L',JCACO)
        EPAIS = ZR(JCACO)
        X3I = 0.D0
        IF (GRILLE) X3I = ZR(JCACO+6)

        IF (INIV.LT.0) THEN
          X3I = X3I - EPAIS/DEUX
        ELSE IF (INIV.GT.0) THEN
          X3I = X3I + EPAIS/DEUX
        END IF
        DO 10 K = 1,9
          H(K,1) = DM(K,1)/EPAIS
   10   CONTINUE
        D1I(1,1) = 3.D0/ (DEUX*EPAIS) -
     +             X3I*X3I*6.D0/ (EPAIS*EPAIS*EPAIS)
        D1I(2,2) = D1I(1,1)
        D1I(1,2) = 0.D0
        D1I(2,1) = 0.D0
      ELSE
        CALL DXDMUL(ICOU,INIV,ZR(LZR-1+LT1VE),ZR(LZR-1+LT2VE),H,D1I,D2I,
     +              X3I)
      END IF
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
      CALL DXTBM(ZR(LZR),BM)
C     ------ SM = BM.DEPM ----------------------------------------------
      DO 40 I = 1,3
        SM(I) = 0.D0
   40 CONTINUE
      DO 60 I = 1,3
        DO 50 J = 1,6
          SM(I) = SM(I) + BM(I,J)*DEPM(J)
   50   CONTINUE
   60 CONTINUE

      IF (GRILLE) THEN
        DO 70 I = 1,2
          VT(I) = 0.D0
   70   CONTINUE
      ELSE

C     ------- CALCUL DU PRODUIT HF.T2 ----------------------------------
        CALL DSXHFT(DF,ZR(LZR),HFT2)
C     ------ VT = HFT2.TKT.DEPF ---------------------------------------
        CALL DKTTXY(HFT2,DEPF,ZR(LZR),VT)

      END IF


      IF (OPTION(1:4).EQ.'EPSI') THEN
C         ---------------------
        DO 100 IE = 1,NE
C           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
          CALL DKTBF(IE+INE,ZR(LZR),BF)
          DO 110 I = 1,3
            SF(I) = 0.D0
 110      CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 120 I = 1,3
            DO 122 J = 1,9
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
 122        CONTINUE
 120      CONTINUE
          DO 130 I = 1,3
            EPS(I) = SM(I) + X3I*SF(I)
 130      CONTINUE
C           ------ DCIS = DCI.VT --------------------------------------
          DCIS(1) = DCI(1,1)*VT(1) + DCI(1,2)*VT(2)
          DCIS(2) = DCI(2,1)*VT(1) + DCI(2,2)*VT(2)
          CDL(1+6* (IE-1)) = EPS(1)
          CDL(2+6* (IE-1)) = EPS(2)
          CDL(3+6* (IE-1)) = 0.D0
C           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
          CDL(4+6* (IE-1)) = EPS(3)/DEUX
          CDL(5+6* (IE-1)) = DCIS(1)/DEUX
          CDL(6+6* (IE-1)) = DCIS(2)/DEUX
 100    CONTINUE

C
      ELSE IF (OPTION(1:4).EQ.'SIGM') THEN
C              ---------------------
C        ------ CIST = D1I.VT ( + D2I.LAMBDA SI MULTICOUCHES ) ---------
        CIST(1) = D1I(1,1)*VT(1) + D1I(1,2)*VT(2)
        CIST(2) = D1I(2,1)*VT(1) + D1I(2,2)*VT(2)

        IF (MULTIC.GT.0 .AND. (.NOT.GRILLE)) THEN
C           ------- CALCUL DU PRODUIT HL.T2 ---------------------------
          CALL DSXHLT(DF,ZR(LZR),HLT2)
C           ------ LAMBDA = HLT2.TKT.DEPF -----------------------------
          CALL DKTLXY(HLT2,DEPF,ZR(LZR),LAMBDA)
          DO 200 J = 1,4
            CIST(1) = CIST(1) + D2I(1,J)*LAMBDA(J)
            CIST(2) = CIST(2) + D2I(2,J)*LAMBDA(J)
 200      CONTINUE
        END IF
        DO 210 IE = 1,NE
C           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
          CALL DKTBF(IE+INE,ZR(LZR),BF)
          DO 212 I = 1,3
            SF(I) = 0.D0
 212      CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 214 I = 1,3
            DO 216 J = 1,9
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
 216        CONTINUE
 214     CONTINUE
          DO 218 I = 1,3
            EPS(I) = SM(I) + X3I*SF(I)
            SIG(I) = 0.D0
 218      CONTINUE
          DO 220 I = 1,3
            DO 222 J = 1,3
              SIG(I) = SIG(I) + H(I,J)*EPS(J)
 222        CONTINUE
 220      CONTINUE
          CDL(1+6* (IE-1)) = SIG(1)
          CDL(2+6* (IE-1)) = SIG(2)
          CDL(3+6* (IE-1)) = 0.D0
          CDL(4+6* (IE-1)) = SIG(3)
          CDL(5+6* (IE-1)) = CIST(1)
          CDL(6+6* (IE-1)) = CIST(2)
 210    CONTINUE

C
      ELSE IF (OPTION(1:4).EQ.'SIEF') THEN
C              ---------------------
C        ------ CIST = D1I.VT ( + D2I.LAMBDA SI MULTICOUCHES ) ---------
        CIST(1) = D1I(1,1)*VT(1) + D1I(1,2)*VT(2)
        CIST(2) = D1I(2,1)*VT(1) + D1I(2,2)*VT(2)

        DISTN = 0.D0
        IF (MULTIC.EQ.0) THEN
          CALL JEVECH ( 'PCACOQU', 'L', JCACO )
          EPAIS = ZR(JCACO)
          IF (GRILLE) DISTN = ZR(JCACO+6)
        ELSE
          CALL JEVECH ( 'PMATERC', 'L', JMATE )
        ENDIF

        IF (MULTIC.GT.0 .AND. (.NOT.GRILLE)) THEN
C           ------- CALCUL DU PRODUIT HL.T2 ---------------------------
          CALL DSXHLT(DF,ZR(LZR),HLT2)
C           ------ LAMBDA = HLT2.TKT.DEPF -----------------------------
          CALL DKTLXY(HLT2,DEPF,ZR(LZR),LAMBDA)
          DO 300 J = 1,4
            CIST(1) = CIST(1) + D2I(1,J)*LAMBDA(J)
            CIST(2) = CIST(2) + D2I(2,J)*LAMBDA(J)
 300      CONTINUE
        END IF

        DO 310 IE = 1,NE
C           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
          CALL DKTBF(IE+INE,ZR(LZR),BF)
          DO 312 I = 1,3
            SF(I) = 0.D0
 312      CONTINUE
C           ------ SF = BF.DEPF ---------------------------------------
          DO 314 I = 1,3
            DO 316 J = 1,9
              SF(I) = SF(I) + BF(I,J)*DEPF(J)
 316        CONTINUE
 314     CONTINUE

          DO 320 IC = 1 , ICOU

            DISTN = 0.D0
            IF (MULTIC.NE.0) THEN
              CALL CODENT ( IC, 'G', NUM )
              CALL CODENT (  1, 'G', VAL )
              NOMRES = 'C'//NUM//'_V'//VAL
              CALL RCVALA ( ZI(JMATE), 'ELAS_COQMU', 0, ' ', R8BID,
     +                      1, NOMRES, EPAIS, CODRET, 'FM' )
            END IF
            HIC  =  EPAIS/ICOU
            IF (GRILLE) THEN
              ZMIN = -EPAIS/DEUX + HIC/DEUX + DISTN
            ELSE
              ZMIN = -EPAIS/DEUX
            END IF

            DO 330, IG = 1 , INIV

              ICPG = 6*INIV*ICOU*(IE-1) + 6*INIV*(IC-1) + 6*(IG-1)

C             -- COTE DES POINTS D'INTEGRATION
C             --------------------------------
              IF (IG.EQ.1) THEN
                ZIC = ZMIN + (IC-1)*HIC
              ELSE IF (IG.EQ.2) THEN
                ZIC = ZMIN + HIC/DEUX + (IC-1)*HIC
              ELSE
                ZIC = ZMIN + HIC + (IC-1)*HIC
              END IF

              DO 332 I = 1,3
                EPS(I) = SM(I) + ZIC*SF(I)
                SIG(I) = 0.D0
 332          CONTINUE
              DO 334 I = 1,3
                DO 336 J = 1,3
                  SIG(I) = SIG(I) + H(I,J)*EPS(J)
 336            CONTINUE
 334          CONTINUE
              CDL(ICPG+1) = SIG(1)
              CDL(ICPG+2) = SIG(2)
              CDL(ICPG+3) = 0.D0
              CDL(ICPG+4) = SIG(3)
              CDL(ICPG+5) = CIST(1)
              CDL(ICPG+6) = CIST(2)
 330        CONTINUE
 320      CONTINUE
 310    CONTINUE
      END IF
      CALL JEDEMA()
      END
