      SUBROUTINE DKTEDG(XYZL,OPTION,PGL,DEPL,EDGL,MULTIC,GRILLE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/06/99   AUTEUR CIBHHGB G.BERTRAND 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 XYZL(3,*),PGL(3,*)
      REAL*8 DEPL(*),EDGL(*)
      CHARACTER*16 OPTION
      LOGICAL GRILLE
C     ------------------------------------------------------------------
C     EFFORTS ET DEFORMATIONS GENERALISES DE L'ELEMENT DE PLAQUE DKT
C     ------------------------------------------------------------------
C     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
C     IN  OPTION : NOM DE L'OPTION DE CALCUL
C     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
C     IN  DEPL   : DEPLACEMENTS
C     IN  GRILLE : .TRUE. => ELEMENT DE GRILLE (MEGRDKT)
C          3 POUR UN MATERIAU ORTHOTROPE (MEGRDKT / MEGRDKQ)
C     OUT EDGL   : EFFORTS OU DEFORMATIONS GENERALISES AUX NOEUDS DANS
C                  LE REPERE INTRINSEQUE A L'ELEMENT
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
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
      CHARACTER*8 TYPELE
      CHARACTER*24 DESR
      REAL*8 DEPF(9),DEPM(6)
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 HFT2(2,6)
      REAL*8 BF(3,9),BM(3,6)
      REAL*8 BDM(3),BDF(3),DCIS(2)
      REAL*8 VF(3),VM(3),VT(2)
      REAL*8 VFM(3),VMF(3)
      REAL*8 DISTN,EPS(3)
      INTEGER MULTIC
      LOGICAL ELASCO
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER NPG,NNO
      PARAMETER (NPG=3)
      PARAMETER (NNO=3)
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IF (GRILLE) THEN
        TYPELE = 'MEGRDKT '
      ELSE
        TYPELE = 'MEDKTR3 '
      END IF
      DESR = '&INEL.'//TYPELE//'.DESR'
      CALL JEVETE(DESR,' ',LZR)
      IF (OPTION(6:9).EQ.'ELGA') THEN
        NE = NPG
        INE = 0
      ELSE IF (OPTION(6:9).EQ.'ELNO') THEN
        NE = NNO
        INE = NPG
      END IF
      DISTN = 0.D0
      IF (GRILLE) THEN
        CALL JEVECH('PCACOQU','L',JCOQU)
        DISTN = ZR(JCOQU+6)
      END IF

C     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
C           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------

C     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
      CALL GTRIA3(XYZL,ZR(LZR))
C     ----- CARACTERISTIQUES DES MATERIAUX --------
      CALL DXMATE(DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,ZR(LZR),MULTIC,
     +            GRILLE,ELASCO)
C     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
      DO 20 J = 1,3
        DO 10 I = 1,2
          DEPM(I+2* (J-1)) = DEPL(I+6* (J-1))
   10   CONTINUE
        DEPF(1+3* (J-1)) = DEPL(1+2+6* (J-1))
        DEPF(2+3* (J-1)) = DEPL(3+2+6* (J-1))
        DEPF(3+3* (J-1)) = -DEPL(2+2+6* (J-1))
   20 CONTINUE
C     ------ CALCUL DE LA MATRICE BM -----------------------------------
      CALL DXTBM(ZR(LZR),BM)
      DO 30 K = 1,3
        BDM(K) = 0.D0
   30 CONTINUE
      DO 50 I = 1,3
        DO 40 J = 1,6
          BDM(I) = BDM(I) + BM(I,J)*DEPM(J)
   40   CONTINUE
   50 CONTINUE
      IF (GRILLE) THEN
        DO 60 I = 1,2
          VT(I) = 0.D0
   60   CONTINUE
      ELSE
C     ------- CALCUL DU PRODUIT HF.T2 ----------------------------------
        CALL DSXHFT(DF,ZR(LZR),HFT2)
C     ------ VT = HFT2.TKT.DEPF ---------------------------------------
        CALL DKTTXY(HFT2,DEPF,ZR(LZR),VT)
      END IF
      IF (OPTION(1:4).EQ.'DEGE') THEN
        DO 110 IE = 1,NE
C           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
          CALL DKTBF(IE+INE,ZR(LZR),BF)
          DO 70 K = 1,3
            BDF(K) = 0.D0
   70     CONTINUE
          DO 90 I = 1,3
            DO 80 J = 1,9
              BDF(I) = BDF(I) + BF(I,J)*DEPF(J)
   80       CONTINUE
   90     CONTINUE
C           ------ DCIS = DCI.VT --------------------------------------
          DCIS(1) = DCI(1,1)*VT(1) + DCI(1,2)*VT(2)
          DCIS(2) = DCI(2,1)*VT(1) + DCI(2,2)*VT(2)
          DO 100 I = 1,3
            EDGL(I+8* (IE-1)) = BDM(I) + DISTN*BDF(I)
            EDGL(I+3+8* (IE-1)) = BDF(I)
  100     CONTINUE
C           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
          EDGL(3+8* (IE-1)) = EDGL(3+8* (IE-1))/2.D0
          EDGL(6+8* (IE-1)) = EDGL(6+8* (IE-1))/2.D0
          EDGL(7+8* (IE-1)) = DCIS(1)/2.D0
          EDGL(8+8* (IE-1)) = DCIS(2)/2.D0
  110   CONTINUE
      ELSE
        IF (.NOT.GRILLE) THEN
          DO 120 K = 1,3
            VM(K) = 0.D0
            VFM(K) = 0.D0
  120     CONTINUE
          DO 140 I = 1,3
            DO 130 J = 1,3
              VM(I) = VM(I) + DM(I,J)*BDM(J)
              VFM(I) = VFM(I) + DMF(I,J)*BDM(J)
  130       CONTINUE
  140     CONTINUE
        END IF
        DO 250 IE = 1,NE
C           ----- CALCUL DE LA MATRICE BF AU POINT QSI ETA ------------
          CALL DKTBF(IE+INE,ZR(LZR),BF)
          DO 150 K = 1,3
            BDF(K) = 0.D0
            VF(K) = 0.D0
            VMF(K) = 0.D0
  150     CONTINUE
C           ------ VF = DF.BF.DEPF , VMF = DMF.BF.DEPF ----------------
          DO 170 I = 1,3
            DO 160 J = 1,9
              BDF(I) = BDF(I) + BF(I,J)*DEPF(J)
  160       CONTINUE
  170     CONTINUE
          IF (GRILLE) THEN
            DO 180 I = 1,3
              EPS(I) = BDM(I) + DISTN*BDF(I)
  180       CONTINUE
            DO 200 I = 1,3
              DO 190 J = 1,3
                VMF(I) = VMF(I) + DM(I,J)*EPS(J)
                VF(I) = VF(I) + DF(I,J)*BDF(J)
  190         CONTINUE
  200       CONTINUE
            DO 210 I = 1,3
              EDGL(I+8* (IE-1)) = VMF(I)
              EDGL(I+3+8* (IE-1)) = VF(I)
  210       CONTINUE
          ELSE
            DO 230 I = 1,3
              DO 220 J = 1,3
                VF(I) = VF(I) + DF(I,J)*BDF(J)
                VMF(I) = VMF(I) + DMF(I,J)*BDF(J)
  220         CONTINUE
  230       CONTINUE
            DO 240 I = 1,3
              EDGL(I+8* (IE-1)) = VM(I) + VMF(I)
              EDGL(I+3+8* (IE-1)) = VF(I) + VFM(I)
  240       CONTINUE
          END IF
          EDGL(7+8* (IE-1)) = VT(1)
          EDGL(8+8* (IE-1)) = VT(2)
  250   CONTINUE
      END IF
      CALL JEDEMA()
      END
