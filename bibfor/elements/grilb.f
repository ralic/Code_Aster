      SUBROUTINE GRILB (DISTN, LZR , IGAU, JACGAU,NCOI,NCOJ,BMAT)
      IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/05/97   AUTEUR INBHHOM O.MERABET 
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
C       ----------------------------------------------------------------
      PARAMETER    (NBNO   = 3)
      PARAMETER    (LDETJ = 1)
      REAL*8       BMAT(NCOI,NBNO*NCOJ),BM(3,6),  BF(3,9)
      REAL*8       JACGAU, DISTN
      INTEGER      NCOI,NCOJ
C     ------------------------------------------------------------------
C --- CALCUL DE LA MATRICE (B) RELIANT LES DEFORMATIONS DU PREMIER
C --- ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION D'INDICE IGAU
C --- POUR UN ELEMENT DE TYPE GRILLE
C --- D'AUTRE_PART, ON CALCULE LE PRODUIT NOTE JACGAU = JACOBIEN*POIDS
C     ------------------------------------------------------------------
C     IN  LZR           : ADRESSE DU VECTEUR .DESR DEFINISSANT UN
C                         CERTAIN NOMBRE DE QUANTITES GEOMETRIQUES
C                         SUR L'ELEMENT
C     IN  IGAU          : INDICE DU POINT D'INTEGRATION
C     IN  DISTN         : EXCENTREMENT DE L'ELEMENT GRILL
C     IN  NCOI          : NOMBRE DE LIGNE DE LA MATRICE BMAT
C     IN  NCOJ          : NOMBRE DE DEGRES DE LIBERTE PAR NOEUD
C     OUT JACGAU        : PRODUIT JACOBIEN*POIDS AU POINT D'INTEGRATION
C                         COURANT
C     OUT BMAT(NCOI,3*NCOJ): MATRICE (B) AU POINT D'INTEGRATION COURANT
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C --- PRODUIT JACOBIEN*POIDS
C     ----------------------
      JACGAU = 1.0D0/6.0D0*ZR(LZR-1+LDETJ)
C
C --- CALCUL DES MATRICES B_MEMBRANE ET B_FLEXION (BM ET BF)
C --- ET AFFECTATION DE LA MATRICE B COMPLETE NOTEE (BMAT)
C     ------------------------------------------------
      CALL R8INIR((NBNO*NCOI*NCOJ),0.0D0,BMAT,1)
      CALL DXTBM (ZR(LZR) , BM)
      CALL DKTBF (IGAU, ZR(LZR) , BF)
C
C --- AFFECTATION DE (BM) A (BMAT)
C     ----------------------------
      DO 10 I = 1, NBNO
         DO 20 K = 1, 2
           DO 30 J = 1, 3
             BMAT(J,NCOJ*(I-1)+K) = BM(J,2*(I-1)+K)
  30       CONTINUE
  20     CONTINUE
  10  CONTINUE
C
C --- AFFECTATION DE (BF) A (BMAT)
C     ----------------------------
      DO 40 I = 1, NBNO
         DO 50 K = 1, 3
           DO 60 J = 1, 3
             BMAT(3+J,NCOJ*(I-1)+K+2) = BF(J,3*(I-1)+K)
  60       CONTINUE
  50     CONTINUE
  40  CONTINUE
C
C
C --- AFFECTATION DE (BMF) A (BMAT)
C     ----------------------------
      DO 11 I = 1, NBNO
         DO 21 K = 1, 3
           DO 31 J = 1, 3
             BMAT(J,NCOJ*(I-1)+K+2)   = DISTN * BF(J,3*(I-1)+K)
  31       CONTINUE
  21     CONTINUE
  11  CONTINUE
C
      END
