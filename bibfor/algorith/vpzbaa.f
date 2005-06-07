      SUBROUTINE VPZBAA(N,IB,A,IA,IBAS,LHI,D)
C
C***********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/10/97   AUTEUR KXBADNG N.GAY 
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
C***********************************************************************
C     PROCEDURE BALANCE
C     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.320-321)
C
C     CONDITIONNEMENT DE LA MATRICE A(N,N) EN REDUISANT LA NORME DE LA
C     MATRICE PAR DES TRANSFORMATIONS BASEES SUR UNE SIMILARITE
C     DIAGONALE EXACTE ET STOCKEES DANS LE VECTEUR D(N)
C
C
C --- DECLARATIONS
C
      IMPLICIT NONE
C
C ARGUMENTS
C ---------
      INTEGER N, IB, IA, IBAS, LHI
      REAL*8 A(IA,N), D(N)
C
C VARIABLES LOCALES
C -----------------
C
      REAL*8 B2, C, F, G, R, S, FF
      INTEGER I, J, IJ, JJ, K, L
      LOGICAL NOCONV
C
C
C***********************************************************************
C                      DEBUT DU CODE EXECUTABLE
C***********************************************************************
C
C
C --- INITIALISATION
C
      B2 = DBLE(IB*IB)
      L = 1
      K = N
C
C======================================================================
C     ON RECHERCHE DES RANGEES ISOLANT UNE VALEUR PROPRE ET
C     ON LES POUSSE VERS LE BAS
C======================================================================
   20 CONTINUE
      IF (K.GE.1) THEN
C
      J = K + 1
C
      DO 100 JJ = 1, K
         J = J - 1
         R = 0.0D0
C
         DO 40 I = 1, K
            IF (I.NE.J) THEN
            R = R + ABS(A(J,I))
            ENDIF
   40    CONTINUE
C
         IF (R.EQ.0.0D0) THEN
C
C --- INTERVERSION DES ELEMENTS 1 A K DES COLONNES J ET K
C     ET DES ELEMENTS L A N DES RANGEES J ET K
C
      D(K)=J
      IF (J .NE. K) THEN
      DO 60 IJ=1,K
        FF = A(IJ,J)
        A(IJ,J)=A(IJ,K)
        A(IJ,K)= FF
 60   CONTINUE
       IF (L .LE. N) THEN
        DO 80 IJ=L,N
         FF=A(J,IJ)
         A(J,IJ)=A(K,IJ)
         A(K,IJ)=FF
 80     CONTINUE
       ENDIF
      ENDIF
         K = K - 1
         GO TO 20
         ENDIF
C
  100 CONTINUE
C
      ENDIF
C
C
C======================================================================
C     ON RECHERCHE DES COLONNES ISOLANT UNE VALEUR PROPRE ET
C     ON LES POUSSE VERS LA GAUCHE
C======================================================================
C
  120 CONTINUE
      IF (L.LE.K) THEN
C
      DO 200 J = L, K
         C = 0.0D0
C
         DO 140 I = L, K
            IF (I.NE.J) THEN
            C = C + ABS(A(I,J))
            ENDIF
  140    CONTINUE
C
         IF (C.EQ.0.0D0) THEN
C
C --- INTERVERSION DES ELEMENTS 1 A K DES COLONNES J ET L
C     ET DES ELEMENTS L A N DES RANGEES J ET L
C
      D(L)=J
      IF (J .NE. L) THEN
      DO 160 IJ=1,K
        FF = A(IJ,J)
        A(IJ,J)=A(IJ,L)
        A(IJ,L)= FF
160   CONTINUE
       IF (L .LE. N) THEN
        DO 180 IJ=L,N
         FF=A(J,IJ)
         A(J,IJ)=A(L,IJ)
         A(L,IJ)=FF
180     CONTINUE
       ENDIF
      ENDIF
         L = L + 1
         GO TO 120
         ENDIF
C
 200  CONTINUE
C
      ENDIF
C
C
C======================================================================
C     CONDITIONNEMENT DE LA SOUS-MATRICE DANS LES RANGEES L A K
C======================================================================
C
      IBAS = L
      LHI = K
C
      IF (L.LE.K) THEN
      DO 220 I = L, K
         D(I) = 1.0D0
  220 CONTINUE
      ENDIF
C
  240 CONTINUE
      NOCONV = .FALSE.
C
      IF (L.LE.K) THEN
C
      DO 400 I = L, K
C
         C = 0.0D0
         R = 0.0D0
C
         DO 260 J = L, K
            IF (J.NE.I) THEN
            C = C + ABS(A(J,I))
            R = R + ABS(A(I,J))
            ENDIF
  260    CONTINUE
C
         G = R/DBLE(IB)
         F = 1.0D0
         S = C + R
C
  280    CONTINUE
         IF (C.GE.G) GO TO 300
         F = F*DBLE(IB)
         C = C*B2
         GO TO 280
  300    CONTINUE
         G = R*DBLE(IB)
  320    CONTINUE
         IF (C.LT.G) GO TO 340
         F = F/DBLE(IB)
         C = C/B2
         GO TO 320
C
  340    CONTINUE
         IF (((C+R)/F).LT.(0.95D0*S)) THEN
          G = 1.0D0/F
          D(I) = D(I)*F
          NOCONV = .TRUE.
C
             IF (L.LE.N) THEN
              DO 360 J = L, N
                A(I,J) = A(I,J)*G
  360         CONTINUE
             ENDIF
C
          DO 380 J = 1, K
            A(J,I) = A(J,I)*F
  380     CONTINUE
C
         ENDIF
C
  400 CONTINUE
C
      ENDIF
C
      IF (NOCONV) THEN
       GO TO 240
      ENDIF
C
      END
