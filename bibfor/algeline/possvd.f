      SUBROUTINE POSSVD(NM,M,N,W,MATU,U,MATV,V,EPS,RG,RV1)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/05/98   AUTEUR H1BAXBG M.LAINET 
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
C-----------------------------------------------------------------------
C
C DESCRIPTION :   POST-TRAITEMENTS AU CALCUL DE LA DECOMPOSITION AUX
C -----------     VALEURS SINGULIERES
C                                      T
C                             A = U S V
C
C                 REALISE PAR LA ROUTINE CALSVD
C                 A EST UNE MATRICE REELLE RECTANGULAIRE (M,N)
C
C IN     : NM   : INTEGER , SCALAIRE
C                 PREMIERE DIMENSION DES TABLEAUX A, U ET V, DECLAREE
C                 DANS L'APPELANT, NM >= MAX(M,N)
C IN     : M    : INTEGER , SCALAIRE
C                 NOMBRE DE LIGNES DES MATRICES A ET U
C IN     : N    : INTEGER , SCALAIRE
C                 NOMBRE DE COLONNES DES MATRICES A ET U
C                  = ORDRE DE LA MATRICE V
C IN/OUT : W    : REAL*8 , VECTEUR DE DIMENSION N
C                 CONTIENT LES N VALEURS SINGULIERES DE A
C                 EN SORTIE LES VALEURS SINGULIERES SONT REORDONNEES
C                 PAR MODULE DECROISSANT
C IN     : MATU : LOGICAL , SCALAIRE
C                 SI MATU = .TRUE. LA MATRICE U A ETE CALCULEE
C IN/OUT : U    : REAL*8 , TABLEAU DE DIMENSION (NM,N)
C                 SI MATU = .TRUE. LE TABLEAU U CONTIENT LA MATRICE U
C                 (MATRICE (M,N) A COLONNES ORTHOGONALES)
C                 EN SORTIE LES COLONNES SONT REORDONNEES CONFORMEMENT
C                 AUX VALEURS SINGULIERES
C IN     : MATV : LOGICAL , SCALAIRE
C                 SI MATV = .TRUE. LA MATRICE V A ETE CALCULEE
C IN/OUT : V    : REAL*8 , TABLEAU DE DIMENSION (NM,N)
C                 SI MATV = .TRUE. LE TABLEAU V CONTIENT LA MATRICE V
C                 (MATRICE CARREE D'ORDRE N ORTHOGONALE)
C                 EN SORTIE LES COLONNES SONT REORDONNEES CONFORMEMENT
C                 AUX VALEURS SINGULIERES
C IN     : EPS  : REAL*8 , SCALAIRE
C                 CRITERE DE PRECISION
C OUT    : RG   : INTEGER , SCALAIRE
C                 RANG DE LA MATRICE A
C IN/OUT : RV1  : REAL*8 , VECTEUR DE DIMENSION N
C                 VECTEUR DE TRAVAIL
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER     NM, M, N, RG
      REAL*8      W(N), U(NM,N), V(NM,N), EPS, RV1(N)
      LOGICAL     MATU, MATV
C
C VARIABLES LOCALES
C -----------------
      INTEGER     I, J, JMAX, RGMAX
      REAL*8      WMAX
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   ON REORDONNE LES VALEURS SINGULIERES PAR MODULE DECROISSANT
C     SIMULTANEMENT ON EFFECTUE LES PERMUTATIONS ADEQUATES DES COLONNES
C     DES MATRICES U ET V SI ELLES ONT ETE CALCULEES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF ( N.GT.1 ) THEN
         DO 10 J = 1, N-1
            JMAX = J
            WMAX = W(J)
            DO 20 I = J+1, N
               IF ( W(I).GT.WMAX ) THEN
                  JMAX = I
                  WMAX = W(I)
               ENDIF
  20        CONTINUE
            IF ( JMAX.NE.J ) THEN
               W(JMAX) = W(J)
               W(J) = WMAX
               IF ( MATU ) CALL R8SWAP(M,U(1,J),1,U(1,JMAX),1)
               IF ( MATV ) CALL R8SWAP(N,V(1,J),1,V(1,JMAX),1)
            ENDIF
  10     CONTINUE
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   DETERMINATION DU RANG DE LA MATRICE A
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF ( W(1).EQ.0.0D0 ) THEN
         RG = 0
      ELSE
         RGMAX = MIN(M,N)
         IF ( RGMAX.GT.1 ) THEN
            CALL R8COPY(RGMAX,W(1),1,RV1(1),1)
            CALL R8SCAL(RGMAX,1.0D0/RV1(1),RV1(1),1)
            DO 30 J = 2, RGMAX
               IF ( RV1(J).LT.EPS ) GO TO 40
  30        CONTINUE
  40        CONTINUE
            RG = J - 1
         ELSE
            RG = 1
         ENDIF
      ENDIF
C
C --- FIN DE POSSVD.
      END
