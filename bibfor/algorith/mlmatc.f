      SUBROUTINE MLMATC ( NI, NK, NJ, A, B, C )
      IMPLICIT   NONE
      INTEGER             NI, NK, NJ
      COMPLEX*16          A(NI,*), B(NK,*), C(NI,*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/97   AUTEUR CIBHHLV L.VIVAN 
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
C     CALCUL COMPLEXE MATRICIEL C = A * B                   
C     MATRICE ORDONNEES PAR COLONNES DESCENDANTES
C ----------------------------------------------------------------------
C IN  : A  : MATRICE A(NI,NK)
C     : B  : MATRICE B(NK,NJ)
C     : C  : MATRICE C(NI,NJ)
C     : NI , NJ ,NK : DIMENSIONS DES MATRICES
C     ------------------------------------------------------------------
      INTEGER       I, J, K
      COMPLEX*16    XCRES
C
      DO 1 I = 1 , NI
         DO 2 J = 1 , NJ
            XCRES = DCMPLX(0.D0,0.D0)
            DO 3 K = 1 , NK
               XCRES = XCRES + A(I,K) * B(K,J)
   3        CONTINUE
            C(I,J) = XCRES
   2     CONTINUE
   1  CONTINUE
C
      END
