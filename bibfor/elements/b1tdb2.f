      SUBROUTINE  B1TDB2( B1 , B2 ,D,JACOB,NBSIG,NBINCO,BTDB)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/12/1999   AUTEUR SABJLMA P.LATRUBESSE 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C       B1TDB2  -- CALCUL DU PRODUIT B1T*D*B2 DONNANT LA MATRICE
C                  DE RIGIDITE ELEMENTAIRE EN FAISANT LE PRODUIT  
C                  MATRICIEL MULTIPLIE PAR LE SCALAIRE JACOBIEN*POIDS 
C
C   ARGUMENT        E/S  TYPE         ROLE
C B1(NBSIG,NBINCO)  IN     R        MATRICE (B1) CALCULEE AU POINT
C B2(NBSIG,NBINCO)  IN     R        MATRICE (B2) CALCULEE AU POINT
C                                   D'INTEGRATION COURANT ET RELIANT
C                                   LES DEFORMATIONS DU PREMIER ORDRE
C                                   AUX DEPLACEMENTS
C D(NBSIG,NBSIG)    IN     R        MATRICE DE HOOKE DANS LE REPERE
C                                   GLOBAL
C JACOB             IN     R        PRODUIT JACOBIEN*POIDS AU POINT
C                                   D'INTEGRATION COURANT
C NBSIG             IN     I        NOMBRE DE CONTRAINTES ASSOCIE A
C                                   L'ELEMENT
C NBINCO            IN     I        NOMBRE D'INCONNUES SUR L'ELEMENT
C BTDB(NBINCO,NBINCO) OUT  R        MATRICE ELEMENTAIRE DE RIGIDITE
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      REAL*8 B1 ( NBSIG , NBINCO ) 
      REAL*8 B2 ( NBSIG , NBINCO ) 
      REAL*8                 D(NBSIG,NBSIG),JACOB,BTDB(NBINCO,NBINCO)
C -----  VARIABLES LOCALES
      REAL*8 TAB1( 9 ), TAB2( 9 )
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      ZERO = 0.0D0
C
      DO 10 I = 1, NBINCO
         DO 20 J = 1, NBSIG
            TAB1(J) = JACOB * B1 ( J , I )
 20   CONTINUE
C
         DO 30 J1 = 1, NBSIG
            S = ZERO
            DO 40 J2 = 1, NBSIG
               S = S + TAB1(J2)*D(J1,J2)
 40         CONTINUE
            TAB2(J1) = S
 30      CONTINUE
C
         DO 50 J1 = 1, NBINCO
            S = ZERO
            DO 60 J2 = 1, NBSIG
               S = S + B2 ( J2 , J1 ) * TAB2(J2)
 60         CONTINUE
C
            BTDB(I,J1) = BTDB(I,J1) + S
C
 50      CONTINUE
 10   CONTINUE
C
C.============================ FIN DE LA ROUTINE ======================
      END
