      SUBROUTINE PRMAMA(IPROD,AMAT,NA,NA1,NA2,BMAT,NB,NB1,NB2,
     &                        CMAT,NC,NC1,NC2,IER)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/05/2000   AUTEUR KXBADNG T.KESTENS 
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
C DESCRIPTION : PRODUITS DE MATRICES PLEINES RECTANGULAIRES
C -----------                                                  T
C               IPROD = 1 : C = A * B     IPROD = 2 : C = A * B
C
C                                T                         T   T
C               IPROD = 3 : C = A * B     IPROD = 4 : C = A * B
C
C               APPELANTS : CALCMD, PRBRD1, PRBRD2, PRLGMA, PROJMD
C
C IN  : IPROD : INTEGER , SCALAIRE
C               INDICATEUR DU PRODUIT A EFFECTUER
C IN  : AMAT  : REAL*8 , TABLEAU DE DIMENSION (NA,*)
C               MATRICE A
C IN  : NA    : INTEGER , SCALAIRE , PARAMETRE DIMENSIONNANT
C IN  : NA1   : INTEGER , SCALAIRE
C               NOMBRE DE LIGNES DE LA MATRICE A
C IN  : NA2   : INTEGER , SCALAIRE
C               NOMBRE DE COLONNES DE LA MATRICE A
C IN  : BMAT  : REAL*8 , TABLEAU DE DIMENSION (NB,*)
C               MATRICE B
C IN  : NB    : INTEGER , SCALAIRE , PARAMETRE DIMENSIONNANT
C IN  : NB1   : INTEGER , SCALAIRE
C               NOMBRE DE LIGNES DE LA MATRICE B
C IN  : NB2   : INTEGER , SCALAIRE
C               NOMBRE DE COLONNES DE LA MATRICE B
C OUT : CMAT  : REAL*8 , TABLEAU DE DIMENSION (NC,*)
C               MATRICE C
C IN  : NC    : INTEGER , SCALAIRE , PARAMETRE DIMENSIONNANT
C IN  : NC1   : INTEGER , SCALAIRE
C               NOMBRE DE LIGNES DE LA MATRICE C
C IN  : NC2   : INTEGER , SCALAIRE
C               NOMBRE DE COLONNES DE LA MATRICE C
C OUT : IER   : INTEGER , SCALAIRE , CODE RETOUR
C               IER = 0  OK
C               IER = 1  DIMENSIONS DE A ET B INCOMPATIBLES POUR
C                        LE PRODUIT DEMANDE
C               IER = 2  DIMENSIONS DE C INCOMPATIBLES AVEC CELLES DE
C                        A ET B POUR LE PRODUIT DEMANDE
C               IER = 3  LES DEUX ERREURS ONT ETE CONSTATEES
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER    IPROD, NA, NA1, NA2, NB, NB1, NB2, NC, NC1, NC2, IER
      REAL*8     AMAT(NA,*), BMAT(NB,*), CMAT(NC,*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER    I, J, K
      REAL*8     CTEMP, ZERO
C
C FONCTIONS EXTERNES
C ------------------
      REAL*8     R8DOT
C     EXTERNAL   R8DOT
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      IER  = 0
      ZERO = 0.0D0
      GO TO (100,200,300,400) IPROD
C
C---- C = A * B
C
 100  CONTINUE
      IF ( NA2 .NE. NB1 ) IER = 1
      IF ( NC1 .NE. NA1 .OR. NC2 .NE. NB2 ) IER = IER + 2
      IF ( IER .NE. 0 ) GO TO 999
C
      DO 110 J = 1,NB2
         DO 120 I = 1,NA1
            CTEMP = ZERO
            DO 130 K = 1,NB1
               CTEMP = CTEMP + AMAT(I,K) * BMAT(K,J)
 130        CONTINUE
            CMAT(I,J) = CTEMP
 120     CONTINUE
 110  CONTINUE
C
      GO TO 999
C
C              T
C---- C = A * B
C
 200  CONTINUE
      IF ( NA2 .NE. NB2 ) IER = 1
      IF ( NC1 .NE. NA1 .OR. NC2 .NE. NB1 ) IER = IER + 2
      IF ( IER .NE. 0 ) GO TO 999
C
      DO 210 J = 1,NB1
         DO 220 I = 1,NA1
            CTEMP = ZERO
            DO 230 K = 1,NB2
               CTEMP = CTEMP + AMAT(I,K) * BMAT(J,K)
 230        CONTINUE
            CMAT(I,J) = CTEMP
 220     CONTINUE
 210  CONTINUE
C
      GO TO 999
C
C          T
C---- C = A * B
C
 300  CONTINUE
      IF ( NA1 .NE. NB1 ) IER = 1
      IF ( NC1 .NE. NA2 .OR. NC2 .NE. NB2 ) IER = IER + 2
      IF ( IER .NE. 0 ) GO TO 999
C
      DO 310 J = 1,NB2
         DO 320 I = 1,NA2
            CMAT(I,J) = R8DOT(NB1,AMAT(1,I),1,BMAT(1,J),1)
 320     CONTINUE
 310  CONTINUE
C
      GO TO 999
C
C          T   T
C---- C = A * B
C
 400  CONTINUE
      IF ( NA1 .NE. NB2 ) IER = 1
      IF ( NC1 .NE. NA2 .OR. NC2 .NE. NB1 ) IER = IER + 2
      IF ( IER .NE. 0 ) GO TO 999
C
      DO 410 J = 1,NB1
         DO 420 I = 1,NA2
            CTEMP = ZERO
            DO 430 K = 1,NB2
               CTEMP = CTEMP + AMAT(K,I) * BMAT(J,K)
 430        CONTINUE
            CMAT(I,J) = CTEMP
 420     CONTINUE
 410  CONTINUE
C
 999  CONTINUE
C
C --- FIN DE PRMAMA.
      END
