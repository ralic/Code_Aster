      SUBROUTINE ANTE2D(ITRIA,XBAR,KSI1,KSI2)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/09/98   AUTEUR CIBHHLV L.VIVAN 
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
C  DESCRIPTION : DETERMINATION DE L'ANTECEDENT DANS L'ELEMENT DE
C  -----------   REFERENCE D'UN POINT APPARTENANT A UN ELEMENT REEL
C                LES ELEMENTS CONSIDERES SONT DES ELEMENTS 2D TRIANGLES
C                OU QUADRANGLES
C
C                APPELANT : RECI2D
C
C  IN     : ITRIA  : INTEGER , SCALAIRE
C                    INDICATEUR DU SOUS-DOMAINE AUQUEL APPARTIENT LE
C                    POINT DE L'ELEMENT REEL :
C                    ITRIA = 1 : TRIANGLE 1-2-3
C                    ITRIA = 2 : TRIANGLE 3-4-1
C  IN     : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES BARYCENTRIQUES DU POINT DE L'ELEMENT
C                    REEL (BARYCENTRE DES SOMMETS DU TRIANGLE 1-2-3
C                    OU 3-4-1)
C  OUT    : KSI1   : REAL*8 , SCALAIRE
C                    PREMIERE COORDONNEE DU POINT ANTECEDENT DANS LE
C                    REPERE ASSOCIE A L'ELEMENT DE REFERENCE
C  OUT    : KSI2   : REAL*8 , SCALAIRE
C                    SECONDE COORDONNEE DU POINT ANTECEDENT DANS LE
C                    REPERE ASSOCIE A L'ELEMENT DE REFERENCE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER       ITRIA
      REAL*8        XBAR(*), KSI1, KSI2
C
C VARIABLES LOCALES
C -----------------
      INTEGER       I, ISOM(3), J
C
      REAL*8        KSI1EL(4), KSI2EL(4)
      DATA          KSI1EL / -1.0D0 , -1.0D0 ,  1.0D0 ,  1.0D0 /
      DATA          KSI2EL /  1.0D0 , -1.0D0 , -1.0D0 ,  1.0D0 /
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   AFFECTATION DES NUMEROS DES SOMMETS
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF ( ITRIA.EQ.1 ) THEN
         ISOM(1) = 1
         ISOM(2) = 2
         ISOM(3) = 3
      ELSE
         ISOM(1) = 3
         ISOM(2) = 4
         ISOM(3) = 1
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   CALCUL DES COORDONNEES DE L'ANTECEDENT DANS L'ELEMENT DE REFERENCE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      KSI1 = 0.0D0
      KSI2 = 0.0D0
      DO 10 I = 1, 3
         J = ISOM(I)
         KSI1 = KSI1 + XBAR(I) * KSI1EL(J)
         KSI2 = KSI2 + XBAR(I) * KSI2EL(J)
  10  CONTINUE
C
C --- FIN DE ANTE2D.
      END
