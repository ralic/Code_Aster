      SUBROUTINE SOMMET(PANNOE,DIME,PAN,NSOM,CSOM)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ----------------------------------------------------------------------
C                    SOMMETS DU CONVEXE ENGLOBANT
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C REAL*8   PANNOE(*)   : PANS TOUCHANT LES NOEUDS PRINCIPAUX DE  
C                        LA MAILLE (CF PANNO)
C INTEGER  DIME        : DIMENSION DE L'ESPACE
C REAL*8   PAN(*)      : EQUATION DES PANS DU CONVEXE (CF BOITE)
C INTEGER  NSOM        : NOMBRE DE NOEUDS PRINCIPAUX DE LA MAILLE
C
C VARIABLES DE SORITE
C REAL*8    CSOM(*)    : COORD. SOMMETS DU CONVEXE (X1,Y1,[Z1],X2,...)
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER DIME,NSOM,PANNOE(DIME,*),I,J,K,L
      REAL*8  PAN(DIME+2,*),CSOM(DIME,*),A(3,3),ZERO 
      LOGICAL FAUX

      ZERO = 0.D0
      FAUX = .FALSE.

      DO 10 I = 1, NSOM

        DO 20 J = 1, DIME

          L = PANNOE(J,I)

          DO 30 K = 1, DIME
            A(K,J) = PAN(K,L)
 30       CONTINUE

          CSOM(J,I) = -PAN(DIME+1,L)

 20     CONTINUE         

        CALL MGAUST(A,CSOM(1,I),3,DIME,1,ZERO,FAUX)

 10   CONTINUE

      END
