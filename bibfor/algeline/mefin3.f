      FUNCTION MEFIN3(NBZ,NBGRP,IMOD,ICYL,JMOD,JCYL,Z,F1,F2,F3,G,H)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER       NBZ,NBGRP,IMOD,ICYL,JMOD,JCYL
      REAL*8        Z(*),F1(NBZ*NBGRP,*),F2(NBZ*NBGRP,*),F3(*),G(*),H(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 10/03/98   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C     CALCUL DE L'INTEGRALE SUR (0,L) DE F3(Z)*F1(Z)*F2''(Z)
C     OU F1 EST LA DEFORMEE DU MODE (IMOD) SUR LE CYLINDRE
C     (ICYL) ET F2 CELLE DU MODE (JMOD) SUR LE CYLINDRE (JCYL)
C     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST, MEFMAT
C ----------------------------------------------------------------------
C     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
C     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
C     DE TUBES SOUS ECOULEMENT AXIAL"
C ----------------------------------------------------------------------
C IN  : NBZ    : NOMBRE DE NOEUDS DE LA DISCRETISATION AXIALE
C IN  : NBGRP  : NOMBRE DE GROUPES D EQUIVALENCE
C IN  : IMOD   : NUMERO DU MODE POUR LA FONCTION F1
C IN  : ICYL   : INDICE DU CYLINDRE POUR LA FONCTION F1
C IN  : JMOD   : NUMERO DU MODE POUR LA FONCTION F2
C IN  : JCYL   : INDICE DU GROUPE DE CYLINDRE POUR LA FONCTION F2
C IN  : Z      : COORDONNEES 'Z' DANS LE REPERE AXIAL DES
C                POINTS DISCRETISES, IDENTIQUES POUR TOUS LES CYLINDRES
C IN  : F1     : PREMIERE FONCTION
C IN  : F2     : DEUXIEME FONCTION
C IN  : F3     : TROISIEME FONCTION
C --  : G      : TABLEAU DE TRAVAIL
C --  : H      : TABLEAU DE TRAVAIL
C OUT : MEFIN3 : INTEGRALE CALCULEE
C ----------------------------------------------------------------------
      INTEGER      I,J
      REAL*8       MEFIN3
C ----------------------------------------------------------------------
C
C
      NBZ1 = NBZ*(ICYL-1)
      NBZ2 = NBZ*(JCYL-1)
C
C     ------------------------------------------------
C     CALCUL DE F"(J) -> G
C     F'(J) -> H : MINIMISATION QUADRATIQUE DES RESTES
C     DES DEVELOPPEMENTS DE TAYLOR DE F(J,N)
C     A GAUCHE ET A DROITE
C     H' -> G : MINIMISATION QUADRATIQUE DES RESTES
C     DES DEVELOPPEMENTS DE TAYLOR DE H(N)
C     A GAUCHE ET A DROITE
C     ------------------------------------------------
C
      H(1) = (F2(NBZ2+2,JMOD)-F2(NBZ2+1,JMOD))/(Z(2)-Z(1))
C
      DO 1 N = 2,NBZ-1
         H(N) = ((F2(N+NBZ2+1,JMOD)-F2(N+NBZ2,JMOD))*(Z(N+1)-Z(N))
     &        +(F2(N+NBZ2-1,JMOD)-F2(N+NBZ2,JMOD))*(Z(N-1)-Z(N)))
     &        /((Z(N+1)-Z(N))*(Z(N+1)-Z(N))
     &         +(Z(N-1)-Z(N))*(Z(N-1)-Z(N)))
  1   CONTINUE
C
      H(NBZ) = (F2(NBZ*JCYL,JMOD)-F2(NBZ*JCYL-1,JMOD))/
     &      (Z(NBZ)-Z(NBZ-1))

      G(1) = (H(2)-H(1))/(Z(2)-Z(1))
C
      DO 11 N = 2,NBZ-1
         G(N) = ((H(N+1)-H(N))*(Z(N+1)-Z(N))
     &        +(H(N-1)-H(N))*(Z(N-1)-Z(N)))
     &       /((Z(N+1)-Z(N))*(Z(N+1)-Z(N))
     &        +(Z(N-1)-Z(N))*(Z(N-1)-Z(N)))
  11  CONTINUE
C
      G(NBZ) = (H(NBZ)-H(NBZ-1))/(Z(NBZ)-Z(NBZ-1))
C
      MEFIN3 = 0.D0
C
      DO 2 N = 1,NBZ-1
         MEFIN3 = MEFIN3+0.5D0*(Z(N+1)-Z(N))*
     &          (F3(N+1)*F1(N+NBZ1+1,IMOD)*G(N+1)
     &          +F3(N)*F1(N+NBZ1,IMOD)*G(N))
  2   CONTINUE
C
      END
