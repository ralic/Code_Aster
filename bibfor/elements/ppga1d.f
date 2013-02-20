      SUBROUTINE PPGA1D(NDIM,NNO,NPG,POIDS,VFF,DFF,GEOM,PG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/02/2013   AUTEUR DESOZA T.DESOZA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER NDIM,NNO,NPG
      REAL*8  POIDS(NPG),VFF(NNO,NPG),DFF(NNO,NPG),GEOM(NDIM,NNO)
      REAL*8  PG(NDIM+1,NPG)
C ----------------------------------------------------------------------
C  POSITION ET POIDS DES POINTS DE GAUSS POUR ELEMENTS 1D
C  *           *                   **                  **
C ----------------------------------------------------------------------
C IN  NDIM    DIMENSION DE L'ESPACE
C IN  NNO     NOMBRE DE NOEUDS
C IN  NPG     NOMBRE DE POINTS DE GAUSS
C IN  POIDS   POIDS DES POINTS DE GAUSS DE L'ELEMENT DE REFERENCE
C IN  VFF     VALEUR DES FONCTIONS DE FORME
C IN  GEOM    COORDONNEES DES NOEUDS
C OUT PG      COORDONNEES DES POINTS DE GAUSS + POIDS
C ----------------------------------------------------------------------
C
      INTEGER G,I,J
      REAL*8  DXDK,DYDK,DZDK
      REAL*8  JAC
C
C ----------------------------------------------------------------------

C     1. CALCUL DES COORDONNEES DES POINTS DE GAUSS
C     =============================================

      DO 10 G = 1,NPG
         DO 20 I = 1,NDIM+1
            PG(I,G) = 0.D0
 20      CONTINUE
 10   CONTINUE

      DO 30 G = 1,NPG
         DO 40 I = 1,NDIM
            DO 50 J = 1,NNO
               PG(I,G) = PG(I,G) + GEOM(I,J)*VFF(J,G)
 50         CONTINUE
 40      CONTINUE
 30   CONTINUE

      IF (NNO.EQ.1) GOTO 9999

C     2. CALCUL DU POIDS
C     ==================
C
      DO 100 G = 1,NPG
         DXDK = 0.D0
         DYDK = 0.D0
         DZDK = 0.D0
C        COMPOSANTES DU VECTEUR TANGENT
         DO 90 J = 1,NNO
            DXDK = DXDK + GEOM(1,J)*DFF(J,G)
            DYDK = DYDK + GEOM(2,J)*DFF(J,G)
            IF (NDIM.EQ.3) DZDK = DZDK + GEOM(3,J)*DFF(J,G)
90       CONTINUE
C        JACOBIEN 1D == DERIVEE DE L'ABSCISSE CURVILIGNE
         JAC = SQRT(DXDK**2+DYDK**2+DZDK**2)

         PG(NDIM+1,G) = JAC*POIDS(G)
 100  CONTINUE

9999  CONTINUE

      END
