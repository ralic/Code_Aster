      SUBROUTINE NMREP2(NR, R, G, GU, RMIN, BMIN, RMAX, BMAX, POS)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/09/2001   AUTEUR PBBHHPB P.BADEL 
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

      IMPLICIT NONE
      LOGICAL  BMIN, BMAX
      INTEGER  NR, POS
      REAL*8   R(3), G(3), GU, RMIN, RMAX
      
C ----------------------------------------------------------------------
C  CALCUL DU MINIMUM POUR LA RECHERCHE LINEAIRE AVEC PILOTAGE (NMREPL.F)
C
C VAR NR      NOMBRE DE POINTS CONSIDERES
C VAR R       ABSCISSES DES POINTS (R(1) < R(2) < R(3))
C IN  G       ORDONNEES DES POINTS 
C IN  GU      ORDONNEE A CONVERGENCE (GAIN)
C IN  RMIN    ABSCISSE MINIMALE
C VAR BMIN    .TRUE. SI R(1)=RMIN
C IN  RMAX    ABSCISSE MAXIMALE
C VAR BMAX    .TRUE. SI R(3)=RMAX
C OUT POS     POSITION DU NOUVEAU R (1, 2 OU 3)
C ----------------------------------------------------------------------


      REAL*8 A,B,X, PENTE, DG, R8GAEM
      



C -- INTERPOLATION QUADRATIQUE

      IF (NR.EQ.3) THEN
        A = ((G(1)-G(2))/(R(1)-R(2)) - (G(2)-G(3))/(R(2)-R(3)))
     &      / (R(1)-R(3))
        B = (G(1)-G(2))/(R(1)-R(2)) - A*(R(1)+R(2))

        IF (A.GT.0) THEN

C        LE MINIMUM EXISTE
          X = -B/(2*A)

        ELSE

C        LE MINIMUM N'EXISTE PAS : ON EXTRAPOLERA SUR 2 POINTS
          NR = 2
          IF (G(3).LT.G(1)) THEN
            R(1) = R(2)
            G(1) = G(2)
          ELSE
            R(3) = R(2)
            G(3) = G(2)
          END IF
        END IF
      END IF

      
C -- EXTRAPOLATION SUR LA BASE DE DEUX POINTS

      IF (NR.EQ.2) THEN
        PENTE = (G(3) - G(1)) / (R(3) - R(1))
        DG    = GU - G(1)
        
        IF (ABS(PENTE) .LE. ABS(DG)/R8GAEM()) THEN
          X = (R(1)+R(3))/2
        ELSE        
          X = R(1) + DG/PENTE
        END IF
      END IF


C -- TRAITEMENT DES BORNES

      IF (X.GE.RMAX) THEN
        IF (BMAX) THEN
          IF (NR.EQ.3) THEN
            X  = (R(1)+R(2)) / 2
          ELSE
            X  = (R(1)+R(3)) / 2
          END IF          
          NR = 2
        ELSE
          X    = RMAX
        END IF
        BMAX = .TRUE.
      END IF
          
      IF (X.LE.RMIN) THEN
        IF (BMIN) THEN
          IF (NR.EQ.3) THEN
            X  = (R(2)+R(3)) / 2
          ELSE
            X  = (R(1)+R(3)) / 2
          END IF
          NR = 2
        ELSE
          X    = RMIN
        END IF
        BMIN = .TRUE.
      END IF


C -- RANGEMENT DES VALEURS

      IF (NR.EQ.2) THEN
        IF (X.LE.R(1)) THEN
          R(2) = R(1)
          G(2) = G(1)
          R(1) = X
          POS  = 1
        ELSE IF (X.GE.R(3)) THEN
          R(2) = R(3)
          G(2) = G(3)
          R(3) = X
          POS  = 3
        ELSE
          R(2) = X
          POS  = 2
        END IF
      END IF

      IF (NR.EQ.3) THEN
        IF (X.LE.R(1)) THEN
          R(3) = R(2)
          G(3) = G(2)
          R(2) = R(1)
          G(2) = G(1)
          R(1) = X
          POS  = 1
          BMAX = .FALSE.
        ELSE IF (X.LE.R(2)) THEN
          R(3) = R(2)
          G(3) = G(2)
          R(2) = X
          POS  = 2
          BMAX = .FALSE.
        ELSE IF (X.LE.R(3)) THEN
          R(1) = R(2)
          G(1) = G(2)
          R(2) = X
          POS  = 2
          BMIN = .FALSE.
        ELSE
          R(1) = R(2)
          G(1) = G(2)
          R(2) = R(3)
          G(2) = G(3)
          R(3) = X
          POS  = 3
          BMIN = .FALSE.
        END IF
      END IF
          
      NR = 3
      END
