      SUBROUTINE PIPEMI(NPG, INDEX, A, X)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/05/2000   AUTEUR VABHHTS J.PELLET 
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
      INTEGER  NPG, INDEX(2,0:NPG)
      REAL*8   A(0:1,NPG), X


C ----------------------------------------------------------------------
C      MINIMISATION P(X) POUR LE PILOTAGE PAR PREDICTION ELASTIQUE
C ----------------------------------------------------------------------
C IN       NPG     I   NOMBRE DE POINTS DE GAUSS
C MEM      INDEX   I   ZONE MEMOIRE 2*(NPG+1)
C IN       A      R8   COEFFICIENTS DES DROITES
C VAR      X      R8   IN:  ESTIMATION INITIALE
C                      OUT: MINIMUM DE LA FONCTION
C ----------------------------------------------------------------------

      LOGICAL INACTI
      INTEGER  G, GMIN, GMAX
      REAL*8  Y, P, INFINI, YMAX, RMIN, XP, XMIN
      REAL*8  R8MAEM
C ----------------------------------------------------------------------



C -- INITIALISATION DES DROITES ACTIVES : LISTE DOUBLE

      DO 10 G = 0,NPG
        INDEX(1,G) = G+1
        INDEX(2,G) = G-1
 10   CONTINUE
      INDEX(1,NPG) = 0
      INDEX(2,0  ) = NPG

      INFINI = R8MAEM()



C ======================================================================
C                       RECHERCHE DU PREMIER POINT
C ======================================================================


C -- ORDONNEE DU POINT INITIAL

      YMAX = A(1,1)*X + A(0,1)
      GMAX = 1
      DO 20 G = 2,NPG
        Y = A(1,G)*X + A(0,G)
        IF (Y.GT.YMAX) THEN
          YMAX = Y
          GMAX = G
        END IF
 20   CONTINUE

C -- LA DROITE N'EST PLUS ACTIVE

      INDEX(1,INDEX(2,GMAX)) = INDEX(1,GMAX)
      INDEX(2,INDEX(1,GMAX)) = INDEX(2,GMAX)


C -- DIRECTION DE DESCENTE

      P = A(1,GMAX)
      Y = YMAX
      IF (P.EQ.0.D0) GOTO 9999



C ======================================================================
C             PARCOURS DES SOMMETS DU CONVEXE VERS LE BAS
C ======================================================================


 30   CONTINUE


C -- PARCOURS DES DROITES ACTIVES

        RMIN = INFINI
        G    = INDEX(1,0)
        IF (G .EQ. 0) CALL UTMESS('F','PIPEMI',
     &                'AUCUNE DROITE ACTIVE (DVLP)')

 40     CONTINUE
          INACTI = .FALSE.
          IF (ABS(P-A(1,G)) .LT. ABS((A(0,G)+P*X-Y))/INFINI) THEN
            INACTI = .TRUE.
          ELSE
            XP = (A(0,G)+P*X-Y)/(P-A(1,G))
            IF ( (XP-X)*P .GT. 0) INACTI = .TRUE.
          END IF

C        LA DROITE CONSIDEREE N'EST PLUS ACTIVE
          IF (INACTI) THEN
            INDEX(1,INDEX(2,G)) = INDEX(1,G)
            INDEX(2,INDEX(1,G)) = INDEX(2,G)

C        LA DROITE ACTIVE REALISE-T-ELLE L'AVANCEE MINIMALE
          ELSE
            IF ( ABS(XP-X) .LT. RMIN) THEN
              RMIN = ABS(XP-X)
              XMIN = XP
              GMIN = G
            END IF
          END IF
          G = INDEX(1,G)
        IF (G.NE.0) GOTO 40


C -- EXAMEN DU NOUVEAU SOMMET

        X = XMIN
        IF (P*A(1,GMIN) .LE. 0) GOTO 9999

        Y = A(1,GMIN)*X + A(0,GMIN)
        P = A(1,GMIN)


C -- LA DROITE CORRESPONDANTE DEVIENT INACTIVE

        INDEX(1,INDEX(2,GMIN)) = INDEX(1,GMIN)
        INDEX(2,INDEX(1,GMIN)) = INDEX(2,GMIN)


      GOTO 30


 9999 CONTINUE
      END
