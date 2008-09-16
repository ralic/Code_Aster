      FUNCTION DVOLU3(COORD,NORM,COORD1)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 16/09/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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

C**********************************************************
C              BUT DE CETTE ROUTINE :                     *
C CALCULER LE VOLUME DE L INTERSECTION CYLINDRE-TETRAEDRE *
C**********************************************************

C IN   COORD  : COORDONNEES DES NOEUDS DU TETRA
C               ET DES INTERSECTIONS AVEC LE CYLINDRE
C IN   NORM   : POSITION DES NOEUDS PAR RAPPORT AU CYLINDRE
C IN   COORD1 : COORDONNEES DE O1
C OUT  DVOLU3 : VOLUME DE L INTERSECTION

      IMPLICIT NONE

C DECLARATION GLOBALE

      INTEGER NORM(2,4)
      REAL*8 COORD(3,12),COORD1(3),DVOLU3

C DECLARATION LOCALE

      INTEGER I,J,K,L,M,E
      REAL*8  VOL3,VOL4,XIO1,YIO1,ZIO1,DIO1
      LOGICAL LNOEU

C 1 - RECHERCHE DES DEUX POINTS INTERNES
C     RQ : 2 POINTS DEDANS
C          3 INTERSECTIONS
C          0 OU 1 POINT HORS PLAN

      I = 0
      DO 10 K = 1,4
        IF ( NORM(1,K).EQ.1 .AND. I.GT.0 ) J = K
        IF ( NORM(1,K).EQ.1 .AND. I.EQ.0 ) I = K
 10   CONTINUE

C 2 - RECHERCHE DU POINT HORS PLAN

      DO 20 K = 1,4
        IF ( NORM(2,K).NE.0 ) E = K
 20   CONTINUE

C 3 - NOEU1 ET NOEU2 SONT CONFONDUS AVEC LES 2 SOMMETS I ET J
C RECHERCHE DE LA CORRESPONDANCE SI LNOEU ALORS I=NOEU1 SINON I=NOEU2

      XIO1 = COORD(1,I) - COORD1(1)
      YIO1 = COORD(2,I) - COORD1(2)
      ZIO1 = COORD(3,I) - COORD1(3)
      DIO1 =( XIO1**2 + YIO1**2 + ZIO1**2 )
      IF ( DIO1.LE.1.0D-6 ) THEN
        IF ( NORM(2,E).EQ.1 ) THEN
          LNOEU = .TRUE.
        ELSE
          LNOEU = .FALSE.
        ENDIF
      ELSE
        IF ( NORM(2,E).EQ.-1 ) THEN
          LNOEU = .TRUE.
        ELSE
          LNOEU = .FALSE.
        ENDIF
      ENDIF

C 4 - DETERMINATION DANS LE TABLEAU DES POSITIONS DES INTERSECTIONS

      IF ( I.EQ.1.AND.J.EQ.2 ) THEN
        IF ( LNOEU .AND. E.EQ.4 ) THEN
          K = 8
          L = 6
          M = 9
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.3 ) THEN
          K = 6
          L = 9
          M = 7
        ELSE IF ( LNOEU .AND. E.EQ.3 ) THEN
          K = 8
          L = 7
          M = 9
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.4 ) THEN
          K = 6
          L = 8
          M = 7
        ENDIF
      ELSE IF ( I.EQ.1.AND.J.EQ.3 ) THEN
        IF ( LNOEU .AND. E.EQ.2 ) THEN
          K = 10
          L = 7
          M = 8
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.4 ) THEN
          K = 7
          L = 8
          M = 5
        ELSE IF ( LNOEU .AND. E.EQ.4 ) THEN
          K = 10
          L = 5
          M = 8
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.2 ) THEN
          K = 7
          L = 10
          M = 5
        ENDIF
      ELSE IF ( I.EQ.1.AND.J.EQ.4 ) THEN
        IF ( LNOEU .AND. E.EQ.3 ) THEN
          K = 9
          L = 5
          M = 10
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.2 ) THEN
          K = 5
          L = 10
          M = 6
        ELSE IF ( LNOEU .AND. E.EQ.2 ) THEN
          K = 9
          L = 6
          M = 10
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.3 ) THEN
          K = 5
          L = 9
          M = 6
        ENDIF
      ELSE IF ( I.EQ.2.AND.J.EQ.3 ) THEN
        IF ( LNOEU .AND. E.EQ.4 ) THEN
          K = 6
          L = 5
          M = 10
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.1 ) THEN
          K = 5
          L = 10
          M = 9
        ELSE IF ( LNOEU .AND. E.EQ.1 ) THEN
          K = 6
          L = 9
          M = 10
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.4 ) THEN
          K = 5
          L = 6
          M = 9
        ENDIF
      ELSE IF ( I.EQ.2.AND.J.EQ.4 ) THEN
        IF ( LNOEU .AND. E.EQ.1 ) THEN
          K = 10
          L = 8
          M = 7
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.3 ) THEN
          K = 5
          L = 7
          M = 8
        ELSE IF ( LNOEU .AND. E.EQ.3 ) THEN
          K = 10
          L = 5
          M = 7
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.1 ) THEN
          K = 8
          L = 10
          M = 5
        ENDIF
      ELSE IF ( I.EQ.3.AND.J.EQ.4 ) THEN
        IF ( LNOEU .AND. E.EQ.2 ) THEN
          K = 7
          L = 6
          M = 9
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.1 ) THEN
          K = 6
          L = 9
          M = 8
        ELSE IF ( LNOEU .AND. E.EQ.1 ) THEN
          K = 7
          L = 8
          M = 9
        ELSE IF ( .NOT.LNOEU .AND. E.EQ.2 ) THEN
          K = 6
          L = 7
          M = 8
        ENDIF
      ENDIF

C 5 - CALCUL DU VOLUME

      VOL3 =(COORD(1,L)-COORD(1,I))*
     &     ((COORD(2,J)-COORD(2,I))*(COORD(3,K)-COORD(3,I)) -
     &      (COORD(3,J)-COORD(3,I))*(COORD(2,K)-COORD(2,I)) )
      VOL3 = VOL3 +(COORD(2,L)-COORD(2,I))*
     &     ((COORD(1,K)-COORD(1,I))*(COORD(3,J)-COORD(3,I)) -
     &      (COORD(3,K)-COORD(3,I))*(COORD(1,J)-COORD(1,I)) )
      VOL3 = VOL3 +(COORD(3,L)-COORD(3,I))*
     &     ((COORD(1,J)-COORD(1,I))*(COORD(2,K)-COORD(2,I)) -
     &      (COORD(2,J)-COORD(2,I))*(COORD(1,K)-COORD(1,I)) )

      IF ( ABS(VOL3).LT.1.0D-10 ) VOL3 = 0.0D0
      IF ( VOL3.LT.0.D0 ) THEN
        VOL3 = - VOL3
      ENDIF
      VOL4 =(COORD(1,M)-COORD(1,I))*
     &     ((COORD(2,J)-COORD(2,I))*(COORD(3,L)-COORD(3,I)) -
     &      (COORD(3,J)-COORD(3,I))*(COORD(2,L)-COORD(2,I)) )
      VOL4 = VOL4 +(COORD(2,M)-COORD(2,I))*
     &     ((COORD(1,L)-COORD(1,I))*(COORD(3,J)-COORD(3,I)) -
     &      (COORD(3,L)-COORD(3,I))*(COORD(1,J)-COORD(1,I)) )
      VOL4 = VOL4 +(COORD(3,M)-COORD(3,I))*
     &     ((COORD(1,J)-COORD(1,I))*(COORD(2,L)-COORD(2,I)) -
     &      (COORD(2,J)-COORD(2,I))*(COORD(1,L)-COORD(1,I)) )

      IF ( ABS(VOL4).LT.1.0D-10 ) VOL4 = 0.0D0
      IF ( VOL4.LT.0.D0 ) THEN
        VOL4 = - VOL4
      ENDIF
      DVOLU3 = VOL3 + VOL4
      DVOLU3 = DVOLU3 / 6.D0
      IF (DVOLU3.GT.1.D+6) THEN
        CALL U2MESS('A','PREPOST_28')
      ENDIF

      END
