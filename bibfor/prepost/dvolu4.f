      FUNCTION DVOLU4(COORD,NORM,COORD1)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
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
C OUT  DVOLU4 : VOLUME DE L INTERSECTION

      IMPLICIT NONE 

C DECLARATION GLOBALE

      INTEGER NORM(2,4)
      REAL*8 COORD(3,12),COORD1(3),DVOLU4

C DECLARATION LOCALE

      INTEGER I,J,K,L,E
      REAL*8  XO1I,YO1I,ZO1I,DO1I
      LOGICAL LNOEU

C 1 - RECHERCHE DES DEUX POINTS INTERNES
C     RQ : 2 POINTS DEDANS 
C          2 INTERSECTIONS 
C          2 POINTS HORS PLAN
C          2 POINTS INFERIEURS A R

      I = 0
      DO 10 K = 1,4
        IF ( NORM(1,K).EQ.1.AND.I.GT.0 ) J = K
        IF ( NORM(1,K).EQ.1.AND.I.EQ.0 ) I = K
 10   CONTINUE

C 2 - RECHERCHE DU PREMIER POINT HORS PLAN
      
      E = 0
      DO 20 K = 1,4
        IF ( NORM(2,K).NE.0 .AND. E.EQ.0 ) E = K
 20   CONTINUE

C 3 - NOEU1 ET NOEU2 SONT CONFONDUS AVEC LES 2 SOMMETS I ET J
C RECHERCHE DE LA CORRESPONDANCE SI LNOEU ALORS I=NOEU1 SINON I=NOEU2

      XO1I = COORD(1,I) - COORD1(1)
      YO1I = COORD(2,I) - COORD1(2)
      ZO1I = COORD(3,I) - COORD1(3)
      DO1I =( XO1I**2 + YO1I**2 + ZO1I**2 )
      IF ( DO1I.LE.1.0D-6 ) THEN
        LNOEU = .TRUE.
      ELSE
        LNOEU = .TRUE.
      ENDIF
 
C 4 - ON DETERMINE DANS LE TABLEAU LES POSITIONS DES INTERSECTIONS

      IF ( I.EQ.1.AND.J.EQ.2 ) THEN
        IF ( LNOEU .AND. NORM(2,E).EQ.1 .OR.
     &      .NOT.LNOEU .AND. NORM(2,E).NE.1 ) THEN
          K = 8
          L = 7
        ELSE
          K = 6
          L = 9
        ENDIF
      ELSE IF ( I.EQ.1.AND.J.EQ.3 ) THEN
        IF ( LNOEU .AND. NORM(2,E).EQ.1 .OR.
     &      .NOT.LNOEU .AND. NORM(2,E).NE.1 ) THEN
          K = 7
          L = 8
        ELSE
          K = 10
          L = 5
        ENDIF
      ELSE IF ( I.EQ.1.AND.J.EQ.4 ) THEN
        IF ( LNOEU .AND. NORM(2,E).EQ.1 .OR.
     &       .NOT.LNOEU .AND. NORM(2,E).NE.1 ) THEN
          K = 9
          L = 6
        ELSE
          K = 5
          L = 10
        ENDIF
      ELSE IF ( I.EQ.2.AND.J.EQ.3 ) THEN
        IF ( LNOEU .AND. NORM(2,E).EQ.1 .OR.
     &      .NOT.LNOEU .AND. NORM(2,E).NE.1 ) THEN
          K = 6
          L = 9
        ELSE
          K = 5
          L = 10
        ENDIF
      ELSE IF ( I.EQ.2.AND.J.EQ.4 ) THEN
        IF ( LNOEU .AND. NORM(2,E).EQ.1 .OR.
     &      .NOT.LNOEU .AND. NORM(2,E).NE.1 ) THEN
          K = 8
          L = 7
        ELSE
          K = 10
          L = 5
        ENDIF
      ELSE IF ( I.EQ.3.AND.J.EQ.4 ) THEN
        IF ( LNOEU .AND. NORM(2,E).EQ.1 .OR.
     &      .NOT.LNOEU .AND. NORM(2,E).NE.1 ) THEN
          K = 7
          L = 8
        ELSE
          K = 6
          L = 9
        ENDIF
      ENDIF

C 5 - CALCUL DU VOLUME

      DVOLU4 =(COORD(1,L)-COORD(1,I))*
     &    ((COORD(2,J)-COORD(2,I))*(COORD(3,K)-COORD(3,I)) -
     &      (COORD(3,J)-COORD(3,I))*(COORD(2,K)-COORD(2,I)) )
      DVOLU4 = DVOLU4 +(COORD(2,L)-COORD(2,I))*
     &    ((COORD(1,K)-COORD(1,I))*(COORD(3,J)-COORD(3,I)) -
     &      (COORD(3,K)-COORD(3,I))*(COORD(1,J)-COORD(1,I)) )
      DVOLU4 = DVOLU4 +(COORD(3,L)-COORD(3,I))*
     &    ((COORD(1,J)-COORD(1,I))*(COORD(2,K)-COORD(2,I)) -
     &      (COORD(2,J)-COORD(2,I))*(COORD(1,K)-COORD(1,I)) )

      IF ( DVOLU4.LT.0.D0 ) THEN
        CALL UTMESS('A','DVOLU4','VOLUME NEGATIF' )
        DVOLU4 = - DVOLU4
      ENDIF
      DVOLU4 = DVOLU4 / 6.D0      
      IF ( ABS(DVOLU4).LT.1.0D-10 ) DVOLU4 = 0.0D0     

      END
