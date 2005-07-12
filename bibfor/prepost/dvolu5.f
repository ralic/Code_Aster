      FUNCTION DVOLU5(NUMELE,COORD,NORM,VOLT,COORD1,COORD2)
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

C IN   NUMELE : NUMERO DE L ELEMENT
C IN   COORD  : COORDONNEES DES NOEUDS DU TETRA
C               ET DES INTERSECTIONS AVEC LE CYLINDRE
C IN   NORM   : POSITION DES NOEUDS PAR RAPPORT AU CYLINDRE 
C IN   VOLT   : VOLUME DES ELEMENTS
C IN   COORD1 : COORDONNEE DE NOEU1
C IN   COORD2 : COORDONNEE DE NOEU2 
C OUT  DVOLU5 : VOLUME DE L INTERSECTION

      IMPLICIT NONE 

C DECLARATION GLOBALE

      INTEGER NUMELE,NORM(2,4)
      REAL*8 COORD(3,12),VOLT(*),COORD1(3),COORD2(3),DVOLU5

C DECLARATION LOCALE

      INTEGER A,B,C,D,E,F,G,H,I,J,K,EX
      REAL*8  VOL3,VOL4,XO1A,YO1A,ZO1A,XO2A,YO2A,ZO2A,DO1A,DO2A
      LOGICAL LNOEU

C 1 - RECHERCHE DES DEUX POINTS INTERNES
C     RQ : 2 POINTS DEDANS 
C          4 INTERSECTIONS
C          2 POINTS HORS PLAN
C          3 OU 4 POINTS INFERIEURS A R

      A = 0
      DO 10 K = 1,4
        IF ( NORM(1,K).EQ.1 .AND. A.GT.0 ) B = K
        IF ( NORM(1,K).EQ.1 .AND. A.EQ.0 ) A = K
 10   CONTINUE

C 2 - RECHERCHE DU PREMIER POINT HORS PLAN
      
      EX = 0
      DO 20 K = 1,4
        IF ( NORM(2,K).NE.0 .AND. EX.EQ.0 ) EX = K
 20   CONTINUE

C 3 - A EST-IL DU COTE DE 01 OU 02
      
      XO1A = COORD(1,A) - COORD1(1)
      YO1A = COORD(2,A) - COORD1(2)
      ZO1A = COORD(3,A) - COORD1(3)
      XO2A = COORD(1,A) - COORD2(1)
      YO2A = COORD(2,A) - COORD2(2)
      ZO2A = COORD(3,A) - COORD2(3)
      DO1A =( XO1A**2 + YO1A**2 + ZO1A**2 )
      DO2A =( XO2A**2 + YO2A**2 + ZO2A**2 )
      IF ( DO1A.LT.DO2A ) THEN
        LNOEU = .TRUE.
      ELSE
        LNOEU = .TRUE.
      ENDIF
 
C 4 - DETERMINATION DANS LE TABLEAU DES POSITIONS DES INTERSECTIONS
      
      IF ( A.EQ.1.AND.B.EQ.2 ) THEN
        IF ( LNOEU .AND. NORM(2,EX).EQ.1 .OR.
     &     .NOT.LNOEU .AND. NORM(2,EX).NE.1 ) THEN
          C = 4
          D = 3
          E = 6
          F = 8
          G = 11
          H = 9
          I = 7
          J = 12
        ELSE
          C = 3
          D = 4
          E = 7
          F = 9
          G = 12
          H = 8
          I = 6
          J = 11
        ENDIF
      ELSE IF ( A.EQ.1.AND.B.EQ.3 ) THEN
        IF ( LNOEU .AND. NORM(2,EX).EQ.1 .OR.
     &     .NOT.LNOEU .AND. NORM(2,EX).NE.1 ) THEN
          C = 4
          D = 2
          E = 5
          F = 8
          G = 11
          H = 10
          I = 7
          J = 12
        ELSE
          C = 2
          D = 4
          E = 7
          F = 10
          G = 12
          H = 8
          I = 5
          J = 11
        ENDIF
      ELSE IF ( A.EQ.1.AND.B.EQ.4 ) THEN
        IF ( LNOEU .AND. NORM(2,EX).EQ.1 .OR.
     &     .NOT.LNOEU .AND. NORM(2,EX).NE.1 ) THEN
          C = 3
          D = 2
          E = 5
          F = 9
          G = 11
          H = 10
          I = 6
          J = 12
        ELSE
          C = 2
          D = 3
          E = 6
          F = 10
          G = 12
          H = 9
          I = 5
          J = 11
        ENDIF
      ELSE IF ( A.EQ.2.AND.B.EQ.3 ) THEN
        IF ( LNOEU .AND. NORM(2,EX).EQ.1 .OR.
     &     .NOT.LNOEU .AND. NORM(2,EX).NE.1 ) THEN
          C = 4
          D = 1
          E = 5
          F = 6
          G = 11
          H = 10
          I = 9
          J = 12
        ELSE
          C = 1
          D = 4
          E = 9
          F = 10
          G = 12
          H = 6
          I = 5
          J = 11
        ENDIF
      ELSE IF ( A.EQ.2.AND.B.EQ.4 ) THEN
        IF ( LNOEU .AND. NORM(2,EX).EQ.1 .OR.
     &       .NOT.LNOEU .AND. NORM(2,EX).NE.1 ) THEN
          C = 3
          D = 1
          E = 5
          F = 7
          G = 11
          H = 10
          I = 8
          J = 12
        ELSE
          C = 1
          D = 3
          E = 8
          F = 10
          G = 12
          H = 7
          I = 5
          J = 11
        ENDIF
      ELSE IF ( A.EQ.3.AND.B.EQ.4 ) THEN
        IF ( LNOEU .AND. NORM(2,EX).EQ.1 .OR.
     &      .NOT.LNOEU .AND. NORM(2,EX).NE.1 ) THEN
          C = 2
          D = 1
          E = 6
          F = 7
          G = 11
          H = 9
          I = 8
          J = 12
        ELSE
          C = 1
          D = 2
          E = 8
          F = 9
          G = 12
          H = 7
          I = 6
          J = 11
        ENDIF
      ENDIF

C 5 - CALCUL DU VOLUME 

      VOL3 =(COORD(1,E)-COORD(1,D))*
     &    ((COORD(2,G)-COORD(2,D))*(COORD(3,F)-COORD(3,D)) -
     &      (COORD(3,G)-COORD(3,D))*(COORD(2,F)-COORD(2,D)) )
      VOL3 = VOL3 +(COORD(2,E)-COORD(2,D))*
     &    ((COORD(1,F)-COORD(1,D))*(COORD(3,G)-COORD(3,D)) -
     &      (COORD(3,F)-COORD(3,D))*(COORD(1,G)-COORD(1,D)) )
      VOL3 = VOL3 +(COORD(3,E)-COORD(3,D))*
     &    ((COORD(1,G)-COORD(1,D))*(COORD(2,F)-COORD(2,D)) -
     &      (COORD(2,G)-COORD(2,D))*(COORD(1,F)-COORD(1,D)) )

      IF ( ABS(VOL3).LT.1.0D-10 ) VOL3 = 0.0D0
      IF ( VOL3.LT.0.D0 ) THEN
        CALL UTMESS('A','DVOLU5','VOLUME NEGATIF' )
        VOL3 = - VOL3
      ENDIF
      VOL3 = VOL3 / 6.D0

      VOL4 =(COORD(1,I)-COORD(1,C))*
     &    ((COORD(2,H)-COORD(2,C))*(COORD(3,J)-COORD(3,C)) -
     &      (COORD(3,H)-COORD(3,C))*(COORD(2,J)-COORD(2,C)) )
      VOL4 = VOL4 +(COORD(2,I)-COORD(2,C))*
     &    ((COORD(1,J)-COORD(1,C))*(COORD(3,H)-COORD(3,C)) -
     &      (COORD(3,J)-COORD(3,C))*(COORD(1,H)-COORD(1,C)) )
      VOL4 = VOL4 +(COORD(3,I)-COORD(3,C))*
     &    ((COORD(1,H)-COORD(1,C))*(COORD(2,J)-COORD(2,C)) -
     &      (COORD(2,H)-COORD(2,C))*(COORD(1,J)-COORD(1,C)) )

      IF ( ABS(VOL4).LT.1.0D-10 ) VOL4 = 0.0D0
      IF ( VOL4.LT.0.D0 ) THEN
        CALL UTMESS('A','DVOLU5','VOLUME NEGATIF' )
        VOL4 = - VOL4
      ENDIF
      VOL4 = VOL4 / 6.D0

      DVOLU5 = VOLT(NUMELE) - VOL3 - VOL4
      IF ( ABS(DVOLU5).LT.1.0D-10 ) DVOLU5 = 0.0D0

      IF ( DVOLU5.LT.0.0D0 ) THEN
        CALL UTMESS('A','DVOLU5','VOLUME NEGATIF' )
        DVOLU5 = - DVOLU5
      ENDIF

      END
