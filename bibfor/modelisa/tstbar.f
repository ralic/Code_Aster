      SUBROUTINE TSTBAR(NBSOM,X3D1,X3D2,X3D3,X3D4,X3DP,XBAR,ITEST)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
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
C  DESCRIPTION : TEST D'APPARTENANCE DU POINT X3DP(3) AU DOMAINE
C  -----------   GEOMETRIQUE DEFINI
C                  - PAR LE SEGMENT  DE SOMMETS  X3D1(3), X3D2(3)
C                    SI NBSOM = 2
C                  - PAR LE TRIANGLE DE SOMMETS  X3D1(3), X3D2(3),
C                    X3D3(3) SI NBSOM = 3
C                  - PAR LE TETRAEDRE DE SOMMETS X3D1(3), X3D2(3),
C                    X3D3(3), X3D4(3) SI NBSOM = 4
C
C                CALCUL DES COORDONNEES BARYCENTRIQUES PUIS TEST
C
C                APPELANT : IMMEHX, IMMEPN, IMMEPY, IMMETT,
C                           PROJSG, PROJTQ
C
C  IN     : NBSOM  : INTEGER , SCALAIRE
C                    NOMBRE DE SOMMETS A CONSIDERER
C                    NBSOM=2 : TEST D'APPARTENANCE AU SEGMENT   1-2
C                    NBSOM=3 : TEST D'APPARTENANCE AU TRIANGLE  1-2-3
C                    NBSOM=4 : TEST D'APPARTENANCE AU TETRAEDRE 1-2-3-4
C  IN     : X3D1   : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU PREMIER SOMMET
C  IN     : X3D2   : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU DEUXIEME SOMMET
C  IN     : X3D3   : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU TROISIEME SOMMET
C  IN     : X3D4   : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU QUATRIEME SOMMET
C  IN     : X3DP   : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU POINT CONSIDERE
C
C  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 2 OU 3 OU 4
C                    COORDONNEES BARYCENTRIQUES DU POINT CONSIDERE
C                    POUR UN SEGMENT   : BARYCENTRE DES SOMMETS 1-2
C                    POUR UN TRIANGLE  : BARYCENTRE DES SOMMETS 1-2-3
C                    POUR UN TETRAEDRE : BARYCENTRE DES SOMMETS 1-2-3-4
C
C  OUT    : ITEST  : INTEGER , SCALAIRE
C                    INDICATEUR DE RESULTAT DU TEST
C
C                    ITEST =  -1  LE POINT CONSIDERE N'APPARTIENT PAS
C                                 AU DOMAINE
C
C                    ITEST =   0  LE POINT CONSIDERE EST A L'INTERIEUR
C                                 DU DOMAINE
C
C                    ITEST =   2  LE POINT CONSIDERE COINCIDE AVEC
C                                 UN DES NOEUDS SOMMETS
C
C                    POUR UN DOMAINE TRIANGLE : SI LE POINT CONSIDERE
C                    SE TROUVE SUR UNE ARETE
C                    ITEST =  10 + NUMERO D'ARETE
C
C                    POUR UN DOMAINE TETRAEDRE : SI LE POINT CONSIDERE
C                    SE TROUVE SUR UNE FACE
C                    ITEST = 100 + 10 * NUMERO DE FACE
C                    SI LE POINT CONSIDERE SE TROUVE SUR UNE ARETE
C                    ITEST = 100 + 10 * NUMERO DE FACE + NUMERO D'ARETE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER       NBSOM, ITEST
      REAL*8        X3D1(*), X3D2(*), X3D3(*), X3D4(*), X3DP(*), XBAR(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER       IERR
      REAL*8        A(4,4), B(4), EPS, EPSG, S(3), U(4,3), V(4,3),
     &              WORK(4)
C
      REAL*8        R8PREM
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      ITEST = -1
C
      EPS  = 1.0D+02 * R8PREM()
      EPSG = 1.0D-5 
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   CALCUL DES COORDONNEES BARYCENTRIQUES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 1.1 CONSTRUCTION DE LA MATRICE A ET DU SECOND MEMBRE
C --- DU SYSTEME A RESOUDRE
C
      A(1,1) = X3D1(1) - X3DP(1)
      A(2,1) = X3D1(2) - X3DP(2)
      A(3,1) = X3D1(3) - X3DP(3)
      A(4,1) = 1.0D0
      A(1,2) = X3D2(1) - X3DP(1)
      A(2,2) = X3D2(2) - X3DP(2)
      A(3,2) = X3D2(3) - X3DP(3)
      A(4,2) = 1.0D0
      IF ( NBSOM.GT.2 ) THEN
         A(1,3) = X3D3(1) - X3DP(1)
         A(2,3) = X3D3(2) - X3DP(2)
         A(3,3) = X3D3(3) - X3DP(3)
         A(4,3) = 1.0D0
         IF ( NBSOM.EQ.4 ) THEN
            A(1,4) = X3D4(1) - X3DP(1)
            A(2,4) = X3D4(2) - X3DP(2)
            A(3,4) = X3D4(3) - X3DP(3)
            A(4,4) = 1.0D0
         ENDIF
      ENDIF
C
      B(1) = 0.0D0
      B(2) = 0.0D0
      B(3) = 0.0D0
      B(4) = 1.0D0
C
C 1.2 RESOLUTION DU SYSTEME
C ---
      IF ( NBSOM.LT.4 ) THEN
C
C 1.2.1  SI NBSOM = 2 OU 3 : SYSTEME SUR-CONTRAINT
C .....  RESOLUTION AU SENS DES MOINDRES CARRES PAR DECOMPOSITION AUX
C        VALEURS SINGULIERES DE LA MATRICE A
C
         CALL RSLSVD(4,4,NBSOM,A(1,1),S(1),U(1,1),V(1,1),
     &               1,B(1),EPS,IERR,WORK(1))
         IF ( IERR.NE.0 ) GO TO 9999
         CALL DCOPY(NBSOM,B(1),1,XBAR(1),1)
C
      ELSE
C
C 1.2.2  SI NBSOM = 4 : SYSTEME EQUI-CONTRAINT
C .....  RESOLUTION EXACTE
C
         CALL MTCROG(A(1,1),B(1),4,4,1,XBAR(1),WORK(1),IERR)
         IF ( IERR.NE.0 ) GO TO 9999
C
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   DISCUSSION DE L'APPARTENANCE DU POINT CONSIDERE AU DOMAINE :
C     LES COORDONNEES BARYCENTRIQUES DOIVENT ETRE POSITIVES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF ( (XBAR(1).LT.0.0D0).AND.(DBLE(ABS(XBAR(1))).GT.EPSG) )
     &   GO TO 9999
      IF ( (XBAR(2).LT.0.0D0).AND.(DBLE(ABS(XBAR(2))).GT.EPSG) )
     &   GO TO 9999
      IF ( NBSOM.GT.2 ) THEN
         IF ( (XBAR(3).LT.0.0D0).AND.(DBLE(ABS(XBAR(3))).GT.EPSG) )
     &      GO TO 9999
         IF ( NBSOM.EQ.4 ) THEN
            IF ( (XBAR(4).LT.0.0D0).AND.(DBLE(ABS(XBAR(4))).GT.EPSG) )
     &         GO TO 9999
         ENDIF
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 3   DISCUSSION PRECISE DE LA SITUATION DU POINT CONSIDERE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ITEST = 0
C
C 3.1 DOMAINE SEGMENT
C ---
      IF ( NBSOM.EQ.2 ) THEN
C
         IF ( (DBLE(ABS(XBAR(1))).LT.EPSG).OR.
     &        (DBLE(ABS(XBAR(2))).LT.EPSG) ) ITEST = 2
C
C 3.2 DOMAINE TRIANGLE
C ---
      ELSE IF ( NBSOM.EQ.3 ) THEN
C
C 3.2.1  POINT SUR L'ARETE ( SOMMET 1 , SOMMET 2 )
C .....
         IF ( DBLE(ABS(XBAR(3))).LT.EPSG ) THEN
            ITEST = 11
C.......... COINCIDENCE AVEC LE SOMMET 1 OU LE SOMMET 2
            IF ( (DBLE(ABS(XBAR(2))).LT.EPSG).OR.
     &           (DBLE(ABS(XBAR(1))).LT.EPSG) ) ITEST = 2
C
C 3.2.2  POINT SUR L'ARETE ( SOMMET 2 , SOMMET 3 )
C .....
         ELSE IF ( DBLE(ABS(XBAR(1))).LT.EPSG ) THEN
            ITEST = 12
C.......... COINCIDENCE AVEC LE SOMMET 3
            IF ( DBLE(ABS(XBAR(2))).LT.EPSG ) ITEST = 2
C
C 3.2.3  POINT SUR L'ARETE ( SOMMET 3 , SOMMET 1 )
C .....
         ELSE IF ( DBLE(ABS(XBAR(2))).LT.EPSG ) THEN
            ITEST = 13
         ENDIF
C
C 3.3 DOMAINE TETRAEDRE
C ---
      ELSE
C
C 3.3.1  POINT SUR LA FACE ( SOMMET 1 , SOMMET 2 , SOMMET 3 )
C .....
         IF ( DBLE(ABS(XBAR(4))).LT.EPSG ) THEN
            ITEST = 110
C.......... POINT SUR L'ARETE ( SOMMET 1 , SOMMET 2 )
            IF ( DBLE(ABS(XBAR(3))).LT.EPSG ) THEN
               ITEST = 111
C............. COINCIDENCE AVEC LE SOMMET 1 OU LE SOMMET 2
               IF ( (DBLE(ABS(XBAR(2))).LT.EPSG).OR.
     &              (DBLE(ABS(XBAR(1))).LT.EPSG) ) ITEST = 2
C.......... POINT SUR L'ARETE ( SOMMET 2 , SOMMET 3 )
            ELSE IF ( DBLE(ABS(XBAR(1))).LT.EPSG ) THEN
               ITEST = 112
C............. COINCIDENCE AVEC LE SOMMET 3
               IF ( DBLE(ABS(XBAR(2))).LT.EPSG ) ITEST = 2
C.......... POINT SUR L'ARETE ( SOMMET 3 , SOMMET 1 )
            ELSE IF ( DBLE(ABS(XBAR(2))).LT.EPSG ) THEN
               ITEST = 113
            ENDIF
C
C 3.3.2  POINT SUR LA FACE ( SOMMET 1 , SOMMET 2 , SOMMET 4 )
C .....
         ELSE IF ( DBLE(ABS(XBAR(3))).LT.EPSG ) THEN
            ITEST = 120
C.......... POINT SUR L'ARETE ( SOMMET 2 , SOMMET 4 )
            IF ( DBLE(ABS(XBAR(1))).LT.EPSG ) THEN
               ITEST = 122
C............. COINCIDENCE AVEC LE SOMMET 4
               IF ( DBLE(ABS(XBAR(2))).LT.EPSG ) ITEST = 2
C.......... POINT SUR L'ARETE ( SOMMET 4 , SOMMET 1 )
            ELSE IF ( DBLE(ABS(XBAR(2))).LT.EPSG ) THEN
               ITEST = 123
            ENDIF
C
C 3.3.3  POINT SUR LA FACE ( SOMMET 2 , SOMMET 3 , SOMMET 4 )
C .....
         ELSE IF ( DBLE(ABS(XBAR(1))).LT.EPSG ) THEN
            ITEST = 130
C.......... POINT SUR L'ARETE ( SOMMET 3 , SOMMET 4 )
            IF ( DBLE(ABS(XBAR(2))).LT.EPSG ) ITEST = 132
C
C 3.3.4  POINT SUR LA FACE ( SOMMET 3 , SOMMET 1 , SOMMET 4 )
C .....
         ELSE IF ( DBLE(ABS(XBAR(2))).LT.EPSG ) THEN
            ITEST = 140
         ENDIF
C
      ENDIF
C
9999  CONTINUE
C
C --- FIN DE TSTBAR.
      END
