      SUBROUTINE PROJTQ(NBCNX,XYZMA,ICNX,X3DP,ITRIA,XBAR,IPROJ)
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
C  DESCRIPTION : TEST D'APPARTENANCE DU POINT PROJETE X3DP(3)
C  -----------   AU DOMAINE GEOMETRIQUE DEFINI PAR UNE MAILLE
C                TRIANGLE OU QUADRANGLE
C
C                POUR CELA, ON CALCULE LES COORDONNEES BARYCENTRIQUES
C                DU POINT PROJETE
C                  - DANS LE TRIANGLE 1-2-3 POUR UNE MAILLE TRIANGLE
C                  - DANS LE TRIANGLE 1-2-3 PUIS SI NECESSAIRE DANS
C                    LE TRIANGLE 3-4-1 POUR UNE MAILLE QUADRANGLE
C                CETTE METHODE EST EXACTE POUR DES MAILLES A BORDS
C                DROITS SUPPORTANT DES ELEMENTS LINEAIRES, APPROCHEE
C                POUR DES MAILLES A BORDS COURBES SUPPORTANT DES
C                ELEMENTS QUADRATIQUES OU AUTRES
C
C                APPELANT : PROJKM
C
C  IN     : NBCNX  : INTEGER , SCALAIRE
C                    NOMBRE DE NOEUDS DE LA MAILLE
C  IN     : XYZMA  : REAL*8 , TABLEAU DE DIMENSIONS (3,NNOMAX)
C                    CONTIENT LES COORDONNEES DES NOEUDS DE LA MAILLE
C  IN     : ICNX   : INTEGER , SCALAIRE
C                    RANG DU NOEUD BETON LE PLUS PROCHE DU POINT PROJETE
C                    DANS LA TABLE DE CONNECTIVITE DE LA MAILLE
C  IN     : X3DP   : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU POINT PROJETE
C  OUT    : ITRIA  : INTEGER , SCALAIRE
C                    SI PROJECTION REUSSIE : INDICATEUR DU SOUS-DOMAINE
C                    AUQUEL APPARTIENT LE POINT PROJETE X3DP(3) :
C                    ITRIA = 1 : TRIANGLE 1-2-3
C                    ITRIA = 2 : TRIANGLE 3-4-1
C  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
C                    SI PROJECTION REUSSIE : COORDONNEES BARYCENTRIQUES
C                    DU POINT PROJETE (BARYCENTRE DES SOMMETS DU
C                    TRIANGLE 1-2-3 OU 3-4-1)
C  OUT    : IPROJ  : INTEGER , SCALAIRE
C                    INDICE DE PROJECTION
C                    IPROJ = -1  PROJECTION NON REUSSIE
C                    IPROJ =  0  LE POINT PROJETE EST A L'INTERIEUR
C                                DE LA MAILLE
C                    IPROJ =  1X LE POINT PROJETE EST SUR UNE FRONTIERE
C                                DE LA MAILLE
C                    IPROJ =  2  LE POINT PROJETE COINCIDE AVEC UN DES
C                                NOEUDS DE LA MAILLE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER       NBCNX, ICNX, ITRIA, IPROJ
      REAL*8        XYZMA(3,*), X3DP(*), XBAR(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER       INO, NBSOM
      REAL*8        D, DX, DY, DZ, EPSG, NRM2
      LOGICAL       NOTLIN
C
      REAL*8        R8NRM2, R8PREM
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   INITIALISATIONS
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      EPSG = 1.0D+08 * R8PREM()
C
      NOTLIN = (NBCNX.GT.4)
      IF ( (NBCNX.EQ.3).OR.(NBCNX.EQ.6) ) THEN
         NBSOM = 3
      ELSE
         NBSOM = 4
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   TEST POUR UNE MAILLE TRIANGLE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF ( NBSOM.EQ.3 ) THEN
C
         ITRIA = 1
C
C....... TEST D'APPARTENANCE AU TRIANGLE 1-2-3, PAR DETERMINATION DES
C....... COORDONNEES BARYCENTRIQUES
C
         CALL TSTBAR(3,XYZMA(1,1),XYZMA(1,2),XYZMA(1,3),XYZMA(1,3),
     &               X3DP(1),XBAR(1),IPROJ)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 3   TESTS POUR UNE MAILLE QUADRANGLE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C 3.1 APPARTENANCE PLUS PROBABLE AU TRIANGLE 3-4-1
C ---
      ELSE IF ( (ICNX.EQ.4).OR.(ICNX.EQ.7).OR.(ICNX.EQ.8) ) THEN
C
         ITRIA = 2
C
C....... TEST D'APPARTENANCE AU TRIANGLE 3-4-1, PAR DETERMINATION DES
C....... COORDONNEES BARYCENTRIQUES
C
         CALL TSTBAR(3,XYZMA(1,3),XYZMA(1,4),XYZMA(1,1),XYZMA(1,1),
     &               X3DP(1),XBAR(1),IPROJ)
C
C....... REAJUSTEMENT DE IPROJ SI APPARTENANCE A UN BORD
C
         IF ( IPROJ.EQ.11 ) THEN
            IPROJ = 13
         ELSE IF ( IPROJ.EQ.12 ) THEN
            IPROJ = 14
         ELSE IF ( IPROJ.EQ.13 ) THEN
            IPROJ = 0
         ENDIF
C
C....... EN CAS D'ECHEC TEST D'APPARTENANCE AU TRIANGLE 1-2-3,
C....... PAR DETERMINATION DES COORDONNEES BARYCENTRIQUES
C
         IF ( IPROJ.LT.0 ) THEN
            ITRIA = 1
            CALL TSTBAR(3,XYZMA(1,1),XYZMA(1,2),XYZMA(1,3),XYZMA(1,3),
     &                  X3DP(1),XBAR(1),IPROJ)
C.......... REAJUSTEMENT DE IPROJ SI PROJECTION SUR LE TROISIEME COTE
C.......... DU TRIANGLE 1-2-3
            IF ( IPROJ.EQ.13 ) IPROJ = 0
         ENDIF
C
C 3.2 APPARTENANCE PLUS PROBABLE AU TRIANGLE 1-2-3
C ---
      ELSE
C
         ITRIA = 1
C
C....... TEST D'APPARTENANCE AU TRIANGLE 1-2-3, PAR DETERMINATION DES
C....... COORDONNEES BARYCENTRIQUES
C
         CALL TSTBAR(3,XYZMA(1,1),XYZMA(1,2),XYZMA(1,3),XYZMA(1,3),
     &               X3DP(1),XBAR(1),IPROJ)
C
C....... REAJUSTEMENT DE IPROJ SI PROJECTION SUR LE TROISIEME COTE
C....... DU TRIANGLE 1-2-3
C
         IF ( IPROJ.EQ.13 ) IPROJ = 0
C
C....... EN CAS D'ECHEC TEST D'APPARTENANCE AU TRIANGLE 3-4-1,
C....... PAR DETERMINATION DES COORDONNEES BARYCENTRIQUES
C
         IF ( IPROJ.LT.0 ) THEN
            ITRIA = 2
            CALL TSTBAR(3,XYZMA(1,3),XYZMA(1,4),XYZMA(1,1),XYZMA(1,1),
     &                  X3DP(1),XBAR(1),IPROJ)
C.......... REAJUSTEMENT DE IPROJ SI APPARTENANCE A UN BORD
            IF ( IPROJ.EQ.11 ) THEN
               IPROJ = 13
            ELSE IF ( IPROJ.EQ.12 ) THEN
               IPROJ = 14
            ELSE IF ( IPROJ.EQ.13 ) THEN
               IPROJ = 0
            ENDIF
         ENDIF
C
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 4   TESTS COMPLEMENTAIRES POUR LES MAILLES A BORDS COURBES SUPPORTANT
C     DES ELEMENTS NON LINEAIRES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF ( NOTLIN ) THEN
C
C 4.1    TEST DE COINCIDENCE AVEC UN NOEUD MILIEU
C ---
         IF ( IPROJ.GT.10 ) THEN
            INO = IPROJ - 10 + NBSOM
            NRM2 = R8NRM2(3,XYZMA(1,INO),1)
            IF ( NRM2.EQ.0.0D0 ) NRM2 = 1.0D0
            DX = XYZMA(1,INO) - X3DP(1)
            DY = XYZMA(2,INO) - X3DP(2)
            DZ = XYZMA(3,INO) - X3DP(3)
            D  = DBLE ( SQRT ( DX*DX + DY*DY + DZ*DZ ) )
            IF ( D/NRM2.LT.EPSG ) IPROJ = 2
         ENDIF
C
C 4.2    TEST DE COINCIDENCE AVEC LE NOEUD CENTRAL POUR UNE MAILLE QUAD9
C ---
         IF ( (IPROJ.EQ.0).AND.(NBCNX.EQ.9) ) THEN
            NRM2 = R8NRM2(3,XYZMA(1,9),1)
            IF ( NRM2.EQ.0.0D0 ) NRM2 = 1.0D0
            DX = XYZMA(1,9) - X3DP(1)
            DY = XYZMA(2,9) - X3DP(2)
            DZ = XYZMA(3,9) - X3DP(3)
            D  = DBLE ( SQRT ( DX*DX + DY*DY + DZ*DZ ) )
            IF ( D/NRM2.LT.EPSG ) IPROJ = 2
         ENDIF
C
      ENDIF
C
C --- FIN DE PROJTQ.
      END
