      SUBROUTINE AJINOE(NBCNX,NBSOM,NOTLIN,IMMER,NBTERM,INOE)
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
C  DESCRIPTION : AJUSTEMENT DES RANGS DES NOEUDS DE LA MAILLE BETON
C  -----------   VOISINE INTERVENANT DANS LA RELATION CINEMATIQUE
C                AVEC LE NOEUD CABLE, LORSQUE CELUI-CI APPARTIENT
C                A UNE FACE OU UNE ARETE DE LA MAILLE
C               (RANGS DES NOEUDS DANS LA TABLE DE CONNECTIVITE DE
C                LA MAILLE)
C                APPELANT : RECI3D
C
C  IN     : NBCNX  : INTEGER , SCALAIRE
C                    NOMBRE DE NOEUDS DE LA MAILLE BETON VOISINE DU
C                    NOEUD CABLE
C  IN     : NBSOM  : INTEGER , SCALAIRE
C                    NOMBRE DE SOMMETS DE LA MAILLE BETON VOISINE DU
C                    NOEUD CABLE
C  IN     : NOTLIN : LOGICAL , SCALAIRE
C                    INDICATEUR BOOLEEN DU TYPE D'ELEMENT SUPPORTE PAR
C                    LA MAILLE BETON VOISINE DU NOEUD CABLE
C                    NOTLIN = .TRUE.  : ELEMENT NON LINEAIRE
C                    NOTLIN = .FALSE. : ELEMENT LINEAIRE
C  IN     : IMMER  : INTEGER , SCALAIRE
C                    INDICE D'IMMERSION DU NOEUD CABLE DANS LA MAILLE
C                    BETON VOISINE
C                    N.B. LORS D'UN APPEL A LA ROUTINE AJINOE, IMMER NE
C                    PEUT PRENDRE QUE DES VALEURS SUPERIEURES A 100
C                    IMMER = 100 + 10 * NUMERO DE FACE
C                                SI LE NOEUD CABLE EST SUR UNE FACE
C                                DE LA MAILLE BETON
C                    IMMER = 100 + 10 * NUMERO DE FACE + NUMERO D'ARETE
C                                SI LE NOEUD CABLE EST SUR UNE ARETE
C                                DE LA MAILLE BETON
C  OUT    : NBTERM : INTEGER , SCALAIRE
C                    NOMBRE DE TERMES DE LA RELATION CINEMATIQUE
C  OUT    : INOE   : INTEGER , VECTEUR DE DIMENSION 9
C                    RANGS DES NOEUDS DE LA MAILLE BETON VOISINE
C                    INTERVENANT DANS LA RELATION CINEMATIQUE
C                   (RANGS DES NOEUDS DANS LA TABLE DE CONNECTIVITE
C                    DE LA MAILLE)
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER       NBCNX, NBSOM, IMMER, NBTERM, INOE(*)
      LOGICAL       NOTLIN
C
C VARIABLES LOCALES
C -----------------
      INTEGER       ICALC, IFACE
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      IF ( IMMER.LT.100 )
     &   CALL UTMESS('F','AJINOE','INDICE D''IMMERSION INVALIDE')
C
      IFACE = (IMMER-100)/10
      ICALC = 100 + 10 * IFACE
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   LE NOEUD CABLE EST A L'INTERIEUR DU DOMAINE GEOMETRIQUE
C     DEFINI PAR LA FACE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF ( ICALC.EQ.IMMER ) THEN
C....... APPARTENANCE A UNE FACE D'UNE MAILLE TETRAEDRE
         IF ( NBSOM.EQ.4 ) THEN
            IF ( NOTLIN ) THEN
               NBTERM = 7
            ELSE
               NBTERM = 4
            ENDIF
            IF ( IFACE.EQ.1 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               INOE(3) = 3
               IF ( NOTLIN ) THEN
                  INOE(4) = 5
                  INOE(5) = 6
                  INOE(6) = 7
               ENDIF
            ELSE IF ( IFACE.EQ.2 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               INOE(3) = 4
               IF ( NOTLIN ) THEN
                  INOE(4) = 5
                  INOE(5) = 9
                  INOE(6) = 8
               ENDIF
            ELSE IF ( IFACE.EQ.3 ) THEN
               INOE(1) = 2
               INOE(2) = 3
               INOE(3) = 4
               IF ( NOTLIN ) THEN
                  INOE(4) = 6
                  INOE(5) = 10
                  INOE(6) = 9
               ENDIF
            ELSE
               INOE(1) = 3
               INOE(2) = 1
               INOE(3) = 4
               IF ( NOTLIN ) THEN
                  INOE(4) = 7
                  INOE(5) = 8
                  INOE(6) = 10
               ENDIF
            ENDIF
C....... APPARTENANCE A UNE FACE D'UNE MAILLE PYRAMIDE
         ELSE IF ( NBSOM.EQ.5 ) THEN
            IF ( IFACE.EQ.1 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               INOE(3) = 3
               INOE(4) = 4
               IF ( NOTLIN ) THEN
                  NBTERM = 9
                  INOE(5) = 6
                  INOE(6) = 7
                  INOE(7) = 8
                  INOE(8) = 9
               ELSE
                  NBTERM = 5
               ENDIF
            ELSE IF ( IFACE.EQ.2 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               INOE(3) = 5
               IF ( NOTLIN ) THEN
                  NBTERM = 7
                  INOE(4) = 6
                  INOE(5) = 11
                  INOE(6) = 10
               ELSE
                  NBTERM = 4
               ENDIF
            ELSE IF ( IFACE.EQ.3 ) THEN
               INOE(1) = 2
               INOE(2) = 3
               INOE(3) = 5
               IF ( NOTLIN ) THEN
                  NBTERM = 7
                  INOE(4) = 7
                  INOE(5) = 12
                  INOE(6) = 11
               ELSE
                  NBTERM = 4
               ENDIF
            ELSE IF ( IFACE.EQ.4 ) THEN
               INOE(1) = 3
               INOE(2) = 4
               INOE(3) = 5
               IF ( NOTLIN ) THEN
                  NBTERM = 7
                  INOE(4) = 8
                  INOE(5) = 13
                  INOE(6) = 12
               ELSE
                  NBTERM = 4
               ENDIF
            ELSE
               INOE(1) = 4
               INOE(2) = 1
               INOE(3) = 5
               IF ( NOTLIN ) THEN
                  NBTERM = 7
                  INOE(4) = 9
                  INOE(5) = 10
                  INOE(6) = 13
               ELSE
                  NBTERM = 4
               ENDIF
            ENDIF
C....... APPARTENANCE A UNE FACE D'UNE MAILLE PENTAEDRE
         ELSE IF ( NBSOM.EQ.6 ) THEN
            IF ( IFACE.EQ.1 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               INOE(3) = 3
               IF ( NOTLIN ) THEN
                  NBTERM = 7
                  INOE(4) = 7
                  INOE(5) = 8
                  INOE(6) = 9
               ELSE
                  NBTERM = 4
               ENDIF
            ELSE IF ( IFACE.EQ.2 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               INOE(3) = 5
               INOE(4) = 4
               IF ( NOTLIN ) THEN
                  NBTERM = 9
                  INOE(5) = 7
                  INOE(6) = 11
                  INOE(7) = 13
                  INOE(8) = 10
               ELSE
                  NBTERM = 5
               ENDIF
            ELSE IF ( IFACE.EQ.3 ) THEN
               INOE(1) = 2
               INOE(2) = 3
               INOE(3) = 6
               INOE(4) = 5
               IF ( NOTLIN ) THEN
                  NBTERM = 9
                  INOE(5) = 8
                  INOE(6) = 12
                  INOE(7) = 14
                  INOE(8) = 11
               ELSE
                  NBTERM = 5
               ENDIF
            ELSE IF ( IFACE.EQ.4 ) THEN
               INOE(1) = 3
               INOE(2) = 1
               INOE(3) = 4
               INOE(4) = 6
               IF ( NOTLIN ) THEN
                  NBTERM = 9
                  INOE(5) = 9
                  INOE(6) = 10
                  INOE(7) = 15
                  INOE(8) = 12
               ELSE
                  NBTERM = 5
               ENDIF
            ELSE
               INOE(1) = 4
               INOE(2) = 5
               INOE(3) = 6
               IF ( NOTLIN ) THEN
                  NBTERM = 7
                  INOE(4) = 13
                  INOE(5) = 14
                  INOE(6) = 15
               ELSE
                  NBTERM = 4
               ENDIF
            ENDIF
C....... APPARTENANCE A UNE FACE D'UNE MAILLE HEXAEDRE
         ELSE
            IF ( NOTLIN ) THEN
               IF ( NBCNX.EQ.27 ) THEN
                  NBTERM = 10
                  INOE(9) = 20 + IFACE
               ELSE
                  NBTERM = 9
               ENDIF
            ELSE
               NBTERM = 5
            ENDIF
            IF ( IFACE.EQ.1 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               INOE(3) = 3
               INOE(4) = 4
               IF ( NOTLIN ) THEN
                  INOE(5) = 9
                  INOE(6) = 10
                  INOE(7) = 11
                  INOE(8) = 12
               ENDIF
            ELSE IF ( IFACE.EQ.2 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               INOE(3) = 6
               INOE(4) = 5
               IF ( NOTLIN ) THEN
                  INOE(5) = 9
                  INOE(6) = 14
                  INOE(7) = 17
                  INOE(8) = 13
               ENDIF
            ELSE IF ( IFACE.EQ.3 ) THEN
               INOE(1) = 2
               INOE(2) = 3
               INOE(3) = 7
               INOE(4) = 6
               IF ( NOTLIN ) THEN
                  INOE(5) = 10
                  INOE(6) = 15
                  INOE(7) = 18
                  INOE(8) = 14
               ENDIF
            ELSE IF ( IFACE.EQ.4 ) THEN
               INOE(1) = 3
               INOE(2) = 4
               INOE(3) = 8
               INOE(4) = 7
               IF ( NOTLIN ) THEN
                  INOE(5) = 11
                  INOE(6) = 16
                  INOE(7) = 19
                  INOE(8) = 15
               ENDIF
            ELSE IF ( IFACE.EQ.5 ) THEN
               INOE(1) = 4
               INOE(2) = 1
               INOE(3) = 5
               INOE(4) = 8
               IF ( NOTLIN ) THEN
                  INOE(5) = 12
                  INOE(6) = 13
                  INOE(7) = 20
                  INOE(8) = 16
               ENDIF
            ELSE
               INOE(1) = 5
               INOE(2) = 6
               INOE(3) = 7
               INOE(4) = 8
               IF ( NOTLIN ) THEN
                  INOE(5) = 17
                  INOE(6) = 18
                  INOE(7) = 19
                  INOE(8) = 20
               ENDIF
            ENDIF
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   LE NOEUD CABLE EST SUR UNE ARETE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE
         IF ( NOTLIN ) THEN
            NBTERM = 4
         ELSE
            NBTERM = 3
         ENDIF
C....... APPARTENANCE A UNE ARETE D'UNE MAILLE TETRAEDRE
         IF ( NBSOM.EQ.4 ) THEN
            IF ( IMMER.EQ.111 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               IF ( NOTLIN ) INOE(3) = 5
            ELSE IF ( IMMER.EQ.112 ) THEN
               INOE(1) = 2
               INOE(2) = 3
               IF ( NOTLIN ) INOE(3) = 6
            ELSE IF ( IMMER.EQ.113 ) THEN
               INOE(1) = 3
               INOE(2) = 1
               IF ( NOTLIN ) INOE(3) = 7
            ELSE IF ( IMMER.EQ.122 ) THEN
               INOE(1) = 2
               INOE(2) = 4
               IF ( NOTLIN ) INOE(3) = 9
            ELSE IF ( IMMER.EQ.123 ) THEN
               INOE(1) = 4
               INOE(2) = 1
               IF ( NOTLIN ) INOE(3) = 8
            ELSE
               INOE(1) = 3
               INOE(2) = 4
               IF ( NOTLIN ) INOE(3) = 10
            ENDIF
C....... APPARTENANCE A UNE ARETE D'UNE MAILLE PYRAMIDE
         ELSE IF ( NBSOM.EQ.5 ) THEN
            IF ( IMMER.EQ.111 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               IF ( NOTLIN ) INOE(3) = 6
            ELSE IF ( IMMER.EQ.112 ) THEN
               INOE(1) = 2
               INOE(2) = 3
               IF ( NOTLIN ) INOE(3) = 7
            ELSE IF ( IMMER.EQ.113 ) THEN
               INOE(1) = 3
               INOE(2) = 4
               IF ( NOTLIN ) INOE(3) = 8
            ELSE IF ( IMMER.EQ.114 ) THEN
               INOE(1) = 4
               INOE(2) = 1
               IF ( NOTLIN ) INOE(3) = 9
            ELSE IF ( IMMER.EQ.122 ) THEN
               INOE(1) = 2
               INOE(2) = 5
               IF ( NOTLIN ) INOE(3) = 11
            ELSE IF ( IMMER.EQ.123 ) THEN
               INOE(1) = 5
               INOE(2) = 1
               IF ( NOTLIN ) INOE(3) = 10
            ELSE IF ( IMMER.EQ.132 ) THEN
               INOE(1) = 3
               INOE(2) = 5
               IF ( NOTLIN ) INOE(3) = 12
            ELSE
               INOE(1) = 4
               INOE(2) = 5
               IF ( NOTLIN ) INOE(3) = 13
            ENDIF
C....... APPARTENANCE A UNE ARETE D'UNE MAILLE PENTAEDRE
         ELSE IF ( NBSOM.EQ.6 ) THEN
            IF ( IMMER.EQ.111 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               IF ( NOTLIN ) INOE(3) = 7
            ELSE IF ( IMMER.EQ.112 ) THEN
               INOE(1) = 2
               INOE(2) = 3
               IF ( NOTLIN ) INOE(3) = 8
            ELSE IF ( IMMER.EQ.113 ) THEN
               INOE(1) = 3
               INOE(2) = 1
               IF ( NOTLIN ) INOE(3) = 9
            ELSE IF ( IMMER.EQ.122 ) THEN
               INOE(1) = 2
               INOE(2) = 5
               IF ( NOTLIN ) INOE(3) = 11
            ELSE IF ( IMMER.EQ.123 ) THEN
               INOE(1) = 5
               INOE(2) = 4
               IF ( NOTLIN ) INOE(3) = 13
            ELSE IF ( IMMER.EQ.124 ) THEN
               INOE(1) = 4
               INOE(2) = 1
               IF ( NOTLIN ) INOE(3) = 10
            ELSE IF ( IMMER.EQ.132 ) THEN
               INOE(1) = 3
               INOE(2) = 6
               IF ( NOTLIN ) INOE(3) = 12
            ELSE IF ( IMMER.EQ.133 ) THEN
               INOE(1) = 6
               INOE(2) = 5
               IF ( NOTLIN ) INOE(3) = 14
            ELSE
               INOE(1) = 4
               INOE(2) = 6
               IF ( NOTLIN ) INOE(3) = 15
            ENDIF
C....... APPARTENANCE A UNE ARETE D'UNE MAILLE HEXAEDRE
         ELSE
            IF ( IMMER.EQ.111 ) THEN
               INOE(1) = 1
               INOE(2) = 2
               IF ( NOTLIN ) INOE(3) = 9
            ELSE IF ( IMMER.EQ.112 ) THEN
               INOE(1) = 2
               INOE(2) = 3
               IF ( NOTLIN ) INOE(3) = 10
            ELSE IF ( IMMER.EQ.113 ) THEN
               INOE(1) = 3
               INOE(2) = 4
               IF ( NOTLIN ) INOE(3) = 11
            ELSE IF ( IMMER.EQ.114 ) THEN
               INOE(1) = 4
               INOE(2) = 1
               IF ( NOTLIN ) INOE(3) = 12
            ELSE IF ( IMMER.EQ.122 ) THEN
               INOE(1) = 2
               INOE(2) = 6
               IF ( NOTLIN ) INOE(3) = 14
            ELSE IF ( IMMER.EQ.123 ) THEN
               INOE(1) = 6
               INOE(2) = 5
               IF ( NOTLIN ) INOE(3) = 17
            ELSE IF ( IMMER.EQ.124 ) THEN
               INOE(1) = 5
               INOE(2) = 1
               IF ( NOTLIN ) INOE(3) = 13
            ELSE IF ( IMMER.EQ.132 ) THEN
               INOE(1) = 3
               INOE(2) = 7
               IF ( NOTLIN ) INOE(3) = 15
            ELSE IF ( IMMER.EQ.133 ) THEN
               INOE(1) = 7
               INOE(2) = 6
               IF ( NOTLIN ) INOE(3) = 18
            ELSE IF ( IMMER.EQ.142 ) THEN
               INOE(1) = 4
               INOE(2) = 8
               IF ( NOTLIN ) INOE(3) = 16
            ELSE IF ( IMMER.EQ.143 ) THEN
               INOE(1) = 8
               INOE(2) = 7
               IF ( NOTLIN ) INOE(3) = 19
            ELSE
               INOE(1) = 5
               INOE(2) = 8
               IF ( NOTLIN ) INOE(3) = 20
            ENDIF
         ENDIF
      ENDIF
C
C --- FIN DE AJINOE.
      END
