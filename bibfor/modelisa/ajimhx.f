      SUBROUTINE AJIMHX(ITETRA,IMMER)
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
C  DESCRIPTION : REAJUSTEMENT DE L'INDICE D'IMMERSION APRES TENTATIVE
C  -----------   D'IMMERSION D'UN NOEUD CABLE DANS UN SOUS-DOMAINE
C                TETRAEDRE D'UNE MAILLE HEXAEDRE APPARTENANT A LA
C                STRUCTURE BETON
C                APPELANT : IMMEHX
C
C  IN     : ITETRA : INTEGER , SCALAIRE
C                    INDICATEUR DU SOUS-DOMAINE TETRAEDRE DANS LEQUEL
C                    A ETE TENTEE L'IMMERSION
C                    ITETRA = 1 OU 2 OU 3 OU 4 OU 5 OU 6
C  OUT    : IMMER  : INTEGER , SCALAIRE
C                    INDICE D'IMMERSION
C                    IMMER = -1  IMMERSION NON REUSSIE
C                    IMMER =  0  LE NOEUD CABLE EST A L'INTERIEUR
C                                DE LA MAILLE
C                    IMMER = 100 + 10 * NUMERO DE FACE
C                                LE NOEUD CABLE EST SUR UNE FACE
C                                DE LA MAILLE
C                    IMMER = 100 + 10 * NUMERO DE FACE + NUMERO D'ARETE
C                                LE NOEUD CABLE EST SUR UNE ARETE
C                                DE LA MAILLE
C                    IMMER =  2  LE NOEUD CABLE COINCIDE AVEC UN DES
C                                NOEUDS DE LA MAILLE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER       ITETRA, IMMER
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   REAJUSTEMENT APRES TENTATIVE D'IMMERSION DANS LE TETRAEDRE NO1
C     DE SOMMETS 4-5-1-3
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF ( ITETRA.EQ.1 ) THEN
C
         IF ( IMMER.EQ.110 ) THEN
            IMMER = 150
         ELSE IF ( IMMER.EQ.120 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.130 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.140 ) THEN
            IMMER = 110
         ELSE IF ( IMMER.EQ.111 ) THEN
            IMMER = 150
         ELSE IF ( IMMER.EQ.112 ) THEN
            IMMER = 124
         ELSE IF ( IMMER.EQ.113 ) THEN
            IMMER = 114
         ELSE IF ( IMMER.EQ.122 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.123 ) THEN
            IMMER = 113
         ELSE IF ( IMMER.EQ.132 ) THEN
            IMMER = 110
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   REAJUSTEMENT APRES TENTATIVE D'IMMERSION DANS LE TETRAEDRE NO2
C     DE SOMMETS 5-1-3-6
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE IF ( ITETRA.EQ.2 ) THEN
C
         IF ( IMMER.EQ.110 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.130 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.140 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.111 ) THEN
            IMMER = 124
         ELSE IF ( IMMER.EQ.112 ) THEN
            IMMER = 110
         ELSE IF ( IMMER.EQ.113 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.122 ) THEN
            IMMER = 120
         ELSE IF ( IMMER.EQ.132 ) THEN
            IMMER = 130
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 3   REAJUSTEMENT APRES TENTATIVE D'IMMERSION DANS LE TETRAEDRE NO3
C     DE SOMMETS 1-3-6-2
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE IF ( ITETRA.EQ.3 ) THEN
C
         IF ( IMMER.EQ.110 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.120 ) THEN
            IMMER = 110
         ELSE IF ( IMMER.EQ.140 ) THEN
            IMMER = 120
         ELSE IF ( IMMER.EQ.111 ) THEN
            IMMER = 110
         ELSE IF ( IMMER.EQ.112 ) THEN
            IMMER = 130
         ELSE IF ( IMMER.EQ.113 ) THEN
            IMMER = 120
         ELSE IF ( IMMER.EQ.122 ) THEN
            IMMER = 112
         ELSE IF ( IMMER.EQ.123 ) THEN
            IMMER = 111
         ELSE IF ( IMMER.EQ.132 ) THEN
            IMMER = 122
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 4   REAJUSTEMENT APRES TENTATIVE D'IMMERSION DANS LE TETRAEDRE NO4
C     DE SOMMETS 3-6-7-4
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE IF ( ITETRA.EQ.4 ) THEN
C
         IF ( IMMER.EQ.110 ) THEN
            IMMER = 130
         ELSE IF ( IMMER.EQ.120 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.130 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.111 ) THEN
            IMMER = 130
         ELSE IF ( IMMER.EQ.112 ) THEN
            IMMER = 133
         ELSE IF ( IMMER.EQ.113 ) THEN
            IMMER = 132
         ELSE IF ( IMMER.EQ.122 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.123 ) THEN
            IMMER = 113
         ELSE IF ( IMMER.EQ.132 ) THEN
            IMMER = 140
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 5   REAJUSTEMENT APRES TENTATIVE D'IMMERSION DANS LE TETRAEDRE NO5
C     DE SOMMETS 6-7-4-5
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE IF ( ITETRA.EQ.5 ) THEN
C
         IF ( IMMER.EQ.110 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.120 ) THEN
            IMMER = 160
         ELSE IF ( IMMER.EQ.130 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.140 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.111 ) THEN
            IMMER = 133
         ELSE IF ( IMMER.EQ.112 ) THEN
            IMMER = 140
         ELSE IF ( IMMER.EQ.113 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.122 ) THEN
            IMMER = 160
         ELSE IF ( IMMER.EQ.132 ) THEN
            IMMER = 150
         ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 6   REAJUSTEMENT APRES TENTATIVE D'IMMERSION DANS LE TETRAEDRE NO6
C     DE SOMMETS 7-4-5-8
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      ELSE
C
         IF ( IMMER.EQ.110 ) THEN
            IMMER = 0
         ELSE IF ( IMMER.EQ.120 ) THEN
            IMMER = 140
         ELSE IF ( IMMER.EQ.130 ) THEN
            IMMER = 150
         ELSE IF ( IMMER.EQ.140 ) THEN
            IMMER = 160
         ELSE IF ( IMMER.EQ.111 ) THEN
            IMMER = 140
         ELSE IF ( IMMER.EQ.112 ) THEN
            IMMER = 150
         ELSE IF ( IMMER.EQ.113 ) THEN
            IMMER = 160
         ELSE IF ( IMMER.EQ.122 ) THEN
            IMMER = 142
         ELSE IF ( IMMER.EQ.123 ) THEN
            IMMER = 143
         ELSE IF ( IMMER.EQ.132 ) THEN
            IMMER = 153
         ENDIF
C
      ENDIF
C
C --- FIN DE AJIMHX.
      END
