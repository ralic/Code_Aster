      SUBROUTINE ELREF7 ( ELREFV,
     >                    TYMVOL, NDEGRE, NBF, ELREF1, ELREF2 )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2009   AUTEUR GNICOLAS G.NICOLAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GNICOLAS G.NICOLAS
C
      IMPLICIT NONE
C
      CHARACTER*8 ELREFV
      INTEGER TYMVOL, NDEGRE
      INTEGER NBF
      CHARACTER*8 ELREF1, ELREF2
C
C
C     BUT:
C         DETERMINATION DES CARACTERISTIQUES DES FACES DES VOLUMES
C
C     ARGUMENTS:
C     ----------
C      ENTREE :
C-------------
C IN   ELREFV : DENOMINATION DE LA MAILLE VOLUMIQUE
C               'QU4', 'QU8', 'QU9'
C               'TR3', 'TR6', 'TR7'
C               'HE8', 'H20', 'H27'
C               'PE6', 'P15'
C               'TE4', 'T10'
C               'PY5', 'P13'
C
C      SORTIE :
C-------------
C OUT  TYMVOL : TYPE DE LA MAILLE VOLUMIQUE SELON LE CODE SUIVANT
C              -2 : QUADRANGLE
C              -1 : TRIANGLE
C               1 : HEXAEDRE
C               2 : PENTAEDRE
C               3 : TETRAEDRE
C               4 : PYRAMIDE
C OUT  NDEGRE : DEGRE DE L'ELEMENT
C OUT  NBF    : NOMBRE DE FACES DE LA MAILLE VOLUMIQUE
C OUT  ELREF1 : DENOMINATION DE LA MAILLE FACE DE ELREFV - FAMILLE 1
C OUT  ELREF2 : DENOMINATION DE LA MAILLE FACE DE ELREFV - FAMILLE 2
C ......................................................................
C
      CHARACTER*6 VALK(2)
C
      ELREF1 = '        '
      ELREF2 = '        '
C               12345678
C
C====
C 1. LA DESCRIPTION DES FACES DES VOLUMES 2D
C====
C 1.1. ==> QUADRANGLES
C
      IF ( ELREFV(1:3).EQ.'QU4' ) THEN
        TYMVOL = -2
        NDEGRE = 1
        NBF = 4
        ELREF1 = 'SE2'
C
      ELSE IF ( ELREFV(1:3).EQ.'QU8' ) THEN
        TYMVOL = -2
        NDEGRE = 2
        NBF = 4
        ELREF1 = 'SE3'
C
      ELSE IF ( ELREFV(1:3).EQ.'QU9' ) THEN
        TYMVOL = -2
        NDEGRE = 2
        NBF = 4
        ELREF1 = 'SE3'
C
C 1.2. ==> TRIANGLES
C
      ELSE IF ( ELREFV(1:3).EQ.'TR3' ) THEN
        TYMVOL = -1
        NDEGRE = 1
        NBF = 3
        ELREF1 = 'SE2'
C
      ELSE IF ( ELREFV(1:3).EQ.'TR6' ) THEN
        TYMVOL = -1
        NDEGRE = 2
        NBF = 3
        ELREF1 = 'SE3'
C
      ELSE IF ( ELREFV(1:3).EQ.'TR7' ) THEN
        TYMVOL = -1
        NDEGRE = 2
        NBF = 3
        ELREF1 = 'SE3'
C
C====
C 2. LA DESCRIPTION DES FACES DES VOLUMES 3D
C====
C 2.1. ==> HEXAEDRES
C
      ELSEIF ( ELREFV(1:3).EQ.'HE8' ) THEN
        TYMVOL = 1
        NDEGRE = 1
        NBF = 6
        ELREF1 = 'QU4'
C
      ELSE IF ( ELREFV(1:3).EQ.'H20' ) THEN
        TYMVOL = 1
        NDEGRE = 2
        NBF = 6
        ELREF1 = 'QU8'
C
      ELSE IF ( ELREFV(1:3).EQ.'H27' ) THEN
        TYMVOL = 1
        NDEGRE = 2
        NBF = 6
        ELREF1 = 'QU9'
C
C 2.2. ==> PENTAEDRES
C
      ELSE IF ( ELREFV(1:3).EQ.'PE6' ) THEN
        TYMVOL = 2
        NDEGRE = 1
        NBF = 5
        ELREF1 = 'TR3'
        ELREF2 = 'QU4'
C
      ELSE IF ( ELREFV(1:3).EQ.'P15' ) THEN
        TYMVOL = 2
        NDEGRE = 2
        NBF = 5
        ELREF1 = 'TR6'
        ELREF2 = 'QU8'
C
C 2.3. ==> TETRAEDRES
C
      ELSE IF ( ELREFV(1:3).EQ.'TE4' ) THEN
        TYMVOL = 3
        NDEGRE = 1
        NBF = 4
        ELREF1 = 'TR3'
C
      ELSE IF ( ELREFV(1:3).EQ.'T10' ) THEN
        TYMVOL = 3
        NDEGRE = 2
        NBF = 4
        ELREF1 = 'TR6'
C
C 2.4. ==> PYRAMIDES
C
      ELSE IF ( ELREFV(1:3).EQ.'PY5' ) THEN
        TYMVOL = 4
        NDEGRE = 1
        NBF = 5
        ELREF1 = 'TR3'
        ELREF2 = 'QU4'
C
      ELSE IF ( ELREFV(1:3).EQ.'P13' ) THEN
        TYMVOL = 4
        NDEGRE = 2
        NBF = 5
        ELREF1 = 'TR6'
        ELREF2 = 'QU8'
C
C====
C 3. INCONNU
C====
C
      ELSE
C
        VALK(1) = ELREFV(1:3)
        CALL U2MESK('F','INDICATEUR_10',1,VALK)
C
      END IF
C
CGN          WRITE(6,*) 'TYMVOL :',TYMVOL, ', NBF :', NBF
CGN          WRITE(6,*) 'ELREF1 : ',ELREF1
CGN          WRITE(6,*) 'ELREF2 : ',ELREF2
C
      END
