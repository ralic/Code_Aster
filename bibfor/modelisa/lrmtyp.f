      SUBROUTINE LRMTYP ( NBTYP, NOMTYP, NNOTYP, TYPGEO, RENUMD,
     &                    MODNUM, NUANOM, NUMNOA )
C_____________________________________________________________________
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 14/09/2010   AUTEUR REZETTE C.REZETTE 
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
C RESPONSABLE GNICOLAS G.NICOLAS
C     RECUP DES NOMS/NBNO DES TYPES DE MAILLES DANS LE CATALOGUE
C     ET RECUP DES TYPE GEO CORRESPONDANT POUR MED
C
C     SORTIE:
C       MODNUM : INDICATEUR SI LA SPECIFICATION DE NUMEROTATION DES
C                NOEUDS DES MAILLES EST DIFFERENTES ENTRE ASTER ET MED:
C                     MODNUM = 0 : NUMEROTATION IDENTIQUE
C                     MODNUM = 1 : NUMEROTATION DIFFERENTE
C       NUANOM : TABLEAU DE CORRESPONDANCE DES NOEUDS (MED/ASTER).
C                NUANOM(ITYP,J): NUMERO DANS ASTER DU J IEME NOEUD
C                DE LA MAILLE DE TYPE ITYP DANS MED.
C       NUMNOA : TABLEAU DE CORRESPONDANCE DES NOEUDS (MED/ASTER).
C                NUMNOA(ITYP,J) : NUMERO DANS MED DU J IEME NOEUD
C                DE LA MAILLE DE TYPE ITYP D'ASTER

C ---------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NTYMAX
      PARAMETER (NTYMAX = 54)
      INTEGER NNOMAX
      PARAMETER (NNOMAX=27)
C
C 0.1. ==> ARGUMENTS
C
      INTEGER NBTYP
      INTEGER NNOTYP(NTYMAX), TYPGEO(NTYMAX), RENUMD(NTYMAX)
      INTEGER MODNUM(NTYMAX)
      INTEGER NUANOM(NTYMAX,NNOMAX), NUMNOA(NTYMAX,NNOMAX)
      CHARACTER*8 NOMTYP(NTYMAX)
C
C 0.2. ==> COMMUNS
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32     JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C 0.3. ==> VARIABLES LOCALES
C
C
      INTEGER NUMMED(NTYMAX)
      INTEGER IAUX, JAUX
      INTEGER ITYP
      CHARACTER*8 NOMAST(NTYMAX)
      CHARACTER*1 K1
C
C 0.4. ==> INITIALISATIONS
C
C     CORRESPONDANCE DES NUMEROS DE TYPE DE GEOMETRIE ENTRE ASTER ET MED
C     (LIE A LORDRE DEFINI DANS LE CATALOGUE TYPE_MAILLE.CATA)
      DATA NOMAST  /'POI1    ','SEG2    ','SEG22   ','SEG3    ',
     &              'SEG33   ','SEG4    ',
     &                         'TRIA3   ','TRIA33  ','TRIA6   ',
     &              'TRIA66  ','TRIA7   ','QUAD4   ','QUAD44  ',
     &              'QUAD8   ','QUAD88  ','QUAD9   ','QUAD99  ',
     &              'TETRA4  ','TETRA10 ','PENTA6  ','PENTA15 ',
     &              'PENTA18 ','PYRAM5  ','PYRAM13 ','HEXA8   ',
     &              'HEXA20  ','HEXA27  ','TR3QU4  ','QU4TR3  ',
     &              'TR6TR3  ','TR3TR6  ','TR6QU4  ','QU4TR6  ',
     &              'TR6QU8  ','QU8TR6  ','TR6QU9  ','QU9TR6  ',
     &              'QU8TR3  ','TR3QU8  ','QU8QU4  ','QU4QU8  ',
     &              'QU8QU9  ','QU9QU8  ','QU9QU4  ','QU4QU9  ',
     &              'QU9TR3  ','TR3QU9  ','SEG32   ','SEG23   ',
     &              'QU4QU4  ','TR3TR3  ','HE8HE8  ','PE6PE6  ',
     &              'TE4TE4  '/
      DATA NUMMED  /1,         102,       0,         103,
     &              0,         0,
     &                         203,       0,         206,
     &              0,         0,         204,       0,
     &              208,       0,         0,         0,
     &              304,       310,       306,       315,
     &              0,         305,       313,       308,
     &              320,       0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0/
C     ------------------------------------------------------------------
      CALL JEMARQ ( )
C
C     VERIFICATION QUE LE CATALOGUE EST ENCORE COHERENT AVEC LE FORTRAN
C
      CALL JELIRA('&CATA.TM.NOMTM','NOMMAX',IAUX,K1)
      IF ( NTYMAX .NE. IAUX ) THEN
         CALL U2MESS('F','MED_38')
      ENDIF
C
C     NOM / NBNO PAR TYPE DE MAILLE
C
      DO 1 ITYP = 1,NTYMAX
         CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',ITYP),NOMTYP(ITYP))
         IF ( NOMAST(ITYP) .NE. NOMTYP(ITYP) ) THEN
           CALL U2MESS('F','MED_39')
         ENDIF
         CALL JEVEUO (JEXNUM('&CATA.TM.NBNO' ,ITYP),'L',JAUX)
         NNOTYP(ITYP) = ZI(JAUX)
         TYPGEO(ITYP) = NUMMED(ITYP)
C
  1   CONTINUE
C
      NBTYP = 0
      DO 21 , ITYP = 1 , NTYMAX
        IF ( NUMMED(ITYP).NE.0 ) THEN
          DO 211 , IAUX = 1 , NBTYP
            IF ( NUMMED(ITYP).LT.NUMMED(RENUMD(IAUX)) ) THEN
              JAUX = IAUX
              GOTO 212
            ENDIF
  211     CONTINUE
          JAUX = NBTYP + 1
  212     CONTINUE
          NBTYP = NBTYP + 1
          DO 213 , IAUX = NBTYP , JAUX + 1 , -1
            RENUMD(IAUX) = RENUMD(IAUX-1)
  213     CONTINUE
          RENUMD(JAUX) = ITYP
        ENDIF
   21 CONTINUE
C
C====
C 3. CHANGEMENT DE CONVENTION DANS LES CONNECTIVITES ENTRE ASTER ET MED
C====
C
C 3.1. ==> PAR DEFAUT, LES DEUX NUMEROTATIONS SONT IDENTIQUES
C
      DO 311 , IAUX = 1 , NTYMAX
C
        MODNUM(IAUX) = 0
C
        DO 312 , JAUX = 1 , NNOMAX
          NUANOM(IAUX,JAUX) = 0
          NUMNOA(IAUX,JAUX) = 0
  312 CONTINUE
C
  311 CONTINUE
C
C 3.2. ==> MODIFICATIONS POUR LES TETRAEDRES
C       ------ TETRA4 -------
C
      MODNUM(18)=1
C
      NUANOM(18,1)=1
      NUMNOA(18,1)=1
      NUANOM(18,2)=3
      NUMNOA(18,2)=3
      NUANOM(18,3)=2
      NUMNOA(18,3)=2
      NUANOM(18,4)=4
      NUMNOA(18,4)=4

C       ------ TETRA10 -------
C
      MODNUM(19)=1
C
      NUANOM(19,1)=1
      NUMNOA(19,1)=1
      NUANOM(19,2)=3
      NUMNOA(19,2)=3
      NUANOM(19,3)=2
      NUMNOA(19,3)=2
      NUANOM(19,4)=4
      NUMNOA(19,4)=4
      NUANOM(19,5)=7
      NUMNOA(19,5)=7
      NUANOM(19,6)=6
      NUMNOA(19,6)=6
      NUANOM(19,7)=5
      NUMNOA(19,7)=5
      NUANOM(19,8)=8
      NUMNOA(19,8)=8
      NUANOM(19,9)=10
      NUMNOA(19,9)=10
      NUANOM(19,10)=9
      NUMNOA(19,10)=9
C
C 3.3. ==> MODIFICATIONS POUR LES PENTAEDRES
C       ------ PENTA6 -------
C
      MODNUM(20)=1
C
      NUANOM(20,1)=1
      NUMNOA(20,1)=1
      NUANOM(20,2)=3
      NUMNOA(20,2)=3
      NUANOM(20,3)=2
      NUMNOA(20,3)=2
      NUANOM(20,4)=4
      NUMNOA(20,4)=4
      NUANOM(20,5)=6
      NUMNOA(20,5)=6
      NUANOM(20,6)=5
      NUMNOA(20,6)=5

C       ------ PENTA15 -------
C
      MODNUM(21)=1
C
      NUANOM(21,1)=1
      NUMNOA(21,1)=1
      NUANOM(21,2)=3
      NUMNOA(21,2)=3
      NUANOM(21,3)=2
      NUMNOA(21,3)=2
      NUANOM(21,4)=4
      NUMNOA(21,4)=4
      NUANOM(21,5)=6
      NUMNOA(21,5)=6
      NUANOM(21,6)=5
      NUMNOA(21,6)=5
      NUANOM(21,7)=9
      NUMNOA(21,7)=9
      NUANOM(21,8)=8
      NUMNOA(21,8)=8
      NUANOM(21,9)=7
      NUMNOA(21,9)=7
      NUANOM(21,10)=15
      NUMNOA(21,10)=13
      NUANOM(21,11)=14
      NUMNOA(21,11)=15
      NUANOM(21,12)=13
      NUMNOA(21,12)=14
      NUANOM(21,13)=10
      NUMNOA(21,13)=12
      NUANOM(21,14)=12
      NUMNOA(21,14)=11
      NUANOM(21,15)=11
      NUMNOA(21,15)=10

C
C 3.4. ==> MODIFICATIONS POUR LES PYRAMIDES
C       ------ PYRAM5 -------
C
      MODNUM(23)=1
C
      NUANOM(23,1)=1
      NUMNOA(23,1)=1
      NUANOM(23,2)=4
      NUMNOA(23,2)=4
      NUANOM(23,3)=3
      NUMNOA(23,3)=3
      NUANOM(23,4)=2
      NUMNOA(23,4)=2
      NUANOM(23,5)=5
      NUMNOA(23,5)=5

C       ------ PYRAM13 -------
      MODNUM(24)=1
C
      NUANOM(24,1)=1
      NUMNOA(24,1)=1
      NUANOM(24,2)=4
      NUMNOA(24,2)=4
      NUANOM(24,3)=3
      NUMNOA(24,3)=3
      NUANOM(24,4)=2
      NUMNOA(24,4)=2
      NUANOM(24,5)=5
      NUMNOA(24,5)=5
      NUANOM(24,6)=9
      NUMNOA(24,6)=9
      NUANOM(24,7)=8
      NUMNOA(24,7)=8
      NUANOM(24,8)=7
      NUMNOA(24,8)=7
      NUANOM(24,9)=6
      NUMNOA(24,9)=6
      NUANOM(24,10)=10
      NUMNOA(24,10)=10
      NUANOM(24,11)=13
      NUMNOA(24,11)=13
      NUANOM(24,12)=12
      NUMNOA(24,12)=12
      NUANOM(24,13)=11
      NUMNOA(24,13)=11
C
C
C 3.2. ==> MODIFICATIONS POUR LES HEXAEDRES
C
C       ------ HEXA8 -------
C
      MODNUM(25)=1
C
      NUANOM(25,1)=1
      NUMNOA(25,1)=1
      NUANOM(25,2)=4
      NUMNOA(25,2)=4
      NUANOM(25,3)=3
      NUMNOA(25,3)=3
      NUANOM(25,4)=2
      NUMNOA(25,4)=2
      NUANOM(25,5)=5
      NUMNOA(25,5)=5
      NUANOM(25,6)=8
      NUMNOA(25,6)=8
      NUANOM(25,7)=7
      NUMNOA(25,7)=7
      NUANOM(25,8)=6
      NUMNOA(25,8)=6
C
C       ------ HEXA20 -------
C
      MODNUM(26)=1
C
      NUANOM(26,1)=1
      NUMNOA(26,1)=1
      NUANOM(26,2)=4
      NUMNOA(26,2)=4
      NUANOM(26,3)=3
      NUMNOA(26,3)=3
      NUANOM(26,4)=2
      NUMNOA(26,4)=2
      NUANOM(26,5)=5
      NUMNOA(26,5)=5
      NUANOM(26,6)=8
      NUMNOA(26,6)=8
      NUANOM(26,7)=7
      NUMNOA(26,7)=7
      NUANOM(26,8)=6
      NUMNOA(26,8)=6
      NUANOM(26,9)=12
      NUMNOA(26,9)=12
      NUANOM(26,10)=11
      NUMNOA(26,10)=11
      NUANOM(26,11)=10
      NUMNOA(26,11)=10
      NUANOM(26,12)=9
      NUMNOA(26,12)=9
      NUANOM(26,13)=20
      NUMNOA(26,13)=17
      NUANOM(26,14)=19
      NUMNOA(26,14)=20
      NUANOM(26,15)=18
      NUMNOA(26,15)=19
      NUANOM(26,16)=17
      NUMNOA(26,16)=18
      NUANOM(26,17)=13
      NUMNOA(26,17)=16
      NUANOM(26,18)=16
      NUMNOA(26,18)=15
      NUANOM(26,19)=15
      NUMNOA(26,19)=14
      NUANOM(26,20)=14
      NUMNOA(26,20)=13
C
      CALL JEDEMA ( )
C
      END
