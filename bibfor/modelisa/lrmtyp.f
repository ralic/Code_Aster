      SUBROUTINE LRMTYP ( NDIM, NBTYP, NOMTYP,
     >                    NNOTYP, NITTYP, TYPGEO, RENUMD )
C_____________________________________________________________________
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 22/07/2003   AUTEUR LAVERNE J.LAVERNE 
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
C     RECUP DES NOMS/NBNO DES TYPES DE MAILLES DANS LE CATALOGUE
C     ET RECUP DES TYPE GEO CORRESPONDANT POUR MED
C_____________________________________________________________________
C
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      INTEGER         NBTYP, NDIM
      INTEGER         NNOTYP(*), TYPGEO(*), NITTYP(*), RENUMD(*)
      CHARACTER*8     NOMTYP(*)        
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
      INTEGER         ITYP, J, NBTMAX
      PARAMETER       (NBTMAX = 48)
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'LRMTYP' )
C
      INTEGER         NUMMED(NBTMAX), NSUP3D(NBTMAX), NSUP2D(NBTMAX)
      INTEGER         IAUX, JAUX
      CHARACTER*8     NOMAST(NBTMAX)
      CHARACTER*1     K1
C
C 0.4. ==> INITIALISATIONS
C
C     CORRESPONDANCE DES NUMEROS DE TYPE DE GEOMETRIE ENTRE ASTER ET MED
C     (LIE A LORDRE DEFINI DANS LE CATALOGUE TYPE_MAILLE.CATA)   
      DATA NOMAST  /'POI1    ','SEG2    ','SEG22   ','SEG3    ',
     +              'SEG33   ','SEG4    ',
     +                         'TRIA3   ','TRIA33  ','TRIA6   ',
     +              'TRIA66  ','TRIA7   ','QUAD4   ','QUAD44  ',
     +              'QUAD8   ','QUAD88  ','QUAD9   ','QUAD99  ',
     +              'TETRA4  ','TETRA10 ','PENTA6  ','PENTA15 ',
     +              'PYRAM5  ','PYRAM13 ','HEXA8   ','HEXA20  ',
     +              'HEXA27  ','TR3QU4  ','QU4TR3  ','TR6TR3  ',
     +              'TR3TR6  ','TR6QU4  ','QU4TR6  ','TR6QU8  ',
     +              'QU8TR6  ','TR6QU9  ','QU9TR6  ','QU8TR3  ',
     +              'TR3QU8  ','QU8QU4  ','QU4QU8  ','QU8QU9  ',
     +              'QU9QU8  ','QU9QU4  ','QU4QU9  ','QU9TR3  ',
     +              'TR3QU9  ','SEG32   ','SEG23   '/
      DATA NUMMED  /1,         102,       0,         103,    
     +              0,         0, 
     +                         203,       0,         206,
     +              0,         0,         204,       0,
     +              208,       0,         0,         0,
     +              304,       310,       306,       315,
     +              305,       313,       308,       320,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0/
      DATA NSUP3D  /0,         1,         0,         1,    
     +              0,         0,
     +                         1,         0,         1,
     +              0,         0,         1,         0,
     +              1,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0/
      DATA NSUP2D  /0,         1,         0,         1,    
     +              0,         0,
     +                         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0,         0,
     +              0,         0,         0/
C     ------------------------------------------------------------------
      CALL JEMARQ ( )
C
C     VERIFICATION QUE LE CATALOGUE EST ENCORE COHERENT AVEC LE FORTRAN
C
      CALL JELIRA('&CATA.TM.NOMTM','NOMMAX',IAUX,K1)
      IF ( NBTMAX .NE. IAUX ) THEN
         CALL UTMESS ( 'F',NOMPRO,'INCOHERENCE CATALOGUE - FORTRAN '//
     &                 '(NBTYP FORTRAN DIFFERENT DE NBTYP CATALOGUE)')
      ENDIF
C
C     NOM / NBNO PAR TYPE DE MAILLE
C      
      DO 1 ITYP = 1,NBTMAX
         CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',ITYP),NOMTYP(ITYP))
         IF ( NOMAST(ITYP) .NE. NOMTYP(ITYP) ) THEN
           CALL UTMESS('F',NOMPRO,'INCOHERENCE CATALOGUE - FORTRAN '//
     &                 '(NOMTYP FORTRAN DIFFERENT DE NOMTYP CATALOGUE)')
         ENDIF
         CALL JEVEUO (JEXNUM('&CATA.TM.NBNO' ,ITYP),'L',J)
         NNOTYP(ITYP) = ZI(J)
         TYPGEO(ITYP) = NUMMED(ITYP)
         IF ( NDIM .EQ. 3 ) THEN
           NITTYP(ITYP) = NNOTYP(ITYP) + NSUP3D(ITYP)
         ELSEIF ( NDIM .EQ. 2 ) THEN
           NITTYP(ITYP) = NNOTYP(ITYP) + NSUP2D(ITYP)
         ELSE
           CALL UTMESS('F',NOMPRO,'LA DIMENSION NE VAUT NI 2, NI 3.')
         ENDIF
C       
  1   CONTINUE
C
      NBTYP = 0
      DO 21 , ITYP = 1 , NBTMAX
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
      CALL JEDEMA ( )
      END
