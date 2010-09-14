      SUBROUTINE IRMAC2 ( KTYPE, ITYCA, GTYPE, NNOE )
      IMPLICIT   NONE
      INTEGER            ITYCA, NNOE
      CHARACTER*8        KTYPE, GTYPE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/09/2010   AUTEUR REZETTE C.REZETTE 
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
C     ------------------------------------------------------------------
C
      IF     ( KTYPE .EQ. 'POI1'   ) THEN
         ITYCA = 1
         GTYPE = 'POI1'
      ELSEIF ( KTYPE .EQ. 'SEG2'   ) THEN
         ITYCA = 2
         GTYPE = 'SEG2'
      ELSEIF ( KTYPE .EQ. 'SEG3'   ) THEN
         ITYCA = 3
         GTYPE = 'SEG3'
      ELSEIF ( KTYPE .EQ. 'SEG4'   ) THEN
         NNOE = NNOE - 2
         CALL U2MESS('I','PREPOST2_64')
         ITYCA = 2
         GTYPE = 'SEG2'
      ELSEIF ( KTYPE .EQ. 'TRIA3'   ) THEN
         ITYCA = 4
         GTYPE = 'TRI3'
      ELSEIF ( KTYPE .EQ. 'TRIA6'   ) THEN
         ITYCA = 6
         GTYPE = 'TRI6'
      ELSEIF ( KTYPE .EQ. 'TRIA7'   ) THEN
         NNOE = NNOE - 1
         CALL U2MESS('I','PREPOST2_65')
         ITYCA = 6
         GTYPE = 'TRI6'
      ELSEIF ( KTYPE .EQ. 'QUAD4'   ) THEN
         ITYCA = 8
         GTYPE = 'QUA4'
      ELSEIF ( KTYPE .EQ. 'QUAD8'   ) THEN
         ITYCA = 10
         GTYPE = 'QUA8'
      ELSEIF ( KTYPE .EQ. 'QUAD9'   ) THEN
         NNOE = NNOE - 1
         CALL U2MESS('I','PREPOST2_66')
         ITYCA = 10
         GTYPE = 'QUA8'
CCC         ITYCA = 11
CCC         GTYPE = 'QUA9'
      ELSEIF ( KTYPE .EQ. 'HEXA8'   ) THEN
         ITYCA = 14
         GTYPE = 'CUB8'
      ELSEIF ( KTYPE .EQ. 'HEXA20'  ) THEN
         ITYCA = 15
         GTYPE = 'CU20'
      ELSEIF ( KTYPE .EQ. 'HEXA27'  ) THEN
         ITYCA = 33
         GTYPE = 'CU27'
      ELSEIF ( KTYPE .EQ. 'PENTA6'  ) THEN
         ITYCA = 16
         GTYPE = 'PRI6'
      ELSEIF ( KTYPE .EQ. 'PENTA15' ) THEN
         ITYCA = 17
         GTYPE = 'PR15'
      ELSEIF ( KTYPE .EQ. 'PENTA18' ) THEN
         ITYCA = 17
         CALL U2MESS('I','PREPOST2_86')
         GTYPE = 'PR15'
      ELSEIF ( KTYPE .EQ. 'TETRA4'  ) THEN
         ITYCA = 23
         GTYPE = 'TET4'
      ELSEIF ( KTYPE .EQ. 'TETRA10' ) THEN
         ITYCA = 24
         GTYPE = 'TE10'
      ELSEIF ( KTYPE .EQ. 'PYRAM5'  ) THEN
         ITYCA = 25
         GTYPE = 'PYR5'
      ELSEIF ( KTYPE .EQ. 'PYRAM13' ) THEN
         ITYCA = 26
         GTYPE = 'PY13'
      ENDIF
C
      END
