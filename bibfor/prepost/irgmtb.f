      SUBROUTINE IRGMTB ( TDEC, TYPD, VERS )
      IMPLICIT   NONE
      INTEGER    NTYELE,MAXEL,MAXNO
      PARAMETER (NTYELE = 27)
      PARAMETER (MAXEL  = 48)
      PARAMETER (MAXNO  =  8)
C
      INTEGER             TDEC(NTYELE,MAXEL,MAXNO)
      INTEGER                   TYPD(NTYELE,3)
      INTEGER                          VERS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/02/2004   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C     ------------------------------------------------------------------
C     IN  : VERS
C     OUT : TDEC, TYPD
C
C     NTYELE : NOMBRE DE TYPES DE MAILLES TRAITEES
C              (MAX DE TYPE_MAILLE__.CATA)
C     MAXEL  : NOMBRE MAX D'ELEMENTS DES MAILLES TRAITEES
C     MAXNO  : NOMBRE MAX DE NOEUDS DES NOUVEAUX ELEMENTS
C
C     RETOURNE LE TABLEAU DE DECOUPAGE DES ELEMENTS :
C     L'INDICE i EST LE NUMERO D'ORDRE DANS LE CATALOGUE
C       TDEC(i,.,.) : POI1 (NON DECOUPE)
C            i      : SEG2 (NON DECOUPE)
C            i      : SEG3
C            i      : SEG4
C            i      : TRIA3 (NON DECOUPE)
C            i      : TRIA6
C            i      : TRIA7
C            i      : QUAD4 (NON DECOUPE EN 1.2)
C            i      : QUAD8
C            i      : QUAD9
C            i      : TETRA4 (NON DECOUPE)
C            i      : TETRA10
C            i      : PENTA6 (NON DECOUPE EN 1.2)
C            i      : PENTA15
C            i      : PYRAM5 (NON DECOUPE EN 1.2)
C            i      : HEXA8 (NON DECOUPE EN 1.2)
C            i      : HEXA20
C            i      : HEXA27
C
C     TYPD DIT EN QUOI ON A DECOUPE ET COMBIEN
C     TYPD(.,1)=i CORRESPONDANT A POI1, SEG2, TRIA3, TETRA4 EN 1.0
C                           + QUAD4, PENTA6, PYRAM5, HEXA8 EN 1.2
C     TYPD(.,2)=NOMBRE D'ELEMENTS CREES
C     TYPD(.,3)=NOMBRE DE POINTS POUR CET ELEMENT (REMPLI A LA FIN)
C
C     VERS = 1 == '1.0'    VERSION DU
C          = 2 == '1.2'    FICHIER GMSH
C     EN VERSION 1.2, ON NE DECOUPE PAS LES QUAD4, PENTA6, PYRAM5, HEXA8
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER    TYPPOI, TYPSEG, TYPTRI, TYPTET, INO, I, J, K, IND
C
C --- VERIF
      IF(VERS.NE.1.AND.VERS.NE.2) GOTO 999
C
C --- INITIALISATIONS
      DO 10 I=1,NTYELE
         DO 11 J=1,MAXEL
            DO 11 K=1,MAXNO
               TDEC(I,J,K)=0
11       CONTINUE
         TYPD(I,1)=0
         TYPD(I,2)=0
         TYPD(I,3)=0
10    CONTINUE
C
      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'POI1'   ), TYPPOI )
      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'SEG2'   ), TYPSEG )
      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'TRIA3'  ), TYPTRI )
      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'TETRA4' ), TYPTET )
C
C
C --- ECLATEMENT POI1 EN 1 POI1
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','POI1'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPPOI
      TYPD(IND,2)=1
      TDEC(IND,1,1)=1
C
C --- ECLATEMENT SEG2 EN 1 SEG2
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG2'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPSEG
      TYPD(IND,2)=1
      TDEC(IND,1,1)=1
      TDEC(IND,1,2)=2
C
C --- ECLATEMENT SEG3 EN 2 SEG2
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG3'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPSEG
      TYPD(IND,2)=2
      TDEC(IND,1,1) = 1
      TDEC(IND,1,2) = 3
      TDEC(IND,2,1) = 3
      TDEC(IND,2,2) = 2
C
C --- ECLATEMENT SEG4 EN 3 SEG2
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','SEG4'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPSEG
      TYPD(IND,2)=3
      TDEC(IND,1,1) = 1
      TDEC(IND,1,2) = 3
      TDEC(IND,2,1) = 3
      TDEC(IND,2,2) = 4
      TDEC(IND,3,1) = 4
      TDEC(IND,3,2) = 2
C
C --- ECLATEMENT TRIA3 EN 1 TRIA3
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA3'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPTRI
      TYPD(IND,2)=1
      TDEC(IND,1,1)=1
      TDEC(IND,1,2)=2
      TDEC(IND,1,3)=3
C
C --- ECLATEMENT TRIA6 EN 4 TRIA3
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA6'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPTRI
      TYPD(IND,2)=4
      TDEC(IND,1,1) = 1
      TDEC(IND,1,2) = 4
      TDEC(IND,1,3) = 6
      TDEC(IND,2,1) = 4
      TDEC(IND,2,2) = 2
      TDEC(IND,2,3) = 5
      TDEC(IND,3,1) = 5
      TDEC(IND,3,2) = 3
      TDEC(IND,3,3) = 6
      TDEC(IND,4,1) = 4
      TDEC(IND,4,2) = 5
      TDEC(IND,4,3) = 6
C
C --- ECLATEMENT TRIA7 EN 4 TRIA3 COMME TRIA6
C --- CAR DX DY DZ INCONNUS SUR LE NOEUD 7 EN COQUE_3D
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TRIA7'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPTRI
      TYPD(IND,2)=4
      TDEC(IND,1,1) = 1
      TDEC(IND,1,2) = 4
      TDEC(IND,1,3) = 6
      TDEC(IND,2,1) = 4
      TDEC(IND,2,2) = 2
      TDEC(IND,2,3) = 5
      TDEC(IND,3,1) = 5
      TDEC(IND,3,2) = 3
      TDEC(IND,3,3) = 6
      TDEC(IND,4,1) = 4
      TDEC(IND,4,2) = 5
      TDEC(IND,4,3) = 6
C
C --- ECLATEMENT QUAD4 EN 2 TRIA3
C     OU NON DECOUPE
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD4'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      IF(VERS.EQ.1)THEN
         TYPD(IND,1)=TYPTRI
         TYPD(IND,2)=2
         TDEC(IND,1,1) = 1
         TDEC(IND,1,2) = 2
         TDEC(IND,1,3) = 3
         TDEC(IND,2,1) = 1
         TDEC(IND,2,2) = 3
         TDEC(IND,2,3) = 4
      ELSEIF(VERS.EQ.2)THEN
         TYPD(IND,1)=IND
         TYPD(IND,2)=1
         TDEC(IND,1,1) = 1
         TDEC(IND,1,2) = 2
         TDEC(IND,1,3) = 3
         TDEC(IND,1,4) = 4
      ENDIF
C
C --- ECLATEMENT QUAD8 EN 6 TRIA3
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD8'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPTRI
      TYPD(IND,2)=6
      TDEC(IND,1,1) = 1
      TDEC(IND,1,2) = 5
      TDEC(IND,1,3) = 8
      TDEC(IND,2,1) = 5
      TDEC(IND,2,2) = 2
      TDEC(IND,2,3) = 6
      TDEC(IND,3,1) = 6
      TDEC(IND,3,2) = 3
      TDEC(IND,3,3) = 7
      TDEC(IND,4,1) = 7
      TDEC(IND,4,2) = 4
      TDEC(IND,4,3) = 8
      TDEC(IND,5,1) = 5
      TDEC(IND,5,2) = 7
      TDEC(IND,5,3) = 8
      TDEC(IND,6,1) = 5
      TDEC(IND,6,2) = 6
      TDEC(IND,6,3) = 7
C
C --- ECLATEMENT QUAD9 EN 6 TRIA3 COMME QUAD8 
C --- CAR DX DY DZ INCONNUS SUR LE NOEUD 9 EN COQUE_3D
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','QUAD9'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPTRI
      TYPD(IND,2)=6
      TDEC(IND,1,1) = 1
      TDEC(IND,1,2) = 5
      TDEC(IND,1,3) = 8
      TDEC(IND,2,1) = 5
      TDEC(IND,2,2) = 2
      TDEC(IND,2,3) = 6
      TDEC(IND,3,1) = 6
      TDEC(IND,3,2) = 3
      TDEC(IND,3,3) = 7
      TDEC(IND,4,1) = 7
      TDEC(IND,4,2) = 4
      TDEC(IND,4,3) = 8
      TDEC(IND,5,1) = 5
      TDEC(IND,5,2) = 7
      TDEC(IND,5,3) = 8
      TDEC(IND,6,1) = 5
      TDEC(IND,6,2) = 6
      TDEC(IND,6,3) = 7
C
C --- ECLATEMENT TETRA4 EN 1 TETRA4
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TETRA4'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPTET
      TYPD(IND,2)=1
      TDEC(IND,1,1) = 1
      TDEC(IND,1,2) = 2
      TDEC(IND,1,3) = 3
      TDEC(IND,1,4) = 4
C
C --- ECLATEMENT TETRA10 EN 8 TETRA4
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','TETRA10'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPTET
      TYPD(IND,2)=8
      TDEC(IND,1,1) = 1
      TDEC(IND,1,2) = 5
      TDEC(IND,1,3) = 7
      TDEC(IND,1,4) = 8
      TDEC(IND,2,1) = 2
      TDEC(IND,2,2) = 9
      TDEC(IND,2,3) = 6
      TDEC(IND,2,4) = 5
      TDEC(IND,3,1) = 3
      TDEC(IND,3,2) = 6
      TDEC(IND,3,3) = 7
      TDEC(IND,3,4) = 10
      TDEC(IND,4,1) = 4
      TDEC(IND,4,2) = 8
      TDEC(IND,4,3) = 9
      TDEC(IND,4,4) = 10
      TDEC(IND,5,1) = 6
      TDEC(IND,5,2) = 7
      TDEC(IND,5,3) = 9
      TDEC(IND,5,4) = 5
      TDEC(IND,6,1) = 7
      TDEC(IND,6,2) = 8
      TDEC(IND,6,3) = 9
      TDEC(IND,6,4) = 5
      TDEC(IND,7,1) = 7
      TDEC(IND,7,2) = 8
      TDEC(IND,7,3) = 9
      TDEC(IND,7,4) = 10
      TDEC(IND,8,1) = 6
      TDEC(IND,8,2) = 7
      TDEC(IND,8,3) = 9
      TDEC(IND,8,4) = 10
C
C --- ECLATEMENT PENTA6 EN 3 TETRA4
C     OU NON DECOUPE
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','PENTA6'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      IF(VERS.EQ.1)THEN
         TYPD(IND,1)=TYPTET
         TYPD(IND,2)=3
         TDEC(IND,1,1) = 1
         TDEC(IND,1,2) = 2
         TDEC(IND,1,3) = 3
         TDEC(IND,1,4) = 5
         TDEC(IND,2,1) = 1
         TDEC(IND,2,2) = 3
         TDEC(IND,2,3) = 6
         TDEC(IND,2,4) = 5
         TDEC(IND,3,1) = 1
         TDEC(IND,3,2) = 6
         TDEC(IND,3,3) = 4
         TDEC(IND,3,4) = 5
      ELSEIF(VERS.EQ.2)THEN
         TYPD(IND,1)=IND
         TYPD(IND,2)=1
         TDEC(IND,1,1) = 1
         TDEC(IND,1,2) = 2
         TDEC(IND,1,3) = 3
         TDEC(IND,1,4) = 4
         TDEC(IND,1,5) = 5
         TDEC(IND,1,6) = 6
      ENDIF
C
C --- ECLATEMENT PENTA15 EN 16 TETRA4
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','PENTA15'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPTET
      TYPD(IND,2)=16
      TDEC(IND,1,1) = 4
      TDEC(IND,1,2) = 10
      TDEC(IND,1,3) = 13
      TDEC(IND,1,4) = 15
      TDEC(IND,2,1) = 1
      TDEC(IND,2,2) = 7
      TDEC(IND,2,3) = 9
      TDEC(IND,2,4) = 13
      TDEC(IND,3,1) = 1
      TDEC(IND,3,2) = 9
      TDEC(IND,3,3) = 15
      TDEC(IND,3,4) = 13
      TDEC(IND,4,1) = 1
      TDEC(IND,4,2) = 15
      TDEC(IND,4,3) = 10
      TDEC(IND,4,4) = 13
      TDEC(IND,5,1) = 5
      TDEC(IND,5,2) = 11
      TDEC(IND,5,3) = 13
      TDEC(IND,5,4) = 14
      TDEC(IND,6,1) = 2
      TDEC(IND,6,2) = 7
      TDEC(IND,6,3) = 8
      TDEC(IND,6,4) = 13
      TDEC(IND,7,1) = 2
      TDEC(IND,7,2) = 8
      TDEC(IND,7,3) = 14
      TDEC(IND,7,4) = 13
      TDEC(IND,8,1) = 2
      TDEC(IND,8,2) = 14
      TDEC(IND,8,3) = 11
      TDEC(IND,8,4) = 13
      TDEC(IND,9,1) = 6
      TDEC(IND,9,2) = 14
      TDEC(IND,9,3) = 15
      TDEC(IND,9,4) = 12
      TDEC(IND,10,1) = 14
      TDEC(IND,10,2) = 13
      TDEC(IND,10,3) = 15
      TDEC(IND,10,4) = 12
      TDEC(IND,11,1) = 3
      TDEC(IND,11,2) = 8
      TDEC(IND,11,3) = 14
      TDEC(IND,11,4) = 7
      TDEC(IND,12,1) = 3
      TDEC(IND,12,2) = 14
      TDEC(IND,12,3) = 13
      TDEC(IND,12,4) = 7
      TDEC(IND,13,1) = 3
      TDEC(IND,13,2) = 13
      TDEC(IND,13,3) = 9
      TDEC(IND,13,4) = 7
      TDEC(IND,14,1) = 3
      TDEC(IND,14,2) = 14
      TDEC(IND,14,3) = 12
      TDEC(IND,14,4) = 15
      TDEC(IND,15,1) = 3
      TDEC(IND,15,2) = 14
      TDEC(IND,15,3) = 13
      TDEC(IND,15,4) = 15
      TDEC(IND,16,1) = 2
      TDEC(IND,16,2) = 13
      TDEC(IND,16,3) = 15
      TDEC(IND,16,4) = 9
C
C --- ECLATEMENT PYRAM5 EN 2 TETRA4
C     OU NON DECOUPE
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','PYRAM5'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      IF(VERS.EQ.1)THEN
         TYPD(IND,1)=TYPTET
         TYPD(IND,2)=2
         TDEC(IND,1,1) = 1
         TDEC(IND,1,2) = 2
         TDEC(IND,1,3) = 4
         TDEC(IND,1,4) = 5
         TDEC(IND,2,1) = 2
         TDEC(IND,2,2) = 3
         TDEC(IND,2,3) = 4
         TDEC(IND,2,4) = 5
      ELSEIF(VERS.EQ.2)THEN
         TYPD(IND,1)=IND
         TYPD(IND,2)=1
         TDEC(IND,1,1) = 1
         TDEC(IND,1,2) = 2
         TDEC(IND,1,3) = 3
         TDEC(IND,1,4) = 4
         TDEC(IND,1,5) = 5
      ENDIF
C
C --- ECLATEMENT HEXA8 EN 6 TETRA4
C     OU NON DECOUPE
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','HEXA8'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      IF(VERS.EQ.1)THEN
         TYPD(IND,1)=TYPTET
         TYPD(IND,2)=6
         TDEC(IND,1,1) = 1
         TDEC(IND,1,2) = 2
         TDEC(IND,1,3) = 3
         TDEC(IND,1,4) = 5
         TDEC(IND,2,1) = 2
         TDEC(IND,2,2) = 3
         TDEC(IND,2,3) = 5
         TDEC(IND,2,4) = 7
         TDEC(IND,3,1) = 6
         TDEC(IND,3,2) = 2
         TDEC(IND,3,3) = 5
         TDEC(IND,3,4) = 7
         TDEC(IND,4,1) = 1
         TDEC(IND,4,2) = 4
         TDEC(IND,4,3) = 3
         TDEC(IND,4,4) = 7
         TDEC(IND,5,1) = 1
         TDEC(IND,5,2) = 7
         TDEC(IND,5,3) = 4
         TDEC(IND,5,4) = 8
         TDEC(IND,6,1) = 8
         TDEC(IND,6,2) = 5
         TDEC(IND,6,3) = 1
         TDEC(IND,6,4) = 7
      ELSEIF(VERS.EQ.2)THEN
         TYPD(IND,1)=IND
         TYPD(IND,2)=1
         TDEC(IND,1,1) = 1
         TDEC(IND,1,2) = 2
         TDEC(IND,1,3) = 3
         TDEC(IND,1,4) = 4
         TDEC(IND,1,5) = 5
         TDEC(IND,1,6) = 6
         TDEC(IND,1,7) = 7
         TDEC(IND,1,8) = 8
      ENDIF
C
C --- ECLATEMENT HEXA20 EN 24 TETRA4
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','HEXA20'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPTET
      TYPD(IND,2)=24
      TDEC(IND,1,1) = 1
      TDEC(IND,1,2) = 9
      TDEC(IND,1,3) = 12
      TDEC(IND,1,4) = 13
      TDEC(IND,2,1) = 2
      TDEC(IND,2,2) = 10
      TDEC(IND,2,3) = 14
      TDEC(IND,2,4) = 9
      TDEC(IND,3,1) = 3
      TDEC(IND,3,2) = 15
      TDEC(IND,3,3) = 11
      TDEC(IND,3,4) = 10
      TDEC(IND,4,1) = 4
      TDEC(IND,4,2) = 11
      TDEC(IND,4,3) = 12
      TDEC(IND,4,4) = 16
      TDEC(IND,5,1) = 7
      TDEC(IND,5,2) = 19
      TDEC(IND,5,3) = 18
      TDEC(IND,5,4) = 15
      TDEC(IND,6,1) = 6
      TDEC(IND,6,2) = 18
      TDEC(IND,6,3) = 17
      TDEC(IND,6,4) = 14
      TDEC(IND,7,1) = 5
      TDEC(IND,7,2) = 20
      TDEC(IND,7,3) = 17
      TDEC(IND,7,4) = 13
      TDEC(IND,8,1) = 9
      TDEC(IND,8,2) = 12
      TDEC(IND,8,3) = 13
      TDEC(IND,8,4) = 20
      TDEC(IND,9,1) = 9
      TDEC(IND,9,2) = 17
      TDEC(IND,9,3) = 16
      TDEC(IND,9,4) = 20
      TDEC(IND,10,1) = 17
      TDEC(IND,10,2) = 9
      TDEC(IND,10,3) = 13
      TDEC(IND,10,4) = 20
      TDEC(IND,11,1) = 9
      TDEC(IND,11,2) = 11
      TDEC(IND,11,3) = 12
      TDEC(IND,11,4) = 16
      TDEC(IND,12,1) = 9
      TDEC(IND,12,2) = 11
      TDEC(IND,12,3) = 16
      TDEC(IND,12,4) = 17
      TDEC(IND,13,1) = 11
      TDEC(IND,13,2) = 19
      TDEC(IND,13,3) = 16
      TDEC(IND,13,4) = 17
      TDEC(IND,14,1) = 16
      TDEC(IND,14,2) = 19
      TDEC(IND,14,3) = 20
      TDEC(IND,14,4) = 17
      TDEC(IND,15,1) = 9
      TDEC(IND,15,2) = 14
      TDEC(IND,15,3) = 11
      TDEC(IND,15,4) = 17
      TDEC(IND,16,1) = 14
      TDEC(IND,16,2) = 11
      TDEC(IND,16,3) = 17
      TDEC(IND,16,4) = 19
      TDEC(IND,17,1) = 9
      TDEC(IND,17,2) = 10
      TDEC(IND,17,3) = 11
      TDEC(IND,17,4) = 14
      TDEC(IND,18,1) = 10
      TDEC(IND,18,2) = 11
      TDEC(IND,18,3) = 14
      TDEC(IND,18,4) = 19
      TDEC(IND,19,1) = 18
      TDEC(IND,19,2) = 10
      TDEC(IND,19,3) = 14
      TDEC(IND,19,4) = 19
      TDEC(IND,20,1) = 18
      TDEC(IND,20,2) = 14
      TDEC(IND,20,3) = 17
      TDEC(IND,20,4) = 19
      TDEC(IND,21,1) = 10
      TDEC(IND,21,2) = 15
      TDEC(IND,21,3) = 11
      TDEC(IND,21,4) = 19
      TDEC(IND,22,1) = 15
      TDEC(IND,22,2) = 10
      TDEC(IND,22,3) = 18
      TDEC(IND,22,4) = 19
      TDEC(IND,23,1) = 8
      TDEC(IND,23,2) = 19
      TDEC(IND,23,3) = 20
      TDEC(IND,23,4) = 16
      TDEC(IND,24,1) = 12
      TDEC(IND,24,2) = 9
      TDEC(IND,24,3) = 16
      TDEC(IND,24,4) = 20
C
C --- ECLATEMENT HEXA27 EN 16 PENTA6 = 48 TETRA4
      CALL JENONU(JEXNOM('&CATA.TM.NOMTM','HEXA27'),IND)
      IF(IND.GT.NTYELE) GOTO 999
      TYPD(IND,1)=TYPTET
      TYPD(IND,2)=48
      TDEC(IND,1,1) = 9
      TDEC(IND,1,2) = 2
      TDEC(IND,1,3) = 14
      TDEC(IND,1,4) = 10
      TDEC(IND,2,1) = 9
      TDEC(IND,2,2) = 14
      TDEC(IND,2,3) = 23
      TDEC(IND,2,4) = 10
      TDEC(IND,3,1) = 9
      TDEC(IND,3,2) = 23
      TDEC(IND,3,3) = 21
      TDEC(IND,3,4) = 10
      TDEC(IND,4,1) = 9
      TDEC(IND,4,2) = 14
      TDEC(IND,4,3) = 22
      TDEC(IND,4,4) = 23
      TDEC(IND,5,1) = 9
      TDEC(IND,5,2) = 22
      TDEC(IND,5,3) = 27
      TDEC(IND,5,4) = 23
      TDEC(IND,6,1) = 9
      TDEC(IND,6,2) = 27
      TDEC(IND,6,3) = 21
      TDEC(IND,6,4) = 23
      TDEC(IND,7,1) = 1
      TDEC(IND,7,2) = 9
      TDEC(IND,7,3) = 22
      TDEC(IND,7,4) = 21
      TDEC(IND,8,1) = 1
      TDEC(IND,8,2) = 22
      TDEC(IND,8,3) = 27
      TDEC(IND,8,4) = 21
      TDEC(IND,9,1) = 1
      TDEC(IND,9,2) = 27
      TDEC(IND,9,3) = 12
      TDEC(IND,9,4) = 21
      TDEC(IND,10,1) = 1
      TDEC(IND,10,2) = 22
      TDEC(IND,10,3) = 13
      TDEC(IND,10,4) = 27
      TDEC(IND,11,1) = 1
      TDEC(IND,11,2) = 13
      TDEC(IND,11,3) = 25
      TDEC(IND,11,4) = 27
      TDEC(IND,12,1) = 1
      TDEC(IND,12,2) = 25
      TDEC(IND,12,3) = 12
      TDEC(IND,12,4) = 27
      TDEC(IND,13,1) = 22
      TDEC(IND,13,2) = 14
      TDEC(IND,13,3) = 6
      TDEC(IND,13,4) = 23
      TDEC(IND,14,1) = 22
      TDEC(IND,14,2) = 6
      TDEC(IND,14,3) = 18
      TDEC(IND,14,4) = 23
      TDEC(IND,15,1) = 22
      TDEC(IND,15,2) = 18
      TDEC(IND,15,3) = 27
      TDEC(IND,15,4) = 23
      TDEC(IND,16,1) = 22
      TDEC(IND,16,2) = 6
      TDEC(IND,16,3) = 17
      TDEC(IND,16,4) = 18
      TDEC(IND,17,1) = 22
      TDEC(IND,17,2) = 17
      TDEC(IND,17,3) = 26
      TDEC(IND,17,4) = 18
      TDEC(IND,18,1) = 22
      TDEC(IND,18,2) = 26
      TDEC(IND,18,3) = 27
      TDEC(IND,18,4) = 18
      TDEC(IND,19,1) = 13
      TDEC(IND,19,2) = 22
      TDEC(IND,19,3) = 17
      TDEC(IND,19,4) = 27
      TDEC(IND,20,1) = 13
      TDEC(IND,20,2) = 17
      TDEC(IND,20,3) = 26
      TDEC(IND,20,4) = 27
      TDEC(IND,21,1) = 13
      TDEC(IND,21,2) = 26
      TDEC(IND,21,3) = 25
      TDEC(IND,21,4) = 27
      TDEC(IND,22,1) = 13
      TDEC(IND,22,2) = 17
      TDEC(IND,22,3) = 5
      TDEC(IND,22,4) = 26
      TDEC(IND,23,1) = 13
      TDEC(IND,23,2) = 5
      TDEC(IND,23,3) = 20
      TDEC(IND,23,4) = 26
      TDEC(IND,24,1) = 13
      TDEC(IND,24,2) = 20
      TDEC(IND,24,3) = 25
      TDEC(IND,24,4) = 26
      TDEC(IND,25,1) = 21
      TDEC(IND,25,2) = 10
      TDEC(IND,25,3) = 23
      TDEC(IND,25,4) = 3
      TDEC(IND,26,1) = 21
      TDEC(IND,26,2) = 23
      TDEC(IND,26,3) = 15
      TDEC(IND,26,4) = 3
      TDEC(IND,27,1) = 21
      TDEC(IND,27,2) = 15
      TDEC(IND,27,3) = 11
      TDEC(IND,27,4) = 3
      TDEC(IND,28,1) = 21
      TDEC(IND,28,2) = 23
      TDEC(IND,28,3) = 27
      TDEC(IND,28,4) = 15
      TDEC(IND,29,1) = 21
      TDEC(IND,29,2) = 27
      TDEC(IND,29,3) = 24
      TDEC(IND,29,4) = 15
      TDEC(IND,30,1) = 21
      TDEC(IND,30,2) = 24
      TDEC(IND,30,3) = 11
      TDEC(IND,30,4) = 15
      TDEC(IND,31,1) = 12
      TDEC(IND,31,2) = 21
      TDEC(IND,31,3) = 27
      TDEC(IND,31,4) = 11
      TDEC(IND,32,1) = 12
      TDEC(IND,32,2) = 27
      TDEC(IND,32,3) = 24
      TDEC(IND,32,4) = 11
      TDEC(IND,33,1) = 12
      TDEC(IND,33,2) = 24
      TDEC(IND,33,3) = 4
      TDEC(IND,33,4) = 11
      TDEC(IND,34,1) = 12
      TDEC(IND,34,2) = 27
      TDEC(IND,34,3) = 25
      TDEC(IND,34,4) = 24
      TDEC(IND,35,1) = 12
      TDEC(IND,35,2) = 25
      TDEC(IND,35,3) = 16
      TDEC(IND,35,4) = 24
      TDEC(IND,36,1) = 12
      TDEC(IND,36,2) = 16
      TDEC(IND,36,3) = 4
      TDEC(IND,36,4) = 24
      TDEC(IND,37,1) = 27
      TDEC(IND,37,2) = 23
      TDEC(IND,37,3) = 18
      TDEC(IND,37,4) = 15
      TDEC(IND,38,1) = 17
      TDEC(IND,38,2) = 18
      TDEC(IND,38,3) = 7
      TDEC(IND,38,4) = 15
      TDEC(IND,39,1) = 27
      TDEC(IND,39,2) = 7
      TDEC(IND,39,3) = 24
      TDEC(IND,39,4) = 15
      TDEC(IND,40,1) = 27
      TDEC(IND,40,2) = 18
      TDEC(IND,40,3) = 26
      TDEC(IND,40,4) = 7
      TDEC(IND,41,1) = 27
      TDEC(IND,41,2) = 26
      TDEC(IND,41,3) = 19
      TDEC(IND,41,4) = 7
      TDEC(IND,42,1) = 27
      TDEC(IND,42,2) = 19
      TDEC(IND,42,3) = 24
      TDEC(IND,42,4) = 7
      TDEC(IND,43,1) = 25
      TDEC(IND,43,2) = 27
      TDEC(IND,43,3) = 26
      TDEC(IND,43,4) = 24
      TDEC(IND,44,1) = 25
      TDEC(IND,44,2) = 26
      TDEC(IND,44,3) = 19
      TDEC(IND,44,4) = 24
      TDEC(IND,45,1) = 25
      TDEC(IND,45,2) = 19
      TDEC(IND,45,3) = 16
      TDEC(IND,45,4) = 24
      TDEC(IND,46,1) = 25
      TDEC(IND,46,2) = 26
      TDEC(IND,46,3) = 20
      TDEC(IND,46,4) = 19
      TDEC(IND,47,1) = 25
      TDEC(IND,47,2) = 20
      TDEC(IND,47,3) = 8
      TDEC(IND,47,4) = 19
      TDEC(IND,48,1) = 25
      TDEC(IND,48,2) = 8
      TDEC(IND,48,3) = 16
      TDEC(IND,48,4) = 19
C
C     ------------------------------------------------------------------
C --- ON STOCKE LE NOMBRE DE NOEUDS POUR EVITER D'AVOIR A LE REFAIRE
C
      DO 101 IND=1,NTYELE
         IF(TYPD(IND,1).NE.0)THEN
            CALL JEVEUO(JEXNUM('&CATA.TM.NBNO',TYPD(IND,1)),'L',INO)
            TYPD(IND,3)=ZI(INO)
         ENDIF
 101  CONTINUE
C
C     ------------------------------------------------------------------
C
      GOTO 9000
C     VERIFICATION EMMELAGE DE PINCEAUX DU PROGRAMMEUR...
 999  CONTINUE
      CALL UTMESS('F','IRGMTB','ERREUR DE PROGRAMMATION')
C     ------------------------------------------------------------------
C
9000  CONTINUE
      END
