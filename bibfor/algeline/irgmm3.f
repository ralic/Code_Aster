      SUBROUTINE IRGMM3 ( NOMAIN, NOMAOU, NBMAT, NUMMAI, BASZ, NJVPOI,
     +            NJVSEG, NJVTRI, NJVTET, NBPOI, NBSEG, NBTRI, NBTET )
      IMPLICIT   NONE
      INTEGER             NBPOI, NBSEG, NBTRI, NBTET, NBMAT, NUMMAI(*)
      INTEGER             NBGRM
      CHARACTER*8         NOMAIN, NOMAOU
      CHARACTER*(*)       BASZ, NJVPOI, NJVSEG, NJVTRI, NJVTET
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 09/12/2002   AUTEUR PABHHHH N.TARDIEU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     TRANSFORME LE MAILLAGE "NOMAIN" EN UN MAILLAGE "NOMAOU"
C     LE MAILLAGE "NOMAOU" NE POSSEDE QUE DES MAILLES DE TYPE
C     POI1, SEG2, TRIA3, TETRA4
C     ------------------------------------------------------------------
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
      INTEGER      I, IMA, NBMA, NBMAIL, TYPPOI, TYPSEG, TYPTRI, TYPTET,
     +             NBNOMX, NBPT, INO, IMA2, IMAV, IATYMA, JREFE, JTITR
      INTEGER      JTYPM, JDIME, JPOI, JSEG, JTRI, JTET, JOPT, JNPT,
     +             NBMAC, JMAIL, IM, JNUMOL, JNBNUN, IDLIMA
      INTEGER      TSEG3(2,2), TSEG4(3,2), TTRI6(4,3), TTRI7(6,3),
     +             TQUA4(2,3), TQUA8(6,3), TQUA9(8,3), TTET10(8,4),
     +             TPEN6(3,4), TPEN15(16,4), TPYR5(2,4), THEX8(6,4),
     +             THEX20(24,4), THEX27(48,4)
      LOGICAL      LOGIC
      CHARACTER*1  BASE
      CHARACTER*8  K8B, NOMG, TYPM
      CHARACTER*24 NOMMAI, TYPMAI, CONNEX, NODIME, NOMNOE,
     &             COOVAL, COODSC, COOREF, TITRE, NUMOLD, NBNUNE
      CHARACTER*24 TYPMAV, CONNEV, NODIMV, NOMNOV,
     &             COOVAV, COODSV, COOREV
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ECLATEMENT SEG3 EN 2 SEG2
      TSEG3(1,1) = 1
      TSEG3(1,2) = 3
      TSEG3(2,1) = 3
      TSEG3(2,2) = 2
C
C --- ECLATEMENT SEG4 EN 3 SEG2
      TSEG4(1,1) = 1
      TSEG4(1,2) = 3
      TSEG4(2,1) = 3
      TSEG4(2,2) = 4
      TSEG4(3,1) = 4
      TSEG4(3,2) = 2
C
C --- ECLATEMENT TRIA6 EN 4 TRIA3
      TTRI6(1,1) = 1
      TTRI6(1,2) = 4
      TTRI6(1,3) = 6
      TTRI6(2,1) = 4
      TTRI6(2,2) = 2
      TTRI6(2,3) = 5
      TTRI6(3,1) = 5
      TTRI6(3,2) = 3
      TTRI6(3,3) = 6
      TTRI6(4,1) = 4
      TTRI6(4,2) = 5
      TTRI6(4,3) = 6
C
C --- ECLATEMENT TRIA7 EN 6 TRIA3
      TTRI7(1,1) = 1
      TTRI7(1,2) = 4
      TTRI7(1,3) = 7
      TTRI7(2,1) = 4
      TTRI7(2,2) = 2
      TTRI7(2,3) = 7
      TTRI7(3,1) = 2
      TTRI7(3,2) = 5
      TTRI7(3,3) = 7
      TTRI7(4,1) = 5
      TTRI7(4,2) = 3
      TTRI7(4,3) = 7
      TTRI7(5,1) = 3
      TTRI7(5,2) = 6
      TTRI7(5,3) = 7
      TTRI7(6,1) = 6
      TTRI7(6,2) = 1
      TTRI7(6,3) = 7
C
C --- ECLATEMENT QUAD4 EN 2 TRIA3
      TQUA4(1,1) = 1
      TQUA4(1,2) = 2
      TQUA4(1,3) = 3
      TQUA4(2,1) = 1
      TQUA4(2,2) = 3
      TQUA4(2,3) = 4
C
C --- ECLATEMENT QUAD8 EN 6 TRIA3
      TQUA8(1,1) = 1
      TQUA8(1,2) = 5
      TQUA8(1,3) = 8
      TQUA8(2,1) = 5
      TQUA8(2,2) = 2
      TQUA8(2,3) = 6
      TQUA8(3,1) = 6
      TQUA8(3,2) = 3
      TQUA8(3,3) = 7
      TQUA8(4,1) = 7
      TQUA8(4,2) = 4
      TQUA8(4,3) = 8
      TQUA8(5,1) = 5
      TQUA8(5,2) = 7
      TQUA8(5,3) = 8
      TQUA8(6,1) = 5
      TQUA8(6,2) = 6
      TQUA8(6,3) = 7
C
C --- ECLATEMENT QUAD9 EN 8 TRIA3
      TQUA9(1,1) = 1
      TQUA9(1,2) = 5
      TQUA9(1,3) = 9
      TQUA9(2,1) = 1
      TQUA9(2,2) = 9
      TQUA9(2,3) = 8
      TQUA9(3,1) = 8
      TQUA9(3,2) = 9
      TQUA9(3,3) = 7
      TQUA9(4,1) = 8
      TQUA9(4,2) = 8
      TQUA9(4,3) = 4
      TQUA9(5,1) = 5
      TQUA9(5,2) = 2
      TQUA9(5,3) = 6
      TQUA9(6,1) = 5
      TQUA9(6,2) = 6
      TQUA9(6,3) = 9
      TQUA9(7,1) = 9
      TQUA9(7,2) = 6
      TQUA9(7,3) = 3
      TQUA9(8,1) = 9
      TQUA9(8,2) = 3
      TQUA9(8,3) = 7
C
C --- ECLATEMENT TETRA10 EN 8 TETRA4
      TTET10(1,1) = 1
      TTET10(1,2) = 5
      TTET10(1,3) = 7
      TTET10(1,4) = 8
      TTET10(2,1) = 2
      TTET10(2,2) = 9
      TTET10(2,3) = 6
      TTET10(2,4) = 5
      TTET10(3,1) = 3
      TTET10(3,2) = 6
      TTET10(3,3) = 7
      TTET10(3,4) = 10
      TTET10(4,1) = 4
      TTET10(4,2) = 8
      TTET10(4,3) = 9
      TTET10(4,4) = 10
      TTET10(5,1) = 6
      TTET10(5,2) = 7
      TTET10(5,3) = 9
      TTET10(5,4) = 5
      TTET10(6,1) = 7
      TTET10(6,2) = 8
      TTET10(6,3) = 9
      TTET10(6,4) = 5
      TTET10(7,1) = 7
      TTET10(7,2) = 8
      TTET10(7,3) = 9
      TTET10(7,4) = 10
      TTET10(8,1) = 6
      TTET10(8,2) = 7
      TTET10(8,3) = 9
      TTET10(8,4) = 10
C
C --- ECLATEMENT PENTA6 EN 3 TETRA4
      TPEN6(1,1) = 1
      TPEN6(1,2) = 2
      TPEN6(1,3) = 3
      TPEN6(1,4) = 5
      TPEN6(2,1) = 1
      TPEN6(2,2) = 3
      TPEN6(2,3) = 6
      TPEN6(2,4) = 5
      TPEN6(3,1) = 1
      TPEN6(3,2) = 6
      TPEN6(3,3) = 4
      TPEN6(3,4) = 5
C
C --- ECLATEMENT PENTA15 EN 16 TETRA4
      TPEN15(1,1) = 4
      TPEN15(1,2) = 10
      TPEN15(1,3) = 13
      TPEN15(1,4) = 15
      TPEN15(2,1) = 1
      TPEN15(2,2) = 7
      TPEN15(2,3) = 9
      TPEN15(2,4) = 13
      TPEN15(3,1) = 1
      TPEN15(3,2) = 9
      TPEN15(3,3) = 15
      TPEN15(3,4) = 13
      TPEN15(4,1) = 1
      TPEN15(4,2) = 15
      TPEN15(4,3) = 10
      TPEN15(4,4) = 13
      TPEN15(5,1) = 5
      TPEN15(5,2) = 11
      TPEN15(5,3) = 13
      TPEN15(5,4) = 14
      TPEN15(6,1) = 2
      TPEN15(6,2) = 7
      TPEN15(6,3) = 8
      TPEN15(6,4) = 13
      TPEN15(7,1) = 2
      TPEN15(7,2) = 8
      TPEN15(7,3) = 14
      TPEN15(7,4) = 13
      TPEN15(8,1) = 2
      TPEN15(8,2) = 14
      TPEN15(8,3) = 11
      TPEN15(8,4) = 13
      TPEN15(9,1) = 6
      TPEN15(9,2) = 14
      TPEN15(9,3) = 15
      TPEN15(9,4) = 12
      TPEN15(10,1) = 14
      TPEN15(10,2) = 13
      TPEN15(10,3) = 15
      TPEN15(10,4) = 12
      TPEN15(11,1) = 3
      TPEN15(11,2) = 8
      TPEN15(11,3) = 14
      TPEN15(11,4) = 7
      TPEN15(12,1) = 3
      TPEN15(12,2) = 14
      TPEN15(12,3) = 13
      TPEN15(12,4) = 7
      TPEN15(13,1) = 3
      TPEN15(13,2) = 13
      TPEN15(13,3) = 9
      TPEN15(13,4) = 7
      TPEN15(14,1) = 3
      TPEN15(14,2) = 14
      TPEN15(14,3) = 12
      TPEN15(14,4) = 15
      TPEN15(15,1) = 3
      TPEN15(15,2) = 14
      TPEN15(15,3) = 13
      TPEN15(15,4) = 15
      TPEN15(16,1) = 2
      TPEN15(16,2) = 13
      TPEN15(16,3) = 15
      TPEN15(16,4) = 9
C
C --- ECLATEMENT PYRAM5 EN 2 TETRA4
      TPYR5(1,1) = 1
      TPYR5(1,2) = 2
      TPYR5(1,3) = 4
      TPYR5(1,4) = 5
      TPYR5(2,1) = 2
      TPYR5(2,2) = 3
      TPYR5(2,3) = 4
      TPYR5(2,4) = 5
C
C --- ECLATEMENT HEXA8 EN 6 TETRA4
      THEX8(1,1) = 1
      THEX8(1,2) = 2
      THEX8(1,3) = 3
      THEX8(1,4) = 5
      THEX8(2,1) = 2
      THEX8(2,2) = 3
      THEX8(2,3) = 5
      THEX8(2,4) = 7
      THEX8(3,1) = 6
      THEX8(3,2) = 2
      THEX8(3,3) = 5
      THEX8(3,4) = 7
      THEX8(4,1) = 2
      THEX8(4,2) = 4
      THEX8(4,3) = 3
      THEX8(4,4) = 7
      THEX8(5,1) = 4
      THEX8(5,2) = 2
      THEX8(5,3) = 6
      THEX8(5,4) = 7
      THEX8(6,1) = 8
      THEX8(6,2) = 4
      THEX8(6,3) = 6
      THEX8(6,4) = 7
C
C --- ECLATEMENT HEXA20 EN 24 TETRA4
      THEX20(1,1) = 1
      THEX20(1,2) = 9
      THEX20(1,3) = 12
      THEX20(1,4) = 13
      THEX20(2,1) = 2
      THEX20(2,2) = 10
      THEX20(2,3) = 14
      THEX20(2,4) = 9
      THEX20(3,1) = 3
      THEX20(3,2) = 15
      THEX20(3,3) = 11
      THEX20(3,4) = 10
      THEX20(4,1) = 4
      THEX20(4,2) = 11
      THEX20(4,3) = 12
      THEX20(4,4) = 16
      THEX20(5,1) = 7
      THEX20(5,2) = 19
      THEX20(5,3) = 18
      THEX20(5,4) = 15
      THEX20(6,1) = 6
      THEX20(6,2) = 18
      THEX20(6,3) = 17
      THEX20(6,4) = 14
      THEX20(7,1) = 5
      THEX20(7,2) = 20
      THEX20(7,3) = 17
      THEX20(7,4) = 13
      THEX20(8,1) = 9
      THEX20(8,2) = 12
      THEX20(8,3) = 13
      THEX20(8,4) = 20
      THEX20(9,1) = 9
      THEX20(9,2) = 17
      THEX20(9,3) = 16
      THEX20(9,4) = 20
      THEX20(10,1) = 17
      THEX20(10,2) = 9
      THEX20(10,3) = 13
      THEX20(10,4) = 20
      THEX20(11,1) = 9
      THEX20(11,2) = 11
      THEX20(11,3) = 12
      THEX20(11,4) = 16
      THEX20(12,1) = 9
      THEX20(12,2) = 11
      THEX20(12,3) = 16
      THEX20(12,4) = 17
      THEX20(13,1) = 11
      THEX20(13,2) = 19
      THEX20(13,3) = 16
      THEX20(13,4) = 17
      THEX20(14,1) = 16
      THEX20(14,2) = 19
      THEX20(14,3) = 20
      THEX20(14,4) = 17
      THEX20(15,1) = 9
      THEX20(15,2) = 14
      THEX20(15,3) = 11
      THEX20(15,4) = 17
      THEX20(16,1) = 14
      THEX20(16,2) = 11
      THEX20(16,3) = 17
      THEX20(16,4) = 19
      THEX20(17,1) = 9
      THEX20(17,2) = 10
      THEX20(17,3) = 11
      THEX20(17,4) = 14
      THEX20(18,1) = 10
      THEX20(18,2) = 11
      THEX20(18,3) = 14
      THEX20(18,4) = 19
      THEX20(19,1) = 18
      THEX20(19,2) = 10
      THEX20(19,3) = 14
      THEX20(19,4) = 19
      THEX20(20,1) = 18
      THEX20(20,2) = 14
      THEX20(20,3) = 17
      THEX20(20,4) = 19
      THEX20(21,1) = 10
      THEX20(21,2) = 15
      THEX20(21,3) = 11
      THEX20(21,4) = 19
      THEX20(22,1) = 15
      THEX20(22,2) = 10
      THEX20(22,3) = 18
      THEX20(22,4) = 19
      THEX20(23,1) = 8
      THEX20(23,2) = 19
      THEX20(23,3) = 20
      THEX20(23,4) = 16
      THEX20(24,1) = 12
      THEX20(24,2) = 9
      THEX20(24,3) = 16
      THEX20(24,4) = 20
C
C --- ECLATEMENT HEXA27 EN 16 PENTA6 = 48 TETRA4
      THEX27(1,1) = 9
      THEX27(1,2) = 2
      THEX27(1,3) = 14
      THEX27(1,4) = 10
      THEX27(2,1) = 9
      THEX27(2,2) = 14
      THEX27(2,3) = 23
      THEX27(2,4) = 10
      THEX27(3,1) = 9
      THEX27(3,2) = 23
      THEX27(3,3) = 21
      THEX27(3,4) = 10
      THEX27(4,1) = 9
      THEX27(4,2) = 14
      THEX27(4,3) = 22
      THEX27(4,4) = 23
      THEX27(5,1) = 9
      THEX27(5,2) = 22
      THEX27(5,3) = 27
      THEX27(5,4) = 23
      THEX27(6,1) = 9
      THEX27(6,2) = 27
      THEX27(6,3) = 21
      THEX27(6,4) = 23
      THEX27(7,1) = 1
      THEX27(7,2) = 9
      THEX27(7,3) = 22
      THEX27(7,4) = 21
      THEX27(8,1) = 1
      THEX27(8,2) = 22
      THEX27(8,3) = 27
      THEX27(8,4) = 21
      THEX27(9,1) = 1
      THEX27(9,2) = 27
      THEX27(9,3) = 12
      THEX27(9,4) = 21
      THEX27(10,1) = 1
      THEX27(10,2) = 22
      THEX27(10,3) = 13
      THEX27(10,4) = 27
      THEX27(11,1) = 1
      THEX27(11,2) = 13
      THEX27(11,3) = 25
      THEX27(11,4) = 27
      THEX27(12,1) = 1
      THEX27(12,2) = 25
      THEX27(12,3) = 12
      THEX27(12,4) = 27
      THEX27(13,1) = 22
      THEX27(13,2) = 14
      THEX27(13,3) = 6
      THEX27(13,4) = 23
      THEX27(14,1) = 22
      THEX27(14,2) = 6
      THEX27(14,3) = 18
      THEX27(14,4) = 23
      THEX27(15,1) = 22
      THEX27(15,2) = 18
      THEX27(15,3) = 27
      THEX27(15,4) = 23
      THEX27(16,1) = 22
      THEX27(16,2) = 6
      THEX27(16,3) = 17
      THEX27(16,4) = 18
      THEX27(17,1) = 22
      THEX27(17,2) = 17
      THEX27(17,3) = 26
      THEX27(17,4) = 18
      THEX27(18,1) = 22
      THEX27(18,2) = 26
      THEX27(18,3) = 27
      THEX27(18,4) = 18
      THEX27(19,1) = 13
      THEX27(19,2) = 22
      THEX27(19,3) = 17
      THEX27(19,4) = 27
      THEX27(20,1) = 13
      THEX27(20,2) = 17
      THEX27(20,3) = 26
      THEX27(20,4) = 27
      THEX27(21,1) = 13
      THEX27(21,2) = 26
      THEX27(21,3) = 25
      THEX27(21,4) = 27
      THEX27(22,1) = 13
      THEX27(22,2) = 17
      THEX27(22,3) = 5
      THEX27(22,4) = 26
      THEX27(23,1) = 13
      THEX27(23,2) = 5
      THEX27(23,3) = 20
      THEX27(23,4) = 26
      THEX27(24,1) = 13
      THEX27(24,2) = 20
      THEX27(24,3) = 25
      THEX27(24,4) = 26
      THEX27(25,1) = 21
      THEX27(25,2) = 10
      THEX27(25,3) = 23
      THEX27(25,4) = 3
      THEX27(26,1) = 21
      THEX27(26,2) = 23
      THEX27(26,3) = 15
      THEX27(26,4) = 3
      THEX27(27,1) = 21
      THEX27(27,2) = 15
      THEX27(27,3) = 11
      THEX27(27,4) = 3
      THEX27(28,1) = 21
      THEX27(28,2) = 23
      THEX27(28,3) = 27
      THEX27(28,4) = 15
      THEX27(29,1) = 21
      THEX27(29,2) = 27
      THEX27(29,3) = 24
      THEX27(29,4) = 15
      THEX27(30,1) = 21
      THEX27(30,2) = 24
      THEX27(30,3) = 11
      THEX27(30,4) = 15
      THEX27(31,1) = 12
      THEX27(31,2) = 21
      THEX27(31,3) = 27
      THEX27(31,4) = 11
      THEX27(32,1) = 12
      THEX27(32,2) = 27
      THEX27(32,3) = 24
      THEX27(32,4) = 11
      THEX27(33,1) = 12
      THEX27(33,2) = 24
      THEX27(33,3) = 4
      THEX27(33,4) = 11
      THEX27(34,1) = 12
      THEX27(34,2) = 27
      THEX27(34,3) = 25
      THEX27(34,4) = 24
      THEX27(35,1) = 12
      THEX27(35,2) = 25
      THEX27(35,3) = 16
      THEX27(35,4) = 24
      THEX27(36,1) = 12
      THEX27(36,2) = 16
      THEX27(36,3) = 4
      THEX27(36,4) = 24
      THEX27(37,1) = 27
      THEX27(37,2) = 23
      THEX27(37,3) = 18
      THEX27(37,4) = 15
      THEX27(38,1) = 17
      THEX27(38,2) = 18
      THEX27(38,3) = 7
      THEX27(38,4) = 15
      THEX27(39,1) = 27
      THEX27(39,2) = 7
      THEX27(39,3) = 24
      THEX27(39,4) = 15
      THEX27(40,1) = 27
      THEX27(40,2) = 18
      THEX27(40,3) = 26
      THEX27(40,4) = 7
      THEX27(41,1) = 27
      THEX27(41,2) = 26
      THEX27(41,3) = 19
      THEX27(41,4) = 7
      THEX27(42,1) = 27
      THEX27(42,2) = 19
      THEX27(42,3) = 24
      THEX27(42,4) = 7
      THEX27(43,1) = 25
      THEX27(43,2) = 27
      THEX27(43,3) = 26
      THEX27(43,4) = 24
      THEX27(44,1) = 25
      THEX27(44,2) = 26
      THEX27(44,3) = 19
      THEX27(44,4) = 24
      THEX27(45,1) = 25
      THEX27(45,2) = 19
      THEX27(45,3) = 16
      THEX27(45,4) = 24
      THEX27(46,1) = 25
      THEX27(46,2) = 26
      THEX27(46,3) = 20
      THEX27(46,4) = 19
      THEX27(47,1) = 25
      THEX27(47,2) = 20
      THEX27(47,3) = 8
      THEX27(47,4) = 19
      THEX27(48,1) = 25
      THEX27(48,2) = 8
      THEX27(48,3) = 16
      THEX27(48,4) = 19
C     ------------------------------------------------------------------
C
      BASE = BASZ
C
      NOMNOV = NOMAIN//'.NOMNOE         '
      TYPMAV = NOMAIN//'.TYPMAIL        '
      CONNEV = NOMAIN//'.CONNEX         '
      NODIMV = NOMAIN//'.DIME           '
      COOVAV = NOMAIN//'.COORDO    .VALE'
      COODSV = NOMAIN//'.COORDO    .DESC'
      COOREV = NOMAIN//'.COORDO    .REFE'
C
      NOMMAI = NOMAOU//'.NOMMAI         '
      NOMNOE = NOMAOU//'.NOMNOE         '
      TYPMAI = NOMAOU//'.TYPMAIL        '
      CONNEX = NOMAOU//'.CONNEX         '
      NODIME = NOMAOU//'.DIME           '
      COOVAL = NOMAOU//'.COORDO    .VALE'
      COODSC = NOMAOU//'.COORDO    .DESC'
      COOREF = NOMAOU//'.COORDO    .REFE'
      TITRE  = NOMAOU//'           .TITR'
      NUMOLD = NOMAOU//'.NUMOLD         '
      NBNUNE = NOMAOU//'.NBNUNE'
C
      CALL WKVECT ( TITRE, BASE//' V K80', 1, JTITR )
      ZK80(JTITR) = 'MAILLAGE CREE PAR IRGMMA POUR GMSH'
C
      CALL JEVEUO ( TYPMAV, 'L', JTYPM )
      CALL JEVEUO ( NODIMV, 'L', JDIME )
      NBMA = ZI(JDIME+3-1) 
C
      LOGIC = .FALSE.
      NBPOI = 0
      NBSEG = 0
      NBTRI = 0
      NBTET = 0
C
      IF ( NBMAT .NE. 0 ) THEN
         NBMAC = NBMAT
         CALL WKVECT ( '&&IRGMMA.NUME_MAILLE', 'V V I', NBMAC, JMAIL )
         DO 20 IMA = 1, NBMAC
            ZI(JMAIL+IMA-1) = NUMMAI(IMA)
 20      CONTINUE
      ELSE
         NBMAC = NBMA
         CALL WKVECT ( '&&IRGMMA.NUME_MAILLE', 'V V I', NBMAC, JMAIL )
         DO 22 IMA = 1, NBMAC
            ZI(JMAIL+IMA-1) = IMA
 22      CONTINUE
      ENDIF
C
      DO 10 IM = 1 , NBMAC
         IMA = ZI(JMAIL+IM-1)
C
         CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',ZI(JTYPM+IMA-1)),TYPM)
C
         IF (TYPM .EQ. 'POI1') THEN 
            NBPOI = NBPOI + 1
C
         ELSE IF (TYPM .EQ. 'SEG2') THEN 
            NBSEG = NBSEG + 1
C
         ELSE IF (TYPM .EQ. 'SEG3') THEN 
            NBSEG = NBSEG + 2
C
         ELSE IF (TYPM .EQ. 'SEG4') THEN 
            NBSEG = NBSEG + 3
C
         ELSE IF (TYPM .EQ. 'TRIA3') THEN
            NBTRI = NBTRI + 1
C
         ELSE IF (TYPM .EQ. 'TRIA6') THEN
            NBTRI = NBTRI + 4
C
         ELSE IF (TYPM .EQ. 'TRIA7') THEN
            NBTRI = NBTRI + 6
C
         ELSE IF (TYPM .EQ. 'QUAD4') THEN
            NBTRI = NBTRI + 2
C
         ELSE IF (TYPM .EQ. 'QUAD8') THEN
            NBTRI = NBTRI + 6
C
         ELSE IF (TYPM .EQ. 'QUAD9') THEN
            NBTRI = NBTRI + 8
C
         ELSE IF (TYPM .EQ. 'TETRA4') THEN 
            NBTET = NBTET + 1
C
         ELSE IF (TYPM .EQ. 'TETRA10') THEN 
            NBTET = NBTET + 8
C
         ELSE IF (TYPM .EQ. 'PENTA6') THEN 
            NBTET = NBTET + 3
C
         ELSE IF (TYPM .EQ. 'PENTA15') THEN 
            NBTET = NBTET + 16
C
         ELSE IF (TYPM .EQ. 'PYRAM5') THEN 
            NBTET = NBTET + 2
C
         ELSE IF (TYPM .EQ. 'PYRAM13') THEN 
            CALL UTMESS ('A','IRGMMA','ELEMENT '//TYPM//' NON TRAITE')
C
         ELSE IF (TYPM .EQ. 'HEXA8') THEN 
            NBTET = NBTET + 6
C
         ELSE IF (TYPM .EQ. 'HEXA20') THEN 
            NBTET = NBTET + 24
C
         ELSE IF (TYPM .EQ. 'HEXA27') THEN 
            NBTET = NBTET + 48
C
         ELSE
            CALL UTMESS ('A','IRGMMA','ELEMENT '//TYPM//' NON TRAITE')
C
         ENDIF
 10   CONTINUE
C
      NBMAIL = NBPOI + NBSEG + NBTRI + NBTET
C
      CALL WKVECT ( NJVPOI, 'V V I', MAX(1,NBPOI), JPOI )
      CALL WKVECT ( NJVSEG, 'V V I', MAX(1,NBSEG), JSEG )
      CALL WKVECT ( NJVTRI, 'V V I', MAX(1,NBTRI), JTRI )
      CALL WKVECT ( NJVTET, 'V V I', MAX(1,NBTET), JTET )
      CALL WKVECT ( NUMOLD, 'V V I', MAX(1,NBMAIL), JNUMOL )
      CALL WKVECT ( NBNUNE, 'V V I', NBMAC        , JNBNUN )
C
      CALL JEDUPO ( NODIMV, BASE, NODIME, LOGIC )
      CALL JEDUPO ( NOMNOV, BASE, NOMNOE, LOGIC )
      CALL JEDUPO ( COOVAV, BASE, COOVAL, LOGIC )
      CALL JEDUPO ( COODSV, BASE, COODSC, LOGIC )
      CALL JEDUPO ( COOREV, BASE, COOREF, LOGIC )
C
      CALL JEVEUO ( COOREF, 'E', JREFE )
      ZK24(JREFE) = NOMAOU
C
      CALL JEVEUO ( NODIME, 'E', JDIME )
      ZI(JDIME+3-1) = NBMAIL

C ----------------------------------------------------------------------
C     LE '.NOMMAI' ET LE '.CONNEX'
C ----------------------------------------------------------------------

      NBNOMX = 4
      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'POI1'   ), TYPPOI )
      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'SEG2'   ), TYPSEG )
      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'TRIA3'  ), TYPTRI )
      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'TETRA4' ), TYPTET )

      CALL JECREO ( NOMMAI, BASE//' N K8' )
      CALL JEECRA ( NOMMAI, 'NOMMAX', NBMAIL, ' ' )

      CALL WKVECT ( TYPMAI, BASE//' V I', NBMAIL, IATYMA )

      CALL JECREC ( CONNEX, BASE//' V I', 'NU', 'CONTIG', 'VARIABLE',
     +                                                    NBMAIL )
      CALL JEECRA ( CONNEX, 'LONT', NBNOMX*NBMAIL, ' ' )

      CALL JEDETC('V','&&IRGMMA.LISMA',1)
      CALL JECREC ( '&&IRGMMA.LISMA', 'V V I', 'NU', 'CONTIG', 
     +                                       'VARIABLE', NBMAIL )

      CALL JEECRA ( '&&IRGMMA.LISMA', 'LONT', NBMAIL, K8B) 

      NBPOI = 0
      NBSEG = 0
      NBTRI = 0
      NBTET = 0
      IMAV  = 0
C
      DO 100 IM = 1 , NBMAC
         IMA = ZI(JMAIL+IM-1)
C
        CALL JENUNO ( JEXNUM('&CATA.TM.NOMTM',ZI(JTYPM+IMA-1)), TYPM )
        CALL JEVEUO ( JEXNUM(CONNEV,IMA), 'L', JOPT )
C
         IF ( TYPM .EQ. 'POI1' ) THEN
C             ----------------
            NBPT = 1
            IMAV = IMAV + 1
            NOMG = 'MA      '
            CALL CODENT ( IMAV, 'G', NOMG(3:8) )
            CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    1, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
            ZI(IDLIMA) = IMA2
            ZI(IATYMA-1+IMA2) = TYPPOI
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
               ZI(JNBNUN-1+IMA )=1
C
            CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
            CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
            DO 102 INO = 0 , NBPT-1
               ZI(JNPT+INO) = ZI(JOPT+INO)
 102        CONTINUE
            NBPOI = NBPOI + 1
            ZI(JPOI-1+NBPOI) = IMAV
C
         ELSEIF ( TYPM .EQ. 'SEG2' ) THEN 
C                 ----------------
            NBPT = 2
            IMAV = IMAV + 1
            NOMG = 'MA      '
            CALL CODENT ( IMAV, 'G', NOMG(3:8) )
            CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    1, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
            ZI(IDLIMA) = IMA2
C
            ZI(IATYMA-1+IMA2) = TYPSEG
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
               ZI(JNBNUN-1+IMA )=1
C
            CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
            CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
            DO 104 INO = 0 , NBPT-1
               ZI(JNPT+INO) = ZI(JOPT+INO)
 104        CONTINUE
            NBSEG = NBSEG + 1
            ZI(JSEG-1+NBSEG) = IMAV
C
         ELSEIF ( TYPM .EQ. 'SEG3' ) THEN 
C                 ----------------
            NBPT = 2
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    2, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 206 I = 1 , 2
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPSEG
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 106 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TSEG3(I,INO))
 106           CONTINUE
               NBSEG = NBSEG + 1
               ZI(JSEG-1+NBSEG) = IMAV
 206        CONTINUE
            ZI(JNBNUN-1+IMA )=2
C
         ELSEIF ( TYPM .EQ. 'SEG4' ) THEN 
C                 ----------------
            NBPT = 2
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    3, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 208 I = 1 , 3
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPSEG
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 108 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TSEG4(I,INO))
 108           CONTINUE
               NBSEG = NBSEG + 1
               ZI(JSEG-1+NBSEG) = IMAV
 208        CONTINUE
            ZI(JNBNUN-1+IMA )=3
C
         ELSEIF ( TYPM .EQ. 'TRIA3' ) THEN
C                 -----------------
            NBPT = 3
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    1, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
            IMAV = IMAV + 1
            NOMG = 'MA      '
            CALL CODENT ( IMAV, 'G', NOMG(3:8) )
            CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
             CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
             ZI(IDLIMA+1-1) = IMA2
             ZI(IATYMA-1+IMA2) = TYPTRI
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
               ZI(JNBNUN-1+IMA) =1
C
            CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
            CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
            DO 110 INO = 1 , NBPT
               ZI(JNPT-1+INO) = ZI(JOPT-1+INO)
 110        CONTINUE
            NBTRI = NBTRI + 1
            ZI(JTRI-1+NBTRI) = IMAV
C
         ELSEIF ( TYPM .EQ. 'TRIA6' ) THEN
C                 -----------------
            NBPT = 3
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    4, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 212 I = 1 , 4
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTRI
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 112 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TTRI6(I,INO))
 112           CONTINUE
               NBTRI = NBTRI + 1
               ZI(JTRI-1+NBTRI) = IMAV
 212        CONTINUE
            ZI(JNBNUN-1+IMA )=4
C
         ELSEIF ( TYPM .EQ. 'TRIA7' ) THEN
C                 -----------------
            NBPT = 3
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    6, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 214 I = 1 , 6
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTRI
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 114 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TTRI7(I,INO))
 114           CONTINUE
               NBTRI = NBTRI + 1
               ZI(JTRI-1+NBTRI) = IMAV
 214        CONTINUE
            ZI(JNBNUN-1+IMA )=6
C
         ELSEIF ( TYPM .EQ. 'QUAD4' ) THEN
C                 -----------------
            NBPT = 3
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    2, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 216 I = 1 , 2
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTRI
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 116 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TQUA4(I,INO))
 116           CONTINUE
               NBTRI = NBTRI + 1
               ZI(JTRI-1+NBTRI) = IMAV
 216        CONTINUE
            ZI(JNBNUN-1+IMA )=2
C
         ELSEIF ( TYPM .EQ. 'QUAD8' ) THEN
C                 -----------------
            NBPT = 3
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    6, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 218 I = 1 , 6
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTRI
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 118 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TQUA8(I,INO))
 118           CONTINUE
               NBTRI = NBTRI + 1
               ZI(JTRI-1+NBTRI) = IMAV
 218        CONTINUE
            ZI(JNBNUN-1+IMA )=6
C
         ELSEIF ( TYPM .EQ. 'QUAD9' ) THEN
C                 -----------------
            NBPT = 3
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    8, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 220 I = 1 , 8
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTRI
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 120 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TQUA9(I,INO))
 120           CONTINUE
               NBTRI = NBTRI + 1
               ZI(JTRI-1+NBTRI) = IMAV
 220        CONTINUE
            ZI(JNBNUN-1+IMA )=8
C
         ELSEIF ( TYPM .EQ. 'TETRA4' ) THEN 
C                 ------------------
            NBPT = 4
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    1, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            IMAV = IMAV + 1
            NOMG = 'MA      '
            CALL CODENT ( IMAV, 'G', NOMG(3:8) )
            CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
            CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
            ZI(IDLIMA+1-1) = IMA2
            ZI(IATYMA-1+IMA2) = TYPTET
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
               ZI(JNBNUN-1+IMA) =1
C
            CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
            CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
            DO 122 INO = 1 , NBPT
               ZI(JNPT-1+INO) = ZI(JOPT-1+INO)
 122        CONTINUE
            NBTET = NBTET + 1
            ZI(JTET-1+NBTET) = IMAV
C
         ELSEIF ( TYPM .EQ. 'TETRA10' ) THEN 
C                 -------------------
            NBPT = 4
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    8, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 224 I = 1 , 8
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTET
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 124 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TTET10(I,INO))
 124           CONTINUE
               NBTET = NBTET + 1
               ZI(JTET-1+NBTET) = IMAV
 224        CONTINUE
            ZI(JNBNUN-1+IMA )=8
C
         ELSEIF ( TYPM .EQ. 'PENTA6' ) THEN 
C                 ------------------
            NBPT = 4
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    3, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 226 I = 1 , 3
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTET
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 126 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TPEN6(I,INO))
 126           CONTINUE
               NBTET = NBTET + 1
               ZI(JTET-1+NBTET) = IMAV
 226        CONTINUE
            ZI(JNBNUN-1+IMA )=3
C
         ELSEIF ( TYPM .EQ. 'PENTA15' ) THEN 
C                 -------------------
            NBPT = 4
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    16, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 228 I = 1 , 16
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTET
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 128 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TPEN15(I,INO))
 128           CONTINUE
               NBTET = NBTET + 1
               ZI(JTET-1+NBTET) = IMAV
 228        CONTINUE
            ZI(JNBNUN-1+IMA )=16
C
         ELSEIF ( TYPM .EQ. 'PYRAM5' ) THEN 
C                 ------------------
            NBPT = 4
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    2, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 230 I = 1 , 2
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTET
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 130 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+TPYR5(I,INO))
 130           CONTINUE
               NBTET = NBTET + 1
               ZI(JTET-1+NBTET) = IMAV
 230        CONTINUE
            ZI(JNBNUN-1+IMA )=2
C
         ELSEIF ( TYPM .EQ. 'HEXA8' ) THEN 
C                 -----------------
            NBPT = 4
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    6, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 232 I = 1 , 6
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTET
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 132 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+THEX8(I,INO))
 132           CONTINUE
               NBTET = NBTET + 1
               ZI(JTET-1+NBTET) = IMAV
 232        CONTINUE
            ZI(JNBNUN-1+IMA )=6
C
         ELSEIF ( TYPM .EQ. 'HEXA20' ) THEN 
C                 ------------------
            NBPT = 4
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    24, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 234 I = 1 , 24
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTET
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 134 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+THEX20(I,INO))
 134           CONTINUE
               NBTET = NBTET + 1
               ZI(JTET-1+NBTET) = IMAV
 234        CONTINUE
            ZI(JNBNUN-1+IMA )=24
C
         ELSEIF ( TYPM .EQ. 'HEXA27' ) THEN 
C                 ------------------
            NBPT = 4
            CALL JECROC ( JEXNUM( '&&IRGMMA.LISMA', IMA ) )
            CALL JEECRA ( JEXNUM( '&&IRGMMA.LISMA', IMA ), 'LONMAX',
     +                    24, K8B )
            CALL JEVEUO ( JEXNUM( '&&IRGMMA.LISMA', IMA), 'E', IDLIMA )
C
            DO 236 I = 1 , 24
               IMAV = IMAV + 1
               NOMG = 'MA      '
               CALL CODENT ( IMAV, 'G', NOMG(3:8) )
               CALL JECROC ( JEXNOM( NOMMAI, NOMG ) )
C
               CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
               ZI(IDLIMA+I-1) = IMA2
               ZI(IATYMA-1+IMA2) = TYPTET
C       STOCKAGE DU NUMERO DE LA MAILLE INITIALE DANS NUMOLD POUR IRGMCE
               ZI(JNUMOL-1+IMA2)=IMA
C
               CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
               CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
               DO 136 INO = 1 , NBPT
                  ZI(JNPT-1+INO) = ZI(JOPT-1+THEX27(I,INO))
 136           CONTINUE
               NBTET = NBTET + 1
               ZI(JTET-1+NBTET) = IMAV
 236        CONTINUE
            ZI(JNBNUN-1+IMA )=24
C
         ENDIF
 100  CONTINUE
C
      CALL JEDETR ( '&&IRGMMA.NUME_MAILLE' )
C
      CALL JEDEMA()
C
      END
