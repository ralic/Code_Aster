      SUBROUTINE XMOIMP(NH8,NH20,NP6,NP15,NP5,NP13,NT4,NT10,NCPQ4,
     &                  NCPQ8,NCPT3,NCPT6,NDPQ4,NDPQ8,NDPT3,NDPT6,
     &                  NF4,NF8,NF3,NF6,NPF2,NPF3,NAXT3,NAXQ4,
     &                  NAXQ8,NAXT6,NAX2,NAX3,NTH8,NTP6,NTP5,NTT4,
     &                  NTPQ4,NTPT3,NTAQ4,NTAT3,NTF4,NTF3,NTPF2,NTAX2)
      IMPLICIT NONE

      INCLUDE 'jeveux.h'
      INTEGER       NH8(14),NH20(7),NP6(14),NP15(7),NP5(14),NP13(7)
      INTEGER       NT4(14),NT10(7)
      INTEGER       NCPQ4(14),NCPQ8(7),NCPT3(14),NCPT6(7), NDPQ4(14)
      INTEGER       NDPQ8(7),NDPT3(14),NDPT6(7),NF4(11),NF8(7),NF3(11)
      INTEGER       NF6(7),NPF2(11),NPF3(7)
      INTEGER       NAXT3(7),NAXQ4(7),NAXQ8(7),NAXT6(7),NAX2(7),NAX3(7)
      INTEGER       NTH8(7),NTP6(7),NTP5(7),NTT4(7),NTPQ4(7),NTPT3(7)
      INTEGER       NTAQ4(7),NTAT3(7),NTF4(7),NTF3(7),NTPF2(7),NTAX2(7)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/04/2013   AUTEUR CUVILLIE M.CUVILLIEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT
C TOLE CRP_21
C
C
C ----------------------------------------------------------------------
C
C ROUTINE XFEM APPELEE PAR MODI_MODELE_XFEM (OP0113)
C
C    BUT : IMPRIMER LES ELEMENT X-FEM
C
C ----------------------------------------------------------------------
C
C
C
C
      INTEGER     IFM,NIV,NBELX
C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFDBG('XFEM',IFM,NIV)

      WRITE(IFM,*)'IMPRESSION DES ELEMENTS X-FEM '
      WRITE(IFM,807)'TYPE','XH','XT','XHT','XHC','XTC','XHTC',
     &'XH1','XH2','XH3','XH4','XH2C','XH3C','XH4C'

C     ELEMENTS MECANIQUES
      IF(NH8(7).NE.0) WRITE(IFM,810)'HEXA8'  ,
     &    NH8(1)  ,NH8(2)  ,NH8(3),  NH8(4)  ,NH8(5)  ,NH8(6),
     &    NH8(8)  ,NH8(9),  NH8(10), NH8(11) ,
     &    NH8(12) ,NH8(13), NH8(14)
      IF(NH20(7).NE.0)
     &    WRITE(IFM,808)'HEXA20'  ,NH20(1) ,NH20(2) ,NH20(3),
     &                             NH20(4) ,NH20(5) ,NH20(6)
      IF(NP6(7).NE.0) WRITE(IFM,810)'PENTA6'  ,
     &    NP6(1)  ,NP6(2)  ,NP6(3),  NP6(4)  ,NP6(5)  ,NP6(6),
     &    NP6(8)  ,NP6(9),  NP6(10), NP6(11) ,
     &    NP6(12) ,NP6(13), NP6(14)
      IF(NP15(7).NE.0)
     &    WRITE(IFM,808)'PENTA15' ,NP15(1) ,NP15(2) ,NP15(3),
     &                             NP15(4) ,NP15(5) ,NP15(6)
      IF(NP5(7).NE.0) WRITE(IFM,810)'PYRAM5'  ,
     &    NP5(1)  ,NP5(2)  ,NP5(3)  ,NP5(4)  ,NP5(5) ,NP5(6),
     &    NP5(8)  ,NP5(9)  ,NP5(10) ,NP5(11) ,
     &    NP5(12) ,NP5(13) ,NP5(14)
      IF(NP13(7).NE.0)
     &    WRITE(IFM,808)'PYRAM13' ,NP13(1) ,NP13(2) ,NP13(3),
     &                             NP13(4) ,NP13(5) ,NP13(6)
      IF(NT4(7).NE.0) WRITE(IFM,810)'TETRA4'  ,
     &    NT4(1)  ,NT4(2)  ,NT4(3)  ,NT4(4)  ,NT4(5) ,NT4(6),
     &    NT4(8)  ,NT4(9)  ,NT4(10) ,NT4(11) ,
     &    NT4(12) ,NT4(13) ,NT4(14)
      IF(NT10(7).NE.0)
     &    WRITE(IFM,808)'TETRA10' ,NT10(1) ,NT10(2) ,NT10(3),
     &                             NT10(4) ,NT10(5) ,NT10(6)
      IF(NCPQ4(7).NE.0) WRITE(IFM,810)'CP QUAD4',
     &    NCPQ4(1),NCPQ4(2),NCPQ4(3),NCPQ4(4),NCPQ4(5),NCPQ4(6),
     &    NCPQ4(8),NCPQ4(9),NCPQ4(10),NCPQ4(11),
     &    NCPQ4(12),NCPQ4(13),NCPQ4(14)
      IF(NCPQ8(7).NE.0)
     &    WRITE(IFM,808)'CP QUAD8',NCPQ8(1),NCPQ8(2),NCPQ8(3),
     &                             NCPQ8(4),NCPQ8(5),NCPQ8(6)
      IF(NCPT3(7).NE.0) WRITE(IFM,810)'CP TRIA3',
     &    NCPT3(1),NCPT3(2),NCPT3(3),NCPT3(4),NCPT3(5),NCPT3(6),
     &    NCPT3(8),NCPT3(9),NCPT3(10),NCPT3(11),
     &    NCPT3(12),NCPT3(13),NCPT3(14)
      IF(NCPT6(7).NE.0)
     &    WRITE(IFM,808)'CP TRIA6',NCPT6(1),NCPT6(2),NCPT6(3),
     &                             NCPT6(4),NCPT6(5),NCPT6(6)
      IF(NDPQ4(7).NE.0) WRITE(IFM,810)'DP QUAD4',
     &    NDPQ4(1),NDPQ4(2),NDPQ4(3),NDPQ4(4),NDPQ4(5),NDPQ4(6),
     &    NDPQ4(8),NDPQ4(9),NDPQ4(10),NDPQ4(11),
     &    NDPQ4(12),NDPQ4(13),NDPQ4(14)
      IF(NDPQ8(7).NE.0)
     &    WRITE(IFM,808)'DP QUAD8',NDPQ8(1),NDPQ8(2),NDPQ8(3),
     &                             NDPQ8(4),NDPQ8(5),NDPQ8(6)
      IF(NDPT3(7).NE.0) WRITE(IFM,810)'DP TRIA3',
     &    NDPT3(1),NDPT3(2),NDPT3(3),NDPT3(4),NDPT3(5),NDPT3(6),
     &    NDPT3(8),NDPT3(9),NDPT3(10),NDPT3(11),
     &    NDPT3(12),NDPT3(13),NDPT3(14)
      IF(NDPT6(7).NE.0)
     &    WRITE(IFM,808)'DP TRIA6',NDPT6(1),NDPT6(2),NDPT6(3),
     &                             NDPT6(4),NDPT6(5),NDPT6(6)
      IF(NAXQ4(7).NE.0) WRITE(IFM,809)'AXI QUAD4',
     &    NAXQ4(1),NAXQ4(2),NAXQ4(3),NAXQ4(4),NAXQ4(5),NAXQ4(6)

      IF(NAXQ8(7).NE.0)
     &    WRITE(IFM,808)'AXI QUAD8',NAXQ8(1),NAXQ8(2),NAXQ8(3),
     &                             NAXQ8(4),NAXQ8(5),NAXQ8(6)
      IF(NAXT3(7).NE.0) WRITE(IFM,808)'AXI TRIA3',
     &    NAXT3(1),NAXT3(2),NAXT3(3),NAXT3(4),NAXT3(5),NAXT3(6)

      IF(NAXT6(7).NE.0)
     &    WRITE(IFM,808)'AXI TRIA6',NAXT6(1),NAXT6(2),NAXT6(3),
     &                             NAXT6(4),NAXT6(5),NAXT6(6)
            IF(NF4(7).NE.0) WRITE(IFM,809)'FACE4',
     &    NF4(1)  ,NF4(2)  ,NF4(3)  ,NF4(4)  ,NF4(5)  ,NF4(6),
     &    NF4(8)  ,NF4(9)  ,NF4(10) ,NF4(11)
      IF(NF8(7).NE.0)
     &    WRITE(IFM,808)'FACE8'   ,NF8(1)  ,NF8(2)  ,NF8(3)
      IF(NF3(7).NE.0)  WRITE(IFM,809)'FACE3',
     &   NF3(1)  ,NF3(2)  ,NF3(3)  ,NF3(4)  ,NF3(5)  ,NF3(6),
     &   NF3(8)  ,NF3(9)  ,NF3(10) ,NF3(11)
      IF(NF6(7).NE.0)
     &    WRITE(IFM,808)'FACE6'   ,NF6(1)  ,NF6(2)  ,NF6(3)
      IF(NPF2(7).NE.0) WRITE(IFM,809)'ARETE 2',
     &    NPF2(1) ,NPF2(2) ,NPF2(3), NPF2(4) ,NPF2(5) ,NPF2(6),
     &    NPF2(8) ,NPF2(9) ,NPF2(10),NPF2(11)
      IF(NPF3(7).NE.0)
     &    WRITE(IFM,808)'ARETE 3' ,NPF3(1) ,NPF3(2) ,NPF3(3)
      IF(NAX2(7).NE.0)
     &    WRITE(IFM,808)'ARETE-AXI 2' ,NAX2(1) ,NAX2(2) ,NAX2(3)
      IF(NAX3(7).NE.0)
     &    WRITE(IFM,808)'ARETE-AXI 3' ,NAX3(1) ,NAX3(2) ,NAX3(3)
      WRITE(IFM,*)'  '

C     ELEMENTS THERMIQUES
      IF(NTH8(7).NE.0) WRITE(IFM,810)'HEXA8'  ,
     &    NTH8(1), NTH8(2), NTH8(3), NTH8(4), NTH8(5), 
     &    NTH8(6), 0      , 0      , 0      , 0      ,
     &    0      , 0      , 0      
      IF(NTP6(7).NE.0) WRITE(IFM,810)'PENTA6'  ,
     &    NTP6(1), NTP6(2), NTP6(3), NTP6(4), NTP6(5), 
     &    NTP6(6), 0      , 0      , 0      , 0      ,
     &    0      , 0      , 0      
      IF(NTP5(7).NE.0) WRITE(IFM,810)'PYRAM5'  ,
     &    NTP5(1), NTP5(2), NTP5(3), NTP5(4), NTP5(5), 
     &    NTP5(6), 0      , 0      , 0      , 0      ,
     &    0      , 0      , 0      
      IF(NTT4(7).NE.0) WRITE(IFM,810)'TETRA4'  ,
     &    NTT4(1), NTT4(2), NTT4(3), NTT4(4), NTT4(5), 
     &    NTT4(6), 0      , 0      , 0      , 0      ,
     &    0      , 0      , 0      
      IF(NTPQ4(7).NE.0) WRITE(IFM,810)'PLAN QUAD4'  ,
     &    NTPQ4(1),NTPQ4(2),NTPQ4(3),NTPQ4(4),NTPQ4(5), 
     &    NTPQ4(6),0       ,0        ,0      ,0       ,
     &    0       ,0       ,0      
      IF(NTPT3(7).NE.0) WRITE(IFM,810)'PLAN TRIA3'  ,
     &    NTPT3(1),NTPT3(2),NTPT3(3),NTPT3(4),NTPT3(5), 
     &    NTPT3(6),0       ,0        ,0      ,0       ,
     &    0       ,0       ,0      

      IF(NTAQ4(7).NE.0) WRITE(IFM,810)'AXI QUAD4'  ,
     &    NTAQ4(1),NTAQ4(2),NTAQ4(3),NTAQ4(4),NTAQ4(5), 
     &    NTAQ4(6),0       ,0        ,0      ,0       ,
     &    0       ,0       ,0      
      IF(NTAT3(7).NE.0) WRITE(IFM,810)'AXI TRIA3'  ,
     &    NTAT3(1),NTAT3(2),NTAT3(3),NTAT3(4),NTAT3(5), 
     &    NTAT3(6),0       ,0        ,0      ,0       ,
     &    0       ,0       ,0      

      IF(NTF4(7).NE.0) WRITE(IFM,810)'FACE4'  ,
     &    NTF4(1), NTF4(2), NTF4(3), NTF4(4), NTF4(5), 
     &    NTF4(6), 0      , 0      , 0      , 0      ,
     &    0      , 0      , 0      
      IF(NTF3(7).NE.0) WRITE(IFM,810)'FACE3'  ,
     &    NTF3(1), NTF3(2), NTF3(3), NTF3(4), NTF3(5), 
     &    NTF3(6), 0      , 0      , 0      , 0       ,
     &    0      , 0      , 0      

      IF(NTPF2(7).NE.0) WRITE(IFM,810)'ARETE 2'  ,
     &    NTPF2(1),NTPF2(2),NTPF2(3),NTPF2(4),NTPF2(5), 
     &    NTPF2(6),0       ,0        ,0      ,0       ,
     &    0       ,0       ,0      

      IF(NTAX2(7).NE.0) WRITE(IFM,810)'ARETE-AXI 2'  ,
     &    NTAX2(1),NTAX2(2),NTAX2(3),NTAX2(4),NTAX2(5), 
     &    NTAX2(6),0       ,0        ,0      ,0       ,
     &    0       ,0       ,0      

      NBELX =   NH8(7) + NH20(7) + NP6(7) + NP15(7)
     &        + NP5(7) + NP13(7) + NT4(7) + NT10(7)
     &        + NCPQ4(7) + NCPQ8(7) + NCPT3(7) + NCPT6(7)
     &        + NDPQ4(7) + NDPQ8(7) + NDPT3(7) + NDPT6(7)
     &        + NAXQ4(7) + NAXQ8(7) + NAXT3(7) + NAXT6(7)
     &        + NAX2(7) + NAX3(7) + NPF2(7) + NPF3(7)
     &        + NF3(7) + NF6(7) + NTH8(7) + NTP6(7) + NTP5(7) 
     &        + NTT4(7) + NTPQ4(7) + NTPT3(7) + NTAQ4(7) 
     &        + NTAT3(7) + NTF4(7) + NTF3(7) + NTPF2(7) 
     &        + NTAX2(7)

      IF (NBELX.EQ.0) CALL U2MESS('F','XFEM_16')

 807  FORMAT (5X,A19,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,
     &        2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6)
 808  FORMAT (5X,A19,2X,I6,2X,I6,2X,I6,2X,I6,2X,I6,2X,I6)
 809  FORMAT (5X,A19,2X,I6,2X,I6,2X,I6,2X,I6,2X,I6,2X,I6,
     &        I6,2X,I6,2X,I6,2X,I6,2X,I6)
 810  FORMAT (5X,A19,2X,I6,2X,I6,2X,I6,2X,I6,2X,I6,2X,I6,
     &        I6,2X,I6,2X,I6,2X,I6,2X,I6,2X,I6,2X,I6,2X,I6)

      CALL JEDEMA()
      END
