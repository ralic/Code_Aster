      FUNCTION NOMIL(TYPMA,IA)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      CHARACTER*8  TYPMA
      INTEGER      NOMIL,IA

C-----------------------------------------------------------------------
C     BUT: RETOURNE LE NUMÉRO LOCAL DU NOEUD MILIEU PORTÉ SUR L'ARETE
C                   D'UNE MAILLE QUADRATIQUE

C ARGUMENTS D'ENTRÉE:
C      TYPMA     : TYPE DE LA MAILLE
C      IA        : NUMÉRO LOCAL DE L'ARETE

C-----------------------------------------------------------------------
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C --- VARIABLES

      INTEGER      TAB(12)

      CALL ASSERT(IA.GT.0)

      IF (TYPMA.EQ.'HEXA20') THEN
        CALL ASSERT(IA.LE.12)
        TAB(1)=9
        TAB(2)=10
        TAB(3)=11
        TAB(4)=12
        TAB(5)=17
        TAB(6)=18
        TAB(7)=19
        TAB(8)=20
        TAB(9)=13
        TAB(10)=14
        TAB(11)=15
        TAB(12)=16
      ELSEIF (TYPMA.EQ.'PENTA15') THEN
        CALL ASSERT(IA.LE.9)
        TAB(1)=7
        TAB(2)=8
        TAB(3)=9
        TAB(4)=13
        TAB(5)=14
        TAB(6)=15
        TAB(7)=10
        TAB(8)=11
        TAB(9)=12
      ELSEIF (TYPMA.EQ.'TETRA10') THEN
        CALL ASSERT(IA.LE.6)
        TAB(1)=5
        TAB(2)=7
        TAB(3)=8
        TAB(4)=6
        TAB(5)=9
        TAB(6)=10
      ELSEIF (TYPMA.EQ.'QUAD8') THEN
        CALL ASSERT(IA.LE.4)
        TAB(1)=5
        TAB(2)=6
        TAB(3)=7
        TAB(4)=8
      ELSEIF (TYPMA.EQ.'TRIA6') THEN
        CALL ASSERT(IA.LE.3)
        TAB(1)=4
        TAB(2)=5
        TAB(3)=6
      ELSE
        CALL U2MESK('F','ALGORITH8_92',1,TYPMA)
      ENDIF

      NOMIL=TAB(IA)

      END
