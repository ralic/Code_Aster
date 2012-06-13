      FUNCTION NOMIL(TYPMA,IA)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INCLUDE 'jeveux.h'
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
      ELSEIF (TYPMA.EQ.'PYRAM13') THEN
        CALL ASSERT(IA.LE.8)
        TAB(1)=6
        TAB(2)=7
        TAB(3)=8
        TAB(4)=9
        TAB(5)=10
        TAB(6)=11
        TAB(7)=12
        TAB(8)=13
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
