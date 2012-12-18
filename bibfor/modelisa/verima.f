      SUBROUTINE VERIMA(NOMZ, LIMANZ, LONLIM, TYPZ)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      INTEGER          LONLIM
      CHARACTER*(*)    NOMZ, LIMANZ(LONLIM), TYPZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     VERIFICATION DE L'APPARTENANCE DES OBJETS DE LA LISTE
C     LIMANO AU MAILLAGE NOMA
C
C IN       : NOMZ     : NOM DU MAILLAGE
C IN       : LIMANZ   : LISTE DE MAILLES OU DE NOEUDS OU DE GROUP_NO
C                       OU DE GROUP_MA
C IN       : LONLIM   : LONGUEUR DE LA LISTE LIMANO
C IN       : TYPZ     : TYPE DES OBJETS DE LA LISTE :
C                       MAILLE OU NOEUD OU GROUP_NO OU GROUP_MA
C ----------------------------------------------------------------------
C
      INTEGER IGR,IRET,INO,IMA
      CHARACTER*8   NOMA, TYPE
      CHARACTER*24  NOEUMA, GRNOMA, MAILMA, GRMAMA, LIMANO
      CHARACTER*24 VALK(2)
C ----------------------------------------------------------------------
C
      NOMA = NOMZ
      TYPE = TYPZ
C
C
      NOEUMA = NOMA//'.NOMNOE'
      GRNOMA = NOMA//'.GROUPENO'
      MAILMA = NOMA//'.NOMMAI'
      GRMAMA = NOMA//'.GROUPEMA'
C
      IF (TYPE.EQ.'GROUP_NO') THEN
C
C      --VERIFICATION DE L'APPARTENANCE DES GROUP_NO
C        AUX GROUP_NO DU MAILLAGE
C        -------------------------------------------------------
          CALL JEEXIN(GRNOMA,IRET)
          IF ( (LONLIM.NE.0).AND.(IRET.EQ.0) ) THEN
            VALK(1) = TYPE
            VALK(2) = NOMA
            CALL U2MESK('F','MODELISA7_12', 2, VALK)
          ENDIF
          DO 10 IGR = 1, LONLIM
              LIMANO = LIMANZ(IGR)
              CALL JENONU (JEXNOM(GRNOMA,LIMANO),IRET)
              IF (IRET .EQ. 0) THEN
                 VALK(1) = LIMANO
                 VALK(2) = NOMA
                 CALL U2MESK('S','MODELISA7_75', 2 ,VALK)
              ENDIF
10        CONTINUE
C
      ELSE IF (TYPE.EQ.'NOEUD') THEN
C
C      --VERIFICATION DE L'APPARTENANCE DES NOEUDS
C        AUX NOEUDS DU MAILLAGE
C        -------------------------------------------------------
            CALL JEEXIN(NOEUMA,IRET)
            IF ( (LONLIM.NE.0).AND.(IRET.EQ.0) ) THEN
              VALK(1) = TYPE
              VALK(2) = NOMA
              CALL U2MESK('F','MODELISA7_12', 2, VALK)
            ENDIF
            DO 20 INO = 1, LONLIM
              LIMANO = LIMANZ(INO)
              CALL JENONU (JEXNOM(NOEUMA,LIMANO),IRET)
              IF (IRET .EQ. 0) THEN
                 VALK(1) = LIMANO
                 VALK(2) = NOMA
                 CALL U2MESK('S','MODELISA7_76', 2 ,VALK)
              ENDIF
 20         CONTINUE
C
      ELSE IF (TYPE.EQ.'GROUP_MA') THEN
C
C      --VERIFICATION DE L'APPARTENANCE DES GROUP_MA
C        AUX GROUP_MA DU MAILLAGE
C        -------------------------------------------------------
            CALL JEEXIN(GRMAMA,IRET)
            IF ( (LONLIM.NE.0).AND.(IRET.EQ.0) ) THEN
              VALK(1) = TYPE
              VALK(2) = NOMA
              CALL U2MESK('F','MODELISA7_12', 2, VALK)
            ENDIF
            DO 30 IGR = 1, LONLIM
              LIMANO = LIMANZ(IGR)
              CALL JENONU (JEXNOM(GRMAMA,LIMANO),IRET)
              IF (IRET .EQ. 0) THEN
                 VALK(1) = LIMANO
                 VALK(2) = NOMA
                 CALL U2MESK('S','MODELISA7_77', 2 ,VALK)
              ENDIF
 30         CONTINUE
C
      ELSE IF (TYPE.EQ.'MAILLE') THEN
C
C      --VERIFICATION DE L'APPARTENANCE DES MAILLES
C        AUX MAILLES DU MAILLAGE
C        -------------------------------------------------------
            CALL JEEXIN(MAILMA,IRET)
            IF ( (LONLIM.NE.0).AND.(IRET.EQ.0) ) THEN
              VALK(1) = TYPE
              VALK(2) = NOMA
              CALL U2MESK('F','MODELISA7_12', 2, VALK)
            ENDIF
            DO 40 IMA = 1, LONLIM
              LIMANO = LIMANZ(IMA)
              CALL JENONU (JEXNOM(MAILMA,LIMANO),IRET)
              IF (IRET .EQ. 0) THEN
                 VALK(1) = LIMANO
                 VALK(2) = NOMA
                 CALL U2MESK('S','MODELISA6_10', 2 ,VALK)
              ENDIF
 40        CONTINUE
C
      ELSE
           CALL U2MESK('S','MODELISA7_79',1,TYPE)
      ENDIF
      END
