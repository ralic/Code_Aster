      SUBROUTINE ACEVPO(NBOCC,NLM,NLG,IER)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER           NBOCC,NLM,NLG,IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     VERIFICATION DES MOTS CLES POUR L'ELEMENT POUTRE
C ----------------------------------------------------------------------
C IN  : NBOCC  : NOMBRE D'OCCURENCE
C OUT : NLM    : NOMBRE TOTAL DE MAILLE
C OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
C ----------------------------------------------------------------------
      REAL*8        R8B
      LOGICAL       BON
      CHARACTER*8   K8B, NOMU, CARA(100), KIOC
      CHARACTER*16  SEC, VSEC, CONCEP, CMD
      INTEGER       VALI(3)
      INTEGER      IARG
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IOC ,NC ,NCAR ,NG ,NM ,NS 
      INTEGER NSOM ,NV ,NVAL ,NVS 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL GETRES(NOMU,CONCEP,CMD)
C
      NLM = 0
      NLG = 0
      DO 100 IOC = 1 , NBOCC
         CALL CODENT(IOC,'G',KIOC)
         CALL GETVTX ( 'POUTRE', 'GROUP_MA' , IOC,IARG,0,  K8B, NG )
         CALL GETVTX ( 'POUTRE', 'MAILLE'   , IOC,IARG,0,  K8B, NM )
         CALL GETVTX ( 'POUTRE', 'SECTION'  , IOC,IARG,1,  SEC, NS )
         CALL GETVTX ( 'POUTRE', 'VARI_SECT', IOC,IARG,1, VSEC, NVS)
         CALL GETVTX ( 'POUTRE', 'CARA'     , IOC,IARG,0,  K8B, NC )
         NCAR = -NC
         CALL GETVTX ( 'POUTRE', 'CARA'     , IOC,IARG,NCAR,CARA,NC)
         CALL GETVR8 ( 'POUTRE', 'VALE'     , IOC,IARG,0,  R8B, NV )
         NVAL = -NV
C
         IF ( NVAL .NE. NCAR ) THEN
            VALI (1) = IOC
            VALI (2) = NCAR
            VALI (3) = NVAL
            CALL U2MESG('E','MODELISA9_31',0,' ',3,VALI,0,0.D0)
            IER = IER + 1
         ENDIF
C
         IF ( SEC .EQ. 'RECTANGLE' ) THEN
            IF ( VSEC .EQ. 'AFFINE' ) THEN

            ENDIF
         ELSEIF ( SEC .EQ. 'CERCLE' ) THEN
            IF ( VSEC .EQ. 'CONSTANT' ) THEN
               BON = .FALSE.
               DO 20 I = 1 , NCAR
                  IF ( CARA(I) .EQ. 'R' ) BON = .TRUE.
 20            CONTINUE
               IF ( .NOT. BON ) THEN
                  CALL U2MESK('E','MODELISA_66',1,KIOC)
                  IER = IER + 1
               ENDIF
            ENDIF
         ENDIF
C
C ---    GROUP_MA + GROUP_NO + NOEUD + MAILLE
         NSOM = NG + NM
         IF (NSOM.EQ.NG .OR. NSOM.EQ.NM) THEN
            NLM = MAX(NLM,-NM)
            NLG = MAX(NLG,-NG)
         ENDIF
C
 100  CONTINUE
C
      CALL JEDEMA()
      END
