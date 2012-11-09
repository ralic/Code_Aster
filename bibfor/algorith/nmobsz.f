      SUBROUTINE NMOBSZ(SDOBSE,NOMTAB,TITOBS,NOMCHA,TYPCHA,
     &                  EXTRCH,EXTRCP,EXTRGA,NOMCMP,NOMNOE,
     &                  NOMMAI,NUM   ,SNUM  ,INSTAN,VALR  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'

      CHARACTER*19 SDOBSE,NOMTAB
      CHARACTER*4  TYPCHA
      CHARACTER*24 NOMCHA
      CHARACTER*80 TITOBS
      CHARACTER*8  EXTRCH,EXTRCP,EXTRGA
      CHARACTER*8  NOMNOE,NOMMAI,NOMCMP
      INTEGER      NUM,SNUM
      REAL*8       INSTAN,VALR
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (OBSERVATION - UTILITAIRE)
C
C SAUVEGARDE DANS LA TABLE
C
C ----------------------------------------------------------------------
C
C
C IN  SDOBSE : SD OBSERVATION
C IN  NOMTAB : NOM DE LA TABLE
C IN  TITOBS : TITRE DE L'OBSERVATION
C IN  TYPCHA : TYPE DU CHAMP
C IN  NOMCHA : NOM DU CHAMP
C IN  NOMCMP : NOM DE LA COMPOSANTE
C IN  NOMNOE : NOM DU NOEUD
C IN  NOMMAI : NOM DE LA MAILLE
C IN  NUM    : NUMERO POINT DE GAUSS
C IN  SNUM   : NUMERO SOUS-POINT DE GAUSS
C IN  NUOBSV : NUMERO DE L'OBSERVATIONC
C IN  INSTAN : VALEUR DE L'INSTANT
C IN  VALR   : VALEUR A SAUVEGARDER DANS LA TABLE
C
C ----------------------------------------------------------------------
C
      INTEGER      NBPARA
      PARAMETER   (NBPARA=16)
      CHARACTER*16 NOPARA(NBPARA)
      INTEGER      NPAR
      CHARACTER*24 OBSINF
      INTEGER      JOBSIN
      INTEGER      NUMREU,NUMOBS
      COMPLEX*16   C16BID
      CHARACTER*16 TYPOBJ
      CHARACTER*24 NOMSD
      REAL*8       TABR(NBPARA)
      INTEGER      TABI(NBPARA)
      CHARACTER*24 TABK(NBPARA),NOPARZ(NBPARA)
      INTEGER      IPAR,KVAL,IVAL,RVAL
C
      DATA NOPARA/'NOM_OBSERVATION','TYPE_OBJET'  ,'NOM_SD' ,
     &            'NUME_REUSE'     ,'NUME_OBSE'   ,'INST'   ,
     &            'NOM_CHAM'       ,'EVAL_CHAM'   ,'NOM_CMP',
     &            'EVAL_CMP'       ,'NOEUD'       ,'MAILLE' ,
     &            'EVAL_ELGA'      ,'POINT'       ,'SOUS_POINT',
     &            'VALE'           /
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- SD PRINCIPALE (INFO)
C
      OBSINF = SDOBSE(1:14)//'     .INFO'
      CALL JEVEUO(OBSINF,'E',JOBSIN)
C
C --- INITIALISATIONS
C
      IPAR   = 1
      KVAL   = 1
      IVAL   = 1
      RVAL   = 1
      TYPOBJ = 'R'
      NOMSD  = ' '
      NUMOBS = ZI(JOBSIN-1+3)
      NUMREU = ZI(JOBSIN-1+4)
C
C --- CE QUI EST COMMUN
C
      NOPARZ(IPAR) = NOPARA(1)
      IPAR         = IPAR + 1
      TABK(KVAL)   = TITOBS
      KVAL         = KVAL + 1

      NOPARZ(IPAR) = NOPARA(2)
      IPAR         = IPAR + 1
      TABK(KVAL)   = TYPOBJ
      KVAL         = KVAL + 1

      NOPARZ(IPAR) = NOPARA(3)
      IPAR         = IPAR + 1
      TABK(KVAL)   = NOMSD
      KVAL         = KVAL + 1

      NOPARZ(IPAR) = NOPARA(4)
      IPAR         = IPAR + 1
      TABI(IVAL)   = NUMREU
      IVAL         = IVAL + 1

      NOPARZ(IPAR) = NOPARA(5)
      IPAR         = IPAR + 1
      TABI(IVAL)   = NUMOBS
      IVAL         = IVAL + 1

      NOPARZ(IPAR) = NOPARA(6)
      IPAR         = IPAR + 1
      TABR(RVAL)   = INSTAN
      RVAL         = RVAL + 1

      NOPARZ(IPAR) = NOPARA(7)
      IPAR         = IPAR + 1
      TABK(KVAL)   = NOMCHA
      KVAL         = KVAL + 1
C
C --- EXTRACTION DU CHAMP: TYPE
C
      NOPARZ(IPAR) = NOPARA(8)
      IPAR         = IPAR + 1
      TABK(KVAL)   = EXTRCH
      KVAL         = KVAL + 1
C
C --- EXTRACTION DES COMPOSANTES: TYPE
C
      IF (EXTRCP.EQ.' ') THEN
        NOPARZ(IPAR) = NOPARA(9)
        IPAR         = IPAR + 1
        TABK(KVAL)   = NOMCMP
        KVAL         = KVAL + 1
      ELSE
        NOPARZ(IPAR) = NOPARA(10)
        IPAR         = IPAR + 1
        TABK(KVAL)   = EXTRCP
        KVAL         = KVAL + 1
      ENDIF
C
C --- NOEUD OU MAILLE
C
      IF (TYPCHA.EQ.'NOEU') THEN
        IF (EXTRCH.EQ.'VALE') THEN
          NOPARZ(IPAR) = NOPARA(11)
          IPAR         = IPAR + 1
          TABK(KVAL)   = NOMNOE
          KVAL         = KVAL + 1
        ELSE
          NOPARZ(IPAR) = NOPARA(8)
          IPAR         = IPAR + 1
          TABK(KVAL)   = EXTRCH
          KVAL         = KVAL + 1
        ENDIF
        NOPARZ(IPAR) = NOPARA(16)
        IPAR         = IPAR + 1
        TABR(RVAL)   = VALR
        RVAL         = RVAL + 1
      ELSEIF (TYPCHA.EQ.'ELGA') THEN
        IF (EXTRCH.EQ.'VALE') THEN
          NOPARZ(IPAR) = NOPARA(12)
          IPAR         = IPAR + 1
          TABK(KVAL)   = NOMMAI
          KVAL         = KVAL + 1
        ELSE
          NOPARZ(IPAR) = NOPARA(8)
          IPAR         = IPAR + 1
          TABK(KVAL)   = EXTRCH
          KVAL         = KVAL + 1
        ENDIF
        IF (EXTRGA.EQ.'VALE') THEN
          NOPARZ(IPAR) = NOPARA(14)
          IPAR         = IPAR + 1
          TABI(IVAL)   = NUM
          IVAL         = IVAL + 1
          NOPARZ(IPAR) = NOPARA(15)
          IPAR         = IPAR + 1
          TABI(IVAL)   = SNUM
          IVAL         = IVAL + 1
          NOPARZ(IPAR) = NOPARA(16)
          IPAR         = IPAR + 1
          TABR(RVAL)   = VALR
          RVAL         = RVAL + 1
        ELSE
          NOPARZ(IPAR) = NOPARA(13)
          IPAR         = IPAR + 1
          TABK(KVAL)   = EXTRGA
          KVAL         = KVAL + 1
          NOPARZ(IPAR) = NOPARA(16)
          IPAR         = IPAR + 1
          TABR(RVAL)   = VALR
          RVAL         = RVAL + 1
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      NPAR = IPAR -1
C
C --- AJOUT DANS LA TABLE
C
      CALL TBAJLI(NOMTAB,NPAR  ,NOPARZ,TABI  ,TABR  ,
     &            C16BID,TABK  ,0     )
C
C --- OBSERVATION SUIVANTE
C
      ZI(JOBSIN-1+3) = ZI(JOBSIN-1+3) + 1
C
      CALL JEDEMA()
C
      END
