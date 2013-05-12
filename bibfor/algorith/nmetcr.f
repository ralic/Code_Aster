      SUBROUTINE NMETCR(MODELE,COMPOR,FONACT,SDDYNA,SDPOST,
     &                  DEFICO,RESOCO,SDIETO,CARELE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/04/2013   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*24 SDIETO,MODELE,COMPOR
      INTEGER      FONACT(*)
      CHARACTER*19 SDDYNA,SDPOST
      CHARACTER*24 DEFICO,RESOCO,CARELE
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (GESTION IN ET OUT)
C
C CREATION DE LA SD IN ET OUT
C
C ----------------------------------------------------------------------
C
C
C IN  MODELE : NOM DU MODELE
C IN  COMPOR : CARTE COMPORTEMENT
C IN  SDDYNA : SD DYNAMIQUE
C IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
C IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
C IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
C IN  RESOCO : SD DE RESOLUTION DU CONTACT
C IN  SDIETO : SD GESTION IN ET OUT
C IN  CARELE : SD CARA_ELEM
C
C ----------------------------------------------------------------------
C
      INTEGER      IFM,NIV
      CHARACTER*32 JEXNUM
      INTEGER      ZIOCH,NBMAX
      PARAMETER    (ZIOCH = 10,NBMAX=20 )
      INTEGER      NBCHAM,NBCHIN,NBCHOU
      CHARACTER*24 IOINFO,IOLCHA
      INTEGER      JIOINF,JIOLCH
      INTEGER      ICHAM,ICH
      LOGICAL      CHAACT(NBMAX)
      CHARACTER*8  RESULT,K8BID
      CHARACTER*19 RESU19
      INTEGER      ICHSY,NBNOSY
      CHARACTER*24 CHARCH,CHNOMS,NOMSYM
      CHARACTER*24 CHETIN,CHINIT,CHOPER
      LOGICAL      LFIND
C
      CHARACTER*24 NOMCHS(NBMAX),MOTCOB(NBMAX)
      CHARACTER*24 NOMGD(NBMAX),MOTCEI(NBMAX),LOCCHA(NBMAX)
      LOGICAL      LARCH(NBMAX),LETIN(NBMAX)
C -- NOM DU CHAMP DANS LA SD RESULTAT
      DATA NOMCHS  /'DEPL'        ,'SIEF_ELGA'   ,'VARI_ELGA'   ,
     &              'COMPORTEMENT','VITE'        ,'ACCE'        ,
     &              'INDC_ELEM'   ,'SECO_ELEM'   ,'COHE_ELEM'   ,
     &              'VALE_CONT'   ,'MODE_FLAMB'  ,'DEPL_VIBR'   ,
     &              'DEPL_ABSOLU' ,'VITE_ABSOLU' ,'ACCE_ABSOLU' ,
     &              'FORC_NODA'   ,'STRX_ELGA'   ,'MODE_STAB'   ,
     &              'FORC_AMOR'   ,'FORC_LIAI'/
C -- NOM DE LA GRANDEUR
      DATA NOMGD   /'DEPL_R','SIEF_R','VARI_R',
     &              'COMPOR','DEPL_R','DEPL_R',
     &              'NEUT_I','NEUT_R','NEUT_R',
     &              'DEPL_R','DEPL_R','DEPL_R',
     &              'DEPL_R','DEPL_R','DEPL_R',
     &              'DEPL_R','STRX_R','DEPL_R',
     &              'DEPL_R','DEPL_R'/
C -- MOT-CLEF DANS ETAT_INIT, ' ' SI PAS DE MOT-CLEF
      DATA MOTCEI  /'DEPL','SIGM','VARI',
     &              ' '   ,'VITE','ACCE',
     &              ' '   ,' '   ,' '   ,
     &              ' '   ,' '   ,' '   ,
     &              ' '   ,' '   ,' '   ,
     &              ' '   ,'STRX',' '   ,
     &              ' '   ,' '/
C -- LOCALISATION DU CHAMP
      DATA LOCCHA  /'NOEU','ELGA','ELGA',
     &              'ELGA','NOEU','NOEU',
     &              'ELEM','ELEM','ELEM',
     &              'NOEU','NOEU','NOEU',
     &              'NOEU','NOEU','NOEU',
     &              'NOEU','ELGA','NOEU',
     &              'NOEU','NOEU'/
C -- .TRUE. SI CHAMP EST LU DANS ETAT_INIT
      DATA LETIN   /.TRUE. ,.TRUE. ,.TRUE. ,
     &              .FALSE.,.TRUE. ,.TRUE. ,
     &              .TRUE. ,.TRUE. ,.TRUE. ,
     &              .FALSE.,.FALSE.,.FALSE.,
     &              .TRUE. ,.TRUE. ,.TRUE. ,
     &              .FALSE.,.TRUE. ,.FALSE.,
     &              .TRUE. ,.TRUE./
C -- .TRUE. SI CHAMP EST ECRIT DANS ARCHIVAGE
      DATA LARCH   /.TRUE. ,.TRUE. ,.TRUE. ,
     &              .TRUE. ,.TRUE. ,.TRUE. ,
     &              .TRUE. ,.TRUE. ,.TRUE. ,
     &              .TRUE. ,.TRUE. ,.TRUE. ,
     &              .TRUE. ,.TRUE. ,.TRUE. ,
     &              .FALSE.,.TRUE. ,.TRUE. ,
     &              .TRUE. ,.TRUE./
C -- MOT-CLEF DANS OBSERVATION, ' ' SI PAS DE MOT-CLEF
      DATA MOTCOB  /'DEPL'        ,'SIEF_ELGA'   ,'VARI_ELGA'   ,
     &              ' '           ,'VITE'        ,'ACCE'        ,
     &              ' '           ,' '           ,' '           ,
     &              'VALE_CONT'   ,' '           ,' '           ,
     &              'DEPL_ABSOLU' ,'VITE_ABSOLU' ,'ACCE_ABSOLU' ,
     &              'FORC_NODA'   ,'STRX_ELGA'   ,' '           ,
     &              ' '           ,' '/
C
C !!!!!!! NE PAS OUBLIER D'AJOUTER LE CHAMP DANS RSCRSD
C

C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... CREATION DE LA SD GESTION'//
     &                ' IN ET OUT'
      ENDIF
C
C --- INITIALISATIONS
C
      NBCHAM = 0
      NBCHIN = 0
      NBCHOU = 0
      DO 1 ICHAM=1,NBMAX
        CHAACT(ICHAM) = .FALSE.
    1 CONTINUE
      RESULT = '&&NMETCR'
      RESU19 = RESULT
C
C --- ACTIVATION DES CHAMPS A TRAITER SUIVANT FONCTIONNALITES ACTIVEES
C
      CALL NMETAC(FONACT,SDDYNA,DEFICO,NBMAX ,CHAACT)
C
C --- DECOMPTE DES CHAMPS
C
      DO 20 ICHAM = 1,NBMAX
        IF (CHAACT(ICHAM)) THEN
          NBCHAM = NBCHAM + 1
          IF (LETIN(ICHAM)) NBCHIN = NBCHIN + 1
          IF (LARCH(ICHAM)) NBCHOU = NBCHOU + 1
        ENDIF
   20 CONTINUE
C
C --- CREATION SD CHAMPS
C
      IOLCHA = SDIETO(1:19)//'.LCHA'
      CALL WKVECT(IOLCHA,'V V K24',NBCHAM*ZIOCH,JIOLCH)
C
C --- AJOUT DES CHAMPS
C
      ICH = 0
      DO 30 ICHAM = 1,NBMAX
        IF (CHAACT(ICHAM)) THEN
          ICH = ICH + 1
          CALL NMETCI(SDIETO,ZIOCH ,ICH   ,
     &                NOMCHS(ICHAM),NOMGD (ICHAM),
     &                MOTCEI(ICHAM),MOTCOB(ICHAM),
     &                LOCCHA(ICHAM),
     &                LETIN (ICHAM),LARCH (ICHAM))
        ENDIF
   30 CONTINUE
      CALL ASSERT(ICH.EQ.NBCHAM)
C
C --- NOM DES CHAMPS DANS OP0070
C
      CALL NMETCC(SDIETO,COMPOR,SDDYNA,SDPOST,RESOCO,
     &            NBCHAM,ZIOCH )
C
C --- NOM DES CHAMPS NULS
C
      CALL NMETC0(MODELE,SDIETO,COMPOR,RESOCO,NBCHAM,
     &            ZIOCH,CARELE )
C
C --- CREATION SD INFOS
C
      IOINFO = SDIETO(1:19)//'.INFO'
      CALL WKVECT(IOINFO,'V V I',4,JIOINF)
      ZI(JIOINF+1-1) = NBCHAM
      ZI(JIOINF+2-1) = NBCHIN
      ZI(JIOINF+3-1) = NBCHOU
      ZI(JIOINF+4-1) = ZIOCH
C
C --- VERIFICATIONS CONFORMITE SD RESULTAT
C
      CALL RSCRSD('V',RESULT,'EVOL_NOLI',1)
      CALL JELIRA(RESU19//'.DESC','NOMMAX',NBNOSY,K8BID)
      DO 50 ICHAM = 1,NBCHAM
        CHARCH = ZK24(JIOLCH+ZIOCH*(ICHAM-1)+9-1)
        CHNOMS = ZK24(JIOLCH+ZIOCH*(ICHAM-1)+1-1)
        CHETIN = ZK24(JIOLCH+ZIOCH*(ICHAM-1)+8-1)
        CHINIT = ZK24(JIOLCH+ZIOCH*(ICHAM-1)+2-1)
        CHOPER = ZK24(JIOLCH+ZIOCH*(ICHAM-1)+6-1)
        IF (CHARCH.EQ.'OUI') THEN
          LFIND = .FALSE. 
          DO 55 ICHSY = 1,NBNOSY
            CALL JENUNO(JEXNUM(RESU19//'.DESC',ICHSY),NOMSYM)
            IF (NOMSYM.EQ.CHNOMS) LFIND = .TRUE.
  55      CONTINUE
C ------- DECLENCHEMENT DU ASSERT -> OUBLI D'IMPACT DANS RSCRSD !
          CALL ASSERT(LFIND)
        ENDIF
C ----- DECLENCHEMENT DU ASSERT -> OUBLI D'IMPACT DANS NMETC0 !
        IF (CHETIN.EQ.'OUI') THEN
          IF (CHINIT.EQ.' ') CALL ASSERT(.FALSE.)
        ENDIF
C ----- DECLENCHEMENT DU ASSERT -> OUBLI D'IMPACT DANS NMETCC !
        IF (CHOPER.EQ.' ') CALL ASSERT(.FALSE.)
   50 CONTINUE
      CALL DETRSD('RESULTAT',RESULT)
C
      CALL JEDEMA()
      END
