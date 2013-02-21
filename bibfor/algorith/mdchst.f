      SUBROUTINE MDCHST ( NUMDDL, TYPNUM, IMODE, IAMOR, PULSAT,
     &                    MASGEN,AMOGEN,LFLU,NBNLI,NBPAL,NOECHO,NBRFIS,
     &                    LOGCHO, PARCHO, INTITU, DDLCHO, IER )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER             NBNLI, IAMOR, IMODE, IER, LOGCHO(NBNLI,*),
     &                    DDLCHO(*),NBRFIS
      REAL*8              PARCHO(NBNLI,*),PULSAT(*),MASGEN(*),AMOGEN(*)
      LOGICAL             LFLU
      CHARACTER*8         NOECHO(NBNLI,*), INTITU(*)
      CHARACTER*14        NUMDDL
      CHARACTER*16        TYPNUM, TYPFRO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/02/2013   AUTEUR PELLET J.PELLET 
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
C
C     ROUTINE APPELEE PAR MDCHOC
C     TRAITEMENT DU CAS OU NUME_DDL = 'NUME_DDL_SDASTER'
C
C IN  : NUMDDL : NOM DE LA NUMEROTATION
C IN  : TYPNUM : TYPE DE LA NUMEROTATION
C IN  : IMODE  : NUMERO DU MODE DE MASSE LA PLUS ELEVEE
C IN  : IAMOR  : NUMERO DE L'AMORTISSEMENT ASSOCIE
C IN  : PULSAT : PULSATIONS DES MODES
C IN  : MASGEN : MASSES GENERALISEES DES MODES
C IN  : AMOGEN : MATRICE DES AMORTISSEMENTS GENERALISES
C IN  : LFLU   : LOGIQUE INDIQUANT LA PRESENCE DE LAME FLUIDE
C IN  : NBNLI  : DIMENSION DES TABLEAUX (NBCHOC+NBSISM+NBFLAM)
C OUT : NOECHO : NOEUD DE CHOC (VOIR MDCHOC)
C OUT : LOGCHO : LOGIQUE CHOC (VOIR MDCHOC)
C OUT : PARCHO : PARAMETRE DE CHOC (VOIR MDCHOC)
C OUT : INTITU : INTITULE DE CHOC
C OUT : DDLCHO : TABLEAU DES NUMEROTATIONS DES NOEUDS DE CHOC
C OUT : IER    : NIVEAU D'ERREUR
C     ------------------------------------------------------------------
C TOLE CRP_20

      INTEGER       NBCHOC, NBSISM, NBFLAM, NBOCC, I, J, IOC, IBID, IL,
     &              JCOOR, JMAMA, NBNMA, KMA, NN1, NN2, INO1, INO2, IG,
     &              N1, NAMTAN, IRET, NMLIAI, JMAIL, IM, ILIAI, NMGR,
     &              NGRM, NUMAI, IRETT,COMPT1,COMPT2,NBMAIL,NBNO,
     &              J1,J2,BONO1,BONO2
      REAL*8        KTANG, CTANG, K, RAP, XJEU, R8BID
      REAL*8        ALPHA, BETA, AXE(3)
      COMPLEX*16    CBID
      LOGICAL       LNOUE2,MEMAIL
      CHARACTER*8   KBID, REPERE, MAILLA, NOMNO1, NOMNO2,
     &              K8TYP,K8BID
      CHARACTER*10  MOTFAC
      CHARACTER*24  MDGENE, REFO, NOMGR1, NOMGR2, MAMAI
      CHARACTER*24  VALK(2)
C     ------------------------------------------------------------------
C  COUPLAGE EDYOS
C  =>
      INTEGER       IPAT,IPAL,NNO,NDDL,NDDL1,NDDL2
C     ANCIENS INCLUDE (CALCIUM.H)
C     ===========================
      CHARACTER*3   COMP(6)
C     =================================
      INTEGER       NBPAL

      INTEGER       PALMAX
      PARAMETER (PALMAX=20)

      INTEGER       DIMNAS
      PARAMETER     (DIMNAS=8)

      INTEGER       IADRK
      CHARACTER*24  CPAL,CNPAL(PALMAX)
      INTEGER      IARG
C
      CALL JEMARQ()
      CALL GETFAC ( 'CHOC'     , NBCHOC )
      CALL GETFAC ( 'ANTI_SISM', NBSISM )
      CALL GETFAC ( 'FLAMBAGE' , NBFLAM )
      NBOCC =  NBCHOC + NBSISM + NBFLAM
      MDGENE = ' '
      CALL DISMOI('F','NOM_MAILLA',NUMDDL,'NUME_DDL',IBID,MAILLA,IRET)
C
      CALL JEVEUO(MAILLA//'.COORDO    .VALE','L',JCOOR)
C
      DO 10 IL = 1 , NBNLI
         NOECHO(IL,3) = NUMDDL(1:8)
         NOECHO(IL,4) = MAILLA
         NOECHO(IL,7) = NUMDDL(1:8)
         NOECHO(IL,8) = MAILLA
 10   CONTINUE
C
      ILIAI = 0
      MOTFAC = 'CHOC'
      DO 100 I = 1 , NBOCC
         IOC = I
         IF (I.GT.NBCHOC+NBSISM) THEN
            MOTFAC = 'FLAMBAGE'
            IOC = I-(NBCHOC+NBSISM)
         ELSEIF (I.GT.NBCHOC) THEN
            MOTFAC = 'ANTI_SISM'
            IOC = I-NBCHOC
         ENDIF
         LNOUE2 = .FALSE.
         NMLIAI = 0
C
         IF (MOTFAC.EQ.'CHOC') THEN
            CALL GETVTX ( MOTFAC, 'MAILLE', IOC,IARG,0, KBID, IBID )
            IF (IBID.NE.0) THEN
               LNOUE2 = .TRUE.
               NMLIAI = -IBID
               CALL WKVECT('&&MDCHST.MAILLE','V V K8',NMLIAI,JMAIL)
               CALL GETVEM ( MAILLA, 'MAILLE', MOTFAC, 'MAILLE',
     &                                    IOC,IARG,NMLIAI,ZK8(JMAIL),
     &                                    IBID)
               DO 110 IM = 1, NMLIAI
                  MAMAI = ZK8(JMAIL-1+IM)
                  CALL JENONU(JEXNOM(MAILLA//'.NOMMAI',MAMAI),NUMAI)
                  CALL JEVEUO(JEXNUM(MAILLA//'.CONNEX',NUMAI),'L',JMAMA)
                  CALL JELIRA(JEXNUM(MAILLA//'.CONNEX',NUMAI),'LONMAX',
     &                                                       NBNMA,KBID)
                  IF (NBNMA.NE.2) THEN
                    VALK (1) = MAMAI
                    VALK (2) = 'SEG2'
                    CALL U2MESG('F', 'ALGORITH13_39',2,VALK,0,0,0,0.D0)
                  ENDIF
                  ILIAI = ILIAI + 1
                  CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',ZI(JMAMA)),
     &                                                 NOECHO(ILIAI,1))
                  CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',ZI(JMAMA+1)),
     &                                                 NOECHO(ILIAI,5))
                  CALL MDCHDL ( NBNLI,NOECHO,LNOUE2,ILIAI,DDLCHO,IER )
 110           CONTINUE
               CALL JEDETR('&&MDCHST.MAILLE')
               GOTO 102
            ENDIF
C
            CALL GETVTX ( MOTFAC, 'GROUP_MA', IOC,IARG,0, KBID, IBID )
            IF (IBID.NE.0) THEN
               LNOUE2 = .TRUE.
               NMLIAI = 0
               NGRM = -IBID
               CALL WKVECT('&&MDCHST.GROUP_MA','V V K24',NGRM,JMAIL)
               CALL GETVEM ( MAILLA, 'GROUP_MA', MOTFAC, 'GROUP_MA',
     &                       IOC,IARG,NGRM,ZK24(JMAIL),IBID)
               DO 120 IG = 1, NGRM
                  MAMAI = ZK24(JMAIL-1+IG)
                  CALL JELIRA(JEXNOM(MAILLA//'.GROUPEMA',MAMAI),
     &                                             'LONMAX',NMGR,KBID)
                 CALL JEVEUO(JEXNOM(MAILLA//'.GROUPEMA',MAMAI),'L',KMA)
                  NMLIAI = NMLIAI + NMGR
                  DO 122 IM = 1, NMGR
                     NUMAI = ZI(KMA-1+IM)
                  CALL JEVEUO(JEXNUM(MAILLA//'.CONNEX',NUMAI),'L',JMAMA)
                  CALL JELIRA(JEXNUM(MAILLA//'.CONNEX',NUMAI),'LONMAX',
     &                                                       NBNMA,KBID)
                     IF (NBNMA.NE.2) THEN
                  CALL JENUNO(JEXNUM(MAILLA//'.NOMMAI',NUMAI),KBID)
                  VALK (1) = KBID
                  VALK (2) = 'SEG2'
                  CALL U2MESG('F', 'ALGORITH13_39',2,VALK,0,0,0,0.D0)
                     ENDIF
                     ILIAI = ILIAI + 1
                     CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',ZI(JMAMA)),
     &                                                 NOECHO(ILIAI,1))
                     CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',ZI(JMAMA+1)),
     &                                                 NOECHO(ILIAI,5))
                     CALL MDCHDL (NBNLI,NOECHO,LNOUE2,ILIAI,DDLCHO,IER)
 122              CONTINUE
 120           CONTINUE
               CALL JEDETR('&&MDCHST.GROUP_MA')
               GOTO 102
            ENDIF
         ENDIF
C
         CALL GETVEM ( MAILLA, 'NOEUD', MOTFAC, 'NOEUD_1',
     &                                             IOC,IARG,1,NOMNO1,
     &                                             IBID)
         IF (IBID.NE.0) THEN
            ILIAI = ILIAI + 1
            NOECHO(ILIAI,1) = NOMNO1
            CALL GETVEM ( MAILLA, 'NOEUD', MOTFAC, 'NOEUD_2',
     &                                              IOC,IARG,1,NOMNO2,
     &                                              NN1)
            IF (NN1.NE.0) THEN
               NOECHO(ILIAI,5) = NOMNO2
               LNOUE2 = .TRUE.
            ELSE
               CALL GETVTX(MOTFAC,'GROUP_NO_2',IOC,IARG,1,NOMGR2,NN2)
               IF (NN2.NE.0) THEN
                  CALL UTNONO(' ',MAILLA,'NOEUD',NOMGR2,NOMNO2,IRET)
                  IF (IRET.EQ.10) THEN
                     CALL U2MESK('F','ELEMENTS_67',1,NOMGR2)
                  ELSEIF (IRET.EQ.1) THEN
                  VALK (1) = NOMGR2
                  VALK (2) = NOMNO2
                  CALL U2MESG('A', 'ALGORITH13_41',2,VALK,0,0,0,0.D0)
                  ENDIF
                  NOECHO(ILIAI,5) = NOMNO2
                  LNOUE2 = .TRUE.
               ELSE
                  NOECHO(ILIAI,5) = NOMNO1
               ENDIF
            ENDIF
            CALL MDCHDL ( NBNLI, NOECHO, LNOUE2, ILIAI, DDLCHO, IER )
            GOTO 102
         ENDIF
C
         CALL GETVEM ( MAILLA, 'GROUP_NO', MOTFAC, 'GROUP_NO_1',
     &                                              IOC,IARG,1,NOMGR1,
     &                                              IBID)
         CALL UTNONO(' ',MAILLA,'NOEUD',NOMGR1,NOMNO1,IRET)
         IF (IRET.EQ.10) THEN
            CALL U2MESK('F','ELEMENTS_67',1,NOMGR1)
         ELSEIF (IRET.EQ.1) THEN
                  VALK (1) = NOMGR1
                  VALK (2) = NOMNO1
            CALL U2MESG('A', 'ALGORITH13_41',2,VALK,0,0,0,0.D0)
         ENDIF
         ILIAI = ILIAI + 1
         NOECHO(ILIAI,1) = NOMNO1
         CALL GETVEM ( MAILLA, 'NOEUD', MOTFAC, 'NOEUD_2',
     &                                               IOC,IARG,1,NOMNO2,
     &                                               NN1)
         IF (NN1.NE.0) THEN
            NOECHO(ILIAI,5) = NOMNO2
            LNOUE2 = .TRUE.
         ELSE
            CALL GETVTX(MOTFAC,'GROUP_NO_2',IOC,IARG,1,NOMGR2,NN2)
            IF (NN2.NE.0) THEN
               CALL UTNONO(' ',MAILLA,'NOEUD',NOMGR2,NOMNO2,IRET)
               IF (IRET.EQ.10) THEN
                  CALL U2MESK('F','ELEMENTS_67',1,NOMGR2)
               ELSEIF (IRET.EQ.1) THEN
                  VALK (1) = NOMGR2
                  VALK (2) = NOMNO2
                  CALL U2MESG('A', 'ALGORITH13_41',2,VALK,0,0,0,0.D0)
               ENDIF
               NOECHO(ILIAI,5) = NOMNO2
               LNOUE2 = .TRUE.
            ELSE
               NOECHO(ILIAI,5) = NOMNO1
            ENDIF
         ENDIF
         CALL MDCHDL ( NBNLI, NOECHO, LNOUE2, ILIAI, DDLCHO, IER )
 102     CONTINUE
C
         ILIAI = ILIAI - MAX(1,NMLIAI)
         DO 130 IL = 1 , MAX(1,NMLIAI)
            ILIAI = ILIAI + 1
            CALL JENONU(JEXNOM(MAILLA//'.NOMNOE',NOECHO(ILIAI,1)),INO1)
            CALL JENONU(JEXNOM(MAILLA//'.NOMNOE',NOECHO(ILIAI,5)),INO2)
            DO 132 J = 1,3
               PARCHO(ILIAI,7+J) = ZR(JCOOR+3*(INO1-1)+J-1)
               PARCHO(ILIAI,10+J) = ZR(JCOOR+3*(INO2-1)+J-1)
 132        CONTINUE
C
            KTANG = 0.D0
            CTANG = 0.D0
            NAMTAN = 0
            IF (MOTFAC.EQ.'CHOC') THEN
           CALL GETVTX(MOTFAC,'INTITULE',IOC,IARG,1,
     &                 INTITU(ILIAI)   ,N1)
           CALL GETVR8(MOTFAC,'JEU',IOC,IARG,1,
     &                 PARCHO(ILIAI,1) ,N1)
           CALL GETVR8(MOTFAC,'DIST_1',IOC,IARG,1,
     &                 PARCHO(ILIAI,30),N1)
           CALL GETVR8(MOTFAC,'DIST_2',IOC,IARG,1,
     &                 PARCHO(ILIAI,31),N1)
           CALL GETVR8(MOTFAC,'RIGI_NOR',IOC,IARG,1,
     &                 PARCHO(ILIAI,2) ,N1)
           CALL GETVR8(MOTFAC,'AMOR_NOR',IOC,IARG,1,
     &                 PARCHO(ILIAI,3) ,N1)
           CALL GETVR8(MOTFAC,'RIGI_TAN'   ,IOC,IARG,1,KTANG       ,N1)
           CALL GETVTX(MOTFAC,'FROTTEMENT',IOC,IARG,1,
     &                 TYPFRO       ,N1)
            IF (TYPFRO .EQ. 'COULOMB         ') THEN
               CALL GETVR8(MOTFAC,'COULOMB'    ,IOC,IARG,1,
     &          PARCHO(ILIAI,6) ,N1)
               CALL GETVR8(MOTFAC,'COULOMB'    ,IOC,IARG,1,
     &          PARCHO(ILIAI,7) ,N1)
             ELSEIF (TYPFRO .EQ. 'COULOMB_STAT_DYN') THEN
               CALL GETVR8(MOTFAC,'COULOMB_DYNA'    ,IOC,IARG,1,
     &          PARCHO(ILIAI,6) ,N1)
               CALL GETVR8(MOTFAC,'COULOMB_STAT'    ,IOC,IARG,1,
     &          PARCHO(ILIAI,7) ,N1)
             ENDIF
           CALL GETVR8(MOTFAC,'AMOR_TAN',IOC,IARG,1,
     &                 CTANG       ,NAMTAN)
           CALL GETVTX(MOTFAC,'LAME_FLUIDE',IOC,IARG,1,KBID        ,N1)
               IF (KBID(1:3).EQ.'OUI') THEN
                  LFLU=.TRUE.
                  LOGCHO(ILIAI,2)=1
              CALL GETVR8('CHOC','ALPHA   ',IOC,IARG,1,
     &                    PARCHO(ILIAI,32),N1)
              CALL GETVR8('CHOC','BETA    ',IOC,IARG,1,
     &                    PARCHO(ILIAI,33),N1)
              CALL GETVR8('CHOC','CHI     ',IOC,IARG,1,
     &                    PARCHO(ILIAI,34),N1)
              CALL GETVR8('CHOC','DELTA   ',IOC,IARG,1,
     &                    PARCHO(ILIAI,35),N1)
               ENDIF
               CALL GETVID(MOTFAC,'OBSTACLE',IOC,IARG,1,
     &                     NOECHO(ILIAI,9),N1)
               CALL TBLIVA(NOECHO(ILIAI,9),1,'LIEU',
     &                     IBID,R8BID,CBID,'DEFIOBST',KBID,R8BID,'TYPE',
     &                     K8TYP,IBID,R8BID,CBID,REFO,IRETT)
               CALL ASSERT(IRETT.EQ.0)
               IF (REFO(1:9).EQ.'BI_PLAN_Y') THEN
                  NOECHO(ILIAI,9) = 'BI_PLANY'
               ELSEIF (REFO(1:9).EQ.'BI_PLAN_Z') THEN
                  NOECHO(ILIAI,9) = 'BI_PLANZ'
               ELSEIF (REFO(1:11).EQ.'BI_CERC_INT') THEN
                  NOECHO(ILIAI,9) = 'BI_CERCI'
               ELSEIF (REFO(1:7).NE.'DISCRET') THEN
                  NOECHO(ILIAI,9) = REFO(1:8)
               ENDIF
               IF ( NOECHO(ILIAI,9).EQ.'BI_CERCI' .AND.
     &              PARCHO(ILIAI,31).LT.PARCHO(ILIAI,30)) THEN
                  CALL U2MESS('F','ALGORITH5_35')
               ENDIF
            ELSEIF (MOTFAC.EQ.'FLAMBAGE') THEN
               INTITU(I) = NOECHO(ILIAI,1)
          CALL GETVR8(MOTFAC,'JEU',IOC,IARG,1,
     &                PARCHO(ILIAI,1) ,N1)
          CALL GETVR8(MOTFAC,'DIST_1',IOC,IARG,1,
     &                PARCHO(ILIAI,30),N1)
          CALL GETVR8(MOTFAC,'DIST_2',IOC,IARG,1,
     &                PARCHO(ILIAI,31),N1)
          CALL GETVR8(MOTFAC,'RIGI_NOR',IOC,IARG,1,
     &                PARCHO(ILIAI,2) ,N1)
          CALL GETVR8(MOTFAC,'FNOR_CRIT',IOC,IARG,1,
     &                PARCHO(ILIAI,50),N1)
          CALL GETVR8(MOTFAC,'FNOR_POST_FL',IOC,IARG,1,
     &                PARCHO(ILIAI,51),N1)
          CALL GETVR8(MOTFAC,'RIGI_NOR_POST_FL',IOC,IARG,1,
     &                PARCHO(ILIAI,52)
     &                                                           ,N1)
               LOGCHO(ILIAI,5) = 1
               IF ( PARCHO(ILIAI,2 ).LE.0.D0 .OR.
     &              PARCHO(ILIAI,52).LE.0.D0 ) THEN
                  CALL U2MESS('F','ALGORITH5_40')
               ELSE
                  RAP=PARCHO(ILIAI,50)/PARCHO(ILIAI,2)-PARCHO(ILIAI,51)/
     &                                                 PARCHO(ILIAI,52)
                  IF (RAP .LT. 0.D0)
     &               CALL U2MESS('F','ALGORITH5_41')
               ENDIF
               CALL GETVID(MOTFAC,'OBSTACLE',IOC,IARG,1,
     &                     NOECHO(ILIAI,9),N1)
               CALL TBLIVA(NOECHO(ILIAI,9),1,'LIEU',
     &                     IBID,R8BID,CBID,'DEFIOBST',KBID,R8BID,'TYPE',
     &                     K8TYP,IBID,R8BID,CBID,REFO,IRETT)
               CALL ASSERT(IRETT.EQ.0)
               IF (REFO(1:9).EQ.'BI_PLAN_Y') THEN
                  NOECHO(ILIAI,9) = 'BI_PLANY'
               ELSEIF (REFO(1:9).EQ.'BI_PLAN_Z') THEN
                  NOECHO(ILIAI,9) = 'BI_PLANZ'
               ELSEIF (REFO(1:11).EQ.'BI_CERC_INT') THEN
                  NOECHO(ILIAI,9) = 'BI_CERCI'
               ELSEIF (REFO(1:7).NE.'DISCRET') THEN
                  NOECHO(ILIAI,9) = REFO(1:8)
               ENDIF
               IF ( NOECHO(ILIAI,9).EQ.'BI_CERCI'    .AND.
     &              PARCHO(ILIAI,31).LT.PARCHO(ILIAI,30) ) THEN
                  CALL U2MESS('F','ALGORITH5_35')
               ENDIF
C
            ELSEIF (MOTFAC.EQ.'ANTI_SISM') THEN
               INTITU(ILIAI) = NOECHO(ILIAI,1)
            CALL GETVR8(MOTFAC,'RIGI_K1   ',IOC,IARG,1,
     &                  PARCHO(ILIAI,39),N1)
            CALL GETVR8(MOTFAC,'RIGI_K2   ',IOC,IARG,1,
     &                  PARCHO(ILIAI,40),N1)
            CALL GETVR8(MOTFAC,'SEUIL_FX  ',IOC,IARG,1,
     &                  PARCHO(ILIAI,41),N1)
            CALL GETVR8(MOTFAC,'C         ',IOC,IARG,1,
     &                  PARCHO(ILIAI,42),N1)
            CALL GETVR8(MOTFAC,'PUIS_ALPHA',IOC,IARG,1,
     &                  PARCHO(ILIAI,43),N1)
            CALL GETVR8(MOTFAC,'DX_MAX    ',IOC,IARG,1,
     &                  PARCHO(ILIAI,44),N1)
                LOGCHO(ILIAI,4)=1
               NOECHO(ILIAI,9) = 'BI_PLANY'
            ENDIF
C --------- SI AMOR_TAN NON RENSEIGNE ON LUI AFFECTE UNE VAL OPTIMISEE
            IF ( NAMTAN.EQ.0 .AND. KTANG.NE.0.D0 ) THEN
               K = PULSAT(IMODE)**2 * MASGEN(IMODE)
               CTANG =   2.D0*SQRT( MASGEN(IMODE)*(K+KTANG) )
     &                 - 2.D0*AMOGEN(IAMOR)*SQRT( K*MASGEN(IMODE) )
               CALL U2MESG('I','ALGORITH16_10',0,' ',1,I,1,CTANG)
            ENDIF
            PARCHO(ILIAI,4) = KTANG
            PARCHO(ILIAI,5) = CTANG
C
            IF (NOECHO(ILIAI,9)(1:2).EQ.'BI') THEN
               XJEU = (PARCHO(ILIAI,11)-PARCHO(ILIAI,8))**2 +
     &                (PARCHO(ILIAI,12)-PARCHO(ILIAI,9))**2 +
     &                (PARCHO(ILIAI,13)-PARCHO(ILIAI,10))**2
            ENDIF
C
            CALL MDCHRE ( MOTFAC, IOC, ILIAI, MDGENE, TYPNUM, REPERE,
     &                                          NBNLI, PARCHO, LNOUE2 )
C
            CALL MDCHAN ( MOTFAC, IOC, ILIAI, MDGENE, TYPNUM, REPERE,
     &                                    XJEU, NBNLI, NOECHO, PARCHO )
C
 130     CONTINUE
C
 100  CONTINUE
C    COUPLAGE EDYOS
      IF (NBPAL .GT. 0 ) THEN
        CPAL = 'C_PAL'
        COMP(1)='DX'
        COMP(2)='DY'
        COMP(3)='DZ'
        COMP(4)='DRX'
        COMP(5)='DRY'
        COMP(6)='DRZ'
        CALL JEVEUO(CPAL,'L',IADRK)
        DO 21 IPAL = 1, NBPAL
          NOECHO(IPAL,1)=ZK8(IADRK+(IPAL-1)+2*PALMAX)(1:DIMNAS)
          NOECHO(IPAL,5)=NOECHO(IPAL,1)
          CNPAL(IPAL)=ZK8(IADRK+(IPAL-1)+2*PALMAX)(1:DIMNAS)
 21     CONTINUE
        DO 22 IPAL = 1, NBPAL
          CALL UTNONO(' ',MAILLA,'NOEUD',CNPAL(IPAL),NOMNO1,IRET)
          IF (IRET.EQ.10) THEN
C            CALL U2MESK('F','ELEMENTS_67',1,NOMGR2)
            NOMNO1 = CNPAL(IPAL)(1:8)
          ELSEIF (IRET.EQ.1) THEN
            VALK (1) = CNPAL(IPAL)
            VALK (2) = NOMNO1
            CALL U2MESG('A', 'ALGORITH13_41',2,VALK,0,0,0,0.D0)
          ENDIF
          DO 23 IPAT = 1, 6
            CALL POSDDL('NUME_DDL',NUMDDL,NOMNO1,
     &                   COMP(IPAT),NNO,NDDL)
            DDLCHO(6*(IPAL-1)+IPAT) = NDDL
 23      CONTINUE
 22    CONTINUE
      ENDIF
C FIN PALIERS EDYOS
C
C    ROTOR FISSURE
      MOTFAC='ROTOR_FISS'
      COMP(1)='DRX'
      COMP(2)='DRY'
      COMP(3)='DRZ'
      IF ( NBRFIS .GT. 0 ) THEN
      DO 61 I = 1,NBRFIS
          ILIAI = ILIAI + 1
         CALL GETVEM ( MAILLA, 'NOEUD', MOTFAC, 'NOEUD_D',
     &                                             I,IARG,1,NOMNO1,IBID)
         CALL GETVEM ( MAILLA, 'NOEUD', MOTFAC, 'NOEUD_G',
     &                                             I,IARG,1,NOMNO2,IBID)


         CALL GETVEM ( MAILLA, 'GROUP_NO', MOTFAC, 'GROUP_NO_D',
     &                 I,IARG,1,NOMGR1,IBID)
         IF (IBID.NE.0) THEN
            CALL UTNONO(' ',MAILLA,'NOEUD',NOMGR1,NOMNO1,IRET)
            IF (IRET.EQ.10) THEN
               CALL U2MESK('F','ELEMENTS_67',1,NOMGR1)
            ELSEIF (IRET.EQ.1) THEN
               VALK (1) = NOMGR1
               VALK (2) = NOMNO1
               CALL U2MESG('A', 'ALGORITH13_41',2,VALK,0,0,0,0.D0)
            ENDIF
         END IF

         CALL GETVEM ( MAILLA, 'GROUP_NO', MOTFAC, 'GROUP_NO_G',
     &                 I,IARG,1,NOMGR2,IBID)
         IF (IBID.NE.0) THEN
            CALL UTNONO(' ',MAILLA,'NOEUD',NOMGR2,NOMNO2,IRET)
            IF (IRET.EQ.10) THEN
               CALL U2MESK('F','ELEMENTS_67',1,NOMGR2)
            ELSEIF (IRET.EQ.1) THEN
               VALK (1) = NOMGR2
               VALK (2) = NOMNO2
               CALL U2MESG('A', 'ALGORITH13_41',2,VALK,0,0,0,0.D0)
            ENDIF
         END IF

         DO 63 IPAT = 1,3
            CALL POSDDL('NUME_DDL',NUMDDL,NOMNO1,COMP(IPAT),NN1,NDDL1)
            CALL POSDDL('NUME_DDL',NUMDDL,NOMNO2,COMP(IPAT),NN2,NDDL2)
            DDLCHO(ILIAI-1+6*(I-1)+IPAT) = NDDL1
            DDLCHO(ILIAI-1+6*(I-1)+IPAT+3) = NDDL2
 63      CONTINUE


C DETERMINATION DES DIRECTION ET ORIENTATION DU ROTOR
         COMPT1=0
         COMPT2=0
         CALL JELIRA(MAILLA//'.CONNEX','NMAXOC',NBMAIL,K8BID)
         DO 66 NUMAI=1,NBMAIL
            CALL JELIRA(JEXNUM(MAILLA//'.CONNEX',NUMAI),
     &                 'LONMAX',NBNO,K8BID)
            IF ((NBNO.GT.1) .AND. (NBNO.LT.4)) THEN
               CALL JEVEUO(JEXNUM(MAILLA//'.CONNEX',NUMAI),'L',IBID)
               DO 77 J1=1,NBNO
                  IF (ZI(IBID+J1-1).EQ.NN1) THEN
                     MEMAIL=.FALSE.
                     DO 78 J2=1,NBNO
                        IF (ZI(IBID+J2-1).EQ.NN2) MEMAIL=.TRUE.
78                   CONTINUE
                     IF (.NOT.MEMAIL) THEN
                        COMPT1=COMPT1+1
                        IF (J1.EQ.1) BONO1=ZI(IBID+1)
                        IF (J1.EQ.2) BONO1=ZI(IBID)
                     ENDIF
                  ENDIF
                  IF (ZI(IBID+J1-1).EQ.NN2) THEN
                     MEMAIL=.FALSE.
                     DO 79 J2=1,NBNO
                        IF (ZI(IBID+J2-1).EQ.NN1) MEMAIL=.TRUE.
79                   CONTINUE
                     IF (.NOT.MEMAIL) THEN
                        COMPT2=COMPT2+1
                        IF (J1.EQ.1) BONO2=ZI(IBID+1)
                        IF (J1.EQ.2) BONO2=ZI(IBID)
                     ENDIF
                  ENDIF
77             CONTINUE
            ENDIF
66       CONTINUE
         CALL ASSERT(COMPT1 .GE. 1)
         CALL ASSERT(COMPT2 .GE. 1)

         DO 89 J=1,3
            AXE(J)=ZR(JCOOR+3*(BONO1-1)+J-1) - ZR(JCOOR+3*(BONO2-1)+J-1)
89       CONTINUE

C ORIENTATION DU ROTOR
         CALL ANGVX ( AXE, ALPHA, BETA )
         PARCHO(ILIAI,17) = SIN(ALPHA)
         PARCHO(ILIAI,18) = COS(ALPHA)
         PARCHO(ILIAI,19) = SIN(BETA)
         PARCHO(ILIAI,20) = COS(BETA)

 61   CONTINUE
      ENDIF

C FIN ROTOR FISSURE
C
      CALL JEDEMA()
      END
