      SUBROUTINE OP0106()

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 07/12/2010   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C
      IMPLICIT   NONE
C
C-----------------------------------------------------------------------
C     COMMANDE :  CALC_NO
C        CALCULE DES FORCES NODALES ET DES REACTIONS EN MECANIQUE.
C        CALCUL DES GRANDEURS AUX NOEUDS.
C-----------------------------------------------------------------------
C

C 0.1. ==> ARGUMENTS

C 0.2. ==> COMMUNS

C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C 0.3. ==> VARIABLES LOCALES

      INTEGER NBPASE,TYPESE,NUORD
      INTEGER IBID
      INTEGER I,IACHAR,IAD,IBD,IC,ICHAR,IE,IND
      INTEGER INUME,IOPT,IORDR,IRET,IRET1,IRET2,J,JCGMP
      INTEGER JCHMP,JDDL,JDDR,JFO,JFONO,JFPIP,JINFC,JNMO
      INTEGER JNOCH,JOPT,JORDR,JRE,JRENO,LACCE,LDEPL,LMAT
      INTEGER LONC2,LONCH,LREF,LTRAV,LVAFON,N0,N2,NBCHAR
      INTEGER NBDDL,NBOPT,NBORDR,NC,NEQ,NH,NP,NBMA
      INTEGER JMAI,N3,N4,NBNO,JNOE,II,LTPS,LTPS2
      INTEGER JREF,NCMPMA,DIMAKI,DIMANV
C     ------------------------------------------------------------------
C     DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
      PARAMETER(DIMAKI=9)
C     DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
      PARAMETER(DIMANV=4)
      PARAMETER(NCMPMA=7+DIMAKI+DIMANV)
C     ------------------------------------------------------------------

      REAL*8 TIME,OMEGA2,PREC,COEF(3),PARTPS(3),ETAN

      CHARACTER*2 CODRET
      CHARACTER*4 TYPCAL
      CHARACTER*6 NOMPRO
      CHARACTER*8 K8BID,CTYP,CRIT,NOPASE,NOMA,MATERI,NOMCMP(3)
      CHARACTER*8 KIORD,NMCMP2(NCMPMA)
      CHARACTER*13 INPSCO
      CHARACTER*16 OPTION,OPTIO2,TYSD,TYPE,OPER,TYPMO,K16BID
      CHARACTER*16 COMPEX,TYPMCL(4),MOTCLE(4),MCL(2)
      CHARACTER*19 RESUCO,KNUM,INFCHA,LIGREL,RESUC1,CHDEP2
      CHARACTER*19 CHAMS0,CHAMS1,PRFCHN
      CHARACTER*24 MODELE,MATER,CARAC,CHARGE,FOMULT,INFOCH,CHAMNO
      CHARACTER*24 NUME,VFONO,VAFONO,SIGMA,CHDEPL,K24BID,CHELEM
      CHARACTER*24 CHACCE,VRENO,VARENO,COMPOR,CHVIVE,CHACVE,MASSE
      CHARACTER*24 MESMAI,MESNOE,BIDON,CHVARC,VAPRIN,STYPSE
      CHARACTER*24 NOOJB,K24B,NUMREF,VALK(3)
      CHARACTER*24 VECHMP,VACHMP,CNCHMP,VEFPIP,VAFPIP,CNFPIP
      CHARACTER*24 VECGMP,VACGMP,CNCGMP
C     ------------------------------------------------------------------
      PARAMETER(NOMPRO='OP0106')
C     ------------------------------------------------------------------

      LOGICAL EXITIM,LBID,LFNONL,FNOEVO

C     ------------------------------------------------------------------
      DATA INFCHA/'&&INFCHA.INFCHA'/
      DATA NOMCMP/'DX','DY','DZ'/
      DATA K24BID/' '/
      DATA TYPCAL/'MECA'/
      DATA CHVARC/'&&OP0106.CHVARC'/
      DATA NMCMP2/'RELCOM  ','NBVARI  ','DEFORM  ','INCELA  ',
     &     'C_PLAN  ','XXXX1   ','XXXX2   ','KIT1    ','KIT2    ',
     &     'KIT3    ','KIT4    ','KIT5    ','KIT6    ','KIT7    ',
     &     'KIT8    ','KIT9    ','NVI_C   ','NVI_T   ','NVI_H   ',
     &     'NVI_M   '/
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ()

C --- ON STOCKE LE COMPORTEMENT EN CAS D'ERREUR AVANT MNL : COMPEX
C --- PUIS ON PASSE DANS LE MODE "VALIDATION DU CONCEPT EN CAS D'ERREUR"
      CALL ONERRF(' ',COMPEX,IBID)
      CALL ONERRF('EXCEPTION+VALID',K16BID,IBID)

      CALL INFMUE()
C
C --- OPTIONS A CALCULER
C
      CALL GETRES(RESUC1,TYPE,OPER)
      CALL GETVID(' ','RESULTAT',1,1,1,RESUCO,N0)
      CALL GETVTX(' ','OPTION',1,1,0,OPTION,N2)
      NBOPT=-N2
      CALL WKVECT('&&'//NOMPRO//'.OPTION','V V K16',NBOPT,JOPT)
C
C --- FORC_NODA_NONL ?
C
      CALL GETVTX(' ','OPTION',1,1,NBOPT,ZK16(JOPT),N2)
      LFNONL=.FALSE.
      DO 10 IOPT=1,NBOPT
        OPTION=ZK16(JOPT+IOPT-1)
        IF (OPTION.EQ.'FORC_NODA_NONL') THEN
          LFNONL=.TRUE.
          GOTO 20

        ENDIF
   10 CONTINUE
   20 CONTINUE
C
      IF (LFNONL) THEN
        IF (NBOPT.GT.1) CALL U2MESS('F','PREPOST3_96')
        IF (RESUC1.EQ.RESUCO(1:8)) THEN
          VALK(1)=RESUC1
          CALL U2MESK('F','PREPOST3_97',1,VALK)
        ENDIF
        IF (TYPE.NE.'DYNA_TRANS') CALL U2MESS('F','PREPOST3_98')
      ELSE
        IF (RESUC1.NE.RESUCO(1:8)) THEN
          VALK(1)=RESUC1
          VALK(2)=RESUCO
          CALL U2MESK('F','PREPOST3_79',2,VALK)
        ENDIF
      ENDIF

      CALL GETTCO(RESUCO(1:8),TYSD)
      INPSCO='&&'//NOMPRO//'_PSCO'

      KNUM='&&'//NOMPRO//'.NUME_ORDRE'

C ----ON VERIFIE SI DERRIERE UN CONCEPT MODE_MECA SE TROUVE UN MODE_DYN

      IF (TYSD(1:9).EQ.'MODE_MECA') THEN
        CALL RSADPA(RESUCO,'L',1,'TYPE_MODE',1,0,IAD,K8BID)
        TYPMO=ZK16(IAD)
      ENDIF

C=======================================================================
      K8BID='&&'//NOMPRO
      IBID=1
      NBPASE=0
      NOPASE=' '
      TYPESE=0
      STYPSE=' '
C=======================================================================

      CALL GETVR8(' ','PRECISION',1,1,1,PREC,NP)
      CALL GETVTX(' ','CRITERE',1,1,1,CRIT,NC)





      CALL RSUTNU(RESUCO,' ',0,KNUM,NBORDR,PREC,CRIT,IRET)
      IF (IRET.EQ.10) THEN
        CALL U2MESK('S','CALCULEL4_8',1,RESUCO)
        GOTO 310

      ENDIF
      IF (IRET.NE.0) THEN
        CALL U2MESS('S','ALGORITH3_41')
        GOTO 310

      ENDIF
      CALL JEVEUO(KNUM,'L',JORDR)
      BIDON='&&'//NOMPRO//'.BIDON'

C TRI DES OPTIONS SUIVANT TYSD
      LMAT=0
      EXITIM=.FALSE.
      IF (TYSD.EQ.'EVOL_ELAS' .OR. TYSD.EQ.'EVOL_NOLI') THEN
        EXITIM=.TRUE.
      ELSEIF (TYSD.EQ.'MODE_MECA' .OR. TYSD.EQ.'DYNA_TRANS') THEN
        CALL JEEXIN(RESUCO//'.REFD',IRET)
        IF (IRET.NE.0) THEN
          CALL JEVEUO(RESUCO//'.REFD','L',LREF)
          MASSE=ZK24(LREF+1)
          IF (MASSE.NE.' ') THEN
            CALL MTDSCR(MASSE)
            CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT)
          ENDIF
        ENDIF
        IF (TYSD.EQ.'DYNA_TRANS')EXITIM=.TRUE.
      ELSEIF (TYSD.EQ.'DYNA_HARMO') THEN
        DO 30 IOPT=1,NBOPT
          OPTION=ZK16(JOPT+IOPT-1)
          IF (OPTION(6:9).NE.'NOEU') THEN
            CALL U2MESK('A','PREPOST3_80',1,TYSD)
            GOTO 310

          ENDIF
   30   CONTINUE
        CALL JEEXIN(RESUCO//'.REFD',IRET)
        IF (IRET.NE.0) THEN
          CALL JEVEUO(RESUCO//'.REFD','L',LREF)
          MASSE=ZK24(LREF+1)
          IF (MASSE.NE.' ') THEN
            CALL MTDSCR(MASSE)
            CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT)
          ENDIF
        ENDIF
      ELSEIF (TYSD.EQ.'ACOU_HARMO' .OR. TYSD.EQ.'MODE_ACOU') THEN
        DO 40 IOPT=1,NBOPT
          OPTION=ZK16(JOPT+IOPT-1)
          IF (OPTION(6:9).NE.'NOEU') THEN
            CALL U2MESK('A','PREPOST3_80',1,TYSD)
            GOTO 310

          ENDIF
   40   CONTINUE
      ENDIF


      CARAC=' '
      CHARGE=' '
      MATER=' '
      MODELE=' '
      NUORD=ZI(JORDR)
      IF (ZK16(JOPT).EQ.'FORC_NODA_NONL') THEN
        CALL RSCRSD('G',RESUC1,TYPE,NBORDR)
        CALL GETVID(' ','MODELE',1,1,1,MODELE,N0)
        CALL GETVID(' ','CHAM_MATER',1,1,1,MATERI,N0)
        IF (N0.NE.0) THEN
          CALL RCMFMC(MATERI,MATER)
        ELSE
          MATER=' '
        ENDIF
        CARAC=' '
        CALL GETVID(' ','CARA_ELEM',1,1,1,CARAC,N0)
      ELSE
        IF (TYPE.EQ.'EVOL_THER') THEN
          CALL NTDOTH(MODELE,MATER,CARAC,K24B,LBID,LBID,INFCHA,NBPASE,
     &                INPSCO,RESUC1(1:8),NUORD)
        ELSE
          CALL NMDOME(MODELE,MATER,CARAC,INFCHA,NBPASE,INPSCO,
     &                RESUC1(1:8),NUORD)
        ENDIF
      ENDIF

C INFO. RELATIVE AUX CHARGES
      FOMULT=INFCHA//'.FCHA'
      CHARGE=INFCHA//'.LCHA'
      INFOCH=INFCHA//'.INFC'
      CALL JEEXIN(INFOCH,IRET)
      IF (IRET.NE.0) THEN
        CALL JEVEUO(INFOCH,'L',JINFC)
        NBCHAR=ZI(JINFC)
        IF (NBCHAR.NE.0) THEN
          CALL JEVEUO(CHARGE,'L',IACHAR)
          CALL WKVECT('&&'//NOMPRO//'.L_CHARGE','V V K8',NBCHAR,ICHAR)
          DO 50 I=1,NBCHAR
            ZK8(ICHAR-1+I)=ZK24(IACHAR-1+I)(1:8)
   50     CONTINUE
        ELSE
          ICHAR=1
        ENDIF
      ELSE
        NBCHAR=0
        ICHAR=1
      ENDIF
      CALL GETVID(' ','MODELE',1,1,1,MODELE,N0)
      IF (N0.NE.0) THEN
        CALL EXLIMA(' ',0,'V',MODELE,LIGREL)
      ENDIF


C ------ EVENTUELLEMENT, ON REDUIT LE CHAM_NO AUX MAILLES
C        POUR LES OPTIONS "XXXX_NOEU_XXXX
      NBMA=0
      JMAI=1
C ------- MAILLES QUI PARTICIPENT A LA MOYENNE
      CALL GETVTX(' ','MAILLE',1,1,0,K8BID,N0)
      CALL GETVTX(' ','GROUP_MA',1,1,0,K8BID,N2)
      IF (N0+N2.NE.0) THEN
        DO 60 IOPT=1,NBOPT
          OPTION=ZK16(JOPT+IOPT-1)
          IF (OPTION(6:9).EQ.'NOEU')GOTO 70
   60   CONTINUE
        GOTO 80

   70   CONTINUE
        OPTIO2=OPTION(1:5)//'ELNO'//OPTION(10:16)
        CALL RSEXCH(RESUCO,OPTIO2,ZI(JORDR),CHELEM,IRET)
        IF (IRET.NE.0) THEN
          CALL CODENT(ZI(JORDR),'G',KIORD)
          VALK(1)=OPTIO2
          VALK(2)=KIORD
          VALK(3)=OPTION
          CALL U2MESK('A','PREPOST5_4',3,VALK)
          GOTO 80

        ENDIF
        CALL DISMOI('F','NOM_MAILLA',CHELEM,'CHAM_ELEM',IBD,NOMA,IE)
        MESMAI='&&OP0106.MES_MAILLES'
        MOTCLE(1)='GROUP_MA'
        MOTCLE(2)='MAILLE'
        TYPMCL(1)='GROUP_MA'
        TYPMCL(2)='MAILLE'
        CALL RELIEM(' ',NOMA,'NU_MAILLE',' ',1,2,MOTCLE,TYPMCL,MESMAI,
     &              NBMA)
        CALL JEVEUO(MESMAI,'L',JMAI)
      ENDIF
   80 CONTINUE
C ------- RESULTAT SUR LES NOEUDS
      NBNO=0
      JNOE=1
      CALL GETVTX(' ','MAILLE_RESU',1,1,0,K8BID,N0)
      CALL GETVTX(' ','GROUP_MA_RESU',1,1,0,K8BID,N2)
      CALL GETVTX(' ','NOEUD_RESU',1,1,0,K8BID,N3)
      CALL GETVTX(' ','GROUP_NO_RESU',1,1,0,K8BID,N4)
      IF (N0+N2+N3+N4.NE.0) THEN
        DO 90 IOPT=1,NBOPT
          OPTION=ZK16(JOPT+IOPT-1)
          IF (OPTION(6:9).EQ.'NOEU')GOTO 100
   90   CONTINUE
        GOTO 110

  100   CONTINUE
        OPTIO2=OPTION(1:5)//'ELNO'//OPTION(10:16)
        CALL RSEXCH(RESUCO,OPTIO2,ZI(JORDR),CHELEM,IRET)
        IF (IRET.NE.0) THEN
          CALL CODENT(ZI(JORDR),'G',KIORD)
          VALK(1)=OPTIO2
          VALK(2)=KIORD
          VALK(3)=OPTION
          CALL U2MESK('A','PREPOST5_4',3,VALK)
          GOTO 110

        ENDIF
        CALL DISMOI('F','NOM_MAILLA',CHELEM,'CHAM_ELEM',IBD,NOMA,IE)
        MESNOE='&&OP0106.MES_NOEUDS'
        MOTCLE(1)='GROUP_MA_RESU'
        MOTCLE(2)='MAILLE_RESU'
        MOTCLE(3)='GROUP_NO_RESU'
        MOTCLE(4)='NOEUD_RESU'
        TYPMCL(1)='GROUP_MA'
        TYPMCL(2)='MAILLE'
        TYPMCL(3)='GROUP_NO'
        TYPMCL(4)='NOEUD'
        CALL RELIEM(' ',NOMA,'NU_NOEUD',' ',1,4,MOTCLE,TYPMCL,MESNOE,
     &              NBNO)
        CALL JEVEUO(MESNOE,'L',JNOE)
      ENDIF
  110 CONTINUE


C============ DEBUT DE LA BOUCLE SUR LES OPTIONS A CALCULER ============
      DO 300 IOPT=1,NBOPT
        OPTION=ZK16(JOPT+IOPT-1)

        TIME=0.D0

C  POUR THM ET TANT QUE LE NETTOYAGE N A PAS ETE FAIT

        PARTPS(1)=0.D0
        PARTPS(2)=0.D0
        PARTPS(3)=0.D0

        IF (OPTION(6:9).EQ.'NOEU') THEN
C       ================================================================
          OPTIO2=OPTION(1:5)//'ELNO'//OPTION(10:16)
          IF (OPTION(6:14).EQ.'NOEU_DEPL') THEN
            I=1
  120       CONTINUE
            IF (I.GT.NBORDR)GOTO 130
            CALL RSEXCH(RESUCO,OPTIO2,ZI(JORDR+I-1),CHELEM,IRET)
            IF (IRET.NE.0) THEN
              I=I+1
              GOTO 120

            ENDIF
  130       CONTINUE
          ENDIF

          NOOJB='12345678.00000.NUME.PRNO'
          CALL GNOMSD(NOOJB,10,14)
          PRFCHN=NOOJB(1:19)
          DO 140 I=1,NBORDR
            IORDR=ZI(JORDR+I-1)
            CALL RSEXCH(RESUCO,OPTIO2,IORDR,CHELEM,IRET)
            IF (IRET.NE.0) THEN
              CALL CODENT(IORDR,'G',KIORD)
              VALK(1)=OPTIO2
              VALK(2)=KIORD
              VALK(3)=OPTION
              CALL U2MESK('A','PREPOST5_4',3,VALK)
              GOTO 140

            ENDIF
            CALL RSEXCH(RESUCO,OPTION,IORDR,CHAMNO,IRET)
            IF (IRET.EQ.101) THEN
              CALL CODENT(IORDR,'G',KIORD)
              VALK(1)=RESUCO
              VALK(2)=KIORD
              VALK(3)=OPTION
              CALL U2MESK('A','PREPOST5_5',3,VALK)
              GOTO 140

            ELSEIF (IRET.GT.110) THEN
              CALL CODENT(IORDR,'G',KIORD)
              VALK(1)=KIORD
              VALK(2)=OPTION
              CALL U2MESK('A','PREPOST5_6',2,VALK)
              GOTO 140

            ELSEIF (IRET.GT.111) THEN
              CALL CODENT(IORDR,'G',KIORD)
              VALK(1)=RESUCO
              VALK(2)=KIORD
              VALK(3)=OPTION
              CALL U2MESK('A','PREPOST5_7',3,VALK)
              GOTO 140

            ENDIF
            CALL JEEXIN(CHAMNO(1:19)//'.REFE',IRET)
            IF (IRET.NE.0) THEN
              CALL CODENT(IORDR,'G',KIORD)
              VALK(1)=OPTION
              VALK(2)=KIORD
              CALL U2MESK('A','PREPOST5_1',2,VALK)
              CALL DETRSD('CHAM_NO',CHAMNO(1:19))
              CALL DETRSD('PROF_CHNO',CHAMNO(1:19))
            ENDIF
C
C ----------- ON REDUIT LE CHAMP SUR LES MAILLES DEMANDEES
C
            CHAMS0='&&OP0106.CHAMS0'
            CHAMS1='&&OP0106.CHAMS1'
            CALL CELCES(CHELEM,'V',CHAMS0)
            IF (NBMA.NE.0) THEN
              CALL CESRED(CHAMS0,NBMA,ZI(JMAI),0,K8BID,'V',CHAMS0)
            ENDIF
            CALL CESCNS(CHAMS0,' ','V',CHAMS1)
            IF (NBNO.NE.0) THEN
              CALL CNSRED(CHAMS1,NBNO,ZI(JNOE),0,K8BID,'V',CHAMS1)
            ENDIF
            CALL CNSCNO(CHAMS1,PRFCHN,'NON','G',CHAMNO,'F',IBID)
            CALL RSNOCH(RESUCO,OPTION,IORDR,' ')
            CALL DETRSD('CHAM_ELEM_S',CHAMS0)
            CALL DETRSD('CHAM_NO_S',CHAMS1)
  140     CONTINUE


        ELSEIF ((OPTION(1:9).EQ.'FORC_NODA') .OR.
     &          (OPTION.EQ.'REAC_NODA')) THEN
          IF (MODELE(1:8).EQ.'&&'//NOMPRO) THEN
            CALL U2MESS('F','CALCULEL3_50')
          ENDIF
C       ================================================================

          IF (OPTION.EQ.'FORC_NODA_NONL') THEN
            NUMREF=' '
            CALL WKVECT(RESUC1//'.REFD','G V K24',7,JREF)
            CALL JEVEUO(RESUCO//'.REFD','L',LREF)
            ZK24(JREF)=ZK24(LREF)
            ZK24(JREF+1)=ZK24(LREF+1)
            ZK24(JREF+2)=ZK24(LREF+2)
            ZK24(JREF+3)=ZK24(LREF+3)
            ZK24(JREF+4)=ZK24(LREF+4)
            ZK24(JREF+5)=ZK24(LREF+5)
            ZK24(JREF+6)=ZK24(LREF+6)
            IF (ZK24(JREF).NE.' ') THEN
              CALL DISMOI('F','NOM_NUME_DDL',ZK24(JREF),'MATR_ASSE',
     &                    IBID,NUMREF,IRET)
            ENDIF
          ENDIF

          IF (TYSD.EQ.'MODE_MECA' .OR. TYSD.EQ.'DYNA_TRANS') THEN
            NUMREF=' '
            CALL JEVEUO(RESUCO//'.REFD','L',JREF)
            IF (ZK24(JREF).NE.' ') THEN
              CALL DISMOI('F','NOM_NUME_DDL',ZK24(JREF),'MATR_ASSE',
     &                    IBID,NUMREF,IRET)
            ENDIF
          ENDIF

          DO 290 I=1,NBORDR
            CALL JEMARQ()
            IORDR=ZI(JORDR+I-1)
            FOMULT=INFCHA//'.FCHA'
            CHARGE=INFCHA//'.LCHA'
            INFOCH=INFCHA//'.INFC'
            CALL JEEXIN(INFOCH,IRET)
            IF (IRET.NE.0) THEN
              CALL JEVEUO(INFOCH,'L',JINFC)
              NBCHAR=ZI(JINFC)
              IF (NBCHAR.NE.0) THEN
                CALL JEVEUO(CHARGE,'L',IACHAR)
                CALL JEDETR('&&'//NOMPRO//'.L_CHARGE')
                CALL WKVECT('&&'//NOMPRO//'.L_CHARGE','V V K8',NBCHAR,
     &                      ICHAR)
                DO 150 II=1,NBCHAR
                  ZK8(ICHAR-1+II)=ZK24(IACHAR-1+II)(1:8)
  150           CONTINUE
              ELSE
                ICHAR=1
              ENDIF
            ELSE
              NBCHAR=0
              ICHAR=1
            ENDIF
            CALL EXLIMA(' ',0,'V',MODELE,LIGREL)

            VECHMP=' '
            VACHMP=' '
            CNCHMP=' '
            VECGMP=' '
            VACGMP=' '
            CNCGMP=' '
            VEFPIP=' '
            VAFPIP=' '
            CNFPIP=' '
            ETAN=0.D0
            VFONO=' '
            VAFONO=' '
            VRENO='&&'//NOMPRO//'           .RELR'
            VARENO='&&'//NOMPRO//'           .RELR'

            NH=0
            IF (TYSD(1:8).EQ.'FOURIER_') THEN
              CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8BID)
              NH=ZI(JNMO)
            ENDIF

            CALL RSEXCH(RESUCO,'SIEF_ELGA',IORDR,SIGMA,IRET)
            IF (IRET.NE.0) THEN
              CALL RSEXCH(RESUCO,'SIEF_ELGA_DEPL',IORDR,SIGMA,IRET2)
              IF (IRET2.NE.0 .AND. OPTION.NE.'FORC_NODA_NONL') THEN
                CALL CODENT(IORDR,'G',KIORD)
                VALK(1)=KIORD
                VALK(2)=OPTION
                CALL U2MESK('A','PREPOST5_2',2,VALK)
                GOTO 280

              ENDIF
              IF (IRET2.NE.0 .AND. OPTION.EQ.'FORC_NODA_NONL') THEN
                SIGMA=' '
              ENDIF
            ENDIF

            CALL RSEXCH(RESUCO,'DEPL',IORDR,CHDEPL,IRET)
            IF (IRET.NE.0) THEN
              CALL CODENT(IORDR,'G',KIORD)
              VALK(1)=KIORD
              VALK(2)=OPTION
              CALL U2MESK('A','PREPOST5_3',2,VALK)
              GOTO 280

            ELSE

C            CREATION D'UN VECTEUR ACCROISSEMENT DE DEPLACEMENT NUL
C            POUR LE CALCUL DE FORC_NODA DANS LES POU_D_T_GD

              CHDEP2='&&'//NOMPRO//'.CHDEP_NUL'
              CALL COPISD('CHAMP_GD','V',CHDEPL,CHDEP2)
              CALL JELIRA(CHDEP2//'.VALE','LONMAX',NBDDL,K8BID)
              CALL JERAZO(CHDEP2//'.VALE',NBDDL,1)
            ENDIF

C             -- CALCUL D'UN NUME_DDL "MINIMUM" POUR ASASVE :
            IF (TYSD.EQ.'MODE_MECA' .OR. TYSD.EQ.'DYNA_TRANS') THEN
              NUME=NUMREF(1:14)//'.NUME'
            ELSE
              CALL NUMECN(MODELE,CHDEPL,NUME)
              IF (OPTION.EQ.'FORC_NODA_NONL') THEN
                IF (NUMREF.NE.' ')NUME=NUMREF(1:14)//'.NUME'
              ENDIF
            ENDIF

            CALL RSEXCH(RESUCO,'VITE',IORDR,CHVIVE,IRET)
            IF (IRET.EQ.0) THEN
              CHVIVE='&&'//NOMPRO//'.CHVIT_NUL'
              CALL COPISD('CHAMP_GD','V',CHDEPL,CHVIVE)
              CALL JELIRA(CHVIVE(1:19)//'.VALE','LONMAX',NBDDL,K8BID)
              CALL JERAZO(CHVIVE(1:19)//'.VALE',NBDDL,1)
            ENDIF
            CALL RSEXCH(RESUCO,'ACCE',IORDR,CHACVE,IRET)
            IF (IRET.EQ.0) THEN
              CHACVE='&&'//NOMPRO//'.CHACC_NUL'
              CALL COPISD('CHAMP_GD','V',CHDEPL,CHACVE)
              CALL JELIRA(CHACVE(1:19)//'.VALE','LONMAX',NBDDL,K8BID)
              CALL JERAZO(CHACVE(1:19)//'.VALE',NBDDL,1)
            ENDIF

            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAD,CTYP)
              TIME=ZR(IAD)
            ENDIF

            CALL VRCINS(MODELE,MATER,CARAC,TIME,CHVARC(1:19),CODRET)

C           --- CALCUL DES VECTEURS ELEMENTAIRES ---
            IF (OPTION.NE.'FORC_NODA_NONL') THEN
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            ELSE
              IF (I.EQ.1) THEN
                COMPOR='&&OP0106.COMPOR'
                MCL(1)='COMP_INCR'
                CALL NMDOCC(COMPOR(1:19),MODELE,1,MCL,NMCMP2,NCMPMA,
     &                      .TRUE.)
              ENDIF
            ENDIF
            FNOEVO=.FALSE.
            CALL VEFNME(MODELE,SIGMA,CARAC,CHDEPL,CHDEP2,VFONO,MATER,
     &                  COMPOR,NH,FNOEVO,PARTPS,K24BID,CHVARC,LIGREL,
     &                  INFCHA,OPTION)

C           --- ASSEMBLAGE DES VECTEURS ELEMENTAIRES ---
            CALL ASASVE(VFONO,NUME,'R',VAFONO)

C           --- CREATION DE LA STRUCTURE CHAM_NO ---
            IF (OPTION.EQ.'FORC_NODA_NONL') THEN
              CALL RSEXCH(RESUC1,'DEPL',IORDR,CHAMNO,IRET)
              CALL RSADPA(RESUC1,'E',1,'INST',IORDR,0,LTPS2,K8BID)
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,LTPS,K8BID)
              ZR(LTPS2)=ZR(LTPS)
            ELSE
              CALL RSEXCH(RESUCO,OPTION,IORDR,CHAMNO,IRET)
            ENDIF
            CALL JEEXIN(CHAMNO(1:19)//'.REFE',IRET)
            IF (IRET.NE.0) THEN
              CALL CODENT(IORDR,'G',KIORD)
              VALK(1)=OPTION
              VALK(2)=KIORD
              CALL U2MESK('A','PREPOST5_1',2,VALK)
              CALL DETRSD('CHAM_NO',CHAMNO(1:19))
            ENDIF
            CALL VTCREB(CHAMNO,NUME,'G','R',NEQ)
            CALL JEVEUO(CHAMNO(1:19)//'.VALE','E',JNOCH)

C           --- REMPLISSAGE DE L'OBJET .VALE DU CHAM_NO ---
            CALL JEVEUO(VAFONO,'L',JFO)
            CALL JEVEUO(ZK24(JFO)(1:19)//'.VALE','L',JFONO)
            CALL JELIRA(ZK24(JFO)(1:19)//'.VALE','LONMAX',LVAFON,K8BID)
            CALL JELIRA(CHAMNO(1:19)//'.VALE','LONMAX',LONCH,K8BID)

C           --- STOCKAGE DES FORCES NODALES ---
            IF (OPTION(1:9).EQ.'FORC_NODA') THEN
              DO 160 J=0,LONCH-1
                ZR(JNOCH+J)=ZR(JFONO+J)
  160         CONTINUE
              GOTO 270

            ENDIF

C           --- CALCUL DES FORCES NODALES DE REACTION

            IF (CHARGE.NE.' ') THEN

              PARTPS(1)=TIME

C --- CHARGES NON PILOTEES (TYPE_CHARGE: 'FIXE_CSTE')

              CALL VECHME(TYPCAL,MODELE,CHARGE,INFOCH,PARTPS,CARAC,
     &                    MATER,CHVARC,LIGREL,VAPRIN,NOPASE,TYPESE,
     &                    STYPSE,VECHMP)
              CALL ASASVE(VECHMP,NUME,'R',VACHMP)
              CALL ASCOVA('D',VACHMP,FOMULT,'INST',TIME,'R',CNCHMP)

C --- CHARGES SUIVEUSE (TYPE_CHARGE: 'SUIV')

              CALL DETRSD('CHAMP_GD',BIDON)
              CALL VTCREB(BIDON,NUME,'G','R',NEQ)
              CALL VECGME(MODELE,CARAC,MATER,CHARGE,INFOCH,PARTPS,
     &                    CHDEPL,BIDON,VECGMP,PARTPS,COMPOR,K24BID,
     &                    LIGREL,CHVIVE)
              CALL ASASVE(VECGMP,NUME,'R',VACGMP)
              CALL ASCOVA('D',VACGMP,FOMULT,'INST',TIME,'R',CNCGMP)

C --- POUR UN EVOL_NOLI, PRISE EN COMPTE DES FORCES PILOTEES

              IF (TYSD.EQ.'EVOL_NOLI') THEN

C - CHARGES PILOTEES (TYPE_CHARGE: 'FIXE_PILO')

                CALL VEFPME(MODELE,CARAC,MATER,CHARGE,INFOCH,PARTPS,
     &                      K24BID,VEFPIP,LIGREL)
                CALL ASASVE(VEFPIP,NUME,'R',VAFPIP)
                CALL ASCOVA('D',VAFPIP,FOMULT,'INST',TIME,'R',CNFPIP)

C - RECUPERATION DU PARAMETRE DE CHARGE ETAN DANS LA SD EVOL_NOLI

                CALL RSADPA(RESUCO,'L',1,'ETA_PILOTAGE',IORDR,0,IAD,
     &                      CTYP)
                ETAN=ZR(IAD)

              ENDIF

C --- CALCUL DU CHAMNO DE REACTION PAR DIFFERENCE DES FORCES NODALES
C --- ET DES FORCES EXTERIEURES MECANIQUES NON SUIVEUSES

              CALL JEVEUO(CNCHMP(1:19)//'.VALE','L',JCHMP)
              CALL JEVEUO(CNCGMP(1:19)//'.VALE','L',JCGMP)
              DO 170 J=0,LONCH-1
                ZR(JNOCH+J)=ZR(JFONO+J)-ZR(JCHMP+J)-ZR(JCGMP+J)
  170         CONTINUE
              IF ((TYSD.EQ.'EVOL_NOLI') .AND. (ETAN.NE.0.D0)) THEN
                CALL JEVEUO(CNFPIP(1:19)//'.VALE','L',JFPIP)
                DO 180 J=0,LONCH-1
                  ZR(JNOCH+J)=ZR(JNOCH+J)-ETAN*ZR(JFPIP+J)
  180           CONTINUE
              ENDIF

            ELSE

C             --- CALCUL DU CHAMNO DE REACTION PAR RECOPIE DE FORC_NODA

              DO 190 J=0,LONCH-1
                ZR(JNOCH+J)=ZR(JFONO+J)
  190         CONTINUE

            ENDIF

C           --- TRAITEMENT DES MODE_MECA ---
            IF (TYSD.EQ.'MODE_MECA' .AND. TYPMO(1:8).EQ.'MODE_DYN') THEN
              CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORDR,0,IAD,CTYP)
              OMEGA2=ZR(IAD)
              CALL JEVEUO(CHDEPL(1:19)//'.VALE','L',LDEPL)
              CALL JELIRA(CHDEPL(1:19)//'.VALE','LONMAX',LONC2,K8BID)
              CALL WKVECT('&&'//NOMPRO//'.TRAV','V V R',LONC2,LTRAV)
              IF (LMAT.EQ.0) CALL U2MESS('F','PREPOST3_81')
              CALL MRMULT('ZERO',LMAT,ZR(LDEPL),'R',ZR(LTRAV),1)
              DO 200 J=0,LONCH-1
                ZR(JNOCH+J)=ZR(JNOCH+J)-OMEGA2*ZR(LTRAV+J)
  200         CONTINUE
              CALL JEDETR('&&'//NOMPRO//'.TRAV')

C           --- TRAITEMENT DES MODE_STAT ---
            ELSEIF (TYSD.EQ.'MODE_MECA' .AND.
     &              TYPMO(1:8).EQ.'MODE_STA') THEN
              CALL RSADPA(RESUCO,'L',1,'TYPE_DEFO',IORDR,0,IAD,CTYP)
              IF (ZK16(IAD)(1:9).EQ.'FORC_IMPO') THEN
                CALL RSADPA(RESUCO,'L',1,'NUME_DDL',IORDR,0,IAD,CTYP)
                INUME=ZI(IAD)
                ZR(JNOCH+INUME-1)=ZR(JNOCH+INUME-1)-1.D0
              ELSEIF (ZK16(IAD)(1:9).EQ.'ACCE_IMPO') THEN
                CALL JELIRA(CHDEPL(1:19)//'.VALE','LONMAX',LONC2,K8BID)
                CALL RSADPA(RESUCO,'L',1,'COEF_X',IORDR,0,IAD,CTYP)
                COEF(1)=ZR(IAD)
                CALL RSADPA(RESUCO,'L',1,'COEF_Y',IORDR,0,IAD,CTYP)
                COEF(2)=ZR(IAD)
                CALL RSADPA(RESUCO,'L',1,'COEF_Z',IORDR,0,IAD,CTYP)
                COEF(3)=ZR(IAD)
                CALL WKVECT('&&'//NOMPRO//'.POSI_DDL','V V I',3*LONC2,
     &                      JDDL)
                CALL PTEDDL('NUME_DDL',NUME,3,NOMCMP,LONC2,ZI(JDDL))
                CALL WKVECT('&&'//NOMPRO//'.POSI_DDR','V V R',LONC2,
     &                      JDDR)
                DO 220 IC=1,3
                  IND=LONC2*(IC-1)
                  DO 210 J=0,LONC2-1
                    ZR(JDDR+J)=ZR(JDDR+J)+ZI(JDDL+IND+J)*COEF(IC)
  210             CONTINUE
  220           CONTINUE
                CALL WKVECT('&&'//NOMPRO//'.TRAV','V V R',LONC2,LTRAV)
                IF (LMAT.EQ.0) CALL U2MESS('F','PREPOST3_81')
                CALL MRMULT('ZERO',LMAT,ZR(JDDR),'R',ZR(LTRAV),1)
                DO 230 J=0,LONCH-1
                  ZR(JNOCH+J)=ZR(JNOCH+J)-ZR(LTRAV+J)
  230           CONTINUE
                CALL JEDETR('&&'//NOMPRO//'.POSI_DDR')
                CALL JEDETR('&&'//NOMPRO//'.POSI_DDL')
                CALL JEDETR('&&'//NOMPRO//'.TRAV')
              ENDIF

C           --- TRAITEMENT DE DYNA_TRANS ---
            ELSEIF (TYSD.EQ.'DYNA_TRANS') THEN
              CALL RSEXCH(RESUCO,'ACCE',IORDR,CHACCE,IRET)
              IF (IRET.EQ.0) THEN
                CALL JEVEUO(CHACCE(1:19)//'.VALE','L',LACCE)
                CALL WKVECT('&&'//NOMPRO//'.TRAV','V V R',LONCH,LTRAV)
                IF (LMAT.EQ.0) CALL U2MESS('F','PREPOST3_81')
                CALL MRMULT('ZERO',LMAT,ZR(LACCE),'R',ZR(LTRAV),1)
                DO 240 J=0,LONCH-1
                  ZR(JNOCH+J)=ZR(JNOCH+J)+ZR(LTRAV+J)
  240           CONTINUE
                CALL JEDETR('&&'//NOMPRO//'.TRAV')
              ELSE
                CALL U2MESS('A','CALCULEL3_1')
              ENDIF

C           --- TRAITEMENT DE DYNA_HARMO ---
            ELSEIF (TYSD.EQ.'DYNA_HARMO') THEN
              CALL RSEXCH(RESUCO,'ACCE',IORDR,CHACCE,IRET)
              IF (IRET.EQ.0) THEN
                CALL JEVEUO(CHACCE(1:19)//'.VALE','L',LACCE)
                CALL WKVECT('&&'//NOMPRO//'.TRAV','V V C',LONCH,LTRAV)
                IF (LMAT.EQ.0) CALL U2MESS('F','PREPOST3_81')
                CALL MCMULT('ZERO',LMAT,ZC(LACCE),'C',ZC(LTRAV),1)
                DO 250 J=0,LONCH-1
                  ZR(JNOCH+J)=ZR(JNOCH+J)+DBLE(ZC(LTRAV+J))
  250           CONTINUE
                CALL JEDETR('&&'//NOMPRO//'.TRAV')
              ELSE
                CALL U2MESS('A','CALCULEL3_1')
              ENDIF

C           --- TRAITEMENT DE EVOL_NOLI ---
            ELSEIF (TYSD.EQ.'EVOL_NOLI') THEN
              CALL RSEXCH(RESUCO,'ACCE',IORDR,CHACCE,IRET)
              IF (IRET.EQ.0) THEN
                OPTIO2='M_GAMMA'

C               --- CALCUL DES MATRICES ELEMENTAIRES DE MASSE
                CALL MEMAM2(OPTIO2,MODELE,NBCHAR,ZK8(ICHAR),MATER,CARAC,
     &                      COMPOR,EXITIM,TIME,CHACCE,VRENO,'V',LIGREL)

C               --- ASSEMBLAGE DES VECTEURS ELEMENTAIRES ---
                CALL ASASVE(VRENO,NUME,'R',VARENO)
                CALL JEVEUO(VARENO,'L',JRE)
                CALL JEVEUO(ZK24(JRE)(1:19)//'.VALE','L',JRENO)
                DO 260 J=0,LONCH-1
                  ZR(JNOCH+J)=ZR(JNOCH+J)+ZR(JRENO+J)
  260           CONTINUE
              ENDIF
            ENDIF

  270       CONTINUE
C
            IF (OPTION.EQ.'FORC_NODA_NONL') THEN
              CALL RSNOCH(RESUC1,'DEPL',IORDR,' ')
            ELSE
              CALL RSNOCH(RESUCO,OPTION,IORDR,' ')
            ENDIF
            IF (TYPE.EQ.'EVOL_THER') THEN
              CALL NTDOTH(MODELE,MATER,CARAC,K24B,LBID,LBID,INFCHA,
     &                    NBPASE,INPSCO,RESUC1(1:8),IORDR)
            ELSE
              CALL NMDOME(MODELE,MATER,CARAC,INFCHA,NBPASE,INPSCO,
     &                    RESUC1(1:8),IORDR)
            ENDIF
            CALL DETRSD('CHAMP_GD','&&'//NOMPRO//'.SIEF')
            CALL DETRSD('VECT_ELEM',VFONO(1:8))
            CALL DETRSD('VECT_ELEM',VRENO(1:8))
            CALL DETRSD('VECT_ELEM',VECHMP(1:8))
            CALL DETRSD('VECT_ELEM',VECGMP(1:8))
            CALL DETRSD('VECT_ELEM',VEFPIP(1:8))
            CALL DETRSD('CHAMP_GD',CNCHMP(1:8)//'.ASCOVA')
            CALL DETRSD('CHAMP_GD',CNCGMP(1:8)//'.ASCOVA')
            CALL DETRSD('CHAMP_GD',CNFPIP(1:8)//'.ASCOVA')
            CALL JEDETR(VACHMP(1:8))
            CALL JEDETR(VACGMP(1:8))
            CALL JEDETR(VAFPIP(1:8))
            CALL JEDETR(VACHMP(1:6)//'00.BIDON')
            CALL JEDETR(VACGMP(1:6)//'00.BIDON')
            CALL JEDETR(VAFPIP(1:6)//'00.BIDON')
            CALL JEDETR(VACHMP(1:6)//'00.BIDON     .VALE')
            CALL JEDETR(VACGMP(1:6)//'00.BIDON     .VALE')
            CALL JEDETR(VAFPIP(1:6)//'00.BIDON     .VALE')
            CALL JEDETR(VACHMP(1:6)//'00.BIDON     .DESC')
            CALL JEDETR(VACGMP(1:6)//'00.BIDON     .DESC')
            CALL JEDETR(VAFPIP(1:6)//'00.BIDON     .DESC')
            CALL JEDETR(VACHMP(1:6)//'00.BIDON     .REFE')
            CALL JEDETR(VACGMP(1:6)//'00.BIDON     .REFE')
            CALL JEDETR(VAFPIP(1:6)//'00.BIDON     .REFE')
            CALL JEDETR(VACHMP(1:8)//'.ASCOVA')
            CALL JEDETR(VACGMP(1:8)//'.ASCOVA')
            CALL JEDETR(VAFPIP(1:8)//'.ASCOVA')
  280       CONTINUE
            CALL JEDEMA()
  290     CONTINUE
        ELSE
          CALL U2MESK('F','CALCULEL6_10',1,OPTION)
        ENDIF
  300 CONTINUE
C============= FIN DE LA BOUCLE SUR LES OPTIONS A CALCULER =============


      IF (NBMA.NE.0) CALL JEDETR(MESMAI)
      IF (NBNO.NE.0) CALL JEDETR(MESNOE)
      CALL JEDETR(KNUM)
      CALL DETRSD('CHAMP_GD',BIDON)

C --- ON REMET LE MECANISME D'EXCEPTION A SA VALEUR INITIALE
      CALL ONERRF(COMPEX,K16BID,IBID)

  310 CONTINUE
      CALL INFBAV()
      CALL JEDEMA()

      END
