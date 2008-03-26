      SUBROUTINE OP0106(IER)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 25/03/2008   AUTEUR REZETTE C.REZETTE 
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

C     COMMANDE :  CALC_NO
C        CALCULE DES FORCES NODALES ET DES REACTIONS EN MECANIQUE.
C        CALCUL DES GRANDEURS AUX NOEUDS.
C-----------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       15/05/02 (OB): CALCUL DE LA SENSIBILITE DU FLUX THERMIQUE +
C                      MODIFS FORMELLES (INDENTATION...)
C-----------------------------------------------------------------------

      IMPLICIT   NONE

C 0.1. ==> ARGUMENTS

      INTEGER IER

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

      LOGICAL FNOEVO
      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='OP0106')

      INTEGER NBPASE,NRPASS,NBPASS,TYPESE,ADRECG, NUORD
      INTEGER IAUX,JAUX,IBID
      INTEGER I,IACHAR,IAD,IBD,IC,ICHAR,IE,IND,INUME,IOPT,IORDR,IRET,
     &        IRET1,IRET2,J,JCGMP,JCHMP,JDDL,JDDR,JFO,JFONO,JFPIP,JINFC,
     &        JNMO,JNOCH,JOPT,JORDR,JRE,JRENO,LACCE,LDEPL,LMAT,LONC2,
     &        LONCH,LREF,LTRAV,LVAFON,N0,N2,NBCHAR,NBDDL,NBOPT,
     &        NBORDR,NC,NEQ,NH,NP,NBMA,JMAI,N3,N4,NBNO,JNOE, II
      REAL*8 TIME,OMEGA2,PREC,COEF(3),PARTPS(3),ETAN
      CHARACTER*4  TYPCAL
      CHARACTER*8  K8BID,RESUC1,CTYP,CRIT,NOMCMP(3),NOPASE,NOMA
      CHARACTER*13 INPSCO
      CHARACTER*16 OPTION,OPTIO2,TYSD,TYPE,OPER,TYPMCL(4),MOTCLE(4)
      CHARACTER*19 LERES0,RESUCO,KNUM,INFCHA,LIGREL
      CHARACTER*19 CHDEP2,CHAMS0,CHAMS1,PRFCHN
      CHARACTER*24 CHAMNO,NUME,VFONO,VAFONO,SIGMA,CHDEPL
      CHARACTER*24 MODELE,MATER,CARAC,CHARGE,FOMULT,INFOCH
      CHARACTER*24 VECHMP,VACHMP,CNCHMP,VEFPIP,VAFPIP,CNFPIP,MASSE
      CHARACTER*24 VECGMP,VACGMP,CNCGMP,BIDON,CHVARC
      CHARACTER*24 K24BID,CHELEM,CHACCE,VRENO,VARENO
      CHARACTER*24 COMPOR,CHVIVE,CHACVE,MESMAI,MESNOE
      CHARACTER*24 NORECG,VAPRIN,STYPSE,NOOJB, K24B,VALK(3)
      CHARACTER*8 KIORD

      LOGICAL EXITIM, LBID
C     ------------------------------------------------------------------
      DATA INFCHA/'&&INFCHA.INFCHA'/
      DATA NOMCMP/'DX','DY','DZ'/
      DATA K24BID/' '/
      DATA TYPCAL/'MECA'/
      DATA CHVARC /'&&OP0106.CHVARC'/
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ()

      CALL INFMUE()
      CNFPIP = ' '
      VEFPIP = ' '
      VAFPIP = ' '
      ETAN = 0.D0

      CALL GETRES(RESUC1,TYPE,OPER)
      CALL GETVID(' ','RESULTAT',1,1,1,RESUCO,N0)

      IF (RESUC1.NE.RESUCO(1:8)) THEN
        VALK(1)=RESUC1
        VALK(2)=RESUCO
        CALL U2MESK('F','PREPOST3_79', 2 ,VALK)
      END IF

      CALL GETTCO(RESUCO(1:8),TYSD)
C               12   345678   90123
      INPSCO = '&&'//NOMPRO//'_PSCO'
C               12   345678   9012345678901234
      NORECG = '&&'//NOMPRO//'_PARA_SENSI     '
      KNUM = '&&'//NOMPRO//'.NUME_ORDRE'

C=======================================================================
C -- SENSIBILITE : NOMBRE DE PASSAGES
C              12   345678
      K8BID = '&&'//NOMPRO
      IAUX = 1
      CALL PSLECT(' ',IBID,K8BID,RESUCO,IAUX,NBPASE,INPSCO,IRET)
      IAUX = 1
      JAUX = 1
      CALL PSRESE(' ',IBID,IAUX,RESUCO,JAUX,NBPASS,NORECG,IRET)
      CALL JEVEUO(NORECG,'L',ADRECG)
C=======================================================================
      CALL GETVTX(' ','OPTION',1,1,0,OPTION,N2)
      NBOPT = -N2
      CALL WKVECT('&&'//NOMPRO//'.OPTION','V V K16',NBOPT,JOPT)
      CALL GETVTX(' ','OPTION',1,1,NBOPT,ZK16(JOPT),N2)

      CALL GETVR8(' ','PRECISION',1,1,1,PREC,NP)
      CALL GETVTX(' ','CRITERE',1,1,1,CRIT,NC)

C============ DEBUT DE LA BOUCLE SUR LE NOMBRE DE PASSAGES =============
      DO 250,NRPASS = 1,NBPASS

C        POUR LE PASSAGE NUMERO NRPASS :
C        . LERES0 : NOM DU CHAMP DE RESULTAT A COMPLETER
C                   C'EST RESUCO POUR UN CALCUL STANDARD, UN NOM
C                   COMPOSE A PARTIR DE RESUC1 ET NOPASE POUR UN CALCUL
C                   DE SENSIBILITE
C        . NOPASE : NOM DU PARAMETRE DE SENSIBILITE EVENTUELLEMENT

        LERES0 = ZK24(ADRECG+2*NRPASS-2) (1:8)
        NOPASE = ZK24(ADRECG+2*NRPASS-1) (1:8)

C DANS LE CAS D'UN CALCUL STANDARD :
        IF (NOPASE.EQ.' ') THEN
          TYPESE = 0
        ELSE
C DANS LE CAS D'UN CALCUL DE DERIVE AVEC UN TYPESE PROVISOIRE
          TYPESE = 3
        END IF
        STYPSE = ' '

C RECUPERATION DES NUMEROS D'ORDRE AU PREMIER PASSAGE
        IF (NRPASS.EQ.1) THEN
          CALL RSUTNU(LERES0,' ',0,KNUM,NBORDR,PREC,CRIT,IRET)
          IF (IRET.EQ.10) THEN
            CALL U2MESK('S','CALCULEL4_8',1,LERES0)
            GO TO 260
          END IF
          IF (IRET.NE.0) THEN
            CALL U2MESS('S','ALGORITH3_41')
            GO TO 260
          END IF
          CALL JEVEUO(KNUM,'L',JORDR)
          BIDON = '&&'//NOMPRO//'.BIDON'
        END IF

C TRI DES OPTIONS SUIVANT TYSD
        LMAT = 0
        EXITIM = .FALSE.
        IF (TYSD.EQ.'EVOL_ELAS' .OR. TYSD.EQ.'EVOL_NOLI') THEN
          EXITIM = .TRUE.
        ELSE IF (TYSD.EQ.'MODE_MECA' .OR. TYSD(1:9).EQ.'MODE_STAT' .OR.
     &           TYSD.EQ.'DYNA_TRANS') THEN
          CALL JEEXIN(LERES0//'.REFD',IRET)
          IF (IRET.NE.0) THEN
            CALL JEVEUO(LERES0//'.REFD','L',LREF)
            MASSE = ZK24(LREF+1)
            IF (MASSE.NE.' ') THEN
              CALL MTDSCR(MASSE)
              CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT)
            END IF
          END IF
          IF (TYSD.EQ.'DYNA_TRANS') EXITIM = .TRUE.
        ELSE IF (TYSD.EQ.'DYNA_HARMO') THEN
          DO 10 IOPT = 1,NBOPT
            OPTION = ZK16(JOPT+IOPT-1)
            IF (OPTION(6:9).NE.'NOEU') THEN
              CALL U2MESK('A','PREPOST3_80',1,TYSD)
              GO TO 260
            END IF
   10     CONTINUE
          CALL JEEXIN(LERES0//'.REFD',IRET)
          IF (IRET.NE.0) THEN
            CALL JEVEUO(LERES0//'.REFD','L',LREF)
            MASSE = ZK24(LREF+1)
            IF (MASSE.NE.' ') THEN
              CALL MTDSCR(MASSE)
              CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT)
            END IF
          END IF
        ELSE IF (TYSD.EQ.'ACOU_HARMO' .OR. TYSD.EQ.'MODE_ACOU') THEN
          DO 20 IOPT = 1,NBOPT
            OPTION = ZK16(JOPT+IOPT-1)
            IF (OPTION(6:9).NE.'NOEU') THEN
              CALL U2MESK('A','PREPOST3_80',1,TYSD)
              GO TO 260
            END IF
   20     CONTINUE
        END IF

        IF (NRPASS.EQ.1) THEN
C ON N'ENREGISTRE LES DONNEES RELATIVES AUX DERIVEES QU'AU 1ER PASSAGE
C EN OUTPUT --> INFCHA ET INPSCO
          CARAC = ' '
          CHARGE = ' '
          MATER = ' '
C   SUPPRESSION DE LA LIGNE SUIVANTE
C          MODELE = '&&'//NOMPRO
          MODELE = ' '
          NUORD  = ZI(JORDR)
          IF (TYPE.EQ.'EVOL_THER') THEN
            CALL NTDOTH(MODELE,MATER,CARAC,K24B,LBID,LBID,INFCHA,
     &                    NBPASE,INPSCO,RESUC1,NUORD)
          ELSE
            CALL NMDOME(MODELE,MATER,CARAC,INFCHA,NBPASE,INPSCO,
     &                RESUC1,NUORD)
          ENDIF

C INFO. RELATIVE AUX CHARGES
          FOMULT = INFCHA//'.FCHA'
          CHARGE = INFCHA//'.LCHA'
          INFOCH = INFCHA//'.INFC'
          CALL JEEXIN(INFOCH,IRET)
          IF (IRET.NE.0) THEN
            CALL JEVEUO(INFOCH,'L',JINFC)
            NBCHAR = ZI(JINFC)
            IF (NBCHAR.NE.0) THEN
              CALL JEVEUO(CHARGE,'L',IACHAR)
              CALL WKVECT('&&'//NOMPRO//'.L_CHARGE','V V K8',NBCHAR,
     &                    ICHAR)
              DO 30 I = 1,NBCHAR
                ZK8(ICHAR-1+I) = ZK24(IACHAR-1+I) (1:8)
   30         CONTINUE
            ELSE
              ICHAR = 1
            END IF
          ELSE
            NBCHAR = 0
            ICHAR = 1
          END IF
          CALL GETVID(' ','MODELE',1,1,1,MODELE,N0)
          IF (N0.NE.0) THEN
            CALL EXLIMA(' ','V',MODELE,LERES0,LIGREL)
          END IF
        END IF

C ------ EVENTUELLEMENT, ON REDUIT LE CHAM_NO AUX MAILLES
C        POUR LES OPTIONS "XXXX_NOEU_XXXX

        IF (NRPASS.EQ.1) THEN
          NBMA = 0
          JMAI = 1
C ------- MAILLES QUI PARTICIPENT A LA MOYENNE
          CALL GETVID(' ','MAILLE'  ,1,1,0,K8BID,N0)
          CALL GETVID(' ','GROUP_MA',1,1,0,K8BID,N2)
          IF (N0+N2.NE.0) THEN
            DO 40 IOPT = 1,NBOPT
              OPTION = ZK16(JOPT+IOPT-1)
              IF (OPTION(6:9).EQ.'NOEU') GO TO 50
   40       CONTINUE
            GO TO 60
   50       CONTINUE
            OPTIO2 = OPTION(1:5)//'ELNO'//OPTION(10:16)
            CALL RSEXCH(LERES0,OPTIO2,ZI(JORDR),CHELEM,IRET)
            IF (IRET.NE.0) THEN
               CALL CODENT(ZI(JORDR),'G',KIORD)
               VALK(1)=OPTIO2
               VALK(2)=KIORD
               VALK(3)=OPTION
               CALL U2MESK('A','PREPOST5_4',3,VALK)
               GO TO 60
            END IF
            CALL DISMOI('F','NOM_MAILLA',CHELEM,'CHAM_ELEM',IBD,NOMA,IE)
            MESMAI = '&&OP0106.MES_MAILLES'
            MOTCLE(1) = 'GROUP_MA'
            MOTCLE(2) = 'MAILLE'
            TYPMCL(1) = 'GROUP_MA'
            TYPMCL(2) = 'MAILLE'
            CALL RELIEM(' ',NOMA,'NU_MAILLE',' ',1,2,MOTCLE,TYPMCL,
     &                  MESMAI,NBMA)
            CALL JEVEUO(MESMAI,'L',JMAI)
          END IF
   60     CONTINUE
C ------- RESULTAT SUR LES NOEUDS
          NBNO = 0
          JNOE = 1
          CALL GETVID(' ','MAILLE_RESU'  ,1,1,0,K8BID,N0)
          CALL GETVID(' ','GROUP_MA_RESU',1,1,0,K8BID,N2)
          CALL GETVID(' ','NOEUD_RESU'   ,1,1,0,K8BID,N3)
          CALL GETVID(' ','GROUP_NO_RESU',1,1,0,K8BID,N4)
          IF (N0+N2+N3+N4.NE.0) THEN
            DO 42 IOPT = 1,NBOPT
              OPTION = ZK16(JOPT+IOPT-1)
              IF (OPTION(6:9).EQ.'NOEU') GO TO 52
   42       CONTINUE
            GO TO 62
   52       CONTINUE
            OPTIO2 = OPTION(1:5)//'ELNO'//OPTION(10:16)
            CALL RSEXCH(LERES0,OPTIO2,ZI(JORDR),CHELEM,IRET)
            IF (IRET.NE.0) THEN
               CALL CODENT(ZI(JORDR),'G',KIORD)
               VALK(1)=OPTIO2
               VALK(2)=KIORD
               VALK(3)=OPTION
               CALL U2MESK('A','PREPOST5_4',3,VALK)
               GO TO 62
            END IF
            CALL DISMOI('F','NOM_MAILLA',CHELEM,'CHAM_ELEM',IBD,NOMA,IE)
            MESNOE = '&&OP0106.MES_NOEUDS'
            MOTCLE(1) = 'GROUP_MA_RESU'
            MOTCLE(2) = 'MAILLE_RESU'
            MOTCLE(3) = 'GROUP_NO_RESU'
            MOTCLE(4) = 'NOEUD_RESU'
            TYPMCL(1) = 'GROUP_MA'
            TYPMCL(2) = 'MAILLE'
            TYPMCL(3) = 'GROUP_NO'
            TYPMCL(4) = 'NOEUD'
            CALL RELIEM(' ',NOMA,'NU_NOEUD',' ',1,4,MOTCLE,TYPMCL,
     &                  MESNOE,NBNO)
            CALL JEVEUO(MESNOE,'L',JNOE)
          END IF
   62     CONTINUE
        END IF

C============ DEBUT DE LA BOUCLE SUR LES OPTIONS A CALCULER ============
        DO 240 IOPT = 1,NBOPT
          OPTION = ZK16(JOPT+IOPT-1)

C VERIFICATION DE LA COMPATIBILITE AVEC LA DERIVATION

          CALL VESECN(OPER,OPTION,NOPASE,TYPESE,IRET)
          IF (IRET.NE.0) THEN
            GO TO 240
          END IF

          TIME = 0.D0

C  POUR THM ET TANT QUE LE NETTOYAGE N A PAS ETE FAIT

          PARTPS(1) = 0.D0
          PARTPS(2) = 0.D0
          PARTPS(3) = 0.D0

          IF (OPTION(6:9).EQ.'NOEU') THEN
C       ================================================================
            OPTIO2 = OPTION(1:5)//'ELNO'//OPTION(10:16)
            IF (OPTION(6:14).EQ.'NOEU_DEPL') THEN
              I = 1
   70         CONTINUE
              IF (I.GT.NBORDR) GO TO 80
              CALL RSEXCH(LERES0,OPTIO2,ZI(JORDR+I-1),CHELEM,IRET)
              IF (IRET.NE.0) THEN
                I = I + 1
                GO TO 70
              END IF
   80         CONTINUE
            END IF

            NOOJB='12345678.00000.NUME.PRNO'
            CALL GNOMSD ( NOOJB,10,14)
            PRFCHN=NOOJB(1:19)
            DO 100 I = 1,NBORDR
              IORDR = ZI(JORDR+I-1)
              CALL RSEXCH(LERES0,OPTIO2,IORDR,CHELEM,IRET)
              IF (IRET.NE.0) THEN
                CALL CODENT(IORDR,'G',KIORD)
                VALK(1)=OPTIO2
                VALK(2)=KIORD
                VALK(3)=OPTION
                CALL U2MESK('A','PREPOST5_4',3,VALK)
                GO TO 100
              END IF
              CALL RSEXCH(LERES0,OPTION,IORDR,CHAMNO,IRET)
              IF (IRET.EQ.101) THEN
                CALL CODENT(IORDR,'G',KIORD)
                VALK(1)=LERES0
                VALK(2)=KIORD
                VALK(3)=OPTION
                CALL U2MESK('A','PREPOST5_5',3,VALK)
                GO TO 100
              ELSE IF (IRET.GT.110) THEN
                CALL CODENT(IORDR,'G',KIORD)
                VALK(1)=KIORD
                VALK(2)=OPTION
                CALL U2MESK('A','PREPOST5_6',2,VALK)
                GO TO 100
              ELSE IF (IRET.GT.111) THEN
                CALL CODENT(IORDR,'G',KIORD)
                VALK(1)=LERES0
                VALK(2)=KIORD
                VALK(3)=OPTION
                CALL U2MESK('A','PREPOST5_7',3,VALK)
                GO TO 100
              END IF
              CALL JEEXIN(CHAMNO(1:19)//'.REFE',IRET)
              IF (IRET.NE.0) THEN
                CALL CODENT(IORDR,'G',KIORD)
                VALK(1)=OPTION
                VALK(2)=KIORD
                CALL U2MESK('A','PREPOST5_1',2,VALK)
                CALL DETRSD('CHAM_NO',CHAMNO(1:19))
                CALL DETRSD('PROF_CHNO',CHAMNO(1:19))
              END IF
C
C ----------- ON REDUIT LE CHAMP SUR LES MAILLES DEMANDEES
C
              CHAMS0 = '&&OP0106.CHAMS0'
              CHAMS1 = '&&OP0106.CHAMS1'
              CALL CELCES ( CHELEM, 'V', CHAMS0 )
              IF (NBMA.NE.0) THEN
                CALL CESRED ( CHAMS0, NBMA,ZI(JMAI),0,K8BID,'V',CHAMS0)
              END IF
              CALL CESCNS ( CHAMS0, ' ', 'V', CHAMS1 )
              IF (NBNO.NE.0) THEN
                CALL CNSRED ( CHAMS1, NBNO,ZI(JNOE),0,K8BID,'V',CHAMS1)
              END IF
              CALL CNSCNO (CHAMS1,PRFCHN,'NON','G',CHAMNO,'F',IBID)
              CALL RSNOCH ( LERES0, OPTION, IORDR, ' ')
              CALL DETRSD ( 'CHAM_ELEM_S', CHAMS0 )
              CALL DETRSD ( 'CHAM_NO_S'  , CHAMS1 )
  100       CONTINUE


          ELSE IF ((OPTION.EQ.'FORC_NODA') .OR.
     &             (OPTION.EQ.'REAC_NODA')) THEN
            IF (MODELE(1:8).EQ.'&&'//NOMPRO) THEN
              CALL U2MESS('F','CALCULEL3_51')
            END IF
C       ================================================================

            DO 230 I = 1,NBORDR
              CALL JEMARQ()
              IORDR = ZI(JORDR+I-1)
              IF (TYPE.EQ.'EVOL_THER') THEN
                CALL NTDOTH(MODELE,MATER,CARAC,K24B,LBID,LBID,INFCHA,
     &                      NBPASE,INPSCO,RESUCO(1:8),IORDR)
              ELSE
                CALL NMDOME(MODELE,MATER,CARAC,INFCHA,NBPASE,INPSCO,
     &                      RESUCO(1:8),IORDR)
              ENDIF
              FOMULT = INFCHA//'.FCHA'
              CHARGE = INFCHA//'.LCHA'
              INFOCH = INFCHA//'.INFC'
              CALL JEEXIN(INFOCH,IRET)
              IF (IRET.NE.0) THEN
                 CALL JEVEUO(INFOCH,'L',JINFC)
                 NBCHAR = ZI(JINFC)
                 IF (NBCHAR.NE.0) THEN
                    CALL JEVEUO(CHARGE,'L',IACHAR)
                    CALL JEDETR('&&'//NOMPRO//'.L_CHARGE')
                    CALL WKVECT('&&'//NOMPRO//'.L_CHARGE','V V K8',
     &                   NBCHAR,ICHAR)
                    DO 31 II = 1,NBCHAR
                       ZK8(ICHAR-1+II) = ZK24(IACHAR-1+II) (1:8)
 31                 CONTINUE
                 ELSE
                    ICHAR = 1
                 END IF
              ELSE
                 NBCHAR = 0
                 ICHAR = 1
              END IF
              CALL EXLIMA(' ','V',MODELE,LERES0,LIGREL)

              VECHMP = ' '
              VACHMP = ' '
              CNCHMP = ' '
              VECGMP = ' '
              VACGMP = ' '
              CNCGMP = ' '
              VFONO = ' '
              VAFONO = ' '
              VRENO =  '&&'//NOMPRO//'           .RELR'
              VARENO = '&&'//NOMPRO//'           .RELR'

              NH = 0
              IF (TYSD(1:8).EQ.'FOURIER_') THEN
                CALL RSADPA(LERES0,'L',1,'NUME_MODE',IORDR,0,JNMO,K8BID)
                NH = ZI(JNMO)
              END IF

              CALL RSEXCH(LERES0,'SIEF_ELGA',IORDR,SIGMA,IRET)
              IF (IRET.NE.0) THEN
                CALL RSEXCH(LERES0,'SIEF_ELGA_DEPL',IORDR,SIGMA,IRET2)
                IF (IRET2.NE.0) THEN
                CALL CODENT(IORDR,'G',KIORD)
                VALK(1)=KIORD
                VALK(2)=OPTION
                CALL U2MESK('A','PREPOST5_2',2,VALK)
                  GO TO 229
                END IF
              END IF

              CALL RSEXCH(LERES0,'DEPL',IORDR,CHDEPL,IRET)
              IF (IRET.NE.0) THEN
                CALL CODENT(IORDR,'G',KIORD)
                VALK(1)=KIORD
                VALK(2)=OPTION
                CALL U2MESK('A','PREPOST5_3',2,VALK)
                GO TO 229
              ELSE

C            CREATION D'UN VECTEUR ACCROISSEMENT DE DEPLACEMENT NUL
C            POUR LE CALCUL DE FORC_NODA DANS LES POU_D_T_GD

                CHDEP2 = '&&'//NOMPRO//'.CHDEP_NUL'
                CALL COPISD('CHAMP_GD','V',CHDEPL,CHDEP2)
                CALL JELIRA(CHDEP2//'.VALE','LONMAX',NBDDL,K8BID)
                CALL JERAZO(CHDEP2//'.VALE',NBDDL,1)
              END IF

C             -- CALCUL D'UN NUME_DDL "MINIMUM" POUR ASASVE :
              NOOJB='12345678.00000.NUME.PRNO'
              CALL GNOMSD ( NOOJB,10,14)
              NUME=NOOJB(1:14)
              CALL NUMECN(MODELE,CHDEPL,NUME)

              CALL RSEXCH(LERES0,'VITE',IORDR,CHVIVE,IRET)
              IF (IRET.EQ.0) THEN
                CHVIVE = '&&'//NOMPRO//'.CHVIT_NUL'
                CALL COPISD('CHAMP_GD','V',CHDEPL,CHVIVE)
                CALL JELIRA(CHVIVE(1:19)//'.VALE','LONMAX',NBDDL,K8BID)
                CALL JERAZO(CHVIVE(1:19)//'.VALE',NBDDL,1)
              END IF
              CALL RSEXCH(LERES0,'ACCE',IORDR,CHACVE,IRET)
              IF (IRET.EQ.0) THEN
                CHACVE = '&&'//NOMPRO//'.CHACC_NUL'
                CALL COPISD('CHAMP_GD','V',CHDEPL,CHACVE)
                CALL JELIRA(CHACVE(1:19)//'.VALE','LONMAX',NBDDL,K8BID)
                CALL JERAZO(CHACVE(1:19)//'.VALE',NBDDL,1)
              END IF

              IF (EXITIM) THEN
                CALL RSADPA(LERES0,'L',1,'INST',IORDR,0,IAD,CTYP)
                TIME = ZR(IAD)
              END IF

              CALL VRCINS(MODELE,MATER,CARAC,TIME,CHVARC(1:19))

C           --- CALCUL DES VECTEURS ELEMENTAIRES ---
              CALL RSEXCH(LERES0,'COMPORTEMENT',IORDR,COMPOR,IRET1)
              FNOEVO = .FALSE.
              CALL VEFNME(MODELE,SIGMA,CARAC,CHDEPL,CHDEP2,VFONO,MATER,
     &                    COMPOR,NH,FNOEVO,PARTPS,K24BID,CHVARC,LIGREL,
     &                    INFCHA)

C           --- ASSEMBLAGE DES VECTEURS ELEMENTAIRES ---
              CALL ASASVE(VFONO,NUME,'R',VAFONO)

C           --- CREATION DE LA STRUCTURE CHAM_NO ---
              CALL RSEXCH(LERES0,OPTION,IORDR,CHAMNO,IRET)
              CALL JEEXIN(CHAMNO(1:19)//'.REFE',IRET)
              IF (IRET.NE.0) THEN
                CALL CODENT(IORDR,'G',KIORD)
                VALK(1)=OPTION
                VALK(2)=KIORD
                CALL U2MESK('A','PREPOST5_1',2,VALK)
                CALL DETRSD('CHAM_NO',CHAMNO(1:19))
              END IF
              CALL VTCREB(CHAMNO,NUME,'G','R',NEQ)
              CALL JEVEUO(CHAMNO(1:19)//'.VALE','E',JNOCH)

C           --- REMPLISSAGE DE L'OBJET .VALE DU CHAM_NO ---
              CALL JEVEUO(VAFONO,'L',JFO)
              CALL JEVEUO(ZK24(JFO) (1:19)//'.VALE','L',JFONO)
              CALL JELIRA(ZK24(JFO) (1:19)//'.VALE','LONMAX',LVAFON,
     &                    K8BID)
              CALL JELIRA(CHAMNO(1:19)//'.VALE','LONMAX',LONCH,K8BID)

C           --- STOCKAGE DES FORCES NODALES ---
              IF (OPTION.EQ.'FORC_NODA') THEN
                DO 110 J = 0,LONCH - 1
                  ZR(JNOCH+J) = ZR(JFONO+J)
  110           CONTINUE
                GO TO 220
              END IF

C           --- CALCUL DES FORCES NODALES DE REACTION

              IF (CHARGE.NE.' ') THEN

                PARTPS(1) = TIME

C --- CHARGES NON PILOTEES (TYPE_CHARGE: 'FIXE_CSTE')

                CALL VECHME(TYPCAL,MODELE,CHARGE,INFOCH,PARTPS,CARAC,
     &                      MATER,CHVARC,LIGREL,VAPRIN,NOPASE,TYPESE,
     &                      STYPSE,VECHMP)
                CALL ASASVE(VECHMP,NUME,'R',VACHMP)
                CALL ASCOVA('D',VACHMP,FOMULT,'INST',TIME,'R',CNCHMP)

C --- CHARGES SUIVEUSE (TYPE_CHARGE: 'SUIV')

                CALL DETRSD('CHAMP_GD',BIDON)
                CALL VTCREB(BIDON,NUME,'G','R',NEQ)
                CALL VECGME(MODELE,CARAC,MATER,CHARGE,INFOCH,PARTPS,
     &                      CHDEPL,BIDON,VECGMP,PARTPS,COMPOR,K24BID,
     &                      LIGREL,CHVIVE,CHACVE)
                CALL ASASVE(VECGMP,NUME,'R',VACGMP)
                CALL ASCOVA('D',VACGMP,FOMULT,'INST',TIME,'R',CNCGMP)

C --- POUR UN EVOL_NOLI, PRISE EN COMPTE DES FORCES PILOTEES

                IF (TYSD.EQ.'EVOL_NOLI') THEN

C - CHARGES PILOTEES (TYPE_CHARGE: 'FIXE_PILO')

                  CALL VEFPME(MODELE,CARAC,MATER,CHARGE,INFOCH,PARTPS,
     &                        K24BID,VEFPIP,LIGREL)
                  CALL ASASVE(VEFPIP,NUME,'R',VAFPIP)
                  CALL ASCOVA('D',VAFPIP,FOMULT,'INST',TIME,'R',CNFPIP)

C - RECUPERATION DU PARAMETRE DE CHARGE ETAN DANS LA SD EVOL_NOLI

                  CALL RSADPA(LERES0,'L',1,'ETA_PILOTAGE',IORDR,0,IAD,
     &                        CTYP)
                  ETAN = ZR(IAD)

                END IF

C --- CALCUL DU CHAMNO DE REACTION PAR DIFFERENCE DES FORCES NODALES
C --- ET DES FORCES EXTERIEURES MECANIQUES NON SUIVEUSES

                CALL JEVEUO(CNCHMP(1:19)//'.VALE','L',JCHMP)
                CALL JEVEUO(CNCGMP(1:19)//'.VALE','L',JCGMP)
                DO 120 J = 0,LONCH - 1
                  ZR(JNOCH+J) = ZR(JFONO+J) - ZR(JCHMP+J) - ZR(JCGMP+J)
  120           CONTINUE
                IF ((TYSD.EQ.'EVOL_NOLI') .AND. (ETAN.NE.0.D0)) THEN
                  CALL JEVEUO(CNFPIP(1:19)//'.VALE','L',JFPIP)
                  DO 130 J = 0,LONCH - 1
                    ZR(JNOCH+J) = ZR(JNOCH+J) - ETAN*ZR(JFPIP+J)
  130             CONTINUE
                END IF

              ELSE

C             --- CALCUL DU CHAMNO DE REACTION PAR RECOPIE DE FORC_NODA

                DO 140 J = 0,LONCH - 1
                  ZR(JNOCH+J) = ZR(JFONO+J)
  140           CONTINUE

              END IF

C           --- TRAITEMENT DES MODE_MECA ---
              IF (TYSD.EQ.'MODE_MECA') THEN
                CALL RSADPA(LERES0,'L',1,'OMEGA2',IORDR,0,IAD,CTYP)
                OMEGA2 = ZR(IAD)
                CALL JEVEUO(CHDEPL(1:19)//'.VALE','L',LDEPL)
                CALL JELIRA(CHDEPL(1:19)//'.VALE','LONMAX',LONC2,K8BID)
                CALL WKVECT('&&'//NOMPRO//'.TRAV','V V R',LONC2,LTRAV)
                IF (LMAT.EQ.0) CALL U2MESS('F','PREPOST3_81')
                CALL MRMULT('ZERO',LMAT,ZR(LDEPL),'R',ZR(LTRAV),1)
                DO 150 J = 0,LONCH - 1
                  ZR(JNOCH+J) = ZR(JNOCH+J) - OMEGA2*ZR(LTRAV+J)
  150           CONTINUE
                CALL JEDETR('&&'//NOMPRO//'.TRAV')

C           --- TRAITEMENT DES MODE_STAT ---
              ELSE IF (TYSD(1:9).EQ.'MODE_STAT') THEN
                CALL RSADPA(LERES0,'L',1,'TYPE_DEFO',IORDR,0,IAD,CTYP)
                IF (ZK16(IAD) (1:9).EQ.'FORC_IMPO') THEN
                  CALL RSADPA(LERES0,'L',1,'NUME_DDL',IORDR,0,IAD,CTYP)
                  INUME = ZI(IAD)
                  ZR(JNOCH+INUME-1) = ZR(JNOCH+INUME-1) - 1.D0
                ELSE IF (ZK16(IAD) (1:9).EQ.'ACCE_IMPO') THEN
                  CALL JELIRA(CHDEPL(1:19)//'.VALE','LONMAX',LONC2,
     &                        K8BID)
                  CALL RSADPA(LERES0,'L',1,'COEF_X',IORDR,0,IAD,CTYP)
                  COEF(1) = ZR(IAD)
                  CALL RSADPA(LERES0,'L',1,'COEF_Y',IORDR,0,IAD,CTYP)
                  COEF(2) = ZR(IAD)
                  CALL RSADPA(LERES0,'L',1,'COEF_Z',IORDR,0,IAD,CTYP)
                  COEF(3) = ZR(IAD)
                  CALL WKVECT('&&'//NOMPRO//'.POSI_DDL','V V I',3*LONC2,
     &                        JDDL)
                  CALL PTEDDL('NUME_DDL',NUME,3,NOMCMP,LONC2,ZI(JDDL))
                  CALL WKVECT('&&'//NOMPRO//'.POSI_DDR','V V R',LONC2,
     &                        JDDR)
                  DO 170 IC = 1,3
                    IND = LONC2* (IC-1)
                    DO 160 J = 0,LONC2 - 1
                      ZR(JDDR+J) = ZR(JDDR+J) + ZI(JDDL+IND+J)*COEF(IC)
  160               CONTINUE
  170             CONTINUE
                  CALL WKVECT('&&'//NOMPRO//'.TRAV','V V R',LONC2,LTRAV)
                  IF (LMAT.EQ.0) CALL U2MESS('F','PREPOST3_81')
                  CALL MRMULT('ZERO',LMAT,ZR(JDDR),'R',ZR(LTRAV),1)
                  DO 180 J = 0,LONCH - 1
                    ZR(JNOCH+J) = ZR(JNOCH+J) - ZR(LTRAV+J)
  180             CONTINUE
                  CALL JEDETR('&&'//NOMPRO//'.POSI_DDR')
                  CALL JEDETR('&&'//NOMPRO//'.POSI_DDL')
                  CALL JEDETR('&&'//NOMPRO//'.TRAV')
                END IF

C           --- TRAITEMENT DE DYNA_TRANS ---
              ELSE IF (TYSD.EQ.'DYNA_TRANS') THEN
                CALL RSEXCH(LERES0,'ACCE',IORDR,CHACCE,IRET)
                IF (IRET.EQ.0) THEN
                  CALL JEVEUO(CHACCE(1:19)//'.VALE','L',LACCE)
                  CALL WKVECT('&&'//NOMPRO//'.TRAV','V V R',LONCH,LTRAV)
                  IF (LMAT.EQ.0) CALL U2MESS('F','PREPOST3_81')
                  CALL MRMULT('ZERO',LMAT,ZR(LACCE),'R',ZR(LTRAV),1)
                  DO 190 J = 0,LONCH - 1
                    ZR(JNOCH+J) = ZR(JNOCH+J) + ZR(LTRAV+J)
  190             CONTINUE
                  CALL JEDETR('&&'//NOMPRO//'.TRAV')
                ELSE
                  CALL U2MESS('A','CALCULEL3_1')
                END IF

C           --- TRAITEMENT DE DYNA_HARMO ---
              ELSE IF (TYSD.EQ.'DYNA_HARMO') THEN
                CALL RSEXCH(LERES0,'ACCE',IORDR,CHACCE,IRET)
                IF (IRET.EQ.0) THEN
                  CALL JEVEUO(CHACCE(1:19)//'.VALE','L',LACCE)
                  CALL WKVECT('&&'//NOMPRO//'.TRAV','V V C',LONCH,LTRAV)
                  IF (LMAT.EQ.0) CALL U2MESS('F','PREPOST3_81')
                  CALL MCMULT('ZERO',LMAT,ZC(LACCE),'C',ZC(LTRAV),1)
                  DO 200 J = 0,LONCH - 1
                    ZR(JNOCH+J) = ZR(JNOCH+J) + DBLE(ZC(LTRAV+J))
  200             CONTINUE
                  CALL JEDETR('&&'//NOMPRO//'.TRAV')
                ELSE
                  CALL U2MESS('A','CALCULEL3_1')
                END IF

C           --- TRAITEMENT DE EVOL_NOLI ---
              ELSE IF (TYSD.EQ.'EVOL_NOLI') THEN
                CALL RSEXCH(LERES0,'ACCE',IORDR,CHACCE,IRET)
                IF (IRET.EQ.0) THEN
                  OPTIO2 = 'M_GAMMA'

C               --- CALCUL DES MATRICES ELEMENTAIRES DE MASSE
                  CALL MEMAM2(OPTIO2,MODELE,NBCHAR,ZK8(ICHAR),MATER,
     &                 CARAC,COMPOR,EXITIM,TIME,CHACCE,VRENO,'V',LIGREL)

C               --- ASSEMBLAGE DES VECTEURS ELEMENTAIRES ---
                  CALL ASASVE(VRENO,NUME,'R',VARENO)
                  CALL JEVEUO(VARENO,'L',JRE)
                  CALL JEVEUO(ZK24(JRE) (1:19)//'.VALE','L',JRENO)
                  DO 210 J = 0,LONCH - 1
                    ZR(JNOCH+J) = ZR(JNOCH+J) + ZR(JRENO+J)
  210             CONTINUE
                END IF
              END IF

  220         CONTINUE
              CALL RSNOCH(LERES0,OPTION,IORDR,' ')
              CALL DETRSD('CHAMP_GD','&&'//NOMPRO//'.SIEF')
              CALL DETRSD('VECT_ELEM',VFONO(1:8))
              CALL DETRSD('VECT_ELEM',VRENO(1:8))
              CALL DETRSD('VECT_ELEM',VECHMP(1:8))
              CALL DETRSD('VECT_ELEM',VECGMP(1:8))
              CALL DETRSD('CHAMP_GD',CNCHMP(1:8)//'.ASCOVA')
              CALL DETRSD('CHAMP_GD',CNCGMP(1:8)//'.ASCOVA')
              CALL JEDETR(VACHMP(1:8))
              CALL JEDETR(VACGMP(1:8))
              CALL JEDETR(VACHMP(1:6)//'00.BIDON')
              CALL JEDETR(VACGMP(1:6)//'00.BIDON')
              CALL JEDETR(VACHMP(1:6)//'00.BIDON     .VALE')
              CALL JEDETR(VACGMP(1:6)//'00.BIDON     .VALE')
              CALL JEDETR(VACHMP(1:6)//'00.BIDON     .DESC')
              CALL JEDETR(VACGMP(1:6)//'00.BIDON     .DESC')
              CALL JEDETR(VACHMP(1:6)//'00.BIDON     .REFE')
              CALL JEDETR(VACGMP(1:6)//'00.BIDON     .REFE')
              CALL JEDETR(VACHMP(1:8)//'.ASCOVA')
              CALL JEDETR(VACGMP(1:8)//'.ASCOVA')
  229         CONTINUE
              CALL JEDEMA()
  230       CONTINUE
          ELSE
            CALL U2MESK('F','CALCULEL6_10',1,OPTION)
          END IF
  240   CONTINUE
C============= FIN DE LA BOUCLE SUR LES OPTIONS A CALCULER =============
  250 CONTINUE
C============= FIN DE LA BOUCLE SUR LE NOMBRE DE PASSAGES ==============

      IF (NBMA.NE.0) CALL JEDETR(MESMAI)
      IF (NBNO.NE.0) CALL JEDETR(MESNOE)
      CALL JEDETR(KNUM)
      CALL DETRSD('CHAMP_GD',BIDON)

  260 CONTINUE
      CALL INFBAV()
      CALL JEDEMA()

      END
