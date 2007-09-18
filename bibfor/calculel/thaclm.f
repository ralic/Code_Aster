      SUBROUTINE THACLM
     &  (NEWCAL,TYSD,KNUM,KCHA,PHENO,RESUCO,RESUC1,CONCEP,NBORDR,
     &   MODELE,MATE,CARA,NCHAR,CTYP)

      IMPLICIT NONE
C TOLE CRP_20
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 18/09/2007   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ------------------------------------------------------------------
C COMMANDE DE CALC_ELEM SPECIFIQUE A LA THERMIQUE ET A L'ACCOUSTIQUE
C ------------------------------------------------------------------
C IN  NEWCAL : TRUE POUR UN NOUVEAU CONCEPT RESULTAT, FALSE SINON
C IN  TYSD   : TYPE DU CONCEPT ATTACHE A RESUCO
C IN  KNUM   : NOM D'OBJET DES NUMERO D'ORDRE
C IN  KCHA   : NOM JEVEUX OU SONT STOCKEES LES CHARGES
C IN  PHENO  : PHENOMENE (MECA,THER,ACOU)
C IN  RESUCO : NOM DE CONCEPT RESULTAT
C IN  RESUC1 : NOM DE CONCEPT DE LA COMMANDE CALC_ELEM
C IN  CONCEP : TYPE DU CONCEPT ATTACHE A RESUC1
C IN  NBORDR : NOMBRE DE NUMERO D'ORDRE
C IN  MODELE : NOM DU MODELE
C IN  MATE   : NOM DU CHAMP MATERIAU
C IN  CARA   : NOM DU CHAMP DES CEARACTERISTIQUES ELEMENTAIRES
C IN  NCHAR  : NOMBRE DE CHARGES
C IN  CTYP   : TYPE DE CHARGE
C ----------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNOM
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER NBORDR,NCHAR
      INTEGER VALI
      CHARACTER*4 CTYP
      CHARACTER*8  RESUCO,RESUC1,MODELE,CARA
      CHARACTER*16 TYSD,PHENO,CONCEP
      CHARACTER*19 KNUM,KCHA
      CHARACTER*24 MATE
      CHARACTER*24 VALK(2)
      LOGICAL      NEWCAL
C
C     --- VARIABLES LOCALES ---
      INTEGER IAUX,JORDR,IORDR,JCHA,IRET1,IRET,BUFIN1,IAD,IOROLD
      INTEGER IFM,NIV,LINST,NIVEAU,JNMO,IAINST,IRET2,TYPESE,NRPASS
      INTEGER CODSEN,NUORD,NH,NBAC,NBPA,ADCRRS,JPA,NBPARA
      INTEGER IADIN,IADOU,NBPASS,NBPASE,IBID,JAUX,ADRECG,IOPT,NBOPT
      INTEGER JOPT,J,IFREQ,III,L1,L2,L3,L4,L5,IERD,LREFE
      INTEGER LMAT,IE,LVALE,IOCC,NBCHRE,N1,L6,IPUIS
      REAL*8 VALTHE,INSOLD,INST,TIME, COEF,PHASE
      CHARACTER*1 BASE
      CHARACTER*4 BUFCH,K4B,TYPE
      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='THACLM')
      CHARACTER*8 MA,K8B,NOMPA1,NOPASE,LERES0,BASENO
      CHARACTER*8  NOMA,SAVCAR(2)
      CHARACTER*13 INPSCO
      CHARACTER*16 OPT1,OPTION,NOMCMD,K16B
      CHARACTER*19 CARTEF,NOMGDF,CARTEH,NOMGDH,CARTET,NOMGDT,CARTES
      CHARACTER*19 NOMGDS,DCEL,LERES1,INFCHA,MASSE
      CHARACTER*19 COMPOR,CHDYNR,REFE
      CHARACTER*24 CHCARA(24),CHSIG,CHELEM,K24B,CHTEMM,CHTEMP
      CHARACTER*24 CHFLUM,CHSOUR,CHFLUP,CHERRG,CHERRN,CHTREF,CHEPS
      CHARACTER*24 CHTIME,CHMETA,STYPSE,MODEL2,MATE2,CARA2
      CHARACTER*24 CHGEOM,CHHARM,CHNUMC,NOMPAR,NOCRRS,NORECG
      CHARACTER*24 LESOPT,CHPRES,CHFREQ,BLAN24
      CHARACTER*24 CHAMGD,CHTESE,CHDESE,SOP,LIGREL,LIGRMO
      LOGICAL EXICAR,EVOL,LBID,EXITIM,EXIPOU,EXIPLA
      REAL*8 ZERO,UN
      PARAMETER (ZERO=0.D0,UN=1.D0)
      COMPLEX*16 CZERO,CCOEF
      PARAMETER (CZERO= (0.D0,0.D0))

      CALL JEMARQ()
      CALL GETRES(K8B,K16B,NOMCMD)
      CALL JERECU('V')

C          '123456789012345678901234'
      BLAN24 ='                        '
      K8B ='        '
      K4B ='    '
      NH = 0
      CHAMGD = BLAN24
      CHGEOM = BLAN24
      CHTEMP = BLAN24
      CHTREF = BLAN24
      CHTIME = BLAN24
      CHNUMC = BLAN24
      CHHARM = BLAN24
      CHSIG = BLAN24
      CHFREQ = BLAN24
      CHMETA = BLAN24
      CHDYNR = ' '
      CHELEM = BLAN24
      SOP = BLAN24
      K24B = BLAN24
      BASE = 'G'
      COEF = UN
      SAVCAR(1) = '????????'
      SAVCAR(2) = '????????'
      LESOPT = '&&'//NOMPRO//'.LES_OPTION     '


      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

      CALL MODOPT(RESUCO,LESOPT,NBOPT)
      CALL JEVEUO(LESOPT,'L',JOPT)

      CALL JEVEUO(KCHA,'L',JCHA)
      CALL JEVEUO(KNUM,'L',JORDR)

      IF (NEWCAL) THEN
        CALL RSCRSD(RESUC1,TYSD,NBORDR)
        CALL TITRE
      END IF

      CALL DISMOI('F','NOM_LIGREL',MODELE,'MODELE',IBID,LIGRMO,IERD)

      EXITIM = .FALSE.
      CALL JEEXIN(RESUCO//'           .INST',IRET)
      IF (IRET.NE.0) EXITIM = .TRUE.

      CALL EXLIMA(' ','V',MODELE,RESUC1,LIGREL)

      EXIPOU = .FALSE.
      CALL DISMOI('F','EXI_POUX',LIGREL,'LIGREL',IBID,K8B,IERD)
      IF (K8B(1:3).EQ.'OUI') EXIPOU = .TRUE.
C=======================================================================
C                   SPECIAL POUTRE A LA POUX (1)
C=======================================================================
      IF (EXIPOU) THEN
        IF (CONCEP.EQ.'MODE_ACOU') THEN
          REFE = RESUCO
          CALL JEVEUO(REFE//'.REFD','L',LREFE)
          MASSE = ZK24(LREFE+1) (1:19)
          CALL MTDSCR(MASSE)
          CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT)
          CALL DISMOI('I','SUR_OPTION',MASSE,'MATR_ASSE',IBID,SOP,IE)
          CHDYNR = '&&'//NOMPRO//'.M.GAMMA'
          CALL VTCREM(CHDYNR,MASSE,'V','R')
          CALL JEVEUO(CHDYNR//'.VALE','E',LVALE)
        END IF
C --- VERIFIE L'UNICITE DE LA CHARGE REPARTIE
        IOCC = 0
        CALL COCHRE(ZK8(JCHA),NCHAR,NBCHRE,IOCC)
        IF (NBCHRE.GT.1) THEN
          CALL U2MESS('A','CALCULEL2_92')
          GO TO 530
        END IF
        DO 10 III = 1,NCHAR
          CALL GETVID('EXCIT','FONC_MULT',III,1,1,K8B,L1)
          CALL GETVID('EXCIT','FONC_MULT_C',III,1,1,K8B,L2)
          CALL GETVR8('EXCIT','COEF_MULT',III,1,1,COEF,L3)
          CALL GETVC8('EXCIT','COEF_MULT_C',III,1,1,CCOEF,L4)
          CALL GETVR8('EXCIT','PHAS_DEG',III,1,1,PHASE,L5)
          CALL GETVIS('EXCIT','PUIS_PULS',III,1,1,IPUIS,L6)
          IF (L1.NE.0 .OR. L2.NE.0 .OR. L3.NE.0 .OR. L4.NE.0 .OR.
     &        L5.NE.0 .OR. L6.NE.0) THEN
            IF (NBCHRE.EQ.0) THEN
              CALL U2MESS('A','CALCULEL2_93')
            END IF
          END IF
   10   CONTINUE
      END IF
C=======================================================================
C     ON VERIFIE QUE CARA_ELEM, NIVE_COUCHE ET NUME_COUCHE ONT ETE
C     RENSEIGNES POUR LES COQUES
C=======================================================================
      EXIPLA = .FALSE.
      CALL DISMOI('F','EXI_COQ1D',MODELE,'MODELE',IBID,K8B,IERD)
      IF (K8B(1:3).EQ.'OUI') EXIPLA = .TRUE.
      CALL DISMOI('F','EXI_COQ3D',MODELE,'MODELE',IBID,K8B,IERD)
      IF (K8B(1:3).EQ.'OUI') EXIPLA = .TRUE.
      CALL DISMOI('F','EXI_PLAQUE',MODELE,'MODELE',IBID,K8B,IERD)
      IF (K8B(1:3).EQ.'OUI') EXIPLA = .TRUE.
      IF (EXIPLA) THEN
        CALL GETVID(' ','CARA_ELEM',1,1,1,K8B,N1)
        IF (N1.EQ.0.AND.CARA.EQ.' ') THEN
          CALL U2MESS('A','CALCULEL2_94')
          GO TO 530
        END IF
      END IF


C=====================================================
C        PHENOMENE THERMIQUE
C====================================================

      IF (PHENO(1:4).EQ.'THER')THEN


C -- SENSIBILITE : NOMBRE DE PASSAGES
C            12   345678
      BASENO = '&&'//NOMPRO
      INPSCO = '&&'//NOMPRO//'_PSCO'
      NORECG = '&&'//NOMPRO//'_PARA_SENSI     '
      IAUX = 1
      CALL PSLECT(' ',IBID,BASENO,RESUC1,IAUX,NBPASE,INPSCO,IRET)
      IAUX = 1
      JAUX = 1
      CALL PSRESE(' ',IBID,IAUX,RESUC1,JAUX,NBPASS,NORECG,IRET)
      CALL JEVEUO(NORECG,'L',ADRECG)

      NOCRRS = '&&'//NOMPRO//'_RESU_CREES     '
      CALL WKVECT(NOCRRS,'V V K24',NBPASS,ADCRRS)



C============ DEBUT DE LA BOUCLE SUR LE NOMBRE DE PASSAGES =============
      DO 480,NRPASS = 1,NBPASS

C        POUR LE PASSAGE NUMERO NRPASS :
C        . NOPASE : NOM DU PARAMETRE DE SENSIBILITE EVENTUELLEMENT
C        . LERES1 : NOM DU CHAMP DE RESULTAT A COMPLETER
C                   C'EST RESUC1 POUR UN CALCUL STANDARD, UN NOM
C                   COMPOSE A PARTIR DE RESUC1 ET NOPASE POUR UN CALCUL
C                   DE SENSIBILITE
C        . LERES0 : IDEM POUR RESUCO

        NOPASE = ZK24(ADRECG+2*NRPASS-1) (1:8)
        LERES1 = ZK24(ADRECG+2*NRPASS-2) (1:19)

C DANS LE CAS D'UN CALCUL STANDARD :

        IF (NOPASE.EQ.' ') THEN

          LERES0 = RESUCO
          TYPESE = 0
          STYPSE = K24B

C DANS LE CAS D'UN CALCUL DE DERIVE :

        ELSE

C ON N'ENREGISTRE LES DONNEES RELATIVES AUX DERIVEES QU'AU 1ER PASSAGE
C EN OUTPUT --> INFCHA ET INPSCO

          IF (NRPASS.EQ.1) THEN
            MODEL2 = ' '
            MATE2 = ' '
            CARA2 = ' '
            INFCHA = '&&'//NOMPRO//'.INFCHA'
            IF (TYSD.EQ.'EVOL_THER') THEN
              CALL NTDOTH(MODEL2,MATE2,CARA2,K24B,LBID,LBID,INFCHA,
     &                    NBPASE,INPSCO,RESUCO,IBID)
            ELSE
              CALL U2MESK('A','SENSIBILITE_5',1,TYSD)
              GO TO 480
            END IF
          END IF

C DETERMINATION DU CHAMP DERIVE LERES0 ASSOCIE A (RESUCO,NOPASE)

          CALL PSGENC(RESUCO,NOPASE,LERES0,IRET)
          IF (IRET.NE.0) THEN
             VALK(1) = RESUCO
             VALK(2) = NOPASE
             CALL U2MESK('A','SENSIBILITE_3', 2 ,VALK)
            GO TO 480
          END IF

C DETERMINATION DU TYPE DE DERIVE: TYPESE ET STYPSE

          IF (TYSD.EQ.'EVOL_THER') THEN
            CALL NTTYSE(NBPASE,INPSCO,NOPASE,TYPESE,STYPSE)
          ELSE
            CALL U2MESK('A','SENSIBILITE_5',1,TYSD)
            GO TO 480
          END IF

          IF (NEWCAL) THEN
            CALL RSCRSD(LERES1,TYSD,NBORDR)
            CALL TITRE
          END IF

        END IF

C============ DEBUT DE LA BOUCLE SUR LES OPTIONS A CALCULER ============
        DO 440 IOPT = 1,NBOPT
          OPTION = ZK16(JOPT+IOPT-1)
          CODSEN=0
C
          CALL JEVEUO(KNUM,'L',JORDR)
          NUORD = ZI(JORDR)
          CALL MEDOM1(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,NUORD)
          CALL JEVEUO(KCHA,'L',JCHA)
C
          CALL MECHAM(OPTION,MODELE,NCHAR,ZK8(JCHA),CARA,NH,CHGEOM,
     &                CHCARA,CHHARM,IRET)
          IF (IRET.NE.0) GO TO 530
          NOMA = CHGEOM(1:8)
          CHNUMC = '&&'//NOMPRO//'.NUMC'
          CHFREQ = '&&'//NOMPRO//'.FREQ'
          CALL MECHN2 ( NOMA, CHNUMC, CHFREQ )

C    ------------------------------------------------------------------
C    -- OPTION "ERRE_ELEM_TEMP"
C    ------------------------------------------------------------------

          IF (OPTION.EQ.'ERRE_ELEM_TEMP') THEN

C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 123
C ---- VERIF SENSIBILITE FIN

C PAR DECRET EDA DU 22/08/01 ON SUPPRIME LE PARAMETRE NIVEAU ET ON LE
C FIXE A 2 (15 VALEURS DE PARAMETRES).
            NIVEAU = 2

C RECUPERATION NIVEAU AFFICHAGE
            CALL INFNIV(IFM,NIV)

C VERIFICATION DU PERIMETRE D'UTILISATION
            CALL GETVTX(' ','TOUT',1,1,1,BUFCH,BUFIN1)
            IF (BUFCH.NE.'OUI') CALL U2MESK('A','CALCULEL4_97',1,OPTION)

C BOUCLE SUR LES INSTANTS CHOISIS PAR LE USER
            IOROLD = 0
            INSOLD = ZERO
            CHTEMM = ' '
            CHTEMP = ' '
            CHFLUM = ' '
            CHFLUP = ' '

C PREPARATION DES CALCULS D'INDICATEUR (CONNECTIVITE INVERSE, CHARGE)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL RESTH2(MODELE,LIGRMO,ZK8(JCHA),NCHAR,IFM,NIV,MA,CARTEF,
     &                  NOMGDF,CARTEH,NOMGDH,CARTET,NOMGDT,CARTES,
     &                  NOMGDS,CHGEOM,CHSOUR,OPT1)

            IF (NIV.GE.1) THEN
              WRITE (IFM,*)
              WRITE (IFM,*)
     &          '*********************************************'
              WRITE (IFM,*) '  CALCUL DE CARTES D''ERREURS EN RESIDU'
              WRITE (IFM,*) '       POUR LE PROBLEME THERMIQUE'
              WRITE (IFM,*)
              WRITE (IFM,*) '  OPTION DE CALCUL      ',OPT1
              WRITE (IFM,*) '  MODELE                ',MODELE
              WRITE (IFM,*) '  SD EVOL_THER DONNEE   ',RESUCO
              WRITE (IFM,*) '             RESULTAT   ',RESUC1
              WRITE (IFM,*)
              WRITE (IFM,*)
     &          '* CONTRAIREMENT AUX CALCULS THERMIQUES, POUR *'
              WRITE (IFM,*)
     &          '* UN TYPE DE CHARGEMENT DONNE, ON NE RETIENT *'
              WRITE (IFM,*)
     &          '* QUE LA DERNIERE OCCURENCE DE AFFE_CHAR_THER*'
              WRITE (IFM,*) '  LISTE DES CHARGEMENTS :'
              DO 200 BUFIN1 = 1,NCHAR
                WRITE (IFM,*) '                        ',
     &            ZK8(JCHA+BUFIN1-1)
  200         CONTINUE
              WRITE (IFM,*) '  CL DE FLUX RETENUE      ',NOMGDF
              WRITE (IFM,*) '  CL D''ECHANGE RETENUE    ',NOMGDH
              WRITE (IFM,*) '  SOURCE RETENUE          ',NOMGDS
              WRITE (IFM,*) '  MATERIAU PRIS EN COMPTE ',MATE(1:8)
              WRITE (IFM,*) '  NOMBRE DE NUMERO D''ORDRE ',NBORDR
            END IF

C BOUCLE SUR LES PAS DE TEMPS
            DO 210,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM1(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
C RECUPERATION DU PARM_THETA CORRESPONDANT A IORDR
              CALL JENONU(JEXNOM(RESUCO//'           .NOVA',
     &                    'PARM_THETA'),IAD)
              IF (IAD.EQ.0) THEN
                VALTHE = 0.57D0
              CALL U2MESK('A','CALCULEL4_98',1,RESUCO)
              ELSE
                CALL RSADPA(RESUCO,'L',1,'PARM_THETA',IORDR,0,IAD,K8B)
                VALTHE = ZR(IAD)
                IF ((VALTHE.GT.1.D0) .OR. (VALTHE.LT.0.D0)) THEN
                  VALTHE = 1.D0
                CALL U2MESK('A','CALCULEL4_99',1,RESUCO)
                END IF
              END IF
              IF (NIV.GE.1) THEN
                WRITE (IFM,*) '   PARAM-THETA/IORDR ',VALTHE,IORDR
                IF (IAUX.EQ.NBORDR) THEN
                  WRITE (IFM,*) '*************************************'
     &              //'*********'
                  WRITE (IFM,*)
                END IF
              END IF

C CALCUL DU CRITERE D'EVOLUTION LEVOL (TRUE=TRANSITOIRE)
C CAS PARTICULIER DE L'INSTANT INITIAL D'UN CALCUL TRANSITOIRE
C ON ESTIME SON ERREUR COMME EN STATIONNAIRE
              IF (IAUX.EQ.1) THEN
                EVOL = .FALSE.
              ELSE
                EVOL = .TRUE.
              END IF
              IF (EVOL .AND. (IORDR-1.NE.IOROLD)) CALL U2MESS('A','CALCU
     &LEL5_1')

C RECUPERATION DU NOM DES CHAMP_GD = RESUCO('FLUX_ELNO_TEMP',I)
C ET RESUCO('TEMP',I) POUR I=IORDR. POUR IORDR-1 ILS SONT STOCKES
C DANS CHFLUM/CHTEMM DEPUIS LA DERNIERE ITERATION.
C RESUCO = NOM USER DE LA SD DESIGNEE PAR LE MOT-CLE RESULTAT
              CALL RSEXC2(1,1,RESUCO,'TEMP',IORDR,CHTEMP,OPTION,IRET)
              IF (IRET.GT.0) THEN
                VALI = IORDR
                CALL U2MESG('F', 'CALCULEL6_46',0,' ',1,VALI,0,0.D0)
              END IF
              CALL RSEXC2(1,1,RESUCO,'FLUX_ELNO_TEMP',IORDR,CHFLUP,
     &                    OPTION,IRET)
              IF (IRET.GT.0) THEN
                VALI = IORDR
                CALL U2MESG('F', 'CALCULEL6_47',0,' ',1,VALI,0,0.D0)
              END IF

C RECUPERATION DE L'INSTANT CORRESPONDANT A IORDR
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,LINST,K8B)
              INST = ZR(LINST)

C IMPRESSIONS NIVEAU 2 POUR DIAGNOSTIC...
              IF (NIV.EQ.2) THEN
                WRITE (IFM,*) NOMPRO,' **********'
                WRITE (IFM,*) 'EVOL/I/IORDR',EVOL,IAUX,IORDR
                WRITE (IFM,*) 'INST/INSOLD',INST,INSOLD
                WRITE (IFM,*) 'CHTEMM/CHTEMP',CHTEMM,' / ',CHTEMP
                WRITE (IFM,*) 'CHFLUM/CHFLUP',CHFLUM,' / ',CHFLUP
              END IF

C RECUPERATION DU NOM DU CHAMP_GD = RESUC1('ERRE_ELEM_TEMP',IORDR)
C RESUC1 = NOM USER DE LA SD CORRESPONDANT AU RESULTAT DE CALC_ELEM
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
C PREPARATION DES DONNEES/LANCEMENT DU CALCUL DES INDICATEURS
              CALL RESTHE(MODELE,LIGRMO,EVOL,CHTEMM,CHTEMP,CHFLUM,
     &                    CHFLUP,MATE,VALTHE,INSOLD,INST,CHELEM,NIVEAU,
     &                    IFM,NIV,MA,CARTEF,NOMGDF,CARTEH,NOMGDH,CARTET,
     &                    NOMGDT,CARTES,NOMGDS,CHGEOM,CHSOUR,OPT1,IAUX)
C CALCUL DE L'ESTIMATEUR GLOBAL
              CALL ZZGLO1(CHELEM,OPTION,INST,NIVEAU,IORDR,RESUCO)
C NOTATION DE LA SD RESULTAT LERES1
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')

C INIT. POUR LE NUMERO D'ORDRE SUIVANT
              IF (NBORDR.NE.1 .AND. IAUX.NE.NBORDR) THEN
                CHTEMM = CHTEMP
                CHFLUM = CHFLUP
                IOROLD = IORDR
                INSOLD = INST
              END IF
              CALL JEDEMA()
  210       CONTINUE
C DESTRUCTION DES OBJETS JEVEUX VOLATILES
            CALL JEDETR(CARTEF//'.PTMA')
            CALL JEDETR(CARTEH//'.PTMA')
            CALL JEDETR(CARTET//'.PTMA')
            CALL JEDETR(CARTEF//'.PTMS')
            CALL JEDETR(CARTEH//'.PTMS')
            CALL JEDETR(CARTET//'.PTMS')

C    ------------------------------------------------------------------
C    -- OPTION "ERRE_ELNO_ELEM"
C    ------------------------------------------------------------------

          ELSE IF (OPTION.EQ.'ERRE_ELNO_ELEM') THEN

C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 123
C ---- VERIF SENSIBILITE FIN
            DO 220,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
C RECUPERATION DU NOM DU CHAMP_GD = RESUCO('ERRE_ELEM_TEMP',IORDR)
              CALL RSEXC2(1,1,RESUCO,'ERRE_ELEM_TEMP',IORDR,CHERRG,
     &                    OPTION,IRET1)
              IF (IRET1.GT.0) GO TO 222
C RECUPERATION DU NOM DU CHAMP_GD = RESUC1('ERRE_ELNO_ELEM',IORDR)
C RESUC1 = NOM USER DE LA SD CORRESPONDANT AU RESULTAT DE CALC_ELEM
              CALL RSEXC1(LERES1,OPTION,IORDR,CHERRN)
              CALL RETHGN(LIGRMO,CHERRG,CHERRN)
C NOTATION DE LA SD RESULTAT LERES1
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  222         CONTINUE
              CALL JEDEMA()
  220       CONTINUE

C    ------------------------------------------------------------------
C    -- OPTIONS "FLUX_ELNO_TEMP","FLUX_ELGA_TEMP","SOUR_ELGA_ELEC"
C    ------------------------------------------------------------------

          ELSE IF (OPTION.EQ.'FLUX_ELNO_TEMP' .OR.
     &             OPTION.EQ.'FLUX_ELGA_TEMP' .OR.
     &             OPTION.EQ.'SOUR_ELGA_ELEC')THEN

C ---- VERIF SENSIBILITE
            IF (TYPESE.EQ.-1) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 123
C ---- VERIF SENSIBILITE FIN

            IF (.NOT.EXITIM) THEN
              CALL U2MESS('A','CALCULEL5_2')
              GO TO 440
            END IF
C RECUPERATION NIVEAU AFFICHAGE
            IF (NRPASS.EQ.1) THEN
              CALL INFNIV(IFM,NIV)
              IF (NIV.EQ.2) THEN
                WRITE (IFM,*)
                WRITE (IFM,*)
     &            '*******************************************'
                WRITE (IFM,*) '         CALCUL DE FLUX THERMIQUES'
                WRITE (IFM,*)
                WRITE (IFM,*) '  OPTION DE CALCUL      ',OPTION
                WRITE (IFM,*) '  MODELE                ',MODELE
                WRITE (IFM,*) '  SD EVOL_THER DONNEE   ',RESUCO
                WRITE (IFM,*) '             RESULTAT   ',RESUC1
                WRITE (IFM,*) '  MATERIAU PRIS EN COMPTE ',MATE(1:8)
                WRITE (IFM,*) '  NOMBRE DE NUMERO D''ORDRE ',NBORDR
                WRITE (IFM,*) '  NOMBRE DE PARAMETRES SENSIBLES ',NBPASE
                WRITE (IFM,*)
     &            '*******************************************'
                WRITE (IFM,*)
              END IF
            END IF
C CALCUL
            CHAMGD = ' '
            CHTREF = ' '
            DO 280,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM1(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
C RECUPERATION DU NOM DU CHAMP_GD = LERES1(OPTION,IORDR)
C LERES1 = NOM USER DE LA SD CORRESPONDANT AU RESULTAT DE CALC_ELEM
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)

              CHTEMP = ' '
              CHTESE = ' '
              IF (TYPESE.EQ.0) THEN
C CALCUL STD : RECUP CHAMP_GD
C T= RESUCO('TEMP',IAUX) --> CHTEMP
                CALL RSEXC2(1,1,RESUCO,'TEMP',IORDR,CHTEMP,OPTION,IRET)
                IF (IRET.GT.0) GO TO 282
              ELSE IF (TYPESE.EQ.-1) THEN
C DERIVEE LAGRANGIENNE
                CALL U2MESS('A','SENSIBILITE_7')
                GO TO 282
              ELSE IF (TYPESE.EQ.1) THEN
C CALCUL INSENSIBLE: CREATION D'UN CHAM_ELEM NUL
                CALL U2MESK('A', 'SENSIBILITE_72', 1, NOPASE)
                NOMPA1 = 'PFLUX_R'
                DCEL = ' '
                CALL ALCHML(LIGREL,OPTION,NOMPA1,BASE,CHELEM,IRET,DCEL)
                IF (IRET.NE.0) THEN
                  CALL U2MESS('A','CALCULEL5_4')
                  GO TO 282
                END IF
                GO TO 270
              ELSE IF (TYPESE.EQ.3) THEN
C CALCUL SENSIBILITE MATERIAU: RECUP CHAMP_GD
C T= RESUCO('TEMP',IAUX) --> CHTESE
C DT/DS= LERES0('TEMP',IAUX) --> CHTEMP
                CALL RSEXC2(1,1,RESUCO,'TEMP',IORDR,CHTESE,OPTION,IRET)
                IF (IRET.GT.0) GO TO 282
                CALL RSEXC2(1,1,LERES0,'TEMP',IORDR,CHTEMP,OPTION,IRET)
                IF (IRET.GT.0) GO TO 282
              ELSE
C CALCUL SENSIBILITE PAR RAPPORT A UNE CL: RECUP CHAMP_GD
C DT/DS= LERES0('TEMP',IAUX) --> CHTEMP
                CALL RSEXC2(1,1,LERES0,'TEMP',IORDR,CHTEMP,OPTION,IRET)
                IF (IRET.GT.0) GO TO 282
              END IF

C TRAITEMENT PARTICULIER EN MODELISATION 'FOURIER_ELAS','FOURIER_THER'
              IF (TYSD(1:8).EQ.'FOURIER_') THEN
                CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
                CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
              END IF
C RECUPERATION DE L'INSTANT CORRESPONDANT A IORDR
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME = ZR(IAINST)

C IMPRESSIONS NIVEAU 2 POUR DIAGNOSTIC...
              IF (NIV.EQ.2) THEN
                WRITE (IFM,*) NOMPRO,' **********'
                WRITE (IFM,*) 'INST/IAUX/IORDR',TIME,IAUX,IORDR
                WRITE (IFM,*) 'NRPASS/TYPESE/NOPASE',NRPASS,TYPESE,'  ',
     &            NOPASE(1:8)
                WRITE (IFM,*) 'CHTEMP/CHTESE',CHTEMP,' / ',CHTESE
              END IF

C CALCUL DE L'OPTION PROPREMENT DIT
              CALL MECHTI(NOMA,TIME,CHTIME)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    CHTEMP,CHTREF,CHTIME,CHNUMC,CHHARM,K24B,
     &                    K24B,CHFREQ,K24B,K24B,'   ',' ',ZERO,
     &                    CZERO,CHDYNR,SOP,CHELEM,LIGREL,BASE,K24B,K24B,
     &                    K24B,COMPOR,CHTESE,CHDESE,NOPASE,TYPESE,
     &                    K24B,IRET)
              IF (IRET.GT.0) GO TO 282

C NOTATION DE LA SD RESULTAT LERES1
  270         CONTINUE
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  282         CONTINUE
              CALL JEDEMA()
  280       CONTINUE

C    ------------------------------------------------------------------
C    -- OPTION "DETE_ELNO_DLTE"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'DETE_ELNO_DLTE') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.-1) THEN
               CODSEN = 2
            ENDIF
            IF(CODSEN.NE.0) GO TO 123
C ---- VERIF SENSIBILITE FIN
            DO 770,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM1(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              K4B = 'TEMP'
              CALL RSEXC2(1,1,RESUCO,K4B,IORDR,CHAMGD,OPTION,IRET)
              IF (IRET.GT.0) GO TO 771
              CALL RSEXC2(1,1,LERES0,K4B,IORDR,K24B,OPTION,IRET)
              IF (IRET.GT.0) GO TO 771
              CHTESE = K24B
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                    K24B,K24B,K24B,K24B,K24B,K24B,K24B,K24B,
     &                    K24B,ZK8(JCHA),' ',ZERO,CZERO,K24B,K24B,
     &                    CHELEM,LIGREL,BASE,K24B,K24B,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,K24B,IRET)
              IF (IRET.GT.0) GO TO 771
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  771         CONTINUE
              CALL JEDEMA()
  770       CONTINUE

C    ----------------------------------------------------
C    -- OPTIONS " DURT_ELGA_META","DURT_ELNO_META"
C    ----------------------------------------------------

          ELSE IF (OPTION.EQ.'DURT_ELGA_META' .OR.
     &             OPTION.EQ.'DURT_ELNO_META') THEN

C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 123
C ---- VERIF SENSIBILITE FIN

            CHAMGD = ' '
            CHTREF = ' '
            DO 350,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM1(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'TEMP',IORDR,CHTEMP,OPTION,IRET)
              IF (IRET.GT.0) GO TO 352
              CALL RSEXC2(1,1,RESUCO,'META_ELNO_TEMP',IORDR,CHMETA,
     &                    OPTION,IRET2)
              IF (IRET2.GT.0) GO TO 352
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME = ZR(IAINST)
              CALL MECHTI(NOMA,TIME,CHTIME)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    CHTEMP,CHTREF,CHTIME,CHNUMC,CHHARM,CHSIG,
     &                    CHEPS,CHFREQ,K24B,CHMETA,'   ',' ',ZERO,
     &                    CZERO,CHDYNR,SOP,CHELEM,LIGREL,BASE,K24B,K24B,
     &                    K24B,COMPOR,CHTESE,CHDESE,NOPASE,TYPESE,
     &                    K24B,IRET)
              IF (IRET.GT.0) GO TO 352
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  352         CONTINUE
              CALL JEDEMA()
  350       CONTINUE
C    ------------------------------------------------------------------
         ELSE
             CALL U2MESK('A','CALCULEL3_22',1,OPTION)
         ENDIF

C     ------------------------------------------------------------------
C     -- ERREUR SENSIBILITE
C     ------------------------------------------------------------------
  123     CONTINUE
          IF ( CODSEN.NE.0 ) THEN
            CALL U2MESK('A','CALCULEL3_23',1,OPTION)
            IF ( NOPASE.NE.' ' ) THEN
             CALL U2MESK('A','SENSIBILITE_71',1,NOPASE)
            ENDIF
            IF ( CODSEN.EQ.1 ) THEN
               CALL U2MESS('A','CALCULEL3_25')
            ELSEIF ( CODSEN.EQ.2 ) THEN
               CALL U2MESS('A','SENSIBILITE_6')
            ENDIF
          ENDIF
  440   CONTINUE
C       ====== FIN DE LA BOUCLE SUR LES OPTIONS A CALCULER =======
        IF (NEWCAL) THEN
          DO 470,IAUX = 1,NRPASS - 1
            IF (ZK24(ADCRRS+IAUX-1)(1:19).EQ.LERES1) THEN
              GO TO 480
            END IF
  470     CONTINUE
          NOMPAR = '&&'//NOMPRO//'.NOMS_PARA '
          CALL RSNOPA(RESUCO,2,NOMPAR,NBAC,NBPA)
          NBPARA = NBAC + NBPA
          CALL JEVEUO(NOMPAR,'L',JPA)
          DO 510,IAUX = 1,NBORDR
            IORDR = ZI(JORDR+IAUX-1)
            DO 500 J = 1,NBPARA
              CALL RSADPA(RESUCO,'L',1,ZK16(JPA+J-1),IORDR,1,IADIN,TYPE)
              CALL RSADPA(LERES1,'E',1,ZK16(JPA+J-1),IORDR,1,IADOU,TYPE)
              IF (TYPE(1:1).EQ.'I') THEN
                ZI(IADOU) = ZI(IADIN)
              ELSE IF (TYPE(1:1).EQ.'R') THEN
                ZR(IADOU) = ZR(IADIN)
              ELSE IF (TYPE(1:1).EQ.'C') THEN
                ZC(IADOU) = ZC(IADIN)
              ELSE IF (TYPE(1:3).EQ.'K80') THEN
                ZK80(IADOU) = ZK80(IADIN)
              ELSE IF (TYPE(1:3).EQ.'K32') THEN
                ZK32(IADOU) = ZK32(IADIN)
              ELSE IF (TYPE(1:3).EQ.'K24') THEN
                ZK24(IADOU) = ZK24(IADIN)
              ELSE IF (TYPE(1:3).EQ.'K16') THEN
                ZK16(IADOU) = ZK16(IADIN)
              ELSE IF (TYPE(1:2).EQ.'K8') THEN
                ZK8(IADOU) = ZK8(IADIN)
              END IF
  500       CONTINUE
  510     CONTINUE
          ZK24(ADCRRS+NRPASS-1)(1:19) = LERES1
        END IF
  480 CONTINUE


C=====================================================
C        PHENOMENE ACOUSTIQUE
C====================================================

      ELSE IF (PHENO(1:4).EQ.'ACOU')THEN

        LERES1=RESUCO

C      ======== DEBUT DE LA BOUCLE SUR LES OPTIONS A CALCULER ======
        DO 987 IOPT = 1,NBOPT

          OPTION = ZK16(JOPT+IOPT-1)
C
          CALL JEVEUO(KNUM,'L',JORDR)
          NUORD = ZI(JORDR)
          CALL MEDOM1(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,NUORD)
          CALL JEVEUO(KCHA,'L',JCHA)
C
          CALL MECHAM(OPTION,MODELE,NCHAR,ZK8(JCHA),CARA,NH,CHGEOM,
     &                CHCARA,CHHARM,IRET)
          IF (IRET.NE.0) GO TO 530

C    ------------------------------------------------------------------
C    -- OPTIONS "PRES_ELNO_DBEL","PRES_ELNO_REEL","PRES_ELNO_IMAG"
C               "INTE_ELNO_ACTI","INTE_ELNO_REAC"
C    ------------------------------------------------------------------
             IF (  OPTION.EQ.'PRES_ELNO_DBEL' .OR.
     &             OPTION.EQ.'PRES_ELNO_REEL' .OR.
     &             OPTION.EQ.'PRES_ELNO_IMAG' .OR.
     &             OPTION.EQ.'INTE_ELNO_ACTI' .OR.
     &             OPTION.EQ.'INTE_ELNO_REAC') THEN
            DO 772,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM1(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'PRES',IORDR,CHPRES,OPTION,IRET)
              TYPE = 'PRES'
              IF (IRET.GT.0) GO TO 773
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              IF (OPTION.EQ.'INTE_ELNO_ACTI' .OR.
     &            OPTION.EQ.'INTE_ELNO_REAC') THEN
                CALL RSADPA(RESUCO,'L',1,'FREQ',IAUX,0,LINST,K8B)
                CALL WKVECT('FREQ.VALE','V V R',1,IFREQ)
                ZR(IFREQ) = ZR(LINST)
                CALL MECOAC(OPTION,MODELE,LIGREL,MATE,CHPRES,CHELEM)
                CALL JEDETR('FREQ.VALE')
              ELSE
               CALL MECOAC(OPTION,MODELE,LIGREL,MATE,CHPRES,CHELEM)
              END IF
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  773         CONTINUE
              CALL JEDEMA()
  772       CONTINUE

           ELSE
             CALL U2MESK('A','CALCULEL3_22',1,OPTION)
           ENDIF
C

  987 CONTINUE

       ENDIF

 530   CONTINUE

       CALL JEDEMA()
       END
