      SUBROUTINE MECALM
     &  (NEWCAL,TYSD,KNUM,KCHA,RESUCO,RESUC1,CONCEP,NBORDR,
     &   MODELE,MATE,CARA,NCHAR,CTYP)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/03/2008   AUTEUR MEUNIER S.MEUNIER 
C TOLE CRP_20
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
C ----------------------------------------------------------------------
C COMMANDE DE CALC_ELEM SPECIFIQUE A LA MECANIQUE
C ----------------------------------------------------------------------
C IN  NEWCAL : TRUE POUR UN NOUVEAU CONCEPT RESULTAT, FALSE SINON
C IN  TYSD   : TYPE DU CONCEPT ATTACHE A RESUCO
C IN  KNUM   : NOM D'OBJET DES NUMEROS D'ORDRE
C IN  KCHA   : NOM JEVEUX OU SONT STOCKEES LES CHARGES
C IN  RESUCO : NOM DE CONCEPT RESULTAT
C IN  RESUC1 : NOM DE CONCEPT DE LA COMMANDE CALC_ELEM
C IN  CONCEP : TYPE DU CONCEPT ATTACHE A RESUC1
C IN  NBORDR : NOMBRE DE NUMEROS D'ORDRE
C IN  MODELE : NOM DU MODELE
C IN  MATE   : NOM DU CHAMP MATERIAU
C IN  CARA   : NOM DU CHAMP DES CARACTERISTIQUES ELEMENTAIRES
C IN  NCHAR  : NOMBRE DE CHARGES
C IN  CTYP   : TYPE DE CHARGE
C ----------------------------------------------------------------------

      IMPLICIT NONE
C
C     --- ARGUMENTS ---

      INTEGER      NBORDR, NCHAR
      CHARACTER*4  CTYP
      CHARACTER*8  RESUCO, RESUC1, MODELE, CARA
      CHARACTER*16 TYSD, CONCEP
      CHARACTER*19 KNUM, KCHA
      CHARACTER*24 MATE
      LOGICAL      NEWCAL
C
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C     --- VARIABLES LOCALES ---

      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='MECALM')

      INTEGER IFM,NIV
      INTEGER LINST,NUORD
      INTEGER LREFE,LVALE,LDEPL,LFREQ,LACCE
      INTEGER IORDR,IORDR1,IORDR2,JORDR,IORDRM
      INTEGER IRET,IRET1,IRET2,IRET3,IRET4,IERD
      INTEGER NH,NOR,NBOPT,NEQ,NBCHRE,IER
      INTEGER IADOU,IADIN,IPUIS
      INTEGER IAUX,II,III,IB,J,JAUX,K,IBID,IE
      INTEGER IOCC,IOPT,IAINST
      INTEGER L1,L2,L3,L4,L5,L6
      INTEGER N1
      INTEGER JPA,JOPT,JCHA,JNMO
      INTEGER NBAC,NBPA,NBPARA
      INTEGER NBPASE,NRPASS,NBPASS,TYPESE
      INTEGER ADRECG,ADCRRS
      INTEGER NBVAL
      INTEGER CODSEN,INUME
      INTEGER IINST1,IINST2
      INTEGER JDIM,JCOOR,JTYPE
      INTEGER NNOEM,NELEM,NDIM,NNCP,NPASS

      CHARACTER*1 BASE,TYPCOE
      CHARACTER*4 TYPE,K4BID
      CHARACTER*8 K8B,NOMA,CHAREP
      CHARACTER*8 CARELE,Z1Z2(2),KIORD,KIORDM
      CHARACTER*8 LERES0,NOPASE
      CHARACTER*8 NOMCMP,SAVCAR(2)
      CHARACTER*13 INPSCO
      CHARACTER*14 NUME
      CHARACTER*16 NOMCMD,OPTION,OPTIO2,NOMCHA,K16B
      CHARACTER*19 LERES1
      CHARACTER*19 INFCHA
      CHARACTER*19 CHDYNR,CHACCE,MASSE,REFE,COMPOR
      CHARACTER*19 CHERRS,CHENES,CHSINS,CHSINN
      CHARACTER*24 CHAMGD,CHSIG,CHSIGN,CHEPSP
      CHARACTER*24 CHEPS,CHDEPL,CHACSE
      CHARACTER*24 CHGEOM,CHCARA(15),CHTEMP,CHTIME,CHMETA
      CHARACTER*24 CHNUMC,CHHARM,CHFREQ,CHMASS,CHELEM,SOP
      CHARACTER*24 LIGREL,CHEPSA,K24B
      CHARACTER*24 CHENEG,CHSING,CHERR1,CHERR2,CHERR3
      CHARACTER*24 CHSIG1,CHSIG2,CHVAR1,CHVAR2,NORME,NOMPAR
      CHARACTER*24 MODEL2,MATE2,CARA2,LESOPT
      CHARACTER*24 CHTETA,CHTESE,CHSIGM,DLAGSI,CHDESE,CHSIC
      CHARACTER*24 CHVARI,CHDEPM
      CHARACTER*24 NORECG,NOCRRS,NOMS(2)
      CHARACTER*24 STYPSE
      CHARACTER*24 LIGRMO
      CHARACTER*24 BLAN24,CHBID,CHSEQ,CHEEQ,CHCMP
      CHARACTER*24 CHTIM1,CHELE1
      CHARACTER*24 CHTIM2,CHELE2
      CHARACTER*24 CHEND2,CHSIGF
      CHARACTER*19 CHVARC,CHVREF,CHVAC2

      REAL*8 COEF,VALRES,VALIM,INST,TIME,R8B
      REAL*8 ALPHA,PREC,PHASE,FREQ,OMEGA
      REAL*8 R8DEPI,R8DGRD,RUNDF,R8VIDE
      REAL*8 RBID
      REAL*8 TIME1,TIME2
      REAL*8 TBGRCA(3)

      COMPLEX*16 CALPHA,CCOEF,CBID

      LOGICAL EXITIM,EXIPOU,EXIPLA,LBID,EXICAR
      REAL*8 ZERO,UN
      PARAMETER (ZERO=0.D0,UN=1.D0)

      COMPLEX*16 CZERO
      PARAMETER (CZERO= (0.D0,0.D0))

      CHARACTER*24 VALKM(2)


      CALL JEMARQ()
      CALL GETRES(K8B,K16B,NOMCMD)
      CALL JERECU('V')
C               123456789012345678901234
      BLAN24 = '                        '
C               12   345678   90123
      INPSCO = '&&'//NOMPRO//'_PSCO'
C               12   345678   9012345678901234
      NOCRRS = '&&'//NOMPRO//'_RESU_CREES     '
      NORECG = '&&'//NOMPRO//'_PARA_SENSI     '
      LESOPT = '&&'//NOMPRO//'.LES_OPTION     '
      NH = 0
      CHAMGD = BLAN24
      CHGEOM = BLAN24
      CHTEMP = BLAN24
      CHTIME = BLAN24
      CHNUMC = BLAN24
      CHHARM = BLAN24
      CHSIG  = BLAN24
      CHSIC  = BLAN24
      CHEPS  = BLAN24
      CHFREQ = BLAN24
      CHMASS = BLAN24
      CHMETA = BLAN24
      CHAREP = ' '
      CHDYNR = ' '
      CHDEPL = BLAN24
      CHELEM = BLAN24
      SOP    = BLAN24
      K24B   = BLAN24
      CHVARC = '&&'//NOMPRO//'.CHVARC'
      CHVAC2 = '&&'//NOMPRO//'.CHVAC2'
      CHVREF = '&&'//NOMPRO//'.CHVREF'
      CHVARI = BLAN24
      BASE = 'G'
      COEF = UN
      SAVCAR(1) = '????????'
      SAVCAR(2) = '????????'
      RUNDF = R8VIDE()

C     COMPTEUR DE PASSAGES DANS LA COMMANDE (POUR MEDOM2.F)
      NPASS = 0

      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
      CARELE = ' '
      CALL GETVID(' ','CARA_ELEM',1,1,1,CARELE,N1)

      CALL MODOPT(RESUCO,LESOPT,NBOPT)
      CALL JEVEUO(LESOPT,'L',JOPT)

      CALL GETVTX(' ','NORME',1,1,1,NORME,NOR)
      CALL JEVEUO(KNUM,'L',JORDR)
      NUORD = ZI(JORDR)
      CALL JEVEUO(KCHA,'L',JCHA)
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
        IF (CONCEP.EQ.'MODE_MECA' .OR. CONCEP.EQ.'DYNA_TRANS' .OR.
     &      CONCEP.EQ.'MODE_ACOU' .OR. CONCEP.EQ.'DYNA_HARMO') THEN
          REFE = RESUCO
          SOP='MASS_MECA'
          INUME=1
          CALL RSEXCH(RESUCO,'DEPL',1,CHDEPL,IRET)
          CALL JELIRA(CHDEPL(1:19)//'.VALE','LONMAX',NEQ,K8B)
          CALL JEVEUO(REFE//'.REFD','L',LREFE)
          MASSE = ZK24(LREFE+1)(1:19)
          IF(MASSE(1:8).NE.'        ') THEN
            CALL DISMOI('I','SUR_OPTION',MASSE,'MATR_ASSE',IBID,SOP,IE)
            IF(SOP(1:14).EQ.'MASS_MECA_DIAG')INUME=0
          ENDIF
          CHDYNR = '&&'//NOMPRO//'.M.GAMMA'
          IF (CONCEP.EQ.'MODE_MECA' .OR. CONCEP.EQ.'DYNA_TRANS' .OR.
     &        CONCEP.EQ.'MODE_ACOU') THEN
            CALL COPICH('V',CHDEPL(1:19),CHDYNR)
          ELSE
            CALL COPICH('V',CHDEPL(1:19),CHDYNR)
          END IF
          CALL JELIRA(CHDYNR//'.VALE','LONMAX',NEQ,K8B)
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
C=======================================================================
C -- SENSIBILITE : NOMBRE DE PASSAGES
C            12   345678
      K8B = '&&'//NOMPRO
      IAUX = 1
      CALL PSLECT(' ',IBID,K8B,RESUC1,IAUX,NBPASE,INPSCO,IRET)
      IAUX = 1
      JAUX = 1
      CALL PSRESE(' ',IBID,IAUX,RESUC1,JAUX,NBPASS,NORECG,IRET)
      CALL JEVEUO(NORECG,'L',ADRECG)
      CALL WKVECT(NOCRRS,'V V K24',NBPASS,ADCRRS)
C=======================================================================
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,NOMA,IERD)
      CHNUMC = '&&'//NOMPRO//'.NUMC'
      CHFREQ = '&&'//NOMPRO//'.FREQ'
      CALL MECHN2 ( NOMA, CHNUMC, CHFREQ )
C=======================================================================
C
C -- GRANDEURS CARACTERISTIQUES DE L'ETUDE
C
      CALL CETULE ( MODELE, TBGRCA, IRET )
C=======================================================================

C============ DEBUT DE LA BOUCLE SUR LE NOMBRE DE PASSAGES =============
      DO 490,NRPASS = 1,NBPASS

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
          STYPSE = BLAN24

C DANS LE CAS D'UN CALCUL DE DERIVE :

        ELSE

C ON N'ENREGISTRE LES DONNEES RELATIVES AUX DERIVEES QU'AU 1ER PASSAGE
C EN OUTPUT --> INFCHA ET INPSCO

          IF (NRPASS.EQ.1) THEN
            MODEL2 = ' '
            MATE2 = ' '
            CARA2 = ' '
            INFCHA = '&&'//NOMPRO//'.INFCHA'
            IF (TYSD.EQ.'EVOL_ELAS') THEN
              CALL NMDOME(MODEL2,MATE2,CARA2,INFCHA,NBPASE,INPSCO,
     &                    RESUCO,1)
            ELSE IF (TYSD.EQ.'DYNA_TRANS') THEN
              CALL NMDOME(MODEL2,MATE2,CARA2,INFCHA,NBPASE,INPSCO,
     &                    RESUCO,1)
            ELSE IF (TYSD.EQ.'EVOL_NOLI') THEN
              CALL NMDOME(MODEL2,MATE2,CARA2,INFCHA,NBPASE,INPSCO,
     &                    RESUCO,1)
            ELSE IF (TYSD.EQ.'DYNA_HARMO') THEN
              CALL NMDOME(MODEL2,MATE2,CARA2,INFCHA,NBPASE,INPSCO,
     &                    RESUCO,1)
            ELSE
              CALL U2MESK('A','SENSIBILITE_5',1,TYSD)
              GO TO 490
            END IF
          END IF

C DETERMINATION DU CHAMP DERIVE LERES0 ASSOCIE A (RESUCO,NOPASE)

          CALL PSGENC(RESUCO,NOPASE,LERES0,IRET)
          IF (IRET.NE.0) THEN
            VALKM(1)=RESUCO
            VALKM(2)=NOPASE
            CALL U2MESK('A','SENSIBILITE_3', 2 ,VALKM)
            GO TO 490
          END IF

C DETERMINATION DU TYPE DE DERIVE: TYPESE ET STYPSE

          IF (TYSD.EQ.'EVOL_ELAS') THEN
            CALL METYSE(NBPASE,INPSCO,NOPASE,TYPESE,STYPSE)
          ELSE IF (TYSD.EQ.'DYNA_TRANS') THEN
            CALL METYSE(NBPASE,INPSCO,NOPASE,TYPESE,STYPSE)
          ELSE IF (TYSD.EQ.'EVOL_NOLI') THEN
            CALL METYSE(NBPASE,INPSCO,NOPASE,TYPESE,STYPSE)
          ELSE IF (TYSD.EQ.'DYNA_HARMO') THEN
            CALL METYSE(NBPASE,INPSCO,NOPASE,TYPESE,STYPSE)
          ELSE
            CALL U2MESK('A','SENSIBILITE_5',1,TYSD)
            GO TO 490
          END IF

          IF (NEWCAL) THEN
            CALL RSCRSD(LERES1,TYSD,NBORDR)
            CALL TITRE
          END IF

        END IF

C============ DEBUT DE LA BOUCLE SUR LES OPTIONS A CALCULER ============
        DO 440 IOPT = 1,NBOPT
C
          OPTION = ZK16(JOPT+IOPT-1)
          CODSEN=0
C
          CALL JEVEUO(KNUM,'L',JORDR)
          NUORD = ZI(JORDR)
          CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,NUORD,
     &                    NBORDR,NPASS,LIGREL)
          CALL JEVEUO(KCHA,'L',JCHA)
C
          CALL MECHAM(OPTION,MODELE,NCHAR,ZK8(JCHA),CARA,NH,CHGEOM,
     &                CHCARA,CHHARM,IRET)
          IF (IRET.NE.0) GO TO 530

C    ------------------------------------------------------------------
C    -- OPTIONS "SIGM_ELNO_DEPL","SIEF_ELGA_DEPL","EPSI_ELNO_DEPL",
C               "EPSI_ELGA_DEPL","EPSG_ELNO_DEPL","EPSG_ELGA_DEPL",
C               "EPME_ELNO_DEPL","EPME_ELGA_DEPL","EPMG_ELNO_DEPL",
C               "EPMG_ELGA_DEPL","EFGE_ELNO_DEPL","EPOT_ELEM_DEPL",
C               "SIPO_ELNO_DEPL","SIRE_ELNO_DEPL","DEGE_ELNO_DEPL",
C               "SIGM_ELNO_SIEF","SIPO_ELNO_SIEF",
C               "EPVC_ELGA","EPVC_ELNO"
C    ------------------------------------------------------------------
          IF (OPTION.EQ.'SIGM_ELNO_DEPL' .OR.
     &        OPTION.EQ.'SIEF_ELGA_DEPL' .OR.
     &        OPTION.EQ.'EPSI_ELNO_DEPL' .OR.
     &        OPTION.EQ.'EPSI_ELGA_DEPL' .OR.
     &        OPTION.EQ.'EPSG_ELNO_DEPL' .OR.
     &        OPTION.EQ.'EPSG_ELGA_DEPL' .OR.
     &        OPTION.EQ.'EPME_ELNO_DEPL' .OR.
     &        OPTION.EQ.'EPME_ELGA_DEPL' .OR.
     &        OPTION.EQ.'EPVC_ELNO'      .OR.
     &        OPTION.EQ.'EPVC_ELGA'      .OR.
     &        OPTION.EQ.'EPMG_ELNO_DEPL' .OR.
     &        OPTION.EQ.'EPMG_ELGA_DEPL' .OR.
     &        OPTION.EQ.'EFGE_ELNO_DEPL' .OR.
     &        OPTION.EQ.'EPOT_ELEM_DEPL' .OR.
     &        OPTION.EQ.'SIPO_ELNO_DEPL' .OR.
     &        OPTION.EQ.'SIRE_ELNO_DEPL' .OR.
     &        OPTION.EQ.'DEGE_ELNO_DEPL' .OR.
     &        OPTION.EQ.'SIGM_ELNO_SIEF' .OR.
     &        OPTION.EQ.'SIPO_ELNO_SIEF') THEN

C ---- VERIF SENSIBILITE
            IF (OPTION.EQ.'EPSI_ELNO_DEPL' .OR.
     &          OPTION.EQ.'EPSI_ELGA_DEPL' .OR.
     &          OPTION.EQ.'SIGM_ELNO_DEPL' .OR.
     &          OPTION.EQ.'SIEF_ELGA_DEPL' .OR.
     &          OPTION.EQ.'EFGE_ELNO_DEPL' .OR.
     &          OPTION.EQ.'SIPO_ELNO_DEPL' ) THEN
               IF (TYPESE.EQ.4) THEN
                  CODSEN = 1
               ENDIF
            ELSE
               IF (TYPESE.NE.0) THEN
                  CODSEN = 1
               ENDIF
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN

C ---- TRAITEMENT DE L EXCENTREMENT POUR OPTIONS DE POST TRAITEMENT
            IF (NCHAR.NE.0 .AND. CTYP.NE.'MECA') THEN
              CALL U2MESS('A','CALCULEL2_98')
              GO TO 440
            END IF
            IF (CONCEP.EQ.'DYNA_HARMO') THEN
              IF (OPTION.EQ.'SIGM_ELNO_DEPL') THEN
                OPTIO2 = OPTION
              ELSE IF (OPTION.EQ.'SIPO_ELNO_DEPL') THEN
                OPTIO2 = OPTION
              ELSE IF (OPTION.EQ.'EFGE_ELNO_DEPL') THEN
                OPTIO2 = OPTION
              ELSE IF (OPTION.EQ.'SIEF_ELGA_DEPL') THEN
                OPTIO2 = OPTION
              ELSE IF (OPTION.EQ.'SIEF_ELNO_ELGA') THEN
                OPTIO2 = OPTION
              ELSE IF (OPTION.EQ.'EPSI_ELGA_DEPL') THEN
                OPTIO2 = OPTION
              ELSE IF (OPTION.EQ.'EPSI_ELNO_DEPL') THEN
                OPTIO2 = OPTION
              ELSE IF (OPTION.EQ.'EPOT_ELEM_DEPL') THEN
                OPTIO2 = OPTION
              ELSE IF (OPTION.EQ.'ECIN_ELEM_DEPL') THEN
                OPTIO2 = OPTION
              ELSE IF (OPTION.EQ.'ENEL_ELGA') THEN
                OPTIO2 = OPTION
               ELSE IF (OPTION.EQ.'ENEL_ELNO_ELGA') THEN
                OPTIO2 = OPTION
             ELSE
                GO TO 520
              END IF
            ELSE IF (CONCEP.EQ.'EVOL_NOLI') THEN
              IF (OPTION.EQ.'SIGM_ELNO_DEPL' .OR.
     &            OPTION.EQ.'SIPO_ELNO_DEPL' .OR.
     &            OPTION.EQ.'SIEF_ELGA_DEPL' .OR.
     &            OPTION.EQ.'EFGE_ELNO_DEPL') THEN
                CALL U2MESK('A','CALCULEL2_99',1,OPTION)
                GO TO 440
              END IF
              OPTIO2 = OPTION
            ELSE
              OPTIO2 = OPTION
            END IF

            IF (TYPESE.EQ.-1) THEN
              IF (OPTIO2.EQ.'SIEF_ELGA_DEPL') THEN
                OPTIO2 = 'DLSI_ELGA_DEPL'
              END IF
            END IF

            DO 70,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              IF (TYPESE.EQ.0) THEN
                CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
                IF (IRET.GT.0) GO TO 72
              ELSE IF (TYPESE.EQ.-1) THEN
                CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
                IF (IRET.GT.0) GO TO 72
                CALL RSEXC2(1,1,LERES0,'DEPL',IORDR,CHDESE,OPTION,IRET)
                IF (IRET.GT.0) GO TO 72
              ELSE IF (TYPESE.EQ.3) THEN
                CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHDESE,OPTION,IRET)
                IF (IRET.GT.0) GO TO 72
                CALL RSEXC2(1,1,LERES0,'DEPL',IORDR,CHAMGD,OPTION,IRET)
                IF (IRET.GT.0) GO TO 72
                IF (TYSD.EQ.'DYNA_HARMO'.OR.TYSD.EQ.'DYNA_TRANS') THEN
                     CALL RSEXC2(1,1,LERES0,'ACCE',IORDR,CHACSE,
     &                           OPTION,IRET)
                IF (IRET.GT.0) GO TO 72
                ENDIF
              ELSE
                CALL RSEXC2(1,1,LERES0,'DEPL',IORDR,CHAMGD,OPTION,IRET)
                IF (IRET.GT.0) GO TO 72
              END IF
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
C=======================================================================
C                   SPECIAL POUTRE A LA POUX (2)
C=======================================================================
              CHAREP = ' '
              TYPCOE = ' '
              ALPHA = ZERO
              CALPHA = CZERO
              IF (EXIPOU) THEN
                IF (TYSD.EQ.'MODE_MECA' .OR. TYSD.EQ.'MODE_ACOU') THEN
                  CALL JEVEUO(CHAMGD(1:19)//'.VALE','L',LDEPL)
                  CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORDR,0,LFREQ,K8B)
                  DO 20 II = 0,NEQ - 1
                    ZR(LVALE+II) = -ZR(LFREQ)*ZR(LDEPL+II)
   20             CONTINUE
                  CALL JELIBE(CHAMGD(1:19)//'.VALE')
                ELSE IF (TYSD.EQ.'DYNA_TRANS') THEN
                  CALL RSEXCH(RESUCO,'ACCE',IORDR,CHACCE,IRET)
                  IF (IRET.EQ.0) THEN
                    CALL JEVEUO(CHACCE//'.VALE','L',LACCE)
                    DO 30 II = 0,NEQ - 1
                      ZR(LVALE+II) = ZR(LACCE+II)
   30               CONTINUE
                    CALL JELIBE(CHACCE//'.VALE')
                  ELSE
                    CALL U2MESS('A','CALCULEL3_1')
                    DO 40 II = 0,NEQ - 1
                      ZR(LVALE+II) = ZERO
   40               CONTINUE
                  END IF
                ELSE IF (TYSD.EQ.'DYNA_HARMO') THEN
                  CALL RSEXCH(RESUCO,'ACCE',IORDR,CHACCE,IRET)
                  IF (IRET.EQ.0) THEN
                    CALL JEVEUO(CHACCE//'.VALE','L',LACCE)
                    DO 50 II = 0,NEQ - 1
                      ZC(LVALE+II) = ZC(LACCE+II)
   50               CONTINUE
                    CALL JELIBE(CHACCE//'.VALE')
                  ELSE
                    CALL U2MESS('A','CALCULEL3_1')
                    DO 60 II = 0,NEQ - 1
                      ZC(LVALE+II) = CZERO
   60               CONTINUE
                  END IF
                END IF
C --- CALCUL DU COEFFICIENT MULTIPLICATIF DE LA CHARGE
C     CE CALCUL N'EST EFFECTIF QUE POUR LES CONDITIONS SUIVANTES
C          * MODELISATION POUTRE
C          * PRESENCE D'UNE (ET D'UNE SEULE) CHARGE REPARTIE
C          * UTILISATION DU MOT-CLE FACTEUR EXCIT
                IF (NBCHRE.NE.0) THEN
                  PHASE = ZERO
                  IPUIS = 0
                  CALL GETVID('EXCIT','FONC_MULT',IOCC,1,1,K8B,L1)
                  CALL GETVID('EXCIT','FONC_MULT_C',IOCC,1,1,K8B,L2)
                  CALL GETVR8('EXCIT','COEF_MULT',IOCC,1,1,COEF,L3)
                  CALL GETVC8('EXCIT','COEF_MULT_C',IOCC,1,1,CCOEF,L4)
                  CALL GETVR8('EXCIT','PHAS_DEG',IOCC,1,1,PHASE,L5)
                  CALL GETVIS('EXCIT','PUIS_PULS',IOCC,1,1,IPUIS,L6)
                  IF (L1.NE.0 .OR. L2.NE.0 .OR. L3.NE.0 .OR.
     &                L4.NE.0 .OR. L5.NE.0 .OR. L6.NE.0) THEN
                    IF (TYSD.EQ.'DYNA_HARMO') THEN
                      TYPCOE = 'C'
                      CALL RSADPA(RESUCO,'L',1,'FREQ',IORDR,0,LFREQ,K8B)
                      FREQ = ZR(LFREQ)
                      OMEGA = R8DEPI()*FREQ
                      IF (L1.NE.0) THEN
                        CALL FOINTE('F ',K8B,1,'FREQ',FREQ,VALRES,IER)
                        CALPHA = DCMPLX(VALRES,ZERO)
                      ELSE IF (L2.NE.0) THEN
                        CALL FOINTC(K8B,1,'FREQ',FREQ,VALRES,VALIM,IER)
                        CALPHA = DCMPLX(VALRES,VALIM)
                      ELSE IF (L3.NE.0) THEN
                        CALPHA = DCMPLX(COEF,UN)
                      ELSE IF (L4.NE.0) THEN
                        CALPHA = CCOEF
                      END IF
                      IF (L5.NE.0) THEN
                        CALPHA = CALPHA*EXP(DCMPLX(ZERO,PHASE*R8DGRD()))
                      END IF
                      IF (L6.NE.0) THEN
                        CALPHA = CALPHA*OMEGA**IPUIS
                      END IF
                    ELSE IF (TYSD.EQ.'DYNA_TRANS') THEN
                      TYPCOE = 'R'
                      CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,LINST,K8B)
                      INST = ZR(LINST)
                      IF (L1.NE.0) THEN
                        CALL FOINTE('F ',K8B,1,'INST',INST,ALPHA,IER)
                      ELSE IF (L3.NE.0) THEN
                        ALPHA = COEF
                      ELSE
                        CALL U2MESS('A','CALCULEL3_2')
                        CALL JEDEMA
                        GO TO 440
                      END IF
                    ELSE IF (TYSD.EQ.'EVOL_ELAS') THEN
                      TYPCOE = 'R'
                      IF (L1.NE.0) THEN
                        CALL FOINTE('F ',K8B,1,'INST',INST,ALPHA,IER)
                      ELSE
                        CALL U2MESS('A','CALCULEL3_3')
                        CALL JEDEMA
                        GO TO 440
                      END IF
                    ELSE
                      CALL U2MESS('A','CALCULEL3_4')
                      CALL JEDEMA
                      GO TO 440
                    END IF
                  END IF
                END IF
                IF (IOCC.GT.0) THEN
                  CALL GETVID('EXCIT','CHARGE',IOCC,1,1,CHAREP,N1)
                  IF(N1.EQ.0) CHAREP=ZK8(JCHA-1+IOCC)
                END IF
              END IF
C=======================================================================
              IF (TYSD.EQ.'FOURIER_ELAS') THEN
                CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
                CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
              END IF
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
                CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
              ELSE
                CHTIME = ' '
                TIME = ZERO
              END IF

              CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC)
              CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
C -- POUR LES POUTRES MULTIFIBRES ON A BESOIN DE COMPOR ISSU DE MATERIAU
C     POUR LE CALCUL DES OPTIONS SIEF_ELGA_DEPL ET EFGE_ELNO_DEPL
              IF(OPTIO2.EQ.'EFGE_ELNO_DEPL'.OR.
     &           OPTIO2.EQ.'SIEF_ELGA_DEPL')THEN
                COMPOR=MATE(1:8)//'.COMPOR'
              ENDIF
              IF (OPTION.EQ.'SIGM_ELNO_SIEF' .OR.
     &            OPTION.EQ.'SIPO_ELNO_SIEF') THEN
                CALL RSEXC2(1,2,RESUCO,'SIEF_ELNO_ELGA',IORDR,CHSIG,
     &                      OPTION,IRET1)
                CALL RSEXC2(2,2,RESUCO,'EFGE_ELNO_DEPL',IORDR,CHSIG,
     &                      OPTION,IRET2)
                IF ((IRET1.GT.0) .AND. (IRET2.GT.0)) THEN
                  CALL U2MESK('A','CALCULEL3_5',1,OPTION)
                  CALL JEDEMA
                  GO TO 440
                END IF
              END IF
              IF (TYPESE.EQ.-1) THEN
                CHTESE = '&&'//NOMPRO//'.TEMP_SENSI'
                CALL NMDETE(MODEL2,MATE2,CARELE,NCHAR,ZK8(JCHA),TIME,
     &                      TYPESE,NOPASE,CHTESE,LBID)
              ELSE
                CHTESE = ' '
              END IF
              CALL MECALC(OPTIO2,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    CHTEMP,K24B,CHTIME,CHNUMC,CHHARM,CHSIG,
     &                    CHEPS,CHFREQ,CHMASS,CHMETA,CHAREP,TYPCOE,
     &                    ALPHA,CALPHA,CHDYNR,SOP,CHELEM,LIGREL,BASE,
     &                    CHVARC,CHVREF,K24B,COMPOR,CHTESE,
     &                    CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 72
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
   72         CONTINUE
              CALL JEDEMA()
   70       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "SIEF_ELNO_ELGA"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'SIEF_ELNO_ELGA') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.EQ.4) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 90,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,2,LERES0,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                    IRET1)
              CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA_DEPL',IORDR,CHSIG,
     &                    OPTION,IRET2)
              IF (IRET1.GT.0 .AND. IRET2.GT.0) GO TO 92
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
              ELSE
                TIME = ZERO
              END IF
              CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC)
              CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
              CALL RSEXCH(RESUCO,'DEPL',IORDR,CHAMGD,IRET)

C      A PARTIR DE SIEF_ELGA
              IF (IRET1.EQ.0) THEN
                CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)

C      A PARTIR DE SIEF_ELGA_DEPL
              ELSE IF (IRET2.EQ.0) THEN
                COMPOR = ' '
              END IF

              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    K24B,K24B,K24B,K24B,K24B,CHSIG,K24B,K24B,
     &                    K24B,K24B,K24B,TYPCOE,ALPHA,CALPHA,K24B,SOP,
     &                    CHELEM,LIGREL,BASE,CHVARC,CHVREF,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 92
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
   92         CONTINUE
              CALL JEDEMA()
   90       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "ECIN_ELEM_DEPL"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'ECIN_ELEM_DEPL') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            IF (NCHAR.NE.0 .AND. CTYP.NE.'MECA') THEN
              CALL U2MESS('A','CALCULEL2_98')
              GO TO 440
            END IF
            IF (TYSD.EQ.'MODE_MECA') THEN
              TYPE = 'DEPL'
            ELSE IF (TYSD.EQ.'EVOL_NOLI') THEN
              TYPE = 'VITE'
            ELSE IF (TYSD.EQ.'DYNA_TRANS') THEN
              TYPE = 'VITE'
            ELSE IF (TYSD.EQ.'DYNA_HARMO') THEN
              TYPE = 'VITE'
            ELSE
              VALKM(1)=OPTION
              VALKM(2)=TYSD
              CALL U2MESK('A','CALCULEL3_6', 2 ,VALKM)
              GO TO 440
            END IF
            CHMASS = '&&'//NOMPRO//'.MASD'
            CALL MECACT('V',CHMASS,'MAILLA',NOMA,'POSI',1,'POS',INUME,
     &                  R8B,CBID,K8B)
            DO 110,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,TYPE//'            ',IORDR,CHAMGD,
     &                    OPTION,IRET)
              IF (IRET.GT.0) GO TO 112
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              FREQ = UN
              IF (TYSD.EQ.'FOURIER_ELAS') THEN
                CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
                CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
              END IF
              IF (TYPE.EQ.'DEPL') THEN
                CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORDR,0,LFREQ,K8B)
                FREQ = ZR(LFREQ)
              END IF
              CHFREQ = '&&'//NOMPRO//'.FREQ'
              CALL MECACT('V',CHFREQ,'MAILLA',NOMA,'FREQ_R',1,'FREQ',
     &                    IBID,FREQ,CBID,K8B)
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
                CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
              ELSE
                CHTIME = ' '
                TIME = ZERO
              END IF
              CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC)
              CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    K24B,K24B,CHTIME,CHNUMC,CHHARM,CHSIG,
     &                    CHEPS,CHFREQ,CHMASS,CHMETA,ZK8(JCHA),K24B,
     &                    ZERO,CZERO,CHDYNR,SOP,CHELEM,LIGREL,BASE,
     &                    CHVARC,CHVREF,K24B,COMPOR,CHTESE,CHDESE,
     &                    NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 112
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
              CALL DETRSD('CHAMP_GD',CHFREQ)
  112         CONTINUE
              CALL JEDEMA()
  110       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTIONS "SIGM_NOZ1_ELGA","SIGM_NOZ2_ELGA"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'SIGM_NOZ1_ELGA' .OR.
     &             OPTION.EQ.'SIGM_NOZ2_ELGA') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 130,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
              IF (IRET.GT.0) GO TO 132
              CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                    IRET)
              CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA_DEPL',IORDR,CHSIG,
     &                    OPTION,IRET)
              IF (IRET.GT.0) THEN
                CALL U2MESK('A','CALCULEL3_7',1,OPTION)
                CALL JEDEMA
                GO TO 440
              END IF
              CALL RSEXC1(LERES1,OPTION,IORDR,CHSIGN)
              IF (OPTION.EQ.'SIGM_NOZ1_ELGA') THEN
                CALL SINOZ1(MODELE,CHSIG,CHSIGN)
              ELSE IF (OPTION.EQ.'SIGM_NOZ2_ELGA') THEN
                CALL DISMOI('F','NOM_NUME_DDL',CHAMGD,'CHAM_NO',IB,NUME,
     &                      IE)
                CALL SINOZ2(MODELE,NUME,CHSIG,CHSIGN)
              END IF
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  132         CONTINUE
              CALL JEDEMA()
  130       CONTINUE

C    ------------------------------------------------------------------
C    -- OPTIONS "EPSP_ELNO","EPSP_ELGA"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'EPSP_ELNO' .OR.
     &             OPTION.EQ.'EPSP_ELGA' .OR.
     &             OPTION.EQ.'EPFP_ELNO' .OR.
     &             OPTION.EQ.'EPFP_ELGA' .OR.
     &             OPTION.EQ.'EPFD_ELNO' .OR.
     &             OPTION.EQ.'EPFD_ELGA'     ) THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 150,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
              IF (IRET.GT.0) GO TO 152
              CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                    IRET)
              CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA_DEPL',IORDR,CHSIG,
     &                    OPTION,IRET)
              IF (IRET.GT.0) THEN
                CALL U2MESK('A','CALCULEL3_7',1,OPTION)
                CALL JEDEMA
                GO TO 440
              END IF
              CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR,CHVARI,OPTION,
     &                    IRET)
              IF (IRET.GT.0) CHVARI = ' '
              CALL RSEXC1(LERES1,OPTION,IORDR,CHEPSP)
              IF (TYSD.EQ.'FOURIER_ELAS') THEN
                CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
                CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
              END IF
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
                CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
              ELSE
                CHTIME = ' '
                TIME = ZERO
              END IF
              CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC)
              CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
              CALL MECHDA(MODELE,NCHAR,ZK8(JCHA),EXITIM,TIME,CHEPSA)
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    K24B,K24B,CHTIME,CHNUMC,CHHARM,CHSIG,
     &                    CHEPSA,CHFREQ,CHMASS,CHMETA,ZK8(JCHA),K24B,
     &                    ZERO,CZERO,CHDYNR,SOP,CHEPSP,LIGREL,BASE,
     &                    CHVARC,CHVREF,CHVARI,COMPOR,CHTESE,
     &                    CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 152
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  152         CONTINUE
              CALL JEDEMA()
  150       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTIONS "EQUI_ELGA_EPSI","EQUI_ELGA_EPME","EQUI_ELGA_SIGM",
C               "EQUI_ELNO_EPSI","EQUI_ELNO_EPME","PMPB_ELGA_SIEF",
C               "PMPB_ELNO_SIEF","EQUI_ELNO_SIGM","CRIT_ELNO_RUPT"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'EQUI_ELGA_EPSI' .OR.
     &             OPTION.EQ.'EQUI_ELGA_EPME' .OR.
     &             OPTION.EQ.'EQUI_ELGA_SIGM' .OR.
     &             OPTION.EQ.'EQUI_ELNO_EPSI' .OR.
     &             OPTION.EQ.'EQUI_ELNO_EPME' .OR.
     &             OPTION.EQ.'PMPB_ELGA_SIEF' .OR.
     &             OPTION.EQ.'PMPB_ELNO_SIEF' .OR.
     &             OPTION.EQ.'EQUI_ELNO_SIGM' .OR.
     &             OPTION.EQ.'CRIT_ELNO_RUPT') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 160,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CHEPS = ' '
              CHSIG = ' '
              CHSIC = ' '
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              IF (OPTION.EQ.'EQUI_ELGA_EPSI') THEN
                CALL RSEXC2(1,1,RESUCO,'EPSI_ELGA_DEPL',IORDR,CHEPS,
     &                      OPTION,IRET)
                IF (IRET.GT.0) GO TO 162
              ELSE IF (OPTION.EQ.'EQUI_ELNO_EPSI') THEN
                CALL RSEXC2(1,1,RESUCO,'EPSI_ELNO_DEPL',IORDR,CHEPS,
     &                      OPTION,IRET)
                IF (IRET.GT.0) GO TO 162
              ELSE IF (OPTION.EQ.'EQUI_ELGA_EPME') THEN
                CALL RSEXC2(1,1,RESUCO,'EPME_ELGA_DEPL',IORDR,CHEPS,
     &                      OPTION,IRET)
                IF (IRET.GT.0) GO TO 162
              ELSE IF (OPTION.EQ.'EQUI_ELNO_EPME') THEN
                CALL RSEXC2(1,1,RESUCO,'EPME_ELNO_DEPL',IORDR,CHEPS,
     &                      OPTION,IRET)
                IF (IRET.GT.0) GO TO 162
              ELSE IF (OPTION.EQ.'EQUI_ELGA_SIGM') THEN
                CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                      IRET)
                CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA_DEPL',IORDR,CHSIG,
     &                      OPTION,IRET)
                IF (IRET.GT.0) THEN
                  CALL U2MESK('A','CALCULEL3_7',1,OPTION)
                  CALL JEDEMA
                  GO TO 440
                END IF
              ELSE IF (OPTION.EQ.'PMPB_ELGA_SIEF') THEN
                CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                      IRET1)
                IF (IRET1.GT.0) GO TO 162
              ELSE IF (OPTION.EQ.'PMPB_ELNO_SIEF') THEN
                CALL RSEXC2(1,1,RESUCO,'SIEF_ELNO_ELGA',IORDR,CHSIG,
     &                      OPTION,IRET1)
                IF (IRET1.GT.0) GO TO 162
              ELSE IF (OPTION.EQ.'EQUI_ELNO_SIGM') THEN
                CALL RSEXCH(RESUCO,'SIEF_ELGA_DEPL',IORDR,CHSIG,IRET1)
                CALL RSEXCH(RESUCO,'SIEF_ELGA',IORDR,CHSIG,IRET2)
                CALL RSEXCH(RESUCO,'SIGM_ELNO_COQU',IORDR,CHSIC,IRET3)
                CALL RSEXCH(RESUCO,'SIGM_ELNO_DEPL',IORDR,CHSIC,IRET4)
C
                IF (IRET1.GT.0 .AND. IRET2.GT.0 .AND. IRET3.GT.0.
     &              AND. IRET4.GT.0) THEN
                  VALKM(1)=OPTION
                  VALKM(2)=TYSD
                  CALL U2MESK('A','CALCULEL3_8', 2 ,VALKM)
                  CALL JEDEMA
                  GO TO 440
                END IF
                IF (TYSD.EQ.'EVOL_ELAS' .OR. TYSD.EQ.'DYNA_TRANS' .OR.
     &              TYSD.EQ.'MULT_ELAS' .OR. TYSD.EQ.'MODE_MECA'  .OR.
     &              TYSD.EQ.'FOURIER_ELAS') THEN
C          CHAMP D'ENTREE POUR ELEMENTS ISOPARAMETRIQUES
                  IF (IRET1.LE.0) THEN
                     CALL RSEXCH(RESUCO,'SIEF_ELGA_DEPL',IORDR,CHSIG,K)
                  END IF
C          CHAMP D'ENTREE POUR COQUES
                  IF (EXIPLA) THEN
                    IF (IRET4.GT.0) THEN
                      VALKM(1)=OPTION
                      VALKM(2)=TYSD
                      CALL U2MESK('A','CALCULEL3_9', 2 ,VALKM)
                      CALL JEDEMA
                      GO TO 440
                    ELSE
                      CALL RSEXCH(RESUCO,'SIGM_ELNO_DEPL',IORDR,CHSIC,K)
                    END IF
                  END IF
                ELSE IF (TYSD.EQ.'EVOL_NOLI') THEN
                  IF (IRET2.LE.0) THEN
                    CALL RSEXCH(RESUCO,'SIEF_ELGA',IORDR,CHSIG,K)
                  END IF
                  IF (EXIPLA) THEN
                    IF (IRET3.GT.0) THEN
                      VALKM(1)=OPTION
                      VALKM(2)=TYSD
                      CALL U2MESK('A','CALCULEL3_10', 2 ,VALKM)
                      CALL JEDEMA
                      GO TO 440
                    ELSE
                      CALL RSEXCH(RESUCO,'SIGM_ELNO_COQU',IORDR,CHSIC,K)
                    END IF
                  END IF
                END IF
              ELSE IF (OPTION.EQ.'CRIT_ELNO_RUPT') THEN
                CALL RSEXC2(1,1,RESUCO,'SIGM_ELNO_DEPL',IORDR,CHSIG,
     &                      OPTION,IRET1)
                IF (IRET1.GT.0) GO TO 162
              END IF
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECALC(OPTION,MODELE,K24B,CHGEOM,MATE,CHCARA,K24B,
     &                    K24B,K24B,CHNUMC,K24B,CHSIG,CHEPS,CHSIC,K24B,
     &                    K24B,ZK8(JCHA),K24B,ZERO,CZERO,K24B,K24B,
     &                    CHELEM,LIGREL,BASE,K24B,K24B,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 162
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  162         CONTINUE
              CALL JEDEMA()
  160       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "VALE_NCOU_MAXI"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'VALE_NCOU_MAXI') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN

            CALL GETVTX(' ','NOM_CHAM',1,1,1,NOMCHA,NBVAL)
            CALL GETVTX(' ','NOM_CMP',1,1,1,NOMCMP,NBVAL)

            DO 170,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,NOMCHA,IORDR,CHBID,OPTION,IRET)
              IF (IRET.GT.0) GO TO 172
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              Z1Z2(1) = 'Z1'
              Z1Z2(2) = 'Z2'
              NOMS(1) = NOMCHA
              NOMS(2) = NOMCMP
              CHCMP = '&&OP0058.ELGA_MAXI'
              CALL MECACT('V',CHCMP,'MODELE',MODELE,'NEUT_K24',2,Z1Z2,
     &                    IBID,RBID,CBID,NOMS)

              IF ((NOMCHA.EQ.'SIEF_ELGA') .OR.
     &            (NOMCHA.EQ.'SIEF_ELGA_DEPL')) THEN
                CHSIG = CHBID
                CHEPS = ' '
                CHSEQ = ' '
                CHEEQ = ' '
                CHVARI = ' '
              ELSE IF (NOMCHA.EQ.'EPSI_ELGA_DEPL') THEN
                CHSIG = ' '
                CHEPS = CHBID
                CHSEQ = ' '
                CHEEQ = ' '
                CHVARI = ' '
              ELSE IF (NOMCHA.EQ.'EQUI_ELGA_SIGM') THEN
                CHSIG = ' '
                CHEPS = ' '
                CHSEQ = CHBID
                CHEEQ = ' '
                CHVARI = ' '
              ELSE IF (NOMCHA.EQ.'EQUI_ELGA_EPSI') THEN
                CHSIG = ' '
                CHEPS = ' '
                CHSEQ = ' '
                CHEEQ = CHBID
                CHVARI = ' '
              ELSE IF (NOMCHA.EQ.'VARI_ELGA') THEN
                CHSIG = ' '
                CHEPS = ' '
                CHSEQ = ' '
                CHEEQ = ' '
                CHVARI = CHBID
              END IF

              CALL MECALC(OPTION,MODELE,K24B,CHGEOM,MATE,CHCARA,K24B,
     &                    K24B,K24B,CHCMP,K24B,CHSIG,CHEPS,CHSIC,K24B,
     &                    K24B,ZK8(JCHA),K24B,ZERO,CZERO,K24B,K24B,
     &                    CHELEM,LIGREL,BASE,CHSEQ,CHEEQ,K24B,
     &                    COMPOR,CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,
     &                    IRET)

              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  172         CONTINUE
              CALL JEDEMA()
  170       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "ARCO_ELNO_SIGM"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'ARCO_ELNO_SIGM') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
            CHMASS='&&MECALCM.ANGLE'
            CALL MECHC2(CHGEOM,CHMASS)
C ---- VERIF SENSIBILITE FIN
            DO 185,IAUX = 1,NBORDR
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,2,RESUCO,'SIGM_ELNO_DEPL',IORDR,CHSIG,
     &                    OPTION,IRET1)
              CALL RSEXC2(2,2,RESUCO,'SIEF_ELNO_ELGA',IORDR,CHSIG,
     &                    OPTION,IRET2)
              IF (IRET1.GT.0 .OR. IRET2.GT.0) GO TO 187
              CHSIGF='&&'//NOMPRO//'.CHAM_SI2D'
              CALL MEARCC(OPTION,MODELE,CHSIG,CHSIGF)
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                   K24B,K24B,K24B,K24B,CHSIGF,CHMASS,K24B,K24B,
     &                   K24B,K24B,K24B,ZERO,CZERO,K24B,K24B,CHELEM,
     &                   LIGREL,BASE,K24B,K24B,K24B,COMPOR,CHTESE,
     &                   CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
              CALL DETRSD('CHAM_ELEM',CHSIGF)
  187         CONTINUE
  185       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTIONS DES INDICATEURS D'ERREURS
C    ------------------------------------------------------------------
          ELSE IF ( OPTION.EQ.'ERZ1_ELEM_SIGM' .OR.
     &              OPTION.EQ.'ERZ2_ELEM_SIGM' .OR.
     &              OPTION.EQ.'ERRE_ELEM_SIGM' .OR.
     &              OPTION.EQ.'ERRE_ELNO_ELEM' .OR.
     &              OPTION.EQ.'QIRE_ELEM_SIGM' .OR.
     &              OPTION.EQ.'QIRE_ELNO_ELEM' .OR.
     &              OPTION.EQ.'QIZ1_ELEM_SIGM' .OR.
     &              OPTION.EQ.'QIZ2_ELEM_SIGM' ) THEN
C
            CALL MECA01 ( OPTION, NBORDR, JORDR ,
     &                    NCHAR , JCHA  , KCHA  , CTYP  ,
     &                    TBGRCA, RESUCO, RESUC1, LERES1,
     &                    NOMA  , MODELE, LIGRMO, MATE  , CARA,
     &                    TYPESE, IRET )
C
            IF ( IRET.EQ.1 ) THEN
              GOTO 900
            ELSEIF ( IRET.EQ.2 ) THEN
              GOTO 530
            ELSEIF ( IRET.EQ.3 ) THEN
              GOTO 440
            ENDIF
C
C    ------------------------------------------------------------------
C    -- OPTION "SING_ELEM"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'SING_ELEM') THEN

            CALL GETVR8(' ','PREC_ERR',1,1,1,PREC,IRET1)
            IF(IRET1.NE.1) THEN
              CALL U2MESS('F','CALCULEL3_12')
            ELSE
              IF(PREC.LE.0.D0.OR.PREC.GT.1.D0) THEN
                CALL U2MESS('F','CALCULEL3_13')
              ENDIF
            ENDIF

C 1 - RECUPERATION DE :
C  NNOEM : NOMBRE DE NOEUDS
C  NELEM : NOMBRE D ELEMENTS FINIS (EF)
C  NDIM  : DIMENSION
C  JCOOR : ADRESSE DES COORDONNEES
C  JTYPE : ADRESSE DU TYPE D ELEMENTS FINIS

            CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,NOMA,
     &                  IERD)

            CALL JEVEUO(NOMA//'.DIME','L',JDIM)
            CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
            CALL JEVEUO(NOMA//'.TYPMAIL','L',JTYPE)

            NNOEM=ZI(JDIM)
            NELEM=ZI(JDIM+2)
            NDIM=ZI(JDIM+5)

C 2 - CREATION D OBJETS TEMPORAIRES UTILES POUR LA SUITE
C '&&SINGUM.DIME' (DIM=3) CONTIENT
C   NBRE MAX DE NOEUDS SOMMETS CONNECTES AUX EF (NSOMMX)
C   NBRE MAX D EF CONNECTES AUX NOEUDS (NELCOM)
C   DEGRE DES EF (1 SI LINEAIRE ET 2 SI QUADRATIQUE)
C '&&SINGUM.MESU' (DIM=NELEM) CONTIENT L AIRE OU LE VOLUME DES EFS
C '&&SINGUM.CONN' (DIM=NELEM*(NSOMMX+2)) CONTIENT
C   1ERE VALEUR = NBRE DE NOEUDS SOMMETS CONNECTES A L EF N°X
C   2EME VALEUR = 1 SI EF EST SURFACIQUE EN 2D ET VOLUMIQUE EN 3D
C                 0 SINON
C   CONNECTIVITE EF N°X=>N° DES NOEUDS SOMMETS CONNECTES A X
C '&&SINGUM.CINV' (DIM=NNOEM*(NELCOM+2)) CONTIENT
C   1ERE VALEUR = NBRE D EF CONNECTES AU NOEUD N°X
C   2EME VALEUR = 0 NOEUD MILIEU OU NON CONNECTE A UN EF UTILE
C                 1 NOEUD SOMMET A L INTERIEUR + LIE A UN EF UTILE
C                 2 NOEUD SOMMET BORD + LIE A UN EF UTILE
C                 EF UTILE = EF SURF EN 2D ET VOL EN 3D
C   CONNECTIVITE INVERSE NOEUD N°X=>N° DES EF CONNECTES A X

            CALL SINGUM (NOMA,NDIM,NNOEM,NELEM,ZI(JTYPE),ZR(JCOOR))

C 3 - BOUCLE SUR LES INSTANTS DEMANDES

            DO 200 IAUX = 1,NBORDR
              CALL JEMARQ()
              IORDR = ZI(JORDR+IAUX-1)

C 3.1 - RECUPERATION DE LA CARTE D ERREUR ET D ENERGIE
C       SI PLUSIEURS INDICATEURS ON PREND PAR DEFAUT
C       ERRE_ELEM_SIGM SI IL EST PRESENT
C       ERZ2_ELEM_SIGM PAR RAPPORT A ERZ1_ELEM_SIGM

              CALL RSEXCH(RESUCO,'ERRE_ELEM_SIGM',IORDR,CHERR1,IRET1)
              CALL RSEXCH(RESUCO,'ERZ1_ELEM_SIGM',IORDR,CHERR2,IRET2)
              CALL RSEXCH(RESUCO,'ERZ2_ELEM_SIGM',IORDR,CHERR3,IRET3)

              IF (IRET1.GT.0.AND.IRET2.GT.0.AND.IRET3.GT.0) THEN
                CALL U2MESS('A','CALCULEL3_14')
                IRET=1
              ENDIF

              IF(TYSD.EQ.'EVOL_NOLI' ) THEN
                CALL RSEXC2(1,1,RESUCO,'ETOT_ELEM',IORDR,CHENEG,
     &                      OPTION,IRET4)
              ELSE
                CALL RSEXC2(1,1,RESUCO,'EPOT_ELEM_DEPL',IORDR,CHENEG,
     &                      OPTION,IRET4)
              ENDIF

              IF ((IRET+IRET4).GT.0) GOTO 210

C 3.2 - TRANSFORMATION DE CES DEUX CARTES EN CHAM_ELEM_S

              CHERRS='&&'//NOMPRO//'.ERRE'

              IF (IRET1.EQ.0) THEN
                CALL CELCES(CHERR1(1:19),'V',CHERRS)
                IF (IRET2.EQ.0.OR.IRET3.EQ.0) CALL U2MESS('A','CALCULEL3
     &_15')
              ELSE IF (IRET3.EQ.0) THEN
                CALL CELCES(CHERR3(1:19),'V',CHERRS)
                IF (IRET2.EQ.0) CALL U2MESS('A','CALCULEL3_16')
              ELSE
                CALL CELCES(CHERR2(1:19),'V',CHERRS)
              ENDIF

              CHENES='&&'//NOMPRO//'.ENER'
              CALL CELCES(CHENEG(1:19),'V',CHENES)

C 3.3 - ROUTINE PRINCIPALE QUI CALCULE DANS CHAQUE EF :
C       * LE DEGRE DE LA SINGULARITE
C       * LE RAPPORT ENTRE L ANCIENNE ET LA NOUVELLE TAILLE
C       DE L EF CONSIDERE
C       => CE RESULAT EST STOCKE DANS CHELEM (CHAM_ELEM)
C       CES DEUX COMPOSANTES SONT CONSTANTES PAR ELEMENT

              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)

              CALL SINGUE (CHERRS,CHENES,NOMA,NDIM,NNOEM,NELEM,
     &                    ZR(JCOOR),PREC,LIGRMO,IORDR,CHELEM)

              CALL RSNOCH(LERES1,OPTION,IORDR,' ')

C 3.4 - DESTRUCTION DES CHAM_ELEM_S

              CALL DETRSD('CHAM_ELEM_S',CHERRS)
              CALL DETRSD('CHAM_ELEM_S',CHENES)

 210          CONTINUE
              CALL JEDEMA()
 200        CONTINUE

C 4 - DESTRUCTION DES OBJETS TEMPORAIRES

            CALL JEDETR('&&SINGUM.DIME           ')
            CALL JEDETR('&&SINGUM.MESU           ')
            CALL JEDETR('&&SINGUM.CONN           ')
            CALL JEDETR('&&SINGUM.CINV           ')
C    ------------------------------------------------------------------
C    -- OPTION "SING_ELNO_ELEM"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'SING_ELNO_ELEM') THEN
            DO 220 IAUX = 1,NBORDR
              CALL JEMARQ()
              IORDR = ZI(JORDR+IAUX-1)

C 1 - RECUPERATION DE LA CARTE DE SINGULARITE

              CALL RSEXC2(1,1,RESUCO,'SING_ELEM',IORDR,CHSING,
     &                    OPTION,IRET1)

              IF (IRET1.GT.0) GOTO 222

C 2 - TRANSFORMATION DE CE CHAMP EN CHAM_ELEM_S

              CHSINS='&&'//NOMPRO//'.SING'
              CALL CELCES(CHSING(1:19),'V',CHSINS)

C 3 - TRANSFOMATION DU CHAMP CHSINS ELEM EN ELNO

              CHSINN='&&'//NOMPRO//'.SINN'
              CALL CESCES(CHSINS,'ELNO',' ',' ',' ','V',CHSINN)

C 4 - STOCKAGE

              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)

              CALL CESCEL(CHSINN,LIGRMO(1:19),'SING_ELNO_ELEM','PSINGNO'
     &                    ,'NON',NNCP,'G',CHELEM(1:19),'F',IBID)

              CALL RSNOCH(LERES1,OPTION,IORDR,' ')

C 5 - DESTRUCTION DES CHAM_ELEM_S

              CALL DETRSD('CHAM_ELEM_S',CHSINS)
              CALL DETRSD('CHAM_ELEM_S',CHSINN)

 222          CONTINUE
              CALL JEDEMA()
 220        CONTINUE
C    ------------------------------------------------------------------
C    -- OPTIONS "SIGM_ELNO_CART" ET "EFGE_ELNO_CART"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'SIGM_ELNO_CART' .OR.
     &             OPTION.EQ.'EFGE_ELNO_CART') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 240,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              IF (OPTION.EQ.'SIGM_ELNO_CART') THEN
                CALL RSEXC2(1,1,RESUCO,'SIGM_ELNO_DEPL',IORDR,CHAMGD,
     &                      OPTION,IRET)
              ELSE
                CALL RSEXC2(1,2,RESUCO,'EFGE_ELNO_DEPL',IORDR,CHSIG,
     &                      OPTION,IRET)
                CALL RSEXC2(2,2,RESUCO,'SIEF_ELNO_ELGA',IORDR,CHSIG,
     &                      OPTION,IRET)
                CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
              END IF
              IF (IRET.GT.0) GO TO 242
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                    K24B,K24B,K24B,K24B,CHSIG,K24B,K24B,K24B,K24B,
     &                    K24B,K24B,ZERO,CZERO,K24B,K24B,CHELEM,LIGREL,
     &                    BASE,K24B,K24B,K24B,COMPOR,CHTESE,CHDESE,
     &                    NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 242
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  242         CONTINUE
              CALL JEDEMA()
  240       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "VNOR_ELEM_DEPL"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'VNOR_ELEM_DEPL') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            IF (NCHAR.NE.0 .AND. CTYP.NE.'MECA') THEN
              CALL U2MESS('A','CALCULEL2_98')
              GO TO 440
            END IF
            DO 250,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'VITE',IORDR,CHAMGD,OPTION,IRET)
              IF (IRET.GT.0) GO TO 252
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              IF (TYSD.EQ.'FOURIER_ELAS') THEN
                CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
                CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
              END IF
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
                CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
              ELSE
                CHTIME = ' '
                TIME = ZERO
              END IF
              CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC)
              CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))

              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                    K24B,CHTIME,CHNUMC,CHHARM,CHSIG,
     &                    CHEPS,CHFREQ,CHMASS,CHMETA,ZK8(JCHA),' ',ZERO,
     &                    CZERO,CHDYNR,SOP,CHELEM,LIGREL,BASE,CHVARC,
     &                    CHVREF,K24B,COMPOR,CHTESE,CHDESE,NOPASE,
     &                    TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 252
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  252         CONTINUE
              CALL JEDEMA()
  250       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "PRES_DBEL_DEPL"
C    ------------------------------------------------------------------
      ELSE IF (OPTION.EQ.'PRES_DBEL_DEPL') THEN

C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 300,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
              TYPE = 'DEPL'
              IF (IRET.GT.0) GO TO 302
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECOA1(OPTION,MODELE,LIGREL,MATE,CHAMGD,CHELEM)
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  302         CONTINUE
              CALL JEDEMA()
  300       CONTINUE


C     ------------------------------------------------------------------
C     --- OPTIONS DE CALCUL DES INDICATEURS LOCAUX DE DECHARGE ET DE
C     --- PERTE DE RADIALITE :
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'DCHA_ELGA_SIGM' .OR.
     &             OPTION.EQ.'DCHA_ELNO_SIGM' .OR.
     &             OPTION.EQ.'RADI_ELGA_SIGM' .OR.
     &             OPTION.EQ.'RADI_ELNO_SIGM') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            IF (NBORDR.EQ.1) THEN
              CALL U2MESK('A','CALCULEL5_63',1,OPTION)
              GO TO 440
            END IF
            DO 310,IAUX = 1,NBORDR - 1
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR1 = ZI(JORDR+IAUX-1)
              IORDR2 = ZI(JORDR+IAUX)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR1,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR1,CHSIG1,OPTION,
     &                    IRET1)
              CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR2,CHSIG2,OPTION,
     &                    IRET2)
              IF (IRET1.GT.0 .OR. IRET2.GT.0) GO TO 312
              IF (NORME.EQ.'VMIS_CINE' .OR. NORME.EQ.'TOTAL_CINE') THEN
                CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR1,CHVAR1,OPTION,
     &                      IRET1)
                CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR2,CHVAR2,OPTION,
     &                      IRET2)
                IF (IRET1.GT.0 .OR. IRET2.GT.0) GO TO 312
              ELSE
                CHVAR1 = ' '
                CHVAR2 = ' '
              END IF
              CALL RSEXC1(LERES1,OPTION,IORDR1,CHELEM)
              CALL INDRAD(OPTION,NORME,MODELE,LIGREL,CHSIG1,CHSIG2,
     &                    CHVAR1,CHVAR2,CHELEM)
              CALL RSNOCH(RESUCO,OPTION,IORDR1,' ')
  312         CONTINUE
              CALL JEDEMA()
  310       CONTINUE
C     ------------------------------------------------------------------
C     --- OPTIONS DE CALCUL DES DENSITES D'ENERGIE TOTALE
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'ETOT_ELGA' .OR.
     &             OPTION.EQ.'ETOT_ELNO_ELGA' .OR.
     &             OPTION.EQ.'ETOT_ELEM') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 320,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)

C ---       RECUPERATION DES CONTRAINTES DE L'INSTANT COURANT :
C           -------------------------------------------------
              CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                    IRET1)
              IF (IRET1.GT.0) THEN
                 CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA_DEPL',IORDR,CHSIG,
     &                    OPTION,IRET2)
                 IF (IRET2.GT.0) THEN
                    CALL CODENT(IORDR,'G',KIORD)
                    VALKM(1)=RESUCO
                    VALKM(2)=KIORD
                    CALL U2MESK('A','CALCULEL3_17', 2 ,VALKM)
                    GO TO 321
                 END IF
              ENDIF

C ---       SI LE NUMERO D'ORDRE COURANT EST SUPERIEUR A 1, ON
C ---       RECUPERE LES CONTRAINTES DE L'INSTANT PRECEDENT :
C           -----------------------------------------------
              IF ((IAUX.GT.1).AND.(CONCEP.NE.'MODE_MECA')) THEN
                IORDRM = ZI(JORDR+IAUX-2)
                CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDRM,CHSIGM,
     &                      OPTION,IRET1)
                IF (IRET1.GT.0) THEN
                  CALL CODENT(IORDRM,'G',KIORDM)
                  VALKM(1)=RESUCO
                  VALKM(2)=KIORDM
                  CALL U2MESK('A','CALCULEL3_17', 2 ,VALKM)
                  GO TO 321
                END IF
              END IF

C ---       RECUPERATION DU CHAMP DE DEPLACEMENT DE L'INSTANT COURANT :
C           ---------------------------------------------------------
              CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHDEPL,OPTION,IRET1)
              IF (IRET1.GT.0) THEN
                CALL CODENT(IORDR,'G',KIORD)
                  VALKM(1)=RESUCO
                  VALKM(2)=KIORD
                CALL U2MESK('A','CALCULEL3_11', 2 ,VALKM)
                GO TO 321
              END IF

C ---       SI LE NUMERO D'ORDRE COURANT EST SUPERIEUR A 1, ON
C ---       RECUPERE LES DEPLACEMENTS DE L'INSTANT PRECEDENT :
C           ------------------------------------------------
              IF ((IAUX.GT.1).AND.(CONCEP.NE.'MODE_MECA')) THEN
                CALL RSEXC2(1,1,RESUCO,'DEPL',IORDRM,CHDEPM,OPTION,
     &                      IRET1)
                IF (IRET1.GT.0) THEN
                  CALL CODENT(IORDRM,'G',KIORDM)
                  VALKM(1)=RESUCO
                  VALKM(2)=KIORDM
                  CALL U2MESK('A','CALCULEL3_11', 2 ,VALKM)
                  GO TO 321
                END IF
              END IF

              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)

              IF (CONCEP.EQ.'MODE_MECA') THEN
              CALL ENETOT(OPTION,1,LIGREL,CHGEOM,CHDEPL,CHDEPM,CHSIG,
     &                    CHSIGM,CHELEM)
              ELSE

              CALL ENETOT(OPTION,IAUX,LIGREL,CHGEOM,CHDEPL,CHDEPM,CHSIG,
     &                    CHSIGM,CHELEM)
              ENDIF

              CALL RSNOCH(RESUCO,OPTION,IORDR,' ')
  321         CONTINUE
              CALL JEDEMA()
  320       CONTINUE
            CALL DETRSD('CHAMP_GD','&&ENETOT.CHAMELEM2')
C     ------------------------------------------------------------------
C     --- OPTIONS DE CALCUL DU TAUX DE TRIAXIALITE DES CONTRAINTES, ET
C     --- DE LA CONTRAINTE D'ENDOMMAGEMENT :
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'ENDO_ELNO_SIGA' .OR.
     &             OPTION.EQ.'ENDO_ELNO_SINO') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 340,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              IF (OPTION.EQ.'ENDO_ELNO_SIGA') THEN
                CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                      IRET)
                CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA_DEPL',IORDR,CHSIG,
     &                      OPTION,IRET)
                IF (IRET.GT.0) THEN
                   CALL U2MESK('A','CALCULEL3_18',1,OPTION)
                   GO TO 342
                ENDIF
              ELSE
                CALL RSEXC2(1,1,RESUCO,'SIGM_ELNO_DEPL',IORDR,CHSIG,
     &                      OPTION,IRET)
                IF (IRET.GT.0) GO TO 342
              END IF
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
              ELSE
                TIME = ZERO
              END IF
              CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC)
              CALL COENDO(OPTION,MODELE,LIGREL,MATE,CHVARC,CHSIG,CHELEM)
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  342         CONTINUE
              CALL JEDEMA()
  340       CONTINUE
C     ------------------------------------------------------------------
C     --- OPTIONS DE CALCUL DE:
C     ---   * TAUX DE TRIAXIALITE DES CONTRAINTES,
C     ---   * CONTRAINTE D'ENDOMMAGEMENT,
C     ---   * ENDOMMAGEMENT DE LEMAITRE-SERMAGE
C     --- RESPONSABLE DEVELOPPEMENT: FRANCK MEISSONNIER (AMA/T65)
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'ENDO_ELNO_ELGA'.OR.
     &             OPTION.EQ.'ENDO_ELGA') THEN
C
            IF (NBORDR.EQ.1) THEN
               CALL U2MESK('A','CALCULEL5_63',1,OPTION)
               GO TO 440
            END IF

C IORDR1 = ORDRE CORRESPONDANT AU TEMPS T-
C IORDR2 = ORDRE CORRESPONDANT AU TEMPS T+
            DO 341 IAUX = 2,NBORDR
               IORDR1 = ZI(JORDR-1+IAUX-1)
               IORDR2 = ZI(JORDR-1+IAUX)
C
C --- A/ TRAITEMENT DE L'OPTION ENDO_ELGA
C     -----------------------------------
               IF (OPTION.EQ.'ENDO_ELGA') THEN
C --- A11/ RECUPERATION DU CHAMP DE CONTRAINTES 'SIEF_ELGA'
C          -> CHSIG[1,2] A T- ET T+
                  CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR1,CHSIG1,
     &                        OPTION,IRET1)
                  CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA_DEPL',IORDR1,CHSIG1,
     &                        OPTION,IRET1)
                  IF (IRET1.GT.0) THEN
                     CALL U2MESK('A','CALCULEL3_18',1,OPTION)
                     GO TO 341
                  ENDIF

                  CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR2,CHSIG2,
     &                        OPTION,IRET2)
                  CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA_DEPL',IORDR2,CHSIG2,
     &                        OPTION,IRET2)
                  IF (IRET2.GT.0) THEN
                     CALL U2MESK('A','CALCULEL3_18',1,OPTION)
                     GO TO 341
                  ENDIF
C
C --- A12/ RECUPERATION DU CHAMP DE VARIABLES INTERNES 'VARI_ELGA'
C          -> CHVARI[1,2] A T- ET T+
                  NORME = 'DOM_LEM'
                  IF (NORME.EQ.'DOM_LEM') THEN
                     CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR1,CHVAR1,
     &                           OPTION,IRET1)
                     CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR2,CHVAR2,
     &                           OPTION,IRET2)
                     IF ((IRET1.GT.0).OR.(IRET2.GT.0)) GO TO 341
                  ELSE
                     CHVAR1 = ' '
                     CHVAR2 = ' '
                  END IF
C
                  IF (IAUX.EQ.2) THEN
C --- A21/ INITIALISATION DES VARIABLES A L'ORDRE IORDR1
                     CALL RSEXC1(LERES1,OPTION,IORDR1,CHELE1)
                     CALL ALCHML(LIGREL,'ENDO_ELGA','PTRIAGM',
     &                          'G',CHELE1,IRET,' ')
                     IF (IRET.GT.0) THEN
                        CALL U2MESK('A','CALCULEL3_19',1,OPTION)
                        GO TO 341
                     ENDIF
                     CALL RSNOCH(LERES1,OPTION,IORDR1,' ')

                  ELSE
C --- A22/ RECUPERATION DES VAR. ENDOMMAGEMENT @T
                     CALL RSEXC2(1,1,RESUCO,'ENDO_ELGA',IORDR1,CHELE1,
     &                           OPTION,IRET1)
                     IF (IRET1.GT.0) THEN
                        CALL U2MESK('A','CALCULEL3_20',1,OPTION)
                        GO TO 341
                     ENDIF
                  ENDIF

C --- A23/ RECUPERATION DU NOM DU CHAMP EXTRAIT DE LA SD LERES1 @T+1
                  CALL RSEXC1(LERES1,OPTION,IORDR2,CHELE2)

C --- A3/ RECUPERATION DU TEMPS CORRESPONDANT AUX ORDRES #IORDR[1,2]
                  IF (EXITIM) THEN
                     CALL RSADPA(RESUCO,'L',1,'INST',
     &                           IORDR1,0,IINST1,K8B)
                     CALL RSADPA(RESUCO,'L',1,'INST',
     &                           IORDR2,0,IINST2,K8B)
                     TIME1 = ZR(IINST1)
                     TIME2 = ZR(IINST2)
                     CALL MECHTI(NOMA,TIME1,RUNDF,RUNDF,CHTIM1)
                     CALL MECHTI(NOMA,TIME2,RUNDF,RUNDF,CHTIM2)
                  ELSE
                     CHTIM1 = ' '
                     CHTIM2 = ' '
                     TIME1 = ZERO
                     TIME2 = ZERO
                  END IF

C --- A4/ EVALUATION DES DONNEES MATERIAUX AUX INSTANTS - ET +
C     --------------------------------------------------------

                  CALL VRCINS(MODELE,MATE,CARA,TIME1,CHVARC)
                  CALL VRCINS(MODELE,MATE,CARA,TIME2,CHVAC2)

C --- A5/ CALCUL DU TAUX DE TRIAXIALITE, DE LA CONTRAINTE
C ---     D'ENDOMMAGEMENT ET DE L'ENDOMMAGEMENT DE LEMAITRE-SERMAGE
C     -------------------------------------------------------------
                  CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR1,COMPOR,IRET1)

                  CALL ENDOLE(OPTION,MODELE,LIGREL,MATE,COMPOR,
     &                        CHVARC,CHSIG1,CHVAR1,
     &                        CHVAC2,CHSIG2,CHVAR2,
     &                        CHEND2,CHELE1,CHELE2)

C --- A6/ ECRITURE DU CHAMP LERES1
C     ----------------------------
                  CALL RSNOCH(LERES1,OPTION,IORDR2,' ')
               ENDIF
C
C --- B/ TRAITEMENT DE L'OPTION ENDO_ELNO_ELGA
C     ----------------------------------------
               IF (OPTION.EQ.'ENDO_ELNO_ELGA') THEN
                  CALL RSEXC2(1,1,RESUCO,'ENDO_ELGA',IORDR2,CHEND2,
     &                        OPTION,IRET1)
                  IF (IRET1.GT.0) THEN
                     CALL U2MESS('F','CALCULEL3_21')
                  ENDIF
                  CALL RSEXC1(LERES1,OPTION,IORDR2,CHELE2)
                  CALL ENDOLE(OPTION,MODELE,LIGREL,MATE,COMPOR,
     &                        CHVARC,CHSIG1,CHVAR1,
     &                        CHVAC2,CHSIG2,CHVAR2,
     &                        CHEND2,CHELE1,CHELE2)
                  CALL RSNOCH(LERES1,OPTION,IORDR2,' ')
               ENDIF

  341       CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION "SIGM_ELNO_COQU"
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'SIGM_ELNO_COQU') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 360,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                    IRET1)
              IF (IRET1.GT.0) GO TO 362
              CALL RSEXCH(RESUCO,'DEPL',IORDR,CHDEPL,IRET1)
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
              ELSE
                TIME = ZERO
              END IF
              CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC)
              CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
              CALL MECALC(OPTION,MODELE,K24B,CHGEOM,MATE,CHCARA,K24B,
     &                    K24B,K24B,CHNUMC,K24B,CHSIG,CHDEPL,K24B,
     &                    K24B,K24B,K24B,TYPCOE,ALPHA,CALPHA,K24B,SOP,
     &                    CHELEM,LIGREL,BASE,CHVARC,CHVREF,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 362
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  362         CONTINUE
              CALL JEDEMA()
  360       CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION "SIGM_ELNO_TUYO"
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'SIGM_ELNO_TUYO') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 370,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXCH(RESUCO,'SIEF_ELGA',IORDR,CHSIG,IRET1)
              IF (IRET1.GT.0) THEN
                CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA_DEPL',IORDR,CHSIG,
     &                      OPTION,IRET2)
                IF (IRET2.GT.0) GO TO 372
              END IF
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)

              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
              ELSE
                TIME = ZERO
              END IF
              CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC)
              CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    K24B,K24B,K24B,CHNUMC,K24B,CHSIG,K24B,
     &                    K24B,K24B,K24B,K24B,TYPCOE,ALPHA,CALPHA,K24B,
     &                    SOP,CHELEM,LIGREL,BASE,CHVARC,CHVREF,K24B,
     &                    COMPOR,CHTESE,CHDESE,NOPASE,TYPESE,
     &                    CHACSE,IRET)
              IF (IRET.GT.0) GO TO 372
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  372         CONTINUE
              CALL JEDEMA()
  370       CONTINUE
C     ------------------------------------------------------------------
C     --- OPTION "EPSI_ELNO_TUYO"
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'EPSI_ELNO_TUYO') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 380,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'EPSI_ELGA_DEPL',IORDR,CHEPS,
     &                      OPTION,IRET2)
              IF (IRET2.GT.0) GO TO 382
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
              ELSE
                TIME = ZERO
              END IF
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    CHTEMP,K24B,K24B,CHNUMC,K24B,K24B,CHEPS,
     &                    K24B,K24B,K24B,K24B,TYPCOE,ALPHA,CALPHA,K24B,
     &                    SOP,CHELEM,LIGREL,BASE,CHVARC,CHVREF,K24B,
     &                    COMPOR,CHTESE,CHDESE,NOPASE,TYPESE,
     &                    CHACSE,IRET)
              IF (IRET.GT.0) GO TO 382
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  382         CONTINUE
              CALL JEDEMA()
  380       CONTINUE
C     ------------------------------------------------------------------
C     --- OPTION "EPEQ_ELNO_TUYO"
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'EPEQ_ELNO_TUYO') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 390,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'EQUI_ELGA_EPSI',IORDR,CHEEQ,
     &                      OPTION,IRET2)
              IF (IRET2.GT.0) GO TO 392
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
              ELSE
                TIME = ZERO
              END IF
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    CHTEMP,K24B,K24B,CHNUMC,K24B,K24B,K24B,
     &                    K24B,K24B,K24B,K24B,TYPCOE,ALPHA,CALPHA,K24B,
     &                    SOP,CHELEM,LIGREL,BASE,CHVARC,CHEEQ,K24B,
     &                    COMPOR,CHTESE,CHDESE,NOPASE,TYPESE,
     &                    CHACSE,IRET)
              IF (IRET.GT.0) GO TO 392
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  392         CONTINUE
              CALL JEDEMA()
  390       CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION "SIEQ_ELNO_TUYO"
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'SIEQ_ELNO_TUYO') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 400,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'EQUI_ELGA_SIGM',IORDR,CHSEQ,
     &                      OPTION,IRET2)
              IF (IRET2.GT.0) GO TO 402
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
              ELSE
                TIME = ZERO
              END IF
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    CHTEMP,K24B,K24B,CHNUMC,K24B,K24B,K24B,
     &                    K24B,K24B,K24B,K24B,TYPCOE,ALPHA,CALPHA,K24B,
     &                    SOP,CHELEM,LIGREL,BASE,CHSEQ,K24B,K24B,
     &                    COMPOR,CHTESE,CHDESE,NOPASE,TYPESE,
     &                    CHACSE,IRET)
              IF (IRET.GT.0) GO TO 402
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  402         CONTINUE
              CALL JEDEMA()
  400      CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION: INDI_LOCA_ELGA
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'INDI_LOCA_ELGA')  THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 1410,IAUX = 1,NBORDR
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
          CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR,CHSIG2,OPTION,IRET)
          CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR,CHVAR2,OPTION,IRET)
              IF (IRET.GT.0) GO TO 1410
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECALC(OPTION,MODELE,CHVAR2,K24B,MATE,K24B,K24B,
     &                    K24B,K24B,K24B,K24B,CHSIG2,K24B,K24B,K24B,
     &                    K24B,K24B,K24B,ZERO,CZERO,K24B,SOP,CHELEM,
     &                    LIGREL,BASE,K24B,K24B,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 1410
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
 1410      CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION " VARI_ELNO_ELGA"
C     ------------------------------------------------------------------
          ELSE IF ((OPTION.EQ.'VARI_ELNO_ELGA') .OR.
     &             (OPTION.EQ.'VARI_ELNO_COQU')) THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.EQ.4) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 410,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,LERES0,'VARI_ELGA',IORDR,CHAMGD,OPTION,
     &                    IRET)
              IF (IRET.GT.0) GO TO 412
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    K24B,K24B,K24B,CHNUMC,K24B,K24B,K24B,K24B,
     &                    K24B,K24B,K24B,K24B,ZERO,CZERO,K24B,SOP,
     &                    CHELEM,LIGREL,BASE,K24B,K24B,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 412
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  412         CONTINUE
              CALL JEDEMA()
  410      CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION "VARI_ELNO_TUYO"
C     ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'VARI_ELNO_TUYO') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            K24B = ' '
            DO 420,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR,CHAMGD,OPTION,
     &                    IRET)

              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    K24B,K24B,K24B,CHNUMC,K24B,K24B,K24B,K24B,
     &                    K24B,K24B,K24B,K24B,ZERO,CZERO,K24B,SOP,
     &                    CHELEM,LIGREL,BASE,K24B,K24B,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 422
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  422         CONTINUE
              CALL JEDEMA()
  420       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTIONS "ENEL_ELGA" ET "ENEL_ELNO_ELGA"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'ENEL_ELGA' .OR.
     &             OPTION.EQ.'ENEL_ELNO_ELGA') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 430,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                    IRET)
              CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA_DEPL',IORDR,CHSIG,
     &                    OPTION,IRET)
              IF (IRET.GT.0) THEN
                CALL U2MESK('A','CALCULEL3_18',1,OPTION)
                GO TO 432
              END IF
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              IF (TYSD.EQ.'FOURIER_ELAS') THEN
                CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
                CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
              END IF
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
                TIME = ZR(IAINST)
                CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
              ELSE
                CHTIME = ' '
                TIME = ZERO
              END IF
              CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC)
              CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))

              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    CHTEMP,K24B,CHTIME,CHNUMC,CHHARM,CHSIG,
     &                    CHEPS,CHFREQ,CHMASS,CHMETA,ZK8(JCHA),' ',ZERO,
     &                    CZERO,CHDYNR,SOP,CHELEM,LIGREL,BASE,CHVARC,
     &                    CHVREF,K24B,COMPOR,CHTESE,CHDESE,NOPASE,
     &                    TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 432
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  432         CONTINUE
              CALL JEDEMA()
  430       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "DEDE_ELNO_DLDE"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'DEDE_ELNO_DLDE') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.-1) THEN
               CODSEN = 2
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 450,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              K4BID = 'DEPL'
              CALL RSEXC2(1,1,RESUCO,K4BID,IORDR,CHAMGD,OPTION,IRET)
              IF (IRET.GT.0) GO TO 452
              CALL RSEXC2(1,1,LERES0,K4BID,IORDR,K24B,OPTION,IRET)
              IF (IRET.GT.0) GO TO 452
              CHDESE = K24B
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    K24B,K24B,K24B,K24B,K24B,K24B,CHTETA,K24B,
     &                    K24B,K24B,ZK8(JCHA),' ',ZERO,CZERO,K24B,K24B,
     &                    CHELEM,LIGREL,BASE,K24B,K24B,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 452
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  452         CONTINUE
              CALL JEDEMA()
  450       CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "DESI_ELNO_DLSI"
C    ------------------------------------------------------------------
          ELSE IF (OPTION.EQ.'DESI_ELNO_DLSI') THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.-1) THEN
               CODSEN = 2
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 460,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL RSEXC2(1,1,LERES0,'SIEF_ELGA_DEPL',IORDR,DLAGSI,
     &                    OPTION,IRET)
              IF (IRET.GT.0) GO TO 462
              CALL RSEXC2(1,1,RESUCO,'SIGM_ELNO_DEPL',IORDR,CHSIGM,
     &                    OPTION,IRET)
              IF (IRET.GT.0) GO TO 462
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECALC(OPTION,MODELE,DLAGSI,CHGEOM,MATE,CHCARA,
     &                    K24B,K24B,K24B,K24B,K24B,CHSIGM,CHTETA,K24B,
     &                    K24B,K24B,ZK8(JCHA),' ',ZERO,CZERO,K24B,K24B,
     &                    CHELEM,LIGREL,BASE,K24B,K24B,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 462
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  462         CONTINUE
              CALL JEDEMA()
  460       CONTINUE

C    ------------------------------------------------------------------
C    -- OPTION "EXTR_ELGA_VARI"
C    ------------------------------------------------------------------

          ELSE IF (OPTION.EQ.'EXTR_ELGA_VARI' ) THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 470,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR,CHAMGD,OPTION,
     &                    IRET)
              IF (IRET.GT.0) GO TO 472
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    K24B,K24B,K24B,CHNUMC,K24B,K24B,K24B,K24B,
     &                    K24B,K24B,K24B,K24B,ZERO,CZERO,K24B,SOP,
     &                    CHELEM,LIGREL,BASE,K24B,K24B,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 472
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  472         CONTINUE
              CALL JEDEMA()
  470      CONTINUE
C      -----------------------------------------------------------------
C    ------------------------------------------------------------------
C    -- OPTION "EXTR_ELNO_VARI"
C    ------------------------------------------------------------------

          ELSE IF (OPTION.EQ.'EXTR_ELNO_VARI' ) THEN
C ---- VERIF SENSIBILITE
            IF (TYPESE.NE.0) THEN
               CODSEN = 1
            ENDIF
            IF(CODSEN.NE.0) GO TO 900
C ---- VERIF SENSIBILITE FIN
            DO 480,IAUX = 1,NBORDR
              CALL JEMARQ()
              CALL JERECU('V')
              IORDR = ZI(JORDR+IAUX-1)
              CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,
     &                    RESUCO,IORDR,NBORDR,NPASS,LIGREL)
              CALL JEVEUO(KCHA,'L',JCHA)
              CALL MECARA(CARA,EXICAR,CHCARA)
              CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
              CALL RSEXC2(1,1,RESUCO,'VARI_ELNO_ELGA',IORDR,CHAMGD,
     &                    OPTION,IRET)
              IF (IRET.GT.0) GO TO 482
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
              CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
              CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,
     &                    K24B,K24B,K24B,CHNUMC,K24B,K24B,K24B,K24B,
     &                    K24B,K24B,K24B,K24B,ZERO,CZERO,K24B,SOP,
     &                    CHELEM,LIGREL,BASE,K24B,K24B,K24B,COMPOR,
     &                    CHTESE,CHDESE,NOPASE,TYPESE,CHACSE,IRET)
              IF (IRET.GT.0) GO TO 482
              CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  482         CONTINUE
              CALL JEDEMA()
  480       CONTINUE
C      -----------------------------------------------------------------

          ELSE
            CALL U2MESK('A','CALCULEL3_22',1,OPTION)
          ENDIF

C     ------------------------------------------------------------------
C     -- ERREUR SENSIBILITE
C     ------------------------------------------------------------------
  900     CONTINUE
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
C============= FIN DE LA BOUCLE SUR LES OPTIONS A CALCULER =============
        IF (NEWCAL) THEN
          DO 475,IAUX = 1,NRPASS - 1
            IF (ZK24(ADCRRS+IAUX-1)(1:19).EQ.LERES1) THEN
              GO TO 490
            END IF
  475     CONTINUE
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
  490 CONTINUE
C============= FIN DE LA BOUCLE SUR LE NOMBRE DE PASSAGES ==============
      GO TO 530
  520 CONTINUE
        VALKM(1)=TYSD
        VALKM(2)=OPTION
        CALL U2MESK('A','CALCULEL3_27', 2 ,VALKM)
  530 CONTINUE
      CALL JEDEMA()
      END
