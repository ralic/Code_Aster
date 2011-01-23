      SUBROUTINE MECALM(NEWCAL,TYSD,KNUM,KCHA,RESUCO,RESUC1,CONCEP,
     &                  NBORDR,MODELE,MATE,CARA,NCHAR,CTYP)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 25/01/2011   AUTEUR PELLET J.PELLET 
C TOLE CRP_20
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

      INTEGER NBORDR,NCHAR
      CHARACTER*4 CTYP
      CHARACTER*8 RESUCO,RESUC1,MODELE,CARA
      CHARACTER*16 TYSD,CONCEP
      CHARACTER*19 KNUM,KCHA
      CHARACTER*24 MATE
      LOGICAL NEWCAL
C
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
      CHARACTER*32 ZK32,JEXNOM
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C     --- VARIABLES LOCALES ---

      CHARACTER*2 CODRET
      CHARACTER*6 NOMPRO
      PARAMETER(NOMPRO='MECALM')

      INTEGER IFM,NIV
      INTEGER LINST,NUORD
      INTEGER LREFE,LVALE,LDEPL,LFREQ,LACCE
      INTEGER IORDR,IORDR1,IORDR2,JORDR,IORDRM
      INTEGER IRET,IRET1,IRET2,IRET3,IRET4,IRET5,IERD,IRETER
      INTEGER NH,NOR,NBOPT,NEQ,NBCHRE,IER,IFISS
      INTEGER IADOU,IADIN,IPUIS
      INTEGER IAUX,II,III,IB,J,K,IBID,IE
      INTEGER IOCC,IOPT,IAINST
      INTEGER L1,L2,L3,L4,L5,L6
      INTEGER N1
      INTEGER JPA,JOPT,JCHA,JNMO
      INTEGER NBAC,NBPA,NBPARA
      INTEGER NBVAL
      INTEGER INUME
      INTEGER IINST1,IINST2
      INTEGER JDIM,JCOOR,JTYPE,LTYMO,NPASS
      INTEGER NNOEM,NELEM,NDIM,NNCP

      CHARACTER*1 BASE,TYPCOE
      CHARACTER*4 TYPE
      CHARACTER*8 K8B,NOMA,CHAREP
      CHARACTER*8 CARELE,Z1Z2(2),KIORD,KIORDM
      CHARACTER*8 LERES0,BLAN8
      CHARACTER*8 NOMCMP,SAVCAR(2)
      CHARACTER*19 PFCHNO
      CHARACTER*16 NOMCMD,OPTION,OPTIO2,OPTIOX,NOMCHA,TYPES,K16B
      CHARACTER*16 BLAN16,TYPEMO
      CHARACTER*19 LERES1
      CHARACTER*19 CHDYNR,CHACCE,MASSE,REFE,COMPOR
      CHARACTER*19 CHERRS,CHENES,CHSINS,CHSINN
      CHARACTER*24 CHENEG,CHSING,CHERR1,CHERR2,CHERR3,CHERR4
      CHARACTER*24 CHAMGD,CHSIG,CHSIGN,CHEPSP
      CHARACTER*24 CHEPS,CHDEPL,CHACSE
      CHARACTER*24 CHGEOM,CHCARA(18),CHTEMP,CHTIME,CHMETA
      CHARACTER*24 CHNUMC,CHHARM,CHFREQ,CHMASS,CHELEM,SOP
      CHARACTER*24 LIGREL,CHEPSA,K24B
      CHARACTER*24 CHSIG1,CHSIG2,CHVAR1,CHVAR2,NORME,NOMPAR
      CHARACTER*24 LESOPT
      CHARACTER*24 CHTESE,CHSIGM,CHDESE,CHSIC
      CHARACTER*24 CHVARI,CHDEPM
      CHARACTER*24 NOMS(2)
      CHARACTER*24 LIGRMO
      CHARACTER*24 BLAN24,CHBID,CHSEQ,CHEEQ,CHCMP
      CHARACTER*24 CHTIM1,CHELE1
      CHARACTER*24 CHTIM2,CHELE2,CHELEX
      CHARACTER*24 CHEND2,CHSIGF
      CHARACTER*19 CHVARC,CHVREF,CHVAC2

      REAL*8 COEF,VALRES,VALIM,INST,TIME,R8B
      REAL*8 ALPHA,PREC,PHASE,FREQ,OMEGA
      REAL*8 R8DEPI,R8DGRD,RUNDF,R8VIDE
      REAL*8 RBID
      REAL*8 TIME1,TIME2
      REAL*8 TBGRCA(3)

      COMPLEX*16 CALPHA,CCOEF,CBID

      LOGICAL EXITIM,EXIPOU,EXIPLA,EXICAR
      REAL*8 ZERO,UN
      PARAMETER(ZERO=0.D0,UN=1.D0)

      COMPLEX*16 CZERO
      PARAMETER(CZERO=(0.D0,0.D0))

      CHARACTER*24 VALKM(2)


      CALL JEMARQ()
      CALL GETRES(K8B,K16B,NOMCMD)
      CALL JERECU('V')

      BLAN16=' '
      BLAN24=' '
      LESOPT='&&'//NOMPRO//'.LES_OPTION'
      NH=0
      CHAMGD=BLAN24
      CHGEOM=BLAN24
      CHTEMP=BLAN24
      CHTIME=BLAN24
      CHNUMC=BLAN24
      CHHARM=BLAN24
      CHSIG=BLAN24
      CHSIC=BLAN24
      CHEPS=BLAN24
      CHFREQ=BLAN24
      CHMASS=BLAN24
      CHMETA=BLAN24
      CHAREP=' '
      CHDYNR=' '
      CHDEPL=BLAN24
      CHELEM=BLAN24
      CHELEX=BLAN24
      SOP=BLAN24
      K24B=BLAN24
      CHVARC='&&'//NOMPRO//'.CHVARC'
      CHVAC2='&&'//NOMPRO//'.CHVAC2'
      CHVREF='&&'//NOMPRO//'.CHVREF'
      CHVARI=BLAN24
      BASE='G'
      COEF=UN
      SAVCAR(1)='????????'
      SAVCAR(2)='????????'
      RUNDF=R8VIDE()

C     COMPTEUR DE PASSAGES DANS LA COMMANDE (POUR MEDOM2.F)
      NPASS=0

      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
      CARELE=' '
      CALL GETVID(' ','CARA_ELEM',1,1,1,CARELE,N1)

      CALL MODOPT(RESUCO,LESOPT,NBOPT)
      CALL JEVEUO(LESOPT,'L',JOPT)

C     ON RECUPERE LE TYPE DE MODE: DYNAMIQUE OU STATIQUE
      TYPEMO=BLAN16
      IF (TYSD.EQ.'MODE_MECA') THEN
        CALL RSADPA(RESUCO,'L',1,'TYPE_MODE',1,0,LTYMO,K8B)
        TYPEMO=ZK16(LTYMO)
      ENDIF

      CALL GETVTX(' ','NORME',1,1,1,NORME,NOR)
      CALL JEVEUO(KNUM,'L',JORDR)
      NUORD=ZI(JORDR)
      CALL JEVEUO(KCHA,'L',JCHA)
      IF (NEWCAL) THEN
        CALL RSCRSD('G',RESUC1,TYSD,NBORDR)
        CALL TITRE
      ENDIF
      CALL DISMOI('F','NOM_LIGREL',MODELE,'MODELE',IBID,LIGRMO,IERD)
      EXITIM=.FALSE.
      CALL JENONU(JEXNOM(RESUCO//'           .NOVA','INST'),IRET)
      IF (IRET.NE.0)EXITIM=.TRUE.
      CALL EXLIMA(' ',0,'V',MODELE,LIGREL)

      EXIPOU=.FALSE.
      CALL DISMOI('F','EXI_POUX',LIGREL,'LIGREL',IBID,K8B,IERD)
      IF (K8B(1:3).EQ.'OUI')EXIPOU=.TRUE.

C=======================================================================
C                   SPECIAL POUTRE A LA POUX (1)
C=======================================================================
      IF (EXIPOU) THEN
C-------ON VERIFIE SI DERIERE LE CONCEPT MODE_MECA ON TROUVE UN MODE_DYN
C        IF (CONCEP.EQ.'MODE_MECA' .OR. CONCEP.EQ.'DYNA_TRANS' .OR.
C     &      CONCEP.EQ.'MODE_ACOU' .OR. CONCEP.EQ.'DYNA_HARMO') THEN
        IF ((CONCEP.EQ.'MODE_MECA'.AND.TYPEMO(1:8).EQ.'MODE_DYN') .OR.
     &      CONCEP.EQ.'DYNA_TRANS' .OR. CONCEP.EQ.'MODE_ACOU' .OR.
     &      CONCEP.EQ.'DYNA_HARMO') THEN
          REFE=RESUCO
          SOP='MASS_MECA'
          INUME=1
          CALL RSEXCH(RESUCO,'DEPL',1,CHDEPL,IRET)
          CALL JELIRA(CHDEPL(1:19)//'.VALE','LONMAX',NEQ,K8B)
          CALL JEVEUO(REFE//'.REFD','L',LREFE)
          MASSE=ZK24(LREFE+1)(1:19)
          IF (MASSE(1:8).NE.'        ') THEN
            CALL DISMOI('C','SUR_OPTION',MASSE,'MATR_ASSE',IBID,SOP,IE)
            IF (SOP(1:14).EQ.'MASS_MECA_DIAG')INUME=0
          ENDIF
          CHDYNR='&&'//NOMPRO//'.M.GAMMA'
          IF (CONCEP.EQ.'MODE_MECA' .OR. CONCEP.EQ.'DYNA_TRANS' .OR.
     &        CONCEP.EQ.'MODE_ACOU') THEN
            CALL COPICH('V',CHDEPL(1:19),CHDYNR)
          ELSE
            CALL COPICH('V',CHDEPL(1:19),CHDYNR)
          ENDIF
          CALL JELIRA(CHDYNR//'.VALE','LONMAX',NEQ,K8B)
          CALL JEVEUO(CHDYNR//'.VALE','E',LVALE)
        ENDIF
C --- VERIFIE L'UNICITE DE LA CHARGE REPARTIE
        IOCC=0
        CALL COCHRE(ZK8(JCHA),NCHAR,NBCHRE,IOCC)
        IF (NBCHRE.GT.1) THEN
          CALL U2MESS('A','CALCULEL2_92')
          GOTO 690

        ENDIF
        DO 10 III=1,NCHAR
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
            ENDIF
          ENDIF
   10   CONTINUE
      ENDIF
C=======================================================================
C     ON VERIFIE QUE CARA_ELEM, NIVE_COUCHE ET NUME_COUCHE ONT ETE
C     RENSEIGNES POUR LES COQUES
C=======================================================================
      EXIPLA=.FALSE.
      CALL DISMOI('F','EXI_COQ1D',MODELE,'MODELE',IBID,K8B,IERD)
      IF (K8B(1:3).EQ.'OUI')EXIPLA=.TRUE.
      CALL DISMOI('F','EXI_COQ3D',MODELE,'MODELE',IBID,K8B,IERD)
      IF (K8B(1:3).EQ.'OUI')EXIPLA=.TRUE.
      CALL DISMOI('F','EXI_PLAQUE',MODELE,'MODELE',IBID,K8B,IERD)
      IF (K8B(1:3).EQ.'OUI')EXIPLA=.TRUE.
      IF (EXIPLA) THEN
        CALL GETVID(' ','CARA_ELEM',1,1,1,K8B,N1)
        IF (N1.EQ.0 .AND. CARA.EQ.' ') THEN
          CALL U2MESS('A','CALCULEL2_94')
          GOTO 690

        ENDIF
      ENDIF
C=======================================================================
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,NOMA,IERD)
      CHNUMC='&&'//NOMPRO//'.NUMC'
      CHFREQ='&&'//NOMPRO//'.FREQ'
      CALL MECHN2(NOMA,CHNUMC,CHFREQ)
C=======================================================================
C
C -- GRANDEURS CARACTERISTIQUES DE L'ETUDE
C
      CALL CETULE(MODELE,TBGRCA,IRET)
C=======================================================================


      BLAN8=' '
      LERES1=RESUC1
      LERES0=RESUCO

C    ------------------------------------------------------------------
C    -- RECOPIE DES PARAMETRES DANS LA NOUVELLE SD RESULTAT
C    ------------------------------------------------------------------

      IF (NEWCAL) THEN
        NOMPAR='&&'//NOMPRO//'.NOMS_PARA '
        CALL RSNOPA(RESUCO,2,NOMPAR,NBAC,NBPA)
        NBPARA=NBAC+NBPA
        CALL JEVEUO(NOMPAR,'L',JPA)
        DO 30,IAUX=1,NBORDR
          IORDR=ZI(JORDR+IAUX-1)
          DO 20 J=1,NBPARA
            CALL RSADPA(RESUCO,'L',1,ZK16(JPA+J-1),IORDR,1,IADIN,TYPE)
            CALL RSADPA(LERES1,'E',1,ZK16(JPA+J-1),IORDR,1,IADOU,TYPE)
            IF (TYPE(1:1).EQ.'I') THEN
              ZI(IADOU)=ZI(IADIN)
            ELSEIF (TYPE(1:1).EQ.'R') THEN
              ZR(IADOU)=ZR(IADIN)
            ELSEIF (TYPE(1:1).EQ.'C') THEN
              ZC(IADOU)=ZC(IADIN)
            ELSEIF (TYPE(1:3).EQ.'K80') THEN
              ZK80(IADOU)=ZK80(IADIN)
            ELSEIF (TYPE(1:3).EQ.'K32') THEN
              ZK32(IADOU)=ZK32(IADIN)
            ELSEIF (TYPE(1:3).EQ.'K24') THEN
              ZK24(IADOU)=ZK24(IADIN)
            ELSEIF (TYPE(1:3).EQ.'K16') THEN
              ZK16(IADOU)=ZK16(IADIN)
            ELSEIF (TYPE(1:2).EQ.'K8') THEN
              ZK8(IADOU)=ZK8(IADIN)
            ENDIF
   20     CONTINUE
   30   CONTINUE
      ENDIF

C    ------------------------------------------------------------------
C    -- FIN RECOPIE DES PARAMETRES DANS LA NOUVELLE SD RESULTAT
C    ------------------------------------------------------------------



C============ DEBUT DE LA BOUCLE SUR LES OPTIONS A CALCULER ============
      DO 660 IOPT=1,NBOPT
C
        OPTION=ZK16(JOPT+IOPT-1)
C
        CALL JEVEUO(KNUM,'L',JORDR)

C         PASSAGE CALC_CHAMP
        CALL CALCOP(OPTION,RESUCO,RESUC1,NBORDR,ZI(JORDR),KCHA,NCHAR,
     &              CTYP,TYSD,NBCHRE,IOCC,SOP,IRET)
        IF (IRET.EQ.0)GOTO 660

        NUORD=ZI(JORDR)
        CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,NUORD,
     &              NBORDR,NPASS,LIGREL)
        CALL JEVEUO(KCHA,'L',JCHA)
C
        CALL MECHAM(OPTION,MODELE,NCHAR,ZK8(JCHA),CARA,NH,CHGEOM,CHCARA,
     &              CHHARM,IRET)
        IF (IRET.NE.0)GOTO 690

C    ------------------------------------------------------------------
C    -- OPTIONS "SIGM_ELNO_DEPL","SIEF_ELGA","EPSI_ELNO_DEPL",
C               "EPSI_ELGA","EPSG_ELNO","EPSG_ELGA",
C               "EPME_ELNO","EPME_ELGA","EPMG_ELNO",
C               "EPMG_ELGA","EFGE_ELNO_DEPL","EPOT_ELEM",
C               "SIPO_ELNO","DEGE_ELNO",
C               "EPVC_ELGA","EPVC_ELNO"
C    ------------------------------------------------------------------
        IF (OPTION.EQ.'SIGM_ELNO_DEPL' .OR.
     &      OPTION.EQ.'SIEF_ELGA' .OR.
     &      OPTION.EQ.'EPSI_ELNO_DEPL' .OR.
     &      OPTION.EQ.'EPSI_ELGA' .OR.
     &      OPTION.EQ.'EPSG_ELNO' .OR.
     &      OPTION.EQ.'EPSG_ELGA' .OR.
     &      OPTION.EQ.'EPME_ELNO' .OR.
     &      OPTION.EQ.'EPME_ELGA' .OR. OPTION.EQ.'EPVC_ELNO' .OR.
     &      OPTION.EQ.'EPVC_ELGA' .OR. OPTION.EQ.'EPMG_ELNO' .OR.
     &      OPTION.EQ.'EPMG_ELGA' .OR.
     &      OPTION.EQ.'EFGE_ELNO_DEPL' .OR.
     &      OPTION.EQ.'EPOT_ELEM' .OR.
     &      OPTION.EQ.'SIPO_ELNO' .OR.
     &      OPTION.EQ.'DEGE_ELNO' .OR. OPTION.EQ.'FLHN_ELGA') THEN


C ---- TRAITEMENT DE L EXCENTREMENT POUR OPTIONS DE POST TRAITEMENT
          IF (NCHAR.NE.0 .AND. CTYP.NE.'MECA') THEN
            CALL U2MESS('A','CALCULEL2_98')
            GOTO 660

          ENDIF

          IF (CONCEP.EQ.'DYNA_HARMO') THEN
            IF ((OPTION.EQ.'SIGM_ELNO_DEPL') .OR.
     &          (OPTION.EQ.'SIPO_ELNO') .OR.
     &          (OPTION.EQ.'EFGE_ELNO_DEPL') .OR.
     &          (OPTION.EQ.'SIEF_ELGA') .OR.
     &          (OPTION.EQ.'SIEF_ELNO') .OR.
     &          (OPTION.EQ.'EPSI_ELGA') .OR.
     &          (OPTION.EQ.'EPSI_ELNO_DEPL') .OR.
     &          (OPTION.EQ.'EPOT_ELEM') .OR.
     &          (OPTION.EQ.'ECIN_ELEM') .OR.
     &          (OPTION.EQ.'ENEL_ELGA') .OR.
     &          (OPTION.EQ.'ENEL_ELNO')) THEN
            ELSE
              GOTO 680

            ENDIF

          ELSEIF (CONCEP.EQ.'EVOL_NOLI') THEN
            IF (OPTION.EQ.'SIGM_ELNO_DEPL' .OR.
     &          OPTION.EQ.'SIPO_ELNO' .OR.
     &          OPTION.EQ.'SIEF_ELGA' .OR.
     &          OPTION.EQ.'EFGE_ELNO_DEPL') THEN
              CALL U2MESK('A','CALCULEL2_99',1,OPTION)
              GOTO 660

            ENDIF
          ENDIF

          OPTIO2=OPTION


          DO 100,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
            IF (IRET.GT.0)GOTO 90
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
C=======================================================================
C                   SPECIAL POUTRE A LA POUX (2)
C=======================================================================
            CHAREP=' '
            TYPCOE=' '
            ALPHA=ZERO
            CALPHA=CZERO
            IF (EXIPOU) THEN
C                IF (TYSD.EQ.'MODE_MECA' .OR. TYSD.EQ.'MODE_ACOU') THEN
              IF ((TYSD.EQ.'MODE_MECA'.AND.TYPEMO(1:
     &            8).EQ.'MODE_DYN') .OR. TYSD.EQ.'MODE_ACOU') THEN
                CALL JEVEUO(CHAMGD(1:19)//'.VALE','L',LDEPL)
                CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORDR,0,LFREQ,K8B)
                DO 40 II=0,NEQ-1
                  ZR(LVALE+II)=-ZR(LFREQ)*ZR(LDEPL+II)
   40           CONTINUE
                CALL JELIBE(CHAMGD(1:19)//'.VALE')
              ELSEIF (TYSD.EQ.'DYNA_TRANS') THEN
                CALL RSEXCH(RESUCO,'ACCE',IORDR,CHACCE,IRET)
                IF (IRET.EQ.0) THEN
                  CALL JEVEUO(CHACCE//'.VALE','L',LACCE)
                  DO 50 II=0,NEQ-1
                    ZR(LVALE+II)=ZR(LACCE+II)
   50             CONTINUE
                  CALL JELIBE(CHACCE//'.VALE')
                ELSE
                  CALL U2MESS('A','CALCULEL3_1')
                  DO 60 II=0,NEQ-1
                    ZR(LVALE+II)=ZERO
   60             CONTINUE
                ENDIF
              ELSEIF (TYSD.EQ.'DYNA_HARMO') THEN
                CALL RSEXCH(RESUCO,'ACCE',IORDR,CHACCE,IRET)
                IF (IRET.EQ.0) THEN
                  CALL JEVEUO(CHACCE//'.VALE','L',LACCE)
                  DO 70 II=0,NEQ-1
                    ZC(LVALE+II)=ZC(LACCE+II)
   70             CONTINUE
                  CALL JELIBE(CHACCE//'.VALE')
                ELSE
                  CALL U2MESS('A','CALCULEL3_1')
                  DO 80 II=0,NEQ-1
                    ZC(LVALE+II)=CZERO
   80             CONTINUE
                ENDIF
              ENDIF
C --- CALCUL DU COEFFICIENT MULTIPLICATIF DE LA CHARGE
C     CE CALCUL N'EST EFFECTIF QUE POUR LES CONDITIONS SUIVANTES
C          * MODELISATION POUTRE
C          * PRESENCE D'UNE (ET D'UNE SEULE) CHARGE REPARTIE
C          * UTILISATION DU MOT-CLE FACTEUR EXCIT
              IF (NBCHRE.NE.0) THEN
                PHASE=ZERO
                IPUIS=0
                CALL GETVID('EXCIT','FONC_MULT',IOCC,1,1,K8B,L1)
                CALL GETVID('EXCIT','FONC_MULT_C',IOCC,1,1,K8B,L2)
                CALL GETVR8('EXCIT','COEF_MULT',IOCC,1,1,COEF,L3)
                CALL GETVC8('EXCIT','COEF_MULT_C',IOCC,1,1,CCOEF,L4)
                CALL GETVR8('EXCIT','PHAS_DEG',IOCC,1,1,PHASE,L5)
                CALL GETVIS('EXCIT','PUIS_PULS',IOCC,1,1,IPUIS,L6)
                IF (L1.NE.0 .OR. L2.NE.0 .OR. L3.NE.0 .OR. L4.NE.0 .OR.
     &              L5.NE.0 .OR. L6.NE.0) THEN
                  IF (TYSD.EQ.'DYNA_HARMO') THEN
                    TYPCOE='C'
                    CALL RSADPA(RESUCO,'L',1,'FREQ',IORDR,0,LFREQ,K8B)
                    FREQ=ZR(LFREQ)
                    OMEGA=R8DEPI()*FREQ
                    IF (L1.NE.0) THEN
                      CALL FOINTE('F ',K8B,1,'FREQ',FREQ,VALRES,IER)
                      CALPHA=DCMPLX(VALRES,ZERO)
                    ELSEIF (L2.NE.0) THEN
                      CALL FOINTC('F',K8B,1,'FREQ',FREQ,VALRES,VALIM,
     &                            IER)
                      CALPHA=DCMPLX(VALRES,VALIM)
                    ELSEIF (L3.NE.0) THEN
                      CALPHA=DCMPLX(COEF,UN)
                    ELSEIF (L4.NE.0) THEN
                      CALPHA=CCOEF
                    ENDIF
                    IF (L5.NE.0) THEN
                      CALPHA=CALPHA*EXP(DCMPLX(ZERO,PHASE*R8DGRD()))
                    ENDIF
                    IF (L6.NE.0) THEN
                      CALPHA=CALPHA*OMEGA**IPUIS
                    ENDIF
                  ELSEIF (TYSD.EQ.'DYNA_TRANS') THEN
                    TYPCOE='R'
                    CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,LINST,K8B)
                    INST=ZR(LINST)
                    IF (L1.NE.0) THEN
                      CALL FOINTE('F ',K8B,1,'INST',INST,ALPHA,IER)
                    ELSEIF (L3.NE.0) THEN
                      ALPHA=COEF
                    ELSE
                      CALL U2MESS('A','CALCULEL3_2')
                      CALL JEDEMA
                      GOTO 660

                    ENDIF
                  ELSEIF (TYSD.EQ.'EVOL_ELAS') THEN
                    TYPCOE='R'
                    IF (L1.NE.0) THEN
                      CALL FOINTE('F ',K8B,1,'INST',INST,ALPHA,IER)
                    ELSE
                      CALL U2MESS('A','CALCULEL3_3')
                      CALL JEDEMA
                      GOTO 660

                    ENDIF
                  ELSE
                    CALL U2MESS('A','CALCULEL3_4')
                    CALL JEDEMA
                    GOTO 660

                  ENDIF
                ENDIF
              ENDIF
              IF (IOCC.GT.0) THEN
                CALL GETVID('EXCIT','CHARGE',IOCC,1,1,CHAREP,N1)
                IF (N1.EQ.0)CHAREP=ZK8(JCHA-1+IOCC)
              ENDIF
            ENDIF
C=======================================================================
            IF (TYSD.EQ.'FOURIER_ELAS' .OR. TYSD.EQ.'COMB_FOURIER') THEN
              CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
              CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
            ENDIF
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
              CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
            ELSE
              CHTIME=' '
              TIME=ZERO
            ENDIF

            CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)
            CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
C -- POUR LES POUTRES MULTIFIBRES ON A BESOIN DE COMPOR ISSU DE MATERIAU
C     POUR LE CALCUL DES OPTIONS SIEF_ELGA ET EFGE_ELNO_DEPL
            IF (OPTIO2.EQ.'EFGE_ELNO_DEPL' .OR.
     &          OPTIO2.EQ.'SIEF_ELGA') THEN
              COMPOR=MATE(1:8)//'.COMPOR'
            ENDIF
            IF (OPTION.EQ.'FLHN_ELGA') THEN
              CALL RSEXC2(1,1,RESUCO,'SIEF_NOEU',IORDR,CHSIG,
     &                    OPTION,IRET)
              IF (IRET.GT.0) THEN
                CALL U2MESK('F','CALCULEL7_1',1,OPTION)
              ENDIF
            ENDIF
            CHTESE=' '
            CALL MECALC(OPTIO2,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,CHTEMP,
     &                  K24B,CHTIME,CHNUMC,CHHARM,CHSIG,CHEPS,CHFREQ,
     &                  CHMASS,CHMETA,CHAREP,TYPCOE,ALPHA,CALPHA,CHDYNR,
     &                  SOP,CHELEM,K24B,LIGREL,BASE,CHVARC,CHVREF,K24B,
     &                  COMPOR,CHTESE,CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 90
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
   90       CONTINUE
            CALL JEDEMA()
  100     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "SIEF_ELNO"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'SIEF_ELNO') THEN
          DO 120,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,2,LERES0,'SIEF_ELGA',IORDR,CHSIG,OPTION,IRET1)
            CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                  IRET2)
            IF (IRET1.GT.0 .AND. IRET2.GT.0)GOTO 110
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
C       OPTION 'SIEF_SENO_SEGA' EST NÉCESSAIRE SI X-FEM
            CALL JEEXIN(MODELE(1:8)//'.FISS',IFISS)
            IF (IFISS.NE.0) THEN
              OPTIOX=BLAN24(1:16)
              OPTIOX(1:14)='SIEF_SENO_SEGA'
              CALL RSEXC1(LERES1,OPTIOX,IORDR,CHELEX)
            ENDIF
C
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
            ELSE
              TIME=ZERO
            ENDIF
            CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)
            CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
            CALL RSEXCH(RESUCO,'DEPL',IORDR,CHAMGD,IRET)

C      A PARTIR DE SIEF_ELGA
            IF (IRET1.EQ.0) THEN
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)

C      A PARTIR DE SIEF_ELGA
            ELSEIF (IRET2.EQ.0) THEN
              COMPOR=' '
            ENDIF

            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,K24B,K24B,K24B,CHSIG,K24B,K24B,K24B,K24B,
     &                  K24B,TYPCOE,ALPHA,CALPHA,K24B,SOP,CHELEM,CHELEX,
     &                  LIGREL,BASE,CHVARC,CHVREF,K24B,COMPOR,CHTESE,
     &                  CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 110
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
            IF (IFISS.NE.0) CALL RSNOCH(LERES1,OPTIOX,IORDR,' ')

  110       CONTINUE
            CALL JEDEMA()
  120     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "ECIN_ELEM"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'ECIN_ELEM') THEN
          IF (NCHAR.NE.0 .AND. CTYP.NE.'MECA') THEN
            CALL U2MESS('A','CALCULEL2_98')
            GOTO 660

          ENDIF
          IF (TYSD.EQ.'MODE_MECA') THEN
            TYPE='DEPL'
          ELSEIF (TYSD.EQ.'EVOL_NOLI') THEN
            TYPE='VITE'
          ELSEIF (TYSD.EQ.'DYNA_TRANS') THEN
            TYPE='VITE'
          ELSEIF (TYSD.EQ.'DYNA_HARMO') THEN
            TYPE='VITE'
          ELSE
            VALKM(1)=OPTION
            VALKM(2)=TYSD
            CALL U2MESK('A','CALCULEL3_6',2,VALKM)
            GOTO 660

          ENDIF
          CHMASS='&&'//NOMPRO//'.MASD'
          CALL MECACT('V',CHMASS,'MAILLA',NOMA,'POSI',1,'POS',INUME,R8B,
     &                CBID,K8B)
          DO 140,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,TYPE//'            ',IORDR,CHAMGD,
     &                  OPTION,IRET)
            IF (IRET.GT.0)GOTO 130
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            FREQ=UN
            IF (TYSD.EQ.'FOURIER_ELAS' .OR. TYSD.EQ.'COMB_FOURIER') THEN
              CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
              CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
            ENDIF
            IF (TYPE.EQ.'DEPL') THEN
              CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORDR,0,LFREQ,K8B)
              FREQ=ZR(LFREQ)
            ENDIF
            CHFREQ='&&'//NOMPRO//'.FREQ'
            CALL MECACT('V',CHFREQ,'MAILLA',NOMA,'FREQ_R',1,'FREQ',IBID,
     &                  FREQ,CBID,K8B)
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
              CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
            ELSE
              CHTIME=' '
              TIME=ZERO
            ENDIF
            CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)
            CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,CHTIME,CHNUMC,CHHARM,CHSIG,CHEPS,CHFREQ,
     &                  CHMASS,CHMETA,ZK8(JCHA),K24B,ZERO,CZERO,CHDYNR,
     &                  SOP,CHELEM,K24B,LIGREL,BASE,CHVARC,CHVREF,K24B,
     &                  COMPOR,CHTESE,CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 130
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
            CALL DETRSD('CHAMP_GD',CHFREQ)
  130       CONTINUE
            CALL JEDEMA()
  140     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTIONS "SIGM_NOZ1_ELGA","SIGM_NOZ2_ELGA"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'SIGM_NOZ1_ELGA' .OR.
     &          OPTION.EQ.'SIGM_NOZ2_ELGA') THEN


          DO 160,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
            IF (IRET.GT.0)GOTO 150
            CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,IRET)
            CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                  IRET)
            IF (IRET.GT.0) THEN
              CALL U2MESK('A','CALCULEL3_7',1,OPTION)
              CALL JEDEMA
              GOTO 660

            ENDIF
            CALL RSEXC1(LERES1,OPTION,IORDR,CHSIGN)
            IF (OPTION.EQ.'SIGM_NOZ1_ELGA') THEN
              CALL SINOZ1(MODELE,CHSIG,CHSIGN)
            ELSEIF (OPTION.EQ.'SIGM_NOZ2_ELGA') THEN
              CALL DISMOI('F','PROF_CHNO',CHAMGD,'CHAM_NO',IB,PFCHNO,IE)
              CALL SINOZ2(MODELE,PFCHNO,CHSIG,CHSIGN)
            ENDIF
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  150       CONTINUE
            CALL JEDEMA()
  160     CONTINUE

C    ------------------------------------------------------------------
C    -- OPTIONS "EPSP_ELNO","EPSP_ELGA"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'EPSP_ELNO' .OR. OPTION.EQ.'EPSP_ELGA' .OR.
     &          OPTION.EQ.'EPFP_ELNO' .OR. OPTION.EQ.'EPFP_ELGA' .OR.
     &          OPTION.EQ.'EPFD_ELNO' .OR. OPTION.EQ.'EPFD_ELGA') THEN
          DO 180,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
            IF (IRET.GT.0)GOTO 170
            CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,IRET)
            CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                  IRET)
            IF (IRET.GT.0) THEN
              CALL U2MESK('A','CALCULEL3_7',1,OPTION)
              CALL JEDEMA
              GOTO 660

            ENDIF
            CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR,CHVARI,OPTION,IRET)
            IF (IRET.GT.0)CHVARI=' '
            CALL RSEXC1(LERES1,OPTION,IORDR,CHEPSP)
            IF (TYSD.EQ.'FOURIER_ELAS' .OR. TYSD.EQ.'COMB_FOURIER') THEN
              CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
              CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
            ENDIF
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
              CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
            ELSE
              CHTIME=' '
              TIME=ZERO
            ENDIF
            CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)
            CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
            CALL MECHDA(MODELE,NCHAR,ZK8(JCHA),EXITIM,TIME,CHEPSA)
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,CHTIME,CHNUMC,CHHARM,CHSIG,CHEPSA,CHFREQ,
     &                  CHMASS,CHMETA,ZK8(JCHA),K24B,ZERO,CZERO,CHDYNR,
     &                  SOP,CHEPSP,K24B,LIGREL,BASE,CHVARC,CHVREF,
     &                  CHVARI,COMPOR,CHTESE,CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 170
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  170       CONTINUE
            CALL JEDEMA()
  180     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTIONS "EQUI_ELGA_EPSI","EQUI_ELGA_EPME","EQUI_ELGA_SIGM",
C               "EQUI_ELNO_EPSI","EQUI_ELNO_EPME","PMPB_ELGA",
C               "PMPB_ELNO","EQUI_ELNO_SIGM","CRIT_ELNO"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'EQUI_ELGA_EPSI' .OR.
     &          OPTION.EQ.'EQUI_ELGA_EPME' .OR.
     &          OPTION.EQ.'EQUI_ELGA_SIGM' .OR.
     &          OPTION.EQ.'EQUI_ELNO_EPSI' .OR.
     &          OPTION.EQ.'EQUI_ELNO_EPME' .OR.
     &          OPTION.EQ.'PMPB_ELGA' .OR.
     &          OPTION.EQ.'PMPB_ELNO' .OR.
     &          OPTION.EQ.'EQUI_ELNO_SIGM' .OR.
     &          OPTION.EQ.'CRIT_ELNO') THEN
          DO 200,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CHEPS=' '
            CHSIG=' '
            CHSIC=' '
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            IF (OPTION.EQ.'EQUI_ELGA_EPSI') THEN
              CALL RSEXC2(1,1,RESUCO,'EPSI_ELGA',IORDR,CHEPS,
     &                    OPTION,IRET)
              IF (IRET.GT.0)GOTO 190
            ELSEIF (OPTION.EQ.'EQUI_ELNO_EPSI') THEN
              CALL RSEXC2(1,1,RESUCO,'EPSI_ELNO_DEPL',IORDR,CHEPS,
     &                    OPTION,IRET)
              IF (IRET.GT.0)GOTO 190
            ELSEIF (OPTION.EQ.'EQUI_ELGA_EPME') THEN
              CALL RSEXC2(1,1,RESUCO,'EPME_ELGA',IORDR,CHEPS,
     &                    OPTION,IRET)
              IF (IRET.GT.0)GOTO 190
            ELSEIF (OPTION.EQ.'EQUI_ELNO_EPME') THEN
              CALL RSEXC2(1,1,RESUCO,'EPME_ELNO',IORDR,CHEPS,
     &                    OPTION,IRET)
              IF (IRET.GT.0)GOTO 190
            ELSEIF (OPTION.EQ.'EQUI_ELGA_SIGM') THEN
              IF (TYSD.EQ.'FOURIER_ELAS') THEN
                CALL U2MESK('F','CALCULEL6_83',1,OPTION)
              ENDIF
              CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                    IRET)
              CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,
     &                    OPTION,IRET)
              IF (IRET.GT.0) THEN
                CALL U2MESK('A','CALCULEL3_7',1,OPTION)
                CALL JEDEMA
                GOTO 660

              ENDIF
            ELSEIF (OPTION.EQ.'PMPB_ELGA') THEN
              CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                    IRET1)
              IF (IRET1.GT.0)GOTO 190
            ELSEIF (OPTION.EQ.'PMPB_ELNO') THEN
              CALL RSEXC2(1,1,RESUCO,'SIEF_ELNO',IORDR,CHSIG,
     &                    OPTION,IRET1)
              IF (IRET1.GT.0)GOTO 190


            ELSEIF (OPTION.EQ.'EQUI_ELNO_SIGM') THEN
              IF (TYSD.EQ.'FOURIER_ELAS') CALL U2MESK('F',
     &            'CALCULEL6_83',1,OPTION)
              CALL RSEXCH(RESUCO,'SIEF_ELGA',IORDR,CHSIG,IRET1)
              CALL RSEXCH(RESUCO,'SIGM_ELNO_COQU',IORDR,CHSIC,IRET3)
              CALL RSEXCH(RESUCO,'SIGM_ELNO_DEPL',IORDR,CHSIC,IRET4)
C
              IF (IRET1.GT.0 .AND. IRET3.GT.0 .AND.
     &            IRET4.GT.0) THEN
                VALKM(1)=OPTION
                VALKM(2)=TYSD
                CALL U2MESK('A','CALCULEL3_8',2,VALKM)
                CALL JEDEMA
                GOTO 660

              ENDIF
              IF (TYSD.EQ.'EVOL_ELAS' .OR. TYSD.EQ.'DYNA_TRANS' .OR.
     &            TYSD.EQ.'MULT_ELAS' .OR. (TYSD.EQ.'MODE_MECA'.AND.
     &            TYPEMO(1:8).EQ.'MODE_DYN') .OR.
     &            TYSD.EQ.'COMB_FOURIER' .OR.
     &            TYSD.EQ.'FOURIER_ELAS') THEN

C             -- POUR COMB_FOURIER, IL PEUT NE PAS EXISTER SIEF_ELGA:
                IF (TYSD.EQ.'COMB_FOURIER' .AND. IRET1.GT.0) THEN
                  CALL U2MESS('F','CALCULEL3_55')
                ENDIF

C          CHAMP D'ENTREE POUR COQUES
                IF (EXIPLA) THEN
                  IF (IRET4.GT.0) THEN
                    VALKM(1)=OPTION
                    VALKM(2)=TYSD
                    CALL U2MESK('A','CALCULEL3_9',2,VALKM)
                    CALL JEDEMA
                    GOTO 660

                  ELSE
                    CALL RSEXCH(RESUCO,'SIGM_ELNO_DEPL',IORDR,CHSIC,K)
                  ENDIF
                ENDIF
              ELSEIF (TYSD.EQ.'EVOL_NOLI') THEN
                IF (IRET2.LE.0) THEN
                  CALL RSEXCH(RESUCO,'SIEF_ELGA',IORDR,CHSIG,K)
                ENDIF
                IF (EXIPLA) THEN
                  IF (IRET3.GT.0) THEN
                    VALKM(1)=OPTION
                    VALKM(2)=TYSD
                    CALL U2MESK('A','CALCULEL3_10',2,VALKM)
                    CALL JEDEMA
                    GOTO 660

                  ELSE
                    CALL RSEXCH(RESUCO,'SIGM_ELNO_COQU',IORDR,CHSIC,K)
                  ENDIF
                ENDIF
              ENDIF


            ELSEIF (OPTION.EQ.'CRIT_ELNO') THEN
              CALL RSEXC2(1,1,RESUCO,'SIGM_ELNO_DEPL',IORDR,CHSIG,
     &                    OPTION,IRET1)
              IF (IRET1.GT.0)GOTO 190
            ENDIF
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            CALL MECALC(OPTION,MODELE,K24B,CHGEOM,MATE,CHCARA,K24B,K24B,
     &                  K24B,CHNUMC,K24B,CHSIG,CHEPS,CHSIC,K24B,K24B,
     &                  ZK8(JCHA),K24B,ZERO,CZERO,K24B,K24B,CHELEM,K24B,
     &                  LIGREL,BASE,K24B,K24B,K24B,COMPOR,CHTESE,CHDESE,
     &                  BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 190
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  190       CONTINUE
            CALL JEDEMA()
  200     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "VALE_NCOU_MAXI"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'VALE_NCOU_MAXI') THEN

          CALL GETVTX(' ','NOM_CHAM',1,1,1,NOMCHA,NBVAL)
          CALL GETVTX(' ','NOM_CMP',1,1,1,NOMCMP,NBVAL)

          DO 220,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,NOMCHA,IORDR,CHBID,OPTION,IRET)
            IF (IRET.GT.0)GOTO 210
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            Z1Z2(1)='Z1'
            Z1Z2(2)='Z2'
            NOMS(1)=NOMCHA
            NOMS(2)=NOMCMP
            CHCMP='&&OP0058.ELGA_MAXI'
            CALL MECACT('V',CHCMP,'MODELE',MODELE,'NEUT_K24',2,Z1Z2,
     &                  IBID,RBID,CBID,NOMS)

            IF ((NOMCHA.EQ.'SIEF_ELGA') .OR.
     &          (NOMCHA.EQ.'SIEF_ELGA')) THEN
              CHSIG=CHBID
              CHEPS=' '
              CHSEQ=' '
              CHEEQ=' '
              CHVARI=' '
            ELSEIF (NOMCHA.EQ.'EPSI_ELGA') THEN
              CHSIG=' '
              CHEPS=CHBID
              CHSEQ=' '
              CHEEQ=' '
              CHVARI=' '
            ELSEIF (NOMCHA.EQ.'EQUI_ELGA_SIGM') THEN
              CHSIG=' '
              CHEPS=' '
              CHSEQ=CHBID
              CHEEQ=' '
              CHVARI=' '
            ELSEIF (NOMCHA.EQ.'EQUI_ELGA_EPSI') THEN
              CHSIG=' '
              CHEPS=' '
              CHSEQ=' '
              CHEEQ=CHBID
              CHVARI=' '
            ELSEIF (NOMCHA.EQ.'VARI_ELGA') THEN
              CHSIG=' '
              CHEPS=' '
              CHSEQ=' '
              CHEEQ=' '
              CHVARI=CHBID
            ENDIF

            CALL MECALC(OPTION,MODELE,K24B,CHGEOM,MATE,CHCARA,K24B,K24B,
     &                  K24B,CHCMP,K24B,CHSIG,CHEPS,CHSIC,K24B,K24B,
     &                  ZK8(JCHA),K24B,ZERO,CZERO,K24B,K24B,CHELEM,K24B,
     &                  LIGREL,BASE,CHSEQ,CHEEQ,K24B,COMPOR,CHTESE,
     &                  CHDESE,BLAN8,0,CHACSE,IRET)

            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  210       CONTINUE
            CALL JEDEMA()
  220     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "SIRO_ELEM"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'SIRO_ELEM') THEN
          DO 240,IAUX=1,NBORDR
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL RSEXC2(1,2,RESUCO,'SIEF_ELNO',IORDR,CHSIG,OPTION,
     &                  IRET1)
            CALL RSEXC2(2,2,RESUCO,'SIGM_ELNO_DEPL',IORDR,CHSIG,OPTION,
     &                  IRET2)
            IF (IRET1.GT.0 .AND. IRET2.GT.0)GOTO 230
            CHSIGF='&&'//NOMPRO//'.CHAM_SI2D'
            CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)

            OPTIO2='SIRO_ELEM'
            CALL MEARCC(OPTIO2,MODELE,CHSIG,CHSIGF)
            CALL RSEXC1(LERES1,OPTIO2,IORDR,CHELEM)
            CALL MECALC(OPTIO2,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,K24B,K24B,K24B,CHSIGF,K24B,K24B,K24B,K24B,
     &                  K24B,K24B,ZERO,CZERO,K24B,K24B,CHELEM,K24B,
     &                  LIGREL,BASE,K24B,K24B,K24B,COMPOR,CHTESE,CHDESE,
     &                  BLAN8,0,CHACSE,IRET)
            CALL RSNOCH(LERES1,OPTIO2,IORDR,' ')
            CALL DETRSD('CHAM_ELEM',CHSIGF)

  230       CONTINUE
  240     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTIONS DES INDICATEURS D'ERREURS
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'ERZ1_ELEM' .OR.
     &          OPTION.EQ.'ERZ2_ELEM' .OR.
     &          OPTION.EQ.'ERME_ELEM' .OR. OPTION.EQ.'ERME_ELNO' .OR.
     &          OPTION.EQ.'QIRE_ELEM' .OR.
     &          OPTION.EQ.'QIRE_ELNO' .OR.
     &          OPTION.EQ.'QIZ1_ELEM' .OR.
     &          OPTION.EQ.'QIZ2_ELEM') THEN
C
          CALL MECA01(OPTION,NBORDR,JORDR,NCHAR,JCHA,KCHA,CTYP,TBGRCA,
     &                RESUCO,RESUC1,LERES1,NOMA,MODELE,LIGRMO,MATE,CARA,
     &                0,NPASS,IRET)
C
          IF (IRET.EQ.1) THEN
            GOTO 650

          ELSEIF (IRET.EQ.2) THEN
            GOTO 690

          ELSEIF (IRET.EQ.3) THEN
            GOTO 660

          ENDIF
C
C    ------------------------------------------------------------------
C    -- OPTION "SING_ELEM"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'SING_ELEM') THEN

          CALL GETVR8(' ','PREC_ERR',1,1,1,PREC,IRET1)
          IF (IRET1.NE.1) THEN
            CALL U2MESS('F','CALCULEL3_12')
          ELSE
            IF (PREC.LE.0.D0) THEN
              CALL U2MESS('F','CALCULEL3_13')
            ENDIF
          ENDIF

          TYPES=' '
          CALL GETVTX(' ','TYPE_ESTI',1,1,1,TYPES,IRETER)
          IF (IRETER.GT.0) THEN
            CALL U2MESK('I','CALCULEL3_24',1,TYPES)
          ENDIF

C 1 - RECUPERATION DE :
C  NNOEM : NOMBRE DE NOEUDS
C  NELEM : NOMBRE D ELEMENTS FINIS (EF)
C  NDIM  : DIMENSION
C  JCOOR : ADRESSE DES COORDONNEES
C  JTYPE : ADRESSE DU TYPE D ELEMENTS FINIS

          CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,NOMA,IERD)

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

          CALL SINGUM(NOMA,NDIM,NNOEM,NELEM,ZI(JTYPE),ZR(JCOOR))

C 3 - BOUCLE SUR LES INSTANTS DEMANDES

          DO 260 IAUX=1,NBORDR
            CALL JEMARQ()
            IORDR=ZI(JORDR+IAUX-1)

            IF (IRETER.GT.0) THEN
              CALL RSEXCH(RESUCO,TYPES,IORDR,CHERR4,IRET5)

              IF (IRET5.GT.0) THEN
                VALKM(1)=TYPES
                VALKM(2)=RESUCO
                CALL U2MESK('A','CALCULEL3_26',2,VALKM)
                IRET=1
              ENDIF

C 3.1 - RECUPERATION DE LA CARTE D ERREUR ET D ENERGIE
C       SI PLUSIEURS INDICATEURS ON PREND PAR DEFAUT
C       ERME_ELEM SI IL EST PRESENT
C       ERZ2_ELEM PAR RAPPORT A ERZ1_ELEM

            ELSE

              IRET5=1
              CALL RSEXCH(RESUCO,'ERME_ELEM',IORDR,CHERR1,IRET1)
              CALL RSEXCH(RESUCO,'ERZ1_ELEM',IORDR,CHERR2,IRET2)
              CALL RSEXCH(RESUCO,'ERZ2_ELEM',IORDR,CHERR3,IRET3)

              IF (IRET1.GT.0 .AND. IRET2.GT.0 .AND. IRET3.GT.0) THEN
                CALL U2MESS('A','CALCULEL3_14')
                IRET=1
              ENDIF

            ENDIF

            IF (TYSD.EQ.'EVOL_NOLI') THEN
              CALL RSEXCH(RESUCO,'ETOT_ELEM',IORDR,CHENEG,IRET4)
            ELSE
              CALL RSEXCH(RESUCO,'EPOT_ELEM',IORDR,CHENEG,IRET4)
            ENDIF
            IF (IRET4.GT.0) THEN
              CALL U2MESS('A','CALCULEL3_29')
            ENDIF

            IF ((IRET+IRET4).GT.0) THEN
              CALL U2MESS('A','CALCULEL3_36')
              GOTO 250

            ENDIF
C 3.2 - TRANSFORMATION DE CES DEUX CARTES EN CHAM_ELEM_S

            CHERRS='&&'//NOMPRO//'.ERRE'

            IF (IRET5.EQ.0) THEN
              CALL CELCES(CHERR4(1:19),'V',CHERRS)
            ELSEIF (IRET1.EQ.0) THEN
              CALL CELCES(CHERR1(1:19),'V',CHERRS)
              IF ((IRET2.EQ.0) .OR. (IRET3.EQ.0)) THEN
                CALL U2MESS('A','CALCULEL3_15')
              ENDIF
            ELSEIF (IRET3.EQ.0) THEN
              CALL CELCES(CHERR3(1:19),'V',CHERRS)
              IF (IRET2.EQ.0) CALL U2MESS('A','CALCULEL3_16')
            ELSEIF (IRET2.EQ.0) THEN
              CALL CELCES(CHERR2(1:19),'V',CHERRS)
            ELSE
              CALL ASSERT(.FALSE.)
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

            CALL SINGUE(CHERRS,CHENES,NOMA,NDIM,NNOEM,NELEM,ZR(JCOOR),
     &                  PREC,LIGRMO,IORDR,CHELEM,TYPES)

            CALL RSNOCH(LERES1,OPTION,IORDR,' ')

C 3.4 - DESTRUCTION DES CHAM_ELEM_S

            CALL DETRSD('CHAM_ELEM_S',CHERRS)
            CALL DETRSD('CHAM_ELEM_S',CHENES)

  250       CONTINUE
            CALL JEDEMA()
  260     CONTINUE

C 4 - DESTRUCTION DES OBJETS TEMPORAIRES

          CALL JEDETR('&&SINGUM.DIME           ')
          CALL JEDETR('&&SINGUM.MESU           ')
          CALL JEDETR('&&SINGUM.CONN           ')
          CALL JEDETR('&&SINGUM.CINV           ')
C    ------------------------------------------------------------------
C    -- OPTION "SING_ELNO"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'SING_ELNO') THEN
          DO 280 IAUX=1,NBORDR
            CALL JEMARQ()
            IORDR=ZI(JORDR+IAUX-1)

C 1 - RECUPERATION DE LA CARTE DE SINGULARITE

            CALL RSEXC2(1,1,RESUCO,'SING_ELEM',IORDR,CHSING,OPTION,
     &                  IRET1)

            IF (IRET1.GT.0)GOTO 270

C 2 - TRANSFORMATION DE CE CHAMP EN CHAM_ELEM_S

            CHSINS='&&'//NOMPRO//'.SING'
            CALL CELCES(CHSING(1:19),'V',CHSINS)

C 3 - TRANSFOMATION DU CHAMP CHSINS ELEM EN ELNO

            CHSINN='&&'//NOMPRO//'.SINN'
            CALL CESCES(CHSINS,'ELNO',' ',' ',' ','V',CHSINN)

C 4 - STOCKAGE

            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)

            CALL CESCEL(CHSINN,LIGRMO(1:19),'SING_ELNO','PSINGNO',
     &                  'NON',NNCP,'G',CHELEM(1:19),'F',IBID)

            CALL RSNOCH(LERES1,OPTION,IORDR,' ')

C 5 - DESTRUCTION DES CHAM_ELEM_S

            CALL DETRSD('CHAM_ELEM_S',CHSINS)
            CALL DETRSD('CHAM_ELEM_S',CHSINN)

  270       CONTINUE
            CALL JEDEMA()
  280     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTIONS "SIGM_ELNO_CART" ET "EFGE_ELNO_CART"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'SIGM_ELNO_CART' .OR.
     &          OPTION.EQ.'EFGE_ELNO_CART') THEN
          DO 300,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            IF (OPTION.EQ.'SIGM_ELNO_CART') THEN
              CALL RSEXC2(1,1,RESUCO,'SIGM_ELNO_DEPL',IORDR,CHAMGD,
     &                    OPTION,IRET)
            ELSE
              CALL RSEXC2(1,2,RESUCO,'EFGE_ELNO_DEPL',IORDR,CHSIG,
     &                    OPTION,IRET)
              CALL RSEXC2(2,2,RESUCO,'SIEF_ELNO',IORDR,CHSIG,
     &                    OPTION,IRET)
              CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
            ENDIF
            IF (IRET.GT.0)GOTO 290
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,K24B,K24B,K24B,CHSIG,K24B,K24B,K24B,K24B,
     &                  K24B,K24B,ZERO,CZERO,K24B,K24B,CHELEM,K24B,
     &                  LIGREL,BASE,K24B,K24B,K24B,COMPOR,CHTESE,CHDESE,
     &                  BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 290
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  290       CONTINUE
            CALL JEDEMA()
  300     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "VNOR_ELEM_DEPL"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'VNOR_ELEM_DEPL') THEN
          IF (NCHAR.NE.0 .AND. CTYP.NE.'MECA') THEN
            CALL U2MESS('A','CALCULEL2_98')
            GOTO 660

          ENDIF
          DO 320,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'VITE',IORDR,CHAMGD,OPTION,IRET)
            IF (IRET.GT.0)GOTO 310
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            IF (TYSD.EQ.'FOURIER_ELAS' .OR. TYSD.EQ.'COMB_FOURIER') THEN
              CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
              CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
            ENDIF
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
              CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
            ELSE
              CHTIME=' '
              TIME=ZERO
            ENDIF
            CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)
            CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))

            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,CHTIME,CHNUMC,CHHARM,CHSIG,CHEPS,CHFREQ,
     &                  CHMASS,CHMETA,ZK8(JCHA),' ',ZERO,CZERO,CHDYNR,
     &                  SOP,CHELEM,K24B,LIGREL,BASE,CHVARC,CHVREF,K24B,
     &                  COMPOR,CHTESE,CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 310
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  310       CONTINUE
            CALL JEDEMA()
  320     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTION "PRME_ELNO"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'PRME_ELNO') THEN

          DO 340,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
            TYPE='DEPL'
            IF (IRET.GT.0)GOTO 330
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            CALL MECOA1(OPTION,MODELE,LIGREL,MATE,CHAMGD,CHELEM)
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  330       CONTINUE
            CALL JEDEMA()
  340     CONTINUE


C     ------------------------------------------------------------------
C     --- OPTIONS DE CALCUL DES INDICATEURS LOCAUX DE DECHARGE ET DE
C     --- PERTE DE RADIALITE :
C     ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'DCHA_ELGA_SIGM' .OR.
     &          OPTION.EQ.'DCHA_ELNO_SIGM' .OR.
     &          OPTION.EQ.'RADI_ELGA_SIGM' .OR.
     &          OPTION.EQ.'RADI_ELNO_SIGM') THEN
          IF (NBORDR.EQ.1) THEN
            CALL U2MESK('A','CALCULEL5_63',1,OPTION)
            GOTO 660

          ENDIF
          DO 360,IAUX=1,NBORDR-1
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR1=ZI(JORDR+IAUX-1)
            IORDR2=ZI(JORDR+IAUX)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR1,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR1,CHSIG1,OPTION,
     &                  IRET1)
            CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR2,CHSIG2,OPTION,
     &                  IRET2)
            IF (IRET1.GT.0 .OR. IRET2.GT.0)GOTO 350
            IF (NORME.EQ.'VMIS_CINE' .OR. NORME.EQ.'TOTAL_CINE') THEN
              CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR1,CHVAR1,OPTION,
     &                    IRET1)
              CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR2,CHVAR2,OPTION,
     &                    IRET2)
              IF (IRET1.GT.0 .OR. IRET2.GT.0)GOTO 350
            ELSE
              CHVAR1=' '
              CHVAR2=' '
            ENDIF
            CALL RSEXC1(LERES1,OPTION,IORDR1,CHELEM)
            CALL INDRAD(OPTION,NORME,MODELE,LIGREL,CHSIG1,CHSIG2,CHVAR1,
     &                  CHVAR2,CHELEM)
            CALL RSNOCH(RESUCO,OPTION,IORDR1,' ')
  350       CONTINUE
            CALL JEDEMA()
  360     CONTINUE
C     ------------------------------------------------------------------
C     --- OPTIONS DE CALCUL DES DENSITES D'ENERGIE TOTALE
C     ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'ETOT_ELGA' .OR.
     &          OPTION.EQ.'ETOT_ELNO' .OR.
     &          OPTION.EQ.'ETOT_ELEM') THEN
          DO 380,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)

C ---       RECUPERATION DES CONTRAINTES DE L'INSTANT COURANT :
C           -------------------------------------------------
            CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,IRET1)
            IF (IRET1.GT.0) THEN
              CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,
     &                    OPTION,IRET2)
              IF (IRET2.GT.0) THEN
                CALL CODENT(IORDR,'G',KIORD)
                VALKM(1)=RESUCO
                VALKM(2)=KIORD
                CALL U2MESK('A','CALCULEL3_17',2,VALKM)
                GOTO 370

              ENDIF
            ENDIF

C ---       SI LE NUMERO D'ORDRE COURANT EST SUPERIEUR A 1, ON
C ---       RECUPERE LES CONTRAINTES DE L'INSTANT PRECEDENT :
C           -----------------------------------------------
            IF ((IAUX.GT.1) .AND. (CONCEP.NE.'MODE_MECA')) THEN
              IORDRM=ZI(JORDR+IAUX-2)
              CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDRM,CHSIGM,OPTION,
     &                    IRET1)
              IF (IRET1.GT.0) THEN
                CALL CODENT(IORDRM,'G',KIORDM)
                VALKM(1)=RESUCO
                VALKM(2)=KIORDM
                CALL U2MESK('A','CALCULEL3_17',2,VALKM)
                GOTO 370

              ENDIF
            ENDIF

C ---       RECUPERATION DU CHAMP DE DEPLACEMENT DE L'INSTANT COURANT :
C           ---------------------------------------------------------
            CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHDEPL,OPTION,IRET1)
            IF (IRET1.GT.0) THEN
              CALL CODENT(IORDR,'G',KIORD)
              VALKM(1)=RESUCO
              VALKM(2)=KIORD
              CALL U2MESK('A','CALCULEL3_11',2,VALKM)
              GOTO 370

            ENDIF

C ---       SI LE NUMERO D'ORDRE COURANT EST SUPERIEUR A 1, ON
C ---       RECUPERE LES DEPLACEMENTS DE L'INSTANT PRECEDENT :
C           ------------------------------------------------
            IF ((IAUX.GT.1) .AND. (CONCEP.NE.'MODE_MECA')) THEN
              CALL RSEXC2(1,1,RESUCO,'DEPL',IORDRM,CHDEPM,OPTION,IRET1)
              IF (IRET1.GT.0) THEN
                CALL CODENT(IORDRM,'G',KIORDM)
                VALKM(1)=RESUCO
                VALKM(2)=KIORDM
                CALL U2MESK('A','CALCULEL3_11',2,VALKM)
                GOTO 370

              ENDIF
            ENDIF

            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)

            IF (CONCEP.EQ.'MODE_MECA' .AND.
     &          TYPEMO(1:8).EQ.'MODE_DYN') THEN
              CALL ENETOT(OPTION,1,LIGREL,CHGEOM,CHDEPL,CHDEPM,CHSIG,
     &                    CHSIGM,CHELEM)
            ELSE

              CALL ENETOT(OPTION,IAUX,LIGREL,CHGEOM,CHDEPL,CHDEPM,CHSIG,
     &                    CHSIGM,CHELEM)
            ENDIF

            CALL RSNOCH(RESUCO,OPTION,IORDR,' ')
  370       CONTINUE
            CALL JEDEMA()
  380     CONTINUE
          CALL DETRSD('CHAMP_GD','&&ENETOT.CHAMELEM2')
C     ------------------------------------------------------------------
C     --- OPTIONS DE CALCUL DE:
C     ---   * TAUX DE TRIAXIALITE DES CONTRAINTES,
C     ---   * CONTRAINTE D'ENDOMMAGEMENT,
C     ---   * ENDOMMAGEMENT DE LEMAITRE-SERMAGE
C     --- RESPONSABLE DEVELOPPEMENT: FRANCK MEISSONNIER (AMA/T65)
C     ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'ENDO_ELGA' .OR.
     &          OPTION.EQ.'ENDO_ELNO') THEN
C
          IF (NBORDR.EQ.1) THEN
            CALL U2MESK('A','CALCULEL5_63',1,OPTION)
            GOTO 660

          ENDIF

C IORDR1 = ORDRE CORRESPONDANT AU TEMPS T-
C IORDR2 = ORDRE CORRESPONDANT AU TEMPS T+
          DO 410 IAUX=2,NBORDR
            IORDR1=ZI(JORDR-1+IAUX-1)
            IORDR2=ZI(JORDR-1+IAUX)
C
C --- A/ TRAITEMENT DE L'OPTION ENDO_ELGA
C     -----------------------------------
            IF (OPTION.EQ.'ENDO_ELGA') THEN
C --- A11/ RECUPERATION DU CHAMP DE CONTRAINTES 'SIEF_ELGA'
C          -> CHSIG[1,2] A T- ET T+
              CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR1,CHSIG1,OPTION,
     &                    IRET1)
              CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA',IORDR1,CHSIG1,
     &                    OPTION,IRET1)
              IF (IRET1.GT.0) THEN
                CALL U2MESK('A','CALCULEL3_18',1,OPTION)
                GOTO 410

              ENDIF

              CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR2,CHSIG2,OPTION,
     &                    IRET2)
              CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA',IORDR2,CHSIG2,
     &                    OPTION,IRET2)
              IF (IRET2.GT.0) THEN
                CALL U2MESK('A','CALCULEL3_18',1,OPTION)
                GOTO 410

              ENDIF
C
C --- A12/ RECUPERATION DU CHAMP DE VARIABLES INTERNES 'VARI_ELGA'
C          -> CHVARI[1,2] A T- ET T+
              NORME='DOM_LEM'
              IF (NORME.EQ.'DOM_LEM') THEN
                CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR1,CHVAR1,OPTION,
     &                      IRET1)
                CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR2,CHVAR2,OPTION,
     &                      IRET2)
                IF ((IRET1.GT.0) .OR. (IRET2.GT.0))GOTO 410
              ELSE
                CHVAR1=' '
                CHVAR2=' '
              ENDIF
C
              IF (IAUX.EQ.2) THEN
C --- A21/ INITIALISATION DES VARIABLES A L'ORDRE IORDR1
                CALL RSEXC1(LERES1,OPTION,IORDR1,CHELE1)
                CALL ALCHML(LIGREL,'ENDO_ELGA','PTRIAGM','G',CHELE1,
     &                      IRET,' ')
                IF (IRET.GT.0) THEN
                  CALL U2MESK('A','CALCULEL3_19',1,OPTION)
                  GOTO 410

                ENDIF
                CALL RSNOCH(LERES1,OPTION,IORDR1,' ')

              ELSE
C --- A22/ RECUPERATION DES VAR. ENDOMMAGEMENT @T
                CALL RSEXC2(1,1,RESUCO,'ENDO_ELGA',IORDR1,CHELE1,OPTION,
     &                      IRET1)
                IF (IRET1.GT.0) THEN
                  CALL U2MESK('A','CALCULEL3_20',1,OPTION)
                  GOTO 410

                ENDIF
              ENDIF

C --- A23/ RECUPERATION DU NOM DU CHAMP EXTRAIT DE LA SD LERES1 @T+1
              CALL RSEXC1(LERES1,OPTION,IORDR2,CHELE2)

C --- A3/ RECUPERATION DU TEMPS CORRESPONDANT AUX ORDRES #IORDR[1,2]
              IF (EXITIM) THEN
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR1,0,IINST1,K8B)
                CALL RSADPA(RESUCO,'L',1,'INST',IORDR2,0,IINST2,K8B)
                TIME1=ZR(IINST1)
                TIME2=ZR(IINST2)
                CALL MECHTI(NOMA,TIME1,RUNDF,RUNDF,CHTIM1)
                CALL MECHTI(NOMA,TIME2,RUNDF,RUNDF,CHTIM2)
              ELSE
                CHTIM1=' '
                CHTIM2=' '
                TIME1=ZERO
                TIME2=ZERO
              ENDIF

C --- A4/ EVALUATION DES DONNEES MATERIAUX AUX INSTANTS - ET +
C     --------------------------------------------------------

              CALL VRCINS(MODELE,MATE,CARA,TIME1,CHVARC,CODRET)
              CALL VRCINS(MODELE,MATE,CARA,TIME2,CHVAC2,CODRET)

C --- A5/ CALCUL DU TAUX DE TRIAXIALITE, DE LA CONTRAINTE
C ---     D'ENDOMMAGEMENT ET DE L'ENDOMMAGEMENT DE LEMAITRE-SERMAGE
C     -------------------------------------------------------------
              CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR1,COMPOR,IRET1)

              CALL ENDOLE(OPTION,LIGREL,MATE,COMPOR,CHVARC,
     &                    CHSIG1,CHVAR1,CHVAC2,CHSIG2,CHVAR2,CHEND2,
     &                    CHELE1,CHELE2)

C --- A6/ ECRITURE DU CHAMP LERES1
C     ----------------------------
              CALL RSNOCH(LERES1,OPTION,IORDR2,' ')
            ENDIF
C
C --- B/ TRAITEMENT DE L'OPTION ENDO_ELNO
C     ----------------------------------------
            IF (OPTION.EQ.'ENDO_ELNO') THEN
              CALL RSEXC2(1,1,RESUCO,'ENDO_ELGA',IORDR2,CHEND2,OPTION,
     &                    IRET1)
              IF (IRET1.GT.0) THEN
                CALL U2MESS('F','CALCULEL3_21')
              ENDIF
              CALL RSEXC1(LERES1,OPTION,IORDR2,CHELE2)
              CALL ENDOLE(OPTION,LIGREL,MATE,COMPOR,CHVARC,
     &                    CHSIG1,CHVAR1,CHVAC2,CHSIG2,CHVAR2,CHEND2,
     &                    CHELE1,CHELE2)
              CALL RSNOCH(LERES1,OPTION,IORDR2,' ')
            ENDIF

  410     CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION "SIGM_ELNO_COQU"
C     ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'SIGM_ELNO_COQU') THEN
          DO 430,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,IRET1)
            IF (IRET1.GT.0)GOTO 420
            CALL RSEXCH(RESUCO,'DEPL',IORDR,CHDEPL,IRET1)
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
            ELSE
              TIME=ZERO
            ENDIF
            CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)
            CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
            CALL MECALC(OPTION,MODELE,K24B,CHGEOM,MATE,CHCARA,K24B,K24B,
     &                  K24B,CHNUMC,K24B,CHSIG,CHDEPL,K24B,K24B,K24B,
     &                  K24B,TYPCOE,ALPHA,CALPHA,K24B,SOP,CHELEM,K24B,
     &                  LIGREL,BASE,CHVARC,CHVREF,K24B,COMPOR,CHTESE,
     &                  CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 420
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  420       CONTINUE
            CALL JEDEMA()
  430     CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION "SIGM_ELNO_TUYO"
C     ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'SIGM_ELNO_TUYO') THEN
          DO 450,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXCH(RESUCO,'SIEF_ELGA',IORDR,CHSIG,IRET1)
            IF (IRET1.GT.0) THEN
              CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR,CHSIG,
     &                    OPTION,IRET2)
              IF (IRET2.GT.0)GOTO 440
            ENDIF
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)

            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
            ELSE
              TIME=ZERO
            ENDIF
            CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)
            CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,K24B,CHNUMC,K24B,CHSIG,K24B,K24B,K24B,K24B,
     &                  K24B,TYPCOE,ALPHA,CALPHA,K24B,SOP,CHELEM,K24B,
     &                  LIGREL,BASE,CHVARC,CHVREF,K24B,COMPOR,CHTESE,
     &                  CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 440
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  440       CONTINUE
            CALL JEDEMA()
  450     CONTINUE
C     ------------------------------------------------------------------
C     --- OPTION "EPSI_ELNO_TUYO"
C     ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'EPSI_ELNO_TUYO') THEN
          DO 470,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'EPSI_ELGA',IORDR,CHEPS,OPTION,
     &                  IRET2)
            IF (IRET2.GT.0)GOTO 460
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
            ELSE
              TIME=ZERO
            ENDIF
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,CHTEMP,
     &                  K24B,K24B,CHNUMC,K24B,K24B,CHEPS,K24B,K24B,K24B,
     &                  K24B,TYPCOE,ALPHA,CALPHA,K24B,SOP,CHELEM,K24B,
     &                  LIGREL,BASE,CHVARC,CHVREF,K24B,COMPOR,CHTESE,
     &                  CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 460
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  460       CONTINUE
            CALL JEDEMA()
  470     CONTINUE
C     ------------------------------------------------------------------
C     --- OPTION "EPEQ_ELNO"
C     ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'EPEQ_ELNO') THEN
          DO 490,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'EQUI_ELGA_EPSI',IORDR,CHEEQ,OPTION,
     &                  IRET2)
            IF (IRET2.GT.0)GOTO 480
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
            ELSE
              TIME=ZERO
            ENDIF
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,CHTEMP,
     &                  K24B,K24B,CHNUMC,K24B,K24B,K24B,K24B,K24B,K24B,
     &                  K24B,TYPCOE,ALPHA,CALPHA,K24B,SOP,CHELEM,K24B,
     &                  LIGREL,BASE,CHVARC,CHEEQ,K24B,COMPOR,CHTESE,
     &                  CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 480
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  480       CONTINUE
            CALL JEDEMA()
  490     CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION "SIEQ_ELNO"
C     ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'SIEQ_ELNO') THEN
          DO 510,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'EQUI_ELGA_SIGM',IORDR,CHSEQ,OPTION,
     &                  IRET2)
            IF (IRET2.GT.0)GOTO 500
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
            ELSE
              TIME=ZERO
            ENDIF
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,CHTEMP,
     &                  K24B,K24B,CHNUMC,K24B,K24B,K24B,K24B,K24B,K24B,
     &                  K24B,TYPCOE,ALPHA,CALPHA,K24B,SOP,CHELEM,K24B,
     &                  LIGREL,BASE,CHSEQ,K24B,K24B,COMPOR,CHTESE,
     &                  CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 500
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  500       CONTINUE
            CALL JEDEMA()
  510     CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION: INDI_LOCA_ELGA
C     ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'INDI_LOCA_ELGA') THEN
          DO 520,IAUX=1,NBORDR
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'SIEF_ELGA',IORDR,CHSIG2,OPTION,IRET)
            CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR,CHVAR2,OPTION,IRET)
            IF (IRET.GT.0)GOTO 520
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            CALL MECALC(OPTION,MODELE,CHVAR2,K24B,MATE,K24B,K24B,K24B,
     &                  K24B,K24B,K24B,CHSIG2,K24B,K24B,K24B,K24B,K24B,
     &                  K24B,ZERO,CZERO,K24B,SOP,CHELEM,K24B,LIGREL,
     &                  BASE,K24B,K24B,K24B,COMPOR,CHTESE,CHDESE,BLAN8,
     &                  0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 520
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  520     CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION " VARI_ELNO_ELGA"
C     ------------------------------------------------------------------
        ELSEIF ((OPTION.EQ.'VARI_ELNO_ELGA') .OR.
     &          (OPTION.EQ.'VARI_ELNO_COQU')) THEN
          DO 540,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,LERES0,'VARI_ELGA',IORDR,CHAMGD,OPTION,IRET)
            IF (IRET.GT.0)GOTO 530
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,K24B,CHNUMC,K24B,K24B,K24B,K24B,K24B,K24B,
     &                  K24B,K24B,ZERO,CZERO,K24B,SOP,CHELEM,K24B,
     &                  LIGREL,BASE,K24B,K24B,K24B,COMPOR,CHTESE,CHDESE,
     &                  BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 530
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  530       CONTINUE
            CALL JEDEMA()
  540     CONTINUE

C     ------------------------------------------------------------------
C     --- OPTION "VARI_ELNO_TUYO"
C     ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'VARI_ELNO_TUYO') THEN
          K24B=' '
          DO 560,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR,CHAMGD,OPTION,IRET)

            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,K24B,CHNUMC,K24B,K24B,K24B,K24B,K24B,K24B,
     &                  K24B,K24B,ZERO,CZERO,K24B,SOP,CHELEM,K24B,
     &                  LIGREL,BASE,K24B,K24B,K24B,COMPOR,CHTESE,CHDESE,
     &                  BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 550
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  550       CONTINUE
            CALL JEDEMA()
  560     CONTINUE
C    ------------------------------------------------------------------
C    -- OPTIONS "ENEL_ELGA" ET "ENEL_ELNO"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'ENEL_ELGA' .OR.
     &          OPTION.EQ.'ENEL_ELNO') THEN
          DO 580,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,IRET)
            CALL RSEXC2(2,2,RESUCO,'SIEF_ELGA',IORDR,CHSIG,OPTION,
     &                  IRET)
            IF (IRET.GT.0) THEN
              CALL U2MESK('A','CALCULEL3_18',1,OPTION)
              GOTO 570

            ENDIF
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            IF (TYSD.EQ.'FOURIER_ELAS' .OR. TYSD.EQ.'COMB_FOURIER') THEN
              CALL RSADPA(RESUCO,'L',1,'NUME_MODE',IORDR,0,JNMO,K8B)
              CALL MEHARM(MODELE,ZI(JNMO),CHHARM)
            ENDIF
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
              CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
            ELSE
              CHTIME=' '
              TIME=ZERO
            ENDIF
            CALL VRCINS(MODELE,MATE,CARA,TIME,CHVARC,CODRET)
            CALL VRCREF(MODELE,MATE(1:8),CARA,CHVREF(1:19))

            CALL RSEXC2(1,1,RESUCO,'DEPL',IORDR,CHAMGD,OPTION,IRET)
            CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR,CHVARI,OPTION,IRET)
            IF (IRET.GT.0)CHVARI=' '
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)

            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,CHTEMP,
     &                  K24B,CHTIME,CHNUMC,CHHARM,CHSIG,CHEPS,CHFREQ,
     &                  CHMASS,CHMETA,ZK8(JCHA),' ',ZERO,CZERO,CHDYNR,
     &                  SOP,CHELEM,K24B,LIGREL,BASE,CHVARC,CHVREF,
     &                  CHVARI,COMPOR,CHTESE,CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 570
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  570       CONTINUE
            CALL JEDEMA()
  580     CONTINUE

C    ------------------------------------------------------------------
C    -- OPTIONS "DISS_ELGA" ET "DISS_ELNO"
C    ------------------------------------------------------------------
        ELSEIF (OPTION.EQ.'DISS_ELGA' .OR.
     &          OPTION.EQ.'DISS_ELNO') THEN
          DO 600,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            IF (EXITIM) THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORDR,0,IAINST,K8B)
              TIME=ZR(IAINST)
              CALL MECHTI(NOMA,TIME,RUNDF,RUNDF,CHTIME)
            ELSE
              CHTIME=' '
              TIME=ZERO
            ENDIF

            CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR,CHVARI,OPTION,IRET)

            IF (IRET.GT.0)CHVARI=' '
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            CALL MECALC(OPTION,MODELE,K24B,CHGEOM,MATE,CHCARA,CHTEMP,
     &                  K24B,CHTIME,CHNUMC,K24B,K24B,CHEPS,CHFREQ,
     &                  CHMASS,CHMETA,ZK8(JCHA),' ',ZERO,CZERO,CHDYNR,
     &                  SOP,CHELEM,K24B,LIGREL,BASE,K24B,K24B,CHVARI,
     &                  COMPOR,CHTESE,CHDESE,BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 590
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  590       CONTINUE
            CALL JEDEMA()
  600     CONTINUE


C    ------------------------------------------------------------------
C    -- OPTION "VAEX_ELGA"
C    ------------------------------------------------------------------

        ELSEIF (OPTION.EQ.'VAEX_ELGA') THEN
          DO 620,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'VARI_ELGA',IORDR,CHAMGD,OPTION,IRET)
            IF (IRET.GT.0)GOTO 610
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,K24B,CHNUMC,K24B,K24B,K24B,K24B,K24B,K24B,
     &                  K24B,K24B,ZERO,CZERO,K24B,SOP,CHELEM,K24B,
     &                  LIGREL,BASE,K24B,K24B,K24B,COMPOR,CHTESE,CHDESE,
     &                  BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 610
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  610       CONTINUE
            CALL JEDEMA()
  620     CONTINUE
C      -----------------------------------------------------------------
C    ------------------------------------------------------------------
C    -- OPTION "VAEX_ELNO"
C    ------------------------------------------------------------------

        ELSEIF (OPTION.EQ.'VAEX_ELNO') THEN
          DO 640,IAUX=1,NBORDR
            CALL JEMARQ()
            CALL JERECU('V')
            IORDR=ZI(JORDR+IAUX-1)
            CALL MEDOM2(MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO,IORDR,
     &                  NBORDR,NPASS,LIGREL)
            CALL JEVEUO(KCHA,'L',JCHA)
            CALL MECARA(CARA,EXICAR,CHCARA)
            CALL MECHC1(SAVCAR,MODELE,MATE,EXICAR,CHCARA)
            CALL RSEXC2(1,1,RESUCO,'VARI_ELNO_ELGA',IORDR,CHAMGD,OPTION,
     &                  IRET)
            IF (IRET.GT.0)GOTO 630
            CALL RSEXCH(RESUCO,'COMPORTEMENT',IORDR,COMPOR,IRET1)
            CALL RSEXC1(LERES1,OPTION,IORDR,CHELEM)
            CALL MECALC(OPTION,MODELE,CHAMGD,CHGEOM,MATE,CHCARA,K24B,
     &                  K24B,K24B,CHNUMC,K24B,K24B,K24B,K24B,K24B,K24B,
     &                  K24B,K24B,ZERO,CZERO,K24B,SOP,CHELEM,K24B,
     &                  LIGREL,BASE,K24B,K24B,K24B,COMPOR,CHTESE,CHDESE,
     &                  BLAN8,0,CHACSE,IRET)
            IF (IRET.GT.0)GOTO 630
            CALL RSNOCH(LERES1,OPTION,IORDR,' ')
  630       CONTINUE
            CALL JEDEMA()
  640     CONTINUE

        ELSE
          CALL U2MESK('A','CALCULEL3_22',1,OPTION)
        ENDIF

  650   CONTINUE
  660 CONTINUE
C
C============= FIN DE LA BOUCLE SUR LES OPTIONS A CALCULER =============
C
      GOTO 690

  680 CONTINUE
      VALKM(1)=TYSD
      VALKM(2)=OPTION
      CALL U2MESK('A','CALCULEL3_27',2,VALKM)
  690 CONTINUE
      CALL JEDEMA()
      END
