      SUBROUTINE OP0151()
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
C       ----------------------------------------------------------------
C       C A L C _ F A T I G U E
C       ----------------------------------------------------------------
C       CREATION D UN CHAM_ELEM D ISODOMMAGE A LA FATIGUE
C       D UN MATERIAU SOUMIS A UN CYCLAGE EN CONTRAINTES
C       A PARTIR D'UN CHAM_ELEM DE GRANDEUR 1D EQUIVALENTE
C       ----------------------------------------------------------------
C       ATTENTION : LE CHAM_ELEM EN SORTIE EST CONSTRUIT ARTIFICIELEMENT
C                   SANS PASSER PAR LA ROUTINE CALCUL
C                   A PARTIR DU CHAM_ELEM DANS LA SD RESULTAT
C                   DE TYPE EVOL_NOLI , EVOL_ELAS , DYNA_TRANS, CREE
C                   PAR CALC_ELEM (OPTIONS SIGM(EPSI)_EQUI_ELNO(ELGA))
C                   LA COHERENCE DU CHAMP EST DONC LIEE A CE DERNIER
C                   (MEMES ELEMENTS/GREL POUR LE CALCUL DE L OPTION ...)
C
C       IMPLANTE  : ACTUELLEMENT  :
C                  - CALCUL DU DOMMAGE A PARTIR DE   = /CONTRAINTE
C                                                      /DEFORMATION
C                  - POINTS DE CALCUL DU DOMMAGE     = /NOEUDS
C                                                      /POINTS DE GAUSS
C                  - COMPOSANTES GRANDEUR EQUIVALENTE= VMIS_SG....
C                  - METHODE  D'EXTRACTION DES PICS  = RAINFLOW
C                  - METHODE  DE COMPTAGE DES CYCLES = RAINFLOW
C                  - METHODE  DE CALCUL   DU DOMMAGE = /WOHLER
C                                                      /MANSON_COFFIN
C                                                      /TAHERI_MANSON
C                                                      /TAHERI_MIXTE
C                  - OPTIONS OUT                     = /DOMA_ELNO_SIGM
C                                                      /DOMA_ELGA_SIGM
C                                                      /DOMA_ELGA_EPSI
C                                                      /DOMA_ELNO_EPSI
C                                                      /DOMA_ELGA_EPME
C                                                      /DOMA_ELNO_EPME
C       ----------------------------------------------------------------
C       ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------
        INTEGER          ZI
        COMMON  /IVARJE/ ZI(1)
        REAL*8           ZR
        COMMON  /RVARJE/ ZR(1)
        COMPLEX*16       ZC
        COMMON  /CVARJE/ ZC(1)
        LOGICAL          ZL
        COMMON  /LVARJE/ ZL(1)
        CHARACTER*8      ZK8
        CHARACTER*16            ZK16
        CHARACTER*24                    ZK24
        CHARACTER*32                            ZK32
        CHARACTER*80                                    ZK80
        COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
        CHARACTER*32       JEXNOM,JEXNUM
C       ---------------------------------------------------------------
        INTEGER         ICODRE,ICODWO,ICODBA,ICODHS,ICODMA
        INTEGER         NPARMA, ID, NP
        CHARACTER*8     NOMU,NOMRES,NOMMAI,K8B,NOMMAT, NOMPF(8)
        CHARACTER*8     NOMFON,NOMNAP,CARA,NOMMOD,GRDVIE
        CHARACTER*8     NOMPA1(3), NOMPA2(8)
        CHARACTER*16    CONCEP,CMD,PHENO,PHENOM,TYPCAL,NOMCRI,NOMMET
        CHARACTER*16    PROAXE,NOMSYM,TYPCHA,NOMOPT,NOMGDE, NOMFOR
        CHARACTER*16    FORVIE
        CHARACTER*16    MEXPIC,MCOMPT,MDOMAG,TYPEQ,TYPOI,TYPDG,OPTION
        CHARACTER*19    NOMSD,CHELEM,CHELRS,LIGREL,NOMSD2
        CHARACTER*24    VALK(6), CHNOM, CBID
        LOGICAL         FORDEF,GRDEXI
        REAL*8           RBID
C
        INTEGER         NVAL,IMPR,IFM,JORDR,JCOEF,JCELK,JCELV
        INTEGER         NBPT,NBORD,NBCMP,NUMCMP(6),NTCMP,IBID,IUNIFI
        INTEGER         IVDMG,NUMSYM,NBPT2,NBORD2,IRET,IVCH
        INTEGER VALI(2)
      INTEGER      IARG

C     ---------------------------------------------------------------
        DATA  NOMPA1/  'DTAUMA', 'PHYDRM' , 'NORMAX'/
C     ---------------------------------------------------------------
C     ---------------------------------------------------------------
        DATA  NOMPA2/  'TAUPR_1','TAUPR_2','SIGN_1','SIGN_2',
     &                 'PHYDR_1','PHYDR_2','EPSPR_1', 'EPSPR_2'  /
C       -------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      NOMFON = ' '
      NOMNAP = ' '
C
C ----- DONNEES UTILISATEUR
C
      CALL GETRES(NOMU,CONCEP,CMD)
C
C ---   TYPE DE CALCUL
      CALL GETVTX(' ','TYPE_CALCUL',1,IARG,1,TYPCAL,NVAL)
C
C ---------------------------------------------------------------------
C ---- FATIGUE MULTIAXIALE
C ---------------------------------------------------------------------
C
      IF (TYPCAL(1:13) .EQ. 'FATIGUE_MULTI') THEN
C
C ---   TYPE DU CHARGEMENT APPLIQUE (PERIODIQUE OU NON_PERIODIQUE)
C
        CALL GETVTX(' ','TYPE_CHARGE',1,IARG,1,TYPCHA,NVAL)
C
C ---   NOM DE L'OPTION (CALCUL AUX POINTS DE GAUSS OU AUX NOEUDS
C                        CHAM_NO)
        CALL GETVTX(' ','OPTION',1,IARG,1,NOMOPT,NVAL)
C
C ---   STRUCTURE RESULTAT CONTENANT LES CHAM_ELEMS DE SIGMA
C       OU LES CHAM_NOS DE SIGMA
        CALL GETVID(' ','RESULTAT',1,IARG,1,NOMRES,NVAL)
        NOMSD = NOMRES
C
C ---   NOM DU CRITERE
        CALL GETVTX(' ','CRITERE',1,IARG,1,NOMCRI,NVAL)

        CALL GETVID(' ','FORMULE_GRDEQ',1,IARG,1,NOMFOR,NVAL)

C   FORDEF EST UNE BOOLEAN QUI INDIQUE S'IL EXISTE LE PARAMETRE
C   DE DEFORMATION DAS LA FORMULE (COMME DANS FATEMISOCIE)

         FORDEF =  .FALSE.
         IF (NOMCRI(1:7) .EQ. 'FORMULE') THEN
C NOMBRE DE PARAMETRES DISPONIBLES
            NPARMA = 8
C RECUPERER LES NOMS DE PARAMETRES FOURNIS PAR L'UTILISATEUR
            CHNOM(20:24) = '.PROL'
            CHNOM(1:19) = NOMFOR

            CALL JEVEUO(CHNOM,'L',JPROF)
            CALL FONBPA ( NOMFOR, ZK24(JPROF), CBID, NPARMA, NP, NOMPF )

C VERIFIER QUE LE NOM DE GRANDEUR A CALCULER EST BON
            IF (TYPCHA .EQ. 'NON_PERIODIQUE') THEN
                DO 10 ID = 1, NP
                   GRDEXI = .FALSE.
                   DO 40 I = 1,NPARMA
                      IF  ( NOMPF(ID) .EQ. NOMPA2(I) ) THEN
                         GRDEXI = .TRUE.
                      ENDIF
40                 CONTINUE
                   IF ( .NOT. GRDEXI) THEN
                      CALL U2MESK('F','FATIGUE1_91',1, NOMPF(ID))
                   ENDIF

                   IF ( NOMPF(ID)(1:3) .EQ. 'EPS') THEN
                      FORDEF =  .TRUE.
                      DO 20 I = 1, NP
                         IF ( NOMPF(I)(1:3) .EQ. 'TAU') THEN
                            CALL U2MESS('F','FATIGUE1_92')
                         ENDIF
20                    CONTINUE
                   ENDIF
                   IF ( NOMPF(ID)(1:3) .EQ. 'TAU') THEN
                      DO 30 I = 1, NP
                         IF ( NOMPF(I)(1:3) .EQ. 'EPS') THEN
                            CALL U2MESS('F','FATIGUE1_92')
                         ENDIF
30                    CONTINUE
                   ENDIF
10              CONTINUE

            ELSE

                DO 60 ID = 1, NP
                   GRDEXI = .FALSE.
                   DO 50 I = 1,NPARMA
                      IF  ( NOMPF(ID) .EQ. NOMPA1(I) ) THEN
                         GRDEXI = .TRUE.
                      ENDIF
50                 CONTINUE

                   IF ( .NOT. GRDEXI) THEN
                      CALL U2MESK('F','FATIGUE1_91',1, NOMPF(ID))
                   ENDIF

60              CONTINUE
            ENDIF

      ENDIF


        CALL GETVTX(' ','COURBE_GRD_VIE',1,IARG,1,GRDVIE,NVAL)

        CALL GETVID(' ','FORMULE_VIE',1,IARG,1,FORVIE,NVAL)
C
C ---   NOM DE LA METHODE PERMETTANT DE DETERMINER LE CERCLE CIRCONSCRIT
        CALL GETVTX(' ','METHODE',1,IARG,1,NOMMET,NVAL)
        IF (NVAL .EQ. 0) THEN
          NOMMET = '        '
        ENDIF
C
C ---   PROJECTION SUR UN AXE OU SUR DEUX AXES
C       (CHARGEMENT NON_PERIODIQUE UNIQUEMENT)
        CALL GETVTX(' ','PROJECTION',1,IARG,1,PROAXE,NVAL)
        IF (NVAL .EQ. 0) THEN
          PROAXE = '        '
        ENDIF
C
C ---   NOM DU MAILLAGE
        CALL GETVID(' ','MAILLAGE',1,IARG,1,NOMMAI,NVAL)
        IF (NVAL .EQ. 0) THEN
          NOMMAI = '        '
        ENDIF
C
        IF (NOMOPT .EQ. 'DOMA_ELGA') THEN
C
C ---   CONSTRUCTION DES PAQUETS DE MAILLES
          CALL PAQMAI(NOMRES, NOMU, NOMMAI, NOMMET, NOMCRI,NOMFOR,
     &                GRDVIE, FORVIE, FORDEF, TYPCHA, PROAXE)
C
        ELSEIF (NOMOPT .EQ. 'DOMA_NOEUD') THEN
C
C ---   CONSTRUCTION DES PAQUETS DE NOEUDS
          CALL PAQNOE(NOMRES, NOMU, NOMMAI, NOMMET, NOMCRI,NOMFOR,
     &                GRDVIE, FORVIE, FORDEF, TYPCHA, PROAXE)
        ENDIF

C
        GOTO 7777
      ENDIF
C
C ---------------------------------------------------------------------
C ---- CAS GENERAL (CUMUL DE DOMMAGE OU FATIGUE MODALE)
C ---------------------------------------------------------------------
C
C ---   NOM DE LA GRANDEUR EQUIVALENTE
      CALL GETVTX('HISTOIRE','EQUI_GD',1,IARG,1,NOMGDE,NVAL)
C
C ---   IMPRESSIONS
      CALL GETVIS(' ' ,'INFO',1,IARG,1,IMPR,NVAL)
C
C ---   CHAMP : NOM DE L'OPTION RESULTANTE
C   'DOMA_ELNO_SIGM'/'DOMA_ELGA_SIGM'/'DOMA_ELNO_EPSI'/'DOMA_ELGA_EPSI
C   'DOMA_ELNO_EPME'/'DOMA_ELGA_EPME'
C
      CALL GETVTX(' ','OPTION',1,IARG,1,NOMOPT,NVAL)
C
C ---   NOM DE LA METHODE DE CALCUL DU DOMMAGE
      CALL GETVTX(' ','DOMMAGE',1,IARG,1,MDOMAG,NVAL)
      CALL GETVID(' ','MATER',1,IARG,1,NOMMAT,NVAL)
C
C ---   NOMBRE DE NUMEROS D ORDRE DE POINTS (NOEUDS/PG) DE CMPS ...
C       ---------------------------------------------------------------
C       NOM SYMBOLIQUE DE L OPTION IN UTILISEE (GRANDEUR EQUIVALENTE)
C       (SIEQ_ELNO  SIEQ_ELGA  EPEQ_ELGA EPEQ_ELNO
C        EPMQ_ELNO  EPMQ_ELGA  )
C       ET NOMBRE TOTAL DE COMPOSANTES DE CETTE OPTION
C
      IF (NOMOPT(11:14).EQ.'SIGM') THEN
        NOMSYM='SIEQ_'//NOMOPT(6:9)
      ELSEIF (NOMOPT(11:14).EQ.'EPSI') THEN
        NOMSYM='EPEQ_'//NOMOPT(6:9)
      ELSEIF (NOMOPT(11:14).EQ.'EPME') THEN
        NOMSYM='EPMQ_'//NOMOPT(6:9)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

C
      IF ( NOMOPT(6:14) .EQ. 'ELGA_SIGM' ) THEN
         NTCMP = 17
      ELSEIF( NOMOPT(6:14) .EQ. 'ELNO_SIGM' ) THEN
         NTCMP = 7
      ELSEIF( NOMOPT(6:14) .EQ. 'ELGA_EPSI' ) THEN
         NTCMP = 14
      ELSEIF( NOMOPT(6:14) .EQ. 'ELNO_EPSI' ) THEN
         NTCMP = 5
      ELSEIF( NOMOPT(6:14) .EQ. 'ELGA_EPME' ) THEN
         NTCMP = 14
      ELSEIF( NOMOPT(6:14) .EQ. 'ELNO_EPME' ) THEN
         NTCMP = 5
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF

C       TYPE DE GRANDEUR EQUIVALENTE UTILISEE LE POUR CALCUL DU DOMMAGE
C       ET NOMBRE DE COMPOSANTES DE CETTE GRANDEUR
C       VMIS_SIG   = CMP NUMERO 6       DE  L OPTION EQUI_...._SIGM
C       INVA_2_SG  = CMP NUMERO 5       DE  L OPTION EQUI_...._EPSI
C       INVA_2_SG  = CMP NUMERO 5       DE  L OPTION EQUI_...._EPME
C
      IF(NOMGDE(1:7).EQ.'VMIS_SG') THEN
        NUMCMP(1) = 6
        NBCMP     = 1
      ELSEIF(NOMGDE(1:9).EQ.'INVA_2_SG') THEN
        NUMCMP(1) = 5
        NBCMP     = 1
      ENDIF

C ---------------------------------------------------------------------
C ---- CUMUL DE DOMMAGE
C ---------------------------------------------------------------------
C
      IF (TYPCAL(1:13) .EQ. 'CUMUL_DOMMAGE') THEN
C
C ---   STRUCTURE RESULTAT CONTENANT LES CHAM_ELEMS DE SIGMA EQUIVALENT
      CALL GETVID('HISTOIRE','RESULTAT',1,IARG,1,NOMRES,NVAL)
      NOMSD = NOMRES
C
      IF(MDOMAG.EQ.'WOHLER') THEN
        IF( NOMOPT(11:14).NE.'SIGM' ) THEN
          CALL U2MESK('F','FATIGUE1_29',1,NOMOPT)
        ENDIF
        PHENO = 'FATIGUE'
        CALL RCCOME (NOMMAT,PHENO,PHENOM,ICODRE)
        IF(ICODRE.EQ.1) CALL U2MESS('F','FATIGUE1_24')
        CARA = 'WOHLER'
        CALL RCPARE(NOMMAT,PHENO,CARA,ICODWO)
        CARA = 'A_BASQUI'
        CALL RCPARE(NOMMAT,PHENO,CARA,ICODBA)
        CARA = 'A0'
        CALL RCPARE(NOMMAT,PHENO,CARA,ICODHS)
        IF(ICODWO.NE.0.AND.ICODBA.NE.0.AND.ICODHS.NE.0)
     &     CALL U2MESS('F','FATIGUE1_30')
      ELSEIF(MDOMAG.EQ.'MANSON_COFFIN') THEN
        IF(NOMOPT(11:14).NE.'EPSI'.AND.NOMOPT(11:14).NE.'EPME') THEN
          CALL U2MESK('F','FATIGUE1_31',1,NOMOPT)
        ENDIF
        PHENO = 'FATIGUE'
        CALL RCCOME (NOMMAT,PHENO,PHENOM,ICODRE)
        IF(ICODRE.EQ.1) CALL U2MESS('F','FATIGUE1_24')
        CARA = 'MANSON_C'
        CALL RCPARE(NOMMAT,PHENO,CARA,ICODMA)
        IF(ICODMA.NE.0)
     &    CALL U2MESS('F','FATIGUE1_32')
      ELSEIF(MDOMAG.EQ.'TAHERI_MANSON') THEN
        IF(NOMOPT(11:14).NE.'EPSI'.AND.NOMOPT(11:14).NE.'EPME') THEN
          CALL U2MESK('F','FATIGUE1_25',1,NOMOPT)
        ENDIF
        PHENO = 'FATIGUE'
        CALL RCCOME (NOMMAT,PHENO,PHENOM,ICODRE)
        IF(ICODRE.EQ.1) CALL U2MESS('F','FATIGUE1_24')
        CARA = 'MANSON_C'
        CALL RCPARE(NOMMAT,PHENO,CARA,ICODMA)
        IF(ICODMA.NE.0)
     &     CALL U2MESS('F','FATIGUE1_32')
        CALL GETVID(' ','TAHERI_NAPPE',1,IARG,1,NOMNAP,NVAL)
        IF(NVAL.EQ.0) THEN
          CALL U2MESS('F','FATIGUE1_26')
        ENDIF
        CALL GETVID(' ','TAHERI_FONC',1,IARG,1,NOMFON,NVAL)
          IF(NVAL.EQ.0) THEN
            CALL U2MESS('F','FATIGUE1_27')
          ENDIF
      ELSEIF(MDOMAG.EQ.'TAHERI_MIXTE') THEN
        IF(NOMOPT(11:14).NE.'EPSI'.AND.NOMOPT(11:14).NE.'EPME') THEN
          CALL U2MESK('F','FATIGUE1_28',1,NOMOPT)
        ENDIF
        PHENO = 'FATIGUE'
        CALL RCCOME (NOMMAT,PHENO,PHENOM,ICODRE)
        IF(ICODRE.EQ.1) CALL U2MESS('F','FATIGUE1_24')
        CARA = 'MANSON_C'
        CALL RCPARE(NOMMAT,PHENO,CARA,ICODMA)
        IF(ICODMA.NE.0)
     &     CALL U2MESS('F','FATIGUE1_32')
        CARA = 'WOHLER'
        CALL RCPARE(NOMMAT,PHENO,CARA,ICODWO)
        CARA = 'A_BASQUI'
        CALL RCPARE(NOMMAT,PHENO,CARA,ICODBA)
        CARA = 'A0'
        CALL RCPARE(NOMMAT,PHENO,CARA,ICODHS)
        IF(ICODWO.NE.0.AND.ICODBA.NE.0.AND.ICODHS.NE.0)
     &     CALL U2MESS('F','FATIGUE1_30')
        CALL GETVID(' ','TAHERI_NAPPE',1,IARG,1,NOMNAP,NVAL)
        IF(NVAL.EQ.0) THEN
          CALL U2MESS('F','FATIGUE1_26')
        ENDIF
C
      ENDIF
C
C --- VERIFICATION DU NOMBRE DE PAS DE TEMPS
      CALL JELIRA(NOMSD//'.ORDR','LONUTI',NBORD,K8B)
      IF (NBORD .LT. 2) THEN
         CALL U2MESI('F', 'FATIGUE1_76',1,NBORD)
      ENDIF
C
      CALL JENONU(JEXNOM(NOMSD//'.DESC',NOMSYM),NUMSYM)
        IF(NUMSYM.EQ.0) THEN
           VALK(1) = NOMSYM
           VALK(2) = NOMSD
           CALL U2MESK('F','PREPOST4_5', 2 ,VALK)
        ENDIF
      CALL JEVEUO(JEXNUM(NOMSD//'.TACH',NUMSYM),'L',IVCH)
      CHELRS = ZK24(IVCH)(1:19)
      IF(CHELRS.EQ.' ') THEN
         VALK(1) = CHELRS
         VALK(2) = NOMSYM
         VALK(3) = NOMSD
         CALL U2MESK('F','PREPOST4_6', 3 ,VALK)
      ENDIF
      CALL JEVEUO ( CHELRS//'.CELK','L',JCELK )
      LIGREL=ZK24(JCELK-1+1)
      CALL JELIRA ( CHELRS//'.CELV','LONMAX',NVAL,K8B )
C
C  -      IL Y A NTCMP COMPOSANTES DANS L OPTION XXXX_EQUI_YYYY
      NBPT = NVAL / NTCMP
C
      IF(IMPR.GE.2) THEN
        VALI (1) = NBORD
        VALI (2) = NBPT
        CALL U2MESG('I','PREPOST6_27',0,' ',2,VALI,0,0.D0)
      ENDIF
C
C ----- CALCUL DU VECTEUR DOMMAGE EN CHAQUE NOEUD/PG
C       ----------------------------------------------------------------
C
      CALL WKVECT( '&&OP0151.DOMMAGE' , 'V V R', NBPT , IVDMG )
C
      MEXPIC = 'RAINFLOW'
      MCOMPT = 'RAINFLOW'
      IF(MDOMAG(1:6).EQ.'TAHERI') MCOMPT = 'TAHERI'
C
      IF(IMPR.GE.2) THEN
        TYPEQ = NOMGDE
        IF(NOMOPT(11:14).EQ.'SIGM') TYPDG = 'CONTRAINTE'
        IF(NOMOPT(11:14).EQ.'EPSI') TYPDG = 'DEFORMATION'
        IF(NOMOPT(11:14).EQ.'EPME') TYPDG = 'DEFORMATION'
        IF(NOMOPT(6:9).EQ.'ELNO')   TYPOI = 'NOEUDS'
        IF(NOMOPT(6:9).EQ.'ELGA')   TYPOI = 'POINTS DE GAUSS'
        VALK (1) = TYPDG
        VALK (2) = TYPOI
        VALK (3) = TYPEQ
        VALK (4) = MEXPIC
        VALK (5) = MCOMPT
        VALK (6) = MDOMAG
        CALL U2MESG('I','PREPOST6_28',6,VALK,0,0,0,0.D0)
      ENDIF
C
      CALL FGVDMG(NOMSYM,NOMSD,NOMMAT,NOMNAP,NOMFON,MEXPIC,MCOMPT,
     &            MDOMAG,NBORD,NBPT,NTCMP,NBCMP,NUMCMP,
     &            IMPR,ZR(IVDMG))
C
      IF(IMPR.GE.2) THEN
        IFM = IUNIFI('MESSAGE')
        CALL JEIMPO(IFM,'&&OP0151.DOMMAGE',' ','DOMMAGE')
      ENDIF
C
C ---------------------------------------------------------------------
C ---- FATIGUE VIBRATOIRE
C ---------------------------------------------------------------------
C
      ELSEIF (TYPCAL(1:13) .EQ. 'FATIGUE_VIBR') THEN
C ---   STRUCTURE RESULTAT CONTENANT LES CHAM_ELEMS DE SIGMA EQUIVALENT
        CALL GETVID('HISTOIRE','RESULTAT',1,IARG,1,NOMRES,NVAL)
        NOMSD = NOMRES
        CALL GETVID('HISTOIRE','MODE_MECA',1,IARG,1,NOMMOD,NVAL)
        NOMSD2 = NOMMOD
C
        IF(MDOMAG.EQ.'WOHLER') THEN
          IF( NOMOPT(11:14).NE.'SIGM' ) THEN
            CALL U2MESK('F','FATIGUE1_29',1,NOMOPT)
          ENDIF
          PHENO = 'FATIGUE'
          CALL RCCOME (NOMMAT,PHENO,PHENOM,ICODRE)
          IF(ICODRE.EQ.1) CALL U2MESK('F','FATIGUE1_88', 1 ,'WOHLER')
          CALL RCPARE(NOMMAT,PHENO,'WOHLER',ICODRE)
          IF(ICODRE.EQ.1) CALL U2MESK('F','FATIGUE1_88', 1 ,'WOHLER')
        ENDIF
C
C --- CONTRAINTE STATIQUE
C
      CALL JELIRA(NOMSD//'.ORDR','LONUTI',NBORD,K8B)
      IF (NBORD .GT. 1) THEN
         CALL U2MESI('F', 'FATIGUE1_84',1,NBORD)
      ENDIF
C
      CALL JENONU(JEXNOM(NOMSD//'.DESC',NOMSYM),NUMSYM)
        IF(NUMSYM.EQ.0) THEN
           VALK(1) = NOMSYM
           VALK(2) = NOMSD
           CALL U2MESK('F','PREPOST4_5', 2 ,VALK)
        ENDIF
      CALL JEVEUO(JEXNUM(NOMSD//'.TACH',NUMSYM),'L',IVCH)
      CHELRS = ZK24(IVCH)(1:19)
      IF(CHELRS.EQ.' ') THEN
         VALK(1) = CHELRS
         VALK(2) = NOMSYM
         VALK(3) = NOMSD
         CALL U2MESK('F','PREPOST4_6', 3 ,VALK)
      ENDIF
      CALL JEVEUO ( CHELRS//'.CELK','L',JCELK )
      LIGREL=ZK24(JCELK-1+1)
      CALL JELIRA ( CHELRS//'.CELV','LONMAX',NVAL,K8B )
C
C  -      IL Y A NTCMP COMPOSANTES DANS L OPTION XXXX_EQUI_YYYY
      NBPT = NVAL / NTCMP
C
C --- CONTRAINTE MODALE
C
      CALL JENONU(JEXNOM(NOMSD2//'.DESC',NOMSYM),NUMSYM)
        IF(NUMSYM.EQ.0) THEN
           VALK(1) = NOMSYM
           VALK(2) = NOMSD2
           CALL U2MESK('F','PREPOST4_5', 2 ,VALK)
        ENDIF
      CALL JEVEUO(JEXNUM(NOMSD2//'.TACH',NUMSYM),'L',IVCH)
      CHELRS = ZK24(IVCH)(1:19)
      IF(CHELRS.EQ.' ') THEN
         VALK(1) = CHELRS
         VALK(2) = NOMSYM
         VALK(3) = NOMSD2
         CALL U2MESK('F','PREPOST4_6', 3 ,VALK)
      ENDIF
      CALL JEVEUO ( CHELRS//'.CELK','L',JCELK )
      LIGREL=ZK24(JCELK-1+1)
      CALL JELIRA ( CHELRS//'.CELV','LONMAX',NVAL,K8B )
C
C  -      IL Y A NTCMP COMPOSANTES DANS L OPTION XXXX_EQUI_YYYY
      NBPT2 = NVAL / NTCMP
C
      IF(NBPT .NE. NBPT2) THEN
        VALI (1) = NBPT
        VALI (2) = NBPT2
        CALL U2MESI('F','FATIGUE1_85',2,VALI)
      ENDIF
C
C-- NOMBRE ET NUMERO D ORDRE
      CALL GETVIS('HISTOIRE' ,'NUME_MODE',1,IARG,0,IBID,NBORD)
      CALL GETVR8('HISTOIRE' ,'FACT_PARTICI',1,IARG,0,RBID,NBORD2)

      IF(NBORD .NE. NBORD2) THEN
        CALL U2MESS('F','FATIGUE1_86')
      ENDIF

      NBORD = -NBORD
      CALL WKVECT('&&OP0151.LMODE','V V I',NBORD,JORDR)
      CALL GETVIS('HISTOIRE','NUME_MODE',1,IARG,NBORD,ZI(JORDR),IBID)
      CALL WKVECT('&&OP0151.CMODE','V V R',NBORD,JCOEF)
      CALL GETVR8('HISTOIRE','FACT_PARTICI',1,IARG,NBORD,ZR(JCOEF),IBID)
C
      IF(IMPR.GE.1) THEN
        TYPEQ = NOMGDE
        TYPDG = 'CONTRAINTE'
        IF(NOMOPT(6:9).EQ.'ELNO')   TYPOI = 'NOEUDS'
        IF(NOMOPT(6:9).EQ.'ELGA')   TYPOI = 'POINTS DE GAUSS'
        VALK (1) = TYPDG
        VALK (2) = TYPOI
        VALK (3) = TYPEQ
        VALI (1) = NBPT
        VALI (2) = NBORD
        CALL U2MESG('I','FATIGUE1_81',3,VALK,2,VALI,0,0.D0)
      ENDIF
C
      CALL WKVECT( '&&OP0151.DOMMAGE' , 'V V R', NBPT , IVDMG )
      CALL DMGMOD(NOMSYM,NOMSD,NOMSD2,NOMMAT,
     &            NBORD,JORDR,JCOEF,NBPT,NTCMP,NUMCMP,
     &            IMPR,ZR(IVDMG))

      CALL JEDETC('V','&&OP0151.LMODE',1)
      CALL JEDETC('V','&&OP0151.CMODE',1)
C
      ENDIF
C
C ----- TRANSFORMATION DU VECTEUR DOMMAGE EN UN VRAI CHAM_ELEM
C       ----------------------------------------------------------------
C       ON ALLOUE LE CHAM_ELEM AVEC LA ROUTINE ALCHML
C       PUIS ON RECOPIE DANS .CELV LES VALEURS CALCULEES
C
      CHELEM = NOMU

      OPTION='TOU_INI_'//NOMOPT(6:9)
      CALL ALCHML(LIGREL,OPTION,'PDOMMAG','G',CHELEM,IRET,' ')
      CALL ASSERT(IRET.EQ.0)


      CALL JEVEUO(CHELEM//'.CELV','E',JCELV)
      CALL JELIRA(CHELEM//'.CELV','LONMAX',IBID,K8B)
      CALL ASSERT(IBID.EQ.NBPT)
      DO 222  I = 1,NBPT
        ZR(JCELV+I-1) = ZR(IVDMG+I-1)
 222  CONTINUE
      CALL JEDETC('V','&&OP0151.DOMMAGE',1)

 7777 CONTINUE
C
      CALL JEDEMA()
      END
