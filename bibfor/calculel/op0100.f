      SUBROUTINE OP0100(IER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 15/01/2008   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C      OPERATEUR :     CALC_G
C
C      BUT:CALCUL DU TAUX DE RESTITUTION D'ENERGIE PAR LA METHODE THETA,
C          CALCUL DES FACTEURS D'INTENSITE DE CONTRAINTES, ...
C ======================================================================
C TOLE CRP_20
C
      IMPLICIT NONE
C 0.1  ==> ARGUMENTS
      INTEGER IER
C 0.2. ==> COMMUNS
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C 0.3. ==> VARIABLES LOCALES
C
      INTEGER IFM,NIV,N1,N2,LONVEC,IORD, IBID,I,IAD,NCAS,JNCAS,JNORD
      INTEGER VALI,NBTROU,NUTROU,INDIIS,IIND
      INTEGER NRES,NP,NC,NEXCI,NCHA,IRET,ICHA,IVEC,DIME,NBPRUP
      INTEGER IFOND,LNOFF,TYPESE,JINST,IAUX,JAUX,NBPASS,ADRECG,NRPASE
      INTEGER ADCHSE,ICHAR,NBPASE,NRPASS,NDEG,NBRE,IADNUM,IADRMA,IPROPA
      INTEGER ITHETA,NBR8,NBV,IADRCO,IADRNO,NBNO,J,IPULS,IORD1,IORD2
      INTEGER NBORN,NBCO,IBOR,IG,LNOEU,LABSCU,NBVAL,NBTYCH
      INTEGER IFF,IFOMUL,NUMFON,IDEPFI,IARRFI,IFON,NDIMTE
      PARAMETER (NBTYCH=17)
C
      REAL*8  TIME,TIMEU,TIMEV,PREC,ALPHA,R8B,DIR(3)
      REAL*8  RINF,RSUP,MODULE,VAL(2),PULS
C
      COMPLEX*16 CBID,C16B
C
      CHARACTER*4 K4B
      CHARACTER*5 SUFFIX
      CHARACTER*6 NOMPRO,NOMLIG(NBTYCH)
      PARAMETER ( NOMPRO = 'OP0100' )
      CHARACTER*8 TABLE1,MODELE,MATERI,RESUCO,K8B,K8BID
      CHARACTER*8 FOND,FISS,TYPRUP(9),SYMECH,NOPASE,MATERS,AFFCHA,CRIT
      CHARACTER*8 LATABL,LERES0,NOMA,THETAI,NOEUD,TYPEPS(-2:NBTYCH)
      CHARACTER*13 INPSCO
      CHARACTER*16 TYPCO,OPER,OPTION,TYSD, NOPRUP(9),OPTIO1,SUITE,NOMCAS
      CHARACTER*16 OPTIO2,K16B
      CHARACTER*19 GRLT,GRLN
      CHARACTER*19 OPTIO3,VCHAR
      CHARACTER*24 DEPLA,MATE,K24B,COMPOR,CHVITE,CHACCE,VECORD
      CHARACTER*24 VALK(3)
      CHARACTER*24 SDTHET,CHFOND,BASLOC,CHDESE,CHEPSE,CHSISE,THETA
      CHARACTER*24 NORECG,MATES,CHARSE,NOMCHA,LIGRCH,LCHIN,STYPSE
      CHARACTER*24 BLAN24,LISSTH,LISSG,OBJMA,NOMNO,COORN
      CHARACTER*24 TRAV1,TRAV2,TRAV3,STOK4,THETLG
      CHARACTER*24 OBJ1,TRAV4,COURB,DEPLA1,DEPLA2
C
      LOGICAL EXITIM,EXCHSE,TROIDL,THLAGR,CONNEX,GLAGR,MILIEU,DIREC
      LOGICAL THLAG2,PAIR,LNCAS,LMELAS
C
      DATA NOMLIG/'.FORNO','.F3D3D','.F2D3D','.F1D3D','.F2D2D','.F1D2D',
     &     '.F1D1D','.PESAN','.ROTAT','.PRESS','.FELEC','.FCO3D',
     &     '.FCO2D','.EPSIN','.FLUX','.VEASS','.ONDPL'/
      DATA TYPEPS/'MATERIAU','CARAELEM','DIRICHLE','FORCE   ',
     &     'FORCE   ','FORCE   ','FORCE   ','FORCE   ','FORCE   ',
     &     'FORCE   ','.PESAN','.ROTAT','FORCE   ','.FELEC','FORCE   ',
     &     'FORCE   ','.EPSIN','.FLUX','.VEASS','.ONDPL'/
C
C==============
C 1. PREALABLES
C==============
C
      CALL JEMARQ()
C               12   345678   9012345678901234
      VCHAR  = '&&'//NOMPRO//'.CHARGES'
      COURB  = '&&'//NOMPRO//'.COURB'
      INPSCO = '&&'//NOMPRO//'_PSCO'
      NORECG = '&&'//NOMPRO//'_PARA_SENSI     '
C               123456789012345678901234
      BLAN24 = '                        '
      TRAV1='&&'//NOMPRO//'.TRAV1'
      TRAV2='&&'//NOMPRO//'.TRAV2'
      TRAV3='&&'//NOMPRO//'.TRAV3'
      TRAV4='&&'//NOMPRO//'.TRAV4'
      STOK4='&&'//NOMPRO//'.STOK4'
C
C===============================
C  2. RECUPERATION DES OPERANDES
C===============================
C
C ---------------------------------------------
C 2.1. ==>  RECUPERATION DU NIVEAU D'IMPRESSION
C ---------------------------------------------
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
C-----------------------------------------------------
C 2.2. ==> LE CONCEPT DE SORTIE, SON TYPE, LA COMMANDE
C-----------------------------------------------------
      CALL GETRES(TABLE1,TYPCO,OPER)
      IF ( NIV.GE.2 ) THEN
        CALL U2MESK('I','CALCULEL4_9',1,TABLE1)
      ENDIF
C
C----------------
C 2.3. ==> OPTION
C----------------
      CALL GETVTX(' ','OPTION',0,1,1,OPTION,IBID)
C
      CALL GETVID (' ','RESULTAT',0,1,1,RESUCO,NRES)
      VECORD = '&&'//NOMPRO//'.VECTORDR'
      CALL GETVR8(' ','PRECISION',0,1,1,PREC,NP)
      CALL GETVTX(' ','CRITERE'  ,0,1,1,CRIT,NC)
      CALL RSUTNU ( RESUCO, ' ', 0, VECORD, LONVEC, PREC, CRIT, IER )
      IF(IER.NE.0) THEN
        CALL U2MESS('F','CALCULEL4_13')
      ENDIF
      CALL GETTCO(RESUCO,TYSD)
      IF (TYSD.EQ.'DYNA_TRANS') THEN
         CALL GETFAC('EXCIT',NEXCI)
         IF (NEXCI.EQ.0 ) THEN
           CALL U2MESS('F','CALCULEL4_16')
         ENDIF
      ELSEIF (TYSD.EQ.'MULT_ELAS') THEN
C 
C        ON INFORME L'UTILISATEUR PAR UNE ALARME DE RENSEIGNER
C        LES MOTS-CLES "EXCIT" ET "NOM_CAS".
C        UNE EVENTUELLE EVOLUTION DU STOCKAGE DE LA SD CHARGE DANS
C        LA SD RESULTAT ISSUE DE MACRO_ELAS_MULT PERMETTRA DE SE
C        PASSER DE LA PRESENCE DE CES MOTS-CLES.
         CALL GETFAC('EXCIT',NEXCI)
         CALL GETVID (' ','NOM_CAS',0,1,0,K8B,IRET)
         IF (NEXCI.EQ.0 .OR. IRET.EQ.0) THEN
           CALL U2MESS('A','RUPTURE0_55')
         ENDIF
      ENDIF

      IF (((OPTION.EQ.'K_G_MODA') .AND. (TYSD.NE.'MODE_MECA')) .OR.
     &   ((TYSD.EQ.'MODE_MECA') .AND. (OPTION.NE.'K_G_MODA'))) THEN
        CALL U2MESS('F','CALCULEL4_17')
      ENDIF

      CALL JEVEUO ( VECORD, 'L', IVEC )
      IORD = ZI(IVEC)
      CALL MEDOM1(MODELE,MATE,K8B,VCHAR,NCHA,K4B,RESUCO,IORD)
      CALL DISMOI('F','DIM_GEOM',MODELE,'MODELE',DIME,K8B,IER)
      CALL NMDORC(MODELE,COMPOR,K24B)
      CALL GVERLC(RESUCO,COMPOR,IORD)

      CALL JEVEUO(VCHAR,'L',ICHA)

      LNCAS=.FALSE.
      LMELAS=.FALSE.
      IF (TYSD.EQ.'MULT_ELAS') THEN
          LMELAS=.TRUE.
          CALL GETVTX(' ','NOM_CAS',0,1,0,K16B,NCAS)

          IF(NCAS.NE.0)THEN
            NCAS=-NCAS
            CALL WKVECT('&&OP0100_MULTELAS_NOMCAS','V V K16',NCAS,JNCAS)
            CALL GETVTX(' ','NOM_CAS',0,1,NCAS,ZK16(JNCAS),IRET)
            LNCAS=.TRUE.
            CALL WKVECT('&&OP0100_MULTELAS_NUMORD','V V L',LONVEC,JNORD)
            DO 23 I=1,NCAS
              CALL RSORAC(RESUCO,'NOM_CAS',IBID,R8B,ZK16(JNCAS+I-1),
     &                    C16B,R8B,K8B,NUTROU,1,NBTROU)
              IF(NBTROU.EQ.0)THEN
                 VALK(1)=ZK16(JNCAS+I-1)
                 VALK(2)=RESUCO
                 CALL U2MESK('F','POSTRELE_64',2,VALK)
              ELSE
                 IIND=INDIIS(ZI(IVEC),NUTROU,LONVEC,1)
                 ZL(JNORD+IIND-1)=.TRUE.
              ENDIF
 23         CONTINUE
          ENDIF
      ENDIF
C
C------------------------------------------
C 2.6. ==> CAS: 2D, 3D LOCAL ou 3D GLOBAL ?
C------------------------------------------
      TROIDL=.FALSE.
      IF(DIME.EQ.3)THEN
         IF(OPTION.NE.'CALC_G_GLOB' .AND.
     &      OPTION.NE.'G_MAX_GLOB'  .AND.
     &      OPTION.NE.'G_BILI_GLOB' .AND.
     &      OPTION.NE.'G_LAGR_GLOB' )THEN
           TROIDL=.TRUE.
           CALL GETVID ( 'THETA','FOND_FISS', 1,1,1,K8B,N1)
           CALL GETVID ( 'THETA','FISSURE', 1,1,1, K8B,N2)
           IF((OPTION(1:6).EQ.'CALC_K' .OR. OPTION.EQ.'K_G_MODA')
     &        .AND.N2.EQ.0)THEN
              CALL U2MESK('F','CALCULEL4_19',1,OPTION(1:8))
           ELSEIF((OPTION(1:6).NE.'CALC_K' .AND. OPTION.NE.'K_G_MODA')
     &          .AND.N1.EQ.0)THEN
              CALL U2MESK('F','CALCULEL4_20',1,OPTION(1:8))
           ENDIF
         ENDIF
      ENDIF
      IF(DIME.EQ.2)THEN
        IF(OPTION.EQ.'CALC_K_G' .OR. OPTION.EQ.'K_G_MODA')THEN
           CALL GETVID ( 'THETA','FOND_FISS', 1,1,1,K8B,N1)
           CALL GETVID ( 'THETA','FISSURE'  , 1,1,1,K8B,N2)
           IF (N2.NE.0.AND.OPTION.EQ.'K_G_MODA')
     &         CALL U2MESS('F','XFEM_9')
           IF (N1+N2.EQ.0.OR.N1*N2.NE.0)
     &         CALL U2MESK('F','CALCULEL4_21',1,OPTION)
        ENDIF
      ENDIF
C
C----------------
C 2.7. ==> THETA
C----------------
      CALL GETVTX ( ' ', 'SYME_CHAR', 0,1,1, SYMECH,IBID)

      CALL GETVID ( 'THETA','THETA', 1,1,1, SDTHET,IRET)
      IF (TROIDL.AND.(OPTION.EQ.'CALC_G') .AND. (IRET.NE.0)) THEN
        CALL U2MESS('F','CALCULEL4_22')
      ENDIF
      IF(IRET.NE.0.AND.OPTION.EQ.'G_LAGR'.AND.TROIDL)THEN
        CALL U2MESK('F','CALCULEL4_23',1,OPTION)
      ENDIF

C ---  2.7.1 : THETA FOURNI
      IF(IRET.NE.0)THEN
        CALL GETTCO(SDTHET,TYPCO)
        IF (TYPCO(1:10).EQ.'THETA_GEOM') THEN
          CALL RSEXCH(SDTHET,'THETA',0,THETA,N1)
          IF (N1.GT.0) THEN
            CALL U2MESK('F','CALCULEL3_90',1,SDTHET)
          ENDIF
        ELSE
          THETA=SDTHET
        ENDIF
      ENDIF

C ---  2.7.2 : THETA CALCULE
      IF ( IRET.EQ.0 .AND. .NOT. TROIDL ) THEN

         OBJ1 = MODELE//'.MODELE    .LGRF'
         CALL JEVEUO ( OBJ1, 'L', IADRMA )
         NOMA = ZK8(IADRMA)
         THETA=TABLE1//'_CHAM_THETA'
         NOMNO = NOMA//'.NOMNOE'
         COORN = NOMA//'.COORDO    .VALE'
         CALL JEVEUO ( COORN, 'L', IADRCO )
         CALL GETVID ( 'THETA', 'FOND_FISS', 1,1,1, FOND, NBV )
         CALL GETVR8 ( 'THETA', 'DIRECTION', 1,1,0, R8B, NBR8)
         DIREC=.FALSE.
         IF ( NBR8 .NE. 0 ) THEN
            NBR8  = -NBR8
            SUITE='.'
            IF (DIME.EQ.2) SUITE=',LA 3-EME NULLE.'
            IF ( NBR8 .NE. 3 ) THEN
               CALL U2MESK('F','CALCULEL4_24',1,SUITE)
            ELSE
               CALL GETVR8 ( 'THETA', 'DIRECTION', 1, 1, 3, DIR, NBR8 )
               DIREC=.TRUE.
            ENDIF
         ELSE
           IF (DIME.EQ.2.AND.N2.EQ.0) CALL U2MESS('F','CALCULEL4_3')
         ENDIF

C      - THETA 2D (COURONNE)
        IF(DIME.EQ.2)THEN
          OPTIO3='COURONNE'
          CALL GVER2D ( NOMA,1,OPTIO3,'THETA',NOMNO,
     &                 NOEUD, RINF, RSUP, MODULE )
          CALL GCOU2D ( 'V',THETA, NOMA, NOMNO, NOEUD, ZR(IADRCO), RINF,
     &                    RSUP, MODULE, DIREC, DIR )
C     - THETA 3D
        ELSE IF(DIME.EQ.3)THEN
          CHFOND  = FOND//'.FOND      .NOEU'
          CALL JELIRA ( CHFOND, 'LONMAX', NBNO, K8B )
          CALL JEVEUO ( CHFOND, 'L', IADRNO )
          CALL GVERIG ( NOMA,1,CHFOND, NBNO, NOMNO, COORN,
     &                  TRAV1, TRAV2, TRAV3, TRAV4 )
          CALL GCOURO ( 'V',THETA, NOMA, NOMNO,COORN,NBNO,TRAV1,TRAV2,
     &                    TRAV3,DIR,ZK8(IADRNO),FOND,DIREC,STOK4)
        ENDIF
C
      ENDIF
C
C----------------------------------------------------
C 2.8. ==> FOND_FISS, FISSURE, LISSAGE,
C          PROPAGATION (OPTIONS: G_LAGR,G_LAGR_GLOB)
C----------------------------------------------------
C
C    - PROPAGATION ALPHA
       CALL GETVR8(' ','PROPAGATION',0,1,1,ALPHA,IPROPA)
       IF (IPROPA.EQ.0) ALPHA = 0.D0
       IF ((OPTION(1:6).NE.'G_LAGR') .AND. (IPROPA.NE.0)) THEN
         CALL U2MESS('F','CALCULEL4_25')
       ENDIF
C
C 2.8.1 ==> SI 3D LOCAL :
      IF(TROIDL)THEN

C     - FOND_FISS ET FISSURE :
      IF ( OPTION(1:6).NE.'CALC_K' .AND. OPTION.NE.'K_G_MODA')THEN
        CALL GETVID ( 'THETA', 'FOND_FISS', 1,1,1,FOND,IFOND)
        CHFOND = FOND//'.FOND      .NOEU'
        CALL JELIRA(CHFOND,'LONMAX',LNOFF,K8B)
      ENDIF
      IF (OPTION(1:6) .EQ. 'CALC_K' .OR. OPTION .EQ. 'K_G_MODA')THEN
        CALL GETVID ( 'THETA','FISSURE', 1,1,1, FISS, IFOND )
        CALL JEVEUO(FISS//'.FONDFISS','L',IFF)
        CALL JEVEUO(FISS//'.FONDMULT','L',IFOMUL)
        CALL GETVIS('THETA','NUME_FOND',1,1,1,NUMFON,IBID)
        IDEPFI=ZI(IFOMUL-1+2*(NUMFON-1)+1)
        IARRFI=ZI(IFOMUL-1+2*(NUMFON-1)+2)
        LNOFF=IARRFI-IDEPFI+1
        CHFOND='&&OP0100.CHFOND'
        CALL WKVECT(CHFOND,'V V R',LNOFF*4,IFON)
        DO 15 I=1,LNOFF
          DO 16 J=1,4
            ZR(IFON-1+4*(I-1)+J)=ZR(IFF-1+4*(I+(IDEPFI-1)-1)+J)
 16       CONTINUE
 15     CONTINUE
        GRLT=FISS//'.GRLTNO'
        GRLN=FISS//'.GRLNNO'
        BASLOC=FISS//'.BASLOC'
      ENDIF

C     - METHODE DE DECOMPOSITION DE THETA ET G : LAGRANGE OU LEGENDRE
      CALL GETVTX('LISSAGE','LISSAGE_THETA',1,1,1,LISSTH,IBID)
      CALL GETVTX('LISSAGE','LISSAGE_G',1,1,1,LISSG,IBID)
      CALL GETVIS('LISSAGE','DEGRE',1,1,1,NDEG,IBID)
      THLAG2=.FALSE.
      IF ((LISSTH.EQ.'LEGENDRE') .AND. (LISSG.EQ.'LEGENDRE')) THEN
        THLAGR = .FALSE.
        GLAGR = .FALSE.
        NBRE = NDEG
      ELSE IF ((LISSTH.EQ.'LAGRANGE') .AND. (LISSG.EQ.'LEGENDRE')) THEN
        THLAGR = .TRUE.
        GLAGR = .FALSE.
        NBRE = LNOFF
        IF ((OPTION .EQ. 'G_MAX') .OR.
     &      (OPTION .EQ. 'G_BILI')) THEN
           CALL U2MESK('F','CALCULEL4_26',1,OPTION)
        ENDIF
        IF (NDEG.GT.LNOFF) THEN
          CALL U2MESS('F','CALCULEL4_27')
        ENDIF
      ELSE IF ((LISSTH.EQ.'LAGRANGE') .AND.
     &         ((LISSG.EQ.'LAGRANGE').OR. (LISSG.EQ.
     &         'LAGRANGE_NO_NO'))) THEN
        THLAGR = .TRUE.
        GLAGR = .TRUE.
        NBRE = LNOFF
      ELSE IF ((LISSTH.EQ.'LEGENDRE') .AND. (LISSG.NE.'LEGENDRE')) THEN
        CALL U2MESS('F','CALCULEL4_28')
      ELSE IF ((LISSTH.EQ.'LAGRANGE_REGU') .AND.
     &         (LISSG.EQ.'LAGRANGE_REGU'))THEN
        THLAG2 = .TRUE.
        GLAGR  = .FALSE.
        THLAGR = .FALSE.
        IF(OPTION.EQ.'G_LAGR')THEN
          CALL U2MESK('F','CALCULEL4_29',1,OPTION(1:6))
        ENDIF
      ELSE IF(((LISSTH.EQ.'LAGRANGE_REGU') .AND.
     &         (LISSG.NE.'LAGRANGE_REGU')).OR.
     &      ((LISSTH.NE.'LAGRANGE_REGU') .AND.
     &       (LISSG.EQ.'LAGRANGE_REGU'))) THEN
        CALL U2MESS('F','CALCULEL4_30')
      ENDIF

C    - PROPAGATION CHAMP THETA
      CALL GETVID(' ','THETA_LAGR',0,1,1,SDTHET,ITHETA)
      IF (ITHETA.EQ.0) THEN
        THETLG = ' '
      ELSE
        CALL RSEXCH(SDTHET,'THETA',0,THETLG,IRET)
        IF (IRET.NE.0) THEN
          CALL U2MESK('F','CALCULEL3_90',1,SDTHET)
        ENDIF
      ENDIF
      IF ((OPTION.EQ.'G_LAGR') .AND. (ITHETA.EQ.0)) THEN
        CALL U2MESK('F','CALCULEL4_31',1,OPTION)
      ENDIF
C
C 2.8.2 ==> SI 2D
      ELSE IF(DIME.EQ.2)THEN
C
C        - FOND_FISS 2D
           CALL GETVID ( 'THETA', 'FOND_FISS', 1,1,1,FOND,N1)
           CALL GETVID ( 'THETA','FISSURE', 1,1,1, FISS, N2 )
           IF ( (OPTION .EQ. 'CALC_K_G') .AND. (N1+N2.EQ.0) ) THEN
              CALL U2MESS('F','CALCULEL4_21')
           ENDIF
C
      ENDIF
C
C-------------------------------------
C 2.9. ==> SENSIBILITE : 2D OU 3D GLOB
C-------------------------------------
C
      IF(.NOT.TROIDL)THEN

C 2.9.1 ==> NOMBRE DE PASSAGES
C           RQ : POUR UN CALCUL STANDARD DE G, CE NOMBRE VAUT 1
C              12   345678
      K8BID = '&&'//NOMPRO
      IAUX = 1
      CALL PSLECT ( ' ', 0, K8BID, TABLE1, IAUX,
     &              NBPASE, INPSCO, IRET )
      IAUX = 1
      JAUX = 1
      CALL PSRESE(' ',0,IAUX,TABLE1,JAUX,NBPASS,NORECG,IRET)
      CALL JEVEUO(NORECG,'L',ADRECG)
C
C 2.9.2 ==> A-T-ON UNE DEPENDANCE VIS-A-VIS D'UN MATERIAU ? (CF. NMDOME)
C
      DO 292 , NRPASE = 1 , NBPASE
C
        IAUX = NRPASE
        JAUX = 1
        MATERI = MATE(1:8)
        CALL PSNSLE ( INPSCO,IAUX,JAUX,NOPASE )
        CALL PSGENC ( MATERI,NOPASE,MATERS,IRET )
        IF (IRET.EQ.0) THEN
          CALL PSTYPA ( NBPASE, INPSCO, MATERI, NOPASE, TYPEPS(-2) )
          CALL RCMFMC ( MATERS,MATES )
        ENDIF
C
  292 CONTINUE
C
C 2.9.3 ==> A-T-ON UNE DEPENDANCE VIS-A-VIS D'UNE CHARGE ? (CF. NMDOME)
C
      EXCHSE = .FALSE.
      IF ( NCHA.NE.0 .AND. NBPASE.NE.0 ) THEN
C
        CHARSE = '&&'//NOMPRO//'.CHARSE'
        IAUX = MAX(NBPASE,1)
        CALL WKVECT(CHARSE,'V V K8',IAUX,ADCHSE)
C
        CALL GETFAC('EXCIT',NEXCI)
C
        DO 293 , ICHAR = 1 , NCHA
C
C 2.9.3.1. ==> LA CHARGE EST-ELLE CONCERNEE PAR UNE SENSIBILITE ?
C
          IF(NEXCI .GT. 0) THEN
            CALL GETVID('EXCIT','CHARGE',ICHAR,1,1,NOMCHA,N1)
          ELSE
            NOMCHA = ZK8(ICHA+ICHAR-1)
          ENDIF

          DO 2931 , NRPASE = 1 , NBPASE
            IAUX = NRPASE
            JAUX = 1
            CALL PSNSLE(INPSCO,IAUX,JAUX,NOPASE)
            CALL PSGENC ( NOMCHA,NOPASE,K8BID,IRET)
            IF (IRET.EQ.0) THEN
              ZK8(ADCHSE+NRPASE-1) = NOPASE
              EXCHSE = .TRUE.
            ELSE
              ZK8(ADCHSE+NRPASE-1) = '        '
            ENDIF
 2931     CONTINUE
C
  293   CONTINUE
C
C 2.9.3.2. ==> SI LA CHARGE EST CONCERNEE, ON AFFINE
C
        IF ( EXCHSE ) THEN
C
          LIGRCH = NOMCHA(1:8)//'.CHME.LIGRE'
C
          DO 2932 , IAUX = 1,NBTYCH
C
            IF (NOMLIG(IAUX).EQ.'.VEASS') THEN
              SUFFIX = '     '
            ELSE
              SUFFIX = '.DESC'
            ENDIF
            LCHIN = LIGRCH(1:13)//NOMLIG(IAUX)//SUFFIX
            CALL JEEXIN(LCHIN,IRET)
C
            IF (IRET.NE.0) THEN
C
              CALL DISMOI('F','TYPE_CHARGE',NOMCHA,'CHARGE',
     &                    IBID,AFFCHA,IRET)
C
              IF (AFFCHA(5:7).EQ.'_FO') THEN
                DO 29321 , NRPASE = 1 , NBPASE
                  NOPASE = ZK8(ADCHSE+NRPASE-1)
                  IF (NOPASE.NE.'        ') THEN
                    CALL TELLME('F','NOM_FONCTION',LCHIN(1:19),NOPASE,
     &                                K8BID,IRET)
                    IF (K8BID.EQ.'OUI') THEN
                      CALL PSTYPA ( NBPASE, INPSCO, NOMCHA, NOPASE,
     &                              TYPEPS(IAUX) )
                    ENDIF
                  ENDIF
29321           CONTINUE
              ENDIF
C
            ENDIF
C
 2932     CONTINUE
C
        ENDIF
C
      ENDIF

      ELSE
         NBPASE=0
         NBPASS=1
         LATABL=TABLE1

      ENDIF
C
C
C=======================
C 3. CALCUL DE L'OPTION
C=======================
C
C============ DEBUT DE LA BOUCLE SUR LE NOMBRE DE PASSAGES =============
      DO 30 , NRPASS = 1 , NBPASS
C
C --- DECODAGE DES NOMS DES CONCEPTS
C        POUR LE PASSAGE NUMERO NRPASS :
C        . NOPASE : NOM DU PARAMETRE DE SENSIBILITE EVENTUELLEMENT
C        . LATABL : NOM DE LA TABLE A COMPLETER
C                   C'EST 'TABLE1' POUR UN CALCUL STANDARD, UN NOM
C                   COMPOSE A PARTIR DE 'TABLE1' ET 'NOPASE' POUR
C                   UN CALCUL DE SENSIBILITE
C        . LERES0 : IDEM POUR RESUCO
C        . OPTIO1 : C'EST OPTION POUR UN CALCUL STANDARD, 'CALC_DG' POUR
C                   UN CALCUL DE SENSIBILITE
        OPTIO1 = OPTION

        IF(.NOT.TROIDL)THEN

          NOPASE = ZK24(ADRECG+2*NRPASS-1) (1:8)
          LATABL = ZK24(ADRECG+2*NRPASS-2) (1:8)
C
C
C DANS LE CAS D'UN CALCUL STANDARD :

          IF (NOPASE.EQ.' ') THEN

          TYPESE = 0
          STYPSE = BLAN24
          CHDESE=' '
          CHEPSE=' '
          CHSISE=' '

C DANS LE CAS D'UN CALCUL DE DERIVE :
C     TYPESE  : TYPE DE SENSIBILITE
C               -1 : DERIVATION EULERIENNE (VIA UN CHAMP THETA)
C                3 : DERIVATION PAR RAPPORT AU MODULE D'YOUNG
C                5 : DERIVATION PAR RAPPORT AU CHARGEMENT
C DANS CES 2 DERNIERS CAS, IL NE FAUT QU'UN SEUL PARAMETRE SENSIBLE
C A CHAQUE APPEL DE CALC_G_THETA
C
          ELSE
C
          CALL METYSE(NBPASE,INPSCO,NOPASE,TYPESE,STYPSE)
          IF ( TYPESE.EQ.-1 ) THEN
            OPTIO1 = 'CALC_DG'
          ELSE IF ( TYPESE.EQ.3 ) THEN
            IF(DIME.EQ.2)THEN
              OPTIO1 = 'CALC_DG_E'
            ELSE
              OPTIO1 = 'CALC_DGG_E'
            ENDIF
            IF(OPTION.EQ.'CALC_K_G') OPTIO1 = 'CALC_DK_DG_E'
            IF(NBPASE.GE.2) THEN
              CALL U2MESS('F','SENSIBILITE_51')
            ENDIF
          ELSE IF ( TYPESE.EQ.5 ) THEN
            IF(DIME.EQ.2)THEN
              OPTIO1 = 'CALC_DG_FORC'
            ELSE
              OPTIO1 = 'CALC_DGG_FORC'
            ENDIF
            IF(OPTION.EQ.'CALC_K_G') OPTIO1 = 'CALC_DK_DG_FORC'
            IF(NBPASE.GE.2) THEN
              CALL U2MESS('F','SENSIBILITE_51')
            ENDIF
          ELSE
            VALK(1) = RESUCO
            VALK(2) = NOPASE
            CALL U2MESK('F','SENSIBILITE_2', 2 ,VALK)
          ENDIF
          CALL PSGENC ( RESUCO, NOPASE, LERES0, IRET )
          IF ( IRET.NE.0 ) THEN
            VALK(1) = RESUCO
            VALK(2) = NOPASE
            CALL U2MESK('F','SENSIBILITE_3', 2 ,VALK)
          ENDIF

        ENDIF
C
        IF ( NIV.GE.2 ) THEN
          IF ( NOPASE.NE.'        ' ) THEN
            CALL U2MESK('I','SENSIBILITE_71',1,NOPASE)
          ENDIF
        ENDIF

      ENDIF
C
C --- DETERMINATION AUTOMATIQUE DE THETA (CAS 3D LOCAL)
C
      IF (TROIDL.AND. OPTIO1(1:6) .NE.'CALC_K'
     &          .AND. OPTIO1 .NE.'K_G_MODA') THEN

      CALL JEVEUO(CHFOND,'L',IADNUM)
      IF (ZK8(IADNUM+1-1).EQ.ZK8(IADNUM+LNOFF-1)) THEN
        CONNEX = .TRUE.
      ELSE
        CONNEX = .FALSE.
      ENDIF
      IF (.NOT.THLAG2)THEN
        IF ((CONNEX.AND. (.NOT.THLAGR)) .OR.
     &    (CONNEX.AND. (.NOT.GLAGR))) THEN
          CALL U2MESS('F','CALCULEL4_36')
        ENDIF
      ENDIF
      OBJMA = MODELE//'.MODELE    .LGRF'
      THETAI = '&&THETA '
      CALL JEVEUO(OBJMA,'L',IADRMA)
      NOMA = ZK8(IADRMA)
      NOMNO = NOMA//'.NOMNOE'
      IF ((OPTIO1.EQ.'CALC_G') .OR.
     &    (OPTIO1.EQ.'G_BILI') .OR.
     &    (OPTIO1.EQ.'G_MAX')) THEN
        COORN = NOMA//'.COORDO    .VALE'
      ELSE
        CALL VTGPLD(NOMA//'.COORDO    ',ALPHA,THETLG,'V','&&GMETH1.G2')
        COORN = '&&GMETH1.G2        '//'.VALE'
      ENDIF
      CALL GVERI2(CHFOND,LNOFF,NOMNO,COORN,TRAV1,TRAV2,TRAV3,
     &            THLAGR,THLAG2,NDEG)
      CALL GCOUR2(THETAI,NOMA,MODELE,NOMNO,COORN,LNOFF,TRAV1,TRAV2,
     &            TRAV3,CHFOND,FOND,CONNEX,STOK4,THLAGR,THLAG2,NDEG,
     &            MILIEU,NDIMTE,PAIR)
      CALL GIMPT2(THETAI,NBRE,TRAV1,TRAV2,TRAV3,CHFOND,STOK4,LNOFF,0)


      ELSE IF((TROIDL.AND. OPTIO1(1:6) .EQ.'CALC_K').OR.
     &        (TROIDL.AND. OPTIO1 .EQ.'K_G_MODA')) THEN

C       ON A TOUJOURS À FAIRE À UN FOND OUVERT AVEC XFEM
        CONNEX = .FALSE.
        THETAI = '&&THETA '
        OBJMA = MODELE//'.MODELE    .LGRF'
        CALL JEVEUO(OBJMA,'L',IADRMA)
        NOMA = ZK8(IADRMA)
        NOMNO = NOMA//'.NOMNOE'
        COORN = NOMA//'.COORDO    .VALE'

        CALL GVERI3(CHFOND,LNOFF,THLAGR,THLAG2,NDEG,TRAV1,TRAV2,TRAV3)
        CALL GCOUR3(THETAI,NOMA,MODELE,NOMNO,COORN,LNOFF,TRAV1,TRAV2,
     &              TRAV3,CHFOND,GRLT,.FALSE.,CONNEX,THLAGR,THLAG2,
     &              NDEG,MILIEU,PAIR,NDIMTE)
        CALL XCOURB(GRLT,GRLN,NOMA,MODELE,COURB)

      ENDIF

      IF(DIME.EQ.3)THEN
        CALL JEEXIN(TRAV1,IRET)
        IF (IRET.NE.0) CALL JEDETR(TRAV1)
        CALL JEEXIN(TRAV2,IRET)
        IF (IRET.NE.0) CALL JEDETR(TRAV2)
        CALL JEEXIN(TRAV3,IRET)
        IF (IRET.NE.0) CALL JEDETR(TRAV3)
        CALL JEEXIN(STOK4,IRET)
        IF (IRET.NE.0) CALL JEDETR(STOK4)
      ENDIF

C --- CREATION DE LA TABLE
C
      NBPRUP=9
      CALL CGCRTB(LATABL,OPTIO1,DIME,LMELAS,TROIDL,NBPRUP,
     &                  NOPRUP,TYPRUP)
C
C--------------------------------------------------------------
C 3.1. ==> CALCUL DE LA FORME BILINEAIRE DU TAUX DE RESTITUTION
C--------------------------------------------------------------
C
      IF (OPTIO1(1:6) .EQ.'G_BILI'.OR. OPTIO1(1:5) .EQ.'G_MAX') THEN

        DO 3111 I = 1 , LONVEC
           
         IF (LMELAS) THEN
            IF (LNCAS) THEN
              IF(.NOT.ZL(JNORD+I-1)) GOTO 3111
            ENDIF
            EXITIM = .FALSE.
            TIMEU=0.D0
            TIMEV=0.D0
            CALL RSADPA(RESUCO,'L',1,'NOM_CAS',IORD,0,IAD,K8B)
            NOMCAS=ZK16(IAD)
          ENDIF

          DO 3112 J = 1,I
            CALL JEMARQ()
            CALL JERECU('V')
            IORD1 = ZI(IVEC-1+I)
            CALL MEDOM1(MODELE,MATE,K8B,VCHAR,NCHA,K4B,RESUCO,IORD1)
            CALL JEVEUO(VCHAR,'L',ICHA)
            CALL RSEXCH(RESUCO,'DEPL',IORD1,DEPLA1,IRET)
            IF(LONVEC.EQ.1)THEN
              IORD2  = IORD1
              DEPLA2 = DEPLA1
            ELSE
              IORD2 = ZI(IVEC-1+J)
              CALL RSEXCH(RESUCO,'DEPL',IORD2,DEPLA2,IRET)
              IF(IRET.NE.0) THEN
                CALL U2MESS('F','CALCULEL4_37')
              ENDIF
            ENDIF

            IF(.NOT.LMELAS)THEN
              CALL RSADPA(RESUCO,'L',1,'INST',IORD1,0,JINST,K8BID)
              TIMEU = ZR(JINST)
              CALL RSADPA(RESUCO,'L',1,'INST',IORD2,0,JINST,K8BID)
              TIMEV = ZR(JINST)
              EXITIM = .TRUE.
            ENDIF

            OPTIO2 = 'G_BILI'
            IF(TROIDL)THEN
               CALL MBILGL(OPTIO2,LATABL,MODELE,DEPLA1,DEPLA2,THETAI,
     &                     MATE,NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,
     &                     NDEG,THLAGR,GLAGR,THLAG2,MILIEU,NDIMTE,PAIR,
     &                     EXITIM,TIMEU,TIMEV,I,J,NBPRUP,NOPRUP,
     &                     LMELAS,NOMCAS)
            ELSE
               CALL MEBILG(OPTIO2,LATABL,MODELE,DEPLA1,DEPLA2,THETA,
     &                     MATE,NCHA,ZK8(ICHA),SYMECH,EXITIM,TIMEU,
     &                     TIMEV,I,J,NBPRUP,NOPRUP)
            ENDIF
            CALL JEDEMA()
 3112       CONTINUE
 3111     CONTINUE
C
C----------------------------------------------------
C 3.2. ==> MAXIMISATION DU G SOUS CONTRAINTES BORNES
C----------------------------------------------------
C
          IF (OPTIO1(1:5) .EQ.'G_MAX') THEN

          CALL GETFAC ('BORNES', NBORN )
          IF (NBORN.NE.0) THEN
              NBCO = 2*NBORN
              CALL WKVECT('&&'//NOMPRO//'.COUPLES_BORNES','V V R8',
     &                   NBCO,IBOR)
              DO 3213 I=1, NBORN
                CALL GETVIS('BORNES','NUME_ORDRE',I,1,1,IORD,N1)
                CALL GETVR8('BORNES','VALE_MIN',I,1,1,
     &                      ZR(IBOR+2*(IORD-1)),N1)
                CALL GETVR8('BORNES','VALE_MAX',I,1,1,
     &                      ZR(IBOR+2*(IORD-1)+1),N1)
 3213         CONTINUE

            IF(TROIDL)THEN
              CALL TBEXVE(LATABL,'G_BILI_LOCAL',
     &                    '&&'//NOMPRO//'.GBILIN','V',NBVAL,K8B)
              CALL JEVEUO('&&'//NOMPRO//'.GBILIN','L',IG)
              CALL TBEXVE(LATABL,'NOEUD',
     &                    '&&'//NOMPRO//'.NOEUD','V',NBVAL,K8B)
              CALL JEVEUO('&&'//NOMPRO//'.NOEUD','L',LNOEU)
              CALL TBEXVE(LATABL,'ABSC_CURV',
     &                    '&&'//NOMPRO//'.ABSCUR','V',NBVAL,K8B)
              CALL JEVEUO('&&'//NOMPRO//'.ABSCUR','L',LABSCU)
C
              CALL DETRSD('TABLE',LATABL)
              CALL MMAXGL(NBCO,ZR(IBOR),ZR(IG),ZK8(LNOEU),
     &                    ZR(LABSCU),LONVEC,LNOFF,LATABL)
            ELSE
              CALL TBEXVE(LATABL,'G_BILIN',
     &                    '&&'//NOMPRO//'.GBILIN','V',NBVAL,K8B)
              CALL JEVEUO('&&'//NOMPRO//'.GBILIN','L',IG)
              CALL DETRSD('TABLE',LATABL)
              CALL MEMAXG(NBCO,ZR(IBOR),ZR(IG),LONVEC,LATABL)
            ENDIF

          ELSE
            CALL U2MESK('F','CALCULEL4_38',1,OPTIO1)
          ENDIF
          ENDIF
C
C-------------------------------
C 3.3. ==> CALCUL DE KG (3D LOC)
C-------------------------------
C
       ELSE IF ( TROIDL .AND. OPTIO1 .EQ. 'CALC_K_G' ) THEN

          DO 33 I = 1,LONVEC
            IORD = ZI(IVEC-1+I)

            IF (LMELAS) THEN
              IF (LNCAS) THEN
                IF(.NOT.ZL(JNORD+I-1)) GOTO 33
              ENDIF
              EXITIM = .FALSE.
              TIME=0.D0
              CALL RSADPA(RESUCO,'L',1,'NOM_CAS',IORD,0,IAD,K8B)
              NOMCAS=ZK16(IAD)
            ELSE
              CALL RSADPA(RESUCO,'L',1,'INST',IORD,0,JINST,K8B)
              TIME  = ZR(JINST)
              EXITIM = .TRUE.
            ENDIF

            CALL MEDOM1(MODELE,MATE,K8B,VCHAR,NCHA,K4B,RESUCO,IORD)
            CALL JEVEUO(VCHAR,'L',ICHA)
            CALL RSEXCH(RESUCO,'DEPL',IORD,DEPLA,IRET)
            IF (IRET.NE.0) THEN
                VALK (1) = 'DEPL'
                VALI     = IORD
              CALL U2MESG('F', 'CALCULEL6_27',1,VALK,1,VALI,0,0.D0)
            ENDIF

            CALL CAKG3D(OPTIO1,LATABL,MODELE,DEPLA,THETAI,MATE,COMPOR,
     &              NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,BASLOC,COURB,
     &              IORD,NDEG,THLAGR,GLAGR,THLAG2,PAIR,NDIMTE,
     &              THETLG,ALPHA,EXITIM,TIME,NBPRUP,NOPRUP,FISS,
     &              LMELAS,NOMCAS)
   33     CONTINUE
C
C------------------------
C 3.3.2. ==>OPTION CALC_K_MAX
C------------------------
C
        ELSE IF (OPTIO1 .EQ.'CALC_K_MAX') THEN
          CALL MMAXKL(LATABL,MODELE,THETAI,MATE,COMPOR,NCHA,SYMECH,
     &               CHFOND,LNOFF,BASLOC,COURB,NDEG,THLAGR,GLAGR,
     &               THLAG2,PAIR,NDIMTE,THETLG,ALPHA,NBPRUP,NOPRUP,
     &               FISS,LONVEC,IVEC,VCHAR,RESUCO,
     &               LMELAS,LNCAS,ZL(JNORD))
C
C------------------------
C 3.4. ==>OPTION K_G_MODA
C------------------------
C
      ELSE IF (OPTIO1 .EQ. 'K_G_MODA') THEN
C
C  3.4.1 ==>  K_G_MODA 2D
C  -----------------------
       IF(.NOT.TROIDL)THEN
          DO 341 I = 1 , LONVEC
            IORD = ZI(IVEC-1+I)
            CALL RSEXCH(RESUCO,'DEPL',IORD,DEPLA,IRET)
            IF(IRET.NE.0) THEN
              CALL U2MESS('F','CALCULEL4_39')
            ENDIF
            CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORD,0,IPULS,K8BID)
            PULS = ZR(IPULS)
            PULS = SQRT(PULS)
            CALL MEMOKG(OPTIO1,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                  ZK8(ICHA),SYMECH,FOND,IORD,PULS,NBPRUP,NOPRUP)
 341     CONTINUE
C
C  3.4.2 ==> K_G_MODA 3D LOC
C  -------------------------
       ELSE
          DO 342 I = 1,LONVEC
            IORD = ZI(IVEC-1+I)
            CALL MEDOM1(MODELE,MATE,K8BID,VCHAR,NCHA,K4B,RESUCO,IORD)
            CALL JEVEUO(VCHAR,'L',ICHA)
            CALL RSEXCH(RESUCO,'DEPL',IORD,DEPLA,IRET)
            IF (IRET.NE.0) THEN
                VALK (1) = 'DEPL'
                VALI = IORD
              CALL U2MESG('F', 'CALCULEL6_28',1,VALK,1,VALI,0,0.D0)
            ENDIF
            CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORD,0,IPULS,K8B)
            PULS = ZR(IPULS)
            PULS = SQRT(PULS)
            EXITIM = .TRUE.
            CALL CAKGMO(OPTIO1,LATABL,MODELE,DEPLA,THETAI,MATE,COMPOR,
     &              NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,BASLOC,COURB,
     &              IORD,NDEG,THLAGR,GLAGR,THLAG2,PAIR,NDIMTE,
     &              PULS,NBPRUP,NOPRUP,FISS)
 342     CONTINUE
       ENDIF
C
C--------------------------------------------
C 3.5. ==> CALCUL DE G, G_LAGR, K_G(2D) ET DG
C--------------------------------------------
C
      ELSE
        DO 35  I = 1 , LONVEC
          CALL JEMARQ()
          CALL JERECU('V')
          IORD = ZI(IVEC-1+I)
C
          IF (LMELAS) THEN
            IF (LNCAS) THEN
              IF(.NOT.ZL(JNORD+I-1)) GOTO 34
            ENDIF
            EXITIM = .FALSE.
            TIME=0.D0
            CALL RSADPA(RESUCO,'L',1,'NOM_CAS',IORD,0,IAD,K8B)
            NOMCAS=ZK16(IAD)
          ELSE
            CALL RSADPA(RESUCO,'L',1,'INST',IORD,0,JINST,K8B)
            TIME  = ZR(JINST)
            EXITIM = .TRUE.
          ENDIF
C
          CALL MEDOM1(MODELE,MATE,K8B,VCHAR,NCHA,K4B,
     &    RESUCO,IORD)
          CALL JEVEUO(VCHAR,'L',ICHA)
          CALL RSEXCH(RESUCO,'DEPL',IORD,DEPLA,IRET)
          IF(IRET.NE.0) THEN
                VALK (1) = 'DEPL'
                VALI = IORD
             CALL U2MESG('F', 'CALCULEL6_27',1,VALK,1,VALI,0,0.D0)
          ENDIF
          CALL RSEXCH(RESUCO,'VITE',IORD,CHVITE,IRET)
          IF(IRET.NE.0) THEN
            CHVITE = ' '
          ELSE
            CALL RSEXCH(RESUCO,'ACCE',IORD,CHACCE,IRET)
          ENDIF
C
C       - RECUPERATION DES CHAMNO DE DERIVEE LAGRANGIENNE DE DEPLACEMENT
C         DANS LA SD RESULTAT DERIVE DE TYPE EVOL_ELAS.
            IF (OPTIO1.EQ.'CALC_DG') THEN
              CALL RSEXC2(1,1,LERES0,'DEPL',IORD,CHDESE,OPTIO1,IRET)
              IF (IRET.GT.0) THEN
                VALI = IORD
                VALK (1) = 'DEPL'
                VALK (2) = RESUCO
                VALK (3) = NOPASE
                CALL U2MESG('F', 'SENSIBILITE_8',3,VALK,1,VALI,0,0.D0)
              ENDIF
            ENDIF
C
            IF (OPTIO1.EQ.'CALC_DG_E'
     &      .OR. OPTIO1.EQ.'CALC_DG_FORC'
     &      .OR. OPTIO1.EQ.'CALC_DGG_E'
     &      .OR. OPTIO1.EQ.'CALC_DGG_FORC'
     &      .OR. OPTIO1.EQ.'CALC_DK_DG_E'
     &      .OR. OPTIO1.EQ.'CALC_DK_DG_FORC') THEN
              CALL RSEXC2(1,1,LERES0,'DEPL',IORD,CHDESE,OPTIO1,IRET)
              IF (IRET.GT.0) THEN
                VALI = IORD
                VALK (1) = 'DEPL'
                VALK (2) = RESUCO
                VALK (3) = NOPASE
                CALL U2MESG('F', 'SENSIBILITE_8',3,VALK,1,VALI,0,0.D0)
              ENDIF
              CALL RSEXC2(1,1,LERES0,'EPSI_ELGA_DEPL',IORD,CHEPSE,
     &                    OPTIO1,IRET)
              IF (IRET.GT.0) THEN
                VALI = IORD
                VALK (1) = 'EPSI_ELGA_DEPL'
                VALK (2) = RESUCO
                VALK (3) = NOPASE
                CALL U2MESG('F', 'SENSIBILITE_8',3,VALK,1,VALI,0,0.D0)
              ENDIF
              CALL RSEXC2(1,1,LERES0,'SIEF_ELGA_DEPL',IORD,
     &                    CHSISE,OPTIO1,IRET)
              IF (IRET.GT.0) THEN
                VALI = IORD
                VALK (1) = 'SIEF_ELGA_DEPL'
                VALK (2) = RESUCO
                VALK (3) = NOPASE
                CALL U2MESG('F', 'SENSIBILITE_8',3,VALK,1,VALI,0,0.D0)
              ENDIF
            ENDIF
C
      IF(    (OPTIO1.EQ.'CALC_G' .AND. DIME.EQ.2 )
     &   .OR. OPTIO1.EQ.'CALC_DG'
     &   .OR. OPTIO1.EQ.'CALC_DG_E'
     &   .OR. OPTIO1.EQ.'CALC_DG_FORC'
     &   .OR. OPTIO1.EQ.'CALC_DGG_E'
     &   .OR. OPTIO1.EQ.'CALC_DGG_FORC'
     &   .OR. OPTIO1.EQ.'CALC_G_GLOB') THEN
C
           CALL MECALG (OPTIO1,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                   ZK8(ICHA),SYMECH,COMPOR,EXITIM,TIME,IORD,
     &                   NBPRUP,NOPRUP,NOPASE,TYPESE,CHDESE,
     &                   CHEPSE,CHSISE,CHVITE,CHACCE,LMELAS,NOMCAS)
C
      ELSEIF((OPTIO1.EQ.'CALC_G'.OR.OPTIO1.EQ.'G_LAGR').AND.TROIDL)THEN
C
           CALL MECAGL(OPTIO1,LATABL,MODELE,DEPLA,THETAI,MATE,COMPOR,
     &              NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,IORD,NDEG,THLAGR,
     &              GLAGR,THLAG2,MILIEU,THETLG,NDIMTE,PAIR,ALPHA,EXITIM,
     &              TIME,NBPRUP,NOPRUP,CHVITE,CHACCE,LMELAS,NOMCAS)
C
      ELSE IF ((OPTIO1 .EQ.'G_LAGR'.AND. DIME.EQ.2) .OR.
     &          OPTIO1 .EQ.'G_LAGR_GLOB' ) THEN
C
            CALL MLAGRG (OPTIO1,LATABL,MODELE,DEPLA,THETA,ALPHA,MATE,
     &                   NCHA,ZK8(ICHA),SYMECH,EXITIM,TIME,IORD,
     &                   NBPRUP, NOPRUP,LMELAS,NOMCAS )
C
      ELSE IF (OPTIO1.EQ.'CALC_K_G' .AND. DIME.EQ.2) THEN
C
            CALL MEFICG (OPTIO1,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                   ZK8(ICHA),SYMECH,FOND,NOEUD,EXITIM,TIME,IORD,
     &                   NBPRUP, NOPRUP, NOPASE,TYPESE,CHDESE,
     &                   CHEPSE,CHSISE,CHVITE,CHACCE,LMELAS,NOMCAS,
     &                   COMPOR)
C
      ELSE IF (OPTIO1.EQ.'CALC_DK_DG_E') THEN
            OPTIO2 = 'CALC_DG_E'
            CALL MECALG (OPTIO2,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                   ZK8(ICHA),SYMECH,COMPOR,EXITIM,TIME,IORD,
     &                   3,NOPRUP,NOPASE,TYPESE,CHDESE,
     &                   CHEPSE,CHSISE,CHVITE,CHACCE,LMELAS,NOMCAS)
C
C         - LES DERIVEES DE KI ET KII SONT NULLES.
C           ON LES AJOUTE DIRECTEMENT
            VAL(1) = 0.D0
            VAL(2) = 0.D0
            CALL TBAJLI(LATABL,2,NOPRUP(4),IORD,VAL,CBID,K8B,1)
      ELSE IF (OPTIO1.EQ.'CALC_DK_DG_FORC') THEN
            CALL MEFICG (OPTIO1,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                   ZK8(ICHA),SYMECH,FOND,NOEUD,EXITIM,TIME,IORD,
     &                   NBPRUP, NOPRUP, NOPASE,TYPESE,CHDESE,
     &                   CHEPSE,CHSISE,CHVITE,CHACCE,LMELAS,NOMCAS,
     &                   COMPOR)

      ELSE
           CALL U2MESS('F','CALCULEL4_40')

      ENDIF
 
 34   CONTINUE

      CALL JEDEMA()
 35   CONTINUE

      ENDIF
C
      IF(.NOT.TROIDL .AND. NBPASE.GT.1)THEN
        K24B = BLAN24
        K24B(1:8)   = LATABL
        K24B(20:24) = '.TITR'
        CALL TITREA('T',LATABL,LATABL,K24B,'C',' ',0,'G' )
      ELSE
        CALL TITRE
      ENDIF

 30   CONTINUE

      CALL JEDETC('G','&&NMDORC',1)

      CALL JEDEMA()

      END
