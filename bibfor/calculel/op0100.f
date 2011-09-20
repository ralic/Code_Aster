      SUBROUTINE OP0100()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 20/09/2011   AUTEUR GENIAUT S.GENIAUT 
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
C ======================================================================
C      OPERATEUR :     CALC_G
C
C      BUT:CALCUL DU TAUX DE RESTITUTION D'ENERGIE PAR LA METHODE THETA,
C          CALCUL DES FACTEURS D'INTENSITE DE CONTRAINTES, ...
C ======================================================================
C TOLE CRP_20
C
      IMPLICIT NONE
C ==> COMMUNS
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

C ==> VARIABLES LOCALES
C
      INTEGER IFM,NIV,N1,LONVEC,IORD, IBID,I,IAD,NCAS,JNCAS,JNORD,JPAR
      INTEGER VALI,NBTROU,NUTROU,INDIIS,IIND
      INTEGER NRES,NP,NC,NEXCI,NCHA,IRET,ICHA,IVEC,NBPARA
      INTEGER IFOND,LNOFF,TYPESE,JINST,NBPASS,ADRECG
      INTEGER ICHAR,NBPASE,NRPASS,NDEG,NBRE,IADNUM,IADRMA
      INTEGER NBR8,IADRCO,IADRNO,NBNO,J,IPULS,IORD1,IORD2
      INTEGER NBORN,NBCO,IBOR,IG,LNOEU,LABSCU,NBVAL,NEINIT
      INTEGER NDIMTE,NCELAS,IER,ITHET,NDIM,IFISS
      INTEGER NXPARA
      PARAMETER (NXPARA = 11)

C
      REAL*8  TIME,TIMEU,TIMEV,PREC,R8B,DIR(3)
      REAL*8  RINF,RSUP,MODULE,VAL(2),PULS,RBID
C
      COMPLEX*16 CBID,C16B
C
      CHARACTER*4 K4B
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'OP0100' )
      CHARACTER*8 TABLE1,MODELE,RESUCO,K8B,K8BID,KCALC,KCALC2
      CHARACTER*8 FOND,FISS,LITYPA(NXPARA),SYMECH,NOPASE,CRIT
      CHARACTER*8 LATABL,LERES0,NOMA,THETAI,NOEUD,CAS
      CHARACTER*13 INPSCO
      CHARACTER*16 TYPCO,OPER,OPTION,TYSD, LINOPA(NXPARA),OPTIO1,SUITE
      CHARACTER*16 OPTIO2,K16B,NOMCAS,K16BID
      CHARACTER*19 GRLT,GRLN
      CHARACTER*19 OPTIO3,VCHAR,SDTAB
      CHARACTER*24 DEPLA,MATE,K24B,COMPOR,CHVITE,CHACCE,VECORD
      CHARACTER*24 VALK(3),BASFON
      CHARACTER*24 SDTHET,CHFOND,BASLOC,CHDESE,CHEPSE,CHSISE,THETA
      CHARACTER*24 NORECG,IGRCH,STYPSE
      CHARACTER*24 BLAN24,LISSTH,LISSG,OBJMA,NOMNO,COORN
      CHARACTER*24 TRAV1,TRAV2,TRAV3,STOK4
      CHARACTER*24 OBJ1,TRAV4,COURB,DEPLA1,DEPLA2
C
      LOGICAL EXITIM,THLAGR,CONNEX,GLAGR,MILIEU,DIREC
      LOGICAL THLAG2,PAIR,LNCAS,LMELAS,LLEVST
C
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
C     ===============================
C       2. RECUPERATION DES OPERANDES
C     ===============================
C
C     ---------------------------------------------
C     2.0. RECUPERATION DU NIVEAU D'IMPRESSION
C     ---------------------------------------------
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
C     -----------------------------------------------------
C     2.1. LE CONCEPT DE SORTIE, SON TYPE, LA COMMANDE
C     -----------------------------------------------------
      CALL GETRES(TABLE1,TYPCO,OPER)
      IF ( NIV.GE.2 ) THEN
        CALL U2MESK('I','RUPTURE0_3',1,TABLE1)
      ENDIF
C
C     ----------------
C     2.2. OPTION
C     ----------------
      CALL GETVTX(' ','OPTION',0,1,1,OPTION,IBID)
C
      CALL GETVID (' ','RESULTAT',0,1,1,RESUCO,NRES)
      VECORD = '&&'//NOMPRO//'.VECTORDR'
      CALL GETVR8(' ','PRECISION',0,1,1,PREC,NP)
      CALL GETVTX(' ','CRITERE'  ,0,1,1,CRIT,NC)
      CALL RSUTNU ( RESUCO, ' ', 0, VECORD, LONVEC, PREC, CRIT, IER )
     
      CALL ASSERT(IER.EQ.0)
      CALL GETTCO(RESUCO,TYSD)
      IF (TYSD.EQ.'DYNA_TRANS') THEN
        CALL GETFAC('EXCIT',NEXCI)
        IF (NEXCI.EQ.0 ) THEN
          CALL U2MESS('F','RUPTURE0_9')
        ENDIF
      ELSEIF (TYSD.EQ.'MULT_ELAS') THEN
C
C       ON INFORME L'UTILISATEUR PAR UNE ALARME DE RENSEIGNER
C       LES MOTS-CLES "EXCIT" ET "NOM_CAS".
C       UNE EVENTUELLE EVOLUTION DU STOCKAGE DE LA SD CHARGE DANS
C       LA SD RESULTAT ISSUE DE MACRO_ELAS_MULT PERMETTRA DE SE
C       PASSER DE LA PRESENCE DE CES MOTS-CLES.
        CALL GETFAC('EXCIT',NEXCI)
        CALL GETVTX (' ','NOM_CAS',0,1,0,K8B,IRET)
        IF (NEXCI.EQ.0 .OR. IRET.EQ.0) THEN
          CALL U2MESS('A','RUPTURE0_55')
        ENDIF
      ENDIF

      IF (((OPTION.EQ.'K_G_MODA') .AND. (TYSD.NE.'MODE_MECA')) .OR.
     &   ((TYSD.EQ.'MODE_MECA') .AND. (OPTION.NE.'K_G_MODA'))) THEN
        CALL U2MESS('F','RUPTURE0_27')
      ENDIF

      CALL JEVEUO ( VECORD, 'L', IVEC )
      IORD = ZI(IVEC)
      CALL MEDOM1(MODELE,MATE,K8B,VCHAR,NCHA,K4B,RESUCO,IORD)
            
      CALL DISMOI('F','DIM_GEOM',MODELE,'MODELE',NDIM,K8B,IER)
      IF (.NOT.(NDIM.EQ.2.OR.NDIM.EQ.3)) CALL U2MESS('F','MODELISA2_6')
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
          CALL WKVECT('&&'//NOMPRO//'_MULTELAS_NOMCAS','V V K16',NCAS,
     &                JNCAS)
          CALL GETVTX(' ','NOM_CAS',0,1,NCAS,ZK16(JNCAS),IRET)
          LNCAS=.TRUE.
          CALL WKVECT('&&'//NOMPRO//'_MULTELAS_NUMORD','V V L',LONVEC,
     &                JNORD)
          DO 23 I=1,NCAS
            CALL RSORAC(RESUCO,'NOM_CAS',IBID,R8B,ZK16(JNCAS+I-1),
     &                  C16B,R8B,K8B,NUTROU,1,NBTROU)
            IF(NBTROU.EQ.0)THEN
               VALK(1)=ZK16(JNCAS+I-1)
               VALK(2)=RESUCO
               CALL U2MESK('F','RUPTURE0_28',2,VALK)
            ELSE
               IIND=INDIIS(ZI(IVEC),NUTROU,LONVEC,1)
               ZL(JNORD+IIND-1)=.TRUE.
            ENDIF
 23       CONTINUE
        ENDIF
      ENDIF
C
C     -------------------------------------------
C     2.4. VERIFICATIONS : CALCUL_CONTRAINTE
C     -------------------------------------------

      KCALC='OUI'
      CALL GETFAC('COMP_ELAS',NCELAS)
      IF (NCELAS.GT.0) THEN
        CALL GETVTX('COMP_ELAS','CALCUL_CONTRAINTE',1,1,1,KCALC,IRET)
        IF (KCALC(1:3).EQ.'NON') THEN
C         CALCUL_CONTRAINTE='NON' et ...
C         ... ETAT_INIT : INTERDIT
          CALL GETFAC('ETAT_INIT',NEINIT)
          IF (NEINIT.GT.0) CALL U2MESS('F','RUPTURE1_39')
C         ... OPTION != CALC_G OU CALC_G_GLOB : INTERDIT
          IF (OPTION(1:6).NE.'CALC_G') CALL U2MESS('F','RUPTURE1_40')
        ENDIF
      ENDIF
C
C     --------------------------------------------
C     2.5. CAS : 2D, 3D LOCAL ou 3D GLOBAL ?
C     --------------------------------------------

      IF (NDIM.EQ.3) THEN
        IF (OPTION.EQ.'CALC_G_GLOB'.OR.
     &      OPTION.EQ.'G_MAX_GLOB' .OR.
     &      OPTION.EQ.'G_BILI_GLOB') THEN
          CAS = '3D_GLOBA'
        ELSE
          CAS = '3D_LOCAL'
        ENDIF
      ELSEIF (NDIM.EQ.2) THEN
        CAS = '2D'
      ENDIF
C
C----------------
C 2.6. ==> THETA
C----------------
      CALL GETVTX (' ', 'SYME_CHAR', 0,1,1, SYMECH,IBID)
      CALL GETVID ('THETA','THETA', 1,1,1, SDTHET,ITHET)

      CALL GETVID('THETA','FOND_FISS',1,1,1,FOND,IFOND)
      CALL GETVID('THETA','FISSURE',  1,1,1,FISS,IFISS)

C     ATTENTION, IL SE PEUT QUE NI FOND_FISS NI FISSURE SOIT PRESENT
C     (EN DONNANT THETA PAR EX...)
      LLEVST=.FALSE.
      IF (IFISS.NE.0) LLEVST=.TRUE.

C     2.6.1 : THETA FOURNI
      IF (ITHET.NE.0) THEN

C       ON NE DOIT PAS ETRE DANS UN CALCUL 3D LOCAL
        IF (CAS.EQ.'3D_LOCAL') CALL U2MESS('F','RUPTURE0_57')

C       LE MOT-CLE DIRECTION NE DOIT PAS ETRE RENSEIGNE
C       NORMALEMENT, LE CAPY L'INTERDIT...
        CALL GETVR8('THETA','DIRECTION',1,1,0,R8B,NBR8)
        IF (NBR8.NE.0) CALL U2MESS('F','RUPTURE0_80')

        CALL GETTCO(SDTHET,TYPCO)
        IF (TYPCO(1:10).EQ.'THETA_GEOM') THEN
          CALL RSEXCH(SDTHET,'THETA',0,THETA,N1)
          IF (N1.GT.0) CALL U2MESK('F','RUPTURE0_59',1,SDTHET)
        ELSE
          THETA=SDTHET
        ENDIF
      ENDIF

C     2.6.2 : THETA CALCULE ????
      IF (ITHET.EQ.0.AND.CAS.NE.'3D_LOCAL') THEN

         OBJ1 = MODELE//'.MODELE    .LGRF'
         CALL JEVEUO ( OBJ1, 'L', IADRMA )
         NOMA = ZK8(IADRMA)
         THETA=TABLE1//'_CHAM_THETA'
         NOMNO = NOMA//'.NOMNOE'
         COORN = NOMA//'.COORDO    .VALE'
         CALL JEVEUO ( COORN, 'L', IADRCO )
         CALL GETVR8 ( 'THETA', 'DIRECTION', 1,1,0, R8B, NBR8)
         DIREC=.FALSE.
         IF ( NBR8 .NE. 0 ) THEN
            NBR8  = -NBR8
            SUITE='.'
            IF (NDIM.EQ.2) SUITE=',LA 3-EME NULLE.'
            IF ( NBR8 .NE. 3 ) THEN
               CALL U2MESK('F','RUPTURE0_30',1,SUITE)
            ELSE
               CALL GETVR8 ( 'THETA', 'DIRECTION', 1, 1, 3, DIR, NBR8 )
               DIREC=.TRUE.
            ENDIF
         ELSE
           IF (.NOT.LLEVST.AND.NDIM.EQ.2) CALL U2MESS('F','RUPTURE0_81')
         ENDIF

C      - THETA 2D (COURONNE)
        IF(NDIM.EQ.2)THEN
          OPTIO3='COURONNE'
          CALL GVER2D ( NOMA,1,OPTIO3,'THETA',NOMNO,
     &                 NOEUD, RINF, RSUP, MODULE )
          CALL GCOU2D ( 'V',THETA, NOMA, NOMNO, NOEUD, ZR(IADRCO), RINF,
     &                    RSUP, MODULE, DIREC, DIR )
C     - THETA 3D
        ELSE IF(NDIM.EQ.3)THEN
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
C 2.7. ==> FOND_FISS, FISSURE, LISSAGE,
C----------------------------------------------------

           
C     SI 3D ET OPTION = CALC_K_G OU K_G_MODA, ALORS IL FAUT FISSURE
      IF ( NDIM.EQ.3.AND.
     &    (OPTION.EQ.'CALC_K_G'.OR.
     &     OPTION.EQ.'K_G_MODA'.OR.
     &     OPTION.EQ.'CALC_K_MAX' ).AND.
     &     IFISS.EQ.0) THEN
        CALL U2MESK('F','RUPTURE0_29',1,OPTION)
      ENDIF
                    
C     CERTAINES OPTIONS DEMANDENT FOND_FISS OBLIGATOIREMENT
      IF (OPTION.EQ.'G_MAX'.OR.OPTION.EQ.'G_BILI') THEN
        IF (IFOND.EQ.0) CALL U2MESK('F','RUPTURE0_48',1,OPTION)
      ENDIF
C
C     2.7.1 FOND DE FISSURE SI 3D LOCAL :
      IF (CAS.EQ.'3D_LOCAL') THEN

C       SI FISSURE
        IF (IFISS.NE.0) THEN
          CHFOND='&&'//NOMPRO//'.CHFOND'
          BASFON='&&'//NOMPRO//'.BASEFOND'
C         CREATION DE LA LISTE DES POINTS DU FOND A CALCULER
          CALL XRECFF(FISS,CHFOND,BASFON,LNOFF)
          GRLT=FISS//'.GRLTNO'
          GRLN=FISS//'.GRLNNO'
          BASLOC=FISS//'.BASLOC'

C       SI FOND_FISS
        ELSEIF (IFOND.NE.0) THEN
          CHFOND = FOND//'.FOND      .NOEU'
          CALL JELIRA(CHFOND,'LONMAX',LNOFF,K8B)
          LLEVST=.FALSE.
        ENDIF

      ENDIF


C     2.7.2 LISSAGE EN FOND DE FISSURE

C     METHODE DE DECOMPOSITION DE THETA ET G : LAGRANGE OU LEGENDRE
      CALL GETVTX('LISSAGE','LISSAGE_THETA',1,1,1,LISSTH,IBID)
      CALL GETVTX('LISSAGE','LISSAGE_G',1,1,1,LISSG,IBID)
      CALL GETVIS('LISSAGE','DEGRE',1,1,1,NDEG,IBID)

C     CE SERAIT BIEN D'ALERTER L'UTILISATEUR SI IL RENSEIGNE LISSAGE
C     DANS UN CAS 2D OU 3D_GLOBAL, MAIS COMME LISSAGE EST PAR DEFAUT
C     ON NE PEUT PAS
      IF (CAS.EQ.'3D_LOCAL') THEN

        THLAG2=.FALSE.
        IF ((LISSTH.EQ.'LEGENDRE') .AND. (LISSG.EQ.'LEGENDRE')) THEN
          THLAGR = .FALSE.
          GLAGR = .FALSE.
          NBRE = NDEG
        ELSE IF (LISSTH.EQ.'LAGRANGE'.AND.LISSG.EQ.'LEGENDRE') THEN
          THLAGR = .TRUE.
          GLAGR = .FALSE.
          NBRE = LNOFF
          IF (OPTION.EQ.'G_MAX'.OR.
     &        OPTION.EQ.'G_BILI') THEN
             CALL U2MESK('F','RUPTURE0_83',1,OPTION)
          ENDIF
          IF (NDEG.GE.LNOFF) THEN
            CALL U2MESI('F','RUPTURE0_84',1,LNOFF)
          ENDIF
        ELSE IF ( (LISSG.EQ.'LAGRANGE'.OR.LISSG.EQ.'LAGRANGE_NO_NO')
     &       .AND.LISSTH.EQ.'LAGRANGE' ) THEN
          THLAGR = .TRUE.
          GLAGR = .TRUE.
          NBRE = LNOFF
        ELSE IF (LISSTH.EQ.'LEGENDRE'.AND.LISSG.NE.'LEGENDRE') THEN
          CALL U2MESS('F','RUPTURE0_85')
        ELSE IF ((LISSTH.EQ.'LAGRANGE_REGU') .AND.
     &           (LISSG.EQ.'LAGRANGE_REGU'))THEN
          THLAG2 = .TRUE.
          GLAGR  = .FALSE.
          THLAGR = .FALSE.
        ELSE IF(((LISSTH.EQ.'LAGRANGE_REGU') .AND.
     &           (LISSG.NE.'LAGRANGE_REGU')).OR.
     &         ((LISSTH.NE.'LAGRANGE_REGU') .AND.
     &         (LISSG.EQ.'LAGRANGE_REGU'))) THEN
          CALL U2MESS('F','RUPTURE0_87')
        ENDIF
C
      ENDIF
C
C     -------------------------------------
C     2.8. ==> SENSIBILITE : 2D OU 3D GLOB
C     -------------------------------------
C
      IF (CAS.NE.'3D_LOCAL') THEN
        CALL GINISE(TABLE1,NBPASE,INPSCO,NORECG,MATE,NCHA,VCHAR,NBPASS)
        CALL JEVEUO(NORECG,'L',ADRECG)
      ELSE
        NBPASE=0
        NBPASS=1
        LATABL=TABLE1
      ENDIF
C
C
C     =======================
C     3. CALCUL DE L'OPTION
C     =======================
C
C     ======= DEBUT DE LA BOUCLE SUR LE NOMBRE DE PASSAGES =============
      DO 30 , NRPASS = 1 , NBPASS
C
C       DECODAGE DES NOMS DES CONCEPTS
C       POUR LE PASSAGE NUMERO NRPASS :
C        . NOPASE : NOM DU PARAMETRE DE SENSIBILITE EVENTUELLEMENT
C        . LATABL : NOM DE LA TABLE A COMPLETER
C                   C'EST 'TABLE1' POUR UN CALCUL STANDARD, UN NOM
C                   COMPOSE A PARTIR DE 'TABLE1' ET 'NOPASE' POUR
C                   UN CALCUL DE SENSIBILITE
C        . LERES0 : IDEM POUR RESUCO
C        . OPTIO1 : C'EST OPTION POUR UN CALCUL STANDARD, 'CALC_DG' POUR
C                   UN CALCUL DE SENSIBILITE
        OPTIO1 = OPTION

        IF (CAS.NE.'3D_LOCAL') THEN

          NOPASE = ZK24(ADRECG+2*NRPASS-1) (1:8)
          LATABL = ZK24(ADRECG+2*NRPASS-2) (1:8)
C
C
C         DANS LE CAS D'UN CALCUL STANDARD :

          IF (NOPASE.EQ.' ') THEN

            TYPESE = 0
            STYPSE = BLAN24
            CHDESE=' '
            CHEPSE=' '
            CHSISE=' '

C           DANS LE CAS D'UN CALCUL DE DERIVE :
C           TYPESE  : TYPE DE SENSIBILITE
C               -1 : DERIVATION EULERIENNE (VIA UN CHAMP THETA)
C                3 : DERIVATION PAR RAPPORT AU MODULE D'YOUNG
C                5 : DERIVATION PAR RAPPORT AU CHARGEMENT
C           DANS CES 2 DERNIERS CAS, 
C           IL NE FAUT QU'UN SEUL PARAMETRE SENSIBLE
C           A CHAQUE APPEL DE CALC_G_THETA
C
          ELSE
C
            CALL METYSE(NBPASE,INPSCO,NOPASE,TYPESE,STYPSE)
            IF ( TYPESE.EQ.-1 ) THEN
              OPTIO1 = 'CALC_DG'
            ELSE IF ( TYPESE.EQ.3 ) THEN
              IF(NDIM.EQ.2)THEN
                OPTIO1 = 'CALC_DG_E'
              ELSE
                OPTIO1 = 'CALC_DGG_E'
              ENDIF
              IF(OPTION.EQ.'CALC_K_G') OPTIO1 = 'CALC_DK_DG_E'
              IF(NBPASE.GE.2) THEN
                CALL U2MESS('F','SENSIBILITE_51')
              ENDIF
            ELSE IF ( TYPESE.EQ.5 ) THEN
              IF(NDIM.EQ.2)THEN
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
C       DETERMINATION AUTOMATIQUE DE THETA (CAS 3D LOCAL)
C
        IF (CAS.EQ.'3D_LOCAL'.AND.IFISS.NE.0) THEN

C         ON A TOUJOURS À FAIRE À UN FOND OUVERT
          CONNEX = .FALSE.
          THETAI = '&&THETA '
          OBJMA = MODELE//'.MODELE    .LGRF'
          CALL JEVEUO(OBJMA,'L',IADRMA)
          NOMA = ZK8(IADRMA)
          NOMNO = NOMA//'.NOMNOE'
          COORN = NOMA//'.COORDO    .VALE'

          CALL GVERI3(CHFOND,LNOFF,THLAGR,THLAG2,NDEG,TRAV1,TRAV2,TRAV3)
          CALL GCOUR3(THETAI,NOMA,COORN,LNOFF,TRAV1,TRAV2,
     &                TRAV3,CHFOND,GRLT,THLAGR,THLAG2,
     &                BASFON,NDEG,MILIEU,PAIR,NDIMTE)
          CALL XCOURB(GRLT,GRLN,NOMA,MODELE,COURB)

        ELSEIF (CAS.EQ.'3D_LOCAL'.AND.IFOND.NE.0) THEN

         CALL JEVEUO(CHFOND,'L',IADNUM)
         IF (ZK8(IADNUM+1-1).EQ.ZK8(IADNUM+LNOFF-1)) THEN
          CONNEX = .TRUE.
         ELSE
          CONNEX = .FALSE.
         ENDIF
         IF (.NOT.THLAG2)THEN
          IF ((CONNEX.AND. (.NOT.THLAGR)) .OR.
     &      (CONNEX.AND. (.NOT.GLAGR))) THEN
            CALL U2MESS('F','RUPTURE0_90')
          ENDIF
         ENDIF
         OBJMA = MODELE//'.MODELE    .LGRF'
         THETAI = '&&THETA '
         CALL JEVEUO(OBJMA,'L',IADRMA)
         NOMA = ZK8(IADRMA)
         NOMNO = NOMA//'.NOMNOE'
         COORN = NOMA//'.COORDO    .VALE'
         CALL GVERI2(CHFOND,LNOFF,NOMNO,COORN,TRAV1,TRAV2,TRAV3,
     &              THLAGR,THLAG2,NDEG)
         CALL GCOUR2(THETAI,NOMA,MODELE,NOMNO,COORN,LNOFF,TRAV1,TRAV2,
     &              TRAV3,CHFOND,FOND,CONNEX,STOK4,THLAGR,THLAG2,NDEG,
     &              MILIEU,NDIMTE,PAIR)
         CALL GIMPT2(THETAI,NBRE,TRAV1,TRAV2,TRAV3,CHFOND,STOK4,LNOFF,0)

        ENDIF

C       MENAGE
        IF(NDIM.EQ.3)THEN
          CALL JEEXIN(TRAV1,IRET)
          IF (IRET.NE.0) CALL JEDETR(TRAV1)
          CALL JEEXIN(TRAV2,IRET)
          IF (IRET.NE.0) CALL JEDETR(TRAV2)
          CALL JEEXIN(TRAV3,IRET)
          IF (IRET.NE.0) CALL JEDETR(TRAV3)
          CALL JEEXIN(STOK4,IRET)
          IF (IRET.NE.0) CALL JEDETR(STOK4)
        ENDIF

C ---   CREATION DE LA TABLE
C
        CALL CGCRTB(LATABL,OPTIO1,NDIM,LMELAS,CAS,LLEVST,
     &              NBPARA,LINOPA,LITYPA)
C
C       --------------------------------------------------------------
C       3.1. ==> CALCUL DE LA FORME BILINEAIRE DU TAUX DE RESTITUTION
C       --------------------------------------------------------------
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
                  VALK(1) = 'DEPL'
                  VALI    = IORD2
                  CALL U2MESG('F', 'RUPTURE0_93',1,VALK,1,VALI,0,0.D0)
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
              IF (CAS.EQ.'3D_LOCAL') THEN
                 CALL MBILGL(OPTIO2,LATABL,MODELE,DEPLA1,DEPLA2,THETAI,
     &                       MATE,NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,
     &                       NDEG,THLAGR,GLAGR,THLAG2,MILIEU,NDIMTE,
     &                       PAIR,EXITIM,TIMEU,TIMEV,I,J,NBPARA,LINOPA,
     &                       LMELAS,NOMCAS)
              ELSE
                 CALL MEBILG(OPTIO2,LATABL,MODELE,DEPLA1,DEPLA2,THETA,
     &                       MATE,NCHA,ZK8(ICHA),SYMECH,EXITIM,TIMEU,
     &                       TIMEV,I,J,NBPARA,LINOPA)
              ENDIF
              CALL JEDEMA()
 3112       CONTINUE
 3111     CONTINUE
C
C       ----------------------------------------------------
C       3.2. ==> MAXIMISATION DU G SOUS CONTRAINTES BORNES
C       ----------------------------------------------------
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

            IF (CAS.EQ.'3D_LOCAL') THEN
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
            CALL U2MESK('F','RUPTURE0_92',1,OPTIO1)
          ENDIF
        ENDIF
C
C       -------------------------------
C       3.3. ==> CALCUL DE KG (3D LOC)
C       -------------------------------
C
        ELSE IF (CAS.EQ.'3D_LOCAL'.AND.OPTIO1.EQ.'CALC_K_G') THEN

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
              CALL U2MESG('F', 'RUPTURE0_93',1,VALK,1,VALI,0,0.D0)
            ENDIF

            CALL CAKG3D(OPTIO1,LATABL,MODELE,DEPLA,THETAI,MATE,COMPOR,
     &              NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,BASLOC,COURB,
     &              IORD,NDEG,THLAGR,GLAGR,THLAG2,PAIR,NDIMTE,
     &              EXITIM,TIME,NBPARA,LINOPA,FISS,
     &              LMELAS,NOMCAS)
   33     CONTINUE
C
C       ------------------------
C       3.3.2. ==>OPTION CALC_K_MAX
C       ------------------------
C
        ELSE IF (OPTIO1 .EQ.'CALC_K_MAX') THEN
          CALL MMAXKL(LATABL,MODELE,THETAI,MATE,COMPOR,NCHA,SYMECH,
     &               CHFOND,LNOFF,BASLOC,COURB,NDEG,THLAGR,GLAGR,
     &               THLAG2,PAIR,NDIMTE,NBPARA,LINOPA,
     &               FISS,LONVEC,IVEC,VCHAR,RESUCO,
     &               LMELAS,LNCAS,ZL(JNORD))
C
C       ------------------------
C       3.4. ==>OPTION K_G_MODA
C       ------------------------
C
        ELSE IF (OPTIO1 .EQ. 'K_G_MODA') THEN
C
C         3.4.1 ==>  K_G_MODA 2D
C         -----------------------
          IF (CAS.EQ.'2D') THEN
            CALL GETVID ( 'THETA','FOND_FISS', 1,1,1,FOND,N1)

C           FEM         
            IF (N1.NE.0) THEN
              DO 341 I = 1 , LONVEC
                IORD = ZI(IVEC-1+I)
                CALL RSEXCH(RESUCO,'DEPL',IORD,DEPLA,IRET)
                IF(IRET.NE.0) THEN
                    VALK(1) = 'DEPL'
                    VALI = IORD
                    CALL U2MESG('F', 'RUPTURE0_95',1,VALK,1,VALI,0,0.D0)
                ENDIF
                CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORD,0,IPULS,K8BID)
                PULS = ZR(IPULS)
                PULS = SQRT(PULS)
             
                CALL MEMOKG(OPTIO1,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                      ZK8(ICHA),SYMECH,FOND,IORD,PULS,NBPARA,
     &                      LINOPA)
 341           CONTINUE
 
C           X-FEM
            ELSE
              CALL ASSERT(LLEVST)
     
              DO 3422 I = 1 , LONVEC
                IORD = ZI(IVEC-1+I)
                CALL RSEXCH(RESUCO,'DEPL',IORD,DEPLA,IRET)
                IF(IRET.NE.0) THEN
                    VALK(1) = 'DEPL'
                    VALI = IORD
                    CALL U2MESG('F', 'RUPTURE0_95',1,VALK,1,VALI,0,0.D0)
                ENDIF
                CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORD,0,IPULS,K8BID)
                PULS = ZR(IPULS)
                PULS = SQRT(PULS)
                EXITIM = .FALSE.
                CHVITE =' '
                CHACCE =' '

                CALL MEFICG(OPTIO1,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                      ZK8(ICHA),SYMECH,FOND,NOEUD,EXITIM,0.D0,
     &                      IORD,PULS,NBPARA, LINOPA, NOPASE,TYPESE,
     &                      CHDESE,CHEPSE,CHSISE,CHVITE,CHACCE,LMELAS,
     &                      K16BID,COMPOR)
 3422         CONTINUE
            ENDIF 
C
C         3.4.2 ==> K_G_MODA 3D LOC
C         -------------------------
          ELSE
            DO 342 I = 1,LONVEC
              IORD = ZI(IVEC-1+I)
              CALL MEDOM1(MODELE,MATE,K8BID,VCHAR,NCHA,K4B,RESUCO,IORD)
              CALL JEVEUO(VCHAR,'L',ICHA)
              CALL RSEXCH(RESUCO,'DEPL',IORD,DEPLA,IRET)
              IF (IRET.NE.0) THEN
                  VALK (1) = 'DEPL'
                  VALI = IORD
                CALL U2MESG('F', 'RUPTURE0_95',1,VALK,1,VALI,0,0.D0)
              ENDIF
              CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORD,0,IPULS,K8B)
              PULS = ZR(IPULS)
              PULS = SQRT(PULS)
              EXITIM = .TRUE.

              CALL CAKGMO(OPTIO1,LATABL,MODELE,DEPLA,THETAI,MATE,COMPOR,
     &                    NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,BASLOC,
     &                    COURB,IORD,NDEG,THLAGR,GLAGR,THLAG2,PAIR,
     &                    NDIMTE,PULS,NBPARA,LINOPA,FISS)
 342        CONTINUE
          ENDIF       
C
C       --------------------------------------------
C       3.5. ==> CALCUL DE G, K_G(2D) ET DG
C       --------------------------------------------
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
     &      RESUCO,IORD)
            CALL JEVEUO(VCHAR,'L',ICHA)
            CALL RSEXCH(RESUCO,'DEPL',IORD,DEPLA,IRET)
            IF(IRET.NE.0) THEN
                  VALK (1) = 'DEPL'
                  VALI = IORD
                CALL U2MESG('F', 'RUPTURE0_93',1,VALK,1,VALI,0,0.D0)
            ENDIF
            CALL RSEXCH(RESUCO,'VITE',IORD,CHVITE,IRET)
            IF(IRET.NE.0) THEN
              CHVITE = ' '
            ELSE
              CALL RSEXCH(RESUCO,'ACCE',IORD,CHACCE,IRET)
            ENDIF
C
C           RECUPERATION DES CHAMNO DE DERIVEE LAGRANGIENNE DE 
C           DEPLACEMENT DANS LA SD RESULTAT DERIVE DE TYPE EVOL_ELAS
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
              CALL RSEXC2(1,1,LERES0,'EPSI_ELGA',IORD,CHEPSE,
     &                    OPTIO1,IRET)
              IF (IRET.GT.0) THEN
                VALI = IORD
                VALK (1) = 'EPSI_ELGA'
                VALK (2) = RESUCO
                VALK (3) = NOPASE
                CALL U2MESG('F', 'SENSIBILITE_8',3,VALK,1,VALI,0,0.D0)
              ENDIF
              CALL RSEXC2(1,1,LERES0,'SIEF_ELGA',IORD,
     &                    CHSISE,OPTIO1,IRET)
              IF (IRET.GT.0) THEN
                VALI = IORD
                VALK (1) = 'SIEF_ELGA'
                VALK (2) = RESUCO
                VALK (3) = NOPASE
                CALL U2MESG('F', 'SENSIBILITE_8',3,VALK,1,VALI,0,0.D0)
              ENDIF
            ENDIF
C
C           VERIFICATION DE LA PRESENCE DU CHAMPS SIEF_ELGA
C           LORSQUE CALCUL_CONTRAINTE='NON'
            IF(KCALC.EQ.'NON')THEN
               CALL RSEXCH(RESUCO,'SIEF_ELGA',IORD,K24B,IRET)
               IF (IRET.NE.0) THEN
                  VALK (1) = 'SIEF_ELGA'
                  VALI = IORD
                  CALL U2MESG('A+', 'RUPTURE0_93',1,VALK,1,VALI,0,0.D0)
                  CALL U2MESG('A', 'RUPTURE1_41',1,VALK,1,VALI,0,0.D0)
                  KCALC='OUI'
               ENDIF
            ENDIF
C
C
            IF(    (OPTIO1(1:6).EQ.'CALC_G'.AND.CAS.EQ.'2D')
     &         .OR. OPTIO1.EQ.'CALC_DG'
     &         .OR. OPTIO1.EQ.'CALC_DG_E'
     &         .OR. OPTIO1.EQ.'CALC_DG_FORC'
     &         .OR. OPTIO1.EQ.'CALC_DGG_E'
     &         .OR. OPTIO1.EQ.'CALC_DGG_FORC'
     &         .OR. OPTIO1.EQ.'CALC_G_GLOB') THEN
C
              CALL MECALG(OPTIO1,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                    ZK8(ICHA),SYMECH,COMPOR,EXITIM,TIME,IORD,
     &                    NBPARA,LINOPA,NOPASE,TYPESE,CHDESE,
     &                    CHEPSE,CHSISE,CHVITE,CHACCE,LMELAS,NOMCAS,
     &                    KCALC)
C
            ELSEIF(OPTIO1(1:6).EQ.'CALC_G'.AND.CAS.EQ.'3D_LOCAL')THEN
C
              CALL MECAGL(OPTIO1,LATABL,MODELE,DEPLA,THETAI,MATE,COMPOR,
     &                    NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,IORD,NDEG,
     &                    THLAGR,GLAGR,THLAG2,MILIEU,NDIMTE,PAIR,EXITIM,
     &                    TIME,NBPARA,LINOPA,CHVITE,CHACCE,LMELAS,
     &                    NOMCAS,KCALC)
C
            ELSE IF (OPTIO1(1:6).EQ.'CALC_K'.AND.CAS.EQ.'2D') THEN
C
              CALL MEFICG(OPTIO1,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                    ZK8(ICHA),SYMECH,FOND,NOEUD,EXITIM,TIME,IORD,
     &                    RBID,NBPARA, LINOPA, NOPASE,TYPESE,CHDESE,
     &                    CHEPSE,CHSISE,CHVITE,CHACCE,LMELAS,NOMCAS,
     &                    COMPOR)
C
            ELSE IF (OPTIO1.EQ.'CALC_DK_DG_E') THEN
              OPTIO2 = 'CALC_DG_E'
              KCALC2 = 'OUI'
              CALL MECALG(OPTIO2,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                    ZK8(ICHA),SYMECH,COMPOR,EXITIM,TIME,IORD,
     &                    3,LINOPA,NOPASE,TYPESE,CHDESE,
     &                    CHEPSE,CHSISE,CHVITE,CHACCE,LMELAS,NOMCAS,
     &                    KCALC2)
C
C             LES DERIVEES DE KI ET KII SONT NULLES.
C             ON LES AJOUTE DIRECTEMENT
              VAL(1) = 0.D0
              VAL(2) = 0.D0
              CALL TBAJLI(LATABL,2,LINOPA(4),IORD,VAL,CBID,K8B,1)
            ELSE IF (OPTIO1.EQ.'CALC_DK_DG_FORC') THEN
              CALL MEFICG(OPTIO1,LATABL,MODELE,DEPLA,THETA,MATE,NCHA,
     &                    ZK8(ICHA),SYMECH,FOND,NOEUD,EXITIM,TIME,IORD,
     &                    RBID, NBPARA, LINOPA, NOPASE,TYPESE,CHDESE,
     &                    CHEPSE,CHSISE,CHVITE,CHACCE,LMELAS,NOMCAS,
     &                    COMPOR)

            ELSE
              CALL U2MESS('F','RUPTURE0_96')
            ENDIF

 34         CONTINUE

            CALL JEDEMA()
 35       CONTINUE

        ENDIF
C
        IF(CAS.NE.'3D_LOCAL'.AND. NBPASE.GT.1)THEN
          K24B = BLAN24
          K24B(1:8)   = LATABL
          K24B(20:24) = '.TITR'
          CALL TITREA('T',LATABL,LATABL,K24B,'C',' ',0,'G' )
        ELSE
          CALL TITRE
        ENDIF

 30   CONTINUE

      CALL JEDETC('G','&&NMDORC',1)

C     ON AJOUTE LA COLONNE IDENTIFIANT LA TABLE
      CALL CGAJID(IFOND,IFISS,FOND,FISS,LATABL)

      CALL JEDEMA()

      END
