      SUBROUTINE OP0100()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 28/08/2012   AUTEUR TRAN V-X.TRAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

C ==> VARIABLES LOCALES
C
      INCLUDE 'jeveux.h'
      INTEGER IFM,NIV,N1,LONVEC,IORD, IBID,I,IAD,NCAS,JNCAS,JNORD
      INTEGER VALI,NBTROU,NUTROU,INDIIS,IIND, IRET2
      INTEGER NRES,NP,NC,NEXCI,NCHA,IRET,ICHA,IVEC,NBPARA
      INTEGER IFOND,LNOFF,JINST, JOPT
      INTEGER NDEG,NBRE,IADNUM,IADRMA, NBROPT
      INTEGER NBR8,IADRCO,IADRNO,NBNO,J,IPULS,IORD1,IORD2
      INTEGER NBORN,NBCO,IBOR,IG,LNOEU,LABSCU,NBVAL
      INTEGER NDIMTE,NCELAS,IER,ITHET,NDIM,IFISS
      INTEGER NXPARA,IRXFEM, NBMO1, NT, N1M
      PARAMETER (NXPARA = 11)

C
      REAL*8  TIME,TIMEU,TIMEV,PREC,R8B,DIR(3)
      REAL*8  RINF,RSUP,MODULE,PULS,RBID
C
      COMPLEX*16 C16B
C
      CHARACTER*4 K4B
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'OP0100' )
      CHARACTER*8 MODELE,RESUCO,K8B,K8BID,KCALC
      CHARACTER*8 FOND,FISS,LITYPA(NXPARA),SYMECH,CRIT
      CHARACTER*8 LATABL,NOMA,THETAI,NOEUD,CAS,TYPFIS,CONFIG
      CHARACTER*16 TYPCO,OPER,OPTION,TYSD, LINOPA(NXPARA),SUITE
      CHARACTER*16 OPTIO2,K16B,NOMCAS,K16BID, MOCLEF(2)
      CHARACTER*19 GRLT
      CHARACTER*19 VCHAR, LISOPT,VECORD
      CHARACTER*24 DEPLA,MATE,K24B,COMPOR,CHVITE,CHACCE
      CHARACTER*24 VALK(3),BASFON,FONOEU
      CHARACTER*24 SDTHET,CHFOND,BASLOC,THETA
      CHARACTER*24 LISSTH,LISSG,OBJMA,NOMNO,COORN
      CHARACTER*24 TRAV1,TRAV2,TRAV3,STOK4
      CHARACTER*24 OBJ1,TRAV4,COURB,DEPLA1,DEPLA2
C
      LOGICAL EXITIM,THLAGR,CONNEX,GLAGR,MILIEU,DIREC
      LOGICAL THLAG2,PAIR,LNCAS,LMELAS,LLEVST, INCR
      INTEGER      IARG
C
C
C==============
C 1. PREALABLES
C==============
C
      CALL JEMARQ()
      VCHAR  = '&&'//NOMPRO//'.CHARGES'
      COURB  = '&&'//NOMPRO//'.COURB'
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
      CALL GETRES(LATABL,TYPCO,OPER)
      IF ( NIV.GE.2 ) CALL U2MESK('I','RUPTURE0_3',1,LATABL)
C
C     ----------------
C     2.2. OPTION
C     ----------------
      CALL GETVTX(' ','OPTION',0,IARG,1,OPTION,IBID)
C
      CALL GETVID (' ','RESULTAT',0,IARG,1,RESUCO,NRES)
      VECORD = '&&OP0100.VECTORDR'
      CALL GETVR8(' ','PRECISION',0,IARG,1,PREC,NP)
      CALL GETVTX(' ','CRITERE'  ,0,IARG,1,CRIT,NC)
      CALL RSUTNU ( RESUCO, ' ', 0, VECORD, LONVEC, PREC, CRIT, IER )

      CALL ASSERT(IER.EQ.0)
      CALL GETTCO(RESUCO,TYSD)
      CALL GETFAC('EXCIT',NEXCI)
      IF (TYSD.EQ.'DYNA_TRANS') THEN
        IF (NEXCI.EQ.0 ) THEN
          CALL U2MESS('F','RUPTURE0_9')
        ENDIF
      ENDIF
      IF (NEXCI.NE.0) THEN
        CALL U2MESS('A','RUPTURE0_55')
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
      
C      CALL NMDORC(MODELE,COMPOR,K24B)
C     RECUPERATION DE LA CARTE DE COMPORTEMENT UTILISEE DANS LE CALCUL
      NBMO1 = 2
      MOCLEF(1) = 'COMP_INCR'
      MOCLEF(2) = 'COMP_ELAS'
      NT=0
      DO 10 I=1,NBMO1
         CALL GETFAC(MOCLEF(I),N1M)
         NT=MAX(NT,N1M)
10    CONTINUE
      
C      CALL NMDORC(MODELE,COMPOR,K24B)
      IRET2 = 1
      IF (NT .EQ. 0) THEN    
         CALL RSEXCH(' ',RESUCO,'COMPORTEMENT',IORD,COMPOR,IRET)
         
C  LE CAS DE MECA-STATIQUE, LA CARTE DE COMPORTEMENT DANS RESUCO 
C  N'EXISTE PAS, ON IMPOSE DANS CALC_G COMP_ELAS       
         IF (IRET .NE. 0) THEN
            CALL NMDORC(MODELE,COMPOR,K24B)
            IRET2 = 0
         ENDIF
      ELSE      
         CALL NMDORC(MODELE,COMPOR,K24B)
      ENDIF
      
C ON VERIFIE LA COHERENCE DE LOI DE COMPORTEMENT SI SNL      
      IF (IRET2 .NE. 0) THEN
         CALL GVERLC(RESUCO,COMPOR,IORD,INCR  ) 
      ELSE
         INCR = .FALSE.
      ENDIF

      IF (INCR) THEN
      
         LISOPT = '&&OP0100.LIS_OPTION'
         NBROPT = 2
         
         CALL WKVECT(LISOPT,'V V K16',NBROPT,JOPT)
         ZK16(JOPT) = 'VARI_ELNO'
         ZK16(JOPT+1) = 'EPSP_ELNO'
         CALL CCBCOP(RESUCO,RESUCO,VECORD, LONVEC,LISOPT,NBROPT)


      ENDIF
      
C      CALL GVERLC(RESUCO,COMPOR,IORD)
      
      CALL JEVEUO(VCHAR,'L',ICHA)

      LNCAS=.FALSE.
      LMELAS=.FALSE.
      IF (TYSD.EQ.'MULT_ELAS') THEN
        LMELAS=.TRUE.
        CALL GETVTX(' ','NOM_CAS',0,IARG,0,K16B,NCAS)

        IF(NCAS.NE.0)THEN
          NCAS=-NCAS
          CALL WKVECT('&&'//NOMPRO//'_MULTELAS_NOMCAS','V V K16',NCAS,
     &                JNCAS)
          CALL GETVTX(' ','NOM_CAS',0,IARG,NCAS,ZK16(JNCAS),IRET)
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
               IIND=INDIIS(ZI(IVEC),NUTROU,1,LONVEC)
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
         CALL GETVTX(' ','CALCUL_CONTRAINTE',0,IARG,1,KCALC,IRET)
         IF(KCALC(1:3).EQ.'NON')THEN
            CALL GETVID('THETA','FISSURE',1,IARG,0,FISS,IFISS)
            IF (IFISS.NE.0) CALL U2MESS('F','RUPTURE1_39')
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
      CALL GETVID ('THETA','THETA', 1,IARG,1, SDTHET,ITHET)

      CALL GETVID('THETA','FOND_FISS',1,IARG,1,FOND,IFOND)
      CALL GETVID('THETA','FISSURE',  1,IARG,1,FISS,IFISS)

C     AVEC UN MODELE FEM ON NE DOIT PAS AVOIR FISSURE
      CALL EXIXFE(MODELE, IRXFEM)
      IF (IRXFEM.EQ.0.AND.IFISS.NE.0) CALL U2MESS('F','RUPTURE1_2')

C     AVEC UN MODELE XFEM ON DOIT AVOIR FISSURE
      IF (IRXFEM.NE.0.AND.IFISS.EQ.0) CALL U2MESS('F','RUPTURE1_3')

      IF (IRXFEM.EQ.0.AND.ITHET.EQ.0) THEN
         TYPFIS = FOND
C        LE CAS DES FONDS DOUBLES N'EST PAS TRAITE
         CALL JEEXIN(FOND//'.FOND.NOEU',IRET)
         IF (IRET.EQ.0) CALL U2MESS('F','RUPTURE1_4')
      ELSE IF (IRXFEM.NE.0.AND.ITHET.EQ.0) THEN
         TYPFIS = FISS
      ENDIF

C     ATTENTION, IL SE PEUT QUE NI FOND_FISS NI FISSURE SOIT PRESENT
C     (EN DONNANT THETA PAR EX...)
      LLEVST=.FALSE.
      IF (IFISS.NE.0) LLEVST=.TRUE.

C     ON RECHERCHE LA PRESENCE DE SYMETRIE
      SYMECH='NON'
      CALL GETVTX ('THETA', 'SYME', 1,IARG,1, SYMECH,IBID)
      IF (IFOND.NE.0) THEN
        CALL DISMOI('F','SYME',FOND,'FOND_FISS',IBID,SYMECH,IER)
      ENDIF

C     2.6.1 : THETA FOURNI
      IF (ITHET.NE.0) THEN

C       ON NE DOIT PAS ETRE DANS UN CALCUL 3D LOCAL
        IF (CAS.EQ.'3D_LOCAL') CALL U2MESS('F','RUPTURE0_57')

C       LE MOT-CLE DIRECTION NE DOIT PAS ETRE RENSEIGNE
C       NORMALEMENT, LE CAPY L'INTERDIT...
        CALL GETVR8('THETA','DIRECTION',1,IARG,0,R8B,NBR8)
        IF (NBR8.NE.0) CALL U2MESS('F','RUPTURE0_80')

        CALL GETTCO(SDTHET,TYPCO)
        IF (TYPCO(1:10).EQ.'THETA_GEOM') THEN
          CALL RSEXCH('F',SDTHET,'THETA',0,THETA,N1)
        ELSE
          THETA=SDTHET
        ENDIF
      ENDIF

C     2.6.2 : THETA CALCULE ????
      IF (ITHET.EQ.0.AND.CAS.NE.'3D_LOCAL') THEN

         OBJ1 = MODELE//'.MODELE    .LGRF'
         CALL JEVEUO ( OBJ1, 'L', IADRMA )
         NOMA  = ZK8(IADRMA)
         THETA = LATABL//'_CHAM_THETA'
         NOMNO = NOMA//'.NOMNOE'
         COORN = NOMA//'.COORDO    .VALE'
         CALL JEVEUO ( COORN, 'L', IADRCO )
         CALL GETVR8 ( 'THETA', 'DIRECTION', 1,IARG,0, R8B, NBR8)
         DIREC=.FALSE.
         IF ( NBR8 .NE. 0 ) THEN
            NBR8  = -NBR8
            SUITE='.'
            IF (NDIM.EQ.2) SUITE=',LA 3-EME NULLE.'
            IF ( NBR8 .NE. 3 ) THEN
               CALL U2MESK('F','RUPTURE0_30',1,SUITE)
            ELSE
               CALL GETVR8 ('THETA','DIRECTION',1,IARG,3,DIR,NBR8)
               DIREC=.TRUE.
            ENDIF
         ELSE
           IF (.NOT.LLEVST.AND.NDIM.EQ.2) CALL U2MESS('F','RUPTURE0_81')
         ENDIF

C      - THETA 2D (COURONNE)
        IF(NDIM.EQ.2)THEN
          CALL GVER2D ( NOMA,1,'THETA',NOMNO,
     &                 NOEUD, RINF, RSUP, MODULE )
          CALL GCOU2D ( 'V',THETA, NOMA, NOMNO, NOEUD, ZR(IADRCO), RINF,
     &                    RSUP, MODULE, DIREC, DIR )
C     - THETA 3D
        ELSE IF(NDIM.EQ.3)THEN
          FONOEU  = FOND//'.FOND.NOEU'
          CALL JELIRA ( FONOEU, 'LONMAX', NBNO, K8B )
          CALL JEVEUO ( FONOEU, 'L', IADRNO )
          CALL GVERIG ( NOMA,1,FONOEU, NBNO, NOMNO, COORN,
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

C     CERTAINES OPTIONS DEMANDENT FOND_FISS OBLIGATOIREMENT
      IF (OPTION.EQ.'G_MAX'.OR.OPTION.EQ.'G_BILI') THEN
        IF (IFOND.EQ.0) CALL U2MESK('F','RUPTURE0_48',1,OPTION)
      ENDIF
C
C     CERTAINES OPTIONS EN MODELISATION FEM 3D NE TRAITENT PAS LES
C     FISSURES EN CONFIGURATION DECOLLEE
      IF (IFOND.NE.0.AND.CAS.EQ.'3D_LOCAL'.AND.
     &    (OPTION.EQ.'CALC_K_G'.OR.
     &     OPTION.EQ.'K_G_MODA'.OR.
     &     OPTION.EQ.'CALC_K_MAX' )) THEN
        CALL DISMOI('F','CONFIG_INIT',FOND,'FOND_FISS',IBID,CONFIG,IER)
        IF(CONFIG.EQ.'DECOLLEE') CALL U2MESK('F','RUPTURE0_29',1,OPTION)
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

C       SI FOND_FISS
        ELSEIF (IFOND.NE.0) THEN
          CHFOND = FOND//'.FONDFISS'
          FONOEU = FOND//'.FOND.NOEU'
          CALL JELIRA(FONOEU,'LONMAX',LNOFF,K8B)
          LLEVST=.FALSE.
        ENDIF

      ENDIF


C     2.7.2 LISSAGE EN FOND DE FISSURE

C     METHODE DE DECOMPOSITION DE THETA ET G : LAGRANGE OU LEGENDRE
      CALL GETVTX('LISSAGE','LISSAGE_THETA',1,IARG,1,LISSTH,IBID)
      CALL GETVTX('LISSAGE','LISSAGE_G',1,IARG,1,LISSG,IBID)
      CALL GETVIS('LISSAGE','DEGRE',1,IARG,1,NDEG,IBID)

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
     &        OPTION.EQ.'G_BILI'.OR.
     &       (OPTION.EQ.'CALC_G'.AND.IFISS.NE.0)) THEN
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
C
C     =======================
C     3. CALCUL DE L'OPTION
C     =======================

C
C     DETERMINATION AUTOMATIQUE DE THETA (CAS 3D LOCAL)
C
      IF (CAS.EQ.'3D_LOCAL'.AND.IFISS.NE.0) THEN

C         ON A TOUJOURS
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


        ELSEIF (CAS.EQ.'3D_LOCAL'.AND.IFOND.NE.0) THEN

         CALL JEVEUO(FONOEU,'L',IADNUM)
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

         CALL GVERI3(CHFOND,LNOFF,THLAGR,THLAG2,NDEG,TRAV1,TRAV2,TRAV3)
         CALL GCOUR2(THETAI,NOMA,MODELE,NOMNO,COORN,LNOFF,TRAV1,TRAV2,
     &              TRAV3,FONOEU,FOND,CONNEX,STOK4,THLAGR,THLAG2,NDEG,
     &              MILIEU,NDIMTE,PAIR)
         CALL GIMPT2(THETAI,NBRE,TRAV1,TRAV2,TRAV3,FONOEU,STOK4,LNOFF,0)

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
      CALL CGCRTB(LATABL,OPTION,NDIM,LMELAS,CAS,LLEVST,
     &            NBPARA,LINOPA,LITYPA)
C
C       --------------------------------------------------------------
C       3.1. ==> CALCUL DE LA FORME BILINEAIRE DU TAUX DE RESTITUTION
C       --------------------------------------------------------------
C
      IF (OPTION(1:6) .EQ.'G_BILI'.OR. OPTION(1:5) .EQ.'G_MAX') THEN

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
              CALL RSEXCH(' ',RESUCO,'DEPL',IORD1,DEPLA1,IRET)
              IF(LONVEC.EQ.1)THEN
                IORD2  = IORD1
                DEPLA2 = DEPLA1
              ELSE
                IORD2 = ZI(IVEC-1+J)
                CALL RSEXCH('F',RESUCO,'DEPL',IORD2,DEPLA2,IRET)
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
     &                       LMELAS,NOMCAS,FONOEU)
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
        IF (OPTION(1:5) .EQ.'G_MAX') THEN

          CALL GETFAC ('BORNES', NBORN )
          IF (NBORN.NE.0) THEN
              NBCO = 2*NBORN
              CALL WKVECT('&&'//NOMPRO//'.COUPLES_BORNES','V V R8',
     &                   NBCO,IBOR)
              DO 3213 I=1, NBORN
                CALL GETVIS('BORNES','NUME_ORDRE',I,IARG,1,IORD,N1)
                CALL GETVR8('BORNES','VALE_MIN',I,IARG,1,
     &                      ZR(IBOR+2*(IORD-1)),N1)
                CALL GETVR8('BORNES','VALE_MAX',I,IARG,1,
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
            CALL U2MESK('F','RUPTURE0_92',1,OPTION)
          ENDIF
        ENDIF
C
C       -------------------------------
C     3.3. ==> CALCUL DE KG (3D LOC)
C       -------------------------------
C
      ELSE IF (CAS.EQ.'3D_LOCAL'.AND.OPTION.EQ.'CALC_K_G') THEN

          BASLOC=TYPFIS//'.BASLOC'
          CALL XCOURB(BASLOC,NOMA,MODELE,COURB)

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
            CALL RSEXCH('F',RESUCO,'DEPL',IORD,DEPLA,IRET)

            CALL CAKG3D(OPTION,LATABL,MODELE,DEPLA,THETAI,MATE,COMPOR,
     &              NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,BASLOC,COURB,
     &              IORD,NDEG,THLAGR,GLAGR,THLAG2,PAIR,NDIMTE,
     &              EXITIM,TIME,NBPARA,LINOPA,TYPFIS,
     &              LMELAS,NOMCAS,MILIEU,CONNEX)

   33     CONTINUE
C
C       ------------------------
C     3.3.2. ==>OPTION CALC_K_MAX
C       ------------------------
C
      ELSE IF (OPTION .EQ.'CALC_K_MAX') THEN

          BASLOC=TYPFIS//'.BASLOC'
          CALL XCOURB(BASLOC,NOMA,MODELE,COURB)

          CALL MMAXKL(LATABL,MODELE,THETAI,MATE,COMPOR,NCHA,SYMECH,
     &               CHFOND,LNOFF,BASLOC,COURB,NDEG,THLAGR,GLAGR,
     &               THLAG2,PAIR,NDIMTE,NBPARA,LINOPA,
     &               TYPFIS,LONVEC,IVEC,VCHAR,RESUCO,
     &               LMELAS,LNCAS,ZL(JNORD),MILIEU,CONNEX)
C
C       ------------------------
C     3.4. ==>OPTION K_G_MODA
C       ------------------------
C
      ELSE IF (OPTION .EQ. 'K_G_MODA') THEN
C
C         3.4.1 ==>  K_G_MODA 2D
C         -----------------------
          IF (CAS.EQ.'2D') THEN
            CALL GETVID ( 'THETA','FOND_FISS', 1,IARG,1,FOND,N1)

C           FEM
            IF (N1.NE.0) THEN
              DO 341 I = 1 , LONVEC
                IORD = ZI(IVEC-1+I)
                CALL RSEXCH('F',RESUCO,'DEPL',IORD,DEPLA,IRET)
                CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORD,0,IPULS,K8BID)
                PULS = ZR(IPULS)
                PULS = SQRT(PULS)

                CALL MEMOKG(OPTION,LATABL,MODELE,DEPLA,THETA,MATE,
     &                      SYMECH,FOND,IORD,PULS,NBPARA,
     &                      LINOPA)
 341           CONTINUE

C           X-FEM
            ELSE
              CALL ASSERT(LLEVST)

              DO 3422 I = 1 , LONVEC
                IORD = ZI(IVEC-1+I)
                CALL RSEXCH('F',RESUCO,'DEPL',IORD,DEPLA,IRET)
                CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORD,0,IPULS,K8BID)
                PULS = ZR(IPULS)
                PULS = SQRT(PULS)
                EXITIM = .FALSE.
                CHVITE =' '
                CHACCE =' '

                CALL MEFICG(OPTION,LATABL,MODELE,DEPLA,THETA,
     &                      MATE,NCHA,ZK8(ICHA),SYMECH,FOND,
     &                      NOEUD,0.D0,IORD  ,PULS  ,NBPARA,
     &                      LINOPA,LMELAS,K16BID,COMPOR)
 3422         CONTINUE
            ENDIF
C
C         3.4.2 ==> K_G_MODA 3D LOC
C         -------------------------
          ELSE

            BASLOC=TYPFIS//'.BASLOC'
            CALL XCOURB(BASLOC,NOMA,MODELE,COURB)

            DO 342 I = 1,LONVEC
              IORD = ZI(IVEC-1+I)
              CALL MEDOM1(MODELE,MATE,K8BID,VCHAR,NCHA,K4B,RESUCO,IORD)
              CALL JEVEUO(VCHAR,'L',ICHA)
              CALL RSEXCH('F',RESUCO,'DEPL',IORD,DEPLA,IRET)
              CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORD,0,IPULS,K8B)
              PULS = ZR(IPULS)
              PULS = SQRT(PULS)
              EXITIM = .TRUE.

              CALL CAKGMO(OPTION,LATABL,MODELE,DEPLA,THETAI,MATE,COMPOR,
     &                    NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,BASLOC,
     &                    COURB,IORD,NDEG,THLAGR,GLAGR,THLAG2,PAIR,
     &                    NDIMTE,PULS,NBPARA,LINOPA,TYPFIS,MILIEU,
     &                    CONNEX)
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
            CALL RSEXCH('F',RESUCO,'DEPL',IORD,DEPLA,IRET)
            CALL RSEXCH(' ',RESUCO,'VITE',IORD,CHVITE,IRET)
            IF(IRET.NE.0) THEN
              CHVITE = ' '
            ELSE
              CALL RSEXCH(' ',RESUCO,'ACCE',IORD,CHACCE,IRET)
            ENDIF
C
C           VERIFICATION DE LA PRESENCE DU CHAMPS SIEF_ELGA
C           LORSQUE CALCUL_CONTRAINTE='NON'
            IF(KCALC.EQ.'NON')THEN
               CALL RSEXCH(' ',RESUCO,'SIEF_ELGA',IORD,K24B,IRET)
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
            IF(    (OPTION(1:6).EQ.'CALC_G'.AND.CAS.EQ.'2D')
     &         .OR. OPTION.EQ.'CALC_G_GLOB') THEN
C
              CALL MECALG(OPTION,LATABL,MODELE,DEPLA,THETA,
     &                    MATE,NCHA,ZK8(ICHA),SYMECH,COMPOR,INCR,
     &                    TIME,IORD,NBPARA,LINOPA,CHVITE,
     &                    CHACCE,LMELAS,NOMCAS,KCALC)
C
            ELSEIF(OPTION(1:6).EQ.'CALC_G'.AND.CAS.EQ.'3D_LOCAL')THEN
C
              CALL MECAGL(OPTION,LATABL,MODELE,DEPLA,THETAI,MATE,COMPOR,
     &                    NCHA,ZK8(ICHA),SYMECH,CHFOND,LNOFF,IORD,NDEG,
     &                    THLAGR,GLAGR,THLAG2,MILIEU,NDIMTE,PAIR,EXITIM,
     &                    TIME,NBPARA,LINOPA,CHVITE,CHACCE,LMELAS,
     &                    NOMCAS,KCALC,FONOEU)
C
            ELSE IF (OPTION(1:6).EQ.'CALC_K'.AND.CAS.EQ.'2D') THEN
C
              CALL MEFICG(OPTION,LATABL,MODELE,DEPLA,THETA,
     &                    MATE,NCHA,ZK8(ICHA),SYMECH,FOND,
     &                    NOEUD,TIME,IORD,RBID,NBPARA,
     &                    LINOPA,LMELAS,NOMCAS,COMPOR)

            ELSE
              CALL U2MESS('F','RUPTURE0_96')
            ENDIF

 34         CONTINUE

            CALL JEDEMA()
 35       CONTINUE

        ENDIF

      CALL TITRE



      CALL JEDETC('G','&&NMDORC',1)

      CALL JEDEMA()

      END
