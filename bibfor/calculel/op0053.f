      SUBROUTINE OP0053 ( IER )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/01/2006   AUTEUR G8BHHXD X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C
C      OPERATEUR :     CALC_G_THETA_T
C
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       JEVEUX: JEMARQ,JEVEUO,RSUTNU,JECREO,JEECRA,DISMOI,RSEXCH,
C               RSEXC2,TBCRSD,TBAJPA,RSADPA,WKVECT,TBEXVE,DETRSD,
C               JEDETR,JEDETC,JEDEMA.
C       MESSAGE: INFMAJ,UTMESS,UTDEBM,UTIMPI,UTIMPK,UTFINM.
C       PARAMETRE: GETRES,GETVID,GETVTX,GETVR8,GETFAC,GETVIS.
C       DIVERS: RCMFMC,NMDORC,MEBILG,MEMAXG,MECALG,MLAGRG,MEFICG,
C               TITRE.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/12/00 (OB): TOILETTAGE FORTRAN, INTRODUCTION DE L'OPTION
C                      'CALC_DG' CALCULANT LA DERIVEE DE G PAR RAPPORT
C                      A UNE VARIATION DE DOMAINE.
C       04/07/03 (GN): MISE EN PLACE DU MECANISME DES SENSIBILITES
C----------------------------------------------------------------------
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
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'OP0053' )
C
      INTEGER IFM, NIV
      INTEGER IAUX, JAUX, ICHAR, NEXCI, L, IEXC
      INTEGER      I,J,ICHA,IBID,IORD,IRET,IVEC,IPROPA,IFOND,IRET1
      INTEGER      JINST,LONVEC,NBPRUP,IORD1,IORD2, IPULS
      INTEGER      NBINST,NC,NCHA,NDEP,NP,NRES,NBVAL,IG,IBOR,NBORN
      INTEGER      NBCO,N1,N2,N3
      INTEGER NRPASE,NBPASE,NRPASS,NBPASS,TYPESE
      INTEGER ADRECG,ADCHSE
      INTEGER NVITES,NACCE

      REAL*8       TIME,ALPHA,PREC,RBID,PULS,TIMEU,TIMEV,VAL(3)
      COMPLEX*16 CBID
      CHARACTER*4 K4BID
      CHARACTER*5 SUFFIX
      CHARACTER*8  MODELE, MATERI, TYPRUP(6)
      CHARACTER*8  RESUCO,TABLE1,FOND,SYMECH,K8BI1,K8BID,CRIT,AFFCHA
      CHARACTER*8 LATAB1, LERES0
      CHARACTER*8 MATERS
      CHARACTER*8 NOPASE
      CHARACTER*13 INPSCO
      CHARACTER*16 TYPCO, OPER, OPTION, NOPRUP(6), OPTIO1, OPTIO2
      CHARACTER*16 TYSD
      CHARACTER*19 KCHA
      CHARACTER*24 BLAN24
      CHARACTER*24 STYPSE, NORECG
      CHARACTER*24 MATE,COMPOR,DEPLA,VECORD,VCHAR,THETA,SDTHET
      CHARACTER*24 NOMCHA,CHARSE,MATES
      CHARACTER*24 DEPLA1,DEPLA2,CHDESE,CHEPSE,CHSISE
      CHARACTER*24 LIGRCH,LCHIN
      CHARACTER*24 CHVITE,CHACCE
      CHARACTER*24 K24BID

      LOGICAL      EXITIM, EXCHSE
C
      INTEGER NBTYCH
      PARAMETER (NBTYCH=17)
C
      CHARACTER*6 NOMLIG(NBTYCH)
      CHARACTER*8 TYPEPS(-2:NBTYCH)

      DATA NOMLIG/'.FORNO','.F3D3D','.F2D3D','.F1D3D','.F2D2D','.F1D2D',
     &     '.F1D1D','.PESAN','.ROTAT','.PRESS','.FELEC','.FCO3D',
     &     '.FCO2D','.EPSIN','.FLUX','.VEASS','.ONDPL'/
      DATA TYPEPS/'MATERIAU','CARAELEM','DIRICHLE','FORCE   ',
     &     'FORCE   ','FORCE   ','FORCE   ','FORCE   ','FORCE   ',
     &     'FORCE   ','.PESAN','.ROTAT','FORCE   ','.FELEC','FORCE   ',
     &     'FORCE   ','.EPSIN','.FLUX','.VEASS','.ONDPL'/
C
C====
C 1. PREALABLES
C====
C
      CALL JEMARQ()
C               123456789012345678901234
      VCHAR  = '&&'//NOMPRO//'.CHARGES'
      BLAN24 = '                        '
      INPSCO = '&&'//NOMPRO//'_PSCO'
C               12   345678   9012345678901234
      NORECG = '&&'//NOMPRO//'_PARA_SENSI     '
C
C====
C  2. RECUPERATION DES OPERANDES
C====
C 2.1. ==>  RECUPERATION DU NIVEAU D'IMPRESSION
C
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
C 2.2. ==> LE CONCEPT DE SORTIE, SON TYPE, LA COMMANDE
C
      CALL GETRES(TABLE1,TYPCO,OPER)
      IF ( NIV.GE.2 ) THEN
        CALL UTMESS('I',NOMPRO,'CREATION DE LA TABLE '//TABLE1)
      ENDIF
C
C=======================================================================
C 2.3. ==> OPTION
C=======================================================================

      CALL GETVTX(' ','OPTION'    ,0,1,1,OPTION,IBID)

C=======================================================================
C 2.4. ==> RECUPERATION DU DEPLACEMENT A PARTIR DU MOT CLE DEPL
C          OU EXTRACTIOND'UN OU PLUSIEURS DEPLACEMENTS A PARTIR D'UN
C          RESULTAT
C=======================================================================

      CALL GETVID(' ','DEPL',0,1,1,DEPLA,NDEP)

      IF(NDEP.NE.0) THEN
        CALL GETVID(' ','MODELE'    ,0,1,1,MODELE,N1)
        CALL GETVID(' ','CHAM_MATER',0,1,1,MATERI,N2)
        IF (N1.EQ.0 ) THEN
           CALL UTMESS('F','OP0053','SI LE MOT-CLE DEPL EST PRESENT'//
     +                    ' ALORS LE MOT-CLE MODELE EST OBLIGATOIRE.')
        ENDIF
        IF (N2.EQ.0 ) THEN
           CALL UTMESS('F','OP0053','SI LE MOT CLE DEPL EST PRESENT'//
     +                 ' ALORS LE MOT-CLE CHAM_MATER EST OBLIGATOIRE.')
        ENDIF
C
        CALL RCMFMC(MATERI,MATE)
        CALL NMDORC(MODELE,COMPOR,K24BID)
        LONVEC = 1
        IORD = 0
        CALL GETVR8(' ','INST',0,1,0,RBID,NBINST)
        IF (NBINST.EQ.0) THEN
          EXITIM = .FALSE.
          TIME  = 0.D0
          TIMEU = 0.D0
          TIMEV = 0.D0
        ELSE
          NBINST = -NBINST
          IF(NBINST.GT.1) THEN
            CALL UTMESS('F',NOMPRO,'LA LISTE D''INSTANTS NE DOIT'
     &        //'COMPORTER QU''UN SEUL INSTANT AVEC LE MOT-CLE DEPL')
          ENDIF
          CALL GETVR8(' ','INST',0,1,NBINST,TIME,IBID)
          EXITIM = .TRUE.
          TIMEU = TIME
          TIMEV = TIME
        ENDIF
      ENDIF

      CHVITE = ' '
      CHACCE = ' '
      CALL GETVID(' ','VITE',0,1,1,CHVITE,NVITES)
      IF(NVITES.NE.0) THEN
        CALL GETVID(' ','ACCE',0,1,1,CHACCE,NACCE)
      ENDIF

      CALL GETVID (' ','RESULTAT',0,1,1,RESUCO,NRES)
      IF (NRES.NE.0) THEN
        VECORD = '&&'//NOMPRO//'.VECTORDR'
        CALL GETVR8(' ','PRECISION',0,1,1,PREC,NP)
        CALL GETVTX(' ','CRITERE'  ,0,1,1,CRIT,NC)
        CALL RSUTNU ( RESUCO, ' ', 0, VECORD, LONVEC, PREC, CRIT, IER )
        IF(IER.NE.0) THEN
          CALL UTMESS('F',NOMPRO,
     >                'PROBLEME A LA RECUPERATION D''UN CHAMP')
        ENDIF
        CALL GETTCO(RESUCO,TYSD)
        IF (TYSD.EQ.'DYNA_TRANS') THEN
           CALL GETVID(' ','MODELE'    ,0,1,1,MODELE,N1)
           CALL GETVID(' ','CHAM_MATER',0,1,1,MATERI,N2)
           CALL GETFAC('EXCIT',NEXCI)
           IF (N1.EQ.0 ) THEN
             CALL UTMESS('F','OP0053','DANS LE CAS D''UNE SD RESULTAT'//
     +                    ' DE TYPE DYNA_TRANS, LE MOT-CLE MODELE EST'//
     +                    ' OBLIGATOIRE.')
           ENDIF
           IF (N2.EQ.0 ) THEN
             CALL UTMESS('F','OP0053','DANS LE CAS D''UNE SD RESULTAT'//
     +                    ' DE TYPE DYNA_TRANS, LE MOT-CLE CHAM_MATER'//
     +                    ' EST OBLIGATOIRE.')
           ENDIF
           IF (NEXCI.EQ.0 ) THEN
             CALL UTMESS('F','OP0053','DANS LE CAS D''UNE SD RESULTAT'//
     +                    ' DE TYPE DYNA_TRANS, LE MOT-CLE EXCIT'//
     +                    ' EST OBLIGATOIRE.')
           ENDIF
        ENDIF
        
C VERIFICATION DE LA COHERENCE DES MOTS CLE AVEC L'OPTION DE CALCUL

        IF (((OPTION.EQ.'K_G_MODA') .AND. (TYSD.NE.'MODE_MECA')) .OR.
     +     ((TYSD.EQ.'MODE_MECA') .AND. (OPTION.NE.'K_G_MODA'))) THEN
          CALL UTMESS('F','OP0053','POUR UN RESULTAT DE TYPE '//
     +          'MODE_MECA L OPTION DE CALCUL DOIT ETRE K_G_MODA.')
        ENDIF

        CALL JEVEUO ( VECORD, 'L', IVEC )
        IORD = ZI(IVEC)
        CALL MEDOM1(MODELE,MATE,K8BID,VCHAR,NCHA,K4BID,RESUCO,IORD)
        CALL NMDORC(MODELE,COMPOR,K24BID)
        CALL JEVEUO(VCHAR,'L',ICHA)
      ENDIF

C=======================================================================
C 2.5. ==> CHARGES
C=======================================================================
      CALL GETVID(' ','DEPL',0,1,1,DEPLA,NDEP)
      IF(NDEP.NE.0) THEN
        CALL GETFAC('EXCIT',NEXCI)
          NCHA = 0
        IF (NEXCI .GT. 0) THEN
          DO 21 IEXC = 1,NEXCI
            CALL GETVID('EXCIT','CHARGE',IEXC,1,1,K24BID,L)
            IF (L .EQ. 1) NCHA = NCHA + 1
 21       CONTINUE
          CALL JEEXIN(VCHAR,IRET)
          IF(IRET.NE.0) CALL JEDETR(VCHAR)
          N3=MAX(1,NCHA)
          CALL WKVECT(VCHAR,'V V K8',N3,ICHA)
          IF (NCHA .NE. 0) THEN
           DO 22 , I = 1,NCHA
             CALL GETVID('EXCIT','CHARGE',I,1,1,ZK8(ICHA+I-1),IBID)
 22        CONTINUE
           CALL DISMOI('F','NOM_MODELE',ZK8(ICHA),'CHARGE',IBID,
     &                  K8BI1,IER)
           IF (K8BI1.NE.MODELE) THEN
             CALL UTMESS('F',NOMPRO,'LES CHARGES NE S''APPUIENT PAS'
     &                         //' SUR LE MODELE DONNE EN ARGUMENT')
           ENDIF
           DO 23 , I = 1,NCHA
             CALL DISMOI('F','NOM_MODELE',ZK8(ICHA-1+I),'CHARGE',IBID,
     &                   K8BID,IER)
             IF (K8BID.NE.K8BI1) THEN
               CALL UTMESS('F',NOMPRO,'LES CHARGES NE '
     &                   // 'S''APPUIENT PAS TOUTES SUR LE MEME MODELE')
             ENDIF
  23       CONTINUE
          ENDIF
        ENDIF
      ENDIF
C=======================================================================
C 2.6. ==> THETA, SYMETRIE DU CHARGEMENT, FOND DE FISSURE
C=======================================================================

      CALL GETVID ( ' ', 'THETA'    , 0,1,1, SDTHET,IBID)
      CALL GETVTX ( ' ', 'SYME_CHAR', 0,1,1, SYMECH,IBID)
      CALL GETVID ( ' ', 'FOND_FISS', 0,1,1, FOND,  IFOND)
      IF ( (OPTION .EQ. 'CALC_K_G') .AND. (IFOND.EQ.0) ) THEN
        CALL UTMESS('F', NOMPRO,'FOND OBLIGATOIRE AVEC OPTION CALC_K_G')
      ENDIF

C=======================================================================
C 2.7. ==> SI OPTION CALC_G_LAGR, RECUPERATION DE LA PROPAGATION ALPHA
C=======================================================================

      CALL GETVR8(' ','PROPAGATION',0,1,1,ALPHA ,IPROPA)
      IF (IPROPA.EQ.0) ALPHA=0.D0
      IF ((OPTION.NE.'CALC_G_LAGR').AND.(IPROPA.NE.0)) THEN
        CALL UTMESS('F', NOMPRO,'MOT CLE PROPAGATION UTILISE'
     &                 //'SEULEMENT AVEC L''OPTION CALC_G_LAGR')
      ENDIF

C=======================================================================
C 2.8. ==> RECUPERATION DU CHAMNO DE THETA DE LA S.D. SDTHET DE TYPE
C          THETA_GEOM
C=======================================================================

      CALL GETTCO(SDTHET,TYPCO)
      IF (TYPCO(1:10).EQ.'THETA_GEOM') THEN
        CALL RSEXCH(SDTHET,'THETA',0,THETA,IRET)
        IF (IRET.GT.0) THEN
          CALL UTMESS('F',NOMPRO,'LE CHAMP DE THETA EST INEXISTANT '//
     &                'DANS LA STRUCTURE DE DONNEES '//SDTHET//' DE '//
     &                'TYPE THETA_GEOM .')
        ENDIF
      ELSE
        THETA=SDTHET
      ENDIF

C=======================================================================
C 2.9. ==> SENSIBILITE
C=======================================================================
C
C 2.9.1 ==> NOMBRE DE PASSAGES
C           RQ : POUR UN CALCUL STANDARD DE G, CE NOMBRE VAUT 1
C              12   345678
      K8BID = '&&'//NOMPRO
      IAUX = 1
      CALL PSLECT ( ' ', IBID, K8BID, TABLE1, IAUX,
     >              NBPASE, INPSCO, IRET )
      IAUX = 1
      JAUX = 1
      CALL PSRESE(' ',IBID,IAUX,TABLE1,JAUX,NBPASS,NORECG,IRET)
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
        CALL PSRENC ( MATERI,NOPASE,MATERS,IRET )
        IF (IRET.EQ.0) THEN
          CALL PSTYPA ( NBPASE, INPSCO, MATERI, NOPASE, TYPEPS(-2) )
          CALL RCMFMC ( MATERS,MATES )
        END IF
C
  292 CONTINUE
C
C 2.9.3 ==> A-T-ON UNE DEPENDANCE VIS-A-VIS D'UNE CHARGE ? (CF. NMDOME)
C
      EXCHSE = .FALSE.
C
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
            CALL PSRENC ( NOMCHA,NOPASE,K8BID,IRET)
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
            END IF
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
     >                                K8BID,IRET)
                    IF (K8BID.EQ.'OUI') THEN
                      CALL PSTYPA ( NBPASE, INPSCO, NOMCHA, NOPASE,
     >                              TYPEPS(IAUX) )
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
C
C
C====
C 3. CALCUL EFFECTIF
C====
C============ DEBUT DE LA BOUCLE SUR LE NOMBRE DE PASSAGES =============
      DO 30 , NRPASS = 1 , NBPASS
C
C 3.1. ==> DECODAGE DES NOMS DES CONCEPTS
C        POUR LE PASSAGE NUMERO NRPASS :
C        . NOPASE : NOM DU PARAMETRE DE SENSIBILITE EVENTUELLEMENT
C        . LATAB1 : NOM DE LA TABLE A COMPLETER
C                   C'EST TABLE1 POUR UN CALCUL STANDARD, UN NOM
C                   COMPOSE A PARTIR DE TABLE1 ET NOPASE POUR UN CALCUL
C                   DE SENSIBILITE
C        . LERES0 : IDEM POUR RESUCO
C        . OPTIO1 : C'EST OPTION POUR UN CALCUL STANDARD, 'CALC_DG' POUR
C                   UN CALCUL DE SENSIBILITE

        NOPASE = ZK24(ADRECG+2*NRPASS-1) (1:8)
        LATAB1 = ZK24(ADRECG+2*NRPASS-2) (1:8)
C
        OPTIO1 = OPTION
C
C DANS LE CAS D'UN CALCUL STANDARD :

        IF (NOPASE.EQ.' ') THEN

          TYPESE = 0
          STYPSE = BLAN24

C DANS LE CAS D'UN CALCUL DE DERIVE :
C     TYPESE  : TYPE DE SENSIBILITE
C               -1 : DERIVATION EULERIENNE (VIA UN CHAMP THETA)
C                3 : DERIVATION PAR RAPPORT AU MODULE D'YOUNG
C                5 : DERIVATION PAR RAPPORT AU CHARGEMENT
C DANS CES 2 DERNIERS CAS, IL NE FAUT QU'UN SEUL PARAMETRE SENSIBLE
C A CHAQUE APPEL DE CALC_G_THETA
C
        ELSE

          CALL METYSE(NBPASE,INPSCO,NOPASE,TYPESE,STYPSE)
          IF ( TYPESE.EQ.-1 ) THEN
            OPTIO1 = 'CALC_DG'
          ELSE IF ( TYPESE.EQ.3 ) THEN
            OPTIO1 = 'CALC_DG_E'
            IF(OPTION.EQ.'CALC_K_G') OPTIO1 = 'CALC_DK_DG_E'
            IF(NBPASE.GE.2) THEN
              CALL UTMESS ('F', NOMPRO,
     >          'DERIVATION DE G : UN SEUL PARAMETRE SENSIBLE '//
     >          'PAR APPEL A CALC_G_THETA ')
            ENDIF
          ELSE IF ( TYPESE.EQ.5 ) THEN
            OPTIO1 = 'CALC_DG_FORC'
            IF(OPTION.EQ.'CALC_K_G') OPTIO1 = 'CALC_DK_DG_FORC'
            IF(NBPASE.GE.2) THEN
              CALL UTMESS ('F', NOMPRO,
     >          'DERIVATION DE G : UN SEUL PARAMETRE SENSIBLE '//
     >          'PAR APPEL A CALC_G_THETA ')
            ENDIF
          ELSE
            CALL UTMESS ('F', NOMPRO,
     >  'ON NE SAIT PAS TRAITER LE TYPE DE SENSIBILITE '//
     >  'ASSOCIE PARAMETRE SENSIBLE '//NOPASE)
          ENDIF
          IF (NRES.NE.0) THEN
            CALL PSRENC ( RESUCO, NOPASE, LERES0, IRET )
            IF ( IRET.NE.0 ) THEN
              CALL UTMESS ('F', NOMPRO,
     >  'IMPOSSIBLE DE TROUVER LE RESULTAT DERIVE ASSOCIE AU RESULTAT '
     >  //RESUCO//' ET AU PARAMETRE SENSIBLE '//NOPASE)
            ENDIF
          ENDIF

        END IF
C
        IF ( NIV.GE.2 ) THEN
          IF ( NOPASE.NE.'        ' ) THEN
            CALL UTMESS('I',NOMPRO,'SENSIBILITE AU PARAMETRE '//NOPASE)
          ENDIF
        ENDIF
C
C=======================================================================
C 3.2. ==> CREATION DE TABLE ET CALCUL
C=======================================================================

      IF ( OPTIO1.EQ.'CALC_G' .OR. OPTIO1.EQ.'CALC_G_LAGR' .OR.
     >     OPTIO1.EQ.'CALC_DG' ) THEN
        IF (NDEP .NE. 0 ) THEN
          NBPRUP = 1
          NOPRUP(1) = 'G'
          TYPRUP(1) = 'R'
        ELSE
          NBPRUP = 3
          NOPRUP(1) = 'NUME_ORDRE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'INST'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'G'
          TYPRUP(3) = 'R'
        ENDIF
      ELSEIF ( OPTIO1 .EQ. 'CALC_DG_E' ) THEN
        IF (NDEP .NE. 0 ) THEN
          NBPRUP = 1
          NOPRUP(1) = 'DG/DE'
          TYPRUP(1) = 'R'
        ELSE
          NBPRUP = 3
          NOPRUP(1) = 'NUME_ORDRE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'INST'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'DG/DE'
          TYPRUP(3) = 'R'
        ENDIF
      ELSEIF ( OPTIO1 .EQ. 'CALC_DG_FORC' ) THEN
        IF (NDEP .NE. 0 ) THEN
          NBPRUP = 1
          NOPRUP(1) = 'DG/DF'
          TYPRUP(1) = 'R'
        ELSE
          NBPRUP = 3
          NOPRUP(1) = 'NUME_ORDRE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'INST'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'DG/DF'
          TYPRUP(3) = 'R'
        ENDIF
      ELSEIF ( OPTION .EQ. 'K_G_MODA' ) THEN
          NBPRUP = 5
          NOPRUP(1) = 'NUME_MODE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'G'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'K1'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'K2'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'G_IRWIN'
          TYPRUP(5) = 'R'
      ELSEIF ( OPTIO1 .EQ. 'CALC_K_G' ) THEN
        IF ( NDEP .NE. 0 ) THEN
          NBPRUP = 4
          NOPRUP(1) = 'G'
          TYPRUP(1) = 'R'
          NOPRUP(2) = 'K1'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'K2'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'G_IRWIN'
          TYPRUP(4) = 'R'
        ELSE
          NBPRUP = 6
          NOPRUP(1) = 'NUME_ORDRE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'INST'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'G'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'K1'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'K2'
          TYPRUP(5) = 'R'
          NOPRUP(6) = 'G_IRWIN'
          TYPRUP(6) = 'R'
        ENDIF
      ELSEIF ( OPTIO1 .EQ. 'CALC_DK_DG_E' ) THEN
        IF ( NDEP .NE. 0 ) THEN
          NBPRUP = 3
          NOPRUP(1) = 'DG/DE'
          TYPRUP(1) = 'R'
          NOPRUP(2) = 'DK1/DE'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'DK2/DE'
          TYPRUP(3) = 'R'
        ELSE
          NBPRUP = 5
          NOPRUP(1) = 'NUME_ORDRE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'INST'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'DG/DE'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'DK1/DE'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'DK2/DE'
          TYPRUP(5) = 'R'
        ENDIF
      ELSEIF ( OPTIO1 .EQ. 'CALC_DK_DG_FORC' ) THEN   
        IF ( NDEP .NE. 0 ) THEN
          NBPRUP = 2
C          NOPRUP(1) = 'DG/DF'
C          TYPRUP(1) = 'R'
          NOPRUP(1) = 'DK1/DF'
          TYPRUP(1) = 'R'
          NOPRUP(2) = 'DK2/DF'
          TYPRUP(2) = 'R'
        ELSE
          NBPRUP = 4
          NOPRUP(1) = 'NUME_ORDRE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'INST'
          TYPRUP(2) = 'R'
C          NOPRUP(3) = 'DG/DF'
C          TYPRUP(3) = 'R'
          NOPRUP(3) = 'DK1/DF'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'DK2/DF'
          TYPRUP(4) = 'R'
        ENDIF
      ELSEIF ( OPTIO1 .EQ. 'G_BILINEAIRE'
     &            .OR.OPTIO1 .EQ. 'CALC_G_MAX') THEN
        NBPRUP = 3
        NOPRUP(1) = 'NUME_CMP_I'
        TYPRUP(1) = 'I'
        NOPRUP(2) = 'NUME_CMP_J'
        TYPRUP(2) = 'I'
        NOPRUP(3) = 'G_BILIN'
        TYPRUP(3) = 'R'
      ENDIF
      CALL TBCRSD ( LATAB1, 'G' )
      CALL TBAJPA ( LATAB1, NBPRUP, NOPRUP, TYPRUP )
C
C 3.3. ==> TRI SELON LES OPTIONS

C=======================================================================
C 3.3.1. ==> FORME BILINEAIRE ET/OU MAXIMISATION
C=======================================================================
C
      IF (OPTIO1 .EQ.'G_BILINEAIRE'.OR. OPTIO1 .EQ.'CALC_G_MAX') THEN

C 3.3.1.1. ==> CALCUL DE LA FORME BILINEAIRE DU TAUX DE RESTITUTION

        DO 3311 I = 1 , LONVEC
          DO 3312 J = 1,I
            CALL JEMARQ()
            CALL JERECU('V')
            IF (NRES.NE.0) THEN
              IORD1 = ZI(IVEC-1+I)
              CALL MEDOM1(MODELE,MATE,K8BID,VCHAR,NCHA,K4BID,
     &        RESUCO,IORD1)
              CALL JEVEUO(VCHAR,'L',ICHA)
              CALL RSEXCH(RESUCO,'DEPL',IORD1,DEPLA1,IRET)
              IORD2 = ZI(IVEC-1+J)
              CALL RSEXCH(RESUCO,'DEPL',IORD2,DEPLA2,IRET)
              IF(IRET.NE.0) THEN
                CALL UTMESS('F',NOMPRO,
     >                      'ACCES IMPOSSIBLE AU DEPLACEMENT')
              ENDIF
              CALL RSADPA(RESUCO,'L',1,'INST',IORD1,0,JINST,K8BID)
              TIMEU = ZR(JINST)
              CALL RSADPA(RESUCO,'L',1,'INST',IORD2,0,JINST,K8BID)
              TIMEV = ZR(JINST)
              EXITIM = .TRUE.
            ELSE
              DEPLA1 = DEPLA
              DEPLA2 = DEPLA
            ENDIF
            OPTIO2 = 'CALC_G_BILI'
            CALL MEBILG (OPTIO2,LATAB1,MODELE,DEPLA1,DEPLA2,THETA,MATE,
     &                   NCHA,ZK8(ICHA),SYMECH,EXITIM,TIMEU,TIMEV,I,J,
     &                   NBPRUP,NOPRUP )
            CALL JEDEMA()
 3312       CONTINUE
 3311     CONTINUE

C==============================================================
C 3.3.1.2. ==> MAXIMISATION DU G SOUS CONTRAINTES BORNES
C==============================================================

          IF (OPTIO1 .EQ.'CALC_G_MAX') THEN

            CALL GETFAC ('BORNES', NBORN )
            IF (NBORN.NE.0) THEN
              NBCO = 2*NBORN
              CALL WKVECT('&&'//NOMPRO//'.COUPLES_BORNES'
     &                     ,'V V R8',NBCO,IBOR)
              DO 3313 I=1, NBORN
                CALL GETVIS('BORNES','NUME_ORDRE',I,1,1,IORD,N1)
                CALL GETVR8('BORNES','VALE_MIN',I,1,1,
     &                      ZR(IBOR+2*(IORD-1)),N1)
                CALL GETVR8('BORNES','VALE_MAX',I,1,1,
     &                      ZR(IBOR+2*(IORD-1)+1),N1)
 3313         CONTINUE
              CALL TBEXVE(LATAB1,'G_BILIN',
     &                          '&&'//NOMPRO//'.GBILIN','V',NBVAL,K8BID)
              CALL JEVEUO('&&'//NOMPRO//'.GBILIN','L',IG)
              CALL DETRSD('TABLE',LATAB1)
              CALL MEMAXG(NBCO,ZR(IBOR),ZR(IG),LONVEC,LATAB1)
            ELSE
              CALL UTMESS('F',NOMPRO,'MOT-CLEF <BORNES> OBLIGATOIRE'//
     &                    ' AVEC CETTE OPTION !')
            ENDIF
          ENDIF
        
C==============================================================
C   FIN DU CALCUL DE LA FORME BILINEAIRE DU TAUX DE RESTITUTION
C==============================================================

      
C==============================================================
C 3.3.1.4. ==>OPTION K_G_MODA
C==============================================================
       
      ELSE IF (OPTIO1 .EQ. 'K_G_MODA') THEN
          DO 3314 I = 1 , LONVEC
            IORD1 = ZI(IVEC-1+I) 
            CALL RSEXCH(RESUCO,'DEPL',IORD1,DEPLA1,IRET)
            IF(IRET.NE.0) THEN
              CALL UTMESS('F',NOMPRO,
     &                    'ACCES IMPOSSIBLE AU MODE PROPRE')
            ENDIF
            CALL RSADPA(RESUCO,'L',1,'OMEGA2',IORD1,0,IPULS,K8BID) 
            PULS = ZR(IPULS)
            PULS = SQRT(PULS)
            
            CALL MEMOKG(OPTIO1,LATAB1,MODELE,DEPLA1,THETA,MATE,NCHA,
     &                  ZK8(ICHA),SYMECH,FOND,IORD1,PULS,NBPRUP,NOPRUP)
 3314     CONTINUE  

C==============================================================
C 3.4. ==> CALCUL DE G, G_LAGR, K_G ET DG
C==============================================================

      ELSE
        DO 34 , I = 1 , LONVEC
          CALL JEMARQ()
          CALL JERECU('V')
          IF(NRES.NE.0) THEN
            IORD = ZI(IVEC-1+I)
            CALL MEDOM1(MODELE,MATE,K8BID,VCHAR,NCHA,K4BID,
     &      RESUCO,IORD)
            CALL JEVEUO(VCHAR,'L',ICHA)
            CALL RSEXCH(RESUCO,'DEPL',IORD,DEPLA,IRET)
            IF(IRET.NE.0) THEN
              CALL UTMESS('F',NOMPRO,'ACCES IMPOSSIBLE AU DEPLACEMENT')
            ENDIF
            CALL RSEXCH(RESUCO,'VITE',IORD,CHVITE,IRET)
            IF(IRET.NE.0) THEN
              CHVITE = ' '
            ELSE
              CALL RSEXCH(RESUCO,'ACCE',IORD,CHACCE,IRET1)
            ENDIF
            CALL RSADPA(RESUCO,'L',1,'INST',IORD,0,JINST,K8BID)
            TIME  = ZR(JINST)
            EXITIM = .TRUE.

C RECUPERATION DES CHAMNO DE DERIVEE LAGRANGIENNE DE DEPLACEMENT
C DANS LA SD RESULTAT DERIVE DE TYPE EVOL_ELAS.
            IF (OPTIO1.EQ.'CALC_DG') THEN
              CALL RSEXC2(1,1,LERES0,'DEPL',IORD,CHDESE,OPTIO1,IRET)
              IF (IRET.GT.0) THEN
                CALL UTDEBM('F',NOMPRO,'LA DERIVEE LAGRANGIENNE')
                CALL UTIMPI('L','DU DEPLACEMENT D''OCCURRENCE N ',1,
     &            IORD)
                CALL UTIMPK('L','EST INEXISTANT DANS LA SD ',1,RESUCO)
                CALL UTIMPK('L','DERIVEE PAR RAPPORT A ',1,NOPASE)
                CALL UTFINM()
              ENDIF
            ENDIF

C
            IF (OPTIO1.EQ.'CALC_DG_E'
     &      .OR. OPTIO1.EQ.'CALC_DG_FORC'
     &      .OR. OPTIO1.EQ.'CALC_DK_DG_E'
     &      .OR. OPTIO1.EQ.'CALC_DK_DG_FORC') THEN
              CALL RSEXC2(1,1,LERES0,'DEPL',IORD,CHDESE,OPTIO1,IRET)
              IF (IRET.GT.0) THEN
                CALL UTDEBM('F',NOMPRO,'LA DERIVEE ')
                CALL UTIMPI('L','DU DEPLACEMENT D''OCCURRENCE N ',1,
     &            IORD)
                CALL UTIMPK('L','EST INEXISTANTE DANS LA SD ',1,RESUCO)
                CALL UTIMPK('L','DERIVEE PAR RAPPORT A ',1,NOPASE)
                CALL UTFINM()
              ENDIF
       CALL RSEXC2(1,1,LERES0,'EPSI_ELGA_DEPL',IORD,CHEPSE,OPTIO1,IRET)
              IF (IRET.GT.0) THEN
                CALL UTDEBM('F',NOMPRO,'LA DERIVEE ')
                CALL UTIMPI('L','DE LA DEFORMATION D''OCCURRENCE N ',1,
     &            IORD)
                CALL UTIMPK('L','EST INEXISTANTE DANS LA SD ',1,RESUCO)
                CALL UTIMPK('L','DERIVEE PAR RAPPORT A ',1,NOPASE)
                CALL UTFINM()
              ENDIF
       CALL RSEXC2(1,1,LERES0,'SIEF_ELGA_DEPL',IORD,CHSISE,OPTIO1,IRET)
              IF (IRET.GT.0) THEN
                CALL UTDEBM('F',NOMPRO,'LA DERIVEE ')
                CALL UTIMPI('L','DE LA CONTRAINTE D''OCCURRENCE N ',1,
     &            IORD)
                CALL UTIMPK('L','EST INEXISTANTE DANS LA SD ',1,RESUCO)
                CALL UTIMPK('L','DERIVEE PAR RAPPORT A ',1,NOPASE)
                CALL UTFINM()
              ENDIF
            ENDIF
          ENDIF
C
          IF (OPTIO1.EQ.'CALC_G' .OR. OPTIO1.EQ.'CALC_DG'
     &                   .OR. OPTIO1.EQ.'CALC_DG_E'
     &                   .OR. OPTIO1.EQ.'CALC_DG_FORC') THEN
            CALL MECALG (OPTIO1,LATAB1,MODELE,DEPLA,THETA,MATE,NCHA,
     &                   ZK8(ICHA),SYMECH,COMPOR,EXITIM,TIME,IORD,
     &                   NBPRUP,NOPRUP,NOPASE,TYPESE,CHDESE,
     &                   CHEPSE,CHSISE,CHVITE,CHACCE)

          ELSE IF (OPTIO1 .EQ.'CALC_G_LAGR') THEN
            CALL MLAGRG (OPTIO1,LATAB1,MODELE,DEPLA,THETA,ALPHA,MATE,
     &                   NCHA,ZK8(ICHA),SYMECH,EXITIM,TIME,IORD,
     &                   NBPRUP, NOPRUP )
          ELSE IF (OPTIO1 .EQ.'CALC_K_G') THEN
            CALL MEFICG (OPTIO1,LATAB1,MODELE,DEPLA,THETA,MATE,NCHA,
     &                   ZK8(ICHA),SYMECH,FOND,EXITIM,TIME,IORD,
     &                   NBPRUP, NOPRUP, NOPASE,TYPESE,CHDESE,
     &                   CHEPSE,CHSISE,CHVITE,CHACCE)
          ELSE IF (OPTIO1.EQ.'CALC_DK_DG_E') THEN 
            OPTIO2 = 'CALC_DG_E'
            CALL MECALG (OPTIO2,LATAB1,MODELE,DEPLA,THETA,MATE,NCHA,
     &                   ZK8(ICHA),SYMECH,COMPOR,EXITIM,TIME,IORD,
     &                   3,NOPRUP,NOPASE,TYPESE,CHDESE,
     &                   CHEPSE,CHSISE,CHVITE,CHACCE)
C
C  LES DERIVEES DE KI ET KII SONT NULLES. ON LES AJOUTE DIRECTEMENT    
            VAL(1) = 0.D0
            VAL(2) = 0.D0
            CALL TBAJLI(LATAB1,2,NOPRUP(4),IORD,VAL,CBID,K8BID,1)
          ELSE IF (OPTIO1.EQ.'CALC_DK_DG_FORC') THEN 
            CALL MEFICG (OPTIO1,LATAB1,MODELE,DEPLA,THETA,MATE,NCHA,
     &                   ZK8(ICHA),SYMECH,FOND,EXITIM,TIME,IORD,
     &                   NBPRUP, NOPRUP, NOPASE,TYPESE,CHDESE,
     &                   CHEPSE,CHSISE,CHVITE,CHACCE)
          ENDIF
          CALL JEDEMA()
   34   CONTINUE

      ENDIF
C
C 3.5. ==> TITRE ATTACHE AU CONCEPT
C          REMARQUE : ON NE PEUT PLUS UTILISER LE SPG TITRE CAR IL NE
C          CREE DE TITRE QUE POUR LE CONCEPT RESULTAT DE LA COMMANDE.
C          OR, POUR UN CAS DE SENSIBILITE, C'EST LE NOM DERIVE
C          QUI CONVIENT.
C          ON POURRAIT PASSER LE NOM DU PARAMETRE SENSIBLE EN ARGUMENT,
C          MAIS CELA SUPPOSE DE MODIFIER PRES DE 80 APPELS ...
C
        K24BID = BLAN24
        K24BID(1:8)   = LATAB1
        K24BID(20:24) = '.TITR'
        CALL TITREA('T',LATAB1,LATAB1,K24BID,'C',' ',0,'G' )

   30 CONTINUE
C============= FIN DE LA BOUCLE SUR LE NOMBRE DE PASSAGES ==============
C====
C 4. MENAGE
C====
C
      CALL JEDETC('G','&&NMDORC',1)
      CALL JEDEMA()
      END
