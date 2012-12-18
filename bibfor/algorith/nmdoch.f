      SUBROUTINE NMDOCH(LISCHA,IEXCIT,EXCIT )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      INTEGER      IEXCIT
      CHARACTER*19 LISCHA,EXCIT
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE
C
C SAISIE ET VERIFICATION DE LA COHERENCE DES CHARGEMENTS
C
C ----------------------------------------------------------------------
C
C
C IN  IEXCIT : INDICE DEFINISSANT L'ORIGINE DU CHARGEMENT
C                      UTILISE LORS DES CALCLULS
C                      0 : LE CHARGEMENT EST ISSU DE LA SD RESULTAT
C                      1 : LE CHARGEMENT EST FOURNI PAR L'UTILISATEUR
C I/O LISCHA : IN - NOM DONNE A SD L_CHARGES
C IN  EXCIT  : NOM EXTRAIT DE LA SD RESULTAT SI IEXCIT=0
C
C
C
C
      INTEGER      ITYCH
      INTEGER      N1,IBID,NOCC,IEXC,INCHA1,IRET2
      INTEGER      NPILO,NEXCI,NCHAR,NCHAR1,NCHAR2,NCHAR3
      INTEGER      INFMAX,INDIC,ICH,IRET,INFC,J,ICHAR
      INTEGER      JINFCH,JLCHA,JINFC,GETEXM
      INTEGER      JLCHA2,JINFC2
      INTEGER      JLISDB,ICHD
      CHARACTER*5  SUFFIX
      CHARACTER*8  K8BID,AFFCHA,PARCHA,TYPCHA
      CHARACTER*8  FCTCSR
      CHARACTER*16 NOMCMD,TYPESD
      CHARACTER*8  NOMCHA,NOMFCT,NOMCH1,NOMFC1
      CHARACTER*24 INFOC1,INFOCH
      CHARACTER*19 LISCH2,LISDBL
      CHARACTER*24 LIGRCH,LCHIN,K24BID
      INTEGER      IVAL,IVAL1
      LOGICAL      LFCPLX,LACCE
      INTEGER      NBINFO
C --- NOMBRE MAXIMUM DE TYPE_INFO
      INTEGER      NBINMX
      PARAMETER   (NBINMX=99)
      CHARACTER*24 LISINF(NBINMX)
C --- NOMBRE MAXIMUM DE RESUELEM POUR LES FORCES DE LAPLACE : NBCHMX
      INTEGER      NBCHMX
      PARAMETER   (NBCHMX=99)
C --- NOMBRE MAXIMUM DE TYPESD DE CHARGE                    : NBTYCH
      INTEGER      NBTYCH
      PARAMETER    (NBTYCH=18)
      CHARACTER*6  NOMLIG(NBTYCH)
      INTEGER      IARG
C
      DATA NOMLIG  /'.FORNO','.F3D3D','.F2D3D','.F1D3D',
     &              '.F2D2D','.F1D2D','.F1D1D','.PESAN',
     &              '.ROTAT','.PRESS','.FELEC','.FCO3D',
     &              '.FCO2D','.EPSIN','.FLUX' ,'.VEASS',
     &              '.ONDPL','.SIINT'/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(K8BID,TYPESD,NOMCMD)
C
C --- INITIALISATIONS
C
      NCHAR  = 0
      INFMAX = 0
      FCTCSR = '&&NMDOME'
      LISDBL = '&&NMDOME.LISDBL'
      LFCPLX = .FALSE.
      LACCE  = .FALSE.
      INFOCH = 'RIEN'
      NPILO  = 0
      INDIC  = 0
      NEXCI  = 0
      ICHAR  = 0
      K24BID = ' '
C
C --- NOMBRE DE CHARGES
C
      IF (IEXCIT.EQ.1) THEN
        IF (GETEXM('EXCIT','CHARGE').EQ.1) THEN
           CALL GETFAC('EXCIT',NEXCI )
        ELSE
           NEXCI=0
        ENDIF
        IF (NEXCI.GT.0) THEN
          DO 20 IEXC = 1,NEXCI
            CALL GETVID('EXCIT','CHARGE',IEXC,IARG,1,K24BID,NOCC  )
C
C --- GLUTE POUR LE CAS DEFI_CABLE_BP: MELANGE
C --- CINEMATIQUE/NEUMANN
C
            CALL JEEXIN(K24BID(1:8)//'.CHME.SIGIN.VALE',IRET)
            IF (IRET.NE.0) THEN
              CALL JEEXIN(K24BID(1:8)//'.CHME.CIMPO.DESC',IRET2)
            ELSE
              IRET2 = 1
            ENDIF
C
            IF ((NOCC.EQ.1).AND.(IRET2.NE.0)) THEN
              NCHAR  = NCHAR  + 1
            ENDIF
   20     CONTINUE
        ELSE
C --- CAS OU LE CHARGEMENT PEUT NE PAS ETRE OBLIGATOIRE (DYNA_NON_LINE)
C     ON CREE UNE SD CHARGE CONTENANT 1 CHARGE FICTIVE
          IF(NOMCMD.EQ.'DYNA_NON_LINE')THEN
             CALL LISCCR(LISCHA,1 ,'V')
             CALL JEVEUO(LISCHA(1:19)//'.INFC','E',JINFCH)
             NCHAR=0
             ZI(JINFCH) = NCHAR
          ELSEIF  (NOMCMD.EQ.'STAT_NON_LINE')THEN
             CALL U2MESS('F','CHARGES_2')
          ENDIF
        ENDIF
      ELSE
        CALL JEVEUO(EXCIT(1:19)//'.INFC','L',JINFC)
        NCHAR  = ZI(JINFC)

C
C --- POUR CALC_CHAMP : AFFE_CHAR_CINE EST ILLICITE: ON LES ENLEVE
C
        IF (NOMCMD.EQ.'CALC_CHAMP') THEN
          NCHAR1 = NCHAR
          NCHAR2 = 0
          CALL JEVEUO(EXCIT(1:19)//'.LCHA','L',JLCHA)
          DO 22 ICH = 1, NCHAR1
            NOMCHA = ZK24(JLCHA-1+ICH)(1:8)

            CALL JEEXIN(NOMCHA//'.CHME.SIGIN',IRET)
            IF (NOMCHA.NE.' ') THEN
              CALL DISMOI('F'   ,'TYPE_CHARGE',NOMCHA,'CHARGE',IBID  ,
     &                    AFFCHA,IRET)

              IF (AFFCHA(1:5).NE.'CIME_') THEN
                NCHAR2 = NCHAR2 + 1
              ENDIF
            ENDIF
   22     CONTINUE
C
          IF (NCHAR1.NE.NCHAR2) THEN
            NCHAR3 = MAX(NCHAR2,1)
            LISCH2 = '&&NMDOME.CHARGES'
            CALL LISCCR(LISCH2,NCHAR3,'V')
            INCHA1 = 0

            DO 24 ICH = 1,NCHAR1
              NBINFO = 1
              CALL LISCLI(EXCIT ,ICH   ,NOMCH1,NOMFC1,NBINFO,
     &                    INFOC1,IVAL1 )
              IF (INFOC1(1:5).NE.'CINE_'.AND.NOMCH1.NE.' ') THEN
                INCHA1 = INCHA1 + 1
                CALL LISCAD(LISCH2,INCHA1,NOMCH1,NOMFC1,NBINFO,
     &                      INFOC1,IVAL1 )
              ENDIF

   24       CONTINUE
            NCHAR  = NCHAR2
            EXCIT  = LISCH2
          ENDIF
        ENDIF

        CALL JEVEUO(EXCIT(1:19)//'.INFC','L',JINFC2)
        CALL JEVEUO(EXCIT(1:19)//'.LCHA','L',JLCHA2)
      ENDIF
C
      IF (NCHAR.NE.0) THEN
C
C ----- CREATION LA SD L_CHARGES
C
        CALL LISCCR(LISCHA,NCHAR ,'V')
C
C ----- LISTE DOUBLE
C
        CALL WKVECT(LISDBL,'V V K8',NCHAR,JLISDB)
C
C ----- BOUCLE SUR LES CHARGES
C
        DO 130 ICH = 1,NCHAR
          IF (IEXCIT.EQ.1) THEN
            INDIC  = INDIC + 1
   30       CONTINUE
            CALL GETVID('EXCIT','CHARGE',INDIC,IARG,0,NOMCHA,N1)
            IF(N1.NE.0)THEN
              CALL GETVID('EXCIT','CHARGE',INDIC,IARG,1,NOMCHA,N1)
              DO 131,ICHD = 1,NCHAR
                IF (NOMCHA.EQ.ZK8(JLISDB+ICHD-1)) THEN
                  CALL U2MESK('E','CHARGES_1',1,NOMCHA)
                ENDIF
 131          CONTINUE
            ELSE
              INDIC  = INDIC + 1
              GOTO 30
            END IF
          ELSE
            NOMCHA = ZK24(JLCHA2+ICH-1)(1:8)
          ENDIF
          ZK8(JLISDB+ICH-1)  = NOMCHA(1:8)
C
C ------- LIGREL DE LA CHARGE
C
          LIGRCH = NOMCHA(1:8)//'.CHME.LIGRE'
C
C ------- TYPE DE LA CHARGE
C
          IF (IEXCIT.EQ.1) THEN
            IF(NOMCMD.EQ.'DYNA_LINE_TRAN' .OR.
     &         NOMCMD.EQ.'DYNA_LINE_HARM') THEN
              TYPCHA='FIXE'
            ELSE
              CALL GETVTX('EXCIT','TYPE_CHARGE',INDIC ,IARG,1,TYPCHA,N1)
            ENDIF
          ELSE
            TYPCHA = 'FIXE_CST'
            IF (NOMCMD.EQ.'CALC_CHAMP')THEN
              IF (ZI(JINFC2+ICH).EQ.4 .OR.
     &            ZI(JINFC2+NCHAR+ICH).EQ.4) THEN
                TYPCHA = 'SUIV'
              ELSEIF (ZI(JINFC2+ICH).EQ.5  .OR.
     &                ZI(JINFC2+NCHAR+ICH).EQ.5) THEN
                TYPCHA = 'FIXE_PIL'
              ELSEIF (ZI(JINFC2+3*NCHAR+2+ICH).EQ.1) THEN
                TYPCHA = 'DIDI'
              ENDIF
            ENDIF
          ENDIF
C
C ------- NOMBRE DE CHARGES PILOTEES
C
          IF (TYPCHA.EQ.'FIXE_PIL') THEN
            NPILO  = NPILO + 1
          ENDIF
C
C ------- CONTROLE DU CARACTERE MECANIQUE DE LA CHARGE
C
          CALL DISMOI('F'   ,'TYPE_CHARGE',NOMCHA,'CHARGE',
     &                IBID  ,AFFCHA       ,IRET)
          IF ((AFFCHA(1:5).NE.'MECA_') .AND.
     &        (AFFCHA(1:5).NE.'CIME_')) THEN
            CALL U2MESK('F','CHARGES_22',1,NOMCHA(1:8))
          END IF
C
C ------- FONCTIONS MULTIPLICATIVES DES CHARGES
C
          LFCPLX = (NOMCMD.EQ.'DYNA_LINE_HARM' .OR.
     &    ( NOMCMD.EQ.'LIRE_RESU' .AND. TYPESD.EQ.'DYNA_HARMO' ) )
          LACCE  = (NOMCMD.EQ.'DYNA_NON_LINE'.OR.
     &      NOMCMD.EQ.'LIRE_RESU')
          CALL LISLFC(EXCIT ,ICH   ,INDIC ,IEXCIT,NEXCI ,
     &                LFCPLX,LACCE ,FCTCSR,NOMFCT)
          IF (NOMFCT.NE.FCTCSR) THEN
            IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL U2MESK('F','CHARGES_38',1,NOMCHA(1:8))
            ENDIF
          ENDIF
C
C ------- CHARGE DE TYPE DIRICHLET PROVENANT D'UN AFFE_CHAR_CINE
C
          NBINFO = 0
          INFOCH = 'RIEN'
          IF (AFFCHA(1:5).EQ.'CIME_') THEN
            IF (TYPCHA(1:4).EQ.'SUIV') THEN
              CALL U2MESK('F','CHARGES_23',1,NOMCHA(1:8))
            ELSE IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL U2MESK('F','CHARGES_27',1,NOMCHA(1:8))
            ELSE IF (TYPCHA(1:4).EQ.'DIDI') THEN
              CALL U2MESK('F','CHARGES_24',1,NOMCHA(1:8))
            ELSE
              IF (AFFCHA(5:7).EQ.'_FT') THEN
                INFOCH = 'CINE_FT'
              ELSE IF (AFFCHA(5:7).EQ.'_FO') THEN
                INFOCH = 'CINE_FO'
              ELSE
                INFOCH = 'CINE_CSTE'
              END IF
            END IF
          END IF
          IF (INFOCH.NE.'RIEN') THEN
            NBINFO = NBINFO + 1
            CALL ASSERT(NBINFO.LT.NBINMX)
            LISINF(NBINFO) = INFOCH
          ENDIF
C
C -------- CHARGE DE TYPE DIRICHLET PROVENANT DE AFFE_CHAR_MECA
C
          INFOCH = 'RIEN'
          LCHIN  = LIGRCH(1:13)//'.CIMPO.DESC'
          CALL JEEXIN(LCHIN,IRET)
          IF (IRET.NE.0) THEN
            IF (TYPCHA(1:4).EQ.'SUIV') THEN
              CALL U2MESK('F','CHARGES_23',1,NOMCHA(1:8))

            ELSE IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL DISMOI('F'   ,'PARA_INST',LCHIN(1:19),'CARTE',IBID  ,
     &                    PARCHA,IRET  )
              IF (PARCHA(1:3).EQ.'OUI') THEN
                CALL U2MESK('F','CHARGES_28',1,NOMCHA(1:8))
              ENDIF

              IF (AFFCHA(5:7).EQ.'_FT') THEN
                CALL U2MESK('F','CHARGES_28',1,NOMCHA(1:8))
              ELSEIF (AFFCHA(5:7).EQ.'_FO') THEN
                INFOCH = 'DIRI_PILO_F'
              ELSE
                INFOCH = 'DIRI_PILO'
              ENDIF
            ELSE
              IF (AFFCHA(5:7).EQ.'_FO') THEN
                INFOCH = 'DIRI_FO'
                CALL DISMOI('F'   ,'PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                      PARCHA,IRET)
                IF (PARCHA(1:3).EQ.'OUI') THEN
                  INFOCH = 'DIRI_FT'
                END IF
              ELSE
                INFOCH = 'DIRI_CSTE'
              END IF
              IF (TYPCHA(1:4).EQ.'DIDI') THEN
                INFOCH = INFOCH(1:9)//'_DIDI'
              END IF
            END IF
          END IF
          IF (INFOCH.NE.'RIEN') THEN
            NBINFO = NBINFO + 1
            CALL ASSERT(NBINFO.LT.NBINMX)
            LISINF(NBINFO) = INFOCH
          ENDIF
C
C ------- CHARGE DE TYPE NEUMANN
C
          DO 70 ITYCH = 1,NBTYCH
            IF (NOMLIG(ITYCH).EQ.'.VEASS') THEN
              SUFFIX = '     '
            ELSE
              SUFFIX = '.DESC'
            END IF
            LCHIN = LIGRCH(1:13)//NOMLIG(ITYCH)//SUFFIX
            CALL JEEXIN(LCHIN,IRET)
            INFOCH = 'RIEN'
            IF (IRET.NE.0) THEN
              IF (NOMLIG(ITYCH).EQ.'.ONDPL') THEN
                INFOCH = 'NEUM_ONDE'

              ELSEIF (NOMLIG(ITYCH).EQ.'.SIINT') THEN
                INFOCH = 'NEUM_SIGM_INT'

              ELSE IF (TYPCHA.EQ.'FIXE_PIL') THEN
                INFOCH = 'NEUM_PILO'
                IF (NOMLIG(ITYCH).NE.'.VEASS') THEN
                  CALL DISMOI('F','PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                      PARCHA,IRET)
                  IF (PARCHA(1:3).EQ.'OUI') THEN
                    CALL U2MESS('F','CHARGES_28')
                  ENDIF
                ENDIF

              ELSE IF (TYPCHA(1:4).EQ.'SUIV') THEN
                INFOCH = 'NEUM_SUIV'

              ELSE IF (AFFCHA(5:7).EQ.'_FO') THEN
                CALL DISMOI('F','PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                      PARCHA,IRET)

                IF (PARCHA(1:3).EQ.'OUI') THEN
                  INFOCH = 'NEUM_FT'
                ELSE
                  INFOCH = 'NEUM_FO'
                END IF
              ELSE
                INFOCH = 'NEUM_CSTE'
              END IF
            END IF
            IF (INFOCH.NE.'RIEN') THEN
              NBINFO = NBINFO + 1
              CALL ASSERT(NBINFO.LT.NBINMX)
              LISINF(NBINFO) = INFOCH
            ENDIF
   70     CONTINUE
C
C ------- CHARGE DE TYPE EVOL_CHAR
C
          INFOCH = 'RIEN'
          LCHIN = LIGRCH(1:13)//'.EVOL.CHAR'
          CALL JEEXIN(LCHIN,IRET)
          IF (IRET.NE.0) THEN
            IF (TYPCHA(1:4).EQ.'SUIV') THEN
              INFOCH = 'NEUM_SUIV'
            ELSE
              INFOCH = 'NEUM_CSTE'
            END IF
            IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL U2MESK('F','CHARGES_34',1,NOMCHA(1:8))
            END IF
          ENDIF
          IF (INFOCH.NE.'RIEN') THEN
            NBINFO = NBINFO + 1
            CALL ASSERT(NBINFO.LT.NBINMX)
            LISINF(NBINFO) = INFOCH
          ENDIF
C
C ------- CHARGE DE TYPE EXCIT_SOL
C
          INFOCH = 'RIEN'
          LCHIN = LIGRCH(1:13)//'.VEISS'
          CALL JEEXIN(LCHIN,IRET)
          IF (IRET.NE.0) THEN
            IF (NOMCMD.EQ.'STAT_NON_LINE') THEN
              CALL U2MESK('F','CHARGES_50',1,NOMCHA(1:8))
            ENDIF
            IF (TYPCHA.EQ.'SUIV') THEN
              CALL U2MESK('F','CHARGES_51',1,NOMCHA(1:8))
            END IF
            IF (TYPCHA.EQ.'DIDI') THEN
              CALL U2MESK('F','CHARGES_52',1,NOMCHA(1:8))
            END IF
            IF (AFFCHA(5:6).EQ.'_F') THEN
              CALL U2MESK('F','CHARGES_53',1,NOMCHA(1:8))
            ENDIF
            IF (NOMFCT.NE.FCTCSR) THEN
              CALL U2MESK('F','CHARGES_54',1,NOMCHA(1:8))
            ENDIF
            INFOCH = 'EXCIT_SOL'
          ENDIF
          IF (INFOCH.NE.'RIEN') THEN
            NBINFO = NBINFO + 1
            CALL ASSERT(NBINFO.LT.NBINMX)
            LISINF(NBINFO) = INFOCH
          ENDIF
C
C -------- CHARGES DE TYPE FORCE DE LAPLACE
C
          INFC   = 0
          INFOCH = 'RIEN'
          DO 80 J = 1,NBCHMX
            LCHIN(1:17) = LIGRCH(1:13)//'.FL1'
            CALL CODENT(J,'D0',LCHIN(18:19))
            LCHIN = LCHIN(1:19)//'.DESC'
            CALL JEEXIN(LCHIN,IRET)
            IF (IRET.NE.0) THEN
              INFC = INFC + 1
            ELSE
              GO TO 90
            END IF
   80     CONTINUE
   90     CONTINUE
          IF (INFC.NE.0) THEN
            IVAL   = MAX(INFMAX,INFC)
            INFOCH = 'NEUM_LAPL'
          ENDIF
          IF (INFOCH.NE.'RIEN') THEN
            NBINFO = NBINFO + 1
            CALL ASSERT(NBINFO.LT.NBINMX)
            LISINF(NBINFO) = INFOCH
          ENDIF
C
C --- AJOUT DE LA CHARGE
C
          IF (NBINFO.GT.0) THEN
             ICHAR  = ICHAR+1
             CALL LISCAD(LISCHA,ICHAR ,NOMCHA,NOMFCT,NBINFO,
     &                   LISINF,IVAL  )
          ENDIF

  130   CONTINUE
C
C ---- PILOTAGE POSSIBLE SI IL YA DES CHARGES PILOTEES !
C
        IF( NOMCMD.NE.'LIRE_RESU') THEN
          IF (NOMCMD.EQ.'STAT_NON_LINE') THEN
            CALL GETVTX('PILOTAGE','TYPE',1,IARG,1,K24BID,N1)
            IF (N1.NE.0 .AND. NPILO.EQ.0) THEN
              CALL U2MESS('F','CHARGES_39')
            END IF
            IF (NPILO.GT.1) THEN
              CALL U2MESS('F','CHARGES_40')
            END IF
          END IF
        ENDIF
      ENDIF
      CALL JEDETR(LISDBL)
      CALL JEDEMA()
      END
