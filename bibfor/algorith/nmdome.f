      SUBROUTINE NMDOME(MODELE,MATE,CARELE,LISCHA,NBPASE,INPSCO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/03/2004   AUTEUR G8BHHXD X.DESROCHES 
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
C RESPONSABLE PBADEL P.BADEL
C TOLE CRP_20

      IMPLICIT NONE

C 0.1. ==> ARGUMENTS

      INTEGER NBPASE

      CHARACTER*19 LISCHA
      CHARACTER*24 MODELE,MATE,CARELE
      CHARACTER*(*) INPSCO

C ----------------------------------------------------------------------
C     SAISIE ET VERIFICATION DE LA COHERENCE DES DONNEES MECANIQUES
C ----------------------------------------------------------------------
C VAR      MODELE  : NOM DU MODELE
C OUT      MATE    : NOM DU CHAMP DE MATERIAU CODE
C OUT      CARELE  : CARACTERISTIQUES DES POUTRES ET COQUES
C IN/JXOUT LISCHA  : SD L_CHARGES
C IN  NBPASE  : NOMBRE DE PARAMETRES SENSIBLES
C IN  INPSCO  : STRUCTURE CONTENANT LA LISTE DES NOMS


C 0.2. ==> COMMUNS

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

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

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

C 0.3. ==> VARIABLES LOCALES

      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='NMDOME')

      INTEGER IE,I0,I1,I
      INTEGER N1,NCHAR,IALICH,IBID,IERD,IRET1,IRET2
      INTEGER NCONTA,NPILO,NEXCI,IEXC,L
      INTEGER JINF,IALIFC,INFMAX,INDIC,ICH,IRET,K,INFC,J,N2,JPRO,JVAL
      INTEGER NRPASE,ADCHSE,IAUX,JAUX,JOPT

      INTEGER N3,NBOPT   
      CHARACTER*4  KNUM
      COMPLEX*16   CCOEF
      REAL*8 COEF
      REAL*8 R

      CHARACTER*5 SUFFIX
      CHARACTER*8 CARA,MODE,K8BID,AFFCHA,PARCHA,TYPCHA,MATERI
      CHARACTER*8 MATERS
      CHARACTER*8 NOPASE
      CHARACTER*16 K16BID,NOMCMD,OPTION
      CHARACTER*19 LIGR
      CHARACTER*24 LIGRCH,LCHIN,NOMFCT,K24BID,CONTA
      CHARACTER*24 NOMCHA,CHARSE,MATES
      LOGICAL NONMEC,EXITHE,EXIANE,EXIHYD,EXISEC,EXIARL
      LOGICAL EXCHSE

C --- NOMBRE MAXIMUM DE RESUELEM POUR LES FORCES DE LAPLACE : NBCHMX
      INTEGER NBCHMX
      PARAMETER (NBCHMX=99)
C --- NOMBRE MAXIMUM DE TYPE DE CHARGE                       : NBTYCH

      INTEGER NBTYCH
      PARAMETER (NBTYCH=17)

      CHARACTER*6 NOMLIG(NBTYCH)
      CHARACTER*8 TYPEPS(-2:NBTYCH)

      DATA NOMLIG/'.FORNO','.F3D3D','.F2D3D','.F1D3D','.F2D2D','.F1D2D',
     &     '.F1D1D','.PESAN','.ROTAT','.PRESS','.FELEC','.FCO3D',
     &     '.FCO2D','.EPSIN','.FLUX','.VEASS','.ONDPL'/
      DATA TYPEPS/'MATERIAU','CARAELEM','DIRICHLE','FORCE   ',
     &     'FORCE   ','FORCE   ','FORCE   ','FORCE   ','FORCE   ',
     &     'FORCE   ','.PESAN','.ROTAT','FORCE   ','.FELEC','FORCE   ',
     &     'FORCE   ','.EPSIN','.FLUX','.VEASS','.ONDPL'/
C ----------------------------------------------------------------------
C====
C 1. PREALABLES
C====

      CALL JEMARQ()
      CALL GETRES(K8BID,K16BID,NOMCMD)

C====
C 2. LECTURES
C====

C 2.1. ==> LE MODELE

      IF (MODELE.EQ.' ') THEN
        CALL GETVID(' ','MODELE',0,1,1,MODE,N1)
        MODELE = MODE
      END IF

C 2.2. ==> LE MATERIAU

      MATERI = ' '
      CALL GETVID(' ','CHAM_MATER',0,1,1,MATERI,N1)
      IF ((NOMCMD.NE.'CALC_NO') .AND. (NOMCMD.NE.'POST_ELEM')) THEN
        CALL DISMOI('F','BESOIN_MATER',MODELE,'MODELE',IBID,K8BID,IE)
        IF ((N1.EQ.0) .AND. (K8BID(1:3).EQ.'OUI')) CALL UTMESS('A',
     &      NOMPRO,'LE MODELE A PROBABLEMENT '//
     &      'BESOIN D UN CHAMP DE MATERIAUX (MOT-CLE CHAM_MATER).')
      END IF
      IF (N1.NE.0) THEN
        CALL RCMFMC(MATERI,MATE)
      ELSE
        MATE = ' '
      END IF

C 2.3. ==> LES CARACTERISTIQUES ELEMENTAIRES

      CARA = ' '
      IF (NOMCMD(1:8).NE.'POST_ZAC') THEN
        CALL GETVID(' ','CARA_ELEM',0,1,1,CARA,N1)
        IF ((NOMCMD.NE.'CALC_NO') .AND. (NOMCMD.NE.'POST_ELEM')) THEN
          CALL DISMOI('F','EXI_RDM',MODELE,'MODELE',IBID,K8BID,IE)
          IF ((N1.EQ.0) .AND. (K8BID(1:3).EQ.'OUI')) CALL UTMESS('A',
     &        NOMPRO,'LE MODELE CONTIENT DES ELEMENTS'//
     &        ' DE STRUCTURE. IL FAUT PROBABLEMENT'//
     &        ' UTILISER LE MOT-CLE CARA_ELEM.')
        ELSE IF (NOMCMD.EQ.'CALC_NO') THEN
          CALL GETVTX(' ','OPTION',1,1,0,OPTION,N2)
          NBOPT = -N2
          CALL WKVECT('&&'//NOMPRO//'.OPTION','V V K16',NBOPT,JOPT)
          CALL GETVTX(' ','OPTION',1,1,NBOPT,ZK16(JOPT),N2)
          DO 1 I=1,NBOPT
            IF(ZK16(JOPT-1+I)(5:9).EQ.'_NODA') THEN
             LIGR=MODELE(1:8)//'.MODELE' 
             CALL DISMOI('F','EXI_RDM',LIGR,'LIGREL',IBID,K8BID,IE)
             IF ((N1.EQ.0).AND.(K8BID(1:3).EQ.'OUI')) CALL UTMESS('A',
     &         NOMPRO,'LE MODELE CONTIENT DES ELEMENTS DE STRUCTURE'//
     &         ' IL FAUT RENSEIGNER LE MOT-CLE CARA_ELEM POUR LES '//
     &         'OPTIONS FORC_NODA ET REAC_NODA.')
            ENDIF
1         CONTINUE
        END IF
      END IF
      CARELE = CARA

C 2.4. ==> QUELS MATERIAUX ET CARACTERISTIQUES SONT TOUCHES PAR LE
C          CALCUL DE SENSIBILITE ?

      DO 10 , NRPASE = 1,NBPASE
C
C 2.4.1. ==> NOM DU PARAMETRE SENSIBLE NUMERO NRPASE
C
        IAUX = NRPASE
        JAUX = 1
        CALL PSNSLE(INPSCO,IAUX,JAUX,NOPASE)
C
C 2.4.2. ==> A-T-ON UN CHAMP DE MATERIAU DERIVE ASSOCIE A CE PARAMETRE ?
C            SI OUI :
C             . ON MEMORISE QUE LE PARAMETRE SENSIBLE IMPACTE
C               LES MATERIAUX
C             . ON CREE LE MATERIAU CODE ASSOCIE AU MATERIAU DERIVE
C
        CALL PSRENC ( MATERI,NOPASE,MATERS,IRET)
        IF (IRET.EQ.0) THEN
          CALL PSTYPA ( NBPASE, INPSCO, MATERI, NOPASE, TYPEPS(-2) )
          CALL RCMFMC(MATERS,MATES)
        END IF
C
C 2.4.3. ==> A-T-ON DES CARACTERISTIQUES DERIVEES ASSOCIEES A
C            CE PARAMETRE ?
C            SI OUI :
C             . ON MEMORISE QUE LE PARAMETRE SENSIBLE IMPACTE
C               LES CARACTERISTIQUES

        CALL PSRENC ( CARA,NOPASE,K8BID,IRET)
        IF (IRET.EQ.0) THEN
          CALL PSTYPA ( NBPASE, INPSCO, CARA, NOPASE, TYPEPS(-1) )
        END IF

   10 CONTINUE

C====
C 3. LES CHARGES
C====

C 3.1. ==> DECOMPTE DU NOMBRE DE CHARGES

      CALL GETFAC('EXCIT',NEXCI)
      NCHAR = 0
      IF (NEXCI.GT.0) THEN
        DO 20 IEXC = 1,NEXCI
          CALL GETVID('EXCIT','CHARGE',IEXC,1,1,K24BID,L)
          IF (L.EQ.1) NCHAR = NCHAR + 1
   20   CONTINUE
      END IF

      IF (NCHAR.NE.0) THEN

C    DIMENSIONNEMENT DE LA SD L_CHARGES
        CALL WKVECT(LISCHA//'.LCHA','V V K24',NCHAR,IALICH)
        CALL WKVECT(LISCHA//'.INFC','V V IS',4*NCHAR+7,JINF)
        CALL WKVECT(LISCHA//'.FCHA','V V K24',NCHAR,IALIFC)
        ZI(JINF) = NCHAR
        INFMAX = 0
        CHARSE = '&&'//NOMPRO//'.CHARSE'
        IAUX = MAX(NBPASE,1)
        CALL WKVECT(CHARSE,'V V K8',IAUX,ADCHSE)


C    PARCOURS DE L'ENSEMBLE DES CHARGES
        EXITHE = .FALSE.
        EXIHYD = .FALSE.
        EXISEC = .FALSE.
        EXIANE = .FALSE.
        EXIARL = .FALSE.
        NONMEC = .FALSE.
        NPILO = 0
        NCONTA = 0
        INDIC = 0

C 3.2. ==> DETAIL DE CHAQUE CHARGE

        DO 130,ICH = 1,NCHAR

          INDIC = INDIC + 1
   30     CONTINUE
          CALL GETVID('EXCIT','CHARGE',INDIC,1,1,NOMCHA,N1)
          IF (N1.EQ.0) THEN
            INDIC = INDIC + 1
            GO TO 30
          END IF

          ZK24(IALICH+ICH-1) = NOMCHA

C 3.2.1. ==> LA CHARGE EST-ELLE CONCERNEE PAR UN CALCUL DE SENSIBILITE ?

          EXCHSE = .FALSE.
          DO 40,NRPASE = 1,NBPASE
            IAUX = NRPASE
            JAUX = 1
            CALL PSNSLE(INPSCO,IAUX,JAUX,NOPASE)
            CALL PSRENC ( NOMCHA,NOPASE,K8BID,IRET)
            IF (IRET.EQ.0) THEN
              ZK8(ADCHSE+NRPASE-1) = NOPASE
              EXCHSE = .TRUE.
            ELSE
              ZK8(ADCHSE+NRPASE-1) = '        '
            END IF
   40     CONTINUE

C 3.2.2. ==> TYPES DE CHARGES UTILISEES

          CALL GETVTX('EXCIT','TYPE_CHARGE',INDIC,1,1,TYPCHA,N1)
          LIGRCH = ZK24(IALICH-1+ICH) (1:8)//'.CHME.LIGRE'


C      CONTROLE DU CARACTERE MECANIQUE DE LA CHARGE
          CALL DISMOI('F','TYPE_CHARGE',ZK24(IALICH-1+ICH),'CHARGE',
     &                IBID,AFFCHA,IERD)
          IF ((AFFCHA(1:5).NE.'MECA_') .AND.
     &        (AFFCHA(1:5).NE.'CIME_')) THEN
            NONMEC = .TRUE.
            CALL UTMESS('E','NMDOME_01','LA CHARGE '//
     &                  ZK24(IALICH-1+ICH) (1:8)//
     &                  ' N''EST PAS MECANIQUE')
          END IF


C      INCREMENTATION DU COMPTEUR DE CHARGES PILOTEES LE CAS ECHEANT
          IF (TYPCHA.EQ.'FIXE_PIL') NPILO = NPILO + 1


C 3.2.3. ==> CHARGE DU TYPE DIRICHLET PROVENANT D'UN AFFE_CHAR_CINE

          IF (AFFCHA(1:5).EQ.'CIME_') THEN
            IF (TYPCHA(1:4).EQ.'SUIV') THEN
              CALL UTMESS('F','NMDOME_02',
     &                    'LA CHARGE '//ZK24(IALICH-1+ICH) (1:8)//
     &                    ' NE PEUT ETRE SUIVEUSE')
            ELSE IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL UTMESS('F','NMDOME_03',
     &                    'LA CHARGE '//ZK24(IALICH-1+ICH) (1:8)//
     &                    ' NE PEUT ETRE PILOTEE')
            ELSE IF (TYPCHA(1:4).EQ.'DIDI') THEN
              CALL UTMESS('F','NMDOME_04',
     &                    'LA CHARGE '//ZK24(IALICH-1+ICH) (1:8)//
     &                    ' NE PEUT ETRE DIFFERENTIELLE')
            ELSE
              IF (AFFCHA(5:7).EQ.'_FT') THEN
                ZI(JINF+ICH) = -3
              ELSE IF (AFFCHA(5:7).EQ.'_FO') THEN
                ZI(JINF+ICH) = -2
              ELSE
                ZI(JINF+ICH) = -1
              END IF
            END IF
          END IF


C 3.2.4. ==> CHARGES DU TYPE DIRICHLET PROVENANT DE AFFE_CHAR_MECA
C                                          OU AFFE_CHAR_MECA_F

          LCHIN = LIGRCH(1:13)//'.CIMPO.DESC'
          CALL JEEXIN(LCHIN,IRET)
          IF (IRET.NE.0) THEN
            IF (TYPCHA(1:4).EQ.'SUIV') THEN
              CALL UTMESS('F','NMDOME_05',
     &                    'LA CHARGE '//ZK24(IALICH-1+ICH) (1:8)//
     &                    ' NE PEUT ETRE SUIVEUSE')
            ELSE IF (TYPCHA.EQ.'FIXE_PIL') THEN
              ZI(JINF+ICH) = 5
              CALL DISMOI('F','PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                  PARCHA,IERD)
              IF (PARCHA(1:3).EQ.'OUI') CALL UTMESS('F',NOMPRO,
     &          'ON NE PEUT PILOTER UNE CHARGE FONCTION DU TEMPS')
            ELSE
              IF (AFFCHA(5:7).EQ.'_FO') THEN
                ZI(JINF+ICH) = 2
                CALL DISMOI('F','PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                      PARCHA,IERD)
                IF (PARCHA(1:3).EQ.'OUI') THEN
                  ZI(JINF+ICH) = 3
                END IF
                IF (EXCHSE) THEN
                  DO 50,NRPASE = 1,NBPASE
                    NOPASE = ZK8(ADCHSE+NRPASE-1)
                    IF (NOPASE.NE.'        ') THEN
                      CALL TELLME('F','NOM_FONCTION',LCHIN(1:19),NOPASE,
     &                            K8BID,IERD)
                      IF (K8BID.EQ.'OUI') THEN
                        CALL PSTYPA ( NBPASE, INPSCO, NOMCHA, NOPASE,
     >                                TYPEPS(0) )
                      END IF
                    END IF
   50             CONTINUE
                END IF
              ELSE
                ZI(JINF+ICH) = 1
              END IF
              IF (TYPCHA(1:4).EQ.'DIDI') THEN
                ZI(JINF+3*NCHAR+2+ICH) = 1
              END IF
            END IF
          END IF

C 3.2.5. ==> CHARGES DU TYPE NEUMANN (FORCE)

          DO 70 K = 1,NBTYCH
            IF (NOMLIG(K).EQ.'.VEASS') THEN
              SUFFIX = '     '
            ELSE
              SUFFIX = '.DESC'
            END IF
            LCHIN = LIGRCH(1:13)//NOMLIG(K)//SUFFIX
            CALL JEEXIN(LCHIN,IRET)
            IF (IRET.NE.0) THEN
              IF (NOMLIG(K).EQ.'.ONDPL') THEN
                ZI(JINF+NCHAR+ICH) = 6
              ELSE IF (TYPCHA.EQ.'FIXE_PIL') THEN
                ZI(JINF+NCHAR+ICH) = 5
                IF (NOMLIG(K).NE.'.VEASS') THEN
                  CALL DISMOI('F','PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                      PARCHA,IERD)
                  IF (PARCHA(1:3).EQ.'OUI') CALL UTMESS('F',NOMPRO,
     &              'ON NE PEUT PILOTER UNE CHARGE FONCTION DU TEMPS')
                ENDIF
              ELSE IF (TYPCHA(1:4).EQ.'SUIV') THEN
                ZI(JINF+NCHAR+ICH) = 4
              ELSE IF (AFFCHA(5:7).EQ.'_FO') THEN
                CALL DISMOI('F','PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                      PARCHA,IERD)
                IF (PARCHA(1:3).EQ.'OUI') THEN
                  ZI(JINF+NCHAR+ICH) = 3
                ELSE
                  ZI(JINF+NCHAR+ICH) = 2
                END IF
                IF (EXCHSE) THEN
                  DO 60,NRPASE = 1,NBPASE
                    NOPASE = ZK8(ADCHSE+NRPASE-1)
                    IF (NOPASE.NE.'        ') THEN
                      CALL TELLME('F','NOM_FONCTION',LCHIN(1:19),NOPASE,
     &                            K8BID,IERD)
                      IF (K8BID.EQ.'OUI') THEN
                        CALL PSTYPA ( NBPASE, INPSCO, NOMCHA, NOPASE,
     >                                TYPEPS(K) )
                      END IF
                    END IF
   60             CONTINUE
                END IF
              ELSE
                ZI(JINF+NCHAR+ICH) = 1
              END IF
            END IF
   70     CONTINUE


C ---- CHARGE DU TYPE EVOL_CHAR
          LCHIN = ZK24(IALICH-1+ICH) (1:8)//'.CHME.EVOL.CHAR'
          CALL JEEXIN(LCHIN,IRET)
          IF (IRET.NE.0) THEN
            IF (TYPCHA(1:4).EQ.'SUIV') THEN
              ZI(JINF+NCHAR+ICH) = 4
            ELSE
              ZI(JINF+NCHAR+ICH) = 1
            END IF
          END IF

C ---- CHARGE DU TYPE THERMIQUE
          LCHIN = LIGRCH(1:13)//'.TEMPE.TEMP'
          CALL JEEXIN(LCHIN,IRET)
          IF (IRET.NE.0) THEN
            IF (EXITHE) THEN
              CALL UTMESS('F','NMDOME_06',
     &                    'IL Y A PLUSIEURS CHARGES'//' THERMIQUES ')
            END IF
            EXITHE = .TRUE.
            ZI(JINF+2*NCHAR+1) = ICH
            IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL UTMESS('F','NMDOME_07',
     &                    'LA CHARGE THERMIQUE '//ZK24(IALICH-1+ICH) (1:
     &                    8)//' NE PEUT ETRE PILOTEE')
            END IF
          END IF


C ---- CHARGE DU TYPE HYDRATATION
          LCHIN = LIGRCH(1:13)//'.EVOL.HYDR'
          CALL JEEXIN(LCHIN,IRET)
          IF (IRET.NE.0) THEN
            IF (EXIHYD) THEN
              CALL UTMESS('F','NMDOME_08',
     &                    'IL Y A PLUSIEURS CHARGES'//' HYDRIQUES ')
            END IF
            EXIHYD = .TRUE.
            ZI(JINF+4*NCHAR+5) = ICH
            IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL UTMESS('F','NMDOME_09',
     &                    'LA CHARGE HYDRIQUE '//ZK24(IALICH-1+ICH) (1:
     &                    8)//' NE PEUT ETRE PILOTEE')
            END IF
          END IF


C ---- CHARGE DU TYPE SECHAGE

          LCHIN = LIGRCH(1:13)//'.EVOL.SECH'
          CALL JEEXIN(LCHIN,IRET)
          IF (IRET.NE.0) THEN
            IF (EXISEC) THEN
              CALL UTMESS('F','NMDOME_10',
     &                    'IL Y A PLUSIEURS CHARGES'//' SECHAGE ')
            END IF
            EXISEC = .TRUE.
            ZI(JINF+4*NCHAR+6) = ICH
            IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL UTMESS('F','NMDOME_11',
     &                    'LA CHARGE DE SECHAGE '//ZK24(IALICH-1+
     &                    ICH) (1:8)//' NE PEUT ETRE PILOTEE')
            END IF
          END IF


C ---- CHARGE DU TYPE DEFO ANELASTIQUES

          LCHIN = LIGRCH(1:13)//'.EPSI.ANEL '
          CALL JEEXIN(LCHIN,IRET)
          IF (IRET.NE.0) THEN
            IF (EXIANE) THEN
              CALL UTMESS('F','NMDOME_12','IL Y A PLUSIEURS CHARGES'//
     &                    ' DEFO.ANELASTIQUES ')
            END IF
            EXIANE = .TRUE.
            ZI(JINF+4*NCHAR+4) = ICH
            IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL UTMESS('F','NMDOME_13',
     &                    'LA CHARGE DEFO.ANELASTIQUES '//
     &                    ZK24(IALICH-1+ICH) (1:8)//
     &                    ' NE PEUT ETRE PILOTEE')
            END IF
          END IF


C ---- CHARGE DU TYPE EVOL_CHAR FCT DU TEMPS (EVOL_CHAR)

          LCHIN = LIGRCH(1:13)//'.EVOL.CHAR'
          CALL JEEXIN(LCHIN,IRET)
          IF ((IRET.NE.0) .AND. (TYPCHA.EQ.'FIXE_PIL')) CALL UTMESS('F',
     &        'NMDOME_15','LA CHARGE DE TYPE EVOL_CHAR '//
     &        ZK24(IALICH-1+ICH) (1:8)//' NE PEUT ETRE PILOTEE')


C ---- CHARGES DE TYPE FORCE DE LAPLACE

          INFC = 0
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
          INFMAX = MAX(INFMAX,INFC)
          ZI(JINF+2*NCHAR+2) = INFMAX


C ---- CHARGES DE TYPE 'LIAISON_UNIL_NO' (1) OU 'CONTACT' (2)

          LCHIN = LIGRCH(1:13)//'.CONI'
          CONTA = ZK24(IALICH-1+ICH) (1:8)//'.CONTACT.METHCO'
          CALL JEEXIN(LCHIN,IRET1)
          CALL JEEXIN(CONTA,IRET2)
          IF ((IRET1.NE.0) .AND. (IRET2.NE.0)) THEN
            CALL UTMESS('F','NMDOME_16','UNE MEME CHARGE NE PEUT '//
     &             'CONTENIR A LA FOIS LE MOT-CLE "LIAISON_UNIL_NO" ET '
     &                  //'LE MOT-CLE "CONTACT"')
          END IF
          IF (IRET1.NE.0) THEN
            ZI(JINF+2*NCHAR+2+ICH) = 1
            IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL UTMESS('F','NMDOME_17',
     &                    'LA CHARGE DE TYPE CONTACT '//ZK24(IALICH-1+
     &                    ICH) (1:8)//' NE PEUT ETRE PILOTEE')
            END IF
          ELSE IF (IRET2.NE.0) THEN
            ZI(JINF+2*NCHAR+2+ICH) = 2
            NCONTA = NCONTA + 1
            IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL UTMESS('F','NMDOME_18',
     &                    'LA CHARGE DE TYPE CONTACT '//ZK24(IALICH-1+
     &                    ICH) (1:8)//' NE PEUT ETRE PILOTEE')
            END IF
          ELSE
            ZI(JINF+2*NCHAR+2+ICH) = 0
          END IF


C ---- CHARGES DE TYPE ARLEQUIN

          LCHIN = LIGRCH(1:8)//'.POIDS_MAILLE'
          CALL JEEXIN(LCHIN,IRET)
          IF (IRET.GT.0) THEN
            CALL JEVEUO(LCHIN,'L',I0)
            CALL JELIRA(LCHIN,'LONMAX',N1,K16BID)
            IF (EXIARL) THEN
              DO 110 J = 1,N1
                R = ZR(I0-1+J)
                IF (R.NE.1.D0) ZR(I1-1+J) = ZR(I1-1+J)*R
  110         CONTINUE
            ELSE
              EXIARL = .TRUE.
              CALL WKVECT('&&POIDS_MAILLE','V V R',N1,I1)
              DO 120 J = 1,N1
                ZR(I1-1+J) = ZR(I0-1+J)
  120         CONTINUE
            END IF
          END IF

C ---- FONCTIONS MULTIPLICATIVES DES CHARGES
         IF (NOMCMD.NE.'DYNA_LINE_HARM') THEN

          CALL GETVID('EXCIT','FONC_MULT',INDIC,1,1,K24BID,N1)

          IF (NOMCMD.EQ.'DYNA_NON_LINE'.OR.
     &     NOMCMD.EQ.'DYNA_TRAN_EXPLI') THEN
            CALL GETVID('EXCIT','ACCE',INDIC,1,1,K24BID,N2)
          ELSE
            N2 = 0
          END IF

C      PAS DE FONCTIONS MULTIPLICATRICES -> CREATION FCT CSTE = 1
          IF (N1.EQ.0 .AND. N2.EQ.0) THEN
            NOMFCT = '&&NMDOME'
            CALL JEEXIN(NOMFCT(1:19)//'.PROL',IRET)
            IF (IRET.EQ.0) THEN
              CALL WKVECT(NOMFCT(1:19)//'.PROL','V V K16',5,JPRO)
              ZK16(JPRO) = 'CONSTANT'
              ZK16(JPRO+1) = 'LIN LIN '
              ZK16(JPRO+2) = 'TOUTPARA'
              ZK16(JPRO+3) = 'TOUTRESU'
              ZK16(JPRO+4) = 'CC      '
              CALL WKVECT(NOMFCT(1:19)//'.VALE','V V R',2,JVAL)
              ZR(JVAL) = 1.0D0
              ZR(JVAL+1) = 1.0D0
            END IF
            ZK24(IALIFC+ICH-1) = '&&NMDOME'

          ELSE

            IF (N1.NE.0) CALL GETVID('EXCIT','FONC_MULT',INDIC,1,1,
     &                               ZK24(IALIFC+ICH-1),N1)

            IF (N2.NE.0) CALL GETVID('EXCIT','ACCE',INDIC,1,1,
     &                               ZK24(IALIFC+ICH-1),N2)

            IF (TYPCHA.EQ.'FIXE_PIL') THEN
              CALL UTMESS('F','NMDOME_20',
     &                    'LA CHARGE '//ZK24(IALICH-1+ICH) (1:8)//
     &                    ' NE PEUT PAS UTILISER '//
     &                    'DE FONCTION MULTIPLICATIVE FONC_MULT '//
     &                    'CAR ELLE EST PILOTEE')
            END IF
          END IF
        ELSE
        
          CALL GETVID('EXCIT','FONC_MULT_C',ICH,1,1,NOMFCT,N1)
        
          IF ( N1 .EQ. 0 ) THEN
            CALL CODENT( ICH , 'D0' , KNUM  )
            NOMFCT = '&&NC'//KNUM
            CALL GETVC8('EXCIT','COEF_MULT_C',ICH,1,1,CCOEF,N2)
            IF ( N2 .EQ. 0 ) THEN
              CALL GETVR8('EXCIT','COEF_MULT',ICH,1,1,COEF,N3)
              CALL WKVECT(NOMFCT(1:19)//'.PROL','V V K16',5,JPRO)
              ZK16(JPRO)   = 'CONSTANT'
              ZK16(JPRO+1) = 'LIN LIN '
              ZK16(JPRO+2) = 'TOUTPARA'
              ZK16(JPRO+3) = 'TOUTRESU'
              ZK16(JPRO+4) = 'CC      '
              CALL WKVECT(NOMFCT(1:19)//'.VALE','V V R',3,JVAL)
              ZR(JVAL)   = 1.0D0
              ZR(JVAL+1) = COEF
              ZR(JVAL+2) = COEF
            ELSE
              CALL WKVECT(NOMFCT(1:19)//'.PROL','V V K16',5,JPRO)
              ZK16(JPRO)   = 'CONSTANT'
              ZK16(JPRO+1) = 'LIN LIN '
              ZK16(JPRO+2) = 'TOUTPARA'
              ZK16(JPRO+3) = 'TOUTRESU'
              ZK16(JPRO+4) = 'CC      '
              CALL WKVECT(NOMFCT(1:19)//'.VALE','V V R',3,JVAL)
              ZR(JVAL)   = 1.0D0
              ZR(JVAL+1) = DBLE( CCOEF )
              ZR(JVAL+2) = DIMAG( CCOEF )
           ENDIF
         ENDIF
         ZK24(IALIFC+ICH-1) = NOMFCT 

       ENDIF

  130   CONTINUE



C ----------------------------------------------------------------------
C                         VERIFICATIONS GLOBALES
C ----------------------------------------------------------------------

C -- EN PRESENCE DE PILOTAGE, VERIFIER QU'IL Y A DES CHARGES PILOTEES

        IF (NOMCMD.EQ.'STAT_NON_LINE') THEN
          CALL GETVTX('PILOTAGE','TYPE',1,1,1,K24BID,N1)
          IF (N1.NE.0 .AND. NPILO.EQ.0) THEN
            CALL UTMESS('F','NMDOME_21','ON NE PEUT PAS FAIRE DE '//
     &                  'PILOTAGE EN L''ABSENCE DE FORCES DE TYPE '//
     &                  '"FIXE_PILO"')
          END IF
        END IF


C -- VERIFICATION QU'AU PLUS UNE CHARGE EST DE TYPE 'CONTACT'

        IF (NCONTA.GT.1) THEN
          CALL UTMESS('F','NMDOME_22','IL NE PEUT PAS Y AVOIR DE '//
     &                'CONTACT (MOT-CLE "CONTACT") DANS PLUS D''UNE '//
     &                'CHARGE')
        END IF


C -- VERIFICATION QU'IL N'Y A QUE DES CHARGES MECANIQUES

        IF (NONMEC) THEN
          CALL UTMESS('F','NMDOME_23','IL Y A AU MOINS UNE CHARGE NON '
     &                //'MECANIQUE : VERIFIER LE FICHIER DE COMMANDES')
        END IF

      END IF

      CALL JEDEMA()
      END
