      SUBROUTINE OP0186 (IER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C RESPONSABLE                            DURAND C.DURAND
C TOLE CRP_20
C ----------------------------------------------------------------------
C     COMMANDE:  THER_NON_LINE
C
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/02/02 (OB): IMPLANTATION DES CALCULS DE SENSIBILITE.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
      
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------

C 0.2  ==> ARGUMENTS
      INTEGER  IER 
      
C 0.3. ==> VARIABLES LOCALES
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'OP0186' )
      LOGICAL  LOSTAT,LNUAR,MATCST,COECST,REASMA,ARRET,CONVER,ITEMAX,
     &         REASVC,REASVT,REASMT,REASRG,REASMS,PREM,LSECHA,LASSEM,
     &         RECHLI
      INTEGER  PARMEI(2),PARCRI(3),IAUX,JAUX,IDEB,JDEB,NUMORD,K,IERD,
     &         ICORET,NBGRPA,NRGRPA,NBPTPA,NRPTPA,NBCHAM,ITERHO,ITMAX,
     &         NUMFIN,IFM,NIV,NOPT,JARCH,IARCH,LONCH,JVAL,ITERAT,TYPESE,
     &         JTEMPP,JTEMPM,JTEMP,NBPASE,NRORES,JTEMPI,I, JCRR
      REAL*8   PARMER(2),TPSTHE(6),DELTAT,TPSNP1,TIMET,TIMTDT,TPS1(4),
     &         TPS2(4),TPS3(4),TPEX,PARCRR(2),THETA,KHI,RHO,TESTR,
     &         TESTM,PARA(2)
      CHARACTER*1  CREAS,BASE
      CHARACTER*3  KREAS
      CHARACTER*4  TYPCAL
      CHARACTER*8  EVOLSC,SAUX08,NOPASE
      CHARACTER*13 INPSCO
      CHARACTER*16 TYSD
      CHARACTER*19 INFCHA,SOLVEU,MAPREC
      CHARACTER*24 MODELE,MATE,CARELE,FOMULT,CHARGE,INFOCH,RESULT,TIME,
     &             TMPCHI,TMPCHF,COMPOR,NOMCH,VTEMP,VTEMPM,VTEMPP,
     &             VTEMPR,VEC2ND,VEC2NI,LIEVOL,LISOPT,LISARC,EXCARC,
     &             NUMEDD,MEDIRI,MATASS,CNDIRP,CNCHCI,CRITHE,
     &             K24BID,CNRESI,VABTLA,VHYDR,VHYDRP,VAPRIN,
     &             VAPRMO,CRITER
      CHARACTER*24 STYPSE
      CHARACTER*76 FMT,FMT2,FMT3,FMT4
      CHARACTER*85 FMT1

C ----------------------------------------------------------------------
      DATA INFCHA                 /'&&OP0186.INFCHA'/
      DATA CRITHE                 /'&&OP0186.CRITERE'/
      DATA SOLVEU                 /'&&OP0186.SOLVEUR'/
      DATA MAPREC                 /'&&OP0186.MAPREC'/
      DATA RESULT,LIEVOL          /2*' '/
      DATA CNDIRP                 /1*' '/
      DATA CNCHCI                 /1*' '/
      DATA VEC2ND                 /'&&OP0186.2ND' /
      DATA VEC2NI                 /'&&OP0186.2NI' /
      DATA TMPCHI,TMPCHF          /'&&OP0186.TCHI','&&OP0186.TCHF'/
      DATA VHYDR,VHYDRP           /'&&OP0186.HY'  ,'&&OP0186.HYP'/
      DATA MEDIRI                 /' '/
      DATA MATASS                 /'&&MTHASS'/
      DATA FMT                    /'(76(''*''))'/
      DATA FMT1                   /'(85(''*''))'/
      DATA FMT2  /'(A,1X,A,6X,A,9X,A,6X,A,3X,A,3X,A,1X,A)'/
      DATA FMT3  /'(A,16X,A,8X,A,6X,A,3X,A,6X,A,4X,A)'/
      DATA FMT4  /'(A,12X,A,2X,A,17X,A,9X,A,4X,A)'/
C ----------------------------------------------------------------------
C
C     MESURE DE TEMPS CPU :
C
C      1 : PAS DE TEMPS
C      2 : ITERATIONS
C      3 : ACTUALISATIONS ET ARCHIVAGE
C
C ----------------------------------------------------------------------

      CALL JEMARQ()
C               12   345678   90123
      INPSCO = '&&'//NOMPRO//'_PSCO'
      CRITER = '&&'//NOMPRO//'_RESGRA_GCPC    '

C **********************************************************************
C                    RECUPERATION DES OPERANDES
C **********************************************************************

C--- RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)

C --- LECTURE DES OPERANDES DE LA COMMANDE
C EN SENSIBILITE: 
C INPSCO --> SD CONTENANT LA LISTE DES CORRESPONDANCES,
C NBPASE --> NBRE DE SENSIBILITES DEMANDEES
C               12   345678
      SAUX08 = '&&'//NOMPRO
      CALL NXLECT (RESULT,MODELE,MATE,CARELE,MATCST,COECST,
     &             FOMULT,INFCHA,CHARGE,INFOCH,
     &             PARMEI,PARMER,SOLVEU,PARCRI,PARCRR,
     &             LISOPT,NOPT,COMPOR,EVOLSC,NBPASE,SAUX08,INPSCO)
      PARA(1) = PARMER(1)
      ITMAX = PARCRI(3)
      RECHLI = .FALSE.

C EST-ON DANS UN CALCUL DE SECHAGE ?
      IF(EVOLSC(1:1).NE.' ') THEN
        LSECHA = .TRUE.
        IF ( NBPASE.GT.0 ) THEN
          CALL UTMESS('F',NOMPRO,
     &      'OPTION SENSIBILITE NON DEVELOPPEE EN SECHAGE')
        ENDIF
      ELSE
        LSECHA = .FALSE.
      ENDIF
C --- CE BOOLEEN ARRET EST DESTINE AUX DEVELOPPEUR QUI VOUDRAIENT
C --- FORCER LE CALCUL MEME SI ON N'A PAS CONVERGENCE (ARRET=TRUE)
      ARRET  = .FALSE.
      IF(PARMEI(2).GT.0) RECHLI = .TRUE.

C **********************************************************************
C    INITIALISATIONS ET DUPLICATION DES STRUCTURES DE DONNEES
C **********************************************************************

C --- LECTURE DE L'ETAT INITIAL ET DES DONNEES D'INCREMENTATION
      CALL NTINIT(RESULT,MODELE,INFCHA,CHARGE,INFOCH,SOLVEU,
     &            NUMEDD,LNUAR,LOSTAT,LIEVOL,LISARC,EXCARC,TIME,
     &            NBPASE, INPSCO,VHYDR,K24BID,K24BID,NUMFIN,
     &            NBGRPA,IDEB,JDEB,NUMORD,TPSNP1,CRITHE)
      CALL JEVEUO( LISARC, 'L', JARCH )
      CALL JEVEUO(LIEVOL(1:19)//'.VALE','L',JVAL)

C --- REPRISE D'UN CALCUL OU ETAT INITIAL NON NUL
      IF (.NOT.LOSTAT) CALL NTREP0(NUMORD,TPSNP1,VHYDR,NBPASE,INPSCO)

C --- CREATION DES OBJETS DE TRAVAIL ET DES STRUCTURES DE DONNEES
      IAUX = 0
      JAUX = 4
      CALL PSNSLE(INPSCO,IAUX,JAUX,VTEMP)
      JAUX = 5
      CALL PSNSLE(INPSCO,IAUX,JAUX,VTEMPP)
      JAUX = 6
      CALL PSNSLE(INPSCO,IAUX,JAUX,VTEMPM)
      JAUX = 7
      CALL PSNSLE(INPSCO,IAUX,JAUX,VTEMPR)
      CALL COPISD('CHAMP_GD','V',VTEMP(1:19),VTEMPM(1:19))
      CALL COPISD('CHAMP_GD','V',VTEMP(1:19),VTEMPP(1:19))
      CALL COPISD('CHAMP_GD','V',VTEMP(1:19),VTEMPR(1:19))
      CALL COPISD('CHAMP_GD','V',VTEMP(1:19),VEC2ND(1:19))
      CALL COPISD('CHAMP_GD','V',VTEMP(1:19),VEC2NI(1:19))
      CALL COPISD('CHAMP_GD','V',VHYDR(1:19),VHYDRP(1:19))

C --- CALCUL DES MATRICES ELEMENTAIRES DES DIRICHLETS
      TYPCAL = 'THER'
      CALL MEDITH(TYPCAL,MODELE,CHARGE,INFOCH,MEDIRI)

C **********************************************************************
C                 BOUCLE SUR LES PAS DE TEMPS
C **********************************************************************

      CALL UTTCPU(1,'INIT',4,TPS1)
      CALL UTTCPU(2,'INIT',4,TPS2)
      CALL UTTCPU(3,'INIT',4,TPS3)
      TPEX   = TPS1(3)
      PREM   = .TRUE.
      REASRG = .FALSE.
      REASMS = .FALSE.

C --- BOUCLE SUR LES NBGRPA "GROS" PAS DE TEMPS (N FOIS LE MEME DELTAT)

      DO 1 NRGRPA = IDEB,NBGRPA

C --- RECUPERATION DU PAS DE TEMPS ET DES PARAMETRES DE RESOLUTION

        CALL NTITIN(LIEVOL,PARMER,NRGRPA,LOSTAT,DELTAT,NBPTPA,
     &              THETA,KHI)

        PARA(2) = DELTAT

C --- MATRICE TANGENTE REACTUALISEE POUR UN NOUVEAU DT

        REASMA = .TRUE.

C --- BOUCLE SUR LES NBPTPA "PETITS" PAS DE TEMPS
C --- ILS ONT TOUS LA MEME LONGUEUR : DELTAT

        DO 2 NRPTPA = JDEB,NBPTPA

          CALL UTTCPU(1,'DEBUT',4,TPS1)
          NUMORD = NUMORD + 1
          IF ( NUMORD .GT. NUMFIN ) GO TO 999
          TPSNP1 = ZR(JVAL+NUMORD)
          TPSTHE(1) = TPSNP1
          TPSTHE(2) = DELTAT
          TPSTHE(3) = THETA
          TPSTHE(4) = KHI
          WRITE (IFM,FMT)
          WRITE (IFM,'(A,9X,A,I4,10X,A,1PE11.3,16X,A)')
     &           '*','NUMERO D''ORDRE:',NUMORD,' INSTANT:',TPSNP1,'*'
          WRITE(IFM,FMT1)
          WRITE(IFM,FMT2) '*','ITERATION','RESIDU','RESIDU',
     &                    'ITERATION','COEFFICIENT','ACTUALISATION','*'
          WRITE(IFM,FMT3) '*','RELATIF','ABSOLU','RECH. LIN.',
     &                    'RECH. LIN.','MATRICE','*'
          WRITE(IFM,FMT4) '*','RESI_GLOB_RELA','RESI_GLOB_MAXI',
     &                    'RHO','TANGENTE','*'
          WRITE(IFM,FMT1)
          CALL JELIRA (VTEMPM(1:19)//'.VALE','LONMAX',LONCH,SAUX08)

C **********************************************************************
C                 BOUCLE SUR LES RESOLUTIONS
C LE PREMIER PASSAGE, NRORES=0, EST CELUI DU CALCUL STD NON-LINEAIRE,
C LES AUTRES SONT CEUX, LINEAIRES, DES CALCULS DE SENSIBILITE
C **********************************************************************
          LASSEM = .TRUE.
          DO 3 NRORES=0,NBPASE

C RECUPERATION DE:
C NOPASE --> NOM DU PARAMETRE SENSIBLE
C RESULT --> NOM DE LA SD RESULTAT
C EN STANDARD:
C VTEMP  --> T+,I+1BIS
C VTEMPP --> T-
C EN SENSIBILITE:
C VTEMP  --> (DT/DS)-
C VAPRIN --> T+
C VAPRMO --> T-
            IAUX = NRORES
            JAUX = 1
            CALL PSNSLE(INPSCO,IAUX,JAUX,NOPASE)
            JAUX = 3
            CALL PSNSLE(INPSCO,IAUX,JAUX,RESULT)
            JAUX = 4
            CALL PSNSLE(INPSCO,IAUX,JAUX,VTEMP)
            IF (NBPASE.NE.0.AND.NRGRPA.GT.0.AND.NRORES.EQ.0) THEN
C CALCUL STD TRANSITOIRE AVEC SENSIBILITE A SUIVRE
              JAUX = 5
              CALL PSNSLE(INPSCO,IAUX,JAUX,VTEMPP)
            ENDIF
            IF (NRORES.GT.0) THEN
C CALCUL DE SENSIBILITE
              IAUX = 0
              JAUX = 4
              CALL PSNSLE(INPSCO,IAUX,JAUX,VAPRIN)
              JAUX = 5
              CALL PSNSLE(INPSCO,IAUX,JAUX,VAPRMO)

C REPERAGE DU TYPE DE DERIVATION
              CALL NTTYSE(NBPASE,INPSCO,NOPASE,TYPESE, STYPSE)
             
              IF (TYPESE.EQ.1) THEN
C PB DERIVEE INSENSIBLE ==> ON N'A PAS A RESOUDRE DE SYSTEME, CAR LA
C SOLUTION EST NULLE. ON CREER DONC UN CHAMP SOLUTION NUL VTEMPM/VHYDRP
                CALL JEVEUO(VTEMPM(1:19)//'.VALE','E',JTEMPI)
                DO 5 I=1,LONCH
                  ZR(JTEMPI+I-1) = 0.D0
 5              CONTINUE
C POUR DECLENCHER L'ASSEMBLAGE MATRICE POUR LA PROCHAINE SENSIBILITE
                IF (NRORES.EQ.1) LASSEM = .FALSE.
                CALL UTDEBM('A',NOMPRO,'CALCUL INSENSIBLE')
                CALL UTIMPK('L','VARIABLE SENSIBLE:',1,NOPASE)
                CALL UTFINM() 
                GOTO 4
              ELSE IF (TYPESE.EQ.-1) THEN
                CALL UTMESS('F',NOMPRO,'OPTION SENSIBILITE '//
     &            'LAGRANGIENNE NON DEVELOPPEE EN NON LINEAIRE')
              ENDIF
            ELSE IF (NRORES.EQ.0) THEN
C CALCUL STD
              TYPESE = 0
              STYPSE = ' '
            ENDIF
                      
C --- RECUPERATION DU CHAMP DE TEMPERATURE A T ET T+DT POUR LE SECHAGE
C     LOIS SECH_GRANGER ET SECH_NAPPE
            IF (LSECHA) THEN
              CALL GETTCO(EVOLSC,TYSD)
              IF (TYSD(1:9).EQ.'EVOL_THER') THEN
                CALL DISMOI('F','NB_CHAMP_UTI',EVOLSC,'RESULTAT',
     &                    NBCHAM,SAUX08,IERD)
                IF (NBCHAM.GT.0) THEN
                  TIMET = TPSNP1
                  TIMTDT = TPSNP1 + DELTAT
                  BASE   = 'V'
                  CALL RSINCH(EVOLSC,'TEMP','INST',TIMET,TMPCHI,
     &                      'CONSTANT','CONSTANT',1,BASE,ICORET)
                  IF (ICORET.GE.10) THEN
                  CALL UTDEBM('F',NOMPRO,'INTERPOLATION TEMPERATURE:')
                    CALL UTIMPK('L','EVOL_THER:',1,EVOLSC)
                    CALL UTIMPR('S','INSTANT:',1,TIMET)
                    CALL UTIMPI('L','ICORET:',1,ICORET)
                    CALL UTFINM()
                  ENDIF
                  CALL RSINCH(EVOLSC,'TEMP','INST',TIMTDT,TMPCHF,
     &                        'CONSTANT','CONSTANT',1,BASE,ICORET)
                  IF (ICORET.GE.10) THEN
                  CALL UTDEBM('F',NOMPRO,'INTERPOLATION TEMPERATURE:')
                    CALL UTIMPK('L','EVOL_THER:',1,EVOLSC)
                    CALL UTIMPR('S','INSTANT:',1,TIMTDT)
                    CALL UTIMPI('L','ICORET:',1,ICORET)
                    CALL UTFINM()
                  ENDIF
                ELSE
                  CALL UTMESS('F',NOMPRO,' LE CONCEPT EVOL_THER : '
     &          //EVOLSC//' NE CONTIENT AUCUN CHAMP DE TEMPERATURE')
               ENDIF
             ENDIF
           ENDIF
C RE-ASSEMBLAGE DES SECONDS MEMBRES DE VECHTH/VECHNL
           REASVC = .TRUE.
C RE-ASSEMBLAGE DES SECONDS MEMBRES DE VETNTH
           REASVT = .TRUE.
C RE-ASSEMBLAGE DE LA MATRICE:
C SAUF POUR UN CALCUL MULTI-SENSIBILITES POUR LEQUEL, A CHAQUE PAS DE
C TEMPS, ON NE REASSEMBLE PAS LA MATRICE DU SYST DERIVE LINEAIRE. SAUF
C APRES UN CALCUL INSENSIBLE SITUE AU PREMIER RANG.
           IF ( (NRORES.GT.1).AND.LASSEM ) THEN
             REASMT = .FALSE.
           ELSE
             REASMT = .TRUE.
             LASSEM = .TRUE.
           ENDIF
           
C ======================================================================
C  ACTUALISATION DES MATRICES ET VECTEURS POUR LE NOUVEAU PAS DE TEMPS
C ======================================================================

C --- ACTUALISATION DU CHARGEMENT A TMOINS
C EN STD:
C ON ASSEMBLE LES SECONDS MEMBRES CHAR_THER_LINEAIRE+CHAR_THER_NONLIN+
C CHAR_THER_EVOLNI EN BETA DANS VEC2ND (IDEM EN RHO_CP DANS VEC2NI)
C ON ASSEMBLE LA MATRICE A = TANGENTE (MTAN_*) + DIRICHLET
C EN SENSIBILITE:
C IDEM POUR LE SECOND MEMBRE (EN RHO_CP DANS VEC2ND) ET POUR LA MATRICE
C (OU ON REMPLACE (DT/DS)- PAR T+)
           IAUX = NRGRPA
           CALL NXACMV(MODELE,MATE,CARELE,FOMULT,CHARGE,INFCHA,
     &                 INFOCH,NUMEDD,SOLVEU,IAUX,TIME,TPSTHE,
     &                 REASVC,REASVT,REASMT,REASRG,REASMS,CREAS,
     &                 VTEMP,VHYDR,TMPCHI,TMPCHF,VEC2ND,VEC2NI,
     &                 MATASS,MAPREC,CNDIRP,CNCHCI,MEDIRI,COMPOR,
     &                 TYPESE,STYPSE,NOPASE,VAPRIN,VAPRMO)
     
C RESOLUTION DU SYSTEME LINEAIRE EN SENSIBILITE NON INSENSIBLE
C ON RESOUT A(T+) * (DT/DS)+ = B(T-,T+,(DT/DS)-)
C A = MATASS, B=VEC2ND, SOLUTION=VTEMPM

           IF (TYPESE.NE.0) THEN
             CALL RESOUD(MATASS,MAPREC,VEC2ND,SOLVEU,
     &         CNCHCI,'V',VTEMPM,CRITER)

C ON STOCKE LA SOLUTION VTEMPM DANS VTEMP
              GOTO 4
           ENDIF
           
C ======================================================================
C                        PHASE DE PREDICTION
C ======================================================================
C SECONDS MEMBRES ASSEMBLES B
C EN STATIONNAIRE: |VEC2ND - RESI_THER - (BT)*LAGRANGE|
C                  | DIRICHLET - B*TEMPERATURE INIT   |
C EN TRANSITOIRE : |            VEC2NI                |
C                  |           DIRICHLET              |
C SYSTEME LINEAIRE RESOLU:  A * (T+,1 - T-) = B
C SOLUTION: VTEMP= T- ET VTEMPM = T+,1

           CALL NXPRED(MODELE,MATE,CARELE,CHARGE,INFOCH,NUMEDD,SOLVEU,
     &                 NRGRPA,TIME,LONCH,MATASS,MAPREC,
     &                 VTEMP,VTEMPM,VTEMPP,
     &                 VHYDR,VHYDRP,TMPCHI,TMPCHF,
     &                 COMPOR,CNDIRP,CNCHCI,VEC2ND,VEC2NI,PREM)
           PREM = .FALSE.

C ======================================================================
C              ITERATIONS DE LA METHODE DE NEWTON-RAPHSON
C ======================================================================

           ITERAT = 0
           ITEMAX = .FALSE.
           CONVER = .FALSE.

C --- REPRISE DE LA BOUCLE D'ITERATIONS DE NEWTON-RAPHSON

 300       CONTINUE

C --- DOIT ON REACTUALISER LA MATRICE TANGENTE

           CALL UTTCPU (2,'DEBUT',4,TPS2)
           ITERAT = ITERAT + 1
           REASMA = .FALSE.
           KREAS  = 'NON'
           IF ((PARMEI(1).NE.0)) THEN
             IF (MOD(ITERAT,PARMEI(1)).EQ.0) THEN
               REASMA = .TRUE.
               KREAS  = 'OUI'
             ENDIF
           ENDIF

C ON ASSEMBLE LE SECOND MEMBRE B= |VEC2ND - RESI_THER - (BT)*LAGRANGE|
C                                 |             0                    |
C SYSTEME LINEAIRE RESOLU:  A * (T+,I+1 - T+,I) = B
C SOLUTION: VTEMPP = T+,I+1 - T+,I

           CALL NXNEWT(MODELE,MATE,CARELE,CHARGE,INFCHA,INFOCH,NUMEDD,
     &                 SOLVEU,TIME,LONCH,MATASS,MAPREC,CNCHCI,VTEMP,
     &                 VTEMPM,VTEMPP,VEC2ND,MEDIRI,CONVER,VHYDR,VHYDRP,
     &                 TMPCHI,TMPCHF,COMPOR,VABTLA,CNRESI,PARCRI,PARCRR,
     &                 REASMA,TESTR,TESTM)

C --- SI NON CONVERGENCE ALORS RECHERCHE LINEAIRE
C       (CALCUL DE RHO) SUR L INCREMENT VTEMPP
C --- ACTUALISATION DE LA TEMPERATURE VTEMPM AVEC L INCREMENT VTEMPP
C     MULTIPLIE PAR RHO
           RHO    = 0.D0
           ITERHO = 0
           IF(.NOT.CONVER) THEN
             IF(RECHLI) THEN
             
C ON CALCULE LE RHO/ VTEMPR = T+,I+1BIS = T+,1 + RHO * (T+,I+1 - T+,I)
C MINIMISE VEC2ND - RESI_THER(T+,I+1BIS) - (BT)*LAGRANGE
               CALL NXRECH(MODELE,MATE,CARELE,CHARGE,
     &                     INFOCH,NUMEDD,TIME,LONCH,COMPOR,
     &                     VTEMPM,VTEMPP,VTEMPR,VTEMP,VHYDR,VHYDRP,
     &                     TMPCHI,TMPCHF,VEC2ND,VABTLA,CNRESI,RHO,
     &                     ITERHO,PARMER,PARMEI)
              ELSE
                RHO = 1.D0
              ENDIF
              CALL JEVEUO (VTEMPP(1:19)//'.VALE','L',JTEMPP)
              CALL JEVEUO (VTEMPM(1:19)//'.VALE','E',JTEMPM)
              CALL JEVEUO (VTEMP(1:19)//'.VALE','L',JTEMP)
              
C SOLUTION: VTEMPM = VTEMPR = T+,I+1BIS    
              DO 155 K = 1,LONCH
                ZR(JTEMPM+K-1)=ZR(JTEMPM+K-1)+RHO*ZR(JTEMPP+K-1)
 155          CONTINUE
            ENDIF

            WRITE (IFM,
     &  '(A,1X,I5,6X,1PE12.5,4X,1PE12.5,7X,I2,5X,1PE12.5,8X,A,6X,A)')
     &      '*',ITERAT,TESTR,TESTM,ITERHO,RHO,KREAS,'*'
            IF (ITERAT.GE.ITMAX) ITEMAX = .TRUE.
            CALL UTTCPU (2,'FIN',4,TPS2)
            IF ( (.NOT.CONVER).AND.(.NOT.ITEMAX) ) THEN
             IF ( 2.D0*TPS2(4).GT.0.95D0*TPS2(1)-TPS3(4) ) THEN
              WRITE (IFM,FMT1)
              CALL UTDEBM('S',NOMPRO,'ARRET PAR MANQUE DE TEMPS CPU')
              CALL UTIMPI('S',' AU NUMERO D''ORDRE : ',1,NUMORD)
              CALL UTIMPI('S',' LORS DE L''ITERATION : ',1,ITERAT)
              CALL UTIMPR('L',' TEMPS MOYEN PAR ITERATION : ',1,
     &                     TPS2(4))
              CALL UTIMPR('L',' TEMPS CPU RESTANT: ',1,TPS2(1))
              CALL UTFINM()
             ELSE
              GOTO 300
             END IF
            ELSEIF ((.NOT.CONVER).AND.ITEMAX.AND.(.NOT.ARRET)) THEN
              WRITE (IFM,FMT1)
              CALL UTDEBM ('S',NOMPRO,'ARRET PAR NON CONVERGENCE')
              CALL UTIMPI ('S',' AU NUMERO D''ORDRE : ',1,NUMORD)
              CALL UTIMPI ('S',' LORS DE L''ITERATION : ',1,ITERAT)
              CALL UTFINM ()
            ENDIF
            WRITE (IFM,FMT1)

C ======================================================================
C                   ACTUALISATIONS ET ARCHIVAGE
C ======================================================================
C INDIRECTION POUR PB INSENSIBLE (TYPESE.EQ.1)
 4          CONTINUE
 
            CALL UTTCPU(3,'DEBUT',4,TPS3)
            IF (LNUAR) THEN
                IARCH = ZI(JARCH+NUMORD-1)
            ELSE
                IARCH = ZI(JARCH+NUMORD)
            ENDIF
            IF (NUMORD.EQ.NUMFIN) IARCH = 1
            IF (NRORES.EQ.0)
     &        CALL COPISD('CHAMP_GD','V',VHYDRP(1:19),VHYDR(1:19))
            
C ==> ON DOIT GARDER LA TEMPERATURE DU PAS DE TEMPS PRECEDENT SI
C     TOUT CE QUI SUIT EST REUNI :
C  . IL Y AURA UN CALCUL DE DERIVEE
C  . ON EST EN TRANSITOIRE
C  . ON EST DANS LE CALCUL STANDARD
C VTEMP --> VTEMPP STRUCTURE RECONNUE COMME T-=VAPRMO POUR LE CALCUL
C DE SENSIBILITE QUI VA SUIVRE
            IF (NBPASE.NE.0 .AND. NRGRPA.GT.0 .AND. NRORES.EQ.0)
     &        CALL COPISD('CHAMP_GD','V',VTEMP(1:19),VTEMPP(1:19))
            IF (NIV.EQ.2) THEN
              WRITE(IFM,*)
              WRITE(IFM,*)'**************************************'
              WRITE(IFM,*)' THER_NON_LINE: OP00186'
              WRITE(IFM,*)
              WRITE(IFM,*)' T+ (OU DT/DS)+       :',VTEMP
              WRITE(IFM,*)' T-                   :',VTEMPP
              WRITE(IFM,*)
            ENDIF
C
C ======================================================================
C -- PREPARATION DES PARAMETRES ARCHIVEES ------------------------------
C ======================================================================
           IF ( CONVER ) THEN
              CALL JEVEUO(CRITHE(1:19)//'.CRTR','E',JCRR)
              ZR(JCRR+0) = ITERAT
              ZR(JCRR+1) = ITERHO
              ZR(JCRR+2) = TESTR
              ZR(JCRR+3) = TESTM
              ZR(JCRR+4) = RHO
           ENDIF
                   
C VTEMPM --> VTEMP ET ON ARCHIVE VTEMP
            CALL NTSTOC(IARCH,NUMORD,RESULT,COMPOR,NOMCH,VTEMP,VTEMPM,
     &                  VHYDR,LONCH,TPSNP1,CRITHE,PARA)
            CALL UTTCPU(3,'FIN',4,TPS3)
          
C --- FIN BOUCLE SUR LES RESOLUTIONS PB STD/PB DERIVEE
 3        CONTINUE
 
C --- TEMPS DISPONIBLE POUR CONTINUER ?

          CALL UTTCPU (1,'FIN',4,TPS1)
          WRITE(IFM,FMT)
          WRITE(IFM,'(A,21X,A,1PE10.2,37X,A)')
     &                                 '*','DUREE:',TPS1(3)-TPEX,'*'
          WRITE(IFM,FMT)
          WRITE(IFM,'(/)')
          TPEX = TPS1(3)
          IF(TPS1(4).GT.0.48D0*TPS1(1)) THEN
            CALL UTDEBM('S',NOMPRO,'ARRET PAR MANQUE DE TEMPS CPU')
            CALL UTIMPI('S',' AU NUMERO D''ORDRE: ',1,NUMORD)
            CALL UTIMPR('L',' TEMPS MOYEN PAR PAS DE TEMPS: ',1,TPS1(4))
            CALL UTIMPR('L',' TEMPS CPU RESTANT: ',1,TPS1(1))
            CALL UTFINM()
          ENDIF

C --- ON VA REFAIRE UN "PETIT" PAS DE TEMPS
 2      CONTINUE
        JDEB = 1

C --- ON VA REFAIRE UN "GROS" PAS DE TEMPS
 1    CONTINUE

C **********************************************************************
C              FIN DE LA BOUCLE SUR LES "GROS" PAS DE TEMPS
C **********************************************************************

 999  CONTINUE

C **********************************************************************
C                         CALCUL DES OPTIONS
C **********************************************************************

C --- CALCUL DES OPTIONS

      CALL NTOPTC (RESULT,MODELE,MATE,CARELE,CHARGE,INFOCH,
     &             LISOPT,NOPT,PARCRR(1))

C --- ON FAIT LE MENAGE ...
      CALL JEDETC ('V','&&',1)
      CALL JEDETC ('V','_',1)
      CALL JEDETC ('V','.CODI',20)
      CALL JEDETC ('V',RESULT(1:8),1)
      CALL JEDETC ('V','.MATE_CODE',9)
      CALL TITRE ()
      LIEVOL = ' '
C ----------------------------------------------------------------------
      CALL JEDEMA()
      END
