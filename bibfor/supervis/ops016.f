      SUBROUTINE OPS016 ( ICMD , ICOND , IER )
      IMPLICIT   NONE
      INTEGER             ICMD , ICOND , IER
C     ------------------------------------------------------------------
C TOLE  CRP_20
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 02/10/2002   AUTEUR F1BHHAJ J.ANGLES 
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
C RESPONSABLE F1BHHAJ J.ANGLES
C                   MACR_ASPIC_MAIL
C
C     ON UTILISE LES UNITES :
C     POUR GIBI :
C         70 : POUR ECRIRE LES DONNEES GIBI DE LA PROCEDURE
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      INUMEX, N1, UNITD, INFO, IERUSR, LLOGIE, I8, I,
     +             LFORMA, LFICHI, NF, VERSIO, UNITS, UNITEF, LRM,
     +             NFISS, NT, NS, NC, NDT, NSDT, NBAZIT, I16, NIMP,
     +             IFORM, ILFOR, IVERS, IFICH, ILFIC, IUNIT, INF, NL,
     +             ITYPSO, I4, NIVGIB, INIVG, NIVMAG
      REAL*8       EPT1, DET1, D1, D2, EPT2, DET2, ZMAX, H, ALPHA,
     +             JEU, EPC, DEC, XMAX, THETA, EPSI, RMB, RMT, VAL1,
     +             VAL2, LZMAX, RMC, LXMAX, XMAXC, ZMAXC, VAL3, VAL4,
     +             EPS, RC0, RC1, RC2, RC3, A, C, LIGA, ZETA, DPENE
      REAL*8       PI,R8PI,LPIQ,LEQU,R8B
      REAL*8       RAP,RAPL,ALP,BETA,FETIRF,FETIRP
      LOGICAL      IMPR, LOK, SAIN, FISLON, FISCOU
      LOGICAL      GROS,CAS1,CAS2,CAS3
      CHARACTER*2  NUME
      CHARACTER*4  TYPELE, AXIS,K4B
      CHARACTER*8  TYPSOU
      CHARACTER*8  NOMRES, TYPMAI, GRNOID, GRNOFD, GRNOII, GRNOFI,
     +             GROUMA(4), K8B, K8B1, K8B2, K8B3, K8B4, SAUGIB,
     +             DONGIB, NOPAR1, NOPAR2, TYPE, POSI, TFISS,MODELE,
     +             GRMAIL(5), NOMMA, GRNOLD, GRNOLI, GRNOND, GRNONI,
     +             GRMA2D(6),MAPROV, NOCO1, NOCO2, NOTU
      CHARACTER*16 TYPRES, NOMCMD, STATUT, FORMAT, FICHIE, OPTION
      CHARACTER*16 K16B1, K16B2,K16B3
      CHARACTER*128 REP,LOGIEL
      OPTION = 'SEGM_DROI_ORDO'
C     ------------------------------------------------------------------
      DATA GROUMA / 'NIDXT' , 'NEDXT' , 'NIIXT' , 'NEIXT' /
      DATA GRMAIL / 'EQUERRE' , 'PEAUINT' , 'EXCORP1' ,
     +              'EXCORP2' , 'EXTUBU'  /
      DATA GRMA2D / 'PEAUINT' , 'EXCORP1' , 'EXCORP2' , 'EXTUBU' ,
     +              'LEVRTUBU' , 'LEVRCORP'  /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER = 0
      IF ( ICOND .NE. -1 ) GOTO 9999
C     ------------------------------------------------------------------
C
      CALL GETRES ( NOMRES, TYPRES, NOMCMD )
C
      PI = R8PI()
C
C     --- L'EXECUTABLE ---
C
      CALL GETVTX ( 'EXEC_MAILLAGE', 'LOGICIEL'  , 1,1,1, LOGIEL, N1)
      CALL GETVIS ( 'EXEC_MAILLAGE', 'UNITE_DATG', 1,1,1,  UNITD, N1)
      CALL GETVIS ( 'EXEC_MAILLAGE', 'UNITE_MGIB', 1,1,1,  UNITS, N1)
      CALL GETVIS ( 'EXEC_MAILLAGE', 'NIVE_GIBI' , 1,1,1, NIVMAG, N1)
C
C     --- TYPE DE MAILLE ---
C
      CALL GETVTX ( ' ', 'TYPE_ELEM' , 1,1,1, TYPELE, N1)
C
C     --- RAFFINEMENT MAILLAGE ---
C
      TYPMAI = 'FIN '
      CALL GETVTX ( ' ', 'RAFF_MAIL', 1,1,1, TYPMAI, N1 )
      GROS=TYPMAI .EQ. 'GROS'
      IF ( GROS ) THEN
         NBAZIT = 40
      ELSE
         NBAZIT = 48
      ENDIF
C
C     --- CARACTERISTIQUES DE LA TUBULURE ---
C
      CALL GETVR8 ( 'TUBULURE', 'E_BASE'   , 1,1,1, EPT1 , N1 )
      CALL GETVR8 ( 'TUBULURE', 'DEXT_BASE', 1,1,1, DET1 , N1 )
      CALL GETVR8 ( 'TUBULURE', 'L_BASE'   , 1,1,1, D1   , N1 )
      CALL GETVR8 ( 'TUBULURE', 'L_CHANF'  , 1,1,1, D2   , N1 )
      CALL GETVR8 ( 'TUBULURE', 'E_TUBU'   , 1,1,1, EPT2 , N1 )
      CALL GETVR8 ( 'TUBULURE', 'DEXT_TUBU', 1,1,1, DET2 , N1 )
      CALL GETVR8 ( 'TUBULURE', 'Z_MAX'    , 1,1,1, ZMAX , N1 )
      CALL GETVTX ( 'TUBULURE', 'TYPE'     , 1,1,1, TYPSOU , N1 )
      CALL GETVR8 ( 'TUBULURE', 'L_PENETR' , 1,1,1, DPENE , N1 )
C
      IF ((TYPSOU.EQ.'TYPE_2') .AND. (DPENE.GT.0.D0)) THEN
         CALL UTMESS('F',NOMCMD,'LES PIQUAGES PENETRANTS SONT '//
     +                   'AUTORISES UNIQUEMENT AVEC LES '//
     +                   'SOUDURES DE TYPE 1')
      ENDIF
C
      IF (TYPSOU.EQ.'TYPE_2') THEN
         ITYPSO=2
      ELSE
         ITYPSO=1
      ENDIF
C
C     --- CARACTERISTIQUES DE LA SOUDURE ---
C
      CALL GETVR8 ( 'SOUDURE', 'H_SOUD'    , 1,1,1, H    , N1 )
      CALL GETVR8 ( 'SOUDURE', 'ANGL_SOUD' , 1,1,1, ALPHA, N1 )
      CALL GETVR8 ( 'SOUDURE', 'JEU_SOUD'  , 1,1,1, JEU  , N1 )
C
C     --- CARACTERISTIQUES DU CORPS ---
C
      CALL GETVR8 ( 'CORPS', 'E_CORP'   , 1,1,1, EPC  , N1 )
      CALL GETVR8 ( 'CORPS', 'DEXT_CORP', 1,1,1, DEC  , N1 )
      CALL GETVR8 ( 'CORPS', 'X_MAX'    , 1,1,1, XMAX , N1 )
C
      EPSI = 1.D-03
      RMB = ( DET1 - EPT1 ) / 2.0D0
      VAL1 = 1.50D0 * SQRT( RMB**3 / EPT1 )
      VAL3 = 3.0D0 * SQRT( RMB * EPT1 )
      RMT = ( DET2 - EPT2 ) / 2.0D0
      VAL2 = 1.50D0 * SQRT( RMT**3 / EPT2 )
      VAL4 = 3.0D0 * SQRT( RMT * EPT2 )
      LZMAX = MAX ( VAL1 , VAL2, VAL3, VAL4 )
      ZMAXC = LZMAX + ( DEC / 2.0D0 ) + D1 + D2
      LOK = ( ABS(ZMAX-ZMAXC) .LE. EPSI * ABS(ZMAXC) )
      IF ( .NOT. LOK ) THEN
         IER = IER + 1
         CALL UTDEBM('E',NOMCMD,'ERREUR DONNEES')
         CALL UTIMPR('L',' Z_MAX FOURNIE  : ',1, ZMAX  )
         CALL UTIMPR('L',' Z_MAX CALCULEE : ',1, ZMAXC )
         CALL UTFINM()
      ENDIF
C
      RMC = ( DEC - EPC ) / 2.0D0
      VAL1 = 1.50D0 * SQRT( RMC**3 / EPC )
      VAL2 = 3.0D0 * SQRT( RMC * EPC )
      LXMAX = MAX ( VAL1 , VAL2 )
      XMAXC = LXMAX + ( DET1 / 2.0D0 )
      LOK = ( ABS(XMAX-XMAXC) .LE. EPSI * ABS(XMAXC) )
      IF ( .NOT. LOK ) THEN
         IER = IER + 1
         CALL UTDEBM('E',NOMCMD,'ERREUR DONNEES')
         CALL UTIMPR('L',' X_MAX FOURNIE  : ',1, XMAX  )
         CALL UTIMPR('L',' X_MAX CALCULEE : ',1, XMAXC )
         CALL UTFINM()
      ENDIF
      IF (IER.NE.0) CALL UTMESS('F',NOMCMD,'ARRET SUR ERREUR')
      CALL UTDEBM('I',NOMCMD,' ')
      CALL UTIMPR('L',' X_MAX CALCULEE : ',1, XMAXC )
      CALL UTIMPR('L',' Z_MAX CALCULEE : ',1, ZMAXC )
      CALL UTFINM()
C
C     --- CARACTERISTIQUES DE LA FISSURE ---
C
      SAIN   = .FALSE.
      FISLON = .FALSE.
      FISCOU = .FALSE.
      THETA  = 0.0D0
      CALL GETFAC ( 'FISS_SOUDURE', NFISS )
      IF ( NFISS .EQ. 0 ) THEN
         SAIN = .TRUE.
      ELSE
         CALL GETVTX ( 'FISS_SOUDURE', 'TYPE', 1,1,1, TYPE, N1 )
         IF ( TYPE .EQ. 'LONGUE' ) THEN
            FISLON = .TRUE.
         ELSEIF ( TYPE .EQ. 'COURTE' ) THEN
            FISCOU = .TRUE.
         ENDIF
        CALL GETVR8 ( 'FISS_SOUDURE', 'AZIMUT'       , 1,1,1, THETA,N1)
        CALL GETVR8 ( 'FISS_SOUDURE', 'ANGL_OUVERTURE',1,1,1, EPS  ,N1)
        CALL GETVTX ( 'FISS_SOUDURE', 'AXIS'         , 1,1,1, AXIS ,N1)
        CALL GETVTX ( 'FISS_SOUDURE', 'POSITION'     , 1,1,1, POSI ,N1)
        CALL GETVTX ( 'FISS_SOUDURE', 'FISSURE'      , 1,1,1, TFISS,N1)
        CALL GETVR8 ( 'FISS_SOUDURE', 'PROFONDEUR'   , 1,1,1, A    ,N1)
        CALL GETVR8 ( 'FISS_SOUDURE', 'LONGUEUR'     , 1,1,1, C    ,N1)
C
        IF ((TFISS.EQ.'DEB_INT') .AND. (POSI.EQ.'INCLINE')
     +      .AND.(DPENE.GT.0.D0) .AND. (JEU.GT.0.D0)      ) THEN
           CALL UTMESS ('F',NOMCMD,'DANS LE CAS DE FISSURES '//
     +              'INCLINEES DEBOUCHANT EN PEAU INTERNE AVEC '//
     +              'PIQUAGE PENETRANT LE JEU DOIT ETRE NUL')
        ENDIF
C
        ZETA = 0.5D0
        IF (TFISS.NE.'DEB_INT'.AND.TFISS.NE.'DEB_EXT') THEN
           CALL GETVR8 ( 'FISS_SOUDURE', 'LIGA_INT'  , 1,1,1, LIGA ,NL)
           IF(NL.EQ.0) THEN
           CALL UTMESS('F',NOMCMD,'DANS LE CAS DE FISSURES INTERNES '//
     .              '(NON_DEB) LE LIGAMENT INFERIEUR EST OBLIGATOIRE')
           ENDIF
C
           IF (POSI.EQ.'DROIT') THEN
              IF (ITYPSO.EQ.1) THEN
                 ZETA = (A+LIGA)/(EPC+H)
              ELSE
                 ZETA = (A+LIGA)/(EPT1+H)
              ENDIF
           ELSE
              IF (ITYPSO.EQ.1) THEN
                 ZETA = (A+LIGA)*COS(ALPHA*PI/180.D0)/EPC
              ELSE
                 ZETA = (A+LIGA)*COS(ALPHA*PI/180.D0)/EPT1
              ENDIF
           ENDIF
C
           IF (ZETA.LT.0.1D0) THEN
           CALL UTMESS('F',NOMCMD,'DANS LE CAS DE FISSURES INTERNES '//
     .              '(NON_DEB) LE LIGAMENT EST TROP PETIT')
           ENDIF
           IF (ZETA.GT.0.9D0) THEN
           CALL UTMESS('F',NOMCMD,'DANS LE CAS DE FISSURES INTERNES '//
     .              '(NON_DEB) LE LIGAMENT EST TROP GRAND')
           ENDIF
           IF (LIGA.LT.(0.1D0 * EPC)) THEN
           CALL UTMESS('F',NOMCMD,'DANS LE CAS DE FISSURES INTERNES '//
     .              '(NON_DEB) LE LIGAMENT EST TROP PETIT')
           ENDIF
           IF ((LIGA + 2.D0*A).GT.(0.9D0 * EPC)) THEN
           CALL UTMESS('F',NOMCMD,'DANS LE CAS DE FISSURES INTERNES '//
     .              '(NON_DEB) LE LIGAMENT EST TROP GRAND')
           ENDIF
        ENDIF
C
        IF (N1.EQ.0) THEN
        IF (FISCOU) THEN
        CALL UTMESS('F',NOMCMD,'DANS LE CAS DE LA FISSURE COURTE IL'//
     .                ' FAUT PRECISER LA LONGUEUR')
        ENDIF
        IF (AXIS.EQ.'NON') THEN
        CALL UTMESS('F',NOMCMD,'DANS LE CAS DE LA FISSURE LONGUE IL'//
     .                ' FAUT PRECISER LA LONGUEUR OU AXIS=OUI')
        ENDIF
        C=0.D0
        ELSE
        IF (AXIS.EQ.'OUI') THEN
        CALL UTMESS('A',NOMCMD,'FISSURE AXISYMETRIQUE : LE MOT CLEF'//
     .                ' <LONGUEUR> NE DOIT PAS ETRE RENSEIGNE')
        ENDIF
        ENDIF
        C = 0.5D0 * C
C
        LEQU=2*(PI*(DEC-EPC)-DET1+2*EPT1)
C LPIQ EST UNE VALEUR QUI DEPEND THEORIQUEMENT DE LA FISSURE. LA VALEUR
C CI-DESSOUS EST APPROCHEE CAR ELLE NE SERT QU'A CALCULER LES FACTEURS
C D'ETIREMENT
        LPIQ=PI*(DET1)
        IF (AXIS.EQ.'OUI') THEN
        C=100.D0*LPIQ
        ENDIF
        RAPL=LEQU/LPIQ
        IF (FISCOU) THEN
        RAP=A/C
        CAS1=RAP.LT.0.3499D0
        CAS3=RAP.GT.0.4999D0
        CAS2=.NOT.(CAS1.OR.CAS3)
        IF (CAS1) ALP=0.8D0
        IF (CAS2) ALP=0.4D0
        IF (CAS3) ALP=0.0D0
        BETA=1.0D0
        IF (GROS.AND..NOT.CAS1) THEN
          NDT=1
          NSDT=2
        ELSE
          NDT=2
          NSDT=4
        ENDIF
        ENDIF
C
        IF (FISLON) THEN
        IF (GROS) THEN
          NDT=2
          FETIRF=30*RAPL
          FETIRP=60*RAPL
        ELSE
          NDT=3
          FETIRF=15*RAPL
          FETIRP=30*RAPL
        ENDIF
        ENDIF
C
        CALL GETVR8 ( 'FISS_SOUDURE', 'RAYON_TORE'   , 1,1,1, RC0  ,N1)
        IF (FISCOU.AND.N1.EQ.0) THEN
          IF (GROS) THEN
            RC0=0.12D0
          ELSE
            RC0=0.10D0
          ENDIF
        IF (CAS1) RC0=0.08D0
        RC0=RC0*A
        ENDIF
        IF (FISLON.AND.N1.EQ.0) THEN
          RC0=A/(NDT+1)
        ENDIF
C
        CALL GETVR8 ( 'FISS_SOUDURE', 'COEF_MULT_RC1', 1,1,1, RC1  ,N1)
        IF (FISCOU.AND.N1.EQ.0) THEN
          IF (GROS) THEN
            RC1=1.2D0
          ELSE
            RC1=1.D0
          ENDIF
        ENDIF
C
        CALL GETVR8 ( 'FISS_SOUDURE', 'COEF_MULT_RC2', 1,1,1, RC2  ,N1)
        IF (FISCOU.AND.N1.EQ.0) THEN
          IF (GROS) THEN
            RC2=1.4D0
          ELSE
            RC2=1.2D0
          ENDIF
        ENDIF
C
        CALL GETVR8 ( 'FISS_SOUDURE', 'COEF_MULT_RC3', 1,1,1, RC3  ,N1)
        IF (FISCOU.AND.N1.EQ.0) THEN
          IF (GROS) THEN
            IF (CAS1) THEN
              RC3=2.5D0
            ELSE
C           VALEUR NON UTILISEE
              RC3=1.D0
            ENDIF
          ELSE
            IF (CAS3) THEN
              RC3=2.2D0
            ELSE
              RC3=2.0D0
            ENDIF
          ENDIF
        ENDIF
C
        CALL GETVIS ( 'FISS_SOUDURE', 'NB_TRANCHE'   , 1,1,1, NT   ,N1)
        IF (FISCOU.AND.N1.EQ.0) THEN
          IF (GROS) THEN
            NT= 8
          ELSE
            NT=16
          ENDIF
C         CAS 1
          IF (CAS1) NT=NT*2
        ENDIF
        IF (FISLON.AND.N1.EQ.0) THEN
            NT= 0
        ENDIF
C
        CALL GETVIS ( 'FISS_SOUDURE', 'NB_SECTEUR'   , 1,1,1, NS   ,N1)
        IF (FISCOU.AND.N1.EQ.0) THEN
          IF (GROS) THEN
            NS= 2
          ELSE
            NS= 4
          ENDIF
        ENDIF
        IF (FISLON.AND.N1.EQ.0) THEN
          IF (GROS) THEN
            NS=2
          ELSE
            NS=4
          ENDIF
        ENDIF
C
        CALL GETVIS ( 'FISS_SOUDURE', 'NB_COURONNE'  , 1,1,1, NC   ,N1)
        IF (FISCOU.AND.N1.EQ.0) THEN
          IF (GROS) THEN
            NC= 3
          ELSE
            NC= 4
          ENDIF
        ENDIF
        IF (FISLON.AND.N1.EQ.0) THEN
          IF (GROS) THEN
            NC=3
          ELSE
            NC=4
          ENDIF
        ENDIF
C
C
      ENDIF
C
C     --- INFO ---
C
      CALL GETVIS ( ' ', 'INFO' , 1,1,1, INFO  , N1 )
C
C     --- IMPRESSION ---
C
      IMPR = .FALSE.
      CALL GETFAC ( 'IMPRESSION', NIMP )
      IF ( NIMP .NE. 0 ) THEN
         IMPR = .TRUE.
C
         CALL WKVECT('&&OPS016.FORM','V V K16',NIMP,IFORM)
         CALL WKVECT('&&OPS016.ILFO','V V I'  ,NIMP,ILFOR)
         CALL WKVECT('&&OPS016.IVER','V V I'  ,NIMP,IVERS)
         CALL WKVECT('&&OPS016.INIV','V V I'  ,NIMP,INIVG)
         CALL WKVECT('&&OPS016.IFIC','V V K16',NIMP,IFICH)
         CALL WKVECT('&&OPS016.ILFI','V V I'  ,NIMP,ILFIC)
         CALL WKVECT('&&OPS016.IUNI','V V I'  ,NIMP,IUNIT)
         CALL WKVECT('&&OPS016.INF','V V I'  ,NIMP,INF)
C
         DO 120 I=1, NIMP
C
           CALL GETVTX ( 'IMPRESSION', 'FORMAT' , I,1,1, FORMAT, N1 )
           CALL GETLTX ( 'IMPRESSION', 'FORMAT' , I,1,1, LFORMA, N1 )
           CALL GETVIS ( 'IMPRESSION', 'VERSION', I,1,1, VERSIO, N1 )
           CALL GETVIS ( 'IMPRESSION', 'NIVE_GIBI', I,1,1, NIVGIB, N1 )
           CALL GETVTX ( 'IMPRESSION', 'FICHIER', I,1,1, FICHIE, NF )
           CALL GETLTX ( 'IMPRESSION', 'FICHIER', I,1,1, LFICHI, N1 )
           CALL GETVIS ( 'IMPRESSION', 'UNITE'  , I,1,1, UNITEF, N1 )
           ZI(INF+I-1)     = NF
           ZK16(IFORM+I-1) = FORMAT
           ZI(ILFOR+I-1)   = LFORMA
           ZI(IVERS+I-1)   = VERSIO
           ZI(INIVG+I-1)   = NIVGIB
           ZK16(IFICH+I-1) = FICHIE
           ZI(ILFIC+I-1)   = LFICHI
           ZI(IUNIT+I-1)   = UNITEF
C
 120    CONTINUE
C
      ENDIF
C     ------------------------------------------------------------------
C
C     --- DESTRUCTION DE LA COMMANDE COURANTE --
      IERUSR = 0
      CALL SMCDEL(ICMD,0,IERUSR)
      ICMD   = ICMD - 1
C
      CALL REPOUT(1,LRM,REP)
      IF ( LOGIEL(1:6) .EQ. 'GIBI98' ) THEN
        LOGIEL = REP(1:LRM)//'gibi98'
        LLOGIE = LRM+6
      ELSEIF ( LOGIEL(1:8) .EQ. 'GIBI2000' ) THEN
        LOGIEL = REP(1:LRM)//'gibi2000'
        LLOGIE = LRM+8
      ELSE
        CALL UTMESS('F',NOMCMD,'SEULS GIBI98 ET GIBI2000 SONT '//
     +                  'APPELABLES.')
      ENDIF
C
C     --- ECRITURE SUR LE FICHIER .DATG  DE LA PROCEDURE ---
C
      SAUGIB = 'sauvgibi'
      CALL DEFUFI( UNITS , SAUGIB )
C
      DONGIB = 'donngib'
      CALL DEFUFI( UNITD , DONGIB )
C
      IF ( SAIN   ) THEN
         CALL ASPID0 ( UNITD, EPT1, DET1, D1, D2, EPT2, DET2, ZMAX, H,
     +              ALPHA, JEU, EPC, DEC, XMAX, TYPMAI, THETA, TYPELE,
     +              ITYPSO, DPENE, NIVMAG)
      ENDIF
      IF ( FISLON ) THEN
         CALL ASPID1 ( UNITD, EPT1, DET1, D1, D2, EPT2, DET2, ZMAX, H,
     +                 ALPHA, JEU, EPC, DEC, XMAX, TYPMAI,THETA,
     +                 A,C,EPS, RC0,NS,NC,NT,POSI, NDT,FETIRF,FETIRP,
     +                 TFISS,ZETA,ITYPSO,DPENE, NIVMAG)
      ENDIF
      IF ( FISCOU ) THEN
         CALL ASPID2 ( UNITD, EPT1, DET1, D1, D2, EPT2, DET2, ZMAX,
     +                 H, ALPHA, JEU, EPC, DEC, XMAX, TYPMAI,
     +                 THETA, A, C, EPS, RC0, RC1, RC2, RC3,
     +                 ALP,BETA, NS, NC, NT, POSI ,NDT,NSDT,TFISS,
     +                 ZETA,ITYPSO,DPENE, NIVMAG)
      ENDIF
C
      CLOSE( UNIT=UNITD )
C
C     --- COMMANDE EXEC_LOGICIEL ---
C
      CALL CODENT ( UNITD, 'G', K8B1 )
      NOPAR1 = 'fort.'//K8B1
      CALL CODENT ( UNITS, 'G', K8B1 )
      NOPAR2 = 'fort.'//K8B1
      I8 = 8
      CLOSE (UNIT=UNITS)
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, ' ', 'EXEC_LOGICIEL', IERUSR )
        CALL PUTVTX ( 'LOGICIEL', 1, LOGIEL, LLOGIE, IERUSR)
        CALL SMDMCF ( 'ARGUMENT', IERUSR )
          CALL PUTVTX ( 'NOM_PARA', 1, NOPAR1, I8, IERUSR)
        CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'ARGUMENT', IERUSR )
          CALL PUTVTX ( 'NOM_PARA', 1, NOPAR2, I8, IERUSR)
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  PRE_GIBI  ---
C
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, ' ', 'PRE_GIBI', IERUSR )
        CALL PUTVIS ( 'UNITE_GIBI', 1, UNITS, IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  LIRE_MAILLAGE  ---
C
      ICMD = ICMD + 1
      CALL GCNCON ('.', MAPROV)
      CALL SMDCMD ( ICMD, MAPROV, 'LIRE_MAILLAGE', IERUSR )
        CALL PUTVIS ( 'INFO' , 1, INFO , IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  DEFI_GROUP  ---
C
      ICMD = ICMD + 1
      I4 = 4
      CALL SMDCMD ( ICMD, '&'//MAPROV, 'DEFI_GROUP', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
        K8B1 = 'EQUERRE'
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
        IF (SAIN) THEN
           K8B1 = 'S_LAT1'
           CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
           CALL SMFMCF ( IERUSR )
           K8B1 = 'S_LAT2'
           CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
           CALL SMFMCF ( IERUSR )

        ELSE
           K8B1 = 'S_LAT1_C'
           CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
           CALL SMFMCF ( IERUSR )
           K8B1 = 'S_LAT2_C'
           CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
           CALL SMFMCF ( IERUSR )
           K8B1 = 'S_LAT1_T'
           CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
           CALL SMFMCF ( IERUSR )
           K8B1 = 'S_LAT2_T'
           CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
             CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
           CALL SMFMCF ( IERUSR )

           IF ((TFISS.EQ.'NON_DEB').AND.(TYPE.EQ.'LONGUE')) THEN
             K8B1 = 'PFONDINF'
             CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
               CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
             CALL SMFMCF ( IERUSR )
             K8B1 = 'PFONDSUP'
             CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
               CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
             CALL SMFMCF ( IERUSR )
           ELSE
             K8B1 = 'PFONDFIS'
             CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
               CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
             CALL SMFMCF ( IERUSR )
           ENDIF

           IF ((TFISS.EQ.'NON_DEB').AND.(TYPE.EQ.'COURTE')) THEN
             K8B1 = 'FONDFISS'
             NOMMA = 'MAIL_ORI'
             K4B = 'INIT'
             CALL SMDMCF ( 'CREA_GROUP_MA', IERUSR )
               CALL PUTVID ( 'GROUP_MA' , 1,  K8B1, IERUSR )
               CALL PUTVID ( 'NOM'      , 1, NOMMA, IERUSR )
               CALL PUTVTX ( 'POSITION' , 1,   K4B, I4, IERUSR )
             CALL SMFMCF ( IERUSR )
           ENDIF

           IF ((TFISS(1:4).EQ.'DEB_').AND.(AXIS.EQ.'OUI')) THEN
             K8B1 = 'FONDFISS'
             NOMMA = 'MAIL_ORI'
             K4B = 'INIT'
             CALL SMDMCF ( 'CREA_GROUP_MA', IERUSR )
               CALL PUTVID ( 'GROUP_MA' , 1,  K8B1, IERUSR )
               CALL PUTVID ( 'NOM'      , 1, NOMMA, IERUSR )
               CALL PUTVTX ( 'POSITION' , 1,   K4B, I4, IERUSR )
             CALL SMFMCF ( IERUSR )
           ENDIF

           IF ((TFISS.EQ.'NON_DEB').AND.(TYPE.EQ.'LONGUE')) THEN
             K8B1 = 'FOND_SUP'
             NOMMA = 'MA_ORI_S'
             K4B = 'INIT'
             CALL SMDMCF ( 'CREA_GROUP_MA', IERUSR )
               CALL PUTVID ( 'GROUP_MA' , 1,  K8B1, IERUSR )
               CALL PUTVID ( 'NOM'      , 1, NOMMA, IERUSR )
               CALL PUTVTX ( 'POSITION' , 1,   K4B, I4, IERUSR )
             CALL SMFMCF ( IERUSR )
             K8B1 = 'FOND_INF'
             NOMMA = 'MA_ORI_I'
             K4B = 'INIT'
             CALL SMDMCF ( 'CREA_GROUP_MA', IERUSR )
               CALL PUTVID ( 'GROUP_MA' , 1,  K8B1, IERUSR )
               CALL PUTVID ( 'NOM'      , 1, NOMMA, IERUSR )
               CALL PUTVTX ( 'POSITION' , 1,   K4B, I4, IERUSR )
             CALL SMFMCF ( IERUSR )
           ENDIF


        ENDIF
        K8B1 = 'S_FOND1'
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
        K8B1 = 'S_FOND2'
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 1, K8B1, IERUSR )
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  MODI_MAILLAGE  ---
C
      K8B1 = 'EQUERRE'
      I8=8
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//MAPROV, 'MODI_MAILLAGE', IERUSR )
        CALL PUTVID ( 'MAILLAGE'  , 1, MAPROV, IERUSR )
        CALL SMDMCF ( 'EQUE_PIQUA', IERUSR )
          CALL PUTVID ( 'GROUP_NO'  , 1, K8B1 , IERUSR )
          CALL PUTVR8 ( 'E_BASE'    , 1, EPT1 , IERUSR )
          CALL PUTVR8 ( 'DEXT_BASE' , 1, DET1 , IERUSR )
          CALL PUTVR8 ( 'L_BASE'    , 1, D1   , IERUSR )
          CALL PUTVR8 ( 'L_CHANF'   , 1, D2   , IERUSR )
          CALL PUTVTX ( 'TYPE'      , 1, TYPSOU,I8,IERUSR )
          CALL PUTVR8 ( 'H_SOUD'    , 1, H    , IERUSR )
          CALL PUTVR8 ( 'ANGL_SOUD' , 1, ALPHA, IERUSR )
          CALL PUTVR8 ( 'JEU_SOUD'  , 1, JEU  , IERUSR )
          CALL PUTVR8 ( 'E_CORP'    , 1, EPC  , IERUSR )
          CALL PUTVR8 ( 'DEXT_CORP' , 1, DEC  , IERUSR )
          CALL PUTVR8 ( 'AZIMUT   ' , 1, THETA, IERUSR )
          CALL PUTVTX ( 'RAFF_MAIL ', 1, TYPMAI,I8,IERUSR )
          CALL PUTVR8 ( 'X_MAX'     , 1, XMAX , IERUSR )
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE AFFE_MODELE ---
C
      K16B1 = 'MECANIQUE'
C     K16B2 = 'DIS_TR'
      K16B3 = '3D'
      K8B1 = 'OUI'
      I16=16
      ICMD = ICMD + 1
      CALL GCNCON ( '.' , MODELE )
      CALL SMDCMD ( ICMD, MODELE, 'AFFE_MODELE', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
        CALL SMDMCF ( 'AFFE', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 5, GRMAIL, IERUSR )
C         CALL PUTVTX ( 'TOUT     '   , 1, K8B1 , I3 , IERUSR)
          CALL PUTVTX ( 'PHENOMENE'   , 1, K16B1, I16, IERUSR)
          CALL PUTVTX ( 'MODELISATION', 1, K16B3, I16, IERUSR)
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     --- COMMANDE  MODI_MAILLAGE  ---
C
      K8B1 = 'PEAUINT'
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//MAPROV, 'MODI_MAILLAGE', IERUSR )
        CALL PUTVID ( 'MAILLAGE'  , 1, MAPROV, IERUSR )
        CALL PUTVID ( 'MODELE'    , 1, MODELE, IERUSR )
        CALL SMDMCF ( 'ORIE_PEAU_3D', IERUSR )
        IF ( TFISS .EQ. 'DEB_INT' ) THEN
           CALL PUTVID ( 'GROUP_MA'  , 6, GRMA2D , IERUSR )
        ELSE
           CALL PUTVID ( 'GROUP_MA'  , 4, GRMA2D , IERUSR )
        ENDIF
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C     INUTILE DANS LE CAS FISSURE
      IF (SAIN) THEN
C
C     --- COMMANDE  DEFI_GROUP  ---
C
      K8B1 = 'NIDXT'
      K8B2 = 'NEDXT'
      K8B3 = 'NIIXT'
      K8B4 = 'NEIXT'
      OPTION = 'SEGM_DROI_ORDO'
      K8B = 'ABSOLU'
      R8B = EPC / 5.D0
      ICMD = ICMD + 1
      CALL SMDCMD ( ICMD, '&'//MAPROV, 'DEFI_GROUP', IERUSR )
        CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
        CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
          CALL PUTVID ( 'GROUP_MA', 4, GROUMA, IERUSR )
        CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
        DO 10 I = 1 , NBAZIT
          CALL CODENT ( I , 'G' , NUME )
          GRNOID = 'NID'//NUME
          GRNOFD = 'NED'//NUME
          GRNOII = 'NII'//NUME
          GRNOFI = 'NEI'//NUME
          GRNOLD = 'LD'//NUME
          GRNOLI = 'LI'//NUME
          GRNOND = 'LDN'//NUME
          GRNONI = 'LIN'//NUME
          ICMD = ICMD + 1
          CALL SMDCMD ( ICMD, '&'//MAPROV, 'DEFI_GROUP', IERUSR )
          CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'NOM'      , 1, GRNOID, IERUSR )
            CALL PUTVID ( 'GROUP_NO' , 1, K8B1  , IERUSR )
            CALL PUTVIS ( 'NUME_INIT', 1, I     , IERUSR )
            CALL PUTVIS ( 'NUME_FIN' , 1, I     , IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'NOM'      , 1, GRNOFD, IERUSR )
            CALL PUTVID ( 'GROUP_NO' , 1, K8B2  , IERUSR )
            CALL PUTVIS ( 'NUME_INIT', 1, I     , IERUSR )
            CALL PUTVIS ( 'NUME_FIN' , 1, I     , IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'NOM'      , 1, GRNOII, IERUSR )
            CALL PUTVID ( 'GROUP_NO' , 1, K8B3  , IERUSR )
            CALL PUTVIS ( 'NUME_INIT', 1, I     , IERUSR )
            CALL PUTVIS ( 'NUME_FIN' , 1, I     , IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'NOM'      , 1, GRNOFI, IERUSR )
            CALL PUTVID ( 'GROUP_NO' , 1, K8B4  , IERUSR )
            CALL PUTVIS ( 'NUME_INIT', 1, I     , IERUSR )
            CALL PUTVIS ( 'NUME_FIN' , 1, I     , IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'NOM'           , 1, GRNOND,      IERUSR )
            CALL PUTVID ( 'GROUP_MA'      , 1, GRNOLD,      IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'NOM'           , 1, GRNOLD,      IERUSR )
            CALL PUTVID ( 'GROUP_NO'      , 1, GRNOND,      IERUSR )
            CALL PUTVTX ( 'OPTION'        , 1, OPTION, I16, IERUSR )
            CALL PUTVR8 ( 'PRECISION'     , 1, R8B,         IERUSR )
            CALL PUTVTX ( 'CRITERE'       , 1, K8B,     I8, IERUSR )
            CALL PUTVID ( 'GROUP_NO_ORIG' , 1, GRNOID ,     IERUSR )
            CALL PUTVID ( 'GROUP_NO_EXTR' , 1, GRNOFD ,     IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'NOM'           , 1, GRNONI,      IERUSR )
            CALL PUTVID ( 'GROUP_MA'      , 1, GRNOLI,      IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMDMCF ( 'CREA_GROUP_NO', IERUSR )
            CALL PUTVID ( 'NOM'           , 1, GRNOLI,      IERUSR )
            CALL PUTVID ( 'GROUP_NO'      , 1, GRNONI,      IERUSR )
            CALL PUTVTX ( 'OPTION'        , 1, OPTION, I16, IERUSR )
            CALL PUTVR8 ( 'PRECISION'     , 1, R8B,         IERUSR )
            CALL PUTVTX ( 'CRITERE'       , 1, K8B,     I8, IERUSR )
            CALL PUTVID ( 'GROUP_NO_ORIG' , 1, GRNOII ,     IERUSR )
            CALL PUTVID ( 'GROUP_NO_EXTR' , 1, GRNOFI ,     IERUSR )
          CALL SMFMCF ( IERUSR )
          CALL SMFCMD ( IERUSR )
 10     CONTINUE
      ENDIF
C
C     --- COMMANDE CREA_MAILLAGE ---
C
       NOCO1  = 'P1_CORP '
       NOCO2  = 'P2_CORP '
       NOTU   = 'P_TUBU  '
C
       ICMD = ICMD + 1
       CALL SMDCMD (ICMD, NOMRES, 'CREA_MAILLAGE', IERUSR)
        CALL PUTVID ( 'MAILLAGE', 1, MAPROV, IERUSR )
        CALL SMDMCF ( 'CREA_POI1', IERUSR )
         CALL PUTVID ( 'NOM_GROUP_MA', 1, NOCO1, IERUSR )
         CALL PUTVID ( 'GROUP_NO'    , 1, NOCO1, IERUSR )
       CALL SMFMCF ( IERUSR )
        CALL SMDMCF ( 'CREA_POI1', IERUSR )
        CALL PUTVID ( 'NOM_GROUP_MA', 1, NOCO2, IERUSR )
        CALL PUTVID ( 'GROUP_NO'    , 1, NOCO2, IERUSR )
       CALL SMFMCF ( IERUSR )
       CALL SMDMCF ( 'CREA_POI1', IERUSR )
        CALL PUTVID ( 'NOM_GROUP_MA', 1, NOTU, IERUSR )
        CALL PUTVID ( 'GROUP_NO'    , 1, NOTU, IERUSR )
       CALL SMFMCF ( IERUSR )
      CALL SMFCMD ( IERUSR )
C
C
C
C     --- COMMANDE  DEFUFI, IMPR_RESU  ---
C
      IF ( IMPR ) THEN
C
       DO 130 I=1, NIMP
C
         IF ( ZI(INF+I-1) .NE. 0 ) THEN
            ICMD = ICMD + 1
            CALL SMDCMD ( ICMD, ' ', 'DEFUFI', IERUSR )
              CALL SMDMCF ( 'IMPRESSION', IERUSR )
                CALL PUTVTX ( 'NOM'   , 1, ZK16(IFICH+I-1) ,
     +                        ZI(ILFIC+I-1), IERUSR)
                CALL PUTVIS ( 'UNITE' , 1, ZI(IUNIT+I-1), IERUSR )
              CALL SMFMCF ( IERUSR )
            CALL SMFCMD ( IERUSR )
         ENDIF
C
         ICMD = ICMD + 1
         CALL SMDCMD ( ICMD, ' ', 'IMPR_RESU', IERUSR )
           CALL SMDMCF ( 'RESU', IERUSR )
             CALL PUTVID ( 'MAILLAGE', 1, NOMRES, IERUSR )
             CALL PUTVTX ( 'FORMAT'  , 1, ZK16(IFORM+I-1),
     +                     ZI(ILFOR+I-1) , IERUSR)
             IF (ZK16(IFORM+I-1)(1:5).EQ.'IDEAS') THEN
               CALL PUTVIS ( 'VERSION' , 1, ZI(IVERS+I-1), IERUSR )
             ELSEIF (ZK16(IFORM+I-1)(1:6).EQ.'CASTEM') THEN
               CALL PUTVIS ( 'NIVE_GIBI' , 1, ZI(INIVG+I-1), IERUSR )
             ENDIF
             IF ( ZI(INF+I-1) .NE. 0 ) THEN
               CALL PUTVTX ( 'FICHIER'  , 1, ZK16(IFICH+I-1) ,
     +                       ZI(ILFIC+I-1), IERUSR)
             ENDIF
           CALL SMFMCF ( IERUSR )
         CALL SMFCMD ( IERUSR )
C
 130   CONTINUE
C
      ENDIF
C
C     ---------------------------------------------------------------
C
 9998 CONTINUE
      IF ( IERUSR .GT. 0 ) THEN
         CALL UTMESS('E',NOMCMD,'ERREURS CONSTATEES DANS LA MACRO')
         IER = IER + IERUSR
      ENDIF
C
 9999 CONTINUE
      CALL JEDETC('V','&&OPS016',1)
      CALL JEDEMA()
      END
