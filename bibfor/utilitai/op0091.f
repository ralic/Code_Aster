      SUBROUTINE OP0091 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
C TOLE CRP_20
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
C RESPONSABLE MCOURTOI M.COURTOIS
C     MODULE "MEDISIS" :  OPERATIONS SUR DES FONCTIONS
C     ------------------------------------------------------------------
C     OPERATIONS  DISPONIBLES       SUR FONCTION    NAPPE
C       -  SPECTRE D'OSCILLATEUR        OUI         NON
C       -  DERIVATION                   OUI         NON
C       -  INTEGRATION                  OUI         NON
C       -  ECART_TYPE                   OUI         NON
C       -  MAXIMUM D'UNE FONCTION       OUI         OUI
C       -  COMBINAISON LINEAIRE         OUI         OUI
C       -  COMBINAISON COMPLEXE         OUI         NON
C       -  EXTRACTION                   OUI         NON
C       -  RECHERCHE D'ENVELOPPE        OUI         OUI
C       -  RECHERCHE DE PICS            OUI         OUI
C       -  COMPOSITION                  OUI         NON
C       -  ASSE                         OUI         NON
C       -  FFT                          OUI         NON
C       -  INDICES NOCIVITE D'UN SEISME OUI         OUI
C       -  CORRECTION DERIVE ACCELERO   OUI         NON
C       -  LISSAGE D'ENVELOPPE SPECTRE  OUI         OUI
C       -  INVERSE                      OUI         NON
C       -  FONCTION VALEUR ABSOLUE      OUI         NON
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      REAL*8         COEF, RBID, NORME, MOY
      CHARACTER*8    K8B, LISTR, NATURE, NATURF, LPARA
      CHARACTER*16   CONCEP, NOMCMD, NOMOPE, METHOD, CRITER,PARTIE
      CHARACTER*19   SORTIE, NOMFON, LISTFR
      CHARACTER*24   NOMTEM(5), PROL, VALE
      INTEGER        LF, L, K, NBAMOR, LAMOR, NBFREQ, LFREQ, LNOMF
      INTEGER        I,NBFON,NBVAL,LTINI,LTFIN,LFINI,LFFIN,LCOEF
      INTEGER        L1, L2, NBMAX, NBPTS, LFON
      CHARACTER*19   NOMFO1, NOMFO2,NOMFI
      INTEGER        N1,  N2,  N3,  N4,  N5,  N6 , N7,  N8,  N9,  N10,
     +               N11, N12, N13, N14, N15, N16, N17, N18, N19, N20
      INTEGER        IOCC, IBID
      INTEGER        NBPRMS, NBPMAX, NBPETY, IFM,NIV,EXPO1
      PARAMETER    ( NBPRMS=5, NBPMAX=2, NBPETY=6 )
      REAL*8         TINI, TFIN, BINF, BSUP, FINI, FFIN, EPSI, RMS,
     +               VALRMS(3),VALINF, VALSUP,VALTYP(4)
      COMPLEX*16     C16B
      CHARACTER*1    BASE, K1BID
      CHARACTER*8    CRIT, SURCHG, NOMRES
      CHARACTER*19   NOMRMS(2),NOMTYP(2)
      CHARACTER*16   NOPRMS(NBPRMS), NOPMAX(NBPMAX), NOPETY(NBPETY),
     +               PROLGD, INTERP
      CHARACTER*8    TYPRMS(NBPRMS), TYPMAX(NBPMAX), TYPETY(NBPETY)
C
      DATA NOPRMS/'FONCTION','METHODE','INST_INIT','INST_FIN' , 'RMS'/
      DATA TYPRMS/'K8'      ,'K8'     ,'R'        ,'R'        , 'R'  /
      DATA NOPMAX/'FONCTION','MAXI'/
      DATA TYPMAX/'K8'      ,'R' /
      DATA NOPETY/'FONCTION','METHODE','INST_INIT','INST_FIN' ,
     +'ECART_TYPE','MOYENNE'/
      DATA TYPETY/'K8'      ,'K8'     ,'R'        ,'R'        ,'R','R'/
C
      DATA     NOMTEM/'&&OP0091.TEMPORAIRE1', '&&OP0091.TEMPORAIRE2',
     .                '&&OP0091.TEMPORAIRE3', '&&OP0091.TEMPORAIRE4',
     .                '&&OP0091.TEMPORAIRE5'/
C     ------------------------------------------------------------------
      CALL JEMARQ()      
C     
      IER= 0
      INTERP = 'LIN LIN '
      PROLGD = 'EE      '
      BASE   = 'G'
C
      CALL GETRES ( SORTIE, CONCEP, NOMCMD )
C
C
C --- RECUPERATION DU NIVEAU D'IMPRESSION
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
C     --- NOM DU MOT CLE FACTEUR ---
      CALL GETFAC ( 'ENVELOPPE'    , N1 )
      CALL GETFAC ( 'SPEC_OSCI'    , N2 )
      CALL GETFAC ( 'DERIVE'       , N3 )
      CALL GETFAC ( 'INTEGRE'      , N4 )
      CALL GETFAC ( 'MAX'          , N5 )
      CALL GETFAC ( 'COMB'         , N6 )
      CALL GETFAC ( 'RMS'          , N7 )
      CALL GETFAC ( 'EXTRACTION'   , N8 )
      CALL GETFAC ( 'COMB_C'       , N9 )
      CALL GETFAC ( 'COMPOSE'      , N10)
      CALL GETFAC ( 'ASSE'         , N11)
      CALL GETFAC ( 'FFT'          , N12)
      CALL GETFAC ( 'NOCI_SEISME'  , N13)
      CALL GETFAC ( 'CORR_ACCE'    , N14)
      CALL GETFAC ( 'NORME'        , N15)
      CALL GETFAC ( 'PUISSANCE'    , N16)
      CALL GETFAC ( 'LISS_ENVELOP' , N17)
      CALL GETFAC ( 'INVERSE'      , N18)
      CALL GETFAC ( 'ECART_TYPE'   , N19)
      CALL GETFAC ( 'ABS'          , N20)
      IF (N1.GT. 0) NOMOPE = 'ENVELOPPE       '
      IF (N2.GT. 0) NOMOPE = 'SPEC_OSCI       '
      IF (N3.GT. 0) NOMOPE = 'DERIVE          '
      IF (N4.GT. 0) NOMOPE = 'INTEGRE         '
      IF (N5.GT. 0) NOMOPE = 'MAX             '
      IF (N6.GT. 0) NOMOPE = 'COMB            '
      IF (N7.GT. 0) NOMOPE = 'RMS             '
      IF (N8.GT. 0) NOMOPE = 'EXTRACTION      '   
      IF (N9.GT. 0) NOMOPE = 'COMB_C          '   
      IF (N10.GT.0) NOMOPE = 'COMPOSE         '   
      IF (N11.GT.0) NOMOPE = 'ASSE            '   
      IF (N12.GT.0) NOMOPE = 'FFT             '   
      IF (N13.GT.0) NOMOPE = 'NOCI_SEISME     ' 
      IF (N14.GT.0) NOMOPE = 'CORR_ACCE       ' 
      IF (N15.GT.0) NOMOPE = 'NORME           '
      IF (N16.GT.0) NOMOPE = 'PUISSANCE       '
      IF (N17.GT.0) NOMOPE = 'LISS_ENVELOP    ' 
      IF (N18.GT.0) NOMOPE = 'INVERSE         ' 
      IF (N19.GT.0) NOMOPE = 'ECART_TYPE      ' 
      IF (N20.GT.0) NOMOPE = 'ABS             ' 
C
C     ------------------------------------------------------------------
C     ------------------- EXECUTION DE L'OPERATION DEMANDEE -----------
C     ------------------------------------------------------------------
C
      IF( NOMOPE .EQ. 'SPEC_OSCI' ) THEN
C
C        --- SPECTRE D'OSCILLATEUR D'UN SIGNAL TEMPOREL ---
C
C        --- FONCTION A TRANSFORMER ---
         CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFON,L)
         CALL FOVECA(NOMOPE,NOMFON,IER)
         IF (IER .NE. 0 ) GOTO 9999
C
C        --- RECUPERATION DE LA LISTE D'AMORTISSEMENT ---
         CALL GETVR8(NOMOPE,'AMOR_REDUIT',1,1,0,RBID,L1)
         L = L1
         IF ( L .EQ. 0 ) THEN
            CALL FOC2DF('AMOR','&&OP0091.LISTE.AMOR',1,NBAMOR)
            CALL JEVEUO('&&OP0091.LISTE.AMOR','E',LAMOR)
         ELSE
            NBAMOR = -L
            CALL WKVECT('&&OP0091.LISTE.AMOR','V V R',NBAMOR,LAMOR)
            CALL GETVR8(NOMOPE,'AMOR_REDUIT',1,1,NBAMOR,ZR(LAMOR),L)
         ENDIF
C
C        --- RECUPERATION DE LA LISTE DE FREQUENCES ---
         CALL GETVR8(NOMOPE,'FREQ',1,1,0,RBID,L)
         IF ( L .EQ. 0 ) THEN
            CALL GETVID(NOMOPE,'LIST_FREQ',1,1,1,LISTFR,LF)
            IF ( LF .EQ. 0 ) THEN
               CALL FOC2DF('FREQ','&&OP0091.LISTE.FREQ',1,NBFREQ)
               CALL JEVEUO('&&OP0091.LISTE.FREQ','E',LFREQ)
            ELSE
               CALL JEVEUO(LISTFR//'.VALE','L',LFREQ)
               CALL JELIRA(LISTFR//'.VALE','LONUTI',NBFREQ,K8B)
            ENDIF
         ELSE
            NBFREQ = -L
            CALL WKVECT('&&OP0091.LISTE.FREQ','V V R',NBFREQ,LFREQ)
            CALL GETVR8(NOMOPE,'FREQ',1,1,NBFREQ,ZR(LFREQ),L)
         ENDIF
C
C        --- NORME DU SPECTRE D'OSCILLATEUR ----
         CALL GETVR8(NOMOPE,'NORME',1,1,1,NORME,L)
C
C        --- NATURE DU SPECTRE D'OSCILLATEUR ----
         CALL GETVTX(NOMOPE,'NATURE'     ,1,1,1,NATURE,L)
         CALL GETVTX(NOMOPE,'NATURE_FONC',1,1,1,NATURF,L)
C
C        --- CALCUL DU SPECTRE D'OSCILLATEUR ---
         CALL GETVTX(NOMOPE,'METHODE',1,1,1,METHOD,L)
         CALL FOCASO(NOMFON,METHOD,NBAMOR,ZR(LAMOR),NBFREQ,ZR(LFREQ),
     +                      NORME,NATURE,NATURF,SORTIE,BASE)
C
C        --- DESTRUCTION DES TABLEAUX DE TRAVAIL ---
         CALL JEDETR('&&OP0091.LISTE.AMOR')
         CALL JEDETR('&&OP0091.LISTE.FREQ')
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'CORR_ACCE' ) THEN
C
C        --- CORRECTION DERIVE ACCELEROGRAMME
C
C        --- FONCTION A TRANSFORMER ---
         CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFON,L)
         CALL FOVECA(NOMOPE,NOMFON,IER)
         IF (IER .NE. 0 ) GOTO 9999
C
C        --- CORRECTION ET STOCKAGE ---
         CALL GETVTX(NOMOPE,'CORR_DEPL',1,1,1,METHOD,L)
         CALL FOCACA(METHOD,NOMFON,SORTIE,BASE)
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'DERIVE' ) THEN
C
C        --- DERIVATION D'UN SIGNAL TEMPOREL ---
C
C        --- FONCTION A TRANSFORMER ---
         CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFON,L)
         CALL FOVECA(NOMOPE,NOMFON,IER)
         IF (IER .NE. 0 ) GOTO 9999
C
C        --- DERIVATION ET STOCKAGE ---
         CALL GETVTX(NOMOPE,'METHODE',1,1,1,METHOD,L)
         CALL FOCADE(METHOD,NOMFON,SORTIE,BASE)
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'INTEGRE' ) THEN
C
C        --- INTEGRATION D'UN SIGNAL TEMPOREL ---
C
C        --- FONCTION A INTEGRER ---
         CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFON,L)
         CALL FOVECA(NOMOPE,NOMFON,IER)
         IF (IER .NE. 0 ) GOTO 9999
C
C        --- INTEGRATION ---
         CALL GETVTX(NOMOPE,'METHODE',1,1,1,METHOD,L)
         CALL GETVR8(NOMOPE,'COEF',1,1,1,COEF,L)

         IF (METHOD.EQ.'SIMPSON') THEN
          WRITE(IFM,'(1X,A)') 'INTEGRATION D"ORDRE 2 (METHODE SIMPSON)'
           WRITE(IFM,'(1X,A,G13.6)') 'CONSTANTE D''INTEGRATION :',COEF
           CALL UTMESS('A',METHOD,'METHODE D''INTEGRATION DE SIMPSON'
     &     //' PEUT PROVOQUER DES OSCILLATIONS SI LA COURBE A '
     &     //'INTEGRER N''EST PAS ASSEZ DISCRETISEE OU REGULIERE. '
     &     //' FAIRE ATTENTION AVEC LES ACCELEROGRAMMES.')
         ELSE IF ( METHOD .EQ. 'TRAPEZE' .OR. METHOD .EQ. '  ' ) THEN
           WRITE(IFM,'(1X,A)') 'INTEGRATION D"ORDRE 1 (METHODE TRAPEZE)'
           WRITE(IFM,'(1X,A,G13.6)') 'CONSTANTE D''INTEGRATION :',COEF
         ELSE
           CALL UTMESS('F',METHOD,'METHODE D''INTEGRATION INEXISTANTE.')
         ENDIF

         CALL FOCAIN(METHOD,NOMFON,COEF,SORTIE,BASE)
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'INVERSE    ' ) THEN
C
C        --- CALCUL DE L INVERSE D'UNE FONCTION ---
C
         CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFO1,L1)
         CALL FOCINV(NOMFO1,SORTIE,BASE)
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'ECART_TYPE' ) THEN
C
C        --- CALCUL L'ECART_TYPE D'UNE FONCTION ---
         CALL TBCRSD ( SORTIE , BASE )
         CALL TBAJPA ( SORTIE, NBPETY, NOPETY, TYPETY )
C
         DO 60 IOCC = 1 , N19
            CALL GETVID ( NOMOPE, 'FONCTION', IOCC,1,1, NOMFON, L )

C           --- CALCUL DE LA MOYENNE ---           
            PROL(1:19)  = NOMFON
            PROL(20:24) = '.PROL'
            CALL JEVEUO ( PROL, 'L', LPRO )
C
            IF ( ZK16(LPRO) .EQ. 'FONCTION' ) THEN
               VALE(20:24) = '.VALE'
               VALE(1:19) = NOMFON
               CALL JELIRA ( VALE, 'LONUTI', NBVAL, K8B )
               CALL JEVEUO ( VALE, 'L', LVAR )
               NBPTS = NBVAL / 2
               LFON  = LVAR + NBPTS

               MOY=0.D0
               DO 100 I = 1, NBPTS-1
                  MOY = MOY + (ZR(LVAR+I)-ZR(LVAR+I-1))
     &                   * (ZR(LFON+I)+ZR(LFON+I-1))*0.5D0
 100           CONTINUE
               MOY=MOY/(ZR(LVAR+NBPTS-1)-ZR(LVAR))

C              ---L'INSTANT INITIAL ET L'INSTANT FINAL ---
               CALL GETVR8(NOMOPE,'INST_INIT', IOCC,1,1, TINI,  LTINI)
               CALL GETVR8(NOMOPE,'INST_FIN',  IOCC,1,1, TFIN,  LTFIN)
               CALL GETVTX(NOMOPE,'METHODE',   IOCC,1,1, METHOD,L)
               CALL GETVTX(NOMOPE,'CRITERE',   IOCC,1,1, CRIT,  L)
               CALL GETVR8(NOMOPE,'PRECISION', IOCC,1,1, EPSI,  L)
C
C              --- CALCUL DE LA MOYENNE QUADRATIQUE ---
               CALL FOCRMS(METHOD,NOMFON,CRIT,EPSI,TINI,LTINI,
     &                                              TFIN,LTFIN,MOY,SIG)
C
               VALTYP(1) = TINI
               VALTYP(2) = TFIN
               VALTYP(3) = SIG
               VALTYP(4) = MOY
               NOMTYP(1) = NOMFON
               NOMTYP(2) = METHOD
               CALL TBAJLI(SORTIE,NBPETY,NOPETY,IBID,VALTYP,C16B,
     &            NOMTYP,0)
            ELSEIF ( ZK16(LPRO) .EQ. 'NAPPE' ) THEN
               CALL UTMESS('F','FOCETY',ZK16(LPRO)//' NON DISPONIBLE.')
C
            ELSE
               CALL UTMESS('F','FOCETY',ZK16(LPRO)//
     &                  ' SOUS TYPE INCONNU DE FONCTION.')
            ENDIF
  60     CONTINUE 
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'RMS' ) THEN
         CALL TBCRSD ( SORTIE , BASE )
         CALL TBAJPA ( SORTIE, NBPRMS, NOPRMS, TYPRMS )
C
C        --- MOYENNE QUADRATIQUE D'UN SIGNAL TEMPOREL ---
C
         DO 70 IOCC = 1 , N7
C           --- FONCTION A MOYENNER ---
            CALL GETVID(NOMOPE,'FONCTION',IOCC,1,1,NOMFON,L)
C
C           ---L'INSTANT INITIAL ET L'INSTANT FINAL ---
            CALL GETVR8(NOMOPE,'INST_INIT', IOCC,1,1, TINI,  LTINI)
            CALL GETVR8(NOMOPE,'INST_FIN',  IOCC,1,1, TFIN,  LTFIN)
            CALL GETVTX(NOMOPE,'METHODE',   IOCC,1,1, METHOD,L)
            CALL GETVTX(NOMOPE,'CRITERE',   IOCC,1,1, CRIT,  L)
            CALL GETVR8(NOMOPE,'PRECISION', IOCC,1,1, EPSI,  L)
C
C           --- CALCUL DE LA MOYENNE QUADRATIQUE ---           
            CALL FOCRMS(METHOD,NOMFON,CRIT,EPSI,TINI,LTINI,
     &                                           TFIN,LTFIN,0.D0,RMS)
C
            VALRMS(1) = TINI
            VALRMS(2) = TFIN  
            VALRMS(3) = RMS        
            NOMRMS(1) = NOMFON
            NOMRMS(2) = METHOD
            CALL TBAJLI(SORTIE,NBPRMS,NOPRMS,IBID,VALRMS,C16B,NOMRMS,0)
  70     CONTINUE 
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'NOCI_SEISME' ) THEN
C
        CALL FONOCI(NOMOPE,SORTIE,BASE,IER)
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'LISS_ENVELOP' ) THEN
C
        CALL FOLIEN ( NOMOPE, SORTIE, 'G', IER )
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'FFT' ) THEN

C        --- TRANSFORMEE DE FOURIER DIRECTE OU INVERSE D'UN SIGNAL

C        --- FONCTION A TRANSFORMER ---
         CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFON,L)
C        --- SENS DE LA TRANSFORMATION
         CALL JEVEUO(NOMFON//'.PROL','L',LPRO)
         IF (ZK16(LPRO+2).EQ.'INST') THEN
            NSENS = 1
         ELSEIF(ZK16(LPRO+2).EQ.'FREQ') THEN
            NSENS = -1
         ENDIF
         CALL FONFFT(NSENS,NOMFON,SORTIE,'G')
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'MAX' ) THEN
         CALL TBCRSD ( SORTIE , BASE )
         CALL TBAJPA ( SORTIE, NBPMAX, NOPMAX, TYPMAX )
C
C        --- CALCUL DES MAX D'UNE FONCTION ---
C
         DO 50 IOCC = 1 , N5
            CALL GETVID ( NOMOPE, 'FONCTION', IOCC,1,1, NOMFON, L )
            CALL FOCAMA ( SORTIE , NOMFON )
  50     CONTINUE 
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'ABS' ) THEN
C
C        --- CALCUL DE LA VALEUR ABSOLUE D'UNE FONCTION ---
C
         CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFO1,L1)
         CALL FOCABS(NOMFO1,SORTIE,BASE)
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'COMB' ) THEN
C
C        --- COMBINAISON LINEAIRE DE FONCTIONS ---
C
C        --- RECUPERATION DES ARGUMENTS ---
         CALL WKVECT(NOMTEM(1),'V V K8',N6,LNOMF)
         CALL WKVECT(NOMTEM(2),'V V R' ,N6,LCOEF)
         CALL WKVECT(NOMTEM(3),'V V L' ,N6,LCCPLX)
         CALL WKVECT(NOMTEM(4),'V V L' ,N6,LFCPLX)
         CALL WKVECT(NOMTEM(5),'V V C' ,N6,LCOEFZ)
         LPARA = '        '
         CALL GETVID(' ','LIST_PARA',1,1,1,LPARA,L)
         DO 610 I = 0, N6-1
            CALL GETVID(NOMOPE,'FONCTION' ,I+1,1,1,ZK8(LNOMF+I),L)
            CALL GETVR8(NOMOPE,'COEF    ' ,I+1,1,1,ZR (LCOEF+I) ,LCOR)
            ZL(LCCPLX+I)=.FALSE.
 610     CONTINUE  
         CALL FOCASU ( N6, ZK8(LNOMF), ZR(LCOEF), ZC(LCOEFZ), 
     .   ZL(LCCPLX),ZL(LFCPLX), LPARA, SORTIE, BASE,'FONCTION')
         CALL JEDETR ( NOMTEM(1) )
         CALL JEDETR ( NOMTEM(2) )
         CALL JEDETR ( NOMTEM(3) )
         CALL JEDETR ( NOMTEM(4) )
         CALL JEDETR ( NOMTEM(5) )
C
      ELSEIF( NOMOPE .EQ. 'COMB_C' ) THEN
C
C        --- COMBINAISON LINEAIRE DE FONCTIONS ---
C
C        --- RECUPERATION DES ARGUMENTS ---
         CALL WKVECT(NOMTEM(1),'V V K8',N9,LNOMF)
         CALL WKVECT(NOMTEM(2),'V V R' ,N9,LCOEF)
         CALL WKVECT(NOMTEM(3),'V V L' ,N9,LCCPLX)
         CALL WKVECT(NOMTEM(4),'V V L' ,N9,LFCPLX)
         CALL WKVECT(NOMTEM(5),'V V C' ,N9,LCOEFZ)
         LPARA = '        '
         CALL GETVID(' ','LIST_PARA',1,1,1,LPARA,L)
         DO 620 I = 0, N9-1
            CALL GETVID(NOMOPE,'FONCTION' ,I+1,1,1,ZK8(LNOMF+I),L)
            CALL GETVR8(NOMOPE,'COEF_R  ' ,I+1,1,1,ZR (LCOEF+I) ,LCOR)
            CALL GETVC8(NOMOPE,'COEF_C  ' ,I+1,1,1,ZC (LCOEFZ+I),LCOC)
C        ON SAIT QUE (LCOR,LCOC) VAUT (0,1) OU (1,0)
            ZL(LCCPLX+I)=LCOR.EQ.0
 620     CONTINUE
         CALL FOCASU ( N9, ZK8(LNOMF), ZR(LCOEF), ZC(LCOEFZ), 
     .   ZL(LCCPLX),ZL(LFCPLX), LPARA, SORTIE, BASE,'FONCT_C ')
         CALL JEDETR ( NOMTEM(1) )
         CALL JEDETR ( NOMTEM(2) )
         CALL JEDETR ( NOMTEM(3) )
         CALL JEDETR ( NOMTEM(4) )
         CALL JEDETR ( NOMTEM(5) )
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'ENVELOPPE' )THEN
C
C        --- RECHERCHE DE L'ENVELOPPE D'UNE FONCTION ---
C
         CALL GETVID(NOMOPE,'FONCTION',1,1,0,NOMFON,L)
         NBFON = -L
         CALL WKVECT(NOMTEM(1),'V V K8',NBFON,LNOMF)
         CALL GETVID(NOMOPE,'FONCTION',1,1,NBFON,ZK8(LNOMF),L)
         CALL GETVTX(NOMOPE,'CRITERE',1,1,1,CRITER,L)
         CALL FOCAEN( NBFON , ZK8(LNOMF) , CRITER , SORTIE, BASE )
         CALL JEDETR(NOMTEM(1))
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'EXTRACTION' ) THEN
C
C        --- EXTRACTION D'UNE PARTIE D'UNE FONCTION COMPLEXE
C
C        --- FONCTION A TRAITER ---
         CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFON,L)
C
C        --- INTEGRATION ---
         CALL GETVTX(NOMOPE,'PARTIE',1,1,1,PARTIE,L)
         CALL FOCAEX(NOMFON,PARTIE,SORTIE,BASE)
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'COMPOSE' ) THEN
C
C        --- COMPOSITION DE DEUX FONCTIONS ---
C      
       CALL GETVID(NOMOPE,'FONC_RESU',1,1,1,NOMFO1,L1)
       CALL GETVID(NOMOPE,'FONC_PARA',1,1,1,NOMFO2,L2)
       CALL FOCACO(NOMFO1,NOMFO2,SORTIE,BASE)
C
C     ------------------------------------------------------------------
      ELSEIF( NOMOPE .EQ. 'ASSE' ) THEN
C
C        --- CONCATENATION DE FONCTIONS ---
C      
         CALL GETVID ( NOMOPE, 'FONCTION',1,1,0, NOMFON, L )
         NBFON = -L
         CALL WKVECT ( NOMTEM(1), 'V V K8', NBFON, LNOMF )
         CALL GETVID ( NOMOPE, 'FONCTION', 1,1,NBFON, ZK8(LNOMF), L )
         CALL GETVTX ( NOMOPE, 'SURCHARGE',   1,1,1, SURCHG, L )
         CALL FOCAAS ( NBFON, ZK8(LNOMF), SURCHG, INTERP, PROLGD, BASE,
     +                                                    SORTIE )
         CALL JEDETR ( NOMTEM(1) )
C
      ELSEIF( NOMOPE .EQ. 'PUISSANCE' ) THEN
C
C        --- PUISSANCE D'UNE FONCTION  ---
C
         CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFO1,L1)
         CALL GETVIS(NOMOPE,'EXPOSANT',1,1,1,EXPO1,L1)
         CALL FOCCAR(NOMFO1,EXPO1,SORTIE,BASE)
C
      ELSEIF( NOMOPE .EQ. 'NORME      ' ) THEN
C
C        --- CALCUL DE NORMES FONCTIONNELLES ---
C
         CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFO1,L1)
         CALL FOCNOR(NOMFO1,SORTIE,BASE)
      ENDIF
C
C     ------------------------------------------------------------------
C
      CALL TITRE
C
C     NORME RENVOIE UNE FONCTION POUR ON NE PASSE PAS ?
C     ON NE LE FAIT PAS POUR LES TABL_FONC
      IF( NOMOPE .NE. 'RMS             ' .AND.
     +    NOMOPE .NE. 'NOCI_SEISME     ' .AND.
     +    NOMOPE .NE. 'MAX             ' .AND.
     +    NOMOPE .NE. 'ECART_TYPE      ' .AND.
     +    NOMOPE .NE. 'PUISSANCE       ' .AND.
     +    NOMOPE .NE. 'NORME           ' ) THEN
         CALL FOATTR(' ',1,SORTIE)
         IF (NIV.GT.1) CALL FOIMPR(SORTIE,NIV,IFM,0,LISTR)
C
C     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
C         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
         CALL ORDONN(SORTIE,NOMCMD,0)
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
