        SUBROUTINE MDITM2(NP2,NP3,NP4,N2,NBM,NBMCD,NBMP,NBNL,INDIC,IMPR,
     &ITRANS,EPST,ICOUPL,TPFL,VECI1,LOCFL0,DT0,TFEXM,TS,DTTR,VECDT,
     &IARCH,VITG0,DEPG0,MASGI,AMORI,PULSI,PHII,VECR5,
     &VECR3,VECR1,VECR2,VGAP,VECR4,XSI0,NBSAUV,INDX,INDXF,INTGE1,INTGE2,
     &ICONFB,TCONF1,TCONF2,TCONFE,TYPCH,NBSEG,OLDIA,ITFORN,AMOR,AMOR0,
     &AMOR00,PULS,PULS0,PULS00,TRANS,PULSD,FMOD0,FMOD00,FMODT,
     &FMOD0T,
     &FEXMOD,FNLMOD,FMODA,FMRES,DEPG,DEPGE,DEPGC,DEPGT,DEPG0T,
     &VITG,VITGE,VITGC,VITGT,VITG0T,ACCG,ACCG0,ACCGT,DEP,VIT,ACC,DEP0,
     &VIT0,ACC0,KMOD,CMOD,KMOD0,CMOD0,KMOD00,CMOD00,KMODCA,CMODCA,
     &CMODFA,AMFLU0,AMFLUC,VG,VD,TTR,VG0,VD0,VVG,RR,RI,RR0,MTMP1,
     &MTMP2,MTMP6,FTMP,DD,U,W,
     &OMEGAF,AA,BB,FEXT,FEXTTS,TEXT,TEXTTS,FEXTTR,FEXTT0,
     &NOMCH,CHOC,ORIG,RC,THETA,ALPHA,BETA,GAMMA,OLD,LOCFLC,LOC,
     &S0,Z0,SR0,ZA1,ZA2,ZA3,ZA4,ZA5,ZIN,ZITR,NBCHOC,PARCHO,NOECHO)
C
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/02/2006   AUTEUR DURAND C.DURAND 
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
C TOLE  CRP_21
C TOLE  CRP_20
C-----------------------------------------------------------------------
C DESCRIPTION : CALCUL DE LA REPONSE DYNAMIQUE NON-LINEAIRE D'UNE
C -----------   STRUCTURE PAR UNE METHODE INTEGRALE
C               (VERSION MULTI-MODALE)
C
C               APPELANT : MDITM1
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C COMMUNS NORMALISES JEVEUX
C -------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C
C ARGUMENTS
C ---------
      INTEGER       NP2, NP3, NP4, N2, NBM, NBMCD, NBMP, NBNL, INDIC,
     &              IMPR, ITRANS,IBID
      REAL*8        EPST,PARCHO(NBNL,*)
      INTEGER       ICOUPL
      CHARACTER*8   TPFL,NOECHO(NBNL,*)
      INTEGER       VECI1(*)
      LOGICAL       LOCFL0(*)
      REAL*8        DT0, TFEXM, TS, DTTR, VECDT(*)
      INTEGER       IARCH,NBCHOC
      REAL*8        VITG0(*),  DEPG0(*),
     &              MASGI(*), AMORI(*), PULSI(*), PHII(NP2,NBM,*),
     &              VECR5(*), VECR3(*), VECR1(*), VECR2(*),
     &              VGAP, VECR4(*),
     &              XSI0(*)
      INTEGER       NBSAUV,
     &              INDX(*), INDXF(*), INTGE1(*), INTGE2(*),
     &              ICONFB(*)
      REAL*8        TCONF1(4,*), TCONF2(4,*), TCONFE(4,*)
      INTEGER       TYPCH(*), NBSEG(*), OLDIA(*), ITFORN(*)
      REAL*8        AMOR(*), AMOR0(*), AMOR00(*),
     &              PULS(*), PULS0(*), PULS00(*),
     &              TRANS(2,2,*), PULSD(*)
      REAL*8        FMOD0(*), FMOD00(*), FMODT(*), FMOD0T(*),
     &              FEXMOD(*), FNLMOD(*), FMODA(*), FMRES(*)
      REAL*8        DEPG(*), DEPGE(*), DEPGC(*), DEPGT(*), DEPG0T(*),
     &              VITG(*), VITGE(*), VITGC(*), VITGT(*), VITG0T(*),
     &              ACCG(*), ACCG0(*), ACCGT(*)
      REAL*8        DEP(3,*), VIT(3,*), ACC(3,*),
     &              DEP0(3,*), VIT0(3,*), ACC0(3,*)
      REAL*8        KMOD(NBM,*), CMOD(NBM,*),
     &              KMOD0(NBM,*), CMOD0(NBM,*),
     &              KMOD00(NBM,*), CMOD00(NBM,*),
     &              KMODCA(NBM,*), CMODCA(NBM,*),
     &              CMODFA(NBM,*), AMFLU0(NBM,*), AMFLUC(NBM,*)
      REAL*8        VG(NBM,*), VD(NBM,*), TTR(N2,*),
     &              VG0(NBM,*), VD0(NBM,*), VVG(NBM,*),
     &              RR(*), RI(*), RR0(*),
     &              MTMP1(NBM,*), MTMP2(NBM,*), MTMP6(3,*),
     &              FTMP(*), DD(*), U(*), W(*)
      REAL*8        OMEGAF(*), AA(NP4,*), BB(NP4,*),
     &              FEXT(NP4,*), FEXTTS(NP4,*), TEXT(*), TEXTTS(*),
     &              FEXTTR(*), FEXTT0(*)
      CHARACTER*8   NOMCH(*)
      REAL*8        CHOC(5,*), ORIG(6,*), RC(NP3,*), THETA(NP3,*),
     &              ALPHA(2,*), BETA(2,*), GAMMA(2,*), OLD(9,*)
      LOGICAL       LOCFLC(*), LOC(*)
      COMPLEX*16    S0(*), Z0(*), SR0(*), ZA1(*), ZA2(*), ZA3(*),
     &              ZA4(NP4,*), ZA5(NP4,*), ZIN(*), ZITR(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER       ITEST, TESTC, IER, IVAR, INDNEW, INDNE0,I,
     &              XIT0, IT0, IDECRT, NTRANS, XNBR0,JINST,
     &              ILONG, NPF, NPFMAX, NPFTS, NTTR, NDEF, INDT, NBR0
      INTEGER       JORDRE, JTEMPS, JDEPG, JVITG, JACCG, JDEP, JFOR,
     &              JVIT, KDEPL, KVITE, KACCE, KORDR, KINST, KPTEM,
     &              KFCHO, KDCHO, KVCHO, KADCHO
      INTEGER       IFR, IFM, LATEST, IERCPU,
     &              NBREDE, NBREVI, KREDC, KREDD
      REAL*8        TC, DT, DIV, TC0, TS0, TOL, TOLC, TOLN, TOLV,
     &              FTEST, FTEST0, TTRANS, PREMAC, PREREL, TPS1(4)
      LOGICAL       LSAUV, LPSTO
      CHARACTER*8   RESU, K8B, METHOD
      CHARACTER*16  NOMCMD, TYPRES, CHAIN1, CHAIN2, CHAIN3, CHAIN4,
     &              CHAIN5, CHAIN6, CHAIN7, CHAIN8
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC     MOD
C
C FONCTIONS EXTERNES
C ------------------
      INTEGER       IUNIFI
      REAL*8        R8MIEM, R8PREM
C     EXTERNAL      IUNIFI, R8MIEM, R8PREM
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL      ACCELE, ADIMVE, ALITMI, CALTOL, CALVOL, DEFMCF,
C    &              DEFTTR, ECRBAS, ECRCHO, ECRGEN, INIALG, INIPAR,
C    &              INIPCT, MDALLO, MDCHOF, MDITM3, PROJMP,
C    &              PROJVD, SOMMVE, TRANSI, UTDEBM, UTFINM, UTIMPI,
C    &              UTIMPR, UTMESS, UTTCPU, VARDEP,
C    &              GETRES, JEDEMA, JELIRA, JEMARQ, JEVEUO
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      CALL JEMARQ()
      PREMAC = R8PREM()
      PREREL = R8MIEM()/R8PREM()
      CALL GETRES ( RESU,TYPRES,NOMCMD)
C
C 1.  PREPARATION TABLEAUX ASTER
C     --------------------------
      CHAIN1 = '&&MDITM2.ORDRE'
      CHAIN2 = '&&MDITM2.TEMPS'
      CHAIN3 = '&&MDITM2.DEPLG'
      CHAIN4 = '&&MDITM2.VITEG'
      CHAIN5 = '&&MDITM2.ACCEG'
      CHAIN6 = '&&MDITM2.DEPNC'
      CHAIN7 = '&&MDITM2.FORNC'
      CHAIN8 = '&&MDITM2.VITNC'
      ILONG = 0
      CALL MDITM3 ( CHAIN1,CHAIN2,CHAIN3,CHAIN4,CHAIN5,CHAIN6,
     &              CHAIN7,CHAIN8,ILONG,NBM,NBNL)
      CALL JEVEUO(CHAIN1,'E',JORDRE)
      CALL JEVEUO(CHAIN2,'E',JTEMPS)
      CALL JEVEUO(CHAIN3,'E',JDEPG)
      CALL JEVEUO(CHAIN4,'E',JVITG)
      CALL JEVEUO(CHAIN5,'E',JACCG)
      CALL JEVEUO(CHAIN6,'E',JDEP)
      CALL JEVEUO(CHAIN7,'E',JFOR)
      CALL JEVEUO(CHAIN8,'E',JVIT)
C
C 2.  INITIALISATION DES PARAMETRES DE CALCUL
C     ---------------------------------------
      CALL INIPCT ( ITEST,DIV,NBR0,TC)
C
C 3.  CALCUL DES TOLERANCES DE CALCUL EN FONCTION DE LA CONFIGURATION
C     ---------------------------------------------------------------
      CALL CALTOL ( NP3,NBNL,TYPCH,NBSEG,RC,THETA,TOL,TOLC,TOLN,TOLV)
C
C 4.  CALCUL DES MATRICES DE RAIDEUR ET AMORTISSEMENT DU SYSTEME EN VOL
C     -----------------------------------------------------------------
      CALL CALVOL ( NBM,NBM,ICOUPL,INDIC,
     &              KMOD00,CMOD00,AMOR00,PULS00,PULSI,AMORI,MASGI,
     &              TPFL,VECI1,VECR1,VECR2,VECR5,VECR3,VGAP,VECR4,
     &              LOCFL0,AMFLU0,XSI0)
C
C 5.  DEFINITION DES MODES PRIS EN COMPTE POUR LE CALCUL DU SAUT DE
C     FORCE FLUIDELASTIQUE D'AMORTISSEMENT EN PHASE DE CHOC
C     -----------------------------------------------------
      CALL DEFMCF ( NBM,NBMP,LOCFL0,LOCFLC)
C
C 6.  CALCUL DU TRANSITOIRE
C     ---------------------
      TC0 = 0.0D0
      NPFMAX = NP4
      IF ( ITRANS.EQ.1 ) THEN
         CALL TRANSI ( NBM,NP2,NP3,NP4,NBMCD,NBNL,NPFMAX,NPFTS,
     &                 DTTR,TTRANS,EPST,FEXT,TEXT,FEXTTS,TEXTTS,
     &                 FEXTTR,FEXTT0,
     &                 MASGI,AMORI,PULSI,PHII,
     &                 TYPCH,NBSEG,RC,ALPHA,BETA,GAMMA,ORIG,THETA,
     &                 VITG,DEPG,AMOR,PULSD,OMEGAF,AA,BB,OLD,
     &                 S0,Z0,SR0,ZA1,ZA2,ZA3,ZA4,ZA5,ZITR,ZIN,TRANS,
     &                 AMOR00,PULS00,ACCG0,VITG0,DEPG0,
     &                 ICONFB,TCONF1,FTEST0,IER)
      ELSE
         TTRANS = 0.0D0
         NTRANS = 0
         NPF = 0
         CALL DEFTTR ( NBM,NP4,NBMCD,NPF,NTTR,NTRANS,TTRANS,TTRANS,TEXT,
     &                 FEXT,FEXTTR,FEXTTR,DTTR)
         CALL ADIMVE ( NBMCD,FEXTTR,MASGI)
         CALL INIALG ( NBM,NP2,NP3,NP4,NBMCD,NBNL,NTTR,NPFMAX,NPFTS,
     &                 DEPG,VITG,DEPG0,VITG0,ACCG0,
     &                 AMOR00,PULS00,FEXTTR,FEXT,TEXT,FEXTTS,TEXTTS,
     &                 TYPCH,NBSEG,PHII,ALPHA,BETA,GAMMA,ORIG,RC,THETA,
     &                 ICONFB,TCONF1,FTEST0)
      ENDIF
      CALL ECRGEN ( NBR0,NBM,TC0,DEPG0,VITG0,ACCG0,
     &              ZR(JDEPG),ZR(JVITG),ZR(JACCG),ZR(JTEMPS),ZI(JORDRE))
      CALL ECRCHO ( NBR0,NBNL,OLD,ZR(JDEP),ZR(JVIT),ZR(JFOR))
C
C-----------------------------------------------------------------------
C 7.  BOUCLE TEMPORELLE APRES PASSAGE DU TRANSITOIRE
C-----------------------------------------------------------------------
C 7.1 INITIALISATIONS
C     ---------------
      IT0    = 0
      IDECRT = 0
      TESTC  = 0
      INDNE0 = 0
      INDNEW = 0
      NDEF   = 0
C
      DT     = DT0
      DT0    = 0.0D0
      TS0    = TFEXM - TTRANS
      IF ( TS0.LT.TS ) THEN
         TS = TS0
         CALL UTMESS('A','MDITM2','DUREE DE LA SIMULATION TEMPORELLE'//
     &               ' APRES TRANSITOIRE INFERIEURE A LA DUREE '//
     &               'DEMANDEE (EXCITATION TEMPORELLE TROP COURTE)')
      ENDIF
C
      TC = 0.0D0
      TC = TC + DT
      LSAUV = .TRUE.
C
C 7.2 IMPRESSIONS AVANT EXECUTION DU CALCUL
C     -------------------------------------
 1001 FORMAT(/3X,'====================================================',
     &'===========================')
 1002 FORMAT(3X,'          RESULTAT DU CALCUL TEMPOREL AVEC COUPLAGE ',
     &'FLUIDE-STRUCTURE           ')
 1003 FORMAT(3X,'=====================================================',
     &'=========================='/)
 1007 FORMAT(3X,'FIN DU TRANSITOIRE                   -            ',
     &'TEMPS COURANT: ',1PD14.6/)
 1004 FORMAT(3X,'DEBUT DU CALCUL TEMPOREL             -            ',
     &'TEMPS COURANT: ',1PD14.6/)
 1005 FORMAT(3X,'FIN DU CALCUL TEMPOREL               -            ',
     &'TEMPS COURANT: ',1PD14.6/)
 1008 FORMAT(3X,'INDICE DE TEMPS DU  CALCUL TEMPOREL  -            ',
     &'TEMPS COURANT: ',1PD14.6/)
 1006 FORMAT(3X,'NOMBRE D APPELS A ALITMI             -            ',
     &'    ',I9/)
 1009 FORMAT(3X,'****************      ARRET  PAR  MANQUE  DE  TEMPS  ',
     &'CPU     ******************'/)
C
      IF ( IMPR.GE.1 ) THEN
         IFR = IUNIFI('RESULTAT')
         IFM = IUNIFI('MESSAGE')
         IF ( IMPR.GE.2 ) THEN
            WRITE(IFR,1001)
            WRITE(IFR,1002)
            WRITE(IFR,1003)
            WRITE(IFR,1007) TTRANS
            WRITE(IFR,1003)
            WRITE(IFR,1004) (TC - DT)
         ENDIF
         WRITE(IFM,1001)
         WRITE(IFM,1002)
         WRITE(IFM,1003)
         WRITE(IFM,1007) TTRANS
         WRITE(IFM,1003)
         WRITE(IFM,1004) (TC - DT)
      ENDIF
C
C 7.3 DEBUT DU BLOC 'TANT QUE'
C     ------------------------
      CALL UTTCPU ( 1,'INIT',4,TPS1)
 100  CONTINUE
      CALL UTTCPU ( 1,'DEBUT',4,TPS1)
      LATEST = TESTC
C
C --- TANT QUE TC EST INFERIEUR A LA DUREE DE LA SIMULATION
C
      IF ( TC.LE.TS ) THEN
C
C 7.3.1  CALCUL DU VECTEUR D'ETAT A L'INSTANT N+1 EN FONCTION DU VECTEUR
C        D'ETAT A L'INSTANT N
C
         CALL ALITMI ( NBM,NP2,NP3,NP4,N2,NBM,NBMCD,ICOUPL,TC,DT0,DT,
     &   VECDT,NBNL,TESTC,ITEST,INDNEW,INDNE0,IDECRT,
     &   FTEST,FTEST0,ICONFB,TCONF1,TCONF2,TCONFE,TYPCH,NBSEG,
     &   PHII,CHOC,ALPHA,BETA,GAMMA,ORIG,RC,THETA,OLD,OLDIA,ITFORN,
     &   VGAP,VECR4,XSI0,INDIC,TPFL,VECI1,VECR1,VECR2,VECR5,VECR3,
     &   MASGI,AMORI,PULSI,AMOR,AMOR0,PULS,PULS0,
     &   ACCG0,VITG0,DEPG0,VITGE,DEPGE,VITG,DEPG,VITGC,DEPGC,
     &   VITGT,DEPGT,VITG0T,DEPG0T,
     &   CMOD0,KMOD0,CMOD,KMOD,CMODCA,KMODCA,AMFLU0,AMFLUC,CMODFA,
     &   LOCFLC,NPFTS,TEXTTS,FEXTTS,NDEF,INDT,
     &   FEXMOD,FNLMOD,FMRES,FMODA,FMOD0,FMOD00,FMODT,FMOD0T,
     &   DIV,TOL,TOLC,TOLN,TOLV,INTGE1,INTGE2,INDX,INDXF,
     &   FTMP,MTMP1,MTMP2,MTMP6,
     &   TTR,U,W,DD,LOC,VVG,VG,VG0,VD,VD0,
     &   RR,RR0,RI,PREMAC,PREREL,TRANS,PULSD,S0,Z0,SR0,ZA1,ZA2,ZA3,ZIN)
C
C 7.3.2  ACTUALISATION DE LA FORCE NON LINEAIRE RESIDUELLE
C        A L'INSTANT N+1
C
         TC0 = TC - DT
         TESTC = LATEST
         IF ( INDNEW.EQ.0 ) THEN
            CALL MDCHOF ( NBM,NP2,NP3,NBM,IMPR,TC0,
     &                    NBNL,TYPCH,NBSEG,PHII,NOMCH,
     &                    CHOC,ALPHA,BETA,GAMMA,ORIG,RC,THETA,
     &                    VITGE,DEPGE,VITG0,DEPG0,
     &                    OLD,OLDIA,FMRES,FNLMOD,FTMP,
     &                    TESTC,ITFORN,TOLN)
         ELSE
            CALL MDCHOF ( NBM,NP2,NP3,NBM,IMPR,TC0,
     &                    NBNL,TYPCH,NBSEG,PHII,NOMCH,
     &                    CHOC,ALPHA,BETA,GAMMA,ORIG,RC,THETA,
     &                    VITGC,DEPGC,VITG0,DEPG0,
     &                    OLD,OLDIA,FMRES,FNLMOD,FTMP,
     &                    TESTC,ITFORN,TOLN)
         ENDIF
         INDNE0 = INDNEW
         INDNEW = 0
         CALL SOMMVE ( NBM,FEXMOD,NBM,FNLMOD,NBM,FMOD00)
         CALL SOMMVE ( NBM,FEXMOD,NBM,FMRES,NBM,FMODA)
         CALL ADIMVE ( NBM,FMODA,MASGI)
C
C 7.3.3  PROJECTION DE LA FORCE MODALE SUR LA BASE MODALE
C
          CALL PROJVD ( TESTC,NBM,NBM,NBM,VG,FMODA,FMODT)
C
C 7.3.4  CALCUL DE L'ACCELERATION
C
         IF ( TESTC.EQ.0 ) THEN
            CALL ACCELE ( NBMCD,AMOR00,PULS00,FMODA,ACCG,VITG,DEPG)
         ELSE
            CALL ACCELE ( NBMCD,AMOR,PULS,FMODT,ACCGT,VITGT,DEPGT)
            CALL PROJVD ( TESTC,NBM,NBM,NBMCD,VD,ACCGT,ACCG)
         ENDIF
C
C 7.3.5  PASSAGE EN BASE PHYSIQUE ET ECRITURE DES RESULTATS
C
         XIT0 = MOD(IT0,IARCH)
         IF ( LSAUV ) THEN
            XIT0 = 0
            LSAUV = .FALSE.
         ENDIF
         IF ( XIT0.EQ.0 ) THEN
            NBR0 = NBR0 + 1
            IF ( TESTC.EQ.0 ) THEN
               CALL PROJMP ( NBM,NP2,NBMCD,NBNL,
     &                       PHII,ACCG,VITG,DEPG,ACC,VIT,DEP)
            ELSE
               CALL PROJMP ( NBM,NP2,NBM,NBNL,
     &                       PHII,ACCG,VITG,DEPG,ACC,VIT,DEP)
            ENDIF
            CALL VARDEP ( NBNL,DEP,DEP0,TCONF2,TCONF1,IVAR,DT0,
     &                    TOLN,TOLC,TOLV)
            IF ( IVAR.NE.0 )
     &         CALL UTMESS('A','MDITM2','VARIATION DU DEPLACEMENT '//
     &            'ENTRE DEUX INSTANTS SUCCESSIFS SUPERIEURE A LA '//
     &            'VALEUR DE TOLERANCE PROPOSEE')
            XNBR0 = MOD(NBR0+1,10000)
            IF ( XNBR0.EQ.0 ) THEN
               CALL JELIRA(CHAIN1,'LONMAX',ILONG,K8B)
               CALL MDITM3(CHAIN1,CHAIN2,CHAIN3,CHAIN4,CHAIN5,CHAIN6,
     &                     CHAIN7,CHAIN8,ILONG,NBM,NBNL)
               CALL JEVEUO(CHAIN1,'E',JORDRE)
               CALL JEVEUO(CHAIN2,'E',JTEMPS)
               CALL JEVEUO(CHAIN3,'E',JDEPG)
               CALL JEVEUO(CHAIN4,'E',JVITG)
               CALL JEVEUO(CHAIN5,'E',JACCG)
               CALL JEVEUO(CHAIN6,'E',JDEP)
               CALL JEVEUO(CHAIN7,'E',JFOR)
               CALL JEVEUO(CHAIN8,'E',JVIT)
            ENDIF
            CALL ECRGEN ( NBR0,NBM,TC0,DEPG,VITG,ACCG,ZR(JDEPG),
     &                    ZR(JVITG),ZR(JACCG),ZR(JTEMPS),ZI(JORDRE))
            CALL ECRCHO ( NBR0,NBNL,OLD,ZR(JDEP),ZR(JVIT),ZR(JFOR))
         ENDIF
C
C 7.3.6  CONDITIONS INITIALES POUR LE PAS DE TEMPS SUIVANT
C
         IT0 = IT0 + 1
         IDECRT = 0
         IF ( TESTC.EQ.0 ) THEN
            CALL INIPAR ( NBM,NBM,NBNL,TESTC,
     &                    CMOD0,CMOD00,KMOD0,KMOD00,
     &                    AMOR00,AMOR0,PULS00,PULS0,
     &                    ACC,VIT,DEP,ACC0,VIT0,DEP0,
     &                    ACCG,VITG,DEPG,ACCG0,VITG0,DEPG0,
     &                    TCONF1,FTEST0,TCONF2,FTEST)
         ELSE IF ( TESTC.EQ.1 ) THEN
            IF ( ICOUPL.EQ.0 ) THEN
               CALL INIPAR ( NBM,NBM,NBNL,TESTC,
     &                       CMOD0,CMODCA,KMOD0,KMODCA,
     &                       AMOR,AMOR0,PULS,PULS0,
     &                       ACC,VIT,DEP,ACC0,VIT0,DEP0,
     &                       ACCG,VITG,DEPG,ACCG0,VITG0,DEPG0,
     &                       TCONF1,FTEST0,TCONF2,FTEST)
            ELSE IF ( ICOUPL.EQ.1 ) THEN
               CALL INIPAR ( NBM,NBM,NBNL,TESTC,
     &                       CMOD0,CMODFA,KMOD0,KMODCA,
     &                       AMOR,AMOR0,PULS,PULS0,
     &                       ACC,VIT,DEP,ACC0,VIT0,DEP0,
     &                       ACCG,VITG,DEPG,ACCG0,VITG0,DEPG0,
     &                       TCONF1,FTEST0,TCONF2,FTEST)
            ENDIF
         ENDIF
C
C --- SI TC EST SUPERIEUR A LA DUREE DE LA SIMULATION,
C --- SORTIE DU BLOC 'TANT QUE'
C
      ELSE
         GO TO 999
      ENDIF
C
C --- SORTIE DU BLOC 'TANT QUE' SI MANQUE DE TEMPS CPU
C
      IERCPU = 0
      CALL UTTCPU ( 1,'FIN',4,TPS1)
      IF ( (TPS1(4).GT.(0.90D0*(TPS1(1)-20.0D0))).OR.
     &     (TPS1(1).LT.20.0D0) ) THEN
         IERCPU = 1
         GO TO 999
      ENDIF
C
C --- RETOURNER AU DEBUT DU BLOC 'TANT QUE'
C
      GO TO 100
C
C 7.4 IMPRESSIONS APRES EXECUTION DU CALCUL
C     -------------------------------------
 999  CONTINUE
      IF ( IMPR.GE.1 ) THEN
         WRITE(IFM,1005) TC
         WRITE(IFM,1003)
         WRITE(IFM,1006) IT0
         IF ( IERCPU.EQ.1 ) WRITE(IFM,1009)
         WRITE(IFM,1003)
         IF ( IMPR.GE.2 ) THEN
            WRITE(IFR,1005) TC
            WRITE(IFR,1003)
            WRITE(IFR,1006) IT0
            IF ( IERCPU.EQ.1 ) WRITE(IFR,1009)
            WRITE(IFR,1003)
         ENDIF
      ENDIF
C
C 7.5 ARCHIVAGE DES RESULTATS
C     -----------------------
      NBSAUV = NBR0
      NBREDE = 0
      NBREVI = 0
      LPSTO = .FALSE.
      METHOD = 'ITMI'

      CALL MDALLO ( RESU,K8B,K8B,K8B,K8B,NBM,DT0,NBSAUV,NBNL,K8B,K8B,
     &              NBREDE,K8B,NBREVI,KDEPL,KVITE,KACCE,KPTEM,KORDR,
     &              KINST,KFCHO,KDCHO,KVCHO,KADCHO,KREDC,KREDD,LPSTO,
     &              METHOD)
      CALL ECRBAS ( NBSAUV,NBNL,NBM,ZR(JDEPG),ZR(JVITG),ZR(JACCG),
     &              ZR(JTEMPS),ZI(JORDRE),ZR(JDEP),ZR(JVIT),ZR(JFOR),
     &              ZR(KDEPL),ZR(KVITE),ZR(KACCE),ZR(KINST),ZI(KORDR),
     &              ZR(KDCHO),ZR(KVCHO),ZR(KFCHO))
C     --- IMPRESSION DES RESULTATS DE CHOC

      IF (NBNL.NE.0) THEN
        CALL MDICHO(RESU,NBSAUV,ZR(KINST),ZR(KFCHO),ZR(KDCHO),
     &              ZR(KVCHO),NBNL,NBCHOC,PARCHO,NOECHO)
      END IF

C 7.6 IMPRESSIONS SUPPLEMENTAIRES SI ARRET PAR MANQUE DE TEMPS CPU
C     ------------------------------------------------------------
      IF ( IERCPU.EQ.1 ) THEN
         CALL UTDEXC(28,'MDITM2','ARRET PAR MANQUE DE TEMPS CPU.')
         CALL UTIMPR('L',' INSTANT COURANT :              ' ,1,TC)
         CALL UTIMPI('L',' NOMBRE D''APPELS A ALITMI :     ',1,IT0)
         CALL UTIMPR('L',' TEMPS MOYEN PAR PAS DE TEMPS : ' ,1,TPS1(4))
         CALL UTIMPR('L',' TEMPS CPU RESTANT:             ' ,1,TPS1(1))
         CALL UTFINM()
      ENDIF
C
      CALL JEDEMA()
C
C --- FIN DE MDITM2.
      END
