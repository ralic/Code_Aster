      SUBROUTINE FONOCI ( NOMOPE, SORTIE, BASE, IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                                   IER
      CHARACTER*1                         BASE
      CHARACTER*16        NOMOPE
      CHARACTER*19                SORTIE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C
C      --- INDICES DE NOCIVITE D'UN SEISME ---
C
C     ------------------------------------------------------------------
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
      INTEGER        LF, L, K, LAMOR, NBFREQ, LFREQ,NBVALU
      INTEGER        I,NBVAL,LTINI,LTFIN,LFINI,LFFIN,LVAR,NS
      INTEGER        NBMAX, NBPTS,IBID,JOPT,LPRO, NBOPT    
      REAL*8         COEF, RBID, NORME, AMOR, VALIND(14)
      REAL*8         TINI, TFIN, BINF, BSUP, FINI, FFIN, EPSI, RMS,
     +               AMAX,VMAX,DMAX, ARIAS, PDES, CAV, DPHFOR,PESA, 
     +               ISPEC,VALINF, VALSUP,ASURV,DEUXG,PI,R8PI
      COMPLEX*16     C16B
      LOGICAL        LTOUT, LMAX, LARIAS, LPDES, LCAV, LPHFOR,
     +               LISPEC,LASURV
      CHARACTER*1    K1BID
      CHARACTER*8    K8B, NATURE, CRIT, TYPIND(15)
      CHARACTER*16   NOPIND(15), NOMRES
      CHARACTER*19   NOMFON, LISTFR, NOMVIT, NOMDEP
      CHARACTER*24   VALE,PROL
C
      DATA TYPIND/'K8','R','R','R','R','R','R','R','R','R','R','R','R',
     .               'R','R'/
C-----------------------------------------------------------------------
       CALL JEMARQ()
C   
       PI   = R8PI()
       CRIT = '????'
C
C      --- D'ABORD, RECUPERATIONS D'ELEMENTS UTILES A LA FOIS ---
C      --- POUR TRAITER UN ACCELEROGRAMME ET UN SRO           ---
C
C       --- CRITERE ET PRECISION ---
       CALL GETVTX(NOMOPE,'CRITERE',   1,1,1, CRIT,  L)
       CALL GETVR8(NOMOPE,'PRECISION', 1,1,1, EPSI,  L)
C
C       --- RECUPERATION DES FREQUENCES INITIALE ET FINALE ---
C       --- D'INTEGRATION, POUR L'INTENSITE SPECTRALE      ---
       CALL GETVR8(NOMOPE,'FREQ_INIT', 1,1,1, FINI, LFINI)
       CALL GETVR8(NOMOPE,'FREQ_FIN',  1,1,1, FFIN, LFFIN)

C       --- RECUPERATION DU COEFFICIENT D'AMORTISSEMENT ---
       CALL GETVR8(NOMOPE,'AMOR_REDUIT',1,1,1,AMOR,LAMOR)
C
C       --- RECUPERATION DE LA PESANTEUR ---
       CALL GETVR8(NOMOPE,'PESANTEUR',1,1,1,PESA,L)
C
C       --- NORME DU SPECTRE D'OSCILLATEUR ---
       CALL GETVR8(NOMOPE,'NORME',1,1,1,NORME,L)
C
C       --- OPTIONS : CHOIX DES INDICES A CALCULER ---
       CALL GETVTX(NOMOPE,'OPTION',1,1,0,K8B,NS)
       NBOPT  = -NS
       CALL WKVECT('&&FONOCI.OPTION','V V K16',NBOPT,JOPT)
       CALL GETVTX(NOMOPE,'OPTION',1,1,NBOPT,ZK16(JOPT),NS)
C      
C          --- INITIALISATION DES VALEURS LOGIQUES ---
       
       LTOUT     = .FALSE.
       LMAX      = .FALSE.
       LARIAS    = .FALSE.
       LPDES     = .FALSE.
       LASURV    = .FALSE.
       LCAV      = .FALSE.
       LPHFOR    = .FALSE.
       LISPEC    = .FALSE.
C
       DO 1301 I = 1 , NBOPT
         IF ( ZK16(JOPT+I-1) .EQ. 'TOUT' )  LTOUT = .TRUE.
 1301  CONTINUE
C
       IF ( .NOT. LTOUT ) THEN
C
          DO 1302 I = 1 , NBOPT
C
             IF     ( ZK16(JOPT+I-1) .EQ. 'MAXI'      ) THEN
                LMAX    = .TRUE.
C
             ELSEIF ( ZK16(JOPT+I-1) .EQ. 'INTE_ARIAS'    ) THEN
                LARIAS  = .TRUE.
C
             ELSEIF ( ZK16(JOPT+I-1) .EQ. 'POUV_DEST'    ) THEN
                LPDES   = .TRUE.
C
             ELSEIF ( ZK16(JOPT+I-1) .EQ. 'ACCE_SUR_VITE'  ) THEN
                LASURV  = .TRUE.
C
             ELSEIF ( ZK16(JOPT+I-1) .EQ. 'VITE_ABSO_CUMU'      ) THEN
                LCAV    = .TRUE.
C
             ELSEIF ( ZK16(JOPT+I-1) .EQ. 'DUREE_PHAS_FORT' ) THEN
                LPHFOR  = .TRUE.
C
             ELSEIF ( ZK16(JOPT+I-1) .EQ. 'INTE_SPEC'    ) THEN
                LISPEC  = .TRUE.
C
             ENDIF
C
 1302     CONTINUE
C
       ENDIF
C
C           --- PARAMETRE DE LA 1ERE COLONNE DU TABLEAU DE RESULTATS ---
       NOPIND(1) = 'FONCTION'
C
C
C      --- RECUPERATION DE L'ACCELEROGRAMME ---
C      NB: SI LA FONCTION A ETUDIER EST UN SRO, ON NE PEUT CALCULER 
C      QUE SON INTENSITE SPECTRALE (VOIR LA FIN DU PROGR "NOCI_SEIS").
C
       CALL GETVID(NOMOPE,'FONCTION',1,1,1,NOMFON,L)
       IF ( L .GT. 0 ) THEN
         PROL = NOMFON//'.PROL'
         CALL JEVEUO(PROL,'L',LPRO)
         NOMRES = ZK16(LPRO+3)
         IF (NOMRES(1:4).EQ.'ACCE') THEN
        
C
C      ---------------------------------
C      --- ETUDE DE L'ACCELEROGRAMME ---
C      ---------------------------------
C
C      --- RECUPERATIONS DES INSTANTS INITIAL ET FINAL ---
          CALL GETVR8(NOMOPE,'INST_INIT', 1,1,1, TINI,  LTINI)
          CALL GETVR8(NOMOPE,'INST_FIN',  1,1,1, TFIN,  LTFIN)
C
C         ---  NOMBRE DE POINTS ----
          VALE = NOMFON//'.VALE'
          CALL JELIRA(VALE,'LONUTI',NBVAL,K1BID)
          CALL JEVEUO(VALE,'L',LVAR)
          NBPTS = NBVAL/2
C
          IF ( LTINI .EQ. 0)  TINI = ZR(LVAR)
          IF ( LTFIN .EQ. 0)  TFIN = ZR(LVAR+NBPTS-1)
C
C        --- RECUPERATION DE LA LISTE DE FREQUENCES ---
          CALL GETVR8(NOMOPE,'FREQ',1,1,0,RBID,L)
          IF ( L .EQ. 0 ) THEN
            CALL GETVID(NOMOPE,'LIST_FREQ',1,1,1,LISTFR,LF)
            IF ( LF .EQ. 0 ) THEN
              CALL FOC2DF('FREQ','&&FONOCI.LISTE.FREQ',1,NBFREQ)
              CALL JEVEUO('&&FONOCI.LISTE.FREQ','E',LFREQ)
            ELSE
              CALL JEVEUO(LISTFR//'.VALE','L',LFREQ)
              CALL JELIRA(LISTFR//'.VALE','LONUTI',NBFREQ,K8B)
            ENDIF
          ELSE
            NBFREQ = -L
            CALL WKVECT('&&FONOCI.LISTE.FREQ','V V R',NBFREQ,LFREQ)
            CALL GETVR8(NOMOPE,'FREQ',1,1,NBFREQ,ZR(LFREQ),L)
          ENDIF
C
C        --- VALEURS DES 2E ET 3E COLONNES DU TABLEAU DE RESULTATS ---
          NOPIND(2)  = 'INST_INIT'
          VALIND(1)  = TINI
          NOPIND(3)  = 'INST_FIN'
          VALIND(2)  = TFIN
C
C
C           -----------------------------------------------------
C           --- ON PEUT ALORS CALCULER LES DIFFERENTS INDICES ---
C           -----------------------------------------------------
C
          K=3
C
C        --- CALCUL DU MAX D'UNE FONCTION ---
          IF ( LMAX .OR. LTOUT ) THEN
C
             CALL GETVR8(NOMOPE,'COEF',1,1,1,COEF,L)
C 
             CALL FOCMAX(NOMFON,LTINI,LTFIN,CRIT,EPSI,
     &       NOMOPE,COEF,TINI,TFIN,NBVALU,AMAX,VMAX,DMAX,BASE,IER)
          
C
             K=K+1
             NOPIND(K)   = 'ACCE_MAX'
             VALIND(K-1) = AMAX
             NOPIND(K+1) = 'VITE_MAX'
             VALIND(K)   = VMAX
             NOPIND(K+2) = 'DEPL_MAX'
             VALIND(K+1) = DMAX
C
             K=K+2
            
          ENDIF
C
C
C        --- INTENSITE ARIAS D'UN SIGNAL TEMPOREL ---
          IF ( LARIAS .OR. LTOUT ) THEN
C
C            --- CALCUL DE LA MOYENNE QUADRATIQUE ---           
             CALL FOCRMS('TRAPEZE',NOMFON,CRIT,EPSI,TINI,LTINI,
     &                                         TFIN,LTFIN,0.D0,RMS)
C
             DEUXG=2.D0*PESA
             ARIAS = RMS**2*PI/(DEUXG)*(TFIN-TINI)
C
             K=K+1
             NOPIND(K)   = 'INTE_ARIAS'
             VALIND(K-1) = ARIAS
C
          ENDIF
C
C
C        --- POUVOIR DESTRUCTEUR D'UN SIGNAL TEMPOREL ---
          IF ( LPDES .OR. LTOUT ) THEN
C
C          --- INTEGRATION D'UN SIGNAL TEMPOREL ---
C
C             --- FONCTION A INTEGRER ---
            CALL FOVECA(NOMOPE,NOMFON,IER)
            IF (IER .NE. 0 ) GOTO 9999
C
            CALL GETVR8(NOMOPE,'COEF',1,1,1,COEF,L)
C
            CALL UTMESS('I','POUVOIR DESTRUCTEUR','ON INTEGRE'
     &      //' L ACCELEROGRAMME POUR OBTENIR LE SIGNAL EN VITESSE,'
     &      //' UTILE POUR CALCULER LE POUVOIR DESTRUCTEUR. ')
C
C           --- AINSI ON OBTIENT PAR INTEGRATION V(T) ("NOMVIT") ---
            NOMVIT = '&&FONOCI.VIT'
            CALL FOCAIN('TRAPEZE',NOMFON,COEF,NOMVIT,BASE)
C
C          --- ALORS ON PEUT CACULER LA RMS DE V(T) COMME ---
C          --- INTERMEDIAIRE DE CALCUL                    ---
            CALL FOCRMS('TRAPEZE',NOMVIT,CRIT,EPSI,TINI,LTINI,
     &                                          TFIN,LTFIN,0.D0,RMS)
C
C          --- ENFIN ON EN DEDUIT LE POUVOIR DESTRUCTEUR --
C
            DEUXG=2.D0*PESA
            PDES = RMS**2*PI**3/(DEUXG)*(TFIN-TINI)
C
            K=K+1
            NOPIND(K)   = 'POUV_DEST'
            VALIND(K-1) = PDES
C
            CALL JEDETR('&&FONOCI.VIT       .VALE')
            CALL JEDETR('&&FONOCI.VIT       .PROL')
          ENDIF

C     --- RAPPORT AMAX/VMAX
          IF (LASURV .OR. LTOUT) THEN
C     --- CALCUL DE AMAX S IL N PAS DEJA ETE CALCULE
            IF (.NOT.LMAX) THEN 
              CALL GETVR8(NOMOPE,'COEF',1,1,1,COEF,L)
              CALL FOCMAX(NOMFON,LTINI,LTFIN,CRIT,EPSI,
     &        NOMOPE,COEF,TINI,TFIN,NBVALU,AMAX,VMAX,DMAX,BASE,IER)
C
            ENDIF
            IF (VMAX.NE.0.D0) THEN
                ASURV=AMAX/VMAX
            ELSE
              CALL UTMESS('F',NOMOPE,'VITESSE MAXIMALE NULLE')  
            ENDIF
            K=K+1
            NOPIND(K)='ACCE_SUR_VITE'
            VALIND(K-1)=ASURV
C               
C
         ENDIF 
C
C     --- VALEUR ABS. CUMULEE DE LA VITESSE D'UN ACCELEROGRAMME ---
          IF ( LCAV .OR. LTOUT ) THEN
C
C     --- CALCUL DE LA CAV ---           
            CALL FOCCAV('TRAPEZE',NOMFON,CRIT,EPSI,TINI,LTINI,
     &                                           TFIN,LTFIN,CAV)
C
            K=K+1
            NOPIND(K)   = 'VITE_ABSO_CUMU'
            VALIND(K-1) = CAV
C
          ENDIF
C
C
C     --- DUREE DE PHASE FORTE D'UN ACCELEROGRAMME ---
          IF ( LPHFOR .OR. LTOUT ) THEN
C
C           --- CALCUL DE L'INTENSITE ARIAS ("ARIAS") DE ---
C           --- L'ACCELEROGRAMME                         ---
C
C              --- CALCUL DE LA MOYENNE QUADRATIQUE ("RMS") ---
            CALL FOCRMS('TRAPEZE',NOMFON,CRIT,EPSI,TINI,LTINI,
     &                                        TFIN,LTFIN,0.D0,RMS)
C
            DEUXG=2.D0*PESA
C
            ARIAS = RMS**2 *PI / (DEUXG)*(TFIN-TINI)
C
C           --- BORNES INFERIEURE ET SUPERIEURE (EN % DU MAXIMUM  ---
C           --- DE L'INTENSITE ARIAS) CONSIDEREES POUR DETERMINER ---
C           --- LA PHASE FORTE, ET VALEURS CORRESPONDANTES        ---
C           --- EN % DE L'INTENSITE ARIAS ("VALINF" ET "VALSUP")  ---
            CALL GETVR8(NOMOPE,'BORNE_INF', 1,1,1, BINF,  L)
            VALINF = ARIAS * BINF
            CALL GETVR8(NOMOPE,'BORNE_SUP', 1,1,1, BSUP,  L)
            VALSUP = ARIAS * BSUP
C
C           --- CALCUL DE LA DUREE DE LA PHASE FORTE ---           
            CALL FOCPHF(NOMFON,'TRAPEZE',CRIT,EPSI,TINI,LTINI,TFIN,
     &                     LTFIN,VALINF,VALSUP,DPHFOR,PESA)
C
            K=K+1
            NOPIND(K)   = 'DUREE_PHAS_FORT'
            VALIND(K-1) = DPHFOR
C
          ENDIF
C
C
C        --- INTENSITE SPECTRALE D'UN SIGNAL TEMPOREL ---
          IF ( LISPEC .OR. LTOUT ) THEN
C
             IF ( LAMOR .EQ. 0 ) THEN
                CALL UTMESS('F',NOMOPE,'COEFFICIENT D AMORTISSEMENT :'
     &          //' POUR LE CACUL DE L INTENSITE SPECTRALE, LE'
     &          //' MOT-CLE "AMOR_REDUIT" DOIT OBLIGATOIREMENT ETRE'
     &          //' RENSEIGNE.')
             ENDIF
C
C        --- SPECTRE D'OSCILLATEUR DU SIGNAL TEMPOREL ---
C
C              --- FONCTION A TRANSFORMER ---
            CALL FOVECA(NOMOPE,NOMFON,IER)
            IF (IER .NE. 0 ) GOTO 9999
C
C              --- CONDITIONS INITIALES ---
C
            CALL UTMESS('I','INTENSITE SPECTRALE','ON CALCULE'
     &      //' LE SPECTRE DE REPONSE D OSCILLATEUR EN VITESSE,'
     &      //' UTILE POUR CALCULER L INTENSITE SPECTRALE. ')
C
C           --- CALCUL DU SPECTRE D'OSCILLATEUR ---
            CALL FOCASO(NOMFON,'NIGAM',1,AMOR,NBFREQ,ZR(LFREQ),
     +                  NORME,'VITE','ACCE',SORTIE,BASE)
C
C
C           --- SRO A ETUDIER : "SORTIE"            ---
C           --- "SORTIE" EST UNE NAPPE F(AMOR,FREQ) ---
C
C           --- CALCUL DE L INTENSITE SPECTRALE ---           
            CALL FOINSP('TRAPEZE',SORTIE,AMOR,CRIT,EPSI,FINI,
     &                                           FFIN,ISPEC)
C
            K=K+1
            NOPIND(K)   = 'FREQ_INIT'
            VALIND(K-1) = FINI
            NOPIND(K+1) = 'FREQ_FIN'
            VALIND(K)   = FFIN
            NOPIND(K+2) = 'AMOR_REDUIT'
            VALIND(K+1) = AMOR
            NOPIND(K+3) = 'INTE_SPECT'
            VALIND(K+2) = ISPEC
C
            K=K+3
C
          ENDIF       
C
          ELSE
          CALL UTMESS('F','OP0091','LA FONCTION DOIT ETRE DE TYPE ACCE')
          ENDIF 
C
C
C        --- NOUS ABORDONS ICI LE CAS OU LA FONCTION A ETUDIER ---
C        --- EST UNE NAPPE DE SRO                              ---
C
         ELSE 
          CALL GETVID(NOMOPE,'SPEC_OSCI',1,1,1,NOMFON,L)
          PROL = NOMFON//'.PROL'
          CALL JEVEUO(PROL,'L',LPRO)
          NOMRES = ZK16(LPRO+3)
          IF (NOMRES(1:4).EQ.'ACCE') THEN
C
C        --------------------------------
C        --- ETUDE DE LA NAPPE DE SRO ---
C        --------------------------------
C
C        --- VALEURS DES 2E ET 3E COLONNES DU TABLEAU DE RESULTATS ---
            NOPIND(2)  = 'FREQ_INIT'
            VALIND(1)  = FINI
            NOPIND(3)  = 'FREQ_FIN'
            VALIND(2)  = FFIN
C
            K=3
C
C        --- INTENSITE SPECTRALE D'UNE NAPPE DE SRO ---
            CALL UTMESS('I','INTENSITE SPECTRALE','AVANT DE CALCULER L'
     &      //' INTENSITE SPECTRALE, IL EST PRUDENT DE VERIFIER LA'
     &      //' NORME DE LA NAPPE SUR LAQUELLE PORTE LE CALCUL'
     &      //' CECI PEUT ETRE UNE SOURCE D ERREURS.')
C
           
          IF ( LISPEC .OR. LTOUT ) THEN
C
             IF ( LAMOR .EQ. 0 ) THEN
                CALL UTMESS('F',NOMOPE,'COEFFICIENT D AMORTISSEMENT :'
     &          //' POUR LE CACUL DE L INTENSITE SPECTRALE, LE'
     &          //' MOT-CLE "AMOR_REDUIT" DOIT OBLIGATOIREMENT ETRE'
     &          //' RENSEIGNE.')
             ENDIF
C
             CALL GETVTX(NOMOPE,'NATURE',1,1,1,NATURE,L)
             IF ( NATURE .EQ. '  ' ) THEN
                CALL UTMESS('F','MOT-CLE NATURE OBLIGATOIRE','POUR '
     &          //' LE MOT-CLE SPEC_OSCI')
             ENDIF
             CALL FOINS3('TRAPEZE',NOMFON,NATURE,AMOR,CRIT,EPSI,
     &                           FINI,FFIN,ISPEC)
C
             K=K+1
             NOPIND(K)   = 'AMOR_REDUIT'
             VALIND(K-1) = AMOR
             NOPIND(K+1) = 'INTE_SPECT'
             VALIND(K)   = ISPEC
C
             K=K+1
C
          ENDIF
C
C
          ELSE
          CALL UTMESS('F','OP0091 ','LE SPECTRE DOIT ETRE DE TYPE ACCE')
          ENDIF
       ENDIF
C
C        ------------------------------
C        --- ECRITURE DES RESULTATS ---
C        ------------------------------
C
C        --- TABLEAU DE RESULTATS ---
         CALL TBCRSD ( SORTIE , BASE )
         CALL TBAJPA ( SORTIE, K, NOPIND, TYPIND)
         CALL TBAJLI ( SORTIE,K,NOPIND,IBID,VALIND,C16B,NOMFON,0)

C        --- DESTRUCTION DES TABLEAUX DE TRAVAIL ---
         CALL JEDETR ( '&&FONOCI.LISTE.FREQ' )
         CALL JEDETR ( '&&FONOCI.OPTION' )
C
 9999    CONTINUE    
         CALL JEDEMA()
         END
