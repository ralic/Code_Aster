      SUBROUTINE PAQMAI(NOMSD, NOMU, NOMMET, NOMCRI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/11/2002   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE F1BHHAJ
      IMPLICIT     NONE
      CHARACTER*8  NOMSD, NOMU
      CHARACTER*16 NOMMET, NOMCRI
C ---------------------------------------------------------------------
C BUT: DETERMINER LE PLUS PETIT CERCLE CIRCONSCRIT AUX POINTS
C      REPRESANTANT LE VECTEUR DE CISAILLEMENT TAU DANS LE PLAN u, v.
C ---------------------------------------------------------------------
C ARGUMENTS:
C NOMSD      IN    K8 : NOM DE LA STRUCTURE DE DONNEES RESULTAT. 
C NOMU       IN    K8 : NOM UTILISATEUR DU CALCUL EN FATIGUE. 
C NOMMET     IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
C                       CIRCONSCRIT.
C NOMCRI     IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
C
C-----------------------------------------------------------------------
C---- COMMUNS NORMALISES  JEVEUX
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
      CHARACTER*32 ZK32,JEXNOM,JEXNUM,JEXATR
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
      INTEGER       JCERD, JCERL, JCERV
      INTEGER       IBID, IERD, LORDR(10), NBORDR, NDIM, IRET, JCESD
      INTEGER       NBMA, JNBPG, IMA, TDISP, JRWORK, TPAQ, NBPAQ, NUMPAQ
      INTEGER       NMAPAQ, BORMAX, NBPMAX, JNBPAQ
      INTEGER       NMAINI, NBMAP, TSPAQ, IORDR, LOR8EM, LOISEM 
      INTEGER       JSIG, K, IMAP, NBPG, SOMPG, IPG, ICMP
C
      REAL*8        R8B, VAL1, VRESU(18)
C
      COMPLEX*16    C16B
C
      CHARACTER*4   LSIG(6)
      CHARACTER*8   K8B, LICMP(22)
      CHARACTER*16  TYPRES
      CHARACTER*19  CESR, LIGRE, CELBID, CHSIG, CHSIGS, CES1, CES2
      CHARACTER*24  TEXTE
C
C-----------------------------------------------------------------------
C234567                                                              012
C-----------------------------------------------------------------------
      DATA  LSIG/ 'SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ', 'SIYZ' /
C-----------------------------------------------------------------------
C
      CALL JEMARQ()

C CONSTRUCTION DU CHAMP SIMPLE DESTINE A RECEVOIR LES RESULTATS :
C DTAUM,....
C
      CALL RSEXCH( NOMSD, 'SIEF_ELGA', 1, CHSIG, IRET )     
      CALL DISMOI('F','NOM_LIGREL',CHSIG,'CHAM_ELEM',IBID,LIGRE,IERD)
      CESR = '&&PAQMAI.FACY'
      CELBID = '&&PAQMAI.BID'
      CALL ALCHML(LIGRE,'TOU_INI_ELGA','PFACY_R','V',CELBID,IERD,' ')
      CALL CELCES( CELBID, 'V', CESR )      

C RECUPERATION DES CONTRAINTES AUX POINTS DE GAUSS
C
      CALL DISMOI('F','TYPE_RESU',NOMSD,'RESULTAT',IBID,TYPRES,IERD)
      IF ( (TYPRES(1:9) .NE. 'EVOL_ELAS') .AND.
     &     (TYPRES(1:9) .NE. 'EVOL_NOLI') ) THEN
          CALL UTMESS('F', 'PAQMAI.1', 'LE TYPE DU CONCEPT RESULTAT '//
     &                ' N''EST NI EVOL_ELAS, NI EVOL_NOLI.')
      ENDIF

C RECUPERATION DU NOMBRE DE NUMEROS D'ORDRE ET DE LA LISTE 
C DES NUMEROS D'ORDRE
C
      CALL RSORAC( NOMSD, 'TOUT_ORDRE', IBID, R8B, K8B, C16B, R8B, K8B,
     &             LORDR, 1, NBORDR )
C
      IF ( NBORDR .LT. 0 ) THEN
         NDIM = -NBORDR
      ELSEIF (NBORDR .GT. 0) THEN
         NDIM = NBORDR
      ENDIF
C
      CALL RSORAC( NOMSD, 'TOUT_ORDRE', IBID, R8B, K8B, C16B, R8B, K8B,
     &             LORDR, NDIM, NBORDR )

C RECUPERATION DU NOMBRE DE MAILLES ET DU NOMBRE DE POINTS DE GAUSS 
C PAR MAILLE
C
      CALL RSEXCH( NOMSD, 'SIEF_ELGA', 1, CHSIG, IRET )      
      CHSIGS = '&&PAQMAI.SIELGA'
      CALL CELCES( CHSIG, 'V', CHSIGS )
      CALL JEVEUO(CHSIGS(1:19)//'.CESD','L',JCESD)
      NBMA = ZI(JCESD-1+1)
      CALL WKVECT( '&&PAQMAI.NBPG', 'V V I', NBMA, JNBPG )

      DO 50 IMA=1, NBMA
         ZI(JNBPG - 1 + IMA) = ZI(JCESD-1 + 5 + 4*(IMA-1) + 1)
 50   CONTINUE
C
C CONSTRUCTION DES PAQUETS DE MAILLES.
C
C 1/ DIMENSIONNEMENT DU VECTEUR DE TRAVAIL (RWORK) ET DU VECTEUR
C    CONTENANT LES CARACTERISTIQUES DES PAQUETS DE MAILLES (PAQMA).
C    JEDISP REND LA DIMENSION EN ENTIERS, ON LA CONVERTIT A L'AIDE
C    DES FONCTIONS ENVIMA POUR ALLOUER UN TABLEAU DE REELS
      CALL JEDISP(1, TDISP)
      TDISP =  (TDISP * LOISEM()) / LOR8EM() 
      TDISP = INT(0.5D0*TDISP)
      CALL WKVECT( '&&PAQMAI.RWORK', 'V V R', TDISP, JRWORK )
C
      BORMAX = NBMA*27*NBORDR*6
      VAL1 = DBLE(TDISP)/DBLE(BORMAX)
      IF (VAL1 .LT. 1.0D0) THEN
         NBPMAX = INT(1.0D0/VAL1) + 1
      ELSE
         NBPMAX = 2
      ENDIF
      CALL WKVECT( '&&PAQMAI.PAQMA', 'V V I', NBPMAX*4, JNBPAQ )
C
      TPAQ = 0
      NBPAQ = 0
      NUMPAQ = 0
      NMAPAQ = 0
      DO 100 IMA=1, NBMA
         TPAQ = TPAQ + ZI(JNBPG - 1 + IMA)*NBORDR*6
         NMAPAQ = NMAPAQ + 1

         IF ( TPAQ .LT. TDISP ) THEN
            IF (IMA .EQ. NBMA) THEN
               NUMPAQ = NUMPAQ + 1
               ZI(JNBPAQ + (NUMPAQ-1)*4) = NUMPAQ
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
               NBPAQ = NUMPAQ
               IF (IMA .EQ. 1) THEN
                  ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = 1
                  ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = 1
               ELSE
                  ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = (IMA - NMAPAQ + 1)
                  ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NMAPAQ
               ENDIF           
            ENDIF

         ELSEIF ( ( TPAQ .GT. TDISP ) .AND. (NMAPAQ .EQ. 1) ) THEN
            CALL UTDEBM('F', 'PAQMAI.2', 'LA TAILLE MEMOIRE '//
     &                   ' NECESSAIRE AU VECTEUR DE TRAVAIL DANS '//
     &                   ' LEQUEL NOUS STOCKONS LES COMPOSANTES '//
     &                   ' u ET v DU VECTEUR TAU EST TROP IMPORTANTE '//
     &                   ' PAR RAPPORT A LA PLACE DISPONIBLE.')
            CALL UTIMPI('L', 'TAILLE DISPONIBLE : ', 1, TDISP)
            CALL UTIMPI('L', 'TAILLE NECESSAIRE : ', 1, TPAQ)
            CALL UTFINM( )

C 2/ STOCKAGE DES NUMEROS DES PAQUETS, DE LA TAILLE DES PAQUETS,
C    DU NUMERO DE LA PREMIERE MAILLE DE CHAQUE PAQUET DE MAILLES,
C    DU NOMBRE DE MAILLE DE CHAQUE PAQUET ET DU NOMBRE DE PAQUET. 

         ELSEIF ( ( TPAQ .GE. TDISP ) .AND. (NMAPAQ .GT. 1) ) THEN
            TPAQ = TPAQ - ZI(JNBPG-1 + IMA)*NBORDR*6
            NUMPAQ = NUMPAQ + 1
            ZI(JNBPAQ + (NUMPAQ-1)*4) = NUMPAQ
            ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
            ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = (IMA - NMAPAQ + 1)
            ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NMAPAQ - 1
            NBPAQ = NUMPAQ
C
            TPAQ = ZI(JNBPG - 1 + IMA)*NBORDR*6
            NMAPAQ = 1
            IF (IMA .EQ. NBMA) THEN
               NUMPAQ = NUMPAQ + 1
               ZI(JNBPAQ + (NUMPAQ-1)*4) = NUMPAQ
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = IMA
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NMAPAQ
               NBPAQ = NUMPAQ
            ENDIF
            IF (NBPAQ .GT. (NBPMAX-1)) THEN
               CALL UTMESS('F', 'PAQMAI.3', 'LA TAILLE DU VECTEUR ' //
     &                     'CONTENANT LES CARACTERISTIQUES DES ' //
     &                     'PAQUETS DE MAILLES EST TROP PETITE.' //
     &                     'ERREUR FORTRAN, EMETTRE UNE AL.')
            ENDIF
         ENDIF

 100  CONTINUE

C
C TRAITEMENT DES PAQUETS DE MAILLES.
C
C  <<REMPLISSAGE>> DU VECTEUR DE TRAVAIL
C
      DO 200 NUMPAQ=1, NBPAQ 
         CALL JERAZO('&&PAQMAI.RWORK', TDISP, 1)
         TPAQ = ZI(JNBPAQ + (NUMPAQ-1)*4 + 1)
         NMAINI = ZI(JNBPAQ + (NUMPAQ-1)*4 + 2)
         NBMAP = ZI(JNBPAQ + (NUMPAQ-1)*4 + 3)
         TSPAQ = TPAQ/NBORDR
C
         DO 220 IORDR=1, NBORDR
            CALL RSEXCH( NOMSD, 'SIEF_ELGA', IORDR, CHSIG, IRET )
            CES1 = '&&PAQMAI.SIG_S1'
            CES2 = '&&PAQMAI.SIG_ORDO'
            TEXTE = CHSIG(1:19)//'.CELV'
            CALL UTIMSD('MESSAGE',2,.TRUE.,.TRUE.,TEXTE,1,' ')
            CALL CELCES(CHSIG, 'V', CES1)
            TEXTE = CES1(1:19)//'.CESV'
            CALL UTIMSD('MESSAGE',2,.TRUE.,.TRUE.,TEXTE,1,' ')
            CALL CESRED(CES1, 0, IBID, 6, LSIG, 'V', CES2)
            TEXTE = CES2(1:19)//'.CESV'
            CALL UTIMSD('MESSAGE',2,.TRUE.,.TRUE.,TEXTE,1,' ')
            CALL JEEXIN(CES2(1:19)//'.CESV', IRET)
            IF (IRET .EQ. 0) THEN
               CALL UTMESS('F', 'PAQMAI.4', 'LES CHAMPS DE '//
     &                 ' CONTRAINTES AUX POINTS DE GAUSS ' //
     &                 'N''EXISTENT PAS.')
            ENDIF
            CALL JEVEUO(CES2(1:19)//'.CESV', 'L', JSIG)
C
            SOMPG = 0
            K = 0
C
            DO 240 IMAP=NMAINI, NMAINI+(NBMAP-1)
               IF (IMAP .GT. NMAINI) THEN
                  K = 1
               ENDIF
               NBPG = ZI(JNBPG + IMAP-1)
               SOMPG = SOMPG + NBPG
C
               DO 260 IPG=1, NBPG
                  DO 280 ICMP=1, 6
                     ZR( JRWORK + (ICMP-1) + (IPG-1)*6 + 
     &                  K*SOMPG*6 + (IORDR-1)*TSPAQ ) =
     &               ZR( JSIG + (ICMP-1) + (IPG-1)*6 + K*SOMPG*6 )
 280              CONTINUE
 260           CONTINUE
 240        CONTINUE
C
            IF (IORDR .EQ. NBORDR) THEN
               CALL DELTAU (JRWORK, JNBPG, NBORDR, NMAINI, NBMAP,
     &                      TSPAQ, NOMMET, NOMCRI, CESR)
            ENDIF
 220     CONTINUE   
 200  CONTINUE
C
C TRANSFORMATION D'UN CHAM_ELEM SIMPLE EN CHAM_ELEM
C
      CALL RSEXCH( NOMSD, 'SIEF_ELGA', 1, CHSIG, IRET )
      CALL DISMOI('F','NOM_LIGREL',CHSIG,'CHAM_ELEM',IBID,LIGRE,IERD)
      CALL CESCEL(CESR,LIGRE,'TOU_INI_ELGA',' ','NON','G',NOMU)
C
C MENAGE
C
      CALL DETRSD('CHAM_ELEM_S',CHSIGS)
      CALL DETRSD('CHAM_ELEM_S',CESR)
      CALL DETRSD('CHAM_ELEM_S',CES1)
      CALL DETRSD('CHAM_ELEM_S',CES2)
C
      CALL JEDETR('&&PAQMAI.RWORK')
      CALL JEDETR('&&PAQMAI.PAQMA')
      CALL JEDETR('&&PAQMAI.NBPG')
C
      CALL JEDEMA()
      END
