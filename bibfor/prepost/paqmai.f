      SUBROUTINE PAQMAI(NOMSD, NOMU, NOMMAI, NOMMET, NOMCRI,
     &                  TYPCHA, PROAXE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 30/08/2005   AUTEUR VABHHTS J.PELLET 
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
C RESPONSABLE F1BHHAJ J.ANGLES
C TOLE  CRP_20
      IMPLICIT     NONE
      CHARACTER*8  NOMSD, NOMU, NOMMAI
      CHARACTER*16 NOMMET, NOMCRI, TYPCHA, PROAXE
C ---------------------------------------------------------------------
C BUT: DETERMINER LE PLUS PETIT CERCLE CIRCONSCRIT AUX POINTS
C      REPRESANTANT LE VECTEUR DE CISAILLEMENT TAU DANS LE PLAN u, v.
C ---------------------------------------------------------------------
C ARGUMENTS:
C NOMSD      IN    K8 : NOM DE LA STRUCTURE DE DONNEES RESULTAT.
C NOMU       IN    K8 : NOM UTILISATEUR DU CALCUL EN FATIGUE.
C NOMMAI     IN    K8 : NOM UTILISATEUR DU MAILLAGE.
C NOMMET     IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
C                       CIRCONSCRIT.
C NOMCRI     IN    K16: NOM DU CRITERE.
C TYPCHA     IN    K16: TYPE DE CHARGEMENT (PERIODIQUE OU NON).
C PROAXE     IN    K16: TYPE DE PROJECTION (UN OU DEUX AXES).
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
      INTEGER       IBID, IERD, LORDR, JORDR, NBORDR, NDIM, IRET, JCESD
      INTEGER       NBMA, NBPGT, NBPGMX, JNBPG, IMA, TDISP, JRWORK, TPAQ
      INTEGER       NBPAQ, NUMPAQ, NMAPAQ, NBCMP, BORMAX, NBPMAX, JNBPAQ
      INTEGER       NMAINI, NBMAP, TSPAQ, IORDR, LOR8EM, LOISEM, JAD
      INTEGER       JSIGV, JSIGD, JSIGL, K, IMAP, NBPG, IPG, ICMP, IRET1
      INTEGER       JEPSV, JEPSD, JEPSL
      INTEGER       I, KWORK, SOMPGW, KCMP, SOMPGS, SOMPGI, JMAIL, JGRMA
      INTEGER       N, NINIT, NBPGGM, NBMAGM, NMEMO,NNCP
C
      REAL*8        R8B, VAL1
C
      COMPLEX*16    C16B
C
      CHARACTER*4   LSIG(6), LEPS(6)
      CHARACTER*8   K8B, MOTCLE(4), TYMOCL(4)
      CHARACTER*16  TYPRES
      CHARACTER*19  LISMAI
      CHARACTER*19  CESR, LIGRE, CELBID, CHSIG, CHSIGS, CES1, CES2
      CHARACTER*19  CHEPS, CES3, CES4
C
C-----------------------------------------------------------------------
C234567                                                              012
C-----------------------------------------------------------------------
      DATA  LSIG/ 'SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ', 'SIYZ' /
C
      DATA  LEPS/ 'EPXX', 'EPYY', 'EPZZ', 'EPXY', 'EPXZ', 'EPYZ' /
C-----------------------------------------------------------------------
C
      CALL JEMARQ()

C RECUPERATION DU TYPE DE CALCUL MECANIQUE EFFECTUE

      CALL DISMOI('F','TYPE_RESU',NOMSD,'RESULTAT',IBID,TYPRES,IERD)
      IF ( (TYPRES(1:9) .NE. 'EVOL_ELAS') .AND.
     &     (TYPRES(1:9) .NE. 'EVOL_NOLI') ) THEN
          CALL UTMESS('F', 'PAQMAI.1', 'LE TYPE DU CONCEPT RESULTAT '//
     &                ' N''EST NI EVOL_ELAS, NI EVOL_NOLI.')
      ENDIF

C CONSTRUCTION DU CHAMP SIMPLE DESTINE A RECEVOIR LES RESULTATS :
C DTAUM,....

      IF ( TYPRES .EQ. 'EVOL_ELAS' ) THEN
         CALL RSEXCH( NOMSD, 'SIEF_ELGA_DEPL', 1, CHSIG, IRET )
      ELSEIF ( TYPRES .EQ. 'EVOL_NOLI' ) THEN
         CALL RSEXCH( NOMSD, 'SIEF_ELGA', 1, CHSIG, IRET )
      ENDIF

      CALL DISMOI('F','NOM_LIGREL',CHSIG,'CHAM_ELEM',IBID,LIGRE,IERD)
      CESR = '&&PAQMAI.FACY'
      CELBID = '&&PAQMAI.BID'
      CALL ALCHML(LIGRE,'TOU_INI_ELGA','PFACY_R','V',CELBID,IERD,' ')
      CALL CELCES( CELBID, 'V', CESR )

C RECUPERATION DU NOMBRE DE NUMEROS D'ORDRE ET DE LA LISTE
C DES NUMEROS D'ORDRE

      CALL RSORAC( NOMSD, 'TOUT_ORDRE', IBID, R8B, K8B, C16B, R8B, K8B,
     &             LORDR, 1, NBORDR )

      IF ( NBORDR .LT. 0 ) THEN
         NDIM = -NBORDR
      ELSEIF (NBORDR .GT. 0) THEN
         NDIM = NBORDR
      ENDIF

      CALL WKVECT('&&PAQMAI.NUME_ORDRE','V V I',NDIM,JORDR)
      CALL RSORAC( NOMSD, 'TOUT_ORDRE', IBID, R8B, K8B, C16B, R8B, K8B,
     &             ZI(JORDR), NDIM, NBORDR )

      IF (ZI(JORDR) .EQ. 0) THEN
         CALL UTMESS('A','PAQMAI',
     &   'VOUS AVEZ PROBABLEMENT ARCHIVE L ETAT INITIAL DANS'//
     &   ' LA COMMANDE STAT_NON_LINE.'//
     &   ' CELA CORRESPOND AU NUMERO D ORDRE 0. NOUS NE TENONS PAS'//
     &   ' COMPTE DU RESULTAT A CE NUMERO D ORDRE POUR LE CALCUL DE'//
     &   ' DE LA FATIGUE.')
         NBORDR = NBORDR - 1
      ENDIF

C RECUPERATION DU NOMBRE DE MAILLES ET DU NOMBRE DE POINTS DE GAUSS
C PAR MAILLE

C CAS OU L'ON CALCULE LA FATIGUE SUR TOUT LE MAILLAGE
C      IF ( NOMMAI .EQ. '        ' ) THEN
      IF ( TYPRES .EQ. 'EVOL_ELAS' ) THEN
         CALL RSEXCH( NOMSD, 'SIEF_ELGA_DEPL', 1, CHSIG, IRET )
      ELSEIF ( TYPRES .EQ. 'EVOL_NOLI' ) THEN
         CALL RSEXCH( NOMSD, 'SIEF_ELGA', 1, CHSIG, IRET )
      ENDIF
      CHSIGS = '&&PAQMAI.SIELGA'
      CALL CELCES( CHSIG, 'V', CHSIGS )
      CALL JEVEUO(CHSIGS(1:19)//'.CESD','L',JCESD)
      NBMA = ZI(JCESD-1+1)

C CAS OU L'ON CALCULE LA FATIGUE SUR UN OU DES GROUPES DE MAILLES

      CALL WKVECT( '&&PAQMAI.NBMAGR', 'V V I', 1, JGRMA )
      CALL JERAZO( '&&PAQMAI.NBMAGR', 1, 1)
      NBMAGM = 0
      IF ( NOMMAI .NE. '        ' ) THEN
         LISMAI = '&&PAQMAI.L_MAILLES'
         MOTCLE(1) = 'GROUP_MA'
         TYMOCL(1) = 'GROUP_MA'
         MOTCLE(2) = 'MAILLE'
         TYMOCL(2) = 'MAILLE'
         CALL RELIEM( ' ', NOMMAI, 'NU_MAILLE', ' ', 0, 2, MOTCLE,
     &                TYMOCL, LISMAI, NBMAGM)
         CALL JEVEUO ( LISMAI, 'L', JMAIL )
         CALL JEDETR('&&PAQMAI.NBMAGR')
         CALL WKVECT( '&&PAQMAI.NBMAGR', 'V V I', NBMAGM, JGRMA )
         DO 40 I=1, NBMAGM
            ZI(JGRMA-1 + I) = ZI(JMAIL-1 + I)
 40      CONTINUE
      ENDIF
      CALL WKVECT( '&&PAQMAI.NBPG', 'V V I', NBMA, JNBPG )

C  NBPGMX : NOMBRE DE POINTS DE GAUSS DANS LES ELEMENTS
C           QUI EN ONT LE PLUS, (EX : ELEMENT 3D = 27)
C  NBPGT  : NOMBRE TOTAL DE POINTS DE GAUSS DANS LE MAILLAGE

      NBPGMX = 0
      NBPGT = 0
      NBPGGM = 0

      DO 50 IMA=1, NBMA
         ZI(JNBPG - 1 + IMA) = ZI(JCESD-1 + 5 + 4*(IMA-1) + 1)
         NBPGT = NBPGT + ZI(JCESD-1 + 5 + 4*(IMA-1) + 1)
         IF ( ZI(JCESD-1 + 5 + 4*(IMA-1) + 1) .GT. NBPGMX ) THEN
            NBPGMX = ZI(JCESD-1 + 5 + 4*(IMA-1) + 1)
         ENDIF
 50   CONTINUE
      IF ( NOMMAI .NE. '        ' ) THEN
         DO 60 IMA=1, NBMAGM
            NBPGGM=NBPGGM + ZI(JCESD-1 + 5 + 4*(ZI(JMAIL+IMA-1)-1) + 1)
 60      CONTINUE
         WRITE(6,*)'NOMBRE DE POINTS DE GAUSS DU GROUPE DE MAILLES ==>',
     &              NBPGGM
         WRITE(6,*)' '
      ENDIF
      WRITE(6,*)'NOMBRE TOTAL DE POINTS DE GAUSS A TRAITER ==>',NBPGT
      WRITE(6,*)' '

      WRITE(6,*)'NUMERO DU PAQUET DE MAILLES  -  ' //
     &           'NOMBRE DE POINTS DE GAUSS TRAITES'

C CONSTRUCTION DES PAQUETS DE MAILLES.

C 1/ DIMENSIONNEMENT DU VECTEUR DE TRAVAIL (RWORK) ET DU VECTEUR
C    CONTENANT LES CARACTERISTIQUES DES PAQUETS DE MAILLES (PAQMA).
C    JEDISP REND LA DIMENSION EN ENTIERS, ON LA CONVERTIT A L'AIDE
C    DES FONCTIONS ENVIMA POUR ALLOUER UN TABLEAU DE REELS.
      CALL JEDISP(1, TDISP)
      TDISP =  (TDISP * LOISEM()) / LOR8EM()
      TDISP = INT(0.6D0*TDISP)
      CALL WKVECT( '&&PAQMAI.RWORK', 'V V R', TDISP, JRWORK )

      IF ( NOMCRI(1:12) .EQ. 'FATEMI_SOCIE' ) THEN
         NBCMP = 12
      ELSE
         NBCMP = 6
      ENDIF
      BORMAX = NBMA*NBPGMX*NBORDR*NBCMP
      VAL1 = DBLE(TDISP)/DBLE(BORMAX)

      IF (VAL1 .LT. 1.0D0) THEN
         NBPMAX = INT(1.0D0/VAL1) + 1
      ELSE
         NBPMAX = 2
      ENDIF
      CALL WKVECT( '&&PAQMAI.PAQMA', 'V V I', NBPMAX*4, JNBPAQ )

C TPAQ   = TAILLE DU PAQUET DE MAILLES
C NBPAQ  = NOMBRE DE PAQUET(S) DE MAILLES
C NUMPAQ = NUMERO DU PAQUET DE MAILLES
C NMAPAQ = NOMBRE DE MAILLES DANS LE PAQUET DE MAILLES
C ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = NUMERO DE LA MAILLE INITIALE
C                                 DANS LE PAQUET

      TPAQ = 0
      NBPAQ = 0
      NUMPAQ = 0
      NMAPAQ = 0

      DO 100 IMA=1, NBMA
         TPAQ = TPAQ + ZI(JNBPG - 1 + IMA)*NBORDR*NBCMP
         NMAPAQ = NMAPAQ + 1

         IF ( TPAQ .LT. TDISP ) THEN
            IF (IMA .EQ. NBMA) THEN
               NUMPAQ = NUMPAQ + 1
               ZI(JNBPAQ + (NUMPAQ-1)*4) = NUMPAQ
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
                 ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = IMA - (NMAPAQ - 1)
                 ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NMAPAQ
                 NBPAQ = NUMPAQ
            ENDIF

         ELSEIF ( ( TPAQ .GE. TDISP ) .AND. (IMA .LT. 3) ) THEN
            CALL UTDEBM('F', 'PAQMAI.2', 'LA TAILLE MEMOIRE '//
     &                   ' NECESSAIRE AU VECTEUR DE TRAVAIL '//
     &                   ' EST TROP IMPORTANTE '//
     &                   ' PAR RAPPORT A LA PLACE DISPONIBLE.')
            CALL UTIMPI('L', 'TAILLE DISPONIBLE : ', 1, TDISP)
            CALL UTIMPI('L', 'TAILLE NECESSAIRE : ', 1, TPAQ)
            CALL UTFINM( )

C 2/ STOCKAGE DES NUMEROS DES PAQUETS, DE LA TAILLE DES PAQUETS,
C    DU NUMERO DE LA PREMIERE MAILLE DE CHAQUE PAQUET DE MAILLES,
C    DU NOMBRE DE MAILLE DE CHAQUE PAQUET ET DU NOMBRE DE PAQUET.

         ELSEIF ( ( TPAQ .GE. TDISP ) .AND. (IMA .GT. 2) ) THEN
C ON RECULE DE DEUX MAILLES POUR ETRE SUR DE NE PAS DEBORDER DU VECTEUR
C DE TRAVAIL (JRWORK).

            TPAQ = TPAQ - ZI(JNBPG-1 + IMA)*NBORDR*NBCMP
            TPAQ = TPAQ - ZI(JNBPG-1 + IMA-1)*NBORDR*NBCMP

            NUMPAQ = NUMPAQ + 1
            ZI(JNBPAQ + (NUMPAQ-1)*4) = NUMPAQ
            ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
            ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = (IMA - NMAPAQ + 1)
            ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NMAPAQ - 2
            NBPAQ = NUMPAQ

            TPAQ = ZI(JNBPG-1 + IMA-1)*NBORDR*NBCMP
            TPAQ = TPAQ + ZI(JNBPG-1 + IMA)*NBORDR*NBCMP
            NMAPAQ = 2
            IF (IMA .EQ. NBMA) THEN
               NUMPAQ = NUMPAQ + 1
               ZI(JNBPAQ + (NUMPAQ-1)*4) = NUMPAQ
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = IMA - (NMAPAQ - 1)
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NMAPAQ
               NBPAQ = NUMPAQ
            ENDIF
         ENDIF

 100  CONTINUE

      IF (NBPAQ .GT. (NBPMAX-1)) THEN
         CALL UTMESS('F', 'PAQMAI.3', 'LA TAILLE DU VECTEUR ' //
     &               'CONTENANT LES CARACTERISTIQUES DES ' //
     &               'PAQUETS DE MAILLES EST TROP PETITE.' //
     &               'ERREUR FORTRAN, EMETTRE UNE AL.')
      ENDIF

C TRAITEMENT DES PAQUETS DE MAILLES.
C
C  <<REMPLISSAGE>> DU VECTEUR DE TRAVAIL

      SOMPGI = 0
      NMEMO = 0

      DO 200 NUMPAQ=1, NBPAQ
         CALL JERAZO('&&PAQMAI.RWORK', TDISP, 1)
         TPAQ = ZI(JNBPAQ + (NUMPAQ-1)*4 + 1)
         NMAINI = ZI(JNBPAQ + (NUMPAQ-1)*4 + 2)
         NBMAP = ZI(JNBPAQ + (NUMPAQ-1)*4 + 3)
         TSPAQ = TPAQ/NBORDR

         IF ( NUMPAQ .GT. 1 ) THEN
            SOMPGI = SOMPGS
         ENDIF

         IF ( NOMCRI(1:12) .NE. 'FATEMI_SOCIE' ) THEN

            DO 220 IORDR=1, NBORDR
               IF ( (NUMPAQ .GT. 1) .AND. (IORDR .EQ. 1) ) THEN
                  NINIT = NMEMO
               ELSEIF ( (NUMPAQ .EQ. 1) .AND. (IORDR .EQ. 1) ) THEN
                  NINIT = NMAINI
               ENDIF
               N = NINIT

               IF ( TYPRES .EQ. 'EVOL_ELAS' ) THEN
                  CALL RSEXCH( NOMSD, 'SIEF_ELGA_DEPL', IORDR, CHSIG,
     &                         IRET )
               ELSEIF ( TYPRES .EQ. 'EVOL_NOLI' ) THEN
                  CALL RSEXCH( NOMSD, 'SIEF_ELGA', IORDR, CHSIG, IRET )
               ENDIF
               IF (IRET .NE. 0) THEN
                  CALL UTMESS('F', 'PAQMAI.4', 'LES CHAMPS DE '//
     &                    'CONTRAINTES AUX POINTS DE GAUSS '//
     &                    'SIGM_NOEU_DEPL OU SIEF_NOEU_ELGA '//
     &                    'SIEF_NOEU_ELGA N''ONT PAS ETE CALCULES.')
               ENDIF
               CES1 = '&&PAQMAI.SIG_S1'
               CES2 = '&&PAQMAI.SIG_ORDO'
               CALL CELCES(CHSIG, 'V', CES1)
               CALL CESRED(CES1, 0, IBID, 6, LSIG, 'V', CES2)
               CALL JEEXIN(CES2(1:19)//'.CESV', IRET)
               IF (IRET .EQ. 0) THEN
                  CALL UTMESS('F', 'PAQMAI.5', 'LES CHAMPS DE '//
     &                    ' CONTRAINTES AUX POINTS DE GAUSS ' //
     &                    'N''EXISTENT PAS.')
               ENDIF
               CALL JEVEUO(CES2(1:19)//'.CESD', 'L', JSIGD)
               CALL JEVEUO(CES2(1:19)//'.CESL', 'L', JSIGL)
               CALL JEVEUO(CES2(1:19)//'.CESV', 'L', JSIGV)

               IF ( NUMPAQ .EQ. 1 ) THEN
                  KCMP = 0
                  SOMPGS = 0
               ELSEIF ( NUMPAQ .GT. 1 ) THEN
                  KCMP = 1
                  SOMPGS = SOMPGI
               ENDIF
               SOMPGW = 0
               KWORK = 0

               DO 240 IMAP=NMAINI, NMAINI+(NBMAP-1)
                  IF ( (IMAP .GT. NMAINI) .AND. (NUMPAQ .EQ. 1) ) THEN
                     KCMP = 1
                     SOMPGS = SOMPGS + ZI(JNBPG + IMAP-2)
                     KWORK = 1
                     SOMPGW = SOMPGW + ZI(JNBPG + IMAP-2)
                  ENDIF

                  IF ( (IMAP .GT. NMAINI) .AND. (NUMPAQ .GT. 1) ) THEN
                     KWORK = 1
                     SOMPGW = SOMPGW + ZI(JNBPG + IMAP-2)
                  ENDIF

                  IF ( NUMPAQ .GT. 1 ) THEN
                     KCMP = 1
                     SOMPGS = SOMPGS + ZI(JNBPG + IMAP-2)
                  ENDIF
                  NBPG = ZI(JNBPG + IMAP-1)

                  IF ( (NOMMAI .NE. '        ') .AND.
     &                 (IMAP .NE. ZI(JGRMA+N-1)) ) THEN
                     N = N - 1
                  ELSE
                     DO 260 IPG=1, NBPG
                        DO 280 ICMP=1, 6
                           CALL CESEXI('C',JSIGD,JSIGL,IMAP,IPG,1,ICMP,
     &                                 JAD)
                           IF (JAD .LE. 0) THEN
                             CALL UTMESS('F', 'PAQMAI.6', 'LE CHAMP '//
     &                           'SIMPLE QUI CONTIENT LES VALEURS DES'//
     &                           ' CONTRAINTES N EXISTE PAS.')
                           ELSE
                             ZR( JRWORK + (ICMP-1) + (IPG-1)*6 +
     &                             KWORK*SOMPGW*6 + (IORDR-1)*TSPAQ ) =
     &                       ZR( JSIGV + (ICMP-1) + (IPG-1)*6 +
     &                             KCMP*SOMPGS*6 )
                           ENDIF
 280                    CONTINUE
 260                 CONTINUE
                  ENDIF
                  IF ( (NOMMAI .NE. '        ') .AND.
     &                 (N .LT. NBMAGM) ) THEN
                     N = N + 1
                  ENDIF

 240           CONTINUE
               NMEMO = N
 220        CONTINUE

         ELSE

            DO 300 IORDR=1, NBORDR
               IF ( (NUMPAQ .GT. 1) .AND. (IORDR .EQ. 1) ) THEN
                  NINIT = NMEMO
               ELSEIF ( (NUMPAQ .EQ. 1) .AND. (IORDR .EQ. 1) ) THEN
                  NINIT = NMAINI
               ENDIF
               N = NINIT

               IF ( TYPRES .EQ. 'EVOL_NOLI' ) THEN
                  CALL RSEXCH(NOMSD, 'SIEF_ELGA', IORDR, CHSIG, IRET)
                  CALL RSEXCH(NOMSD, 'EPSI_ELGA_DEPL', IORDR, CHEPS,
     &                        IRET1)
               ELSE
                  CALL UTMESS('F', 'PAQMAI.7', 'LE CRITERE DE '//
     &                   'FATEMI ET SOCIE EST PREVU POUR FONCTIONNER '//
     &                   'APRES UN CALCUL ELASTOPLASTIQUE, '//
     &                   'SON UTILISATION APRES MECA_STATIQUE N''EST '//
     &                   'PAS PREVUE.')
               ENDIF

               IF (IRET .NE. 0) THEN
                  CALL UTMESS('F', 'PAQMAI.8', 'LE CHAMP DE '//
     &                    'CONTRAINTES AUX POINTS DE GAUSS SIEF_ELGA'//
     &                    ' OU SIEF_ELGA_DEPL N''A PAS ETE CALCULE.')
               ELSEIF (IRET1 .NE. 0) THEN
                  CALL UTMESS('F', 'PAQMAI.9', 'LE CHAMP DE '//
     &                    'DEFORMATIONS AUX POINTS DE GAUSS '//
     &                    'EPSI_ELGA_DEPL N''A PAS ETE CALCULE.')
               ENDIF

               CES1 = '&&PAQMAI.SIG_S1'
               CES2 = '&&PAQMAI.SIG_ORDO'
               CALL CELCES(CHSIG, 'V', CES1)
               CALL CESRED(CES1, 0, IBID, 6, LSIG, 'V', CES2)
               CALL JEEXIN(CES2(1:19)//'.CESV', IRET)
               IF (IRET .EQ. 0) THEN
                  CALL UTMESS('F', 'PAQMAI.10', 'LES CHAMPS DE '//
     &                    ' CONTRAINTES AUX POINTS DE GAUSS ' //
     &                    'N''EXISTENT PAS.')
               ENDIF
               CALL JEVEUO(CES2(1:19)//'.CESD', 'L', JSIGD)
               CALL JEVEUO(CES2(1:19)//'.CESL', 'L', JSIGL)
               CALL JEVEUO(CES2(1:19)//'.CESV', 'L', JSIGV)

               CES3 = '&&PAQMAI.EPS_S3'
               CES4 = '&&PAQMAI.EPS_ORDO'
               CALL CELCES(CHEPS, 'V', CES3)
               CALL CESRED(CES3, 0, IBID, 6, LEPS, 'V', CES4)
               CALL JEEXIN(CES4(1:19)//'.CESV', IRET)
               IF (IRET .EQ. 0) THEN
                  CALL UTMESS('F', 'PAQMAI.11', 'LES CHAMPS DE '//
     &                    ' DEFORMATIONS AUX POINTS DE GAUSS ' //
     &                    'N''EXISTENT PAS.')
               ENDIF
               CALL JEVEUO(CES4(1:19)//'.CESD', 'L', JEPSD)
               CALL JEVEUO(CES4(1:19)//'.CESL', 'L', JEPSL)
               CALL JEVEUO(CES4(1:19)//'.CESV', 'L', JEPSV)

               IF ( NUMPAQ .EQ. 1 ) THEN
                  KCMP = 0
                  SOMPGS = 0
               ELSEIF ( NUMPAQ .GT. 1 ) THEN
                  KCMP = 1
                  SOMPGS = SOMPGI
               ENDIF
               SOMPGW = 0
               KWORK = 0

               DO 320 IMAP=NMAINI, NMAINI+(NBMAP-1)
                  IF ( (IMAP .GT. NMAINI) .AND. (NUMPAQ .EQ. 1) ) THEN
                     KCMP = 1
                     SOMPGS = SOMPGS + ZI(JNBPG + IMAP-2)
                     KWORK = 1
                     SOMPGW = SOMPGW + ZI(JNBPG + IMAP-2)
                  ENDIF

                  IF ( (IMAP .GT. NMAINI) .AND. (NUMPAQ .GT. 1) ) THEN
                     KWORK = 1
                     SOMPGW = SOMPGW + ZI(JNBPG + IMAP-2)
                  ENDIF

                  IF ( NUMPAQ .GT. 1 ) THEN
                     KCMP = 1
                     SOMPGS = SOMPGS + ZI(JNBPG + IMAP-2)
                  ENDIF
                  NBPG = ZI(JNBPG + IMAP-1)

                  IF ( (NOMMAI .NE. '        ') .AND.
     &                 (IMAP .NE. ZI(JGRMA+N-1)) ) THEN
                     N = N - 1
                  ELSE
                     DO 340 IPG=1, NBPG

C BOUCLE SUR LES CONTRAINTES (6 COMPOSANTES)
                        DO 360 ICMP=1, 6
                           CALL CESEXI('C',JSIGD,JSIGL,IMAP,IPG,1,ICMP,
     &                                 JAD)
                           IF (JAD .LE. 0) THEN
                             CALL UTMESS('F', 'PAQMAI.12', 'LE CHAMP '//
     &                           'SIMPLE QUI CONTIENT LES VALEURS DES'//
     &                           ' CONTRAINTES N EXISTE PAS.')
                           ELSE
                             ZR( JRWORK + (ICMP-1) + (IPG-1)*12 +
     &                           KWORK*SOMPGW*12 + (IORDR-1)*TSPAQ ) =
     &                       ZR( JSIGV + (ICMP-1) + (IPG-1)*6 +
     &                           KCMP*SOMPGS*6 )
                           ENDIF
 360                    CONTINUE

C BOUCLE SUR LES DEFORMATIONS (6 COMPOSANTES)
                        DO 380 ICMP=1, 6
                           CALL CESEXI('C',JEPSD,JEPSL,IMAP,IPG,1,ICMP,
     &                                 JAD)
                           IF (JAD .LE. 0) THEN
                             CALL UTMESS('F', 'PAQMAI.13', 'LE CHAMP '//
     &                           'SIMPLE QUI CONTIENT LES VALEURS DES'//
     &                           ' DEFORMATIONS N EXISTE PAS.')
                           ELSE
                             ZR( JRWORK + (ICMP+6-1) + (IPG-1)*12 +
     &                           KWORK*SOMPGW*12 + (IORDR-1)*TSPAQ ) =
     &                       ZR( JEPSV + (ICMP-1) + (IPG-1)*6 +
     &                           KCMP*SOMPGS*6 )
                           ENDIF
 380                    CONTINUE

 340                 CONTINUE
                  ENDIF
                  IF ( (NOMMAI .NE. '        ') .AND.
     &                 (N .LT. NBMAGM) ) THEN
                     N = N + 1
                  ENDIF

 320           CONTINUE
               NMEMO = N
 300        CONTINUE

         ENDIF

         IF (TYPCHA .EQ. 'PERIODIQUE') THEN
            CALL DELTAU (JRWORK, JNBPG, NBPGT, NBORDR, NMAINI, NBMAP,
     &                   NUMPAQ, TSPAQ, NOMMET, NOMCRI, CESR)

         ELSEIF (TYPCHA .EQ. 'NON_PERIODIQUE') THEN
            CALL AVGRMA (ZR(JRWORK), TDISP, ZI(JNBPG), NBPGT, NBORDR,
     &                   NMAINI, NBMAP, NUMPAQ, TSPAQ, NOMCRI,
     &                   PROAXE, CESR)
         ENDIF

 200  CONTINUE

      WRITE(6,*)' '

C TRANSFORMATION D'UN CHAM_ELEM SIMPLE EN CHAM_ELEM

      IF ( TYPRES .EQ. 'EVOL_ELAS' ) THEN
         CALL RSEXCH( NOMSD, 'SIEF_ELGA_DEPL', 1, CHSIG, IRET )
      ELSEIF ( TYPRES .EQ. 'EVOL_NOLI' ) THEN
         CALL RSEXCH( NOMSD, 'SIEF_ELGA', 1, CHSIG, IRET )
      ENDIF
      CALL DISMOI('F','NOM_LIGREL',CHSIG,'CHAM_ELEM',IBID,LIGRE,IERD)
      CALL CESCEL(CESR,LIGRE,'TOU_INI_ELGA',' ','NON',NNCP,'G',NOMU)

C MENAGE

      CALL DETRSD('CHAM_ELEM',CELBID)
      CALL DETRSD('CHAM_ELEM_S',CESR)
      CALL DETRSD('CHAM_ELEM_S',CHSIGS)
      CALL DETRSD('CHAM_ELEM_S',CES1)
      CALL DETRSD('CHAM_ELEM_S',CES2)
      CALL DETRSD('CHAM_ELEM_S',CES3)
      CALL DETRSD('CHAM_ELEM_S',CES4)

      CALL JEDETR('&&PAQMAI.NUME_ORDRE')
      CALL JEDETR('&&PAQMAI.NBMAGR')
      CALL JEDETR('&&PAQMAI.NBPG')
      CALL JEDETR('&&PAQMAI.L_MAILLES')
      CALL JEDETR('&&PAQMAI.RWORK')
      CALL JEDETR('&&PAQMAI.PAQMA')
C
      CALL JEDEMA()
      END
