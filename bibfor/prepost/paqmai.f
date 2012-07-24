      SUBROUTINE PAQMAI(NOMSD, NOMU, NOMMAI, NOMMET, NOMCRI,NOMFOR,
     &                  GRDVIE, FORVIE,FORDEF, TYPCHA, PROAXE,
     &                  INSTIC,INSCRI,PREC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE F1BHHAJ J.ANGLES
C TOLE  CRP_20
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8  NOMSD, NOMU, NOMMAI, GRDVIE,INSCRI
      CHARACTER*16 NOMMET, NOMCRI, TYPCHA, PROAXE, NOMFOR, FORVIE
      LOGICAL      FORDEF
      REAL*8       INSTIC,PREC
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
C NOMFOR   IN    K16: LE NOM DE FORMULE DE GRNADUER EQUIVALENTE
C GRDVIE   IN    K16: NOM DE LA COURBE GRANDEUR EQUIVALENT _DUREE VIE
C TYPCHA     IN    K16: TYPE DE CHARGEMENT (PERIODIQUE OU NON).
C PROAXE     IN    K16: TYPE DE PROJECTION (UN OU DEUX AXES).
C-----------------------------------------------------------------------
      INTEGER       IBID, IERD, LORDR, JORDR, NBORDR, NDIM, IRET, JCESD
      INTEGER       NBMA, NBPGT, NBPGMX, JNBPG, IMA, TDISP, JRWORK, TPAQ
      INTEGER       NBPAQ, NUMPAQ, NMAPAQ, NBCMP, BORMAX, NBPMAX, JNBPAQ
      INTEGER       NMAINI, NBMAP, TSPAQ, IORDR, LOR8EM, LOISEM, JAD
      INTEGER       JSIGV, JSIGD, JSIGL, IMAP, NBPG, IPG, ICMP, IRET1
      INTEGER       JEPSV, JEPSD, JEPSL, PARACT(30),JEPPED, JEPPEL
      INTEGER       JEPSPV, JEPSPD, JEPSPL, IRET2, JEPPEV, VALEP
      INTEGER       I, KWORK, SOMPGW, SOMPGS, SOMPGI, JMAIL, JGRMA
      INTEGER       N, NINIT, NBPGGM, NBMAGM, NMEMO,NNCP
      INTEGER       VALI(2), DECAL, ORDINI, K, JINST, IORD
C
      REAL*8        R8B, VAL1, R8PREM
C
      COMPLEX*16    C16B
C
      CHARACTER*1   KBID
      CHARACTER*4   LSIG(6), LEPS(6)
      CHARACTER*8   K8B, MOTCLE(4), TYMOCL(4)
      CHARACTER*16  TYPRES, NOMOPT
      CHARACTER*19  LISMAI
      CHARACTER*19  CESR, LIGRE, CELBID, CHSIG, CHSIGS, CES1, CES2
      CHARACTER*19  CHEPS, CES3, CES4, CHEPPE
      CHARACTER*19  CHEPSP, CES5, CES6, CES7, CES8
      LOGICAL       LBID, CRSIGM, CREPST, CREPSE,CREPSP, CREPPE
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
          CALL U2MESS('F','PREPOST4_26')
      ENDIF

C CONSTRUCTION DU CHAMP SIMPLE DESTINE A RECEVOIR LES RESULTATS :
C DTAUM,....

      CALL RSEXCH('F',NOMSD, 'SIEF_ELGA', 1, CHSIG, IRET )

      CALL DISMOI('F','NOM_LIGREL',CHSIG,'CHAM_ELEM',IBID,LIGRE,IERD)
      CESR = '&&PAQMAI.FACY'
      CELBID = '&&PAQMAI.BID'
      CALL ALCHML(LIGRE,'TOU_INI_ELGA','PFACY_R','V',CELBID,IERD,' ')
      IF (IERD .NE. 0) THEN
         CALL U2MESS('A','FATIGUE1_1')
      ENDIF
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
         CALL U2MESS('A','PREPOST4_27')
         NBORDR = NBORDR - 1
      ENDIF

      ORDINI = 1
      DO 420 K = 2, NBORDR
         IORD = ZI(JORDR-1+K)
         CALL RSADPA(NOMSD,'L',1,'INST',IORD,0,JINST,KBID)
         IF (INSTIC .GT. R8PREM() ) THEN
           IF (INSCRI .EQ.'ABSOLU') THEN
            IF (ABS(ZR(JINST) - INSTIC) .LT. PREC ) THEN
               ORDINI = K
               GOTO 410
            ENDIF
            ELSE
            IF (INSCRI .EQ.'RELATIF') THEN
               IF (ABS(ZR(JINST)/INSTIC - 1.D0).LT. PREC ) THEN
                  ORDINI = K
                  GOTO 410
               ENDIF
            ENDIF
           ENDIF
         ENDIF
420   CONTINUE
410   CONTINUE

      IF  ((ORDINI .EQ. 1) .AND.
     &   ((INSCRI .EQ.'ABSOLU') .OR. (INSCRI .EQ.'RELATIF') ) ) THEN
         CALL U2MESS('A','PREPOST4_48')
      ENDIF

C  INITIALISER
      CRSIGM = .FALSE.
      CREPST = .FALSE.
      CREPSE = .FALSE.
      CREPSP = .FALSE.
C---    ANALYSER LE CRITERE
      CALL ANACRI( NOMCRI,NOMFOR,TYPCHA,'NON', PARACT,
     &            LBID, CRSIGM, CREPST, CREPSE,CREPSP)


C IF CRITERE CONTIENT DEFORMATION ELASTIQUE
       CREPPE = .FALSE.
       IF (CREPSE) THEN
          IF ( .NOT. CREPST ) THEN
               CALL U2MESS('A','PREPOST4_45')
               CREPST = .TRUE.
            ENDIF
          IF  (( .NOT. CREPSP ) ) THEN
             CALL U2MESS('A','PREPOST4_46')
             CREPPE = .TRUE.
          ENDIF

       ENDIF
C RECUPERATION DU NOMBRE DE MAILLES ET DU NOMBRE DE POINTS DE GAUSS
C PAR MAILLE

C CAS OU L'ON CALCULE LA FATIGUE SUR TOUT LE MAILLAGE
      CALL RSEXCH('F',NOMSD, 'SIEF_ELGA', 1, CHSIG, IRET )
      CHSIGS = '&&PAQMAI.SIELGA'
      CALL CELCES( CHSIG, 'V', CHSIGS )
      CALL JEVEUO(CHSIGS(1:19)//'.CESD','L',JCESD)
      NBMA = ZI(JCESD-1+1)

C CAS OU L'ON CALCULE LA FATIGUE SUR UN OU DES GROUPES DE MAILLES

      CALL WKVECT( '&&PAQMAI.NBMAGR', 'V V I', 1, JGRMA )
      CALL JERAZO( '&&PAQMAI.NBMAGR', 1, 1 )
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

C        VECTEUR CONTENANT LE NBR. DE PT. DE GAUSS DES MAILLES DU OU
C        DES GROUPE(S) DE MAILLES
         CALL JEDETR( '&&PAQMAI.NBMAGR' )
         CALL WKVECT( '&&PAQMAI.NBMAGR', 'V V I', NBMAGM, JGRMA )

         DO 40 I=1, NBMAGM
            ZI(JGRMA-1 + I) = ZI(JMAIL-1 + I)
 40      CONTINUE
      ENDIF

C     VECTEUR CONTENANT LE NBR. DE PT. DE GAUSS DES MAILLES DU MAILLAGE
      CALL WKVECT( '&&PAQMAI.NBPG', 'V V I', NBMA, JNBPG )

C NBPGMX : NOMBRE DE POINTS DE GAUSS DANS LES ELEMENTS
C          QUI EN ONT LE PLUS, (EX : ELEMENT 3D = 27)
C NBPGT  : NOMBRE TOTAL DE POINTS DE GAUSS DANS LE MAILLAGE
C NBPGGM : NOMBRE DE POINTS DE GAUSS DANS LE GROUPE DE MAILLES

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
      WRITE(6,*)'NOMBRE TOTAL DE POINTS DE GAUSS A EXPLORER ==>',NBPGT
      WRITE(6,*)' '

      WRITE(6,*)'NUMERO DU PAQUET DE MAILLES  -  ' //
     &           'NOMBRE DE POINTS DE GAUSS TRAITES'

C CONSTRUCTION DES PAQUETS DE MAILLES.

C 1/ DIMENSIONNEMENT DU VECTEUR DE TRAVAIL (RWORK) ET DU VECTEUR
C    CONTENANT LES CARACTERISTIQUES DES PAQUETS DE MAILLES (PAQMA).
C    JEDISP REND LA DIMENSION EN ENTIERS, ON LA CONVERTIT A L'AIDE
C    DES FONCTIONS ENVIMA POUR ALLOUER UN TABLEAU DE REELS.
      CALL JEDISP(1, TDISP)
      TDISP =  (TDISP  / LOR8EM()) * LOISEM()
      TDISP = INT(0.6D0*TDISP)
      CALL WKVECT( '&&PAQMAI.RWORK', 'V V R', TDISP, JRWORK )

C       IF (( NOMCRI(1:16) .EQ. 'FATESOCI_MODI_AV' ) .OR.
C      &     FORDEF )THEN
C          NBCMP = 12
C       ELSE
C          NBCMP = 6
C       ENDIF
      NBCMP = 18
C     POUR CALCULER LE NB DE PAQUETS MAXI NOUS NE FAISONS PAS LA MEME
C     CHOSE QUE DANS paqnoe.f PARCE QUE BORMAX EST NATURELLEMENT
C     SURDIMENSIONNEE CAR NBMA TIENT COMPTE DES MAILLES NON VOLUMIQUES.
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
            VALI (1) = TDISP
            VALI (2) = TPAQ
            CALL U2MESG('F', 'PREPOST5_67',0,' ',2,VALI,0,0.D0)

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

      IF ( NBPAQ .GT. NBPMAX ) THEN
            VALI (1) = NBPMAX
            VALI (2) = NBPAQ
            CALL U2MESG('F', 'PREPOST5_68',0,' ',2,VALI,0,0.D0)
      ENDIF

C TRAITEMENT DES PAQUETS DE MAILLES.
C
C  <<REMPLISSAGE>> DU VECTEUR DE TRAVAIL

      SOMPGI = 0
      SOMPGS = 0
      NMEMO = 0

      DO 200 NUMPAQ=1, NBPAQ
         CALL JERAZO('&&PAQMAI.RWORK', TDISP, 1)
         TPAQ = ZI(JNBPAQ + (NUMPAQ-1)*4 + 1)
         NMAINI = ZI(JNBPAQ + (NUMPAQ-1)*4 + 2)
         NBMAP = ZI(JNBPAQ + (NUMPAQ-1)*4 + 3)
         TSPAQ = TPAQ/NBORDR

C        PERMET D'INITIALISER SOMPGS A CHAQUE PAQUET
         IF ( NUMPAQ .GT. 1 ) THEN
            SOMPGI = SOMPGS
         ENDIF

            DO 300 IORDR=1, NBORDR
               IF ( (NUMPAQ .GT. 1) .AND. (IORDR .EQ. 1) ) THEN
                  NINIT = NMEMO
               ELSEIF ( (NUMPAQ .EQ. 1) .AND. (IORDR .EQ. 1) ) THEN
                  NINIT = NMAINI
               ENDIF
               N = NINIT

C IF CRITERE CONTIENT CONTRAINTE
               IF (CRSIGM) THEN

                  CALL RSEXCH('F',NOMSD,'SIEF_ELGA',IORDR,CHSIG,IRET)

                  CES1 = '&&PAQMAI.SIG_S1'
                  CES2 = '&&PAQMAI.SIG_ORDO'
                  CALL CELCES(CHSIG, 'V', CES1)
                  CALL CESRED(CES1, 0, IBID, 6, LSIG, 'V', CES2)
                  CALL JEEXIN(CES2(1:19)//'.CESV', IRET)
                  IF (IRET .EQ. 0) THEN
                     CALL U2MESS('F','PREPOST4_29')
                  ENDIF
                     CALL JEVEUO(CES2(1:19)//'.CESD', 'L', JSIGD)
                     CALL JEVEUO(CES2(1:19)//'.CESL', 'L', JSIGL)
                     CALL JEVEUO(CES2(1:19)//'.CESV', 'L', JSIGV)
               ENDIF

C IF CRITERE CONTIENT DEFORMATION TOTALE
               IF (CREPST) THEN

                  CALL RSEXCH('F',NOMSD, 'EPSI_ELGA', IORDR, CHEPS,
     &                            IRET1)

                  CES3 = '&&PAQMAI.EPS_S3'
                  CES4 = '&&PAQMAI.EPS_ORDO'
                  CALL CELCES(CHEPS, 'V', CES3)
                  CALL CESRED(CES3, 0, IBID, 6, LEPS, 'V', CES4)
                  CALL JEEXIN(CES4(1:19)//'.CESV', IRET)
                  IF (IRET .EQ. 0) THEN
                     CALL U2MESS('F','PREPOST4_34')
                  ENDIF
                  CALL JEVEUO(CES4(1:19)//'.CESD', 'L', JEPSD)
                  CALL JEVEUO(CES4(1:19)//'.CESL', 'L', JEPSL)
                  CALL JEVEUO(CES4(1:19)//'.CESV', 'L', JEPSV)
               ENDIF

C IF CRITERE CONTIENT DEFORMATION PLASTIQUE
               IF (CREPSP) THEN

                  CALL RSEXCH('F',NOMSD, 'EPSP_ELGA', IORDR, CHEPSP,
     &                            IRET2)

                  CES5 = '&&PAQMAI.EPSP_S3'
                  CES6 = '&&PAQMAI.EPSP_ORDO'
                  CALL CELCES(CHEPSP, 'V', CES5)
                  CALL CESRED(CES5, 0, IBID, 6, LEPS, 'V', CES6)
                  CALL JEEXIN(CES5(1:19)//'.CESV', IRET)
                  IF (IRET .EQ. 0) THEN
                     CALL U2MESS('F','PREPOST4_37')
                  ENDIF
                  CALL JEVEUO(CES6(1:19)//'.CESD', 'L', JEPSPD)
                  CALL JEVEUO(CES6(1:19)//'.CESL', 'L', JEPSPL)
                  CALL JEVEUO(CES6(1:19)//'.CESV', 'L', JEPSPV)
               ENDIF

C IF CRITERE CONTIENT DEFORMATION ELASTIQUE
               IF (CREPPE) THEN

                  CALL RSEXCH(' ',NOMSD, 'EPSP_ELGA', IORDR, CHEPPE,
     &                            VALEP)
                  IF (VALEP .NE. 0) THEN
                     CALL U2MESS('A','PREPOST4_46')
                  ENDIF
                  IF (VALEP .EQ. 0) THEN
                     CES7 = '&&PAQMAI.EPSPE_S3'
                     CES8 = '&&PAQMAI.EPSPE_ORDO'
                     CALL CELCES(CHEPPE, 'V', CES7)
                     CALL CESRED(CES7, 0, IBID, 6, LEPS, 'V', CES8)
                     CALL JEEXIN(CES7(1:19)//'.CESV', IRET)
                     IF (IRET .EQ. 0) THEN
                        CALL U2MESS('F','PREPOST4_37')
                     ENDIF
                     CALL JEVEUO(CES8(1:19)//'.CESD', 'L', JEPPED)
                     CALL JEVEUO(CES8(1:19)//'.CESL', 'L', JEPPEL)
                     CALL JEVEUO(CES8(1:19)//'.CESV', 'L', JEPPEV)
                  ENDIF
               ENDIF


               IF ( NUMPAQ .EQ. 1 ) THEN
                  SOMPGS = 0
               ELSEIF ( NUMPAQ .GT. 1 ) THEN
                  SOMPGS = SOMPGI
               ENDIF
               SOMPGW = 0
               KWORK = 0
               DECAL = 18

               DO 320 IMAP=NMAINI, NMAINI+(NBMAP-1)
                  IF ( (IMAP .GT. NMAINI) .AND. (NUMPAQ .EQ. 1) ) THEN
                     SOMPGS = SOMPGS + ZI(JNBPG + IMAP-2)
                     KWORK = 1
                     SOMPGW = SOMPGW + ZI(JNBPG + IMAP-2)
                  ENDIF

                  IF ( (IMAP .GT. NMAINI) .AND. (NUMPAQ .GT. 1) ) THEN
                     KWORK = 1
                     SOMPGW = SOMPGW + ZI(JNBPG + IMAP-2)
                  ENDIF

                  IF ( NUMPAQ .GT. 1 ) THEN
                     SOMPGS = SOMPGS + ZI(JNBPG + IMAP-2)
                  ENDIF
                  NBPG = ZI(JNBPG + IMAP-1)

                  IF ( (NOMMAI .NE. '        ') .AND.
     &                 (IMAP .NE. ZI(JGRMA+N-1)) ) THEN
                     N = N - 1
                  ELSE
                     DO 340 IPG=1, NBPG

C BOUCLE SUR LES CONTRAINTES (6 COMPOSANTES)
                      IF (CRSIGM) THEN

                        DO 360 ICMP=1, 6
                           CALL CESEXI('C',JSIGD,JSIGL,IMAP,IPG,1,ICMP,
     &                                 JAD)
                           IF (JAD .LE. 0) THEN
                             IF ( ICMP .EQ. 5 ) THEN
                                CALL U2MESI('F', 'FATIGUE1_2',1,ICMP)
                             ELSE
                                CALL U2MESS('F','PREPOST4_30')
                             ENDIF
                           ELSE
                             ZR( JRWORK + (ICMP-1) + (IPG-1)*DECAL +
     &                         KWORK*SOMPGW*DECAL + (IORDR-1)*TSPAQ ) =
     &                       ZR( JSIGV -1 +JAD)
                           ENDIF
 360                    CONTINUE
                      ENDIF

C BOUCLE SUR LES DEFORMATIONS TOTALES (6 COMPOSANTES)
                      IF (CREPST) THEN
                        DO 380 ICMP=1, 6
                           CALL CESEXI('C',JEPSD,JEPSL,IMAP,IPG,1,ICMP,
     &                                 JAD)
                           IF (JAD .LE. 0) THEN
                             IF ( ICMP .EQ. 5 ) THEN
                                CALL U2MESI('F', 'FATIGUE1_3',1,ICMP)
                             ELSE
                                CALL U2MESS('F','PREPOST4_35')
                             ENDIF
                           ELSE
                             ZR( JRWORK + (ICMP+6-1) + (IPG-1)*DECAL +
     &                         KWORK*SOMPGW*DECAL + (IORDR-1)*TSPAQ ) =
     &                       ZR( JEPSV -1+JAD)
                           ENDIF
 380                    CONTINUE
                     ENDIF

C BOUCLE SUR LES DEFORMATIONS PLASTIQUES (6 COMPOSANTES)
                      IF (CREPSP) THEN
                        DO 400 ICMP=1, 6
                           CALL CESEXI('C',JEPSPD,JEPSPL,IMAP,IPG,1,
     &                                 ICMP,JAD)
                           IF (JAD .LE. 0) THEN
                             IF ( ICMP .EQ. 5 ) THEN
                                CALL U2MESI('F', 'FATIGUE1_3',1,ICMP)
                             ELSE
                                CALL U2MESS('F','PREPOST4_35')
                             ENDIF
                           ELSE
                             ZR( JRWORK + (ICMP+6+6-1) + (IPG-1)*DECAL +
     &                         KWORK*SOMPGW*DECAL + (IORDR-1)*TSPAQ ) =
     &                       ZR( JEPSPV -1+JAD)
                           ENDIF
 400                    CONTINUE
                     ENDIF

C BOUCLE SUR LES DEFORMATIONS PLASTIQUES (6 COMPOSANTES)
                     IF (CREPSE) THEN
                       IF (VALEP .EQ. 0) THEN
                          DO 401 ICMP=1, 6
                             CALL CESEXI('C',JEPPED,JEPPEL,IMAP,IPG,1,
     &                                ICMP,JAD)
                             IF (JAD .LE. 0) THEN
                             IF ( ICMP .EQ. 5 ) THEN
                               CALL U2MESI('F', 'FATIGUE1_3',1,ICMP)
                             ELSE
                               CALL U2MESS('F','PREPOST4_35')
                             ENDIF
                             ELSE
                                ZR(JRWORK + (ICMP+6+6-1)+(IPG-1)*DECAL+
     &                        KWORK*SOMPGW*DECAL + (IORDR-1)*TSPAQ ) =
     &                          ZR( JEPPEV -1+JAD)
                             ENDIF
 401                      CONTINUE
                       ELSE
                          DO 405 ICMP=1, 6
                             ZR( JRWORK + (ICMP+6+6-1) +(IPG-1)*DECAL +
     &                 KWORK*SOMPGW*DECAL + (IORDR-1)*TSPAQ ) = 0.D0
 405                      CONTINUE
                       ENDIF
                     ENDIF

 340                 CONTINUE
                  ENDIF
                  IF ( (NOMMAI .NE. '        ') .AND.
     &                 (N .LT. NBMAGM) ) THEN
                     N = N + 1
                  ENDIF

 320           CONTINUE
               NMEMO = N
 300        CONTINUE

C         ENDIF

         IF ( NOMCRI(1:11) .EQ. 'VMIS_TRESCA' ) THEN
            NOMOPT = 'DOMA_ELGA'
            CALL VAMPLI(ZR(JRWORK), TDISP, ZI(JNBPG), NBPGT, NBORDR,
     &                  NMAINI, NBMAP, NUMPAQ, TSPAQ, NOMCRI,
     &                  NOMMAI, NOMOPT, CESR)
            GOTO 200
         ENDIF

         IF (TYPCHA .EQ. 'PERIODIQUE') THEN
            CALL DELTAU (JRWORK, JNBPG, NBPGT, NBORDR,ORDINI, NMAINI,
     &                   NBMAP, NUMPAQ, TSPAQ, NOMMET, NOMCRI,NOMFOR,
     &                   GRDVIE, FORVIE, CESR)

         ELSEIF (TYPCHA .EQ. 'NON_PERIODIQUE') THEN
            CALL AVGRMA (ZR(JRWORK), TDISP, ZI(JNBPG), NBPGT, NBORDR,
     &                   NMAINI, NBMAP, NUMPAQ, TSPAQ, NOMCRI,NOMFOR,
     &                   GRDVIE, FORVIE,FORDEF, PROAXE, CESR)
         ENDIF

 200  CONTINUE


C TRANSFORMATION D'UN CHAM_ELEM SIMPLE EN CHAM_ELEM

      CALL RSEXCH('F',NOMSD, 'SIEF_ELGA', 1, CHSIG, IRET )
      CALL DISMOI('F','NOM_LIGREL',CHSIG,'CHAM_ELEM',IBID,LIGRE,IERD)
      CALL CESCEL(CESR,LIGRE,'TOU_INI_ELGA',' ','NON',NNCP,'G',NOMU,'F',
     &            IBID)

C MENAGE

      CALL DETRSD('CHAM_ELEM',CELBID)
      CALL DETRSD('CHAM_ELEM_S',CESR)
      CALL DETRSD('CHAM_ELEM_S',CHSIGS)

      IF (CRSIGM) THEN
         CALL DETRSD('CHAM_ELEM_S',CES1)
         CALL DETRSD('CHAM_ELEM_S',CES2)
      ENDIF

      IF (CREPST) THEN
         CALL DETRSD('CHAM_ELEM_S',CES3)
         CALL DETRSD('CHAM_ELEM_S',CES4)
      ENDIF

      IF (CREPSP) THEN
         CALL DETRSD('CHAM_ELEM_S',CES5)
         CALL DETRSD('CHAM_ELEM_S',CES6)
      ENDIF

      IF (((CREPPE)) .AND. (VALEP .EQ. 0)) THEN
         CALL DETRSD('CHAM_ELEM_S',CES7)
         CALL DETRSD('CHAM_ELEM_S',CES8)
      ENDIF

      CALL JEDETR('&&PAQMAI.NUME_ORDRE')
      CALL JEDETR('&&PAQMAI.NBMAGR')
      CALL JEDETR('&&PAQMAI.NBPG')
      CALL JEDETR('&&PAQMAI.RWORK')
      CALL JEDETR('&&PAQMAI.PAQMA')
      IF ( NOMMAI .NE. '        ' ) THEN
         CALL JEDETR('&&PAQMAI.L_MAILLES')
      ENDIF
C
      CALL JEDEMA()
      END
