      SUBROUTINE PAQNOE(NOMSD, NOMU, NOMMOD, NOMMAI, NOMMET, NOMCRI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/06/2003   AUTEUR F1BHHAJ J.ANGLES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8  NOMSD, NOMU, NOMMOD, NOMMAI
      CHARACTER*16 NOMMET, NOMCRI
C ---------------------------------------------------------------------
C BUT: CONSTRUIRE LES PAQUETS DE NOEUDS AFIN DE CALCULER LE VECTEUR
C      CISAILLEMENT TAU DANS LE PLAN u, v.
C ---------------------------------------------------------------------
C ARGUMENTS:
C NOMSD    IN    K8 : NOM DE LA STRUCTURE DE DONNEES RESULTAT (CALC_NO).
C NOMU     IN    K8 : NOM UTILISATEUR DU CALCUL EN FATIGUE.
C NOMMOD   IN    K8 : NOM UTILISATEUR DU MODELE.
C NOMMAI   IN    K8 : NOM UTILISATEUR DU MAILLAGE.
C NOMMET   IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
C                     CIRCONSCRIT.
C NOMCRI   IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
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
      INTEGER       IBID, IERD, LORDR, JORDR, NBORDR, NDIM, IRET, JCESD
      INTEGER       NBNO, INO, TDISP, JRWORK, TPAQ
      INTEGER       NBPAQ, NUMPAQ, NBNOPA, NNOPAQ, BORMAX, NBPMAX
      INTEGER       JNBPAQ
      INTEGER       NNOINI, NBNOP, TSPAQ, IORDR, LOR8EM, LOISEM, JAD
      INTEGER       JSIGV, JSIGD, JSIGL, KWORK, INOP, SOMNOW, ICMP
      INTEGER       I, JNOEU, NUNOE
C
      REAL*8        R8B, VAL1, VRESU(18)
C
      COMPLEX*16    C16B
C
      CHARACTER*4   LSIG(6)
      CHARACTER*8   K8B, LRESU(22), MOTCLE(4), TYMOCL(4)
      CHARACTER*16  TYPRES
      CHARACTER*19  CNSR, LISNOE, LIGRE, PRCHNO, CELBID
      CHARACTER*19  CHSIG, CHSIGS, CNS1, CNS2
C
C-----------------------------------------------------------------------
C234567                                                              012
C-----------------------------------------------------------------------
      DATA  LSIG/ 'SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ', 'SIYZ' /
C
      DATA  LRESU/ 'DTAUM1', 'VNM1X', 'VNM1Y', 'VNM1Z', 'SINMAX1',
     &             'SINMOY1', 'EPNMAX1', 'EPNMOY1', 'SIGEQ1', 'NBRUP1',
     &             'ENDO1', 'DTAUM2', 'VNM2X', 'VNM2Y', 'VNM2Z',
     &             'SINMAX2', 'SINMOY2', 'EPNMAX2', 'EPNMOY2', 'SIGEQ2',
     &             'NBRUP2', 'ENDO2' /
C-----------------------------------------------------------------------
C
      CALL JEMARQ()

C RECUPERATION DU TYPE DE CALCUL MECANIQUE EFFECTUE
C
      CALL DISMOI('F','TYPE_RESU',NOMSD,'RESULTAT',IBID,TYPRES,IERD)
      IF ( (TYPRES(1:9) .NE. 'EVOL_ELAS') .AND.
     &     (TYPRES(1:9) .NE. 'EVOL_NOLI') ) THEN
          CALL UTMESS('F', 'PAQNOE.1', 'LE TYPE DU CONCEPT RESULTAT '//
     &                ' N''EST NI EVOL_ELAS, NI EVOL_NOLI.')
      ENDIF

C CONSTRUCTION DU CHAMP SIMPLE DESTINE A RECEVOIR LES RESULTATS :
C DTAUM,....
C
      IF ( TYPRES .EQ. 'EVOL_ELAS' ) THEN
         CALL RSEXCH( NOMSD, 'SIGM_NOEU_DEPL', 1, CHSIG, IRET )     
      ELSEIF ( TYPRES .EQ. 'EVOL_NOLI' ) THEN
         CALL RSEXCH( NOMSD, 'SIEF_NOEU_ELGA', 1, CHSIG, IRET )     
      ENDIF
C
      CNSR = '&&PAQNOE.FACY'
      CALL CNSCRE( NOMMAI, 'FACY_R', 22, LRESU, 'V', CNSR )
C
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
      CALL WKVECT('&&PAQNOE.NUME_ORDRE','V V I',NDIM,JORDR)
      CALL RSORAC( NOMSD, 'TOUT_ORDRE', IBID, R8B, K8B, C16B, R8B, K8B,
     &             ZI(JORDR), NDIM, NBORDR )
C
C CREATION D'UN OBJET JEVEUX CONTENANT LA LISTE DES NUMEROS
C DE NOEUDS AINSI QUE LE NOMBRE DE NOEUDS
C
      LISNOE = '&&PAQNOE.L_NOEUDS'
      MOTCLE(1) = 'GROUP_MA'
      TYMOCL(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYMOCL(2) = 'MAILLE'
      MOTCLE(3) = 'GROUP_NO'
      TYMOCL(3) = 'GROUP_NO'
      MOTCLE(4) = 'NOEUD'
      TYMOCL(4) = 'NOEUD'
      CALL RELIEM( ' ', NOMMAI, 'NU_NOEUD', ' ', 0, 4, MOTCLE,
     &             TYMOCL, LISNOE, NBNO)
      CALL JEVEUO ( LISNOE, 'L', JNOEU )
C
      WRITE(6,*)'NOMBRE TOTAL DE NOEUDS A TRAITER ==>',NBNO
      WRITE(6,*)' '
      WRITE(6,*)'NUMERO DU PAQUET DE NOEUDS   -   ' //
     &           'NOMBRE DE NOEUDS TRAITES'
C
C CONSTRUCTION DES PAQUETS DE NOEUDS.
C
C 1/ DIMENSIONNEMENT DU VECTEUR DE TRAVAIL (RWORK) ET DU VECTEUR
C    CONTENANT LES CARACTERISTIQUES DES PAQUETS DE NOEUDS (PAQNO).
C    JEDISP REND LA DIMENSION EN ENTIERS, ON LA CONVERTIT A L'AIDE
C    DES FONCTIONS ENVIMA POUR ALLOUER UN TABLEAU DE REELS
      CALL JEDISP(1, TDISP)
      TDISP =  (TDISP * LOISEM()) / LOR8EM() 
      TDISP = INT(0.6D0*TDISP)
      CALL WKVECT( '&&PAQNOE.RWORK', 'V V R', TDISP, JRWORK )
C
      BORMAX = NBNO*NBORDR*6
      VAL1 = DBLE(TDISP)/DBLE(BORMAX)
C
      IF (VAL1 .LT. 1.0D0) THEN
         NBPMAX = INT(1.0D0/VAL1) + 1
      ELSE
         NBPMAX = 2
      ENDIF
      CALL WKVECT( '&&PAQNOE.PAQNO', 'V V I', NBPMAX*4, JNBPAQ )
C
      TPAQ = 0
      NBPAQ = 0
      NUMPAQ = 0
      NNOPAQ = 0
      DO 100 INO=1, NBNO
         TPAQ = TPAQ + NBORDR*6
         NNOPAQ = NNOPAQ + 1
C
         IF ( TPAQ .LT. TDISP ) THEN
            IF (INO .EQ. NBNO) THEN
               NUMPAQ = NUMPAQ + 1
               ZI(JNBPAQ + (NUMPAQ-1)*4) = NUMPAQ
C CAS OU LA TAILLE DU PAQUET EST TOUT JUSTE INFERIEURE A LA TAILLE
C MEMOIRE DISPONIBLE
               IF ( (TDISP-TPAQ) .LT. (NBORDR*6) ) THEN
C
                 TPAQ = TPAQ - NBORDR*6
                 ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
                 ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = INO - (NNOPAQ - 1)
                 ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NNOPAQ - 1
                 NBPAQ = NUMPAQ
                 TPAQ = NBORDR*6
                 NNOPAQ = 1
C
                 NUMPAQ = NUMPAQ + 1
                 ZI(JNBPAQ + (NUMPAQ-1)*4) = NUMPAQ
                 ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
                 ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = INO
                 ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NNOPAQ
                 NBPAQ = NUMPAQ
               ELSE
                 ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
                 NBPAQ = NUMPAQ
                 IF (INO .EQ. 1) THEN
                    ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = INO 
                    ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = 1
                 ELSE
                    ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = INO - (NNOPAQ - 1)
                    ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NNOPAQ
                 ENDIF
               ENDIF
            ENDIF

         ELSEIF ( ( TPAQ .GT. TDISP ) .AND. (INO .EQ. 1) ) THEN
            CALL UTDEBM('F', 'PAQNOE.2', 'LA TAILLE MEMOIRE '//
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

         ELSEIF ( ( TPAQ .GE. TDISP ) .AND. (INO .GT. 1) ) THEN
C ON RECULE DE DEUX NOEUDS POUR NE PAS DEBORDER DU VECTEUR DE TRAVAIL
C (JRWORK). CECI PEUT SE PRODUIRE QUAND (TDISP - TPAQ + NBORDR*6) < 6 .
C
            TPAQ = TPAQ - (2*NBORDR*6)
            IF ( (TDISP - TPAQ) .LT. 6 ) THEN
              CALL UTMESS('F', 'PAQNOE.3', 'DEBORDEMENT PROBABLE '//
     &                    'DU VECTEUR DE TRAVAIL (JRWORK).')
            ENDIF
            NUMPAQ = NUMPAQ + 1
            ZI(JNBPAQ + (NUMPAQ-1)*4) = NUMPAQ
            ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
            ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = INO - (NNOPAQ - 1)
            ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NNOPAQ - 2
            NBPAQ = NUMPAQ
C
            TPAQ = 2*NBORDR*6
            NNOPAQ = 2
            IF (INO .EQ. NBNO) THEN
               NUMPAQ = NUMPAQ + 1
               ZI(JNBPAQ + (NUMPAQ-1)*4) = NUMPAQ
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 1) = TPAQ
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 2) = INO - (NNOPAQ - 1)
               ZI(JNBPAQ + (NUMPAQ-1)*4 + 3) = NNOPAQ
               NBPAQ = NUMPAQ
            ENDIF
            IF (NBPAQ .GT. (NBPMAX-1)) THEN
               CALL UTMESS('F', 'PAQNOE.4', 'LA TAILLE DU VECTEUR ' //
     &                     'CONTENANT LES CARACTERISTIQUES DES ' //
     &                     'PAQUETS DE NOEUDS EST TROP PETITE.' //
     &                     'ERREUR FORTRAN, EMETTRE UNE AL.')
            ENDIF
         ENDIF

 100  CONTINUE
C
C TRAITEMENT DES PAQUETS DE NOEUDS.
C
C  <<REMPLISSAGE>> DU VECTEUR DE TRAVAIL
C
C
      DO 200 NUMPAQ=1, NBPAQ 
         CALL JERAZO('&&PAQNOE.RWORK', TDISP, 1)
         TPAQ = ZI(JNBPAQ + (NUMPAQ-1)*4 + 1)
         NNOINI = ZI(JNBPAQ + (NUMPAQ-1)*4 + 2)
         NBNOP = ZI(JNBPAQ + (NUMPAQ-1)*4 + 3)
         TSPAQ = TPAQ/NBORDR
C
         DO 220 IORDR=1, NBORDR
            IF ( TYPRES .EQ. 'EVOL_ELAS' ) THEN
               CALL RSEXCH(NOMSD, 'SIGM_NOEU_DEPL', IORDR, CHSIG, IRET)
            ELSEIF ( TYPRES .EQ. 'EVOL_NOLI' ) THEN
               CALL RSEXCH(NOMSD, 'SIEF_NOEU_ELGA', IORDR, CHSIG, IRET)
            ENDIF
            CNS1 = '&&PAQNOE.SIG_S1'
            CNS2 = '&&PAQNOE.SIG_ORDO'
            CALL CNOCNS(CHSIG, 'V', CNS1)
            CALL CNSRED(CNS1, 0, IBID, 6, LSIG, 'V', CNS2)
            CALL JEEXIN(CNS2(1:19)//'.CNSV', IRET)
            IF (IRET .EQ. 0) THEN
               CALL UTMESS('F', 'PAQNOE.5', 'LES CHAMPS DE '//
     &                 ' CONTRAINTES AUX NOEUDS N''EXISTENT PAS.')
            ENDIF
            CALL JEVEUO(CNS2(1:19)//'.CNSD', 'L', JSIGD)
            CALL JEVEUO(CNS2(1:19)//'.CNSL', 'L', JSIGL)
            CALL JEVEUO(CNS2(1:19)//'.CNSV', 'L', JSIGV)
C
            KWORK = 0
            SOMNOW = 0
C
            DO 240 INOP=NNOINI, NNOINI+(NBNOP-1)
              IF ( INOP .GT. NNOINI ) THEN
                 KWORK = 1
                 SOMNOW = SOMNOW + 1
              ENDIF
C
              NUNOE = ZI(JNOEU + INOP-1)
              DO 280 ICMP=1, 6
                IF ( ZL(JSIGL + (ICMP-1) + (NUNOE-1)*6) ) THEN
                  ZR( JRWORK + (ICMP-1) + KWORK*SOMNOW*6 +
     &                         (IORDR-1)*TSPAQ ) =
     &            ZR( JSIGV + (ICMP-1) + (NUNOE-1)*6 )
                ELSE
                  CALL UTMESS('F', 'PAQNOE.6', 'LE CHAMP SIMPLE '//
     &                'QUI CONTIENT LES VALEURS DES CONTRAINTES '//
     &                'N EXISTE PAS.')
                ENDIF
 280          CONTINUE
 240        CONTINUE
 220     CONTINUE   
C
         CALL DTAUNO (JRWORK, ZI(JNOEU), NBNO, NBORDR, NNOINI,
     &                NBNOP, NUMPAQ, TSPAQ, NOMMET, NOMCRI,
     &                NOMMAI, CNSR)
C
 200  CONTINUE
C
      WRITE(6,*)' '
C
C TRANSFORMATION D'UN CHAM_NO SIMPLE EN CHAM_NO
C
      IF ( TYPRES .EQ. 'EVOL_ELAS' ) THEN
         CALL RSEXCH( NOMSD, 'SIGM_NOEU_DEPL', 1, CHSIG, IRET )     
      ELSEIF ( TYPRES .EQ. 'EVOL_NOLI' ) THEN
         CALL RSEXCH( NOMSD, 'SIEF_NOEU_ELGA', 1, CHSIG, IRET )
      ENDIF
      CALL CNSCNO(CNSR,' ','G',NOMU)
C
C MENAGE
C
      CALL DETRSD('CHAM_NO',CELBID)
      CALL DETRSD('CHAM_NO_S',CNSR)
      CALL DETRSD('CHAM_NO_S',CHSIGS)
      CALL DETRSD('CHAM_NO_S',CNS1)
      CALL DETRSD('CHAM_NO_S',CNS2)
C
      CALL JEDETR('&&PAQNOE.NUME_ORDRE')
      CALL JEDETR('&&PAQNOE.RWORK')
      CALL JEDETR('&&PAQNOE.PAQNO')
C
      CALL JEDEMA()
      END
