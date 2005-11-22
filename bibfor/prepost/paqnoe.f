      SUBROUTINE PAQNOE(NOMSD, NOMU, NOMMAI, NOMMET, NOMCRI,
     &                  TYPCHA, PROAXE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 21/11/2005   AUTEUR F1BHHAJ J.ANGLES 
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
C RESPONSABLE F1BHHAJ J.ANGLES
      IMPLICIT     NONE
      CHARACTER*8  NOMSD, NOMU, NOMMAI
      CHARACTER*16 NOMMET, NOMCRI, TYPCHA, PROAXE
C ---------------------------------------------------------------------
C BUT: CONSTRUIRE LES PAQUETS DE NOEUDS AFIN DE CALCULER LE VECTEUR
C      CISAILLEMENT TAU DANS LE PLAN u, v.
C ---------------------------------------------------------------------
C ARGUMENTS:
C NOMSD    IN    K8 : NOM DE LA STRUCTURE DE DONNEES RESULTAT (CALC_NO).
C NOMU     IN    K8 : NOM UTILISATEUR DU CALCUL EN FATIGUE.
C NOMMAI   IN    K8 : NOM UTILISATEUR DU MAILLAGE.
C NOMMET   IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
C                     CIRCONSCRIT.
C NOMCRI   IN    K16: NOM DU CRITERE D'ENDOMMAGEMENT PAR FATIGUE.
C TYPCHA   IN    K16: TYPE DE CHARGEMENT (PERIODIQUE OU NON).
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
      INTEGER       IBID, IERD, LORDR, JORDR, NBORDR, NDIM, IRET, IRET1 
      INTEGER       NBNO, INO, TDISP, JRWORK, TPAQ
      INTEGER       NBPAQ, NUMPAQ, NNOPAQ, BORMAX, NBPMAX, NBP0, BOR0
      INTEGER       NBCMP, JPAQNO
      INTEGER       NNOINI, NBNOP, TSPAQ, IORDR, LOR8EM, LOISEM
      INTEGER       JSIGV, JSIGD, JSIGL, JEPSD, JEPSL, JEPSV, KWORK
      INTEGER       INOP, SOMNOW, ICMP, JNOEU, NUNOE
C
      REAL*8        R8B, VAL1, VAL2, VRESU(18)
C
      COMPLEX*16    C16B
C
      CHARACTER*4   LSIG(6), LEPS(6)
      CHARACTER*8   K8B, LRESU(22), MOTCLE(4), TYMOCL(4)
      CHARACTER*16  TYPRES
      CHARACTER*19  CNSR, LISNOE
      CHARACTER*19  CHSIG, CHEPS, CNS1, CNS2, CNS3, CNS4
C
C-----------------------------------------------------------------------
C234567                                                              012
C-----------------------------------------------------------------------
      DATA  LSIG/ 'SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ', 'SIYZ' /
C
      DATA  LEPS/ 'EPXX', 'EPYY', 'EPZZ', 'EPXY', 'EPXZ', 'EPYZ' /
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

      CALL DISMOI('F','TYPE_RESU',NOMSD,'RESULTAT',IBID,TYPRES,IERD)
      IF ( (TYPRES(1:9) .NE. 'EVOL_ELAS') .AND.
     &     (TYPRES(1:9) .NE. 'EVOL_NOLI') ) THEN
          CALL UTMESS('F', 'PAQNOE.1', 'LE TYPE DU CONCEPT RESULTAT '//
     &                ' N''EST NI EVOL_ELAS, NI EVOL_NOLI.')
      ENDIF

C CONSTRUCTION DU CHAMP SIMPLE DESTINE A RECEVOIR LES RESULTATS :
C DTAUM,....

      CNSR = '&&PAQNOE.FACY'
      CALL CNSCRE( NOMMAI, 'FACY_R', 22, LRESU, 'V', CNSR )

C RECUPERATION DU NOMBRE DE NUMEROS D'ORDRE ET DE LA LISTE 
C DES NUMEROS D'ORDRE

      CALL RSORAC( NOMSD, 'TOUT_ORDRE', IBID, R8B, K8B, C16B, R8B, K8B,
     &             LORDR, 1, NBORDR )

      IF ( NBORDR .LT. 0 ) THEN
         NDIM = -NBORDR
      ELSEIF (NBORDR .GT. 0) THEN
         NDIM = NBORDR
      ENDIF

      CALL WKVECT('&&PAQNOE.NUME_ORDRE','V V I',NDIM,JORDR)
      CALL RSORAC( NOMSD, 'TOUT_ORDRE', IBID, R8B, K8B, C16B, R8B, K8B,
     &             ZI(JORDR), NDIM, NBORDR )

      IF (ZI(JORDR) .EQ. 0) THEN
         CALL UTMESS('A','PAQNOE',
     &   'VOUS AVEZ PROBABLEMENT ARCHIVE L ETAT INITIAL DANS'//
     &   ' LA COMMANDE STAT_NON_LINE.'//
     &   ' CELA CORRESPOND AU NUMERO D ORDRE 0. NOUS NE TENONS PAS'//
     &   ' COMPTE DU RESULTAT A CE NUMERO D ORDRE POUR LE CALCUL DE'//
     &   ' DE LA FATIGUE.')
         NBORDR = NBORDR - 1
      ENDIF

C CREATION D'UN OBJET JEVEUX CONTENANT LA LISTE DES NUMEROS
C DE NOEUDS AINSI QUE LE NOMBRE DE NOEUDS

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

      WRITE(6,*)'NOMBRE TOTAL DE NOEUDS A TRAITER ==>',NBNO
      WRITE(6,*)' '
      WRITE(6,*)'NUMERO DU PAQUET DE NOEUDS   -   ' //
     &           'NOMBRE DE NOEUDS TRAITES'

C CONSTRUCTION DES PAQUETS DE NOEUDS.

C 1/ DIMENSIONNEMENT DU VECTEUR DE TRAVAIL (RWORK) ET DU VECTEUR
C    CONTENANT LES CARACTERISTIQUES DES PAQUETS DE NOEUDS (PAQNO).
C    JEDISP REND LA DIMENSION EN ENTIERS, ON LA CONVERTIT A L'AIDE
C    DES FONCTIONS ENVIMA POUR ALLOUER UN TABLEAU DE REELS
      CALL JEDISP(1, TDISP)
      TDISP = (TDISP * LOISEM()) / LOR8EM() 
      TDISP = INT(0.6D0*TDISP)
      CALL WKVECT( '&&PAQNOE.RWORK', 'V V R', TDISP, JRWORK )

      IF ( NOMCRI(1:12) .EQ. 'FATEMI_SOCIE' ) THEN
         NBCMP = 12
      ELSE
         NBCMP = 6
      ENDIF      
      BORMAX = NBNO*NBORDR*NBCMP
      VAL1 = DBLE(TDISP)/DBLE(BORMAX)

C     ON TIENT COMPTE DU RECUL DE DEUX NOEUDS POUR CALCULER NBPMAX
      IF (VAL1 .LT. 1.0D0) THEN
         NBP0 = INT(1.0D0/VAL1) + 1
         BOR0 = NBP0*2*NBORDR*NBCMP
         VAL2 = DBLE(TDISP)/DBLE(BORMAX+BOR0)
         NBPMAX = INT(1.0D0/VAL2) + 1
      ELSE
         NBPMAX = 2
      ENDIF
      CALL WKVECT( '&&PAQNOE.PAQNO', 'V V I', NBPMAX*4, JPAQNO )

C TPAQ   = TAILLE DU PAQUET DE NOEUDS
C NBPAQ  = NOMBRE DE PAQUET(S) DE NOEUDS
C NUMPAQ = NUMERO DU PAQUET DE NOEUDS
C NNOPAQ = NOMBRE DE NOEUDS DANS LE PAQUET DE NOEUDS
C ZI(JPAQNO + (NUMPAQ-1)*4 + 2) = NUMERO DU NOEUD INITIAL DANS LE PAQUET

      TPAQ = 0
      NBPAQ = 0
      NUMPAQ = 0
      NNOPAQ = 0

      DO 100 INO=1, NBNO
         TPAQ = TPAQ + NBORDR*NBCMP
         NNOPAQ = NNOPAQ + 1

         IF ( TPAQ .LT. TDISP ) THEN
            IF (INO .EQ. NBNO) THEN
               NUMPAQ = NUMPAQ + 1
               ZI(JPAQNO + (NUMPAQ-1)*4) = NUMPAQ
               ZI(JPAQNO + (NUMPAQ-1)*4 + 1) = TPAQ
               ZI(JPAQNO + (NUMPAQ-1)*4 + 2) = INO - (NNOPAQ - 1)
               ZI(JPAQNO + (NUMPAQ-1)*4 + 3) = NNOPAQ
               NBPAQ = NUMPAQ
            ENDIF

         ELSEIF ( ( TPAQ .GE. TDISP ) .AND. (INO .LT. 3) ) THEN
            CALL UTDEBM('F', 'PAQNOE.2', 'LA TAILLE MEMOIRE '//
     &                   ' NECESSAIRE AU VECTEUR DE TRAVAIL '//
     &                   ' EST TROP IMPORTANTE '//
     &                   ' PAR RAPPORT A LA PLACE DISPONIBLE.')
            CALL UTIMPI('L', 'TAILLE DISPONIBLE : ', 1, TDISP)
            CALL UTIMPI('L', 'TAILLE NECESSAIRE : ', 1, TPAQ)
            CALL UTFINM( )

C 2/ STOCKAGE DES NUMEROS DES PAQUETS, DE LA TAILLE DES PAQUETS,
C    DU NUMERO DE LA PREMIERE MAILLE DE CHAQUE PAQUET DE MAILLES,
C    DU NOMBRE DE MAILLE DE CHAQUE PAQUET ET DU NOMBRE DE PAQUET. 

         ELSEIF ( ( TPAQ .GE. TDISP ) .AND. (INO .GT. 2) ) THEN
C ON RECULE DE DEUX NOEUDS POUR ETRE SUR DE NE PAS DEBORDER DU VECTEUR
C DE TRAVAIL (JRWORK). 

            TPAQ = TPAQ - (2*NBORDR*NBCMP)
            NUMPAQ = NUMPAQ + 1
            ZI(JPAQNO + (NUMPAQ-1)*4) = NUMPAQ
            ZI(JPAQNO + (NUMPAQ-1)*4 + 1) = TPAQ
            ZI(JPAQNO + (NUMPAQ-1)*4 + 2) = INO - (NNOPAQ - 1)
            ZI(JPAQNO + (NUMPAQ-1)*4 + 3) = NNOPAQ - 2
            NBPAQ = NUMPAQ

            TPAQ = 2*NBORDR*NBCMP
            NNOPAQ = 2
            IF (INO .EQ. NBNO) THEN
               NUMPAQ = NUMPAQ + 1
               ZI(JPAQNO + (NUMPAQ-1)*4) = NUMPAQ
               ZI(JPAQNO + (NUMPAQ-1)*4 + 1) = TPAQ
               ZI(JPAQNO + (NUMPAQ-1)*4 + 2) = INO - (NNOPAQ - 1)
               ZI(JPAQNO + (NUMPAQ-1)*4 + 3) = NNOPAQ
               NBPAQ = NUMPAQ
            ENDIF
         ENDIF

 100  CONTINUE

      IF (NBPAQ .GT. NBPMAX) THEN
         CALL UTDEBM('F', 'PAQNOE.3', 'LA TAILLE DU VECTEUR '//
     &               'CONTENANT LES CARACTERISTIQUES DES ' //
     &               'PAQUETS DE NOEUDS EST TROP PETITE.')
         CALL UTIMPI('L', 'NB DE PAQUETS MAXI : ', 1, NBPMAX)
         CALL UTIMPI('L', 'NB DE PAQUETS REELS: ', 1, NBPAQ)
         CALL UTFINM( )
      ENDIF

C TRAITEMENT DES PAQUETS DE NOEUDS.

C  <<REMPLISSAGE>> DU VECTEUR DE TRAVAIL

      DO 200 NUMPAQ=1, NBPAQ 
         CALL JERAZO('&&PAQNOE.RWORK', TDISP, 1)
         TPAQ = ZI(JPAQNO + (NUMPAQ-1)*4 + 1)
         NNOINI = ZI(JPAQNO + (NUMPAQ-1)*4 + 2)
         NBNOP = ZI(JPAQNO + (NUMPAQ-1)*4 + 3)
         TSPAQ = TPAQ/NBORDR

         IF ( NOMCRI(1:12) .NE. 'FATEMI_SOCIE' ) THEN

            DO 220 IORDR=1, NBORDR
               IF ( TYPRES .EQ. 'EVOL_ELAS' ) THEN
                  CALL RSEXCH(NOMSD, 'SIGM_NOEU_DEPL', IORDR, CHSIG,
     &                        IRET)
               ELSEIF ( TYPRES .EQ. 'EVOL_NOLI' ) THEN
                  CALL RSEXCH(NOMSD, 'SIEF_NOEU_ELGA', IORDR, CHSIG,
     &                        IRET)
               ENDIF
               IF (IRET .NE. 0) THEN
                  CALL UTMESS('F', 'PAQNOE.4', 'LES CHAMPS DE '//
     &                    'CONTRAINTES AUX NOEUDS SIGM_NOEU_DEPL OU '//
     &                    'SIEF_NOEU_ELGA N''ONT PAS ETE CALCULES.')
               ENDIF
               CNS1 = '&&PAQNOE.SIG_S1'
               CNS2 = '&&PAQNOE.SIG_ORDO'
               CALL CNOCNS(CHSIG, 'V', CNS1)
               CALL CNSRED(CNS1, 0, IBID, NBCMP, LSIG, 'V', CNS2)
               CALL JEEXIN(CNS2(1:19)//'.CNSV', IRET)
               IF (IRET .EQ. 0) THEN
                  CALL UTMESS('F', 'PAQNOE.5', 'LES CHAMPS DE '//
     &                    ' CONTRAINTES AUX NOEUDS N''EXISTENT PAS.')
               ENDIF
               CALL JEVEUO(CNS2(1:19)//'.CNSD', 'L', JSIGD)
               CALL JEVEUO(CNS2(1:19)//'.CNSL', 'L', JSIGL)
               CALL JEVEUO(CNS2(1:19)//'.CNSV', 'L', JSIGV)

               KWORK = 0
               SOMNOW = 0

               DO 240 INOP=NNOINI, NNOINI+(NBNOP-1)
                 IF ( INOP .GT. NNOINI ) THEN
                    KWORK = 1
                    SOMNOW = SOMNOW + 1
                 ENDIF

                 NUNOE = ZI(JNOEU + INOP-1)
                 DO 280 ICMP=1, 6
                   IF ( ZL(JSIGL + (ICMP-1) + (NUNOE-1)*6) ) THEN
                     ZR( JRWORK + (ICMP-1) + KWORK*SOMNOW*6 +
     &                            (IORDR-1)*TSPAQ ) =
     &               ZR( JSIGV + (ICMP-1) + (NUNOE-1)*6 )
                   ELSE
                     CALL UTMESS('F', 'PAQNOE.6', 'LE CHAMP SIMPLE '//
     &                   'QUI CONTIENT LES VALEURS DES CONTRAINTES '//
     &                   'N EXISTE PAS.')
                   ENDIF
 280             CONTINUE
 240           CONTINUE
 220        CONTINUE   

         ELSE
         
            DO 300 IORDR=1, NBORDR
               IF ( TYPRES .EQ. 'EVOL_NOLI' ) THEN
                  CALL RSEXCH(NOMSD, 'SIEF_NOEU_ELGA', IORDR, CHSIG,
     &                        IRET)
                  CALL RSEXCH(NOMSD, 'EPSI_NOEU_DEPL', IORDR, CHEPS,
     &                        IRET1)
               ELSE
                  CALL UTMESS('F', 'PAQNOE.7', 'LE CRITERE DE '//
     &                   'FATEMI ET SOCIE EST PREVU POUR FONCTIONNER '//
     &                   'APRES UN CALCUL ELASTOPLASTIQUE, '//
     &                   'SON UTILISATION APRES MECA_STATIQUE N''EST '//
     &                   'PAS PREVUE.')          
               ENDIF

               IF (IRET .NE. 0) THEN
                  CALL UTMESS('F', 'PAQNOE.8', 'LE CHAMP DE '//
     &                    'CONTRAINTES AUX NOEUDS '//
     &                    'SIEF_NOEU_ELGA N''A PAS ETE CALCULE.')
               ELSEIF (IRET1 .NE. 0) THEN
                  CALL UTMESS('F', 'PAQNOE.9', 'LE CHAMP DE '//
     &                    'DEFORMATIONS AUX NOEUDS '//
     &                    'EPSI_NOEU_DEPL N''A PAS ETE CALCULE.')
               ENDIF

               CNS1 = '&&PAQNOE.SIG_S1'
               CNS2 = '&&PAQNOE.SIG_ORDO'
               CALL CNOCNS(CHSIG, 'V', CNS1)
               CALL CNSRED(CNS1, 0, IBID, 6, LSIG, 'V', CNS2)
               CALL JEEXIN(CNS2(1:19)//'.CNSV', IRET)
               IF (IRET .EQ. 0) THEN
                  CALL UTMESS('F', 'PAQNOE.10', 'LE CHAMP DE '//
     &                    ' CONTRAINTES AUX NOEUDS N''EXISTE PAS.')
               ENDIF
               CALL JEVEUO(CNS2(1:19)//'.CNSD', 'L', JSIGD)
               CALL JEVEUO(CNS2(1:19)//'.CNSL', 'L', JSIGL)
               CALL JEVEUO(CNS2(1:19)//'.CNSV', 'L', JSIGV)

               CNS3 = '&&PAQNOE.EPS_S3'
               CNS4 = '&&PAQNOE.EPS_ORDO'
               CALL CNOCNS(CHEPS, 'V', CNS3)
               CALL CNSRED(CNS3, 0, IBID, 6, LEPS, 'V', CNS4)
               CALL JEEXIN(CNS4(1:19)//'.CNSV', IRET)
               IF (IRET .EQ. 0) THEN
                  CALL UTMESS('F', 'PAQNOE.11', 'LE CHAMP DE '//
     &                    ' DEFORMATIONS AUX NOEUDS N''EXISTE PAS.')
               ENDIF
               CALL JEVEUO(CNS4(1:19)//'.CNSD', 'L', JEPSD)
               CALL JEVEUO(CNS4(1:19)//'.CNSL', 'L', JEPSL)
               CALL JEVEUO(CNS4(1:19)//'.CNSV', 'L', JEPSV)

               KWORK = 0
               SOMNOW = 0

               DO 320 INOP=NNOINI, NNOINI+(NBNOP-1)
                 IF ( INOP .GT. NNOINI ) THEN
                    KWORK = 1
                    SOMNOW = SOMNOW + 1
                 ENDIF

                 NUNOE = ZI(JNOEU + INOP-1)

C BOUCLE SUR LES CONTRAINTES (6 COMPOSANTES)
                 DO 340 ICMP=1, 6
                   IF ( ZL(JSIGL + (ICMP-1) + (NUNOE-1)*6) ) THEN
                     ZR( JRWORK + (ICMP-1) + KWORK*SOMNOW*12 +
     &                            (IORDR-1)*TSPAQ ) =
     &               ZR( JSIGV + (ICMP-1) + (NUNOE-1)*6 )
                   ELSE
                     CALL UTMESS('F', 'PAQNOE.12', 'LE CHAMP SIMPLE '//
     &                   'QUI CONTIENT LES VALEURS DES CONTRAINTES '//
     &                   'N EXISTE PAS.')
                   ENDIF
 340             CONTINUE

C BOUCLE SUR LES DEFORMATIONS (6 COMPOSANTES)
                 DO 360 ICMP=1, 6
                   IF ( ZL(JEPSL + (ICMP-1) + (NUNOE-1)*6) ) THEN
                     ZR( JRWORK + (ICMP+6-1) + KWORK*SOMNOW*12 +
     &                            (IORDR-1)*TSPAQ ) =
     &               ZR( JEPSV + (ICMP-1) + (NUNOE-1)*6 )
                   ELSE
                     CALL UTMESS('F', 'PAQNOE.13', 'LE CHAMP SIMPLE '//
     &                   'QUI CONTIENT LES VALEURS DES DEFORMATIONS '//
     &                   'N EXISTE PAS.')
                   ENDIF
 360             CONTINUE

 320           CONTINUE
 300        CONTINUE   
         
         ENDIF

         IF (TYPCHA .EQ. 'PERIODIQUE') THEN
            CALL DTAUNO (JRWORK, ZI(JNOEU), NBNO, NBORDR, NNOINI,
     &                   NBNOP, NUMPAQ, TSPAQ, NOMMET, NOMCRI,
     &                   NOMMAI, CNSR)

         ELSEIF (TYPCHA .EQ. 'NON_PERIODIQUE') THEN
            CALL AVGRNO (ZR(JRWORK), TDISP, ZI(JNOEU), NBNO, NBORDR,
     &                   NNOINI, NBNOP, NUMPAQ, TSPAQ, NOMCRI,
     &                   NOMMAI, PROAXE, CNSR)
         ENDIF

 200  CONTINUE

      WRITE(6,*)' '

C TRANSFORMATION D'UN CHAM_NO SIMPLE EN CHAM_NO

      CALL CNSCNO(CNSR,' ','NON','G',NOMU)

C MENAGE

      CALL DETRSD('CHAM_NO_S',CNSR)
      CALL DETRSD('CHAM_NO_S',CNS1)
      CALL DETRSD('CHAM_NO_S',CNS2)
      CALL DETRSD('CHAM_NO_S',CNS3)
      CALL DETRSD('CHAM_NO_S',CNS4)

      CALL JEDETR('&&PAQNOE.NUME_ORDRE')
      CALL JEDETR('&&PAQNOE.RWORK')
      CALL JEDETR('&&PAQNOE.PAQNO')
C
      CALL JEDEMA()
      END
