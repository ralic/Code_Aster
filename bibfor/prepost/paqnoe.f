      SUBROUTINE PAQNOE(NOMSD, NOMU, NOMMAI, NOMMET, NOMCRI,NOMFOR,
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
C RESPONSABLE ANGLES J.ANGLES
C TOLE  CRP_20
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8  NOMSD, NOMU, NOMMAI, GRDVIE,INSCRI
      CHARACTER*16 NOMMET, NOMCRI, TYPCHA, PROAXE, NOMFOR, FORVIE
      LOGICAL      FORDEF
      REAL*8       INSTIC,PREC
C ---------------------------------------------------------------------
C BUT: CONSTRUIRE LES PAQUETS DE NOEUDS AFIN DE CALCULER LE VECTEUR
C      CISAILLEMENT TAU DANS LE PLAN u, v.
C ---------------------------------------------------------------------
C ARGUMENTS:
C NOMSD    IN    K8 : NOM DE LA STRUCTURE DE DONNEES RESULTAT.
C NOMU     IN    K8 : NOM UTILISATEUR DU CALCUL EN FATIGUE.
C NOMMAI   IN    K8 : NOM UTILISATEUR DU MAILLAGE.
C NOMMET   IN    K16: NOM DE LA METHODE DE CALCUL DU CERCLE
C                     CIRCONSCRIT.
C NOMCRI   IN    K16: NOM DU CRITERE D'ENDOMMAGEMENT PAR FATIGUE.
C NOMFOR   IN    K16: LE NOM DE FORMULE DE GRNADUER EQUIVALENTE
C GRDVIE   IN    K16: NOM DE LA COURBE GRANDEUR EQUIVALENT _DUREE VIE
C TYPCHA   IN    K16: TYPE DE CHARGEMENT (PERIODIQUE OU NON).
C NOMCRI   IN    K16: NOM DU CRITERE AVEC PLANS CRITIQUES.
C
C-----------------------------------------------------------------------
      INTEGER       IBID, IERD, LORDR, JORDR, NBORDR, NDIM, IRET, IRET1
      INTEGER       NBNO, INO, TDISP, JRWORK, TPAQ, IRET2, IRET3
      INTEGER       NBPAQ, NUMPAQ, NNOPAQ, BORMAX, NBPMAX, NBP0, BOR0
      INTEGER       NBCMP, JPAQNO, K, ORDINI, IORD, JINST
      INTEGER       NNOINI, NBNOP, TSPAQ, IORDR, LOR8EM, LOISEM
      INTEGER       JSIGV, JSIGD, JSIGL, JEPSD, JEPSL, JEPSV, KWORK
      INTEGER       JEPSPD, JEPSPL,JEPSPV, IRET4,JEPPED, JEPPEL
      INTEGER       INOP, SOMNOW, ICMP, JNOEU, NUNOE, JEPPEV
      INTEGER       VALI(2), PARACT(30), VALEP, IC
      LOGICAL       POST, CRSIGM, CREPST, CREPSE,CREPSP, LBID,CREPPE
C
      REAL*8        R8B, VAL1, VAL2, RESU(4),VRESU(24),VALPAR(22),R8PREM
      COMPLEX*16    C16B
C
      CHARACTER*1   KBID
      CHARACTER*4   LSIG(6), LEPS(6)
      CHARACTER*8   K8B, LRESU(24), MOTCLE(4), TYMOCL(4)
      CHARACTER*16  TYPRES, NOMOPT
      CHARACTER*19  CNSR, LISNOE, CHEPPE, CNS7, CNS8
      CHARACTER*19  CHSIG, CHEPS, CNS1, CNS2, CNS3, CNS4,CHSIG1,CHSIG2
      CHARACTER*19  CHEPSP, CNS5, CNS6

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
     &             'NBRUP2', 'ENDO2' ,'VMIS', 'TRESCA' /
C-----------------------------------------------------------------------
C
      CALL JEMARQ()

C               1234567890123456789
      CHSIG  = '                   '
      CHSIG1 = '                   '
      CHSIG2 = '                   '
      CHEPS = '                   '
      CHEPSP = '                   '
C RECUPERATION DU TYPE DE CALCUL MECANIQUE EFFECTUE

      CALL DISMOI('F','TYPE_RESU',NOMSD,'RESULTAT',IBID,TYPRES,IERD)
      IF ( (TYPRES(1:9) .NE. 'EVOL_ELAS') .AND.
     &     (TYPRES(1:9) .NE. 'EVOL_NOLI') ) THEN
          CALL U2MESS('F','PREPOST4_26')
      ENDIF

C CONSTRUCTION DU CHAMP SIMPLE DESTINE A RECEVOIR LES RESULTATS :
C DTAUM,....

      CNSR = '&&PAQNOE.FACY'
      CALL CNSCRE( NOMMAI, 'FACY_R', 24, LRESU, 'V', CNSR )

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
      ORDINI = 1
      DO 400 K = 2, NBORDR
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
400   CONTINUE
410   CONTINUE

      IF  ((ORDINI .EQ. 1) .AND.
     &   ((INSCRI .EQ.'ABSOLU') .OR. (INSCRI .EQ.'RELATIF') ) ) THEN
         CALL U2MESS('A','PREPOST4_48')
      ENDIF

      IF (ZI(JORDR) .EQ. 0) THEN
         CALL U2MESS('A','PREPOST4_27')
         NBORDR = NBORDR - 1
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

      NBCMP = 18

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
         VALI (1) = TDISP
         VALI (2) = TPAQ
            CALL U2MESG('F', 'PREPOST5_67',0,' ',2,VALI,0,0.D0)

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
         VALI (1) = NBPMAX
         VALI (2) = NBPAQ
         CALL U2MESG('F', 'PREPOST5_70',0,' ',2,VALI,0,0.D0)
      ENDIF

C TRAITEMENT DES PAQUETS DE NOEUDS.

C  <<REMPLISSAGE>> DU VECTEUR DE TRAVAIL

      DO 200 NUMPAQ=1, NBPAQ
         CALL JERAZO('&&PAQNOE.RWORK', TDISP, 1)
         TPAQ = ZI(JPAQNO + (NUMPAQ-1)*4 + 1)
         NNOINI = ZI(JPAQNO + (NUMPAQ-1)*4 + 2)
         NBNOP = ZI(JPAQNO + (NUMPAQ-1)*4 + 3)
         TSPAQ = TPAQ/NBORDR

            DO 300 IORDR=1, NBORDR

C IF CRITERE CONTIENT CONTRAINTE
               IF (CRSIGM) THEN

                  CALL RSEXCH(' ',NOMSD,'SIGM_NOEU',IORDR,CHSIG1,IRET1)
                  CALL RSEXCH(' ',NOMSD,'SIEF_NOEU',IORDR,CHSIG2,IRET2)

                  IF ((IRET1.NE.0).AND.(IRET2.NE.0)) THEN
                     CALL U2MESS('A','PREPOST4_38')
                  ENDIF

                  IF (IRET1.EQ.0) THEN
                     CHSIG = CHSIG1
                  ELSE IF (IRET2.EQ.0) THEN
                     CHSIG = CHSIG2
                  ENDIF

                  CNS1 = '&&PAQNOE.SIG_S1'
                  CNS2 = '&&PAQNOE.SIG_ORDO'
                  CALL CNOCNS(CHSIG, 'V', CNS1)
                  CALL CNSRED(CNS1, 0, IBID, 6, LSIG, 'V', CNS2)
                  CALL JEEXIN(CNS2(1:19)//'.CNSV', IRET)
                  IF (IRET .EQ. 0) THEN
                     CALL U2MESS('F','PREPOST4_40')
                  ENDIF
                  CALL JEVEUO(CNS2(1:19)//'.CNSD', 'L', JSIGD)
                  CALL JEVEUO(CNS2(1:19)//'.CNSL', 'L', JSIGL)
                  CALL JEVEUO(CNS2(1:19)//'.CNSV', 'L', JSIGV)
               ENDIF

C IF CRITERE CONTIENT DEFORMATION TOTALE
               IF (CREPST) THEN

                  CALL RSEXCH('F',NOMSD,'EPSI_NOEU',IORDR,CHEPS,IRET3)

                  CNS3 = '&&PAQNOE.EPS_S3'
                  CNS4 = '&&PAQNOE.EPS_ORDO'
                  CALL CNOCNS(CHEPS, 'V', CNS3)
                  CALL CNSRED(CNS3, 0, IBID, 6, LEPS, 'V', CNS4)
                  CALL JEEXIN(CNS4(1:19)//'.CNSV', IRET)
                  IF (IRET .EQ. 0) THEN
                     CALL U2MESS('F','PREPOST4_41')
                  ENDIF
                  CALL JEVEUO(CNS4(1:19)//'.CNSD', 'L', JEPSD)
                  CALL JEVEUO(CNS4(1:19)//'.CNSL', 'L', JEPSL)
                  CALL JEVEUO(CNS4(1:19)//'.CNSV', 'L', JEPSV)

               ENDIF

C IF CRITERE CONTIENT DEFORMATION PLASTQIUE
               IF (CREPSP) THEN

                  CALL RSEXCH('F',NOMSD,'EPSP_NOEU',IORDR,CHEPSP,IRET4)

                  CNS5 = '&&PAQNOE.EPSP_S5'
                  CNS6 = '&&PAQNOE.EPSP_ORDO'
                  CALL CNOCNS(CHEPSP, 'V', CNS5)
                  CALL CNSRED(CNS5, 0, IBID, 6, LEPS, 'V', CNS6)
                  CALL JEEXIN(CNS6(1:19)//'.CNSV', IRET)
                  IF (IRET .EQ. 0) THEN
                     CALL U2MESS('F','PREPOST4_43')
                  ENDIF
                  CALL JEVEUO(CNS6(1:19)//'.CNSD', 'L', JEPSPD)
                  CALL JEVEUO(CNS6(1:19)//'.CNSL', 'L', JEPSPL)
                  CALL JEVEUO(CNS6(1:19)//'.CNSV', 'L', JEPSPV)

               ENDIF

C IF CRITERE CONTIENT DEFORMATION ELASTIQUE
               IF (CREPPE) THEN

                  CALL RSEXCH(' ',NOMSD, 'EPSP_NOEU', IORDR, CHEPPE,
     &                            VALEP)
                  IF (VALEP .NE. 0) THEN
                     CALL U2MESS('A','PREPOST4_46')
                  ENDIF
                  IF (VALEP .EQ. 0) THEN
                     CNS7 = '&&PAQNOE.EPSPE_S7'
                     CNS8 = '&&PAQNOE.EPSPE_ORDO'
                     CALL CNOCNS(CHEPPE, 'V', CNS7)
                     CALL CNSRED(CNS7, 0, IBID, 6, LEPS, 'V', CNS8)
                     CALL JEEXIN(CNS8(1:19)//'.CNSV', IRET)
                     IF (IRET .EQ. 0) THEN
                        CALL U2MESS('F','PREPOST4_43')
                     ENDIF
                     CALL JEVEUO(CNS8(1:19)//'.CNSD', 'L', JEPPED)
                     CALL JEVEUO(CNS8(1:19)//'.CNSL', 'L', JEPPEL)
                     CALL JEVEUO(CNS8(1:19)//'.CNSV', 'L', JEPPEV)
                  ENDIF
               ENDIF

               KWORK = 0
               SOMNOW = 0

               DO 320 INOP=NNOINI, NNOINI+(NBNOP-1)
                 IF ( INOP .GT. NNOINI ) THEN
                    KWORK = 1
                    SOMNOW = SOMNOW + 1
                 ENDIF

                 NUNOE = ZI(JNOEU + INOP-1)

C BOUCLE SUR LES CONTRAINTES (6 COMPOSANTES)
                 IF (CRSIGM) THEN
                    DO 340 ICMP=1, 6
                      IF ( ZL(JSIGL + (ICMP-1) + (NUNOE-1)*6) ) THEN
                        ZR( JRWORK + (ICMP-1) + KWORK*SOMNOW*18 +
     &                               (IORDR-1)*TSPAQ ) =
     &                  ZR( JSIGV + (ICMP-1) + (NUNOE-1)*6 )
                      ELSEIF (ICMP .EQ. 5) THEN
                         CALL U2MESI('F', 'FATIGUE1_2', 1, ICMP)
                      ELSE
                        CALL U2MESS('F','PREPOST4_30')
                      ENDIF
 340                CONTINUE
                 ENDIF

C BOUCLE SUR LES DEFORMATIONS TOTALES (6 COMPOSANTES)
                 IF (CREPST) THEN
                    DO 360 ICMP=1, 6
                      IF ( ZL(JEPSL + (ICMP-1) + (NUNOE-1)*6) ) THEN
                        ZR( JRWORK + (ICMP+6-1) + KWORK*SOMNOW*18 +
     &                               (IORDR-1)*TSPAQ ) =
     &                  ZR( JEPSV + (ICMP-1) + (NUNOE-1)*6 )
                      ELSEIF (ICMP .EQ. 5) THEN
                         CALL U2MESI('F', 'FATIGUE1_3', 1, ICMP)
                      ELSE
                        CALL U2MESS('F','PREPOST4_35')
                      ENDIF
 360                CONTINUE
                 ENDIF

C BOUCLE SUR LES DEFORMATIONS TOTALES (6 COMPOSANTES)
                 IF (CREPSP) THEN
                    DO 380 ICMP=1, 6
                      IF ( ZL(JEPSPL + (ICMP-1) + (NUNOE-1)*6) ) THEN
                        ZR( JRWORK + (ICMP+6+6-1) + KWORK*SOMNOW*18 +
     &                               (IORDR-1)*TSPAQ ) =
     &                  ZR( JEPSPV + (ICMP-1) + (NUNOE-1)*6 )
                      ELSEIF (ICMP .EQ. 5) THEN
                         CALL U2MESI('F', 'FATIGUE1_3', 1, ICMP)
                      ELSE
                        CALL U2MESS('F','PREPOST4_35')
                      ENDIF
 380                CONTINUE
                 ENDIF

                 IF (CREPPE) THEN
                  IF (VALEP .EQ. 0) THEN

                    DO 390 ICMP=1, 6
                      IF ( ZL(JEPPEL + (ICMP-1) + (NUNOE-1)*6) ) THEN
                        ZR( JRWORK + (ICMP+6+6-1) + KWORK*SOMNOW*18 +
     &                               (IORDR-1)*TSPAQ ) =
     &                  ZR( JEPPEV + (ICMP-1) + (NUNOE-1)*6 )
                      ELSEIF (ICMP .EQ. 5) THEN
                         CALL U2MESI('F', 'FATIGUE1_3', 1, ICMP)
                      ELSE
                        CALL U2MESS('F','PREPOST4_35')
                      ENDIF
 390                CONTINUE

                  ELSE
                     DO 395 IC=1, 6
                      ZR( JRWORK + (IC+6+6-1) + KWORK*SOMNOW*18 +
     &                               (IORDR-1)*TSPAQ ) = 0.D0
 395                CONTINUE
                   ENDIF
                 ENDIF

 320           CONTINUE
 300        CONTINUE




         IF ( NOMCRI(1:11) .EQ. 'VMIS_TRESCA' ) THEN
            NOMOPT = 'DOMA_NOEUD'
            CALL VAMPLI(ZR(JRWORK), TDISP, ZI(JNOEU), NBNO, NBORDR,
     &                  NNOINI, NBNOP, NUMPAQ, TSPAQ, NOMCRI,
     &                  NOMMAI, NOMOPT, CNSR)
            GOTO 200
         ENDIF

         IF (TYPCHA .EQ. 'PERIODIQUE') THEN
            POST = .FALSE.

            CALL DTAUNO (JRWORK, ZI(JNOEU), NBNO, NBORDR,ORDINI,NNOINI,
     &                   NBNOP, NUMPAQ, TSPAQ, NOMMET, NOMCRI,NOMFOR,
     &          GRDVIE, FORVIE, NOMMAI, CNSR,K8B, POST,VALPAR, VRESU )

         ELSEIF (TYPCHA .EQ. 'NON_PERIODIQUE') THEN

C   POUR POST_FATIGUE
            POST = .FALSE.

            CALL AVGRNO (ZR(JRWORK), TDISP, ZI(JNOEU), NBNO, NBORDR,
     &                   NNOINI, NBNOP, NUMPAQ, TSPAQ, NOMCRI,
     &                   NOMFOR,GRDVIE, FORVIE, FORDEF, NOMMAI,
     &                   PROAXE, K8B, CNSR, POST, RESU)
         ENDIF

 200  CONTINUE

C TRANSFORMATION D'UN CHAM_NO SIMPLE EN CHAM_NO

      CALL CNSCNO(CNSR,' ','NON','G',NOMU,'F',IBID)

C MENAGE
      CALL DETRSD('CHAM_NO_S',CNSR)

      IF (CRSIGM) THEN
         CALL DETRSD('CHAM_NO_S',CNS1)
         CALL DETRSD('CHAM_NO_S',CNS2)
      ENDIF

      IF (CREPST) THEN
         CALL DETRSD('CHAM_NO_S',CNS3)
         CALL DETRSD('CHAM_NO_S',CNS4)
      ENDIF

      IF (CREPSP) THEN
         CALL DETRSD('CHAM_NO_S',CNS5)
         CALL DETRSD('CHAM_NO_S',CNS6)
      ENDIF

      IF (((CREPPE)) .AND. (VALEP .EQ. 0)) THEN
         CALL DETRSD('CHAM_NO_S',CNS7)
         CALL DETRSD('CHAM_NO_S',CNS8)
      ENDIF

      CALL JEDETR('&&PAQNOE.NUME_ORDRE')
      CALL JEDETR('&&PAQNOE.RWORK')
      CALL JEDETR('&&PAQNOE.PAQNO')
C
      CALL JEDEMA()
      END
