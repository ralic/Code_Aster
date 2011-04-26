      SUBROUTINE CAIMCH(CHARGZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)

C       CAIMCH -- TRAITEMENT DU MOT FACTEUR CHAMNO_IMPO

C      TRAITEMENT DU MOT FACTEUR CHAMNO_IMPO DE AFFE_CHAR_MECA
C      CE MOT FACTEUR PERMET D'IMPOSER SUR DES DDL DES NOEUDS
C      D'UN MODELELES VALEURS DES COMPOSANTES DU CHAM_NO DONNE
C      APRES LE MOT CLE : CHAM_NO.

C -------------------------------------------------------
C  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
C                - JXVAR -      -   LA  CHARGE EST ENRICHIE
C                                   DE LA RELATION LINEAIRE DECRITE
C                                   CI-DESSUS.
C -------------------------------------------------------

C.========================= DEBUT DES DECLARATIONS ====================
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ------
      CHARACTER*32 JEXNUM,JEXNOM
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
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ------

C -----  ARGUMENTS
      CHARACTER*(*) CHARGZ
C ------ VARIABLES LOCALES
      CHARACTER*1 K1BID
      CHARACTER*2 TYPLAG
      CHARACTER*4 TYCH,TYPVAL,TYPCOE
      CHARACTER*8 K8BID,CHAMNO,NOMA,NOMCMP,NOMNOE,BETAF
      CHARACTER*8 CHARGE,POSLAG,NOMGD
      CHARACTER*16 MOTFAC
      CHARACTER*19 LISREL,CHAM19,PRCHNO
      CHARACTER*24 NOEUMA

      REAL*8 BETA,ALPHA
      COMPLEX*16 BETAC

C.========================= DEBUT DU CODE EXECUTABLE ==================

      CALL JEMARQ()

      MOTFAC = 'CHAMNO_IMPO'

      CALL GETFAC(MOTFAC,NLIAI)
      IF (NLIAI.EQ.0) GO TO 30

C --- INITIALISATIONS :
C     ---------------
      ZERO = 0.0D0
C --- ALPHA EST LE COEFFICIENT REEL DE LA
C --- RELATION LINEAIRE

      ALPHA = 1.0D0
C --- BETA, BETAC ET BETAF SONT LES VALEURS DU SECOND MEMBRE DE LA
C --- RELATION LINEAIRE SUIVANT QUE C'EST UN REEL, UN COMPLEXE OU
C --- UNE FONCTION, DANS NOTRE CAS C'EST UN REEL

      BETA = ZERO
      BETAC = (0.0D0,0.0D0)
      BETAF = '&FOZERO'

      CHAM19 = '                   '
      CHARGE = CHARGZ

C --- TYPE DES VALEURS AU SECOND MEMBRE DE LA RELATION

      TYPVAL = 'REEL'

C --- TYPE DES VALEURS DES COEFFICIENTS

      TYPCOE = 'REEL'

C --- NOM DE LA LISTE_RELA

      LISREL = '&CAIMCH.RLLISTE'

C --- BOUCLE SUR LES OCCURENCES DU MOT-FACTEUR CHAMNO_IMPO :
C     -------------------------------------------------------
      DO 20 IOCC = 1,NLIAI

C ---   ON REGARDE SI LES MULTIPLICATEURS DE LAGRANGE SONT A METTRE
C ---   APRES LES NOEUDS PHYSIQUES LIES PAR LA RELATION DANS LA MATRICE
C ---   ASSEMBLEE :
C ---   SI OUI TYPLAG = '22'
C ---   SI NON TYPLAG = '12'

        CALL GETVTX(MOTFAC,'NUME_LAGR',IOCC,1,1,POSLAG,IBID)
        IF (POSLAG.EQ.'APRES') THEN
          TYPLAG = '22'
        ELSE
          TYPLAG = '12'
        END IF

C ---   RECUPERATION DU CHAMNO
C       ----------------------
        CALL GETVID(MOTFAC,'CHAM_NO',IOCC,1,1,CHAMNO,NB)
        IF (NB.EQ.0) THEN
          CALL U2MESS('F','MODELISA2_83')
        END IF

        CHAM19(1:8) = CHAMNO

C ---   VERIFICATION DE L'EXISTENCE DU CHAMNO
C       -------------------------------------
        CALL JEEXIN(CHAM19//'.VALE',IRET)
        IF (IRET.EQ.0) THEN
          CALL U2MESS('F','MODELISA2_84')
        END IF

C ---   VERIFICATION DU TYPE DU CHAMP
C       -----------------------------
        CALL DISMOI('F','TYPE_CHAMP',CHAMNO,'CHAM_NO',IBID,TYCH,IER)

        IF (TYCH.NE.'NOEU') THEN
          CALL U2MESS('F','MODELISA2_85')
        END IF

C ---   RECUPERATION DE LA VALEUR DU SECOND MEMBRE DE LA RELATION
C ---   LINEAIRE
C       --------
        CALL GETVR8(MOTFAC,'COEF_MULT',IOCC,1,1,ALPHA,NB)
        IF (NB.EQ.0) THEN
          CALL U2MESS('F','MODELISA2_86')
        END IF

C ---   RECUPERATION DE LA GRANDEUR ASSOCIEE AU CHAMNO :
C       ----------------------------------------------
        CALL DISMOI('F','NOM_GD',CHAMNO,'CHAM_NO',IBID,NOMGD,IER)

C ---   RECUPERATION DU NOMBRE DE MOTS SUR-LESQUELS SONT CODEES LES
C ---   LES INCONNUES ASSOCIEES A LA GRANDEUR DE NOM NOMGD
C       --------------------------------------------------
        CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NBEC,K8BID,IER)
        IF (NBEC.GT.10) THEN
          CALL U2MESK('F','MODELISA2_87',1,NOMGD)
        END IF

C ---   RECUPERATION DU MAILLAGE ASSOCIE AU CHAM_NO
C       -------------------------------------------
        CALL DISMOI('F','NOM_MAILLA',CHAMNO,'CHAM_NO',IBID,NOMA,IER)

C ---   RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE
C       --------------------------------------------
        CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOEU,K8BID,IER)

C ---   RECUPERATION DU NOMBRE DE TERMES DU CHAM_NO
C       -------------------------------------------
        CALL DISMOI('F','NB_EQUA',CHAMNO,'CHAM_NO',NEQUA,K8BID,IER)

C ---   RECUPERATION DU PROF_CHNO DU CHAM_NO
C       ------------------------------------
        CALL DISMOI('F','PROF_CHNO',CHAMNO,'CHAM_NO',IBID,PRCHNO,IER)

C ---   RECUPERATION DU NOMBRE DE COMPOSANTES ASSOCIEES A LA LA GRANDEUR
C       ----------------------------------------------------------------
        CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'LONMAX',NBCMP,
     &              K1BID)

C ---   RECUPERATION DU NOM DES COMPOSANTES ASSOCIEES A LA LA GRANDEUR
C       --------------------------------------------------------------
        CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'L',INOCMP)

C ---   RECUPERATION DU .VALE DU CHAM_NO
C       --------------------------------
        CALL JEVEUO(CHAM19//'.VALE','E',IDVALE)

C ---   RECUPERATION DU .DEEQ DU PROF_CHNO
C       ----------------------------------
        CALL JEVEUO(PRCHNO//'.DEEQ','L',IDEEQ)

        NBTERM = NEQUA

C ---   CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
C ---   DE LA LISTE_RELA
C       ----------------
C ---     VECTEUR DU NOM DES NOEUDS
        CALL WKVECT('&&CAIMCH.LISNO','V V K8',NBTERM,IDNOEU)
C ---     VECTEUR DU NOM DES DDLS
        CALL WKVECT('&&CAIMCH.LISDDL','V V K8',NBTERM,IDDDL)
C ---      VECTEUR DES COEFFICIENTS REELS
        CALL WKVECT('&&CAIMCH.COER','V V R',NBTERM,IDCOER)
C ---     VECTEUR DES COEFFICIENTS COMPLEXES
        CALL WKVECT('&&CAIMCH.COEC','V V C',NBTERM,IDCOEC)
C ---     VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
        CALL WKVECT('&&CAIMCH.DIRECT','V V R',3*NBTERM,IDIREC)
C ---     VECTEUR DES DIMENSIONS DE CES DIRECTIONS
        CALL WKVECT('&&CAIMCH.DIME','V V I',NBTERM,IDIMEN)

C ---   COLLECTION DES NOMS DES NOEUDS DU MAILLAGE
C       ------------------------------------------
        NOEUMA = NOMA//'.NOMNOE'

C ---   AFFECTATION DES TABLEAUX DE TRAVAIL :
C       -----------------------------------
        K = 0

C ---   BOUCLE SUR LES TERMES DU CHAM_NO

        DO 10 IEQUA = 1,NEQUA

C ---     INO  : NUMERO DU NOEUD INO CORRESPONDANT AU DDL IEQUA

          INO = ZI(IDEEQ+2* (IEQUA-1)+1-1)

C ---     NUCMP  : NUMERO DE COMPOSANTE CORRESPONDANTE AU DDL IEQUA

          NUCMP = ZI(IDEEQ+2* (IEQUA-1)+2-1)

C ---     ON NE PREND PAS EN COMPTE LES MULTIPLICATEURS DE LAGRANGE
C ---     (CAS OU NUCMP < 0)

          IF (NUCMP.GT.0) THEN

C ---       RECUPERATION DU NOM DU NOEUD INO

            CALL JENUNO(JEXNUM(NOEUMA,INO),NOMNOE)

            VALE = ZR(IDVALE+IEQUA-1)

            K = K + 1
            NOMCMP = ZK8(INOCMP+NUCMP-1)
            ZK8(IDNOEU+K-1) = NOMNOE
            ZK8(IDDDL+K-1) = NOMCMP
            ZR(IDCOER+K-1) = ALPHA
            BETA = VALE

C ---       AFFECTATION DE LA RELATION A LA LISTE_RELA  :

            CALL AFRELA(ZR(IDCOER+K-1),ZC(IDCOEC+K-1),ZK8(IDDDL+K-1),
     &                  ZK8(IDNOEU+K-1),ZI(IDIMEN+K-1),RBID,1,BETA,
     &                  BETAC,BETAF,TYPCOE,TYPVAL,TYPLAG,0.D0,LISREL)
          END IF

   10   CONTINUE

        NBTERM = K

C ---   MENAGE :
C       ------
        CALL JEDETR('&&CAIMCH.LISNO')
        CALL JEDETR('&&CAIMCH.LISDDL')
        CALL JEDETR('&&CAIMCH.COER')
        CALL JEDETR('&&CAIMCH.COEC')
        CALL JEDETR('&&CAIMCH.DIRECT')
        CALL JEDETR('&&CAIMCH.DIME')

   20 CONTINUE

C --- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
C     ----------------------------------------
      CALL AFLRCH(LISREL,CHARGE)

C --- MENAGE :
C     ------
      CALL JEDETR(LISREL)

   30 CONTINUE

      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
