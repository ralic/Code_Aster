      SUBROUTINE CAFONO ( CHAR,LIGRCZ,IGREL,INEMA,NOMA,LIGRMZ,FONREE )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                         IGREL,INEMA
      CHARACTER*4                                             FONREE
      CHARACTER*8         CHAR,                   NOMA
      CHARACTER*(*)            LIGRCZ,                 LIGRMZ
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 17/11/2003   AUTEUR VABHHTS J.PELLET 
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
C     REMPLIR LA CARTE .FORNO, ET LE LIGREL POUR FORC_NO
C     -----------------------------------------------------------------
C   ARGUMENTS D'ENTREE:
C      CHAR  : NOM UTILISATEUR DU RESULTAT DE CHARGE
C      LIGRCZ: NOM DU LIGREL DE CHARGE
C      IGREL : NUMERO DU GREL DE CHARGE
C      INEMA : NUMERO  DE LA DERNIERE MAILLE TARDIVE DANS LIGRCH
C      NBTOUT: NOMBRE TOTAL DE GROUPES, NOEUDS,.. DANS LES OCCURENCES
C      NOMA  : NOM DU MAILLAGE
C      LIGRMZ: NOM DU LIGREL DE MODELE
C      FONREE  : 'FONC' OU 'REEL'
C     -----------------------------------------------------------------
C     ------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32       JEXNOM,JEXNUM
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER       NMOCL, NFONO, N2DL, N3DL, N6DL, NCOQ2D, NBCOMP
      PARAMETER     (NMOCL=10)
      INTEGER       NTYPEL(NMOCL), DDL(NMOCL), FORIMP(NMOCL)
      REAL*8        R8DGRD, DGRD, VALFOR(NMOCL)
      LOGICAL       EXISDG, VERIF
      CHARACTER*1   K1BID
      CHARACTER*8   K8BID, NOMN, TYPMCL(2), TYPLAG, VALFOF(NMOCL)
      CHARACTER*16  MOTCLE(NMOCL), MOTCLF, MOTCLS(2)
      CHARACTER*19  CARTE, LIGRMO, LIGRCH
      CHARACTER*24  LIEL, NOMNOE, NOMELE, MESNOE
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTCLF = 'FORCE_NODALE'
      CALL GETFAC ( MOTCLF, NFONO )
      IF ( NFONO .EQ. 0 ) GO TO 9999
C
      LIGRCH = LIGRCZ
      TYPLAG(1:2) = '12'
C
      VERIF = .FALSE.
      CALL GETVTX ( ' ', 'VERI_DDL', 1,1,1, K8BID, N )
      IF (K8BID(1:3).EQ.'OUI') VERIF = .TRUE.
C
      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','FORCE_NOD_2DDL' ),N2DL  )
      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','FORCE_NOD_3DDL' ),N3DL  )
      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','FORCE_NOD_6DDL' ),N6DL  )
      CALL JENONU(JEXNOM('&CATA.TE.NOMTE','FORCE_NOD_COQ2D'),NCOQ2D)
      NTYPEL(1) = N2DL
      NTYPEL(2) = N2DL
      NTYPEL(3) = N3DL
      NTYPEL(4) = N6DL
      NTYPEL(5) = N6DL
      NTYPEL(6) = N6DL

C ---------------------------------------------------
C     RECUPERATION DES MOTS-CLES DDL POSSIBLES SOUS FORCE_NODALE
C ---------------------------------------------------
      MOTCLE(1) = 'FX'
      MOTCLE(2) = 'FY'
      MOTCLE(3) = 'FZ'
      MOTCLE(4) = 'MX'
      MOTCLE(5) = 'MY'
      MOTCLE(6) = 'MZ'
      MOTCLE(7) = 'REP'
      MOTCLE(8) = 'ALPHA'
      MOTCLE(9) = 'BETA'
      MOTCLE(10) = 'GAMMA'
      NBCOMP = 10

C ---------------------------------------------------
C *** RECUPERATION DU DESCRIPTEUR GRANDEUR .PRNM
C *** DU MODELE
C ---------------------------------------------------

      CALL DISMOI('F','NB_EC','FORC_R','GRANDEUR',NBECF,K8BID,IERD)
      IF (NBECF.GT.10) THEN
        CALL UTMESS('F','CAFONO_1','LE DESCRIPTEUR_GRANDEUR DES FORCES'
     &              //' NE TIENT PAS SUR DIX ENTIERS CODES')
      ELSE
        LIGRMO = LIGRMZ
        CALL JEVEUO(LIGRMO//'.PRNM','L',JPRNM)
      END IF

      CALL DISMOI('F','NB_EC','DEPL_R','GRANDEUR',NBEC,K8BID,IERD)
      IF (NBEC.GT.10) THEN
        CALL UTMESS('F','CAFONO_1',
     &              'LE DESCRIPTEUR_GRANDEUR DES DEPLACEMENTS'//
     &              ' NE TIENT PAS SUR DIX ENTIERS CODES')
      END IF

      CALL JEVEUO(LIGRCH//'.NBNO','E',JNBNO)
      NOMNOE = NOMA//'.NOMNOE'
      CALL JELIRA(NOMNOE,'NOMMAX',NBNOEU,K1BID)
C
      MESNOE = '&&CAFONO.MES_NOEUDS'
      MOTCLS(1) = 'GROUP_NO'
      MOTCLS(2) = 'NOEUD'
      TYPMCL(1) = 'GROUP_NO'
      TYPMCL(2) = 'NOEUD'

C ---------------------------------------------------
C     ALLOCATION DE TABLEAUX DE TRAVAIL
C ---------------------------------------------------
C   OBJETS INTERMEDIAIRES PERMETTANT D'APPLIQUER LA REGLE DE SURCHARGE
C        -  VECTEUR (K8) CONTENANT LES NOMS DES NOEUDS
C        -  TABLEAU DES VALEURS DES DDLS DES FORCES IMPOSEES
C                         DIM NBNOEU * NBCOMP
C        -  VECTEUR (IS) CONTENANT LE DESCRIPTEUR GRANDEUR ASSOCIE AUX
C                         FORCES IMPOSEES PAR NOEUD

      CALL WKVECT('&&CAFONO.NOMS_NOEUDS','V V K8',NBNOEU,JNONO)
      IF (FONREE.EQ.'REEL') THEN
        CALL WKVECT('&&CAFONO.VALDDLR','V V R',NBCOMP*NBNOEU,JVAL)
      ELSE
        CALL WKVECT('&&CAFONO.VALDDLF','V V K8',NBCOMP*NBNOEU,JVAL)
      END IF
      CALL WKVECT('&&CAFONO.DESGI','V V I',NBNOEU,JDESGI)

      DGRD = R8DGRD()
      IF (FONREE.EQ.'FONC') THEN
        DO 10 I = 1,NBCOMP*NBNOEU
          ZK8(JVAL-1+I) = '&FOZERO'
   10   CONTINUE
      END IF
      NSURCH = 0
      LON = 0

C --------------------------------------------------------------
C     BOUCLE SUR LES OCCURENCES DU MOT-CLE FACTEUR FORCE_NODALE
C --------------------------------------------------------------

      DO 110 I = 1,NFONO
        DO 20 II = 1,NBCOMP
          FORIMP(II) = 0
   20   CONTINUE

        IF (FONREE.EQ.'REEL') THEN
          DO 30 J = 1,6
            CALL GETVR8( MOTCLF, MOTCLE(J), I,1,1, VALFOR(J), FORIMP(J))
   30     CONTINUE

          CALL GETVR8 ( MOTCLF, 'ANGL_NAUT', I,1,3, VALFOR(8), NANGL )
          IF (NANGL.NE.0) THEN
C              --- REPERE UTILISATEUR ---
            VALFOR(7) = -1.D0
            FORIMP(7) = 1
            DO 40 II = 1,MIN(3,ABS(NANGL))
              VALFOR(7+II) = VALFOR(7+II)*DGRD
              FORIMP(7+II) = 1
   40       CONTINUE
          ELSE
C              --- REPERE GLOBAL ---
            VALFOR(7) = 0.D0
          END IF

        ELSE IF (FONREE.EQ.'FONC') THEN
          DO 50 II = 1,NBCOMP
            VALFOF(II) = '&FOZERO'
   50     CONTINUE
          DO 60 J = 1,6
            CALL GETVID ( MOTCLF, MOTCLE(J), I,1,1, VALFOF(J),FORIMP(J))
   60     CONTINUE

          CALL GETVID ( MOTCLF, 'ANGL_NAUT', I,1,3, VALFOF(8), NANGL )
          IF (NANGL.NE.0) THEN
C              --- REPERE UTILISATEUR ---
            VALFOF(7) = 'UTILISAT'
            FORIMP(7) = 1
            DO 70 II = 1,MIN(3,ABS(NANGL))
              FORIMP(7+II) = 1
   70       CONTINUE
          ELSE
C              --- REPERE GLOBAL ---
            VALFOF(7) = 'GLOBAL'
          END IF
        END IF
        IF (NANGL.LT.0) THEN
          CALL UTMESS('A','CAFONO','TROP DE VALEURS D''ANGLES,'//
     &                ' ON NE GARDE QUE LES 3 PREMIERS.')
        END IF

C       ---------------------------
C       CAS DE GROUP_NO ET DE NOEUD
C       ---------------------------

        CALL RELIEM(LIGRMO, NOMA, 'NO_NOEUD', MOTCLF, I, 2,
     +                                  MOTCLS, TYPMCL, MESNOE, NBNO )
        LON = LON + NBNO
        CALL JEVEUO ( MESNOE, 'L', JNO )

        DO 100 JJ = 1,NBNO
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNO-1+JJ)),INO)
            ZK8(JNONO-1+INO) = ZK8(JNO-1+JJ)
            CALL AFFONO(ZR(JVAL),ZK8(JVAL),ZI(JDESGI+INO-1),
     &                  ZI(JPRNM-1+(INO-1)*NBEC+1),NBCOMP,FONREE,
     &                  ZK8(JNO-1+JJ),INO,NSURCH,FORIMP,VALFOR,
     &                  VALFOF,MOTCLE,VERIF,NBEC)
  100   CONTINUE

        CALL JEDETR ( MESNOE )

  110 CONTINUE

C     -----------------------------------------------
C     AFFECTATION DU LIGREL ET STOCKAGE DANS LA CARTE
C              DIMENSIONS AUX VRAIES VALEURS
C     -----------------------------------------------

      LIEL = LIGRCH//'.LIEL'
      CARTE = CHAR//'.CHME.FORNO'
C
      LON = LON - NSURCH
      LONCAR = LON
      CALL JEEXIN ( CARTE//'.DESC', IRET )

      IF ( IRET .EQ. 0 ) THEN
         IF (FONREE.EQ.'REEL') THEN
            CALL ALCART('G',CARTE,NOMA,'FORC_R',LONCAR,LON)
         ELSE IF (FONREE.EQ.'FONC') THEN
            CALL ALCART('G',CARTE,NOMA,'FORC_F',LONCAR,LON)
         ELSE
            CALL UTMESS('F','CAFONO','VALEUR INATTENDUE: '//FONREE)
         END IF
      END IF

      CALL JEVEUO ( CARTE//'.NCMP', 'E', JNCMP )
      CALL JEVEUO ( CARTE//'.VALV', 'E', JVALV )
C
      ZK8(JNCMP-1+1) = 'FX'
      ZK8(JNCMP-1+2) = 'FY'
      ZK8(JNCMP-1+3) = 'FZ'
      ZK8(JNCMP-1+4) = 'MX'
      ZK8(JNCMP-1+5) = 'MY'
      ZK8(JNCMP-1+6) = 'MZ'
      ZK8(JNCMP-1+7) = 'REP'
      ZK8(JNCMP-1+8) = 'ALPHA'
      ZK8(JNCMP-1+9) = 'BETA'
      ZK8(JNCMP-1+10) = 'GAMMA'
C
      CALL JEVEUO(LIGRCH//'.NBNO','E',JNBNO)

C     -----------------------------------------------
C     BOUCLE SUR TOUS LES NOEUDS DU MAILLAGE
C     -----------------------------------------------

      DO 150 INO = 1,NBNOEU

         IF (ZI(JDESGI-1+INO).NE.0) THEN

            NOMN = ZK8(JNONO-1+INO)
            CALL JENONU(JEXNOM(NOMNOE,NOMN),IN)
            IDGEX = JPRNM - 1 + (IN-1)*NBEC + 1

            DO 120 I = 1,6
               IF ((I-1).GT.30) CALL VERI32()
               DDL(I) = IAND(ZI(JDESGI-1+INO),2** (I-1))
               IF ((DDL(I).NE.0) .AND. (EXISDG(ZI(IDGEX),I))) THEN
                  NUMEL = NTYPEL(I)
               END IF
  120       CONTINUE
            IF (((DDL(6).NE.0).AND. (EXISDG(ZI(IDGEX),6))) .AND.
     &           (.NOT. (EXISDG(ZI(IDGEX),4)))) THEN
               NUMEL = NCOQ2D
            END IF

            IGREL = IGREL + 1
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',NUMEL),NOMELE)
            CALL NOLIGR ( LIGRCH, IGREL, NUMEL, 1, IN,' ',1,1, INEMA,
     &                    ZI(JNBNO), TYPLAG )

            CALL JEVEUO ( JEXNUM(LIEL,IGREL), 'E', JL )
            IF (FONREE.EQ.'REEL') THEN
               DO 130 I = 1,NBCOMP
                  ZR(JVALV-1+I) = ZR(JVAL-1+NBCOMP* (INO-1)+I)
  130          CONTINUE
            ELSE
               DO 140 I = 1,NBCOMP
                  ZK8(JVALV-1+I) = ZK8(JVAL-1+NBCOMP* (INO-1)+I)
  140          CONTINUE
            END IF
C
C   ON CREE UNE CARTE POUR CHAQUE NOEUD AFFECTE ET ON NOTE TOUTES
C   LES COMPOSANTES (NBCOMP)
C
            CALL NOCART(CARTE,-3,' ','NUM',1,' ',ZI(JL),LIEL,NBCOMP)
C
         END IF

  150 CONTINUE
C
      CALL JEDETR ( '&&CAFONO.NOMS_NOEUDS' )
      CALL JEDETR ( '&&CAFONO.DESGI'       )
      IF (FONREE.EQ.'REEL') THEN
         CALL JEDETR ( '&&CAFONO.VALDDLR' )
      ELSE IF (FONREE.EQ.'FONC') THEN
         CALL JEDETR ( '&&CAFONO.VALDDLF' )
      END IF
 9999 CONTINUE
      CALL JEDEMA()
      END
