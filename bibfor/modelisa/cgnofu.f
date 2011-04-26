      SUBROUTINE CGNOFU ( MOFAZ, IOCC, NOMAZ, LISNOZ, NBNO )
      IMPLICIT  NONE
      INTEGER             IOCC, NBNO
      CHARACTER*(*)       MOFAZ, NOMAZ, LISNOZ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_6
C -------------------------------------------------------
C
C       CGNOFU -- TRAITEMENT DE L'OPTION "TUNNEL"
C                 DU MOT FACTEUR CREA_GROUP_NO DE
C                 LA COMMANDE DEFI_GROUP
C
C      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_NO CONSTITUE
C      DE TOUS LES NOEUDS APPARTENANT A UN CYLINDRE CENTRE SUR
C      UN AXE DEFINI PAR L'UTILISATEUR.
C
C ----------------------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS
C                                   APPARTENANT A L'ENVELOPPE
C                                   DU CYLINDRE.
C  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR,DDOT
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
      CHARACTER*32        JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER        IRET, NRF, NLF, NBNOT, NBMAT, NBMB, NBNB, NBNC,
     &               I, J, IDCOOR, JMAIL, JNOTR, IDLINO, JTRAV, JNOBE,
     &               IDNONO, INO1, INO2, INO, JNORD, NBNOR, IREST
      REAL*8         C1(3), C2(3), NB(3), C1NB(3), C1C2(3), LC1C2, PSCA,
     &               ZERO, RFUT, RFUT2, LFUT, LCUMUL, XC1H, XC2H, R,
     &               C2NB(3), LC1NB, R8MAEM, X, Y, Z, XMIN, XMAX, LC2NB,
     &               C2H(3), YMIN, YMAX, ZMIN, ZMAX, HNB(3), C1H(3),L12
      CHARACTER*8    K8B, NOMA, PREFIX
      CHARACTER*16   MOTFAC, MOTCLE(3), TYPMCL(3)
      CHARACTER*24   LISNOE, NOMNOE, MESMAI, LISNOM
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      MOTFAC = MOFAZ
      NOMA   = NOMAZ
      LISNOE = LISNOZ
C
      NOMNOE = NOMA//'.NOMNOE'
C
      ZERO = 0.0D0
C
C --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
C     --------------------------------------------------
      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', IDCOOR )
C
C --- RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE :
C     ---------------------------------------------
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOT,K8B,IRET)
C
C --- RECUPERATION DU NOMBRE DE MAILLES DU MAILLAGE :
C     ---------------------------------------------
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMAT,K8B,IRET)
C
C --- RECUPERATION DU GROUPE DE MAILLES BETON :
C     ---------------------------------------
      MESMAI = '&&CGNOFU.MES_MAILLES'
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      MOTCLE(3) = 'TOUT'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      TYPMCL(3) = 'TOUT'
      CALL RELIEM ( ' ', NOMA, 'NU_MAILLE', MOTFAC, IOCC,
     &                                3, MOTCLE, TYPMCL, MESMAI, NBMB )
      CALL JEVEUO ( MESMAI, 'L', JMAIL )

C --- TRANSFORMATION EN LISTE DE NOEUDS

      CALL WKVECT ( '&&CGNOFU.TRAVAIL'    , 'V V I', NBNOT, JTRAV )
      CALL WKVECT ( '&&CGNOFU.NOEUD_BETON', 'V V I', NBNOT, JNOBE )
      CALL GMGNRE ( NOMA, NBNOT, ZI(JTRAV), ZI(JMAIL), NBMB,
     &                                      ZI(JNOBE), NBNB, 'TOUS' )
C
C --- RECUPERATION DES NOEUDS AXE :
C     ---------------------------
      MOTCLE(1) = 'GROUP_MA_AXE'
      MOTCLE(2) = 'MAILLE_AXE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      PREFIX    = '&&CGNOFU'
      CALL FONFIS ( PREFIX, NOMA, MOTFAC, IOCC, 2, MOTCLE, TYPMCL, 'V')
      LISNOM = PREFIX//'.FOND      .NOEU'
      CALL JELIRA ( LISNOM, 'LONMAX', NBNC, K8B )
      CALL JEVEUO ( LISNOM, 'L', IDNONO )
C
C --- RECUPERATION DU RAYON DU TUNNEL :
C     -------------------------------
      CALL GETVR8 ( MOTFAC, 'RAYON', IOCC,1,1, RFUT, NRF )
      RFUT2 = RFUT * RFUT
C
C --- RECUPERATION DE LA LONGUEUR TUNNEL A TRAITER :
C     --------------------------------------------
      LFUT = R8MAEM( )
      CALL GETVR8 ( MOTFAC, 'LONGUEUR', IOCC,1,1, LFUT, NLF )
C
      CALL WKVECT ( '&&CGNOFU.NOEUDS_CUBE'   , 'V V I', NBNOT, JNORD )
      CALL WKVECT ( '&&CGNOFU.NOEUDS_TROUVES', 'V V I', NBNOT, JNOTR )
C
      LCUMUL = ZERO
C
C     C1    : NOEUD 1 D'UN SEGMENT DE L'AXE
C     C2    : NOEUD 2 D'UN SEGMENT DE L'AXE
C     LC1C2 : LONGUEUR D'UN SEGMENT DE L'AXE
C     NB    : NOEUD DE LA BOITE A PERCER
C
C --- BOUCLE SUR LE NOMBRE DE SEGMENTS DE L'AXE :
C     -----------------------------------------
C
      IREST = 0
      DO 100 I = 1 , NBNC-1
C
C ------ RECUPERATION DE LA DIRECTION DEFINISSANT L'AXE DU SEGMENT :
C        ---------------------------------------------------------
         CALL JENONU(JEXNOM(NOMNOE,ZK8(IDNONO+I-1)),INO1)
         CALL JENONU(JEXNOM(NOMNOE,ZK8(IDNONO+I  )),INO2)
C
         C1(1) = ZR(IDCOOR-1+3*(INO1-1)+1)
         C1(2) = ZR(IDCOOR-1+3*(INO1-1)+2)
         C1(3) = ZR(IDCOOR-1+3*(INO1-1)+3)
         XMAX = C1(1) + RFUT
         XMIN = C1(1) - RFUT
         YMAX = C1(2) + RFUT
         YMIN = C1(2) - RFUT
         ZMAX = C1(3) + RFUT
         ZMIN = C1(3) - RFUT
C
         C2(1) =  ZR(IDCOOR-1+3*(INO2-1)+1)
         C2(2) =  ZR(IDCOOR-1+3*(INO2-1)+2)
         C2(3) =  ZR(IDCOOR-1+3*(INO2-1)+3)
         XMAX = MAX ( XMAX, (C2(1) + RFUT) )
         XMIN = MIN ( XMIN, (C2(1) - RFUT) )
         YMAX = MAX ( YMAX, (C2(2) + RFUT) )
         YMIN = MIN ( YMIN, (C2(2) - RFUT) )
         ZMAX = MAX ( ZMAX, (C2(3) + RFUT) )
         ZMIN = MIN ( ZMIN, (C2(3) - RFUT) )
C
         C1C2(1) = C2(1) - C1(1)
         C1C2(2) = C2(2) - C1(2)
         C1C2(3) = C2(3) - C1(3)
C
         LC1C2 = C1C2(1)*C1C2(1) + C1C2(2)*C1C2(2) + C1C2(3)*C1C2(3)
         IF ( LC1C2 .EQ. ZERO ) THEN
            CALL U2MESS('F','MODELISA3_92')
         ENDIF
         L12 = SQRT(LC1C2)
         IF ( (LCUMUL+L12) .GE. LFUT ) THEN
            IF ( IREST .NE. 0 ) GOTO 9999
            IREST = IREST + 1
            C1C2(1) = C1C2(1) / L12
            C1C2(2) = C1C2(2) / L12
            C1C2(3) = C1C2(3) / L12
            Y = LFUT - LCUMUL
            C2(1) = C1(1) + Y*C1C2(1)
            C2(2) = C1(2) + Y*C1C2(2)
            C2(3) = C1(3) + Y*C1C2(3)
            C1C2(1) = C2(1) - C1(1)
            C1C2(2) = C2(2) - C1(2)
            C1C2(3) = C2(3) - C1(3)
            LC1C2 = C1C2(1)*C1C2(1) + C1C2(2)*C1C2(2) + C1C2(3)*C1C2(3)
            LCUMUL = LFUT
         ELSE
            LCUMUL = LCUMUL + L12
         ENDIF
C
C ------ ON LIMITE LA RECHECHE AUX NOEUDS SITUES DANS UNE BOITE
C        DONT LES DIMENSIONS SONT XMAX,XMIN, YMAX,YMIN, ZMAX,ZMIN :
C        --------------------------------------------------------
         NBNOR = 0
         DO 110 J = 1, NBNB
            INO = ZI(JNOBE+J-1)
            X =  ZR(IDCOOR-1+3*(INO-1)+1)
            Y =  ZR(IDCOOR-1+3*(INO-1)+2)
            Z =  ZR(IDCOOR-1+3*(INO-1)+3)
            IF ( (X.LE.XMAX .AND. X.GE.XMIN) .AND.
     &           (Y.LE.YMAX .AND. Y.GE.YMIN) .AND.
     &           (Z.LE.ZMAX .AND. Z.GE.ZMIN) ) THEN
               NBNOR = NBNOR + 1
               ZI(JNORD+NBNOR-1) = INO
            ENDIF
 110     CONTINUE
C
C ------ PARCOURS DES NOEUDS DE LA BOITE A INTERSECTER :
C        ---------------------------------------------
         DO 120 J = 1, NBNOR
            INO = ZI(JNORD+J-1)
C
            NB(1) =  ZR(IDCOOR-1+3*(INO-1)+1)
            NB(2) =  ZR(IDCOOR-1+3*(INO-1)+2)
            NB(3) =  ZR(IDCOOR-1+3*(INO-1)+3)
C
            C1NB(1) = NB(1) - C1(1)
            C1NB(2) = NB(2) - C1(2)
            C1NB(3) = NB(3) - C1(3)
C
            C2NB(1) = NB(1) - C2(1)
            C2NB(2) = NB(2) - C2(2)
            C2NB(3) = NB(3) - C2(3)
C
            LC1NB = C1NB(1)*C1NB(1) + C1NB(2)*C1NB(2) + C1NB(3)*C1NB(3)
            LC2NB = C2NB(1)*C2NB(1) + C2NB(2)*C2NB(2) + C2NB(3)*C2NB(3)
C
            PSCA=DDOT(3,C1NB,1,C1C2,1)
            C1H(1) = PSCA * C1C2(1) / LC1C2
            C1H(2) = PSCA * C1C2(2) / LC1C2
            C1H(3) = PSCA * C1C2(3) / LC1C2
            XC1H = C1H(1)*C1H(1) + C1H(2)*C1H(2) + C1H(3)*C1H(3)
            PSCA=DDOT(3,C2NB,1,C1C2,1)
            C2H(1) = PSCA * C1C2(1) / LC1C2
            C2H(2) = PSCA * C1C2(2) / LC1C2
            C2H(3) = PSCA * C1C2(3) / LC1C2
            XC2H = C2H(1)*C2H(1) + C2H(2)*C2H(2) + C2H(3)*C2H(3)
C
            HNB(1) = C1NB(1) - C1H(1)
            HNB(2) = C1NB(2) - C1H(2)
            HNB(3) = C1NB(3) - C1H(3)
            R = HNB(1)*HNB(1) + HNB(2)*HNB(2) + HNB(3)*HNB(3)
C
C ---       SI LE NOEUD COURANT APPARTIENT AU TUNNEL, ON L'AFFECTE
C ---       A LA LISTE DE NOEUDS QUI SERA AFFECTEE AU GROUP_NO :
C           --------------------------------------------------
            IF ( R .LE. RFUT2 ) THEN
               IF ( XC1H.LE.LC1C2 .AND. XC2H.LE.LC1C2 ) THEN
                  ZI(JNOTR+INO-1) = 1
               ENDIF
C
C ---          ON TRAITE LES EXTREMITES COMME DES ROTULES :
C              ------------------------------------------
               IF ( (LCUMUL+SQRT(XC2H)).LE.LFUT ) THEN
                  IF (LC2NB.LE.RFUT2)  ZI(JNOTR+INO-1) = 1
               ENDIF
               IF ( (LCUMUL-L12+SQRT(XC1H)).LE.LFUT ) THEN
                  IF (LC1NB.LE.RFUT2)  ZI(JNOTR+INO-1) = 1
               ENDIF
            ENDIF
C
 120     CONTINUE
C
 100  CONTINUE
C
 9999 CONTINUE
C
C --- ON COMPTE LES NOEUDS ET ON LES AFFECTE A LISNOE
C
      CALL WKVECT ( LISNOE, 'V V I', NBNOT, IDLINO )
C
      NBNO = 0
      DO 200 I = 1, NBNOT
         IF ( ZI(JNOTR+I-1) .EQ. 1 ) THEN
            NBNO = NBNO + 1
            ZI(IDLINO+NBNO-1) = I
         ENDIF
 200  CONTINUE
C
      CALL JEDETR ( MESMAI )
      CALL JEDETR ( '&&CGNOFU.TRAVAIL'     )
      CALL JEDETR ( '&&CGNOFU.NOEUDS_CUBE' )
      CALL JEDETR ( '&&CGNOFU.NOEUDS_TROUVES' )
      CALL JEDETR ( '&&CGNOFU.NOEUD_BETON' )
      CALL JEDETR ( PREFIX//'.FOND      .NOEU' )
      CALL JEDETR ( PREFIX//'.FOND      .TYPE' )
C
      CALL JEDEMA()
C
      END
