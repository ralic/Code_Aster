      SUBROUTINE OP0055()
      IMPLICIT   NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 22/04/2013   AUTEUR CUVILLIE M.CUVILLIEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C      OPERATEUR :     DEFI_FOND_FISS
C
C-----------------------------------------------------------------------
C
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      INTEGER       IADR1,IFM,NIV
      INTEGER       NBOCC, NBNOFF
      INTEGER       IBAS, IBID, IOCC, IDON, IDONN, IFONOE, NDONN
      INTEGER       IRET1, IRET2, IRET, IRETS
      INTEGER       N1, N2
      CHARACTER*6   K6B, NOMPRO
      CHARACTER*8   K8B, RESU, NOMA, TYPFON, CONFIN
      CHARACTER*9   ENTIT(8)
      CHARACTER*13  MOTCL(8)
      CHARACTER*16  TYPRES, OPER
      CHARACTER*19  BASFON, BASLOC, CNXINV, FONTYP, LNNO, LTNO
      CHARACTER*24  VALK(2),ENTNOM, FONDFI, FONOEU
      INTEGER      IARG
C DEB-------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMPRO = 'OP0055'

      CALL INFNIV(IFM,NIV)
C
C ---  RECUPERATION DES ARGUMENTS DE LA COMMANDE
C
      CALL GETRES (RESU,TYPRES,OPER)
C
C ---  RECUPERATIONS RELATIVES AU MAILLAGE
C      -----------------------------------
C
      CALL GETVID (' ', 'MAILLAGE', 0,IARG, 1, NOMA, NBOCC)
C
C ---  RECUPERATION DE LA CONNECTIVITE INVERSE
C
      CNXINV='&&'//NOMPRO//'.CNXINV'
      CALL CNCINV (NOMA, IBID, 0, 'V', CNXINV )
C
C
C     ---------------------------------------------------------------
C     RECUPERATION DU TYPE DE FOND
C     OUVERT OU FERME OU INF/SUP
C     ---------------------------------------------------------------
C
      CALL GETFAC ( 'FOND_FISS', NBOCC )
      DO 1 IOCC=1,NBOCC

        CALL GETVTX ( 'FOND_FISS', 'TYPE_FOND', IOCC,IARG,0, K6B, N1)
        IF (N1.NE.0) THEN
          CALL GETVTX ('FOND_FISS','TYPE_FOND',IOCC,IARG,1,
     &                 TYPFON, N1)
        ELSE
          TYPFON = 'OUVERT'
        ENDIF
C
C
C     ---------------------------------------------------------------
C     VERIFICATION DE L'EXISTANCE DES ENTITES DU MAILLAGE RENSEIGNEES
C     ET CONSTRUCTION DE VECTEURS DE TRAVAIL POUR CHACUNE D'ELLES
C     ---------------------------------------------------------------
C
        ENTIT(1)  = '.NOMNOE'
        ENTIT(2)  = '.NOMMAI'
        ENTIT(3)  = '.GROUPENO'
        ENTIT(4)  = '.GROUPEMA'
        ENTIT(5)  = '.NOMNOE'
        ENTIT(6)  = '.GROUPENO'
        MOTCL(1)  = 'NOEUD'
        MOTCL(2)  = 'MAILLE'
        MOTCL(3)  = 'GROUP_NO'
        MOTCL(4)  = 'GROUP_MA'
        MOTCL(5)  = 'NOEUD_ORIG'
        MOTCL(6)  = 'GROUP_NO_ORIG'
        IF (TYPFON.EQ.'OUVERT') THEN
          ENTIT(7)  = '.NOMNOE'
          ENTIT(8)  = '.GROUPENO'
          MOTCL(7)  = 'NOEUD_EXTR'
          MOTCL(8)  = 'GROUP_NO_EXTR'
          NDONN = 8
        ELSEIF (TYPFON.EQ.'FERME') THEN
          ENTIT(7)  = '.NOMMAI'
          ENTIT(8)  = '.GROUPEMA'
          MOTCL(7)  = 'MAILLE_ORIG'
          MOTCL(8)  = 'GROUP_MA_ORIG'
          NDONN = 8
        ELSE
          NDONN = 6
        ENDIF
        DO 11 IDONN=1,NDONN
          CALL GETVTX ( 'FOND_FISS', MOTCL(IDONN), IOCC,IARG,0, K8B, N1)
          N1 = -N1
          IF (N1.GT.0) THEN
            CALL WKVECT ('&&'//NOMPRO//'.'//MOTCL(IDONN),
     &                     'V V K24', N1, IADR1)
            CALL GETVTX('FOND_FISS', MOTCL(IDONN), IOCC,IARG,N1,
     &                     ZK24(IADR1),N2)
            DO 111 IDON=1,N1
              ENTNOM = ZK24(IADR1-1 + IDON)
              CALL JENONU(JEXNOM(NOMA//ENTIT(IDONN),ENTNOM),IBID)
              IF (IBID.EQ.0) THEN
                VALK(1) = ENTNOM
                VALK(2) = MOTCL(IDONN)
                CALL U2MESK('F','RUPTURE0_7',2,VALK)
              ENDIF
 111        CONTINUE
          ENDIF
 11     CONTINUE

C
C
C       ---------------------------------------------------------------
C       CONSTRUCTION DE FOND DE FISSURE
C       ---------------------------------------------------------------
C
C        SI LE MOT CLE FACTEUR EST NOEUD OU GROUP_NO
C        ----------------------------------------
C
          CALL JEEXIN ('&&'//NOMPRO//'.NOEUD',    IRET1 )
          CALL JEEXIN ('&&'//NOMPRO//'.GROUP_NO', IRET2 )
          IF ((IRET1.NE.0).OR.(IRET2.NE.0)) THEN
            CALL FONNOE ( RESU, NOMA, CNXINV, NOMPRO, TYPFON, NBNOFF)
          ENDIF
C
C        SI LE MOT CLE FACTEUR EST MAILLE OU GROUP_MA
C        ----------------------------------------
C
          CALL JEEXIN ('&&'//NOMPRO//'.MAILLE',   IRET1 )
          CALL JEEXIN ('&&'//NOMPRO//'.GROUP_MA', IRET2 )
          IF ((IRET1.NE.0).OR.(IRET2.NE.0)) THEN
            CALL FONMAI ( RESU, NOMA, TYPFON, IOCC, NBNOFF)
          ENDIF
CC
C
C       DESTRUCTION DES VECTEURS DE TRAVAIL
C       ----------------------------------------
        DO 20 IDONN=1,NDONN
          CALL JEEXIN ( '&&'//NOMPRO//'.'//MOTCL(IDONN), IRET )
          IF (IRET.NE.0) CALL JEDETR('&&'//NOMPRO//'.'//MOTCL(IDONN))
  20    CONTINUE

  1   CONTINUE
C
C
C     ---------------------------------------------------------------
C     VERIFICATION DES DONNEES SUR LES LEVRES ET LES VECTEURS
C     ---------------------------------------------------------------
C
C
C     TRAITEMENT DES LEVRES: LEVRE_SUP ET LEVRE_INF
C     ----------------------------------------
C
      CALL FONLEV(RESU,NOMA,NBNOFF)

C
C     TRAITEMENT DE LA NORMALE ET DES
C     MOTS CLES FACTEUR : DTAN_EXTR, DTAN_ORIG
C                         VECT_GRNO_ORIG, VECT_GRNO_EXTR
C     ----------------------------------------
C
      CALL FONVEC(RESU,NOMA,CNXINV)

      CALL JEDETR(CNXINV)

C     ---------------------------------------------------------------
C     CREATION DU VECTEUR .FONDFISS CONTENANT LES COORDONNEES ET LES
C     ABSCISSES CURVILIGNES DES NOEUDS DU FOND
C     ---------------------------------------------------------------

C     VECTEUR CONTENANT LES NOMS DES NOEUDS DU FOND DE FISSURE
C     ----------------------------------------
      CALL JEEXIN(RESU//'.FOND.NOEU',IRET)
      IF (IRET.NE.0) THEN
        FONOEU = RESU//'.FOND.NOEU'
        CALL JEVEUO(FONOEU,'L',IFONOE)
        IF (TYPFON.EQ.'FERME')THEN
           CALL ASSERT(ZK8(IFONOE+1-1).EQ.ZK8(IFONOE+NBNOFF-1))
        ENDIF
      ELSE
        FONOEU = RESU//'.FOND_SUP.NOEU'
      ENDIF

      FONDFI = RESU//'.FONDFISS'
      CALL FONFIS(NOMA,NBNOFF,FONOEU,FONDFI)

C     ---------------------------------------------------------------
C     CREATION DE LA BASE LOCALE ET DES LEVEL SETS EN CHAQUE NOEUD
C     ---------------------------------------------------------------

C     LA BASE LOCALE ET DES LEVEL SETS SONT CALCULEES EN CHAQUE NOEUD
C     QUE SI L'OBJET .BASEFOND EXISTE DEJA
      CALL JEEXIN(RESU//'.BASEFOND',IBAS)
      IF (IBAS.NE.0)THEN
        BASFON = RESU//'.BASEFOND'
        IF (NBNOFF.NE.1)THEN
          FONTYP = RESU//'.FOND.TYPE'
        ENDIF
        BASLOC = RESU//'.BASLOC'
        LNNO   = RESU//'.LNNO'
        LTNO   = RESU//'.LTNO'
        CALL FONBAS( NOMA, BASFON, FONTYP, FONDFI, NBNOFF, BASLOC,
     &               LNNO, LTNO)
      ENDIF
C
C
C     ---------------------------------------------------------------
C     EXTRACTION DES NOEUDS DES LEVRES SUR DIRECTON NORMALE
C     ---------------------------------------------------------------
C
      CALL GETVTX (' ', 'CONFIG_INIT',0,IARG,1,CONFIN,IBID)
      IF(CONFIN.EQ.'COLLEE') THEN
        CALL JEEXIN(RESU//'.LEVRESUP.MAIL',IRETS)
        IF(IRETS.NE.0) THEN
          CALL  FONNOF ( RESU,NOMA,TYPFON,NBNOFF )
        ENDIF
      ENDIF

C     ---------------------------------------------------------------
C     STOCKAGE D'INFOS UTILES DANS LA SD EN SORTIE
C     ---------------------------------------------------------------
C
      CALL FONINF(RESU,TYPFON)

C     ---------------------------------------------------------------
C     IMPRESSIONS SI INFO=2
C     ---------------------------------------------------------------
C
      IF (NIV.EQ.2) THEN
        CALL FONIMP(RESU)
      ENDIF

      CALL JEDEMA()
      END
