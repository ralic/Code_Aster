      SUBROUTINE FONMAI ( RESU, NOMAIL, TYPFON, IOCC, NBNOFF)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER             IOCC, NBNOFF
      CHARACTER*6         TYPFON
      CHARACTER*8         RESU, NOMAIL
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C-----------------------------------------------------------------------
C FONCTION REALISEE:
C
C     VERIFICATION DES ENTITES LORSQUE LE FOND EST DECRIT PAR
C     DES MAILLES OU DE GROUPES DE MAILLES
C     RENSEIGNEES DANS DEFI_FOND_FISS
C     CONSTRUCTION DU FOND DE FISSURE A PARTIR CES DONNEES
C
C     ENTREES:
C        RESU   : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
C        NOMAIL : NOM DU MAILLAGE
C        TYPFON : TYPE DE FOND
C                 IL PEUT VALOIR OUVERT/FERME/INF/SUP
C        IOCC   : OCCURENCE COURANTE DE MOTFAC
C     SORTIES:
C        NBNOFF : NOMBRE DE NOEUDS EN FOND DE FISSURE
C
C-----------------------------------------------------------------------
C
      REAL*8             VECORI(3)
C
      INTEGER       JCOUR2, JCOUR5, JTYPM, IATYMA, IDNONO, IDLINO, JTYP
      INTEGER       I,      NBMA,   N1,    IM,     NIG
      INTEGER       NID,    NUMNO,  IRET,  TROUV,  NUMMA
      CHARACTER*8   K8B, NOMMA, TYPM, NDORIG, NDEXTR
      CHARACTER*8   NOEUD,VALK(2)
      CHARACTER*16  K16BID, NOMCMD,MOTFAC
      CHARACTER*16  MOTCLE(2), TYPMCL(2)
      CHARACTER*24  CONEC, TYPP, NOMMAI, NOMNOE,NOEORD
      CHARACTER*24  MESNOE,MAFOUR,NOGRP
      INTEGER      IARG
C DEB-------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL GETRES ( K8B, K16BID, NOMCMD )
C     ------------------------------------------------------------------
C     INITIALISATION DE VARIABLES
C     ------------------------------------------------------------------
      MOTFAC = 'FOND_FISS'
      TYPP   = NOMAIL//'.TYPMAIL        '
      NOMMAI = NOMAIL//'.NOMMAI         '
      NOMNOE = NOMAIL//'.NOMNOE         '
      CONEC  = NOMAIL//'.CONNEX         '
      CALL JEVEUO ( TYPP, 'L', IATYMA )
      CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(IATYMA)),TYPM)
C
C     ------------------------------------------------------------------
C     --- RECHERCHE DES NOEUDS SOMMET DES MAILLES RENSEIGNEES
C     ------------------------------------------------------------------
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      MAFOUR='&&FONMAI.MALIGNE'
      CALL CGNOOR(MAFOUR, NOMAIL, MOTFAC, IOCC, 2,
     &    MOTCLE, TYPMCL, TYPFON, NBMA, NDORIG, NDEXTR, TYPM,VECORI)
      CALL JEVEUO(MAFOUR,'L',JCOUR2)

C
C     ------------------------------------------------------------------
C     --- SI FERME : RECUPERATION DE MAILLE_ORIG POUR AVOIR
C     --- LE SENS DE PARCOURS DE LA COURBE FERMEE
C     ------------------------------------------------------------------
C
      IF (TYPFON.EQ.'FERME') THEN
C
        NUMMA = 0
        CALL GETVTX ( MOTFAC, 'MAILLE_ORIG', 1,IARG,0, NOMMA, N1 )
        IF ( N1 .NE. 0 ) THEN
          CALL GETVTX ( MOTFAC, 'MAILLE_ORIG', 1,IARG,1, NOMMA, N1 )
          CALL JENONU ( JEXNOM(NOMMAI,NOMMA), NUMMA )
        ELSE
          CALL GETVTX ( MOTFAC, 'GROUP_MA_ORIG', 1,IARG,0, NOGRP, N1 )
          IF ( N1 .NE. 0 ) THEN
            CALL GETVTX ( MOTFAC, 'GROUP_MA_ORIG', 1,IARG,1, NOGRP, N1)
            CALL UTNONO ( ' ', NOMAIL, 'MAILLE', NOGRP, NOMMA, IRET )
            IF ( IRET .EQ. 10 ) THEN
               CALL U2MESK('F','RUPTURE0_41',1,NOGRP)
            ELSEIF ( IRET .EQ. 1 ) THEN
               CALL U2MESK('F','RUPTURE0_45',1,NDORIG)
            ENDIF
          CALL JENONU ( JEXNOM(NOMMAI,NOMMA), NUMMA )
          ENDIF
        ENDIF
C
        IF ( NUMMA .EQ. 0 ) THEN
          CALL U2MESS('F','RUPTURE0_42')
        ELSE
          CALL JENONU(JEXNOM(NOMNOE,NDORIG),NUMNO)
          CALL I2EXTF (NUMMA,1,CONEC(1:15),TYPP(1:16),NIG,NID)
          IF ((NUMNO.NE.NIG).AND.(NUMNO.NE.NID)) THEN
            CALL U2MESS('F','RUPTURE0_43')
          ENDIF
          TROUV = 0
          DO 545 IM = 1 , NBMA
            IF(NUMMA.EQ.ZI(JCOUR2-1 + IM)) TROUV = IM
 545      CONTINUE
          IF (TROUV.EQ.0) THEN
            CALL U2MESK('F','RUPTURE0_44',1,NOMMA)
          ELSE
C
C     ON REMONTE LA MAILLE_ORIG EN TETE DE LISTE
C
            CALL WKVECT('&&FONMAI.MAILLESTRIEES','V V I',3*NBMA,
     &                    JCOUR5)
            DO 546 IM = TROUV , NBMA
              ZI(JCOUR5-1 + IM+1-TROUV) = ZI(JCOUR2-1 + IM)
 546        CONTINUE
            DO 547 IM = 1 , TROUV-1
              ZI(JCOUR5-1 + IM+1+NBMA-TROUV) = ZI(JCOUR2-1 + IM)
 547        CONTINUE
            DO 548 IM = 1 , NBMA
              ZI(JCOUR2-1 + IM)=ZI(JCOUR5-1 + IM)
 548        CONTINUE
            CALL JEDETR ( '&&FONMAI.MAILLESTRIEES'  )
          ENDIF
        ENDIF
      ENDIF

C     ------------------------------------------------------------------
C     --- ORDONNANCEMENT DES NOEUDS EN FOND DE FISSURE
C     ------------------------------------------------------------------
      MESNOE = '&&FONMAI.NOEUD'
      CALL ORNOFD (MAFOUR, NOMAIL, NBMA, MESNOE, NDORIG, NDEXTR,
     &            'V',VECORI)
      IF(TYPFON.EQ.'INF') THEN
         NOEORD = RESU//'.FOND_INF.NOEU'
      ELSEIF(TYPFON.EQ.'SUP') THEN
         NOEORD = RESU//'.FOND_SUP.NOEU'
      ELSE
         NOEORD = RESU//'.FOND.NOEU'
      ENDIF
      CALL JELIRA(MESNOE,'LONMAX',NBNOFF,K8B)
      CALL JEVEUO(MESNOE,'L',IDNONO)

      CALL WKVECT(NOEORD,'G V K8',NBNOFF,IDLINO)
      DO 90 I = 1,NBNOFF
        CALL JENUNO(JEXNUM(NOMAIL//'.NOMNOE',ZI(IDNONO-1 + I)),NOEUD)
        ZK8(IDLINO-1 + I) = NOEUD
 90   CONTINUE


C
C     ------------------------------------------------------------------
C     --- ON STOCKE LE TYPE DE MAILLES DEFINISSANT LE FOND DE FISSURE
C     ------------------------------------------------------------------
C
      CALL JEEXIN ( RESU//'.FOND.TYPE', IRET )
      IF (IRET.EQ.0) THEN
        CALL WKVECT(RESU//'.FOND.TYPE','G V K8',1,JTYPM)
        ZK8(JTYPM) = TYPM
      ELSE
        CALL JEVEUO( RESU//'.FOND.TYPE','L',JTYP)
        IF (TYPM.EQ.ZK8(JTYP)) THEN
          VALK(1) = TYPM
          VALK(2) = ZK8(JTYP)
          CALL U2MESK('F','RUPTURE0_68',2,VALK)
        ENDIF
      ENDIF

C     ------------------------------------------------------------------
      CALL JEDETR (MESNOE)
      CALL JEDETR (MAFOUR)
C
      CALL JEDEMA()
      END
