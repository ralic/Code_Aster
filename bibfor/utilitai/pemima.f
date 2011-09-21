      SUBROUTINE PEMIMA(INDCH, CHAMGD, RESU, MODELE, NBOCC)
      IMPLICIT   NONE
      INTEGER           NBOCC,INDCH
      CHARACTER*8       MODELE
      CHARACTER*19      RESU
      CHARACTER*24      CHAMGD
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     OPERATEUR   POST_ELEM
C     TRAITEMENT DU MOT CLE-FACTEUR "MINMAX"
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
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
      CHARACTER*32     JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER IRET,NBCMP,NZERO,IBID,NBORDR,IOCC,JNUMA,NBMA
      INTEGER JCMP,N1,NR,NP,NC,NI,NO,JNO,JIN,NUMO
      INTEGER NBGMA,JGMA,NMA,IGM,NBPAR,NN,INUM,NLI,NLO
      PARAMETER(NZERO=0,NBPAR=3)
      REAL*8 RBID,PREC,INST
      COMPLEX*16 CBID
      CHARACTER*8 K8B,KBID,MAILLA, RESUCO,CRIT,TYCH
      CHARACTER*8 NOMGD,TOUT,GRPMA,TYPPAR(NBPAR)
      PARAMETER(TOUT='TOUT',GRPMA='GROUP_MA')
      CHARACTER*16 NOMPAR(NBPAR)
      CHARACTER*19 KNUM,CHAM,KINS,LISINS
      CHARACTER*24 NOMCHA
      LOGICAL      EXIORD
      INTEGER      IARG
      DATA NOMPAR/'CHAMP_GD','NUME_ORDRE','INST'/
      DATA TYPPAR/'K16','I','R'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
C     --- CREATION DE LA TABLE
      CALL TBCRSD ( RESU, 'G' )
      CALL TBAJPA ( RESU,NBPAR,NOMPAR,TYPPAR )
C
C     --- RECUPERATION DU MAILLAGE ET DU NOMBRE DE MAILLES
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,MAILLA,IRET)
      CALL DISMOI('F','NB_MA_MAILLA',MAILLA,'MAILLAGE',NBMA,K8B,IRET)

      IF (INDCH.EQ.0) THEN

C        --- RECUPERATION DU RESULTAT ET DU NUMERO D'ORDRE
         CALL GETVID ( 'MINMAX', 'RESULTAT' , 1,IARG,1, RESUCO, NR )
         CALL GETVR8 ( 'MINMAX', 'PRECISION', 1,IARG,1, PREC  , NP )
         CALL GETVTX ( 'MINMAX', 'CRITERE'  , 1,IARG,1, CRIT  , NC )
         CALL GETVR8 ( 'MINMAX', 'INST'      ,1,IARG,0, RBID,   NI)
         CALL GETVIS ( 'MINMAX', 'NUME_ORDRE',1,IARG,0, IBID,   NO)
         CALL GETVID ( 'MINMAX', 'LIST_INST' ,1,IARG,0, KBID,   NLI)
         CALL GETVID ( 'MINMAX', 'LIST_ORDRE',1,IARG,0, KBID,   NLO)

         KNUM = '&&PEMIMA.NUME_ORDRE'
         KINS = '&&PEMIMA.INST'
         EXIORD=.FALSE.
         IF(NO.NE.0)THEN
            EXIORD=.TRUE.
            NBORDR=-NO
            CALL WKVECT(KNUM,'V V I',NBORDR,JNO)
            CALL GETVIS ( 'MINMAX', 'NUME_ORDRE',1,IARG,NBORDR,
     &                    ZI(JNO),IRET)
         ENDIF

         IF(NI.NE.0)THEN
            NBORDR=-NI
            CALL WKVECT(KINS,'V V R',NBORDR,JIN)
            CALL GETVR8 ( 'MINMAX', 'INST',1,IARG,NBORDR,ZR(JIN),IRET)
         ENDIF

         IF(NLI.NE.0)THEN
            CALL GETVID ( 'MINMAX', 'LIST_INST'  ,1,IARG,1,LISINS,IRET)
            CALL JEVEUO(LISINS // '.VALE', 'L', JIN)
            CALL JELIRA(LISINS // '.VALE', 'LONMAX', NBORDR, KBID)
         ENDIF

         IF(NLO.NE.0)THEN
            EXIORD=.TRUE.
            CALL GETVID ( 'MINMAX', 'LIST_ORDRE'  ,1,IARG,1,LISINS,IRET)
            CALL JEVEUO(LISINS // '.VALE', 'L', JNO)
            CALL JELIRA(LISINS // '.VALE', 'LONMAX', NBORDR, KBID)
         ENDIF

         NN=NLO+NLI+NO+NI
         IF(NN.EQ.0)THEN
           EXIORD=.TRUE.
           CALL RSUTNU ( RESUCO,' ',0,KNUM,NBORDR,PREC,CRIT,IRET)
           CALL JEVEUO ( KNUM, 'L', JNO )
         ENDIF

      ELSE
C        CAS DU CHAMGD
         NBORDR=1
         EXIORD=.TRUE.
      ENDIF


C     --- ON PARCOURT LES OCCURENCES DU MOT CLE 'MINMAX':
C     =====================================================

      DO 10 IOCC = 1 , NBOCC


C     --- BOUCLE SUR LES NUMEROS D'ORDRE:
C     ===================================

      DO 5 INUM=1,NBORDR


          IF (INDCH.EQ.0) THEN
C             --- NUME_ORDRE, INST ---
              IF(EXIORD)THEN
                NUMO=ZI(JNO+INUM-1)
                CALL RSADPA ( RESUCO,'L',1,'INST',NUMO,0,JIN,KBID)
                INST=ZR(JIN)
              ELSE
                INST=ZR(JIN+INUM-1)
                CALL RSORAC(RESUCO,'INST',0,ZR(JIN+INUM-1),KBID,
     &                       CBID,PREC,CRIT,NUMO,NBORDR,IRET)
              ENDIF

C             --- CHAMP DU POST-TRAITEMENT
              CALL GETVTX('MINMAX','NOM_CHAM',IOCC,IARG,1,NOMCHA,IRET)
              CALL RSEXCH(RESUCO,NOMCHA,NUMO,CHAM,IRET)
          ELSE
             CHAM=CHAMGD
             NOMCHA=CHAMGD
             NUMO=0
             INST=0.D0
          ENDIF

          CALL DISMOI('C','TYPE_CHAMP',CHAM,'CHAMP',IBID,TYCH,IRET)

          CALL DISMOI('C','NOM_GD',CHAM,'CHAMP',IBID,NOMGD,IRET)
          IF(NOMGD(6:6).EQ.'C')GOTO 10

C         --- COMPOSANTES DU POST-TRAITEMENT
          CALL GETVTX('MINMAX','NOM_CMP',IOCC,IARG,NZERO,K8B,NBCMP)
          NBCMP=-NBCMP
          CALL WKVECT('&&PEMIMA.CMP','V V K8',NBCMP,JCMP)
          CALL GETVTX('MINMAX','NOM_CMP',IOCC,IARG,NBCMP,ZK8(JCMP),IRET)
C
C         --- CALCUL ET STOCKAGE DES MINMAX : MOT-CLE 'TOUT'

          CALL GETVTX('MINMAX','TOUT',IOCC,IARG,NZERO,K8B,IRET)
          IF(IRET.NE.0)THEN
             IF (TYCH(1:2).EQ.'EL') THEN
                 CALL  PEMAXE(RESU,NOMCHA,TOUT,TOUT,MODELE,
     &                 CHAM,NBCMP,ZK8(JCMP),NUMO,INST,IOCC)
             ELSE
                 CALL  PEMAXN(RESU,NOMCHA,TOUT,TOUT,MODELE,
     &                 CHAM,NBCMP,ZK8(JCMP),NUMO,INST,IOCC)
             ENDIF
          ENDIF

C         --- CALCUL ET STOCKAGE DES MOYENNES : MOT-CLE 'GROUP_MA'
          CALL GETVTX('MINMAX','GROUP_MA',IOCC,IARG,NZERO,K8B,N1)
          IF(N1.NE.0)THEN
            NBGMA=-N1
            CALL WKVECT('&&PEMIMA_GMA','V V K8',NBGMA,JGMA)
            CALL GETVTX('MINMAX','GROUP_MA',IOCC,IARG,NBGMA,
     &                   ZK8(JGMA),N1)
            DO 20 IGM=1,NBGMA
              CALL JELIRA(JEXNOM(MAILLA//'.GROUPEMA',ZK8(JGMA+IGM-1)),
     &                    'LONMAX',NMA,K8B)
              CALL JEVEUO(JEXNOM(MAILLA//'.GROUPEMA',ZK8(JGMA+IGM-1)),
     &                    'L',JNUMA)
             IF (TYCH(1:2).EQ.'EL') THEN
                CALL  PEMAXE(RESU,NOMCHA,GRPMA,ZK8(JGMA+IGM-1),MODELE,
     &                       CHAM,NBCMP,ZK8(JCMP),NUMO,INST,IOCC)
             ELSE
                CALL  PEMAXN(RESU,NOMCHA,GRPMA,ZK8(JGMA+IGM-1),MODELE,
     &                       CHAM,NBCMP,ZK8(JCMP),NUMO,INST,IOCC)
             ENDIF
 20         CONTINUE
            CALL JEDETR('&&PEMIMA_GMA')
          ENDIF

          CALL JEDETR('&&PEMIMA.CMP')

 5        CONTINUE

 10       CONTINUE

          CALL JEDEMA()

          END
