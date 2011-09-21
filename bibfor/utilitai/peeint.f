      SUBROUTINE PEEINT(RESU, MODELE, NBOCC)
      IMPLICIT   NONE
      INTEGER           NBOCC
      CHARACTER*8       MODELE
      CHARACTER*19      RESU
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
C
C     OPERATEUR   POST_ELEM
C     TRAITEMENT DU MOT CLE-FACTEUR "INTEGRALE"
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
      INTEGER IRET,NBCMP,NZERO,IBID,NBORDR,IOCC,JNUMA,NBMA,NCMPM
      INTEGER JCMP,N1,NUMA,NR,NP,NC,IM,NI,NO,JNO,JIN,NUMO,I
      INTEGER NBGMA,JGMA,NMA,JMA,IGM,NBPA1,NBPA2,NN,INUM,NLI,NLO
      INTEGER ND,IB,JLICMP,JLICM2,JLICM1,NUCMP,INDIK8,JCPINI
      PARAMETER(NZERO=0,NBPA1=4,NBPA2=2)
      REAL*8 RBID,PREC,INST
      COMPLEX*16 CBID
      CHARACTER*8 K8B,KBID,MAILLA, RESUCO,CRIT,NOPAR2,NOPAR,TMPRES
      CHARACTER*4 TYCH,KI,EXIRDM
      CHARACTER*8 NOMGD,TOUT,GRPMA,MAILLE,TYPPA1(NBPA1),TYPPA2(NBPA2)
      PARAMETER(TOUT='TOUT',GRPMA='GROUP_MA',MAILLE='MAILLE')
      CHARACTER*16 NOMPA1(NBPA1),NOMPA2(NBPA2),OPTIO2
      CHARACTER*19 KNUM,CHAM,KINS,LISINS,CHAMG,CELMOD,LIGREL,TMPCHA
      CHARACTER*19 CHAM2,CHAMTM, LIGTMP
      CHARACTER*24 NOMCHA
      LOGICAL      EXIORD,TONEUT
      INTEGER      IARG
      DATA NOMPA1/'NOM_CHAM','NUME_ORDRE','INST','VOL'/
      DATA TYPPA1/'K16','I','R','R'/
      DATA NOMPA2/'CHAM_GD','VOL'/
      DATA TYPPA2/'K16','R'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
C     --- RECUPERATION DU MAILLAGE ET DU NOMBRE DE MAILLES
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,MAILLA,IRET)
      CALL DISMOI('F','NB_MA_MAILLA',MAILLA,'MAILLAGE',NBMA,K8B,IRET)


C     --- RECUPERATION DU RESULTAT ET DU NUMERO D'ORDRE
      CALL GETVID ( ' ', 'RESULTAT' , 1,IARG,1, RESUCO, NR )
      CALL GETVR8 ( ' ', 'PRECISION', 1,IARG,1, PREC  , NP )
      CALL GETVTX ( ' ', 'CRITERE'  , 1,IARG,1, CRIT  , NC )
      CALL GETVR8 ( ' ', 'INST'      ,1,IARG,0, RBID,   NI)
      CALL GETVIS ( ' ', 'NUME_ORDRE',1,IARG,0, IBID,   NO)
      CALL GETVID ( ' ', 'LIST_INST' ,1,IARG,0, KBID,   NLI)
      CALL GETVID ( ' ', 'LIST_ORDRE',1,IARG,0, KBID,   NLO)
      CALL GETVID ( ' ', 'CHAM_GD'   ,1,IARG,1, CHAMG,  ND)

C     --- CREATION DE LA TABLE
      CALL TBCRSD ( RESU, 'G' )
      IF(NR.NE.0)THEN
         CALL TBAJPA (RESU,NBPA1,NOMPA1,TYPPA1 )
      ELSE
         CALL TBAJPA (RESU,NBPA2,NOMPA2,TYPPA2 )
      ENDIF

      KNUM = '&&PEEINT.NUME_ORDRE'
      KINS = '&&PEEINT.INST'
      EXIORD=.FALSE.
      TONEUT=.FALSE.

      IF (ND.NE.0) THEN
        NBORDR = 1
        CALL WKVECT(KNUM,'V V I',NBORDR,JNO)
        ZI(JNO) = 1
        EXIORD=.TRUE.
      ELSE
        IF(NO.NE.0)THEN
          EXIORD=.TRUE.
          NBORDR=-NO
          CALL WKVECT(KNUM,'V V I',NBORDR,JNO)
          CALL GETVIS ( ' ', 'NUME_ORDRE',1,IARG,NBORDR,ZI(JNO),IRET)
        ENDIF

        IF(NI.NE.0)THEN
         NBORDR=-NI
         CALL WKVECT(KINS,'V V R',NBORDR,JIN)
         CALL GETVR8 ( ' ', 'INST',1,IARG,NBORDR,ZR(JIN),IRET)
        ENDIF

        IF(NLI.NE.0)THEN
         CALL GETVID ( ' ', 'LIST_INST'  ,1,IARG,1,LISINS,IRET)
         CALL JEVEUO(LISINS // '.VALE', 'L', JIN)
         CALL JELIRA(LISINS // '.VALE', 'LONMAX', NBORDR, KBID)
        ENDIF

        IF(NLO.NE.0)THEN
         EXIORD=.TRUE.
         CALL GETVID ( ' ', 'LIST_ORDRE'  ,1,IARG,1,LISINS,IRET)
         CALL JEVEUO(LISINS // '.VALE', 'L', JNO)
         CALL JELIRA(LISINS // '.VALE', 'LONMAX', NBORDR, KBID)
        ENDIF

        NN=NLO+NLI+NO+NI
        IF(NN.EQ.0)THEN
          EXIORD=.TRUE.
          CALL RSUTNU ( RESUCO,' ',0,KNUM,NBORDR,PREC,CRIT,IRET)
          CALL JEVEUO ( KNUM, 'L', JNO )
        ENDIF
      ENDIF
      CHAM='&&CHPCHD.CHAM'


C     --- ON PARCOURT LES OCCURENCES DU MOT CLE 'INTEGRALE':
C     =====================================================
      IF (NR.NE.0) THEN
          TMPRES='TMP_RESU'
          CALL COPISD('RESULTAT','V',RESUCO,TMPRES)
      ELSE
          TMPCHA='TMP_CHAMP_GD'
          CALL COPISD('CHAMP','V',CHAMG,TMPCHA)
      ENDIF

      DO 10 IOCC = 1 , NBOCC
      
C     --- VERIFICATION SI ON VA TRAITER DES ELEMENTS DE STRUCTURE
C     ===========================================================

        LIGTMP='&&PEEINT.LIGTMP'
        CALL EXLIMA('INTEGRALE', IOCC, 'V', MODELE, LIGTMP)
        CALL DISMLG('EXI_RDM',LIGTMP,IBID,EXIRDM,IRET)
        IF (EXIRDM.EQ.'OUI') THEN
           CALL U2MESS('F','UTILITAI8_60')
        ENDIF


C     --- BOUCLE SUR LES NUMEROS D'ORDRE:
C     ===================================

        DO 5 INUM=1,NBORDR

C         --- SI RESULTAT ---
C         --- NUME_ORDRE, INST ---
          IF (NR.NE.0) THEN
            IF(EXIORD) THEN
              NUMO=ZI(JNO+INUM-1)
              CALL RSADPA ( RESUCO,'L',1,'INST',NUMO,0,JIN,KBID)
              INST=ZR(JIN)
            ELSE
              INST=ZR(JIN+INUM-1)
              CALL RSORAC(RESUCO,'INST',0,ZR(JIN+INUM-1),KBID,
     &                     CBID,PREC,CRIT,NUMO,NBORDR,IRET)
            ENDIF

C         --- CHAMP DU POST-TRAITEMENT
            CALL GETVTX('INTEGRALE','NOM_CHAM',IOCC,IARG,1,NOMCHA,IRET)
            IF (IRET.EQ.0) CALL U2MESS('F','POSTELEM_4')
            CHAM2='&&PEEINT.CHAM_2'
            CALL RSEXCH(TMPRES,NOMCHA,NUMO,CHAM2,IRET)

          ELSE
C         --- SI CHAM_GD ---
            NUMO = NBORDR
            NOMCHA = CHAMG
            CHAM2 = TMPCHA

          ENDIF

          CALL DISMOI('C','TYPE_CHAMP',CHAM2,'CHAMP',IBID,TYCH,IRET)
          CALL DISMOI('C','NOM_GD',CHAM2,'CHAMP',IBID,NOMGD,IRET)

          IF(NOMGD(6:6).EQ.'C')GOTO 10

          IF (TYCH(1:2).NE.'EL') THEN

C          --- 1. TRANSFORMATION DU CHAMP EN CHAMP NEUTRE:
C              - CHANGEMENT DE LA GRANDEUR EN NEUT_R
C              - CHAMGEMENT DES COMPOSANTES EN X1,X2,X3,...
               TONEUT=.TRUE.
               CHAMTM='&&PEEINT.CHS1'
               CALL CNOCNS(CHAM2,'V',CHAMTM)
               CALL JEVEUO(CHAMTM//'.CNSC','L',JLICMP)
               CALL JELIRA(CHAMTM//'.CNSC','LONMAX',NCMPM,K8B)
               CALL JEDETR('&&PEEINT.CMP1')
               CALL WKVECT('&&PEEINT.CMP1','V V K8',NCMPM,JLICM1)
               CALL JEDETR('&&PEEINT.CMP2')
               CALL WKVECT('&&PEEINT.CMP2','V V K8',NCMPM,JLICM2)
               DO 15 I=1,NCMPM
                   CALL CODENT(I,'G',KI)
                   ZK8(JLICM2+I-1)='X'//KI(1:LEN(KI))
                   ZK8(JLICM1+I-1)=ZK8(JLICMP+I-1)
 15            CONTINUE
               CALL CHSUT1(CHAMTM,'NEUT_R',NCMPM,ZK8(JLICM1),
     &                     ZK8(JLICM2),'V',CHAMTM)
               CALL CNSCNO(CHAMTM,' ','NON','V',CHAM2,'F',IBID)
               CALL DETRSD('CHAM_NO_S',CHAMTM)

C           --- 2. CHANGEMENT DE DISCRETISATION : NOEU -> ELGA
               OPTIO2 ='TOU_INI_ELGA'
               CALL DISMOI('C','NOM_GD',CHAM2,'CHAMP',IBID,NOMGD,IRET)
               NOPAR = NOPAR2(OPTIO2,NOMGD,'OUT')
               CELMOD = '&&PEEINT.CELMOD'
               LIGREL = MODELE//'.MODELE'
               CALL ALCHML(LIGREL,OPTIO2,NOPAR,'V',CELMOD,IB,' ')
               IF (IB.NE.0) CALL U2MESK('F','UTILITAI3_23',1,OPTIO2)
               CALL CHPCHD(CHAM2,'ELGA',CELMOD,'OUI','V',CHAM)
               CALL DETRSD('CHAMP',CELMOD)
C
          ELSE
               CHAM=CHAM2
          ENDIF

         CALL DISMOI('C','TYPE_CHAMP',CHAM,'CHAMP',IBID,TYCH,IRET)
 
C         --- COMPOSANTES DU POST-TRAITEMENT
          CALL GETVTX('INTEGRALE','NOM_CMP',IOCC,IARG,NZERO,K8B,NBCMP)
          NBCMP=-NBCMP
          CALL WKVECT('&&PEEINT.CMP','V V K8',NBCMP,JCMP)
          CALL GETVTX('INTEGRALE','NOM_CMP',IOCC,IARG,NBCMP,
     &                ZK8(JCMP),IRET)
C 
C         COMPOSANTES A AFFICHER DANS LA TABLE: ZK8(JCPINI)
          CALL WKVECT('&&PEEINT.CMP_INIT','V V K8',NBCMP,JCPINI)
          DO 50 I=1,NBCMP
             ZK8(JCPINI+I-1)=ZK8(JCMP+I-1)
 50       CONTINUE

          IF(TONEUT)THEN
             DO 55 I=1,NBCMP
                NUCMP=INDIK8(ZK8(JLICM1),ZK8(JCPINI+I-1),1,NCMPM)
                ZK8(JCMP+I-1)=ZK8(JLICM2+NUCMP-1)
 55          CONTINUE
          ENDIF
C
C         --- CALCUL ET STOCKAGE DES MOYENNE : MOT-CLE 'TOUT'
          CALL GETVTX('INTEGRALE','TOUT',IOCC,IARG,NZERO,K8B,IRET)
          IF(IRET.NE.0)THEN
            CALL  PEECAL(TYCH,RESU,NOMCHA,TOUT,TOUT,MODELE,NR,
     &                 CHAM,NBCMP,ZK8(JCMP),ZK8(JCPINI),NUMO,INST,IOCC)
          ENDIF

C         --- CALCUL ET STOCKAGE DES MOYENNES : MOT-CLE 'GROUP_MA'
          CALL GETVTX('INTEGRALE','GROUP_MA',IOCC,IARG,NZERO,K8B,N1)
          IF(N1.NE.0)THEN
            NBGMA=-N1
            CALL WKVECT('&&PEEINT_GMA','V V K8',NBGMA,JGMA)
            CALL GETVTX('INTEGRALE','GROUP_MA',IOCC,IARG,NBGMA,
     &                   ZK8(JGMA),N1)
            DO 20 IGM=1,NBGMA
              CALL JELIRA(JEXNOM(MAILLA//'.GROUPEMA',ZK8(JGMA+IGM-1)),
     &                    'LONMAX',NMA,K8B)
              CALL JEVEUO(JEXNOM(MAILLA//'.GROUPEMA',ZK8(JGMA+IGM-1)),
     &                    'L',JNUMA)
              CALL  PEECAL(TYCH,RESU,NOMCHA,GRPMA,ZK8(JGMA+IGM-1),
     &                  MODELE,NR,CHAM,NBCMP,ZK8(JCMP),ZK8(JCPINI),
     &                  NUMO,INST,IOCC)
 20         CONTINUE
            CALL JEDETR('&&PEEINT_GMA')
          ENDIF

C         --- CALCUL ET STOCKAGE DES MOYENNES : MOT-CLE 'MAILLE'
          CALL GETVTX('INTEGRALE','MAILLE',IOCC,IARG,NZERO,K8B,N1)
          IF(N1.NE.0)THEN
            NMA=-N1
            CALL WKVECT('&&PEEINT_MAIL','V V K8',NMA,JMA)
            CALL GETVTX('INTEGRALE','MAILLE',IOCC,IARG,NMA,
     &                   ZK8(JMA),N1)
            DO 30 IM=1,NMA
              CALL JENONU(JEXNOM(MAILLA//'.NOMMAI',ZK8(JMA+IM-1)),NUMA)
              CALL  PEECAL(TYCH,RESU,NOMCHA,MAILLE,ZK8(JMA+IM-1),MODELE,
     &                    NR,CHAM,NBCMP,ZK8(JCMP),ZK8(JCPINI),NUMO,
     &                    INST,IOCC)
 30         CONTINUE
            CALL JEDETR('&&PEEINT_MAIL')
          ENDIF

          IF (NR.NE.0) THEN
            CALL DETRSD('CHAMP',CHAM2)
          ENDIF
          CALL JEDETR('&&PEEINT.CMP')
          CALL JEDETR('&&PEEINT.CMP_INIT')

 5    CONTINUE

 10   CONTINUE
          
      IF (NR.NE.0) THEN
          CALL DETRSD('RESULTAT',TMPRES)
      ELSE
          CALL DETRSD('CHAMP',TMPCHA)
      ENDIF

      CALL JEDEMA()

      END
