      SUBROUTINE SSGNGM(NOMA,IOCC,NBGNAJ)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8 NOMA
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C-----------------------------------------------------------------------
C     BUT: AJOUTER S'IL LE FAUT A UN MAILLAGE DES GROUP_NO DE MEME NOM
C          QUE LES GROUP_MA ET CONTENANT LES NOEUDS DE CES MAILLES.

C     IN: NOMA  : NOM DU MAILLAGE.
C         IOCC  : NUMERO D'OCCURENCE DU MOT CLEF CREA_GROUP_NO
C-----------------------------------------------------------------------

      CHARACTER*8 K8B,NOMGNO,NOMGMA,KOUI
      CHARACTER*16 SELEC
      CHARACTER*24 GRPMA,GRPNO
      INTEGER      IARG

C DEB-------------------------------------------------------------------

C-----------------------------------------------------------------------
      INTEGER I ,IAD2 ,IALGMA ,IALIMA ,IALINO ,IANBNO ,IANGNO 
      INTEGER IBID ,IER ,IERD ,IOCC ,IRET ,J ,JTRAV 
      INTEGER N1 ,NB ,NBGMA ,NBGNAJ ,NBGNO ,NBMA ,NBNOTO 
      INTEGER NO 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      GRPMA = NOMA//'.GROUPEMA       '
      GRPNO = NOMA//'.GROUPENO       '
      NBGNAJ = 0
      SELEC  = 'TOUS'
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOTO,K8B,IERD)
      IF (NBNOTO.EQ.0) GO TO 60

C     ---  CAS : "TOUT_GROUP_MA"
C     --------------------------
      CALL GETVTX('CREA_GROUP_NO','TOUT_GROUP_MA',IOCC,IARG,1,KOUI,N1)
      IF (N1.EQ.1) THEN
        CALL JELIRA(NOMA//'.GROUPEMA','NMAXOC',NBGMA,K8B)
        CALL WKVECT('&&SSGNGM.LISTE_GMA','V V K8',NBGMA,IALGMA)
        DO 10 I = 1,NBGMA
          CALL JENUNO(JEXNUM(GRPMA,I),ZK8(IALGMA-1+I))
   10   CONTINUE
        IANGNO = IALGMA

C     ---  CAS : "GROUP_MA"
C     ---------------------
      ELSE

C      CRITERE DE SELECTION
        CALL GETVTX('CREA_GROUP_NO','CRIT_NOEUD',IOCC,IARG,1,SELEC,IBID)
        CALL GETVEM(NOMA,'GROUP_MA','CREA_GROUP_NO','GROUP_MA',IOCC,
     &              IARG,0,
     &              K8B,NB)
        CALL GETVTX('CREA_GROUP_NO','NOM',IOCC,IARG,0,K8B,NO)
        NBGMA = -NB
        CALL WKVECT('&&SSGNGM.LISTE_GMA','V V K8',NBGMA,IALGMA)
        CALL GETVEM(NOMA,'GROUP_MA','CREA_GROUP_NO','GROUP_MA',IOCC,
     &              IARG,
     &              NBGMA,ZK8(IALGMA),NB)
        IF (NO.NE.0) THEN
          NBGNO = -NO
          IF (NBGNO.NE.NBGMA) CALL U2MESS('F','MODELISA7_8')
          CALL WKVECT('&&SSGNGM.NOM_GNO','V V K8',NBGNO,IANGNO)
          CALL GETVTX('CREA_GROUP_NO','NOM',IOCC,IARG,NBGNO,
     &                ZK8(IANGNO),NO)
        ELSE
          IANGNO = IALGMA
        END IF
        IER = 0
        DO 20 I = 1,NBGMA
          NOMGMA = ZK8(IALGMA-1+I)
          CALL JEEXIN(JEXNOM(GRPMA,NOMGMA),IRET)
          IF (IRET.EQ.0) THEN
            IER = IER + 1
            CALL U2MESK('E','ELEMENTS_62',1,NOMGMA)
          END IF
   20   CONTINUE
        CALL ASSERT(IER.EQ.0)
      END IF
      IF (NBGMA.EQ.0) GO TO 60

      CALL WKVECT('&&SSGNGM.LISTE_NO ','V V I',NBNOTO,IALINO)
      CALL WKVECT('&&SSGNGM.TRAV ','V V I',NBNOTO,JTRAV)
      CALL WKVECT('&&SSGNGM.NB_NO    ','V V I',NBGMA,IANBNO)

C ---------------------------------------------------------------------
C     -- ON AJOUTE LES NOUVEAUX GROUPES:

      DO 30 I = 1,NBGMA
        NOMGMA = ZK8(IALGMA-1+I)
        CALL JELIRA(JEXNOM(GRPMA,NOMGMA),'LONUTI',NBMA,K8B)
        CALL JEVEUO(JEXNOM(GRPMA,NOMGMA),'L',IALIMA)
        CALL GMGNRE(NOMA,NBNOTO,ZI(JTRAV),ZI(IALIMA),NBMA,ZI(IALINO),
     &              ZI(IANBNO-1+I),SELEC)

        NOMGNO = ZK8(IANGNO-1+I)
        N1 = ZI(IANBNO-1+I)
        CALL JEEXIN(JEXNOM(GRPNO,NOMGNO),IRET)
        IF (IRET.GT.0) THEN
          CALL U2MESK('A','MODELISA7_9',1,NOMGNO)
        ELSE
          CALL JECROC(JEXNOM(GRPNO,NOMGNO))
          CALL JEECRA(JEXNOM(GRPNO,NOMGNO),'LONMAX',MAX(N1,1),K8B)
          CALL JEECRA(JEXNOM(GRPNO,NOMGNO),'LONUTI',N1,K8B)
          CALL JEVEUO(JEXNOM(GRPNO,NOMGNO),'E',IAD2)
          DO 40 J = 1,N1
            ZI(IAD2-1+J) = ZI(IALINO-1+J)
   40     CONTINUE
          NBGNAJ = NBGNAJ + 1
        END IF
   30 CONTINUE

   60 CONTINUE
      CALL JEDETR('&&SSGNGM.LISTE_GMA')
      CALL JEDETR('&&SSGNGM.NOM_GNO')
      CALL JEDETR('&&SSGNGM.LISTE_NO')
      CALL JEDETR('&&SSGNGM.TRAV')
      CALL JEDETR('&&SSGNGM.NB_NO')
      CALL JEDEMA()
      END
