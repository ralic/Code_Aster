      SUBROUTINE CGNOXF (MOFAZ, IOCC, NOMAZ, LISNOZ, NBNO)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER IOCC,NBNO
      CHARACTER*(*) MOFAZ,NOMAZ,LISNOZ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
C       CGNOXF -- TRAITEMENT DE L'OPTION FISS_XFEM
C                 DU MOT FACTEUR CREA_GROUP_NO DE
C                 LA COMMANDE DEFI_GROUP
C
C      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_NO CONSTITUE
C      DE TOUS LES NOEUDS DE TYPE XFEM DEFINI PAR L'UTILISATEUR.
C
C -------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS
C                                   DU TYPE XFEM DEMANDE PAR
C                                   L'UTILISATEUR
C  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
C -------------------------------------------------------
C
C.========================= DEBUT DES DECLARATIONS ====================
C
C
C --------- VARIABLES LOCALES ---------------------------
      INTEGER        IBID,   IRET
      INTEGER        N1,     IFISS,  NFISS
      INTEGER        INO,    VALENO, NBNOT
      INTEGER        IDLIST, JNOEU,  JFISS,  JSTNO,JLST,JLSN
      CHARACTER*8    NOMA,   K8BID,  NOMNOE, FISS, NOMOFI, NOMAFI,
     &               NOMOGR, NOMAGR, VALK(2), MA
      CHARACTER*16   MOTFAC, TYPGRP
      CHARACTER*19   STNO,CNSLT,CNSLN
      CHARACTER*24   STNOT
      CHARACTER*24   LISNOE
      LOGICAL        GRILLE
      REAL*8         RAYON,DIST
      INTEGER      IARG
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ================
      MOTFAC    = MOFAZ
      NOMA      = NOMAZ
      LISNOE    = LISNOZ
      NBNO = 0

      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOT,K8BID,IRET)
      CALL WKVECT('&&CGNOXF.NOEU','V V I',  NBNOT,JNOEU)
C
C --  RECUPERATION DU TYPE GROUPE :
C     ============================
      CALL GETVTX(MOTFAC,'TYPE_GROUP',IOCC,IARG,1,TYPGRP,N1)
C
C --  RECUPERATION DES NOMS DES FISSURES :
C     ===================================
      CALL GETVID(MOTFAC,'FISSURE',IOCC,IARG,0,K8BID,NFISS)
      NFISS = -NFISS
      CALL WKVECT('&&CGNOXF.FISS', 'V V K8',NFISS,JFISS)
      CALL GETVID(MOTFAC,'FISSURE',IOCC,IARG,NFISS,ZK8(JFISS),IBID)
C
C --- TYPE DE NOEUD = 'HEAVISIDE'
C     ============================
      IF (TYPGRP.EQ.'HEAVISIDE') THEN
        DO 10 IFISS=1,NFISS
          FISS =   ZK8(JFISS-1+IFISS)
          STNO   = FISS//'.STNO'
          CALL JEVEUO(STNO//'.VALE','L',JSTNO)
          DO 110 INO=1,NBNOT
            VALENO = ZI(JSTNO+INO-1)
            IF (VALENO.EQ.1) THEN
                NBNO = NBNO + 1
                ZI(JNOEU+NBNO-1) = INO
            ENDIF
 110     CONTINUE
          CALL JEDETR(STNO)
 10     CONTINUE
C
C --- TYPE DE NOEUD = 'CRACKTIP'
C     ============================
      ELSEIF (TYPGRP.EQ.'CRACKTIP') THEN
        DO 11 IFISS=1,NFISS
          FISS =   ZK8(JFISS-1+IFISS)
          STNO   = FISS//'.STNO'
          CALL JEVEUO(STNO//'.VALE','L',JSTNO)
          DO 111 INO=1,NBNOT
            VALENO = ZI(JSTNO+INO-1)
            IF (VALENO.EQ.2) THEN
                NBNO = NBNO + 1
                ZI(JNOEU+NBNO-1) = INO
             ENDIF
 111      CONTINUE
          CALL JEDETR(STNO)
 11     CONTINUE
C
C --- TYPE DE NOEUD = 'MIXTE'
C     ============================
      ELSEIF (TYPGRP.EQ.'MIXTE') THEN
        DO 12 IFISS=1,NFISS
          FISS =   ZK8(JFISS-1+IFISS)
          STNO   = FISS//'.STNO'
          CALL JEVEUO(STNO//'.VALE','L',JSTNO)
          DO 112 INO=1,NBNOT
            VALENO = ZI(JSTNO+INO-1)
            IF (VALENO.EQ.3) THEN
                NBNO = NBNO + 1
                ZI(JNOEU+NBNO-1) = INO
            ENDIF
 112      CONTINUE
 12     CONTINUE
C
C --- TYPE DE NOEUD = 'XFEM'
C     ============================
      ELSEIF (TYPGRP.EQ.'XFEM') THEN
        DO 13 IFISS=1,NFISS
          FISS =   ZK8(JFISS-1+IFISS)
          STNO   = FISS//'.STNO'
          CALL JEVEUO(STNO//'.VALE','L',JSTNO)
          DO 113 INO=1,NBNOT
            VALENO = ZI(JSTNO+INO-1)
            IF (VALENO.NE.0) THEN
                NBNO = NBNO + 1
                ZI(JNOEU+NBNO-1) = INO
            ENDIF
 113     CONTINUE
          CALL JEDETR(STNO)
 13     CONTINUE

C
C --- TYPE DE NOEUD = 'TORE'
C     ============================
      ELSEIF ((TYPGRP.EQ.'TORE').OR.(TYPGRP.EQ.'ZONE_MAJ')) THEN

         CNSLT = '&&CGNOXF.CNSLT'
         CNSLN = '&&CGNOXF.CNSLN'

         DO 15 IFISS=1,NFISS
            FISS =   ZK8(JFISS-1+IFISS)

C           CHECK IF THE LOCALISATION HAS BEEN USED
            STNOT = FISS//'.PRO.RAYON_TORE'
            CALL JEEXIN(STNOT,IBID)
            IF ((IBID.GT.0).AND.(TYPGRP.EQ.'TORE')) THEN
               TYPGRP='ZONE_MAJ'
               CALL U2MESK('A','XFEM2_92',1,FISS)
            ENDIF

            IF (TYPGRP.EQ.'TORE') THEN

C              GET THE CRACK MESH
               CALL DISMOI('F','NOM_MODELE',FISS,'FISS_XFEM',IBID,
     &                     NOMOFI,IRET)
               STNOT = NOMOFI//'.MODELE    .LGRF'
               CALL JEVEUO(STNOT,'L',IBID)
               NOMAFI = ZK8(IBID)

               CALL GETVR8(MOTFAC,'RAYON_TORE',1,IARG,1,RAYON,IBID)
               RAYON = RAYON**2

C              RETREIVE THE TWO LEVEL SETS
               CALL GETVID(' ','MAILLAGE',1,IARG,1,MA,IBID)
               IF (IBID.EQ.0) THEN
                   CALL GETVID(' ','GRILLE',1,IARG,1,MA,IBID)
C                  CHECK FOR THE PRESENCE OF THE GRID
                   STNOT = FISS//'.GRI.MODELE'
                   CALL JEEXIN(STNOT,IBID)
                   IF (IBID.GT.0) THEN
                     CALL JEVEUO(STNOT,'L',IBID)
C                    GRID MODEL NAME
                     NOMOGR = ZK8(IBID)
C                    GRID NAME
                     STNOT = NOMOGR//'.MODELE    .LGRF'
                     CALL JEVEUO(STNOT,'L',IBID)
                     NOMAGR = ZK8(IBID)
                     IF (NOMAGR.NE.MA) CALL U2MESS('F','XFEM2_86')
                  ELSE
                     CALL U2MESS('F','XFEM2_86')  
                  ENDIF
                  CALL CNOCNS(FISS//'.GRI.LTNO','V',CNSLT)
                  CALL CNOCNS(FISS//'.GRI.LNNO','V',CNSLN)
               ELSE
                  IF (NOMAFI.NE.MA) CALL U2MESS('F','XFEM2_86')
                  CALL CNOCNS(FISS//'.LTNO','V',CNSLT)
                  CALL CNOCNS(FISS//'.LNNO','V',CNSLN)
               ENDIF
               CALL JEVEUO(CNSLT//'.CNSV','L',JLST)
               CALL JEVEUO(CNSLN//'.CNSV','L',JLSN)

               DO 116 INO=1,NBNOT
                  DIST=ZR(JLST-1+INO)**2+ZR(JLSN-1+INO)**2
                  IF (DIST.LE.RAYON) THEN
                     NBNO = NBNO + 1
                     ZI(JNOEU+NBNO-1) = INO
                  ENDIF
 116           CONTINUE

               CALL JEDETR(CNSLT)
               CALL JEDETR(CNSLN)

            ENDIF

C
C --- TYPE DE NOEUD = 'ZONE_MAJ'
C     ============================
            IF (TYPGRP.EQ.'ZONE_MAJ') THEN

C             GET THE CRACK MESH
              CALL DISMOI('F','NOM_MODELE',FISS,'FISS_XFEM',IBID,NOMOFI,
     &                    IRET)
              STNOT = NOMOFI//'.MODELE    .LGRF'
              CALL JEVEUO(STNOT,'L',IBID)
              NOMAFI = ZK8(IBID)

C             CHECK FOR THE PRESENCE OF THE GRID
              STNOT = FISS//'.GRI.MODELE'
              CALL JEEXIN(STNOT,IBID)
              IF (IBID.GT.0) THEN
                 GRILLE = .TRUE.
                 CALL JEVEUO(STNOT,'L',IBID)
C                GRID MODEL NAME
                 NOMOGR = ZK8(IBID)
C                GRID NAME
                 STNOT = NOMOGR//'.MODELE    .LGRF'
                 CALL JEVEUO(STNOT,'L',IBID)
                 NOMAGR = ZK8(IBID)
              ELSE
                 GRILLE = .FALSE.
              ENDIF

              IF (NOMA.EQ.NOMAFI) THEN
                IF (GRILLE) THEN
                   STNOT = FISS//'.PRO.NOEUD_PROJ'
                ELSE
                   STNOT = FISS//'.PRO.NOEUD_TORE'
                ENDIF
              ELSE IF (GRILLE.AND.(NOMA.EQ.NOMAGR)) THEN
                STNOT = FISS//'.PRO.NOEUD_TORE'
              ELSE
                VALK(1) = NOMAFI
                IF (GRILLE) THEN
                   VALK(2) = NOMAGR
                ELSE
                   VALK(2) = 'AUCUN'
                ENDIF
                CALL U2MESK('F','XFEM2_96',2,VALK)
              ENDIF

              CALL JEEXIN(STNOT,IBID)
              IF (IBID.GT.0) THEN
                 CALL JEVEUO(STNOT,'L',JSTNO)
                 DO 114 INO=1,NBNOT
                    IF (ZL(JSTNO+INO-1)) THEN
                       NBNO = NBNO + 1
                       ZI(JNOEU+NBNO-1) = INO
                    ENDIF
 114             CONTINUE
              ELSE
C                THE LOCALISATION HAS NOT BEEN USED. ZONE_MAJ IS
C                COINCIDENT WITH THE WHOLE MODEL.
                 DO 115 INO=1,NBNOT
                    NBNO = NBNO + 1
                    ZI(JNOEU+NBNO-1) = INO
 115             CONTINUE
              ENDIF
            ENDIF

15       CONTINUE
      
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF

      IF (NBNO.NE.0) THEN
        CALL WKVECT (LISNOE, 'V V I', NBNO, IDLIST )

        DO 20 INO=1,NBNO
          ZI(IDLIST+INO-1)=ZI(JNOEU+INO-1)
          CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',ZI(IDLIST+INO-1))
     &        ,NOMNOE)
 20     CONTINUE
      ENDIF
C
C --- FIN
C     ===
C
C --- MENAGE
C

      CALL JEDETR('&&CGNOXF.FISS')
      CALL JEDETR('&&CGNOXF.NOEU')
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
