      SUBROUTINE XFISNO(NOMA  ,MODELX)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
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
C RESPONSABLE MASSIN P.MASSIN
      IMPLICIT NONE
      CHARACTER*8   NOMA,MODELX

C----------------------------------------------------------------------
C  BUT: CREATION D'UN CHAMPS ELNO QUI ASSOCIE POUR CHAQUE NOEUD LE
C       NUM�RO DE FISSURE LOCALE AU DDL HEAVISIDE
C
C----------------------------------------------------------------------
C
C     ARGUMENTS/
C  NOMA       IN       K8 : MAILLAGE
C  MODELX     IN/OUT   K8 : MODELE XFEM
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C

      INTEGER JLCNX,JNBSP,JNBSP2,JCESFD,JCESFL,JCESFV,JCESD,JCESL,JCESV
      INTEGER NBMA,IMA,NBNO,INO,NHEAV,IHEAV,NFISS,IFISS
      INTEGER IRET,IBID,IAD,NNCP
      CHARACTER*8   K8BID
      CHARACTER*19  FISSNO,CES,CESF,LIGREL
      CHARACTER*32  JEXATR
C     ------------------------------------------------------------------

      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      LIGREL = MODELX(1:8)//'.MODELE'
      FISSNO = MODELX(1:8)//'.FISSNO'
      CES  = '&&XFISNO.FISSNO'
      CESF = '&&XFISNO.STNO'
C
C --- TRANSFO CHAM_ELEM -> CHAM_ELEM_S DE STANO
C
      CALL CELCES(MODELX(1:8)//'.STNO','V',CESF)
C
      CALL JEVEUO(CESF//'.CESD','L',JCESFD)
      CALL JEVEUO(CESF//'.CESV','L',JCESFV)
      CALL JEVEUO(CESF//'.CESL','L',JCESFL)
C
C --- RECUPERATION DU NOMBRE DE FISSURES VUES
C
      CALL JEVEUO('&&XTYELE.NBSP','L',JNBSP)

C
C --- RECUPERATION DU NOMBRE DE FONCTIONS HEAVISIDES
C
      CALL JEVEUO('&&XTYELE.NBSP2','L',JNBSP2)
C
C --- CREATION DE LA SD ELNO FISSNO
C
      CALL CESCRE('V',CES,'ELNO',NOMA,'NEUT_I',1,'X1',
     &              IBID,ZI(JNBSP2),-1)
C
      CALL JEVEUO(CES//'.CESD','L',JCESD)
      CALL JEVEUO(CES//'.CESV','E',JCESV)
      CALL JEVEUO(CES//'.CESL','E',JCESL)
C
C --- INFOS SUR LE MAILLAGE
C
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IRET)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JLCNX)
C
      DO 10 IMA = 1,NBMA
        NFISS = ZI(JNBSP-1+IMA)
        NHEAV = ZI(JNBSP2-1+IMA)
        IF (NFISS.GE.2) THEN
          NBNO = ZI(JLCNX+IMA)-ZI(JLCNX-1+IMA)
          DO 20 INO = 1,NBNO
C
C --- PREMIERE PASSE, ON REMPLIT AVEC LES HEAVISIDES ACTIFS
C
            DO 30 IFISS = 1,NFISS
              CALL CESEXI('S',JCESFD,JCESFL,IMA,INO,IFISS,1,IAD)
              CALL ASSERT(IAD.GT.0)
              IF (ZI(JCESFV-1+IAD).EQ.1) THEN
                DO 40 IHEAV = 1,NHEAV
                  CALL CESEXI('S',JCESD,JCESL,IMA,INO,IHEAV,1,IAD)
                  IF (IAD.LT.0) THEN
                    ZL(JCESL-1-IAD) = .TRUE.
                    ZI(JCESV-1-IAD) = IFISS
                    GOTO 30
                  ENDIF
 40             CONTINUE
              ENDIF
 30         CONTINUE
C
C --- DEUXIEME PASSE, ON REMPLIT AVEC LES HEAVISIDES INACTIFS
C
            DO 50 IFISS = 1,NFISS
              CALL CESEXI('S',JCESFD,JCESFL,IMA,INO,IFISS,1,IAD)
              CALL ASSERT(IAD.GT.0)
              IF (ZI(JCESFV-1+IAD).EQ.0) THEN
                DO 60 IHEAV = 1,NHEAV
                  CALL CESEXI('S',JCESD,JCESL,IMA,INO,IHEAV,1,IAD)
                  IF (IAD.LT.0) THEN
                    ZL(JCESL-1-IAD) = .TRUE.
                    ZI(JCESV-1-IAD) = IFISS
                    GOTO 50
                  ENDIF
 60             CONTINUE
              ENDIF
 50         CONTINUE
C
C --- FIN DES DEUX PASSES, LA SD EST DEFINI ENTIEREMENT POUR LE NOEUD
C
 20       CONTINUE
        ENDIF
 10   CONTINUE
C
C --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM
C
      CALL CESCEL(CES,LIGREL,'FULL_MECA','PFISNO','NON',NNCP,'G',FISSNO,
     &            'F',IBID)
      CALL DETRSD('CHAM_ELEM_S',CES)
      CALL DETRSD('CHAM_ELEM_S',CESF)
      CALL JEDEMA()
      END
