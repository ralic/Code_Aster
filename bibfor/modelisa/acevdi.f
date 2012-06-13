      SUBROUTINE ACEVDI(NBOCC,NOMAZ,NOMOZ,MCF,NLM,NLG,NLN,NLJ,IER)
      IMPLICIT          NONE
      INCLUDE 'jeveux.h'
      INTEGER           NBOCC,NLM,NLG,NLN,NLJ,IER
      CHARACTER*(*)     NOMAZ,NOMOZ,MCF
C ----------------------------------------------------------------------
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C --- ------------------------------------------------------------------
C        AFFE_CARA_ELEM
C           VERIFICATION DES MOTS CLES POUR L'ELEMENT DISCRET
C --- ------------------------------------------------------------------
C IN
C     NBOCC    :  NOMBRE D'OCCURENCE
C     NOMAZ    :  NOM DU MAILLAGE
C     NOMOZ    :  NOM DU MODELE
C     MCF      :  MOT CLEF
C OUT
C     NLM      :  NOMBRE TOTAL DE MAILLE
C     NLG      :  NOMBRE TOTAL DE GROUPE DE MAILLE
C     NLN      :  NOMBRE TOTAL DE NOEUD
C     NLJ      :  NOMBRE TOTAL DE GROUP_NO
C     IER      :  ERREUR
C
C --- ------------------------------------------------------------------
C
      CHARACTER*4    TYPE
      CHARACTER*8    K8B,NOMU,NOMA,NOMO,NOMAIL,TYPEL,NOGRM
      CHARACTER*16   CONCEP,CMD
      CHARACTER*24   GRMAMA,MAILMA,CARA
      CHARACTER*24   VALK(4)
      INTEGER        IDTYMA,I3D,I2D,NDIM1,IOC,NC,NG,NM,NJ,NN,NSOM,NBMAIL
      INTEGER        N1,IMA,NBGRM,JGRM,IG,JMAIL,NUMA,NUTYMA,LMAX2
      INTEGER      IARG
C     ------------------------------------------------------------------
      CALL GETRES(NOMU,CONCEP,CMD)
C
      NOMA = NOMAZ
      NOMO = NOMOZ
      NLM = 0
      NLG = 0
      NLN = 0
      NLJ = 0
      GRMAMA = NOMA//'.GROUPEMA'
      MAILMA = NOMA//'.NOMMAI'
C
C --- VECTEUR DU TYPE DES MAILLES DU MAILLAGE :
C     ---------------------------------------
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IDTYMA)

C --- VERIFICATION DES DIMENSIONS / MODELISATIONS
      CALL VERDIS(NOMO,NOMA,'E',I3D,I2D,NDIM1,IER)
      CALL ASSERT( (MCF.EQ.'DISCRET_2D').OR.(MCF.EQ.'DISCRET') )

C --- BOUCLE SUR LES OCCURENCES :
C     -------------------------
      DO 10 IOC = 1,NBOCC
         CALL GETVTX(MCF,'CARA',IOC,IARG,1,CARA,NC)
C
         CALL GETVEM(NOMA,'GROUP_MA',MCF,'GROUP_MA',IOC,IARG,0,K8B,NG)
         CALL GETVEM(NOMA,'MAILLE'  ,MCF,'MAILLE'  ,IOC,IARG,0,K8B,NM)
         CALL GETVEM(NOMA,'GROUP_NO',MCF,'GROUP_NO',IOC,IARG,0,K8B,NJ)
         CALL GETVEM(NOMA,'NOEUD'   ,MCF,'NOEUD'   ,IOC,IARG,0,K8B,NN)
C
         NSOM = NG + NM + NJ + NN
         IF ( (NSOM.EQ.NG).OR.(NSOM.EQ.NM).OR.(NSOM.EQ.NJ).OR.
     &        (NSOM.EQ.NN)) THEN
            NLM = MAX(NLM,-NM)
            NLG = MAX(NLG,-NG)
            NLN = MAX(NLN,-NN)
            NLJ = MAX(NLJ,-NJ)
         ENDIF
C
C ------ VERIFICATION DU BON TYPE DE MAILLE EN FONCTION DE CARA :
C        ------------------------------------------------------
         IF ( (CARA(2:7) .EQ. '_T_D_N').OR.
     &        (CARA(2:8) .EQ. '_TR_D_N').OR.
     &        (CARA(2:5) .EQ. '_T_N').OR.
     &        (CARA(2:6) .EQ. '_TR_N') ) THEN
            TYPE = 'POI1'
         ELSE
            TYPE = 'SEG2'
         ENDIF
C
         IF ( NM .NE. 0 ) THEN
            NBMAIL = -NM
            CALL WKVECT ( '&&ACEVDI.MAILLE', 'V V K8', NBMAIL, JMAIL )
            CALL GETVTX (MCF,'MAILLE',IOC,IARG,NBMAIL,
     &                   ZK8(JMAIL), N1 )
            DO 12 IMA = 1, NBMAIL
               NOMAIL = ZK8(JMAIL+IMA-1)
               CALL VERIMA(NOMA, NOMAIL, 1, 'MAILLE')
               CALL JENONU(JEXNOM(MAILMA,NOMAIL),NUMA)
               NUTYMA = ZI(IDTYMA+NUMA-1)
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPEL)
               IF (TYPEL(1:4).NE.TYPE) THEN
                   VALK(1) = NOMAIL
                   VALK(2) = TYPE
                   VALK(3) = TYPEL
                   VALK(4) = CARA
                   CALL U2MESK('F','MODELISA_56', 4 ,VALK)
               ENDIF
 12         CONTINUE
            CALL JEDETR ( '&&ACEVDI.MAILLE' )
         ENDIF
C
         IF ( NG .NE. 0 ) THEN
            NBGRM = -NG
            CALL WKVECT ( '&&ACEVDI.GROUP_MA', 'V V K8', NBGRM, JGRM )
            CALL GETVTX (MCF,'GROUP_MA',IOC,IARG,NBGRM,
     &                   ZK8(JGRM), N1 )
            DO 14 IG = 1, NBGRM
               NOGRM = ZK8(JGRM+IG-1)
               CALL VERIMA(NOMA, NOGRM, 1, 'GROUP_MA')
               CALL JELIRA(JEXNOM(GRMAMA,NOGRM),'LONUTI',NBMAIL,K8B)
               CALL JEVEUO(JEXNOM(GRMAMA,NOGRM),'L',JMAIL)
               DO 16 IMA = 1 , NBMAIL
                  NUMA = ZI(JMAIL+IMA-1)
                  NUTYMA = ZI(IDTYMA+NUMA-1)
                  CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',NUTYMA),TYPEL)
                  IF (TYPEL(1:4).NE.TYPE) THEN
                     CALL JENUNO(JEXNUM(MAILMA,NUMA),NOMAIL)
                   VALK(1) = NOMAIL
                   VALK(2) = TYPE
                   VALK(3) = TYPEL
                   VALK(4) = CARA
                   CALL U2MESK('F','MODELISA_56', 4 ,VALK)
                  ENDIF
 16            CONTINUE
 14         CONTINUE
            CALL JEDETR ( '&&ACEVDI.GROUP_MA' )
         ENDIF
C
 10   CONTINUE
C
      LMAX2 = MAX(1,NLM,NLG,NLN,NLJ)
      CALL ACEVD2(NOMA,NOMO,MCF,LMAX2,NBOCC)
C
      END
