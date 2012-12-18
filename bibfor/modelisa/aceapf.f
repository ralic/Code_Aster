      SUBROUTINE ACEAPF(NOMU,NOMA,LMAX,NBOCC)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER                     LMAX,NBOCC
      CHARACTER*8       NOMU,NOMA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT POUTRE_FLUIDE
C ----------------------------------------------------------------------
C IN  : NOMU   : NOM UTILISATEUR DE LA COMMANDE
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : LMAX   : LONGUEUR
C IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE POUFL ------------------
      REAL*8       B(3), AFL, ACE, RAPP
      CHARACTER*19 CARTPF
      CHARACTER*24 TMPNPF, TMPVPF
      INTEGER      IARG
C     ------------------------------------------------------------------
C
C --- CONSTRUCTION DES CARTES ET ALLOCATION
C-----------------------------------------------------------------------
      INTEGER I ,IOC ,JDCC ,JDLS ,JDVC ,NACE ,NAFL 
      INTEGER NB1 ,NB2 ,NB3 ,NG ,NM ,NR ,JDLS2
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CARTPF = NOMU//'.CARPOUFL'
      TMPNPF = CARTPF//'.NCMP'
      TMPVPF = CARTPF//'.VALV'
      CALL ALCART('G',CARTPF,NOMA,'CAPOUF')
      CALL JEVEUO(TMPNPF,'E',JDCC)
      CALL JEVEUO(TMPVPF,'E',JDVC)
C
      CALL WKVECT('&&TMPPOUFL','V V K24',LMAX,JDLS)
      CALL WKVECT('&&TMPPOUFL2','V V K8',LMAX,JDLS2)
C
      ZK8(JDCC)   = 'B_T'
      ZK8(JDCC+1) = 'B_N'
      ZK8(JDCC+2) = 'B_TN'
      ZK8(JDCC+3) = 'A_FLUI'
      ZK8(JDCC+4) = 'A_CELL'
      ZK8(JDCC+5) = 'COEF_ECH'
C
C --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CARTPF
      DO 10 IOC = 1 , NBOCC
         B(1) = 0.D0
         B(2) = 0.D0
         B(3) = 0.D0
         AFL  = 0.D0
         ACE  = 0.D0
         RAPP = 0.D0
      CALL GETVEM(NOMA,'GROUP_MA','POUTRE_FLUI','GROUP_MA',
     +              IOC,IARG,LMAX,ZK24(JDLS),NG)
      CALL GETVEM(NOMA,'MAILLE','POUTRE_FLUI','MAILLE',
     +            IOC,IARG,LMAX,ZK8(JDLS2),NM)
      CALL GETVR8('POUTRE_FLUI','B_T',IOC,IARG,1,
     &            B(1)     ,NB1 )
      CALL GETVR8('POUTRE_FLUI','B_N',IOC,IARG,1,
     &            B(2)     ,NB2 )
      CALL GETVR8('POUTRE_FLUI','B_TN',IOC,IARG,1,
     &            B(3)     ,NB3 )
      CALL GETVR8('POUTRE_FLUI','A_FLUI',IOC,IARG,1,
     &            AFL      ,NAFL)
      CALL GETVR8('POUTRE_FLUI','A_CELL',IOC,IARG,1,
     &            ACE      ,NACE)
      CALL GETVR8('POUTRE_FLUI','COEF_ECHELLE',IOC,IARG,1,
     &            RAPP     ,NR  )
C
         IF(NB2.EQ.0 ) B(2) = B(1)
         ZR(JDVC)   = B(1)
         ZR(JDVC+1) = B(2)
         ZR(JDVC+2) = B(3)
         ZR(JDVC+3) = AFL
         ZR(JDVC+4) = ACE
         ZR(JDVC+5) = RAPP
C
C ---    "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
         IF (NG.GT.0) THEN
            DO 20 I = 1 , NG
               CALL NOCART(CARTPF,2,ZK24(JDLS+I-1),' ',0,' ',0,' ',6)
 20         CONTINUE
         ENDIF
C
C ---    "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
C
         IF (NM.GT.0) THEN
            CALL NOCART(CARTPF,3,' ','NOM',NM,ZK8(JDLS2),0,' ',6)
         ENDIF
C
 10   CONTINUE
C
      CALL JEDETR('&&TMPPOUFL')
      CALL JEDETR('&&TMPPOUFL2')
      CALL JEDETR(TMPNPF)
      CALL JEDETR(TMPVPF)
C
      CALL JEDEMA()
      END
