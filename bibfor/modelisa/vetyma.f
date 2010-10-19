      SUBROUTINE VETYMA(NOMA,LISTMA,NBMA,LISTGR,NBGR,OPTION,NDIM,CODRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 19/10/2010   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C BUT : VERIFICATION DU TYPE DES MAILLES AFFECTEES SUIVANT LE CHARGEMENT
C
C ARGUMENTS D'ENTREE:
C      NOMA   : NOM DU MAILLAGE
C      LISTMA : LISTE DES MAILLES
C      NBMA   : NOMBRE DE MAILLES
C      LISTGR : LISTE DES GROUPES DE MAILLES
C      NBGR   : NOMBRE DE GROUPES DE MAILLES
C      OPTION : MOT-CLE FACTEUR DANS AFFE_CHAR_MECA OU AFFE_CHAR_THER
C      NDIM   : DIMENSION DU PROBLEME (2D OU 3D)
C
C ARGUMENT DE SORTIE:
C      CODRET : CODE RETOUR : 0 SI OK, >0 SINON
C
C MOT-CLES FACTEUR VERIFIES :   FLUX_REP  ECHANGE     SOURCE
C                               PRES_REP  FORCE_FACE  FORCE_CONTOUR
C                               VITE_FACE IMPE_FACE   FORCE_INTERNE
C
C ROUTINES APPELEES:
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON/IVARJE/ZI(1)
      INTEGER ZI,CODRET
      COMMON/RVARJE/ZR(1)
      REAL*8 ZR
      COMMON/CVARJE/ZC(1)
      COMPLEX*16 ZC
      COMMON/LVARJE/ZL(1)
      LOGICAL ZL
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*8 LISTMZ, LISTGZ
      CHARACTER*(*) NOMA,LISTMA(1),LISTGR(1),OPTION
      CHARACTER*8  ZK8,TYPE,KIMA,NOMA8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24,TYPMA,GRPMA,OPTIOZ
      CHARACTER*24 VALK(2)
      CHARACTER*32 ZK32,JEXNUM,JEXNOM
      CHARACTER*80 ZK80
      CHARACTER*1 K1BID
C ----------------------------------------------------------------------
      CALL JEMARQ()
      OPTIOZ = OPTION
      NOMA8=NOMA
      CALL JEVEUO(NOMA8//'.TYPMAIL','L',IATYMA)
      GRPMA = NOMA8//'.GROUPEMA'
      NERR=0
C
      IF(OPTION.EQ.'FLUX_REP' .OR.OPTION.EQ.'PRES_REP'  .OR.
     &   OPTION.EQ.'ECHANGE'  .OR.OPTION.EQ.'FORCE_FACE'.OR.
     &   OPTION.EQ.'IMPE_FACE'.OR.OPTION.EQ.'VITE_FACE' .OR.
     &   OPTION.EQ.'FORCE_CONTOUR') THEN
C
C  MOT-CLE MAILLE
         IF(NBMA.GT.0) THEN
            DO 1 I=1,NBMA
               CALL JENONU(JEXNOM(NOMA8//'.NOMMAI',LISTMA(I)),IBID)
               IADTYP=IATYMA-1+IBID
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(IADTYP)),TYPE)
               LISTMZ = LISTMA(I)
               IF(NDIM.EQ.2.AND.TYPE(1:3).NE.'SEG') THEN
                  NERR=NERR+1
                   VALK(1) = LISTMZ
                   VALK(2) = OPTIOZ
                   CALL U2MESK('A','MODELISA7_86', 2 ,VALK)
               ELSEIF(NDIM.EQ.3.AND.TYPE(1:4).NE.'QUAD'.
     &                          AND.TYPE(1:4).NE.'TRIA') THEN
                  NERR=NERR+1
                   VALK(1) = LISTMZ
                   VALK(2) = OPTIOZ
                   CALL U2MESK('A','MODELISA7_87', 2 ,VALK)
               ENDIF
1           CONTINUE
            IF(NBMA.EQ.NERR) THEN
              CALL U2MESK('A','MODELISA7_88',1,OPTIOZ)
            ENDIF
         ENDIF
C
C  MOT-CLE GROUP_MA
         IF(NBGR.GT.0) THEN
            DO 2 I=1,NBGR
               CALL JEVEUO(JEXNOM(GRPMA,LISTGR(I)),'L',IADGMA)
               CALL JELIRA(JEXNOM(GRPMA,LISTGR(I)),'LONUTI',NBMA,K1BID)
               DO 3 J=1,NBMA
               IMA=ZI(IADGMA-1+J)
               CALL CODENT(IMA,'G',KIMA)
               IADTYP=IATYMA-1+IMA
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(IADTYP)),TYPE)
               IF(NDIM.EQ.2.AND.TYPE(1:3).NE.'SEG') THEN
                  NERR=NERR+1
                   VALK(1) = KIMA
                   VALK(2) = OPTIOZ
                   CALL U2MESK('A','MODELISA7_89', 2 ,VALK)
               ELSEIF(NDIM.EQ.3.AND.TYPE(1:4).NE.'QUAD'.
     &                          AND.TYPE(1:4).NE.'TRIA'.
     &                          AND.TYPE(1:3).NE.'SEG') THEN
                  NERR=NERR+1
                   VALK(1) = KIMA
                   VALK(2) = OPTIOZ
                   CALL U2MESK('A','MODELISA7_90', 2 ,VALK)
               ENDIF
3              CONTINUE
            IF(NBMA.EQ.NERR) THEN
              LISTGZ = LISTGR(I)
               VALK(1) = LISTGZ
               VALK(2) = OPTIOZ
               CALL U2MESK('A','MODELISA7_91', 2 ,VALK)
            ENDIF
2           CONTINUE
         ENDIF
C
C
      ELSEIF(OPTION.EQ.'SOURCE' .OR.OPTION.EQ.'FORCE_INTERNE') THEN
C  MOT-CLE MAILLE
        IF(NBMA.GT.0) THEN
            DO 10 I=1,NBMA
               CALL JENONU(JEXNOM(NOMA8//'.NOMMAI',LISTMA(I)),IBID)
               IADTYP=IATYMA-1+IBID
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(IADTYP)),TYPE)
               LISTMZ = LISTMA(I)
               IF(NDIM.EQ.2.AND.TYPE(1:4).NE.'QUAD'.
     &                      AND.TYPE(1:4).NE.'TRIA') THEN
                  NERR=NERR+1
                   VALK(1) = LISTMZ
                   VALK(2) = OPTIOZ
                   CALL U2MESK('A','MODELISA7_87', 2 ,VALK)
               ELSEIF(NDIM.EQ.3.AND.TYPE(1:4).NE.'HEXA'.
     &                          AND.TYPE(1:4).NE.'PENT'.
     &                          AND.TYPE(1:4).NE.'PYRA'.
     &                          AND.TYPE(1:4).NE.'TETR') THEN
                  NERR=NERR+1
                   VALK(1) = LISTMZ
                   VALK(2) = OPTIOZ
                   CALL U2MESK('A','MODELISA7_92', 2 ,VALK)
               ENDIF
10           CONTINUE
            IF(NBMA.EQ.NERR) THEN
              CALL U2MESK('A','MODELISA7_88',1,OPTIOZ)
            ENDIF
         ENDIF
C  MOT-CLE GROUP_MA
         IF(NBGR.GT.0) THEN
            DO 20 I=1,NBGR
               CALL JEVEUO(JEXNOM(GRPMA,LISTGR(I)),'L',IADGMA)
               CALL JELIRA(JEXNOM(GRPMA,LISTGR(I)),'LONUTI',NBMA,K1BID)
                  DO 30 J=1,NBMA
                 IMA=ZI(IADGMA-1+J)
                 CALL CODENT(IMA,'G',KIMA)
                 IADTYP=IATYMA-1+IMA
                 CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(IADTYP)),TYPE)
                 IF(NDIM.EQ.2.AND.TYPE(1:4).NE.'QUAD'.
     &                        AND.TYPE(1:4).NE.'TRIA') THEN
                   NERR=NERR+1
                    VALK(1) = KIMA
                    VALK(2) = OPTIOZ
                    CALL U2MESK('A','MODELISA7_90', 2 ,VALK)
                 ELSEIF(NDIM.EQ.3.AND.TYPE(1:4).NE.'HEXA'.
     &                            AND.TYPE(1:4).NE.'PENT'.
     &                            AND.TYPE(1:4).NE.'PYRA'.
     &                            AND.TYPE(1:4).NE.'TETR') THEN
                    NERR=NERR+1
                     VALK(1) = KIMA
                     VALK(2) = OPTIOZ
                     CALL U2MESK('A','MODELISA7_93', 2 ,VALK)
                 ENDIF
30             CONTINUE
               IF(NBMA.EQ.NERR) THEN
               LISTGZ = LISTGR(I)
                VALK(1) = LISTGZ
                VALK(2) = OPTIOZ
                CALL U2MESK('A','MODELISA7_91', 2 ,VALK)
               ENDIF
20         CONTINUE
        ENDIF
      ENDIF
      CODRET = NERR
      CALL JEDEMA()
      END
