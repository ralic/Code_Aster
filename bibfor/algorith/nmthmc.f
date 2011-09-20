      SUBROUTINE NMTHMC(COMP, MODELZ, MOCLEF, K, COMEL, NCOMEL, NBNVI)

C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ALGORITH  DATE 19/09/2011   AUTEUR GRANET S.GRANET 
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
C RESPONSABLE GRANET S.GRANET
C =====================================================================
C --- BUT : DETERMINER LA COHERENCE DE LA RELATION DE COUPLAGE THM ----
C =====================================================================
      IMPLICIT NONE
      INTEGER       NCOMEL, K
      INTEGER NBNVI(4)
      CHARACTER*16  COMP, MOCLEF, COMEL(*)
      CHARACTER*(*)  MODELZ
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C =====================================================================
C --- DEFINITION DES DIMENSIONS DES VECTEURS DE POSSIBILITE DES LOIS --
C =====================================================================
      LOGICAL       LTHMC, LHYDR, LMECA, TOUT
      INTEGER       DMTHMC, DMHYDR, DMMECA, JMAIL, ITYPEL
      INTEGER       NBMA, IERD, IBID, JNOMA, JMESM, NUMLC
      PARAMETER   ( DMTHMC = 8  )
      PARAMETER   ( DMHYDR = 5  )
      PARAMETER   ( DMMECA = 18 )
      CHARACTER*16  POTHMC(DMTHMC), MODELI, NOMTE,KBID
      CHARACTER*16  POHYDR(DMHYDR), POMECA(DMMECA)
      CHARACTER*16  COMCOD
      CHARACTER*16  THMC, THER, HYDR, MECA, MOCLES(2)
      CHARACTER*8   NOMA, TYPMCL(2),MODELE
      CHARACTER*24  MESMAI
      CHARACTER*24  VALK(2)
C
      INTEGER       JJ, II, IM, IMA
C *********************************************************************
C --- DEBUT INITIALISATION ------------------------------------------ *
C *********************************************************************
      THMC = ' '
      THER = ' '
      HYDR = ' '
      MECA = ' '
      MODELE=MODELZ
C =====================================================================
C --- PARTIE THMC -----------------------------------------------------
C =====================================================================
      DATA POTHMC / 'LIQU_SATU'     ,
     &              'LIQU_GAZ'      ,
     &              'GAZ'           ,
     &              'LIQU_GAZ_ATM'  ,
     &              'LIQU_VAPE_GAZ' ,
     &              'LIQU_VAPE'     ,
     &              'LIQU_AD_GAZ_VAPE',
     &              'LIQU_AD_GAZ' /
C =====================================================================
C --- PARTIE HYDR -----------------------------------------------------
C =====================================================================
      DATA POHYDR / 'HYDR'      ,
     &              'HYDR_UTIL' ,
     &              'HYDR_VGM' ,
     &              'HYDR_VGC' ,
     &              'HYDR_ENDO' /
C =====================================================================
C --- PARTIE MECA -----------------------------------------------------
C =====================================================================
      DATA POMECA / 'ELAS'            ,
     &              'CJS'             ,
     &              'HUJEUX'          ,
     &              'CAM_CLAY'        ,
     &              'BARCELONE'       ,
     &              'LAIGLE'          ,
     &              'LETK'            ,
     &              'VISC_DRUC_PRAG'  ,
     &              'HOEK_BROWN_EFF'  ,
     &              'HOEK_BROWN_TOT'  ,
     &              'MAZARS'          ,
     &              'ENDO_ISOT_BETON' ,
     &              'ELAS_GONF'       ,
     &              'DRUCK_PRAGER'    ,
     &              'DRUCK_PRAG_N_A'  ,
     &              'JOINT_BANDIS'    ,
     &              'CZM_LIN_REG'     ,
     &              'CZM_EXP_REG'    /
C *********************************************************************
C --- FIN INITIALISATION -------------------------------------------- *
C *********************************************************************
      CALL JEVEUO(MODELE//'.MAILLE','L',JMAIL)
      CALL JEVEUO(MODELE//'.MODELE    .LGRF','L',JNOMA)
      NOMA = ZK8(JNOMA)
C =====================================================================
C --- LE COMPORTEMENT DEFINI EST-IL COHERENT ? ------------------------
C =====================================================================
      LTHMC = .FALSE.
      LHYDR = .FALSE.
      LMECA = .FALSE.
      TOUT = .FALSE.
      MOCLES(1) = 'GROUP_MA'
      MOCLES(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      MESMAI = '&&NMTHMC.MES_MAILLES'

      CALL RELIEM(MODELE,NOMA,'NU_MAILLE',MOCLEF,K,2,MOCLES,
     &           TYPMCL,MESMAI,NBMA)
      IF (NBMA.EQ.0) THEN
         CALL JELIRA(MODELE//'.MAILLE','LONUTI',NBMA,KBID)
         TOUT=.TRUE.
      ELSE
      CALL JEVEUO(MESMAI,'L',JMESM)
      ENDIF

      DO 1 IM = 1,NBMA
C =====================================================================
C --- COHERENCE DE LA LOI DE COUPLAGE ---------------------------------
C =====================================================================
         IF (TOUT) THEN
            IMA = IM
         ELSE
            IMA = ZI(JMESM+IM-1)
         ENDIF
         ITYPEL = ZI(JMAIL-1+IMA)
         IF (ITYPEL.NE.0) THEN
            CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),NOMTE)
            CALL DISMOI('F','MODELISATION',NOMTE,'TYPE_ELEM',IBID,
     &                   MODELI,IERD)
            DO 5 JJ = 1, NCOMEL
              IF ((COMEL(JJ)(1:3).EQ.'GAZ').OR.
     &            (COMEL(JJ)(1:9).EQ.'LIQU_SATU').OR.
     &            (COMEL(JJ)(1:12).EQ.'LIQU_GAZ_ATM')) THEN

                  IF ((MODELI(1:6).NE.'3D_THM').AND.
     &                (MODELI(1:5).NE.'3D_HM').AND.
     &                (MODELI(1:5).NE.'3D_HS').AND.
     &                (MODELI(1:8).NE.'AXIS_THM').AND.
     &                (MODELI(1:7).NE.'AXIS_HM').AND.
     &                (MODELI(1:10).NE.'D_PLAN_THM').AND.
     &                (MODELI(1:9).NE.'D_PLAN_HS').AND.
     &                (MODELI(1:9).NE.'D_PLAN_HM').AND.
     &                (MODELI(1:8).NE.'PLAN_JHM').AND.
     &                (MODELI(1:8).NE.'AXIS_JHM').AND.
     &                (MODELI.NE.'#PLUSIEURS')) THEN

                          VALK(1) = COMEL(JJ)
                          VALK(2) = MODELI
                          CALL U2MESK('F','ALGORITH8_35', 2 ,VALK)
                  ENDIF

               ELSEIF ((COMEL(JJ)(1:13).EQ.'LIQU_VAPE_GAZ').OR.
     &                      (COMEL(JJ)(1:8).EQ.'LIQU_GAZ')) THEN

                  IF ((MODELI(1:6).NE.'3D_THH').AND.
     &                (MODELI(1:6).NE.'3D_HHM').AND.
     &                (MODELI(1:5).NE.'3D_HH').AND.
     &                (MODELI(1:8).NE.'AXIS_THH').AND.
     &                (MODELI(1:8).NE.'AXIS_HHM').AND.
     &                (MODELI(1:7).NE.'AXIS_HH').AND.
     &                (MODELI(1:10).NE.'D_PLAN_THH').AND.
     &                (MODELI(1:10).NE.'D_PLAN_HHM').AND.
     &                (MODELI(1:9) .NE.'D_PLAN_HH').AND.
     &                (MODELI.NE.'#PLUSIEURS')) THEN
C
                      VALK(1) = COMEL(JJ)
                      VALK(2) = MODELI
                      CALL U2MESK('F','ALGORITH8_35', 2 ,VALK)
                  ENDIF
               ELSEIF   (COMEL(JJ)(1:9).EQ.'LIQU_VAPE') THEN

                  IF ((MODELI(1:6).NE.'3D_THV').AND.
     &                (MODELI(1:8).NE.'AXIS_THV').AND.
     &                (MODELI(1:10).NE.'D_PLAN_THV').AND.
     &                (MODELI.NE.'#PLUSIEURS')) THEN

                       VALK(1) = COMEL(JJ)
                       VALK(2) = MODELI
                       CALL U2MESK('F','ALGORITH8_35', 2 ,VALK)
                  ENDIF
               ELSEIF  (COMEL(JJ)(1:16).EQ.'LIQU_AD_GAZ_VAPE') THEN

                  IF ((MODELI(1:9).NE.'AXIS_HH2M').AND.
     &                (MODELI(1:9).NE.'AXIS_THH2').AND.
     &                (MODELI(1:8).NE.'AXIS_HH2').AND.
     &                (MODELI(1:11).NE.'D_PLAN_HH2M').AND.
     &                (MODELI(1:11).NE.'D_PLAN_THH2').AND.
     &                (MODELI(1:11).NE.'D_PLAN_THH2').AND.
     &                (MODELI(1:10).NE.'D_PLAN_HH2').AND.
     &                (MODELI(1:7).NE.'3D_HH2M').AND.
     &                (MODELI(1:7).NE.'3D_THH2').AND.
     &                (MODELI(1:6).NE.'3D_HH2').AND.
     &                (MODELI.NE.'#PLUSIEURS')) THEN
                      VALK(1) = COMEL(JJ)
                      VALK(2) = MODELI
                      CALL U2MESK('F','ALGORITH8_35', 2 ,VALK)
                  ENDIF
               ENDIF
   5        CONTINUE
         ENDIF
   1  CONTINUE
      DO 10 JJ=1, NCOMEL
C =====================================================================
C --- DEFINITION DE LA LOI DE COUPLAGE --------------------------------
C =====================================================================
         DO 20 II = 1, DMTHMC
            IF (COMEL(JJ).EQ.POTHMC(II)) THEN
               THMC = COMEL(JJ)
               IF ( LTHMC ) THEN
                  CALL U2MESS('F','ALGORITH8_36')
               ENDIF
               LTHMC = .TRUE.
               GOTO 10
            ENDIF
 20      CONTINUE
C =====================================================================
C --- DEFINITION DE LA LOI HYDRAULIQUE --------------------------------
C =====================================================================
         DO 40 II = 1, DMHYDR
            IF (COMEL(JJ).EQ.POHYDR(II)) THEN
               HYDR = COMEL(JJ)
               IF ( LHYDR ) THEN
                  CALL U2MESS('F','ALGORITH8_37')
               ENDIF
               LHYDR = .TRUE.
               GOTO 10
            ENDIF
 40      CONTINUE
C =====================================================================
C --- DEFINITION DE LA LOI MECANIQUE ----------------------------------
C =====================================================================
         DO 50 II = 1, DMMECA
            IF (COMEL(JJ).EQ.POMECA(II)) THEN
               MECA = COMEL(JJ)
               IF ( LMECA ) THEN
                  CALL U2MESS('F','ALGORITH8_38')
               ENDIF
               LMECA = .TRUE.
               GOTO 10
            ENDIF
 50      CONTINUE
 10   CONTINUE
C =====================================================================
C --- VERIFICATION DE LA COHERENCE AVEC LA RELATION DEMANDEE ----------
C =====================================================================
      IF (.NOT.LTHMC) THEN
        CALL U2MESS('F','ALGORITH8_39')
      ENDIF
      IF (.NOT.LHYDR) THEN
        CALL U2MESS('F','ALGORITH8_40')
      ENDIF
C =====================================================================
C --- PARTIE KIT_HM ---------------------------------------------------
C =====================================================================
      IF (COMP.EQ.'KIT_HM') THEN
         IF (.NOT.LMECA) THEN
            CALL U2MESS('F','ALGORITH8_41')
         ENDIF
         IF ( THMC.NE.'LIQU_SATU'    .AND.
     &        THMC.NE.'GAZ'          .AND.
     &        THMC.NE.'LIQU_GAZ_ATM'      ) THEN
           VALK(1) = 'HM'
           CALL U2MESK('F','ALGORITH8_42',1,VALK)
         ENDIF
         IF ( HYDR.EQ.'HYDR_ENDO'         .AND.
     &        ( MECA.NE.'MAZARS'          .AND.
     &          MECA.NE.'ENDO_ISOT_BETON'      ) ) THEN
            CALL U2MESS('F','ALGORITH8_43')
         ENDIF
         IF ( MECA.EQ.'BARCELONE' ) THEN
           VALK(1) = 'HM'
           CALL U2MESK('F','ALGORITH8_44',1,VALK)
         ENDIF
C =====================================================================
C --- PARTIE KIT_HHM --------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_HHM') THEN
         IF (.NOT.LMECA) THEN
            CALL U2MESS('F','ALGORITH8_41')
         ENDIF
         IF ( THMC.NE.'LIQU_GAZ'.AND.THMC.NE.'LIQU_VAPE_GAZ'.AND.
     &        THMC.NE.'LIQU_AD_GAZ_VAPE'.AND.
     &        THMC.NE.'LIQU_AD_GAZ'      ) THEN
           VALK(1) = 'HHM'
           CALL U2MESK('F','ALGORITH8_42',1,VALK)
         ENDIF
         IF ( HYDR.EQ.'HYDR_ENDO'         .AND.
     &        ( MECA.NE.'MAZARS'          .AND.
     &          MECA.NE.'ENDO_ISOT_BETON'      ) ) THEN
            CALL U2MESS('F','ALGORITH8_43')
         ENDIF
         IF ( MECA.EQ.'BARCELONE' .AND.
     &        (THMC.NE.'LIQU_GAZ' .AND.
     &         THMC.NE.'LIQU_VAPE_GAZ')) THEN
           VALK(1) = 'HHM'
           CALL U2MESK('F','ALGORITH8_44',1,VALK)
         ENDIF
C =====================================================================
C --- PARTIE KIT_H ----------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_H') THEN
         IF (LMECA) THEN
           VALK(1) = 'H'
           CALL U2MESK('F','ALGORITH8_46',1,VALK)
         ENDIF
         IF ( THMC.NE.'LIQU_SATU'.AND.THMC.NE.'GAZ') THEN
            CALL U2MESS('F','ALGORITH8_59')
         ENDIF
C =====================================================================
C --- PARTIE KIT_THH --------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_THH') THEN
         THER = 'THER'
         IF (LMECA) THEN
           VALK(1) = 'THH'
           CALL U2MESK('F','ALGORITH8_46',1,VALK)
         ENDIF
         IF ( THMC.NE.'LIQU_GAZ' .AND.THMC.NE.'LIQU_VAPE_GAZ'.AND.
     &        THMC.NE.'LIQU_AD_GAZ_VAPE'.AND.
     &        THMC.NE.'LIQU_AD_GAZ'      ) THEN
           VALK(1) = 'THH'
           CALL U2MESK('F','ALGORITH8_42',1,VALK)
         ENDIF
C =====================================================================
C --- PARTIE KIT_HH --------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_HH') THEN
         IF (LMECA) THEN
           VALK(1) = 'HH'
           CALL U2MESK('F','ALGORITH8_46',1,VALK)
         ENDIF
         IF ( THMC.NE.'LIQU_GAZ' .AND.THMC.NE.'LIQU_VAPE_GAZ'.AND.
     &        THMC.NE.'LIQU_AD_GAZ_VAPE'.AND.
     &        THMC.NE.'LIQU_AD_GAZ'      ) THEN
           VALK(1) = 'HH'
           CALL U2MESK('F','ALGORITH8_42',1,VALK)
         ENDIF
C =====================================================================
C --- PARTIE KIT_THV --------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_THV') THEN
         THER = 'THER'
         IF (LMECA) THEN
           VALK(1) = 'THV'
           CALL U2MESK('F','ALGORITH8_46',1,VALK)
         ENDIF
         IF ( THMC.NE.'LIQU_VAPE' ) THEN
           VALK(1) = 'THV'
           CALL U2MESK('F','ALGORITH8_42',1,VALK)
         ENDIF
C =====================================================================
C --- PARTIE KIT_THM --------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_THM') THEN
         THER = 'THER'
         IF (.NOT.LMECA) THEN
            CALL U2MESS('F','ALGORITH8_41')
         ENDIF
         IF ( THMC.NE.'LIQU_SATU'     .AND.
     &        THMC.NE.'LIQU_GAZ_ATM'  .AND.
     &        THMC.NE.'GAZ'                ) THEN
           VALK(1) = 'THM'
           CALL U2MESK('F','ALGORITH8_42',1,VALK)
         ENDIF
         IF ( HYDR.EQ.'HYDR_ENDO'         .AND.
     &        ( MECA.NE.'MAZARS'          .AND.
     &          MECA.NE.'ENDO_ISOT_BETON'      ) ) THEN
            CALL U2MESS('F','ALGORITH8_43')
         ENDIF
         IF ( MECA.EQ.'BARCELONE' ) THEN
           VALK(1) = 'THM'
           CALL U2MESK('F','ALGORITH8_44',1,VALK)
         ENDIF
C =====================================================================
C --- PARTIE KIT_THHM -------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_THHM') THEN
         THER = 'THER'
         IF (.NOT.LMECA) THEN
            CALL U2MESS('F','ALGORITH8_41')
         ENDIF
         IF ( THMC.NE.'LIQU_VAPE_GAZ' .AND.
     &        THMC.NE.'LIQU_AD_GAZ_VAPE' .AND.
     &        THMC.NE.'LIQU_AD_GAZ'.AND.
     &        THMC.NE.'LIQU_GAZ'           ) THEN
           VALK(1) = 'THHM'
           CALL U2MESK('F','ALGORITH8_42',1,VALK)
         ENDIF
         IF ( HYDR.EQ.'HYDR_ENDO'         .AND.
     &        ( MECA.NE.'MAZARS'          .AND.
     &          MECA.NE.'ENDO_ISOT_BETON'      ) ) THEN
            CALL U2MESS('F','ALGORITH8_43')
         ENDIF
         IF ( MECA.EQ.'BARCELONE' .AND.
     &        (THMC.NE.'LIQU_GAZ' .AND.
     &        THMC.NE.'LIQU_VAPE_GAZ')) THEN
           VALK(1) = 'THHM'
           CALL U2MESK('F','ALGORITH8_42',1,VALK)
         ENDIF
      ENDIF
C =========================================================
C MISE A JOUR DES RELATIONS DE COMPORTEMENTS --------------
C =========================================================
      COMEL(1) = THMC
      COMEL(2) = THER
      COMEL(3) = HYDR
      COMEL(4) = MECA
C  RECUPERATION DES NOMBRES DE VARIABLES INTERNES

C ======================================================================
C --- POUR CHAQUE RELATION DE COMPORTEMENT PRESENTE ON RECUPERE --------
C --- LE NOMBRE DE VARIABLES INTERNES ASSOCIE A CETTE LOI --------------
C ======================================================================
C --- LOI DE COUPLAGE --------------------------------------------------
C ======================================================================
      IF (COMEL(1).NE.' ') THEN
         CALL LCCREE(1, COMEL(1), COMCOD)
         CALL LCINFO(COMCOD, NUMLC, NBNVI(1))
      ELSE
         NBNVI(1)=0
      ENDIF
C ======================================================================
C --- LOI DE THERMIQUE -------------------------------------------------
C ======================================================================
      IF (COMEL(2).NE.' ') THEN
         CALL LCCREE(1, COMEL(2), COMCOD)
         CALL LCINFO(COMCOD, NUMLC, NBNVI(2))
      ELSE
         NBNVI(2)=0
      ENDIF
C ======================================================================
C --- LOI HYDRAULIQUE --------------------------------------------------
C ======================================================================
      IF (COMEL(3).NE.' ') THEN
         CALL LCCREE(1, COMEL(3), COMCOD)
         CALL LCINFO(COMCOD, NUMLC, NBNVI(3))
      ELSE
         NBNVI(3)=0
      ENDIF
C ======================================================================
C --- LOI DE MECANIQUE -------------------------------------------------
C ======================================================================
      IF (COMEL(4).NE.' ') THEN
         CALL LCCREE(1, COMEL(4), COMCOD)
         CALL LCINFO(COMCOD, NUMLC, NBNVI(4))
      ELSE
         NBNVI(4)=0
      ENDIF
C =====================================================================
      END
