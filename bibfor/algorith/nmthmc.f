      SUBROUTINE NMTHMC(COMP, COMEL, NCOMEL)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/09/2003   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE UFBHHLL C.CHAVANT
C =====================================================================
C --- BUT : DETERMINER LA COHERENCE DE LA RELATION DE COUPLAGE THM ----
C =====================================================================
      IMPLICIT      NONE
      INTEGER       NCOMEL
      CHARACTER*16  COMP, COMEL(NCOMEL)
C =====================================================================
C --- DEFINITION DES DIMENSIONS DES VECTEURS DE POSSIBILITE DES LOIS --
C =====================================================================
      LOGICAL       LTHMC, LTHER, LHYDR, LMECA
      INTEGER       DMTHMC, DMTHER, DMHYDR, DMMECA
      PARAMETER   ( DMTHMC = 8  )
      PARAMETER   ( DMTHER = 2  )
      PARAMETER   ( DMHYDR = 3  )
      PARAMETER   ( DMMECA = 10 )
      CHARACTER*16  POTHMC(DMTHMC), POTHER(DMTHER)
      CHARACTER*16  POHYDR(DMHYDR), POMECA(DMMECA)
      CHARACTER*16  THMC, THER, HYDR, MECA
C
      INTEGER       JJ, II
C *********************************************************************
C --- DEBUT INITIALISATION ------------------------------------------ *
C *********************************************************************
      THMC = '        '
      THER = '        '
      HYDR = '        '
      MECA = '        '
C =====================================================================
C --- PARTIE THMC -----------------------------------------------------
C =====================================================================
      DATA POTHMC / 'LIQU_SATU'     ,
     +              'LIQU_GAZ'      ,
     +              'GAZ'           ,
     +              'LIQU_GAZ_ATM'  ,
     +              'LIQU_VAPE_GAZ' ,
     +              'LIQU_VAPE'     ,
     +              'LIQU_SATU_GAT' ,
     +              'LIQU_NSAT_GAT' /
C =====================================================================
C --- PARTIE THER -----------------------------------------------------
C =====================================================================
      DATA POTHER / 'THER_HOMO' ,
     +              'THER_POLY' /
C =====================================================================
C --- PARTIE HYDR -----------------------------------------------------
C =====================================================================
      DATA POHYDR / 'HYDR'      ,
     +              'HYDR_UTIL' ,
     +              'HYDR_ENDO' /
C =====================================================================
C --- PARTIE MECA -----------------------------------------------------
C =====================================================================
      DATA POMECA / 'ELAS'            ,
     +              'CJS'             ,
     +              'CAM_CLAY'        ,
     +              'LAIGLE'          ,
     +              'ELAS_THM'        ,
     +              'SURF_ETAT_NSAT'  ,
     +              'SURF_ETAT_SATU'  ,
     +              'CAM_CLAY_THM'    ,
     +              'MAZARS'          ,
     +              'ENDO_ISOT_BETON' /
C *********************************************************************
C --- FIN INITIALISATION -------------------------------------------- *
C *********************************************************************
C =====================================================================
C --- LE COMPORTEMENT DEFINIT EST-IL COHERENT ? -----------------------
C =====================================================================
      LTHMC = .FALSE.
      LTHER = .FALSE.
      LHYDR = .FALSE.
      LMECA = .FALSE.
      DO 10 JJ = 1, NCOMEL
C =====================================================================
C --- DEFINITION DE LA LOI DE COUPLAGE --------------------------------
C =====================================================================
         DO 20 II = 1, DMTHMC
            IF (COMEL(JJ).EQ.POTHMC(II)) THEN
               THMC = COMEL(JJ)
               IF ( LTHMC ) THEN
                  CALL UTMESS('F','NMTHMC_1','IL Y A DEJA UNE LOI '//
     +                                                  'DE COUPLAGE')
               ENDIF
               LTHMC = .TRUE.
               GOTO 10
            ENDIF
 20      CONTINUE
C =====================================================================
C --- DEFINITION DE LA LOI THERMIQUE ----------------------------------
C =====================================================================
         DO 30 II = 1, DMTHER
            IF (COMEL(JJ).EQ.POTHER(II)) THEN
               THER = COMEL(JJ)
               IF ( LTHER ) THEN
                  CALL UTMESS('F','NMTHMC_2','IL Y A DEJA UNE LOI '//
     +                                                 'DE THERMIQUE')
               ENDIF
               LTHER = .TRUE.
               GOTO 10
            ENDIF
 30      CONTINUE
C =====================================================================
C --- DEFINITION DE LA LOI HYDRAULIQUE --------------------------------
C =====================================================================
         DO 40 II = 1, DMHYDR
            IF (COMEL(JJ).EQ.POHYDR(II)) THEN
               HYDR = COMEL(JJ)
               IF ( LHYDR ) THEN
                  CALL UTMESS('F','NMTHMC_3','IL Y A DEJA UNE LOI '//
     +                                                 'HYDRAULIQUE')
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
                  CALL UTMESS('F','NMTHMC_4','IL Y A DEJA UNE LOI '//
     +                                                 'DE MECANIQUE')
               ENDIF
               LMECA = .TRUE.
               GOTO 10
            ENDIF
 50      CONTINUE
 10   CONTINUE
C =====================================================================
C --- VERIFICATION DE LA COHERENCE AVEC LA RELATION DEMANDEE ----------
C =====================================================================
C --- PARTIE KIT_HM ---------------------------------------------------
C =====================================================================
      IF (COMP.EQ.'KIT_HM') THEN
         IF (.NOT.LTHMC) THEN
            CALL UTMESS('F','NMTHMC_5','IL N Y A PAS DE LOI DE '//
     +                                                     'COUPLAGE')
         ENDIF
         IF (LTHER) THEN
            CALL UTMESS('F','NMTHMC_6','IL Y A UNE LOI DE '//
     +                                'THERMIQUE DANS LA RELATION HM')
         ENDIF
         IF (.NOT.LHYDR) THEN
            CALL UTMESS('F','NMTHMC_7','IL N Y A PAS DE LOI '//
     +                                                 'HYDRAULIQUE')
         ENDIF
         IF (.NOT.LMECA) THEN
            CALL UTMESS('F','NMTHMC_8','IL N Y A PAS DE LOI DE '//
     +                                                    'MECANIQUE')
         ENDIF
         IF ( THMC.NE.'LIQU_SATU'    .AND.
     +        THMC.NE.'GAZ'          .AND.
     +        THMC.NE.'LIQU_GAZ_ATM'      ) THEN
            CALL UTMESS('F','NMTHMC_9','LA LOI DE COUPLAGE EST '//
     +                          'INCORRECTE POUR UNE MODELISATION HM')
         ENDIF
         IF ( HYDR.EQ.'HYDR_ENDO'         .AND.
     +        ( MECA.NE.'MAZARS'          .AND.
     +          MECA.NE.'ENDO_ISOT_BETON'      ) ) THEN
            CALL UTMESS('F','NMTHMC_10','INCOMPATIBILITE DES '//
     +                       'COMPORTEMENTS MECANIQUE ET HYDRAULIQUE')
         ENDIF
         IF ( MECA.EQ.'ELAS_THM' .OR.
     +        MECA.EQ.'SURF_ETAT_SATU' .OR.
     +        MECA.EQ.'CAM_CLAY_THM'   .OR.
     +        MECA.EQ.'SURF_ETAT_NSAT'      ) THEN
            CALL UTMESS('F','NMTHMC_11','LOI DE MECANIQUE '//
     +                                  'INCOMPATIBLE AVEC UNE LOI HM')
         ENDIF
C =====================================================================
C --- PARTIE KIT_HHM --------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_HHM') THEN
         IF (.NOT.LTHMC) THEN
            CALL UTMESS('F','NMTHMC_12','IL N Y A PAS DE LOI DE '//
     +                                                     'COUPLAGE')
         ENDIF
         IF (LTHER) THEN
            CALL UTMESS('F','NMTHMC_13','IL Y A UNE LOI DE '//
     +                               'THERMIQUE DANS LA RELATION HHM')
         ENDIF
         IF (.NOT.LHYDR) THEN
            CALL UTMESS('F','NMTHMC_14','IL N Y A PAS DE LOI '//
     +                                                 'HYDRAULIQUE')
         ENDIF
         IF (.NOT.LMECA) THEN
            CALL UTMESS('F','NMTHMC_15','IL N Y A PAS DE LOI DE '//
     +                                                    'MECANIQUE')
         ENDIF
         IF ( THMC.NE.'LIQU_GAZ'      .AND.
     +        THMC.NE.'LIQU_VAPE_GAZ'      ) THEN
            CALL UTMESS('F','NMTHMC_16','LA LOI DE COUPLAGE EST '//
     +                         'INCORRECTE POUR UNE MODELISATION HHM')
         ENDIF
         IF ( HYDR.EQ.'HYDR_ENDO'         .AND.
     +        ( MECA.NE.'MAZARS'          .AND.
     +          MECA.NE.'ENDO_ISOT_BETON'      ) ) THEN
            CALL UTMESS('F','NMTHMC_17','INCOMPATIBILITE DES '//
     +                       'COMPORTEMENTS MECANIQUE ET HYDRAULIQUE')
         ENDIF
         IF ( MECA.EQ.'ELAS_THM' .OR.
     +        MECA.EQ.'SURF_ETAT_SATU' .OR.
     +        MECA.EQ.'CAM_CLAY_THM'   .OR.
     +        MECA.EQ.'SURF_ETAT_NSAT'      ) THEN
            CALL UTMESS('F','NMTHMC_18','LOI DE MECANIQUE '//
     +                                 'INCOMPATIBLE AVEC UNE LOI HHM')
         ENDIF
C =====================================================================
C --- PARTIE KIT_THH --------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_THH') THEN
         IF (.NOT.LTHMC) THEN
            CALL UTMESS('F','NMTHMC_19','IL N Y A PAS DE LOI DE '//
     +                                                     'COUPLAGE')
         ENDIF
         IF (.NOT.LTHER) THEN
            CALL UTMESS('F','NMTHMC_20','IL N Y A PAS DE LOI DE '//
     +                                                    'THERMIQUE')
         ENDIF
         IF (.NOT.LHYDR) THEN
            CALL UTMESS('F','NMTHMC_21','IL N Y A PAS DE LOI '//
     +                                                 'HYDRAULIQUE')
         ENDIF
         IF (LMECA) THEN
            CALL UTMESS('F','NMTHMC_22','IL Y A UNE LOI DE '//
     +                                'MECANIQUE DANS LA RELATION THH')
         ENDIF
         IF ( THMC.NE.'LIQU_GAZ'      .AND.
     +        THMC.NE.'LIQU_VAPE_GAZ'      ) THEN
            CALL UTMESS('F','NMTHMC_23','LA LOI DE COUPLAGE EST '//
     +                         'INCORRECTE POUR UNE MODELISATION THH')
         ENDIF
         IF ( MECA.EQ.'ELAS_THM' .OR.
     +        MECA.EQ.'SURF_ETAT_SATU' .OR.
     +        MECA.EQ.'CAM_CLAY_THM'   .OR.
     +        MECA.EQ.'SURF_ETAT_NSAT'      ) THEN
            CALL UTMESS('F','NMTHMC_24','LOI DE MECANIQUE '//
     +                                 'INCOMPATIBLE AVEC UNE LOI THH')
         ENDIF
C =====================================================================
C --- PARTIE KIT_THV --------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_THV') THEN
         IF (.NOT.LTHMC) THEN
            CALL UTMESS('F','NMTHMC_25','IL N Y A PAS DE LOI DE '//
     +                                                     'COUPLAGE')
         ENDIF
         IF (.NOT.LTHER) THEN
            CALL UTMESS('F','NMTHMC_26','IL N Y A PAS DE LOI DE '//
     +                                                    'THERMIQUE')
         ENDIF
         IF (.NOT.LHYDR) THEN
            CALL UTMESS('F','NMTHMC_27','IL N Y A PAS DE LOI '//
     +                                                 'HYDRAULIQUE')
         ENDIF
         IF (LMECA) THEN
            CALL UTMESS('F','NMTHMC_28','IL Y A UNE LOI DE '//
     +                                'MECANIQUE DANS LA RELATION THV')
         ENDIF
         IF ( THMC.NE.'LIQU_VAPE' ) THEN
            CALL UTMESS('F','NMTHMC_29','LA LOI DE COUPLAGE EST '//
     +                          'INCORRECTE POUR UNE MODELISATION THV')
         ENDIF
         IF ( MECA.EQ.'ELAS_THM' .OR.
     +        MECA.EQ.'SURF_ETAT_SATU' .OR.
     +        MECA.EQ.'CAM_CLAY_THM'   .OR.
     +        MECA.EQ.'SURF_ETAT_NSAT'      ) THEN
            CALL UTMESS('F','NMTHMC_30','LOI DE MECANIQUE '//
     +                                 'INCOMPATIBLE AVEC UNE LOI THV')
         ENDIF
C =====================================================================
C --- PARTIE KIT_THM --------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_THM') THEN
         IF (.NOT.LTHMC) THEN
            CALL UTMESS('F','NMTHMC_31','IL N Y A PAS DE LOI DE '//
     +                                                      'COUPLAGE')
         ENDIF
         IF (.NOT.LTHER) THEN
            CALL UTMESS('F','NMTHMC_32','IL N Y A PAS DE LOI DE '//
     +                                                     'THERMIQUE')
         ENDIF
         IF (.NOT.LHYDR) THEN
            CALL UTMESS('F','NMTHMC_33','IL N Y A PAS DE LOI '//
     +                                                   'HYDRAULIQUE')
         ENDIF
         IF (.NOT.LMECA) THEN
            CALL UTMESS('F','NMTHMC_34','IL N Y A PAS DE LOI DE '//
     +                                                     'MECANIQUE')
         ENDIF
         IF ( THMC.NE.'LIQU_SATU'     .AND.
     +        THMC.NE.'LIQU_SATU_GAT' .AND.
     +        THMC.NE.'LIQU_GAZ_ATM'  .AND.
     +        THMC.NE.'GAZ'                ) THEN
            CALL UTMESS('F','NMTHMC_35','LA LOI DE COUPLAGE EST '//
     +                         'INCORRECTE POUR UNE MODELISATION THM')
         ENDIF
         IF ( HYDR.EQ.'HYDR_ENDO'         .AND.
     +        ( MECA.NE.'MAZARS'          .AND.
     +          MECA.NE.'ENDO_ISOT_BETON'      ) ) THEN
            CALL UTMESS('F','NMTHMC_36','INCOMPATIBILITE DES '//
     +                       'COMPORTEMENTS MECANIQUE ET HYDRAULIQUE')
         ENDIF
         IF ( MECA.EQ.'SURF_ETAT_NSAT' ) THEN
            CALL UTMESS('F','NMTHMC_37','LOI DE MECANIQUE '//
     +                                 'INCOMPATIBLE AVEC UNE LOI THM')
         ENDIF
C =====================================================================
C --- PARTIE KIT_THHM -------------------------------------------------
C =====================================================================
      ELSE IF (COMP.EQ.'KIT_THHM') THEN
         IF (.NOT.LTHMC) THEN
            CALL UTMESS('F','NMTHMC_38','IL N Y A PAS DE LOI DE '//
     +                                                      'COUPLAGE')
         ENDIF
         IF (.NOT.LTHER) THEN
            CALL UTMESS('F','NMTHMC_39','IL N Y A PAS DE LOI DE '//
     +                                                     'THERMIQUE')
         ENDIF
         IF (.NOT.LHYDR) THEN
            CALL UTMESS('F','NMTHMC_40','IL N Y A PAS DE LOI '//
     +                                                   'HYDRAULIQUE')
         ENDIF
         IF (.NOT.LMECA) THEN
            CALL UTMESS('F','NMTHMC_41','IL N Y A PAS DE LOI DE '//
     +                                                     'MECANIQUE')
         ENDIF
         IF ( THMC.NE.'LIQU_VAPE_GAZ' .AND.
     +        THMC.NE.'LIQU_NSAT_GAT' .AND.
     +        THMC.NE.'LIQU_GAZ'           ) THEN
            CALL UTMESS('F','NMTHMC_42','LA LOI DE COUPLAGE EST '//
     +                         'INCORRECTE POUR UNE MODELISATION THHM')
         ENDIF
         IF ( HYDR.EQ.'HYDR_ENDO'         .AND.
     +        ( MECA.NE.'MAZARS'          .AND.
     +          MECA.NE.'ENDO_ISOT_BETON'      ) ) THEN
            CALL UTMESS('F','NMTHMC_43','INCOMPATIBILITE DES '//
     +                        'COMPORTEMENTS MECANIQUE ET HYDRAULIQUE')
         ENDIF
         IF ( MECA.EQ.'ELAS_THM' .OR.
     +        MECA.EQ.'SURF_ETAT_SATU' .OR.
     +        MECA.EQ.'CAM_CLAY_THM'        ) THEN
            CALL UTMESS('F','NMTHMC_44','LOI DE MECANIQUE '//
     +                                'INCOMPATIBLE AVEC UNE LOI THHM')
         ENDIF
      ENDIF
C =====================================================================
C --- MISE A JOUR FINALE ----------------------------------------------
C =====================================================================
      COMEL(1) = THMC
      COMEL(2) = THER
      COMEL(3) = HYDR
      COMEL(4) = MECA
C =====================================================================
      END
