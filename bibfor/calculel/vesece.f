      SUBROUTINE VESECE ( NOMCMD, OPTION, NOPASE, TYPESE, CODRET )
C
C     VERIFICATION DE LA SENSIBILITE POUR CALC_ELEM
C     **                 **               *    *
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/10/2002   AUTEUR ASSIRE A.ASSIRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C              SEE THE FILE "LICENSE.TERMS" FOR INFORMATION ON USAGE AND
C              REDISTRIBUTION OF THIS FILE.
C ======================================================================
C ----------------------------------------------------------------------
C IN  NOMCMD : NOM DE LA COMMANDE
C IN  OPTION : NOM DE L'OPTION A CONTROLER
C IN  NOPASE : NOM DU PARAMETRE SENSIBLE
C IN  TYPESE : TYPE DE SENSIBILITE
C               -1 : DERIVATION EULERIENNE (VIA UN CHAMP THETA)
C                0 : CALCUL STANDARD
C                1 : CALCUL INSENSIBLE
C                2 : DEPLACEMENT IMPOSE
C                3 : PARAMETRE MATERIAU
C                4 : CARACTERISTIQUE ELEMENTAIRE (COQUES, ...)
C               EN THERMIQUE (CF NTTYSE) :
C                5 : SOURCE
C                6 : FLUX IMPOSE
C                7 : TEMPERATURE EXTERIEURE
C                8 : COEFFICIENT D'ECHANGE
C               EN MECANIQUE (CF METYSE) :
C
C OUT CODRET : CODRET DE RETOUR (O, TOUT VA BIEN)
C                0 : TOUT VA BIEN
C                1 : CALCUL DERIVE NON DISPONIBLE POUR CETTE OPTION
C                2 : LE PARAMETRE SENSIBLE N'EST PAS UN CHAMP THETA ET
C                    ON VEUT DU LAGRANGIEN/EULERIEN
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       02/05/02 (OB): DEBLINDAGE DE FLUX_ELNO/ELGA_TEMP
C ----------------------------------------------------------------------

      IMPLICIT   NONE

C 0.1. ==> ARGUMENTS

      INTEGER TYPESE, CODRET

      CHARACTER*8 NOPASE
      CHARACTER*16 OPTION, NOMCMD 

C 0.2. ==> COMMUNS

C 0.3. ==> VARIABLES LOCALES

      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'VESECE' )

C====
C 1. ON PASSE EN REVUE TOUTES LES OPTIONS
C====

      CODRET = 0

      IF ( OPTION.EQ.'EPSG_ELNO_DEPL' .OR.
     >     OPTION.EQ.'EPSG_ELGA_DEPL' .OR.
     >     OPTION.EQ.'EPME_ELNO_DEPL' .OR.
     >     OPTION.EQ.'EPME_ELGA_DEPL' .OR.
     >     OPTION.EQ.'EPMG_ELNO_DEPL' .OR.
     >     OPTION.EQ.'EPMG_ELGA_DEPL' .OR.
     >     OPTION.EQ.'EFGE_ELNO_DEPL' .OR.
     >     OPTION.EQ.'EPOT_ELEM_DEPL' .OR.
     >     OPTION.EQ.'SIPO_ELNO_DEPL' .OR.
     >     OPTION.EQ.'SIRE_ELNO_DEPL' .OR.
     >     OPTION.EQ.'DEGE_ELNO_DEPL' .OR.
     >     OPTION.EQ.'SIGM_ELNO_SIEF' .OR.
     >     OPTION.EQ.'SIPO_ELNO_SIEF' .OR.
     >     OPTION.EQ.'VALE_NCOU_MAXI' .OR.
     >     OPTION.EQ.'CRIT_ELNO_RUPT' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF
C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'EPSI_ELNO_DEPL' .OR.
     >         OPTION.EQ.'EPSI_ELGA_DEPL' .OR.
     >         OPTION.EQ.'SIGM_ELNO_DEPL' .OR.
     >         OPTION.EQ.'SIEF_ELNO_ELGA' .OR.
     >         OPTION.EQ.'SIEF_ELGA_DEPL' ) THEN

        IF (TYPESE.EQ.4) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'SIEF_ELNO_ELGA' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'HYDR_ELNO_ELGA' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'ECIN_ELEM_DEPL' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'SIGM_NOZ1_ELGA' .OR.
     >         OPTION.EQ.'SIGM_NOZ2_ELGA' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'EPGR_ELNO' .OR.
     >         OPTION.EQ.'EPGR_ELGA' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'EPSP_ELNO' .OR.
     >         OPTION.EQ.'EPSP_ELGA' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'EQUI_ELGA_EPSI' .OR.
     >         OPTION.EQ.'EQUI_ELGA_EPME' .OR.
     >         OPTION.EQ.'EQUI_ELGA_SIGM' .OR.
     >         OPTION.EQ.'EQUI_ELNO_EPSI' .OR.
     >         OPTION.EQ.'EQUI_ELNO_EPME' .OR.
     >         OPTION.EQ.'PMPB_ELGA_SIEF' .OR.
     >         OPTION.EQ.'PMPB_ELNO_SIEF' .OR.
     >         OPTION.EQ.'EQUI_ELNO_SIGM' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'ERRE_ELEM_NOZ1' .OR.
     >         OPTION.EQ.'ERRE_ELEM_NOZ2' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'ERRE_ELGA_NORE' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'ERTH_ELEM_TEMP' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'ERTH_ELNO_ELEM' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'ERRE_ELNO_ELGA' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'SIGM_ELNO_CART' .OR.
     >         OPTION.EQ.'EFGE_ELNO_CART') THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'VNOR_ELEM_DEPL' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'FLUX_ELNO_TEMP' .OR.
     >         OPTION.EQ.'FLUX_ELGA_TEMP' ) THEN

        IF ( TYPESE.EQ.-1 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'PRES_ELNO_DBEL' .OR.
     >         OPTION.EQ.'PRES_DBEL_DEPL' .OR.
     >         OPTION.EQ.'PRES_ELNO_REEL' .OR.
     >         OPTION.EQ.'PRES_ELNO_IMAG' .OR.
     >         OPTION.EQ.'INTE_ELNO_ACTI' .OR.
     >         OPTION.EQ.'INTE_ELNO_REAC' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'DCHA_ELGA_SIGM' .OR.
     >         OPTION.EQ.'DCHA_ELNO_SIGM' .OR.
     >         OPTION.EQ.'RADI_ELGA_SIGM' .OR.
     >         OPTION.EQ.'RADI_ELNO_SIGM' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'ENDO_ELNO_SIGA' .OR.
     >         OPTION.EQ.'ENDO_ELNO_SINO' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C    ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'DURT_ELGA_META' .OR.
     >         OPTION.EQ.'DURT_ELNO_META' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'SIGM_ELNO_COQU' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF( OPTION.EQ.'SIGM_ELNO_TUYO' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'VARI_ELNO_ELGA' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION .EQ. 'VARI_ELNO_TUYO' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF ( OPTION .EQ. 'VARI_ELNO_COQU' ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C    ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'ENEL_ELGA'      .OR.
     >         OPTION.EQ.'ENEL_ELNO_ELGA' .OR.
     >         OPTION.EQ.'ETOT_ELGA'      .OR.
     >         OPTION.EQ.'ETOT_ELNO_ELGA' .OR.
     >         OPTION.EQ.'ETOT_ELEM'      ) THEN

        IF ( TYPESE.NE.0 ) THEN
          CODRET = 1
        ENDIF

C    ------------------------------------------------------------------
      ELSEIF ( OPTION.EQ.'DEDE_ELNO_DLDE' .OR.
     >         OPTION.EQ.'DETE_ELNO_DLTE' .OR.
     >         OPTION.EQ.'DESI_ELNO_DLSI' ) THEN

        IF ( TYPESE.NE.-1 ) THEN
          CODRET = 2
        ENDIF
C    ------------------------------------------------------------------
      ELSE
        CALL UTMESS ( 'A', NOMCMD, 'PROGRAMME '//NOMPRO )
        CALL UTMESS ( 'F', NOMCMD, 'OPTION INEXISTANTE : '//OPTION )
      ENDIF

C====
C 2. LES ERREURS
C====

      IF ( CODRET.NE.0 ) THEN

      CALL UTMESS ( 'A', NOMCMD, 'OPTION : '//OPTION )
      IF ( NOPASE.NE.' ' ) THEN
        CALL UTMESS ('A', NOMCMD, 'PARAMETRE SENSIBLE '//NOPASE)
      ENDIF

      IF ( CODRET.EQ.1 ) THEN

        CALL UTMESS ('A', NOMCMD, 'CALCUL NON DISPONIBLE' )

      ELSEIF ( CODRET.EQ.2 ) THEN

        CALL UTMESS ( 'A', NOMCMD,
     >  'LE PARAMETRE DE SENSIBILITE DOIT ETRE UN CHAMP THETA' )

      ENDIF

      ENDIF

      END
