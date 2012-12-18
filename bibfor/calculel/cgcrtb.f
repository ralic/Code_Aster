      SUBROUTINE CGCRTB(TABLE,OPTION,LMELAS,CAS,TYPFIS,LMODA,
     &                  NBPRUP,NOPRUP,TYPRUP)

      IMPLICIT NONE

      INTEGER       NBPRUP
      LOGICAL       LMELAS,LMODA
      CHARACTER*8   TABLE,TYPRUP(NBPRUP),TYPFIS
      CHARACTER*16  OPTION,CAS,NOPRUP(NBPRUP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 17/12/2012   AUTEUR DELMAS J.DELMAS 
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
C RESPONSABLE GENIAUT S.GENIAUT
C
C     SOUS-ROUTINE DE L'OPERATEUR CALC_G
C
C     BUT : CREATION DE LA TABLE ISSUE DE CALC_G
C           ET AFFECTATION DES PARAMETRES
C
C ----------------------------------------------
C  IN :
C     TABLE : NOM DE LA TABLE
C     OPTION : OPTION DE CALCUL
C     LMELAS : .TRUE.  SI TYPE SD RESULTAT = MULT_ELAS
C              .FALSE. SINON
C     CAS    : '2D', '3D_LOCAL'  OU '3D_GLOBAL'
C     TYPFIS : TYPE D'OBJET POUR DECRIRE LE FOND DE FISSURE
C              'FONDFISS' OU 'FISSURE' OU 'THETA'
C     LMODA  : .TRUE.  SI TYPE SD RESULTAT = MODE_MECA
C              .FALSE. SINON
C
C  OUT :
C     NBPRUP : NOMBRE DE PARAMETRES
C     NOPRUP : NOMS DES PARAMETRES
C     TYPRUP : TYPES DES PARAMETRES
C ----------------------------------------------
C
      IF((OPTION.EQ.'CALC_G'.AND.CAS.EQ.'2D'.AND.TYPFIS.NE.'FISSURE')
     &   .OR. (OPTION.EQ.'CALC_G_GLOB')) THEN
          NBPRUP = 3
          IF(LMELAS)THEN
            NOPRUP(1) = 'NUME_CAS'
            TYPRUP(1) = 'I'
            NOPRUP(2) = 'NOM_CAS'
            TYPRUP(2) = 'K16'
          ELSE
            NOPRUP(1) = 'NUME_ORDRE'
            TYPRUP(1) = 'I'
            NOPRUP(2) = 'INST'
            TYPRUP(2) = 'R'
          ENDIF
          NOPRUP(3) = 'G'
          TYPRUP(3) = 'R'
      ELSEIF(OPTION.EQ.'CALC_G'.AND.CAS.EQ.'3D_LOCAL'.AND.
     &       TYPFIS.EQ.'FISSURE')THEN
          NBPRUP = 6
          NOPRUP(1) = 'NUME_FOND'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'NUME_ORDRE'
          TYPRUP(2) = 'I'
          NOPRUP(3) = 'INST'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'NUM_PT'
          TYPRUP(4) = 'I'
          NOPRUP(5) = 'ABSC_CURV'
          TYPRUP(5) = 'R'
          NOPRUP(6) = 'G'
          TYPRUP(6) = 'R'
      ELSEIF(OPTION.EQ.'CALC_G'.AND.CAS.EQ.'2D'.AND.
     &       TYPFIS.EQ.'FISSURE')THEN
          NBPRUP = 4
          NOPRUP(1) = 'NUME_FOND'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'NUME_ORDRE'
          TYPRUP(2) = 'I'
          NOPRUP(3) = 'INST'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'G'
          TYPRUP(4) = 'R'
      ELSEIF(OPTION.EQ.'CALC_G'.AND.CAS.EQ.'3D_LOCAL'.AND.
     &       TYPFIS.NE.'FISSURE') THEN
          NBPRUP = 5
          IF(LMELAS)THEN
            NOPRUP(1) = 'NUME_CAS'
            TYPRUP(1) = 'I'
            NOPRUP(2) = 'NOM_CAS'
            TYPRUP(2) = 'K16'
          ELSE
            NOPRUP(1) = 'NUME_ORDRE'
            TYPRUP(1) = 'I'
            NOPRUP(2) = 'INST'
            TYPRUP(2) = 'R'
          ENDIF
          NOPRUP(3) = 'NOEUD'
          TYPRUP(3) = 'K8'
          NOPRUP(4) = 'ABSC_CURV'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'G'
          TYPRUP(5) = 'R'
      ELSE IF (OPTION.EQ.'CALC_K_G'.AND.CAS.EQ.'2D'.AND.
     &         TYPFIS.NE.'FISSURE'.AND..NOT. LMODA) THEN
          NBPRUP = 6
          IF(LMELAS)THEN
            NOPRUP(1) = 'NUME_CAS'
            TYPRUP(1) = 'I'
            NOPRUP(2) = 'NOM_CAS'
            TYPRUP(2) = 'K16'
          ELSE
            NOPRUP(1) = 'NUME_ORDRE'
            TYPRUP(1) = 'I'
            NOPRUP(2) = 'INST'
            TYPRUP(2) = 'R'
          ENDIF
          NOPRUP(3) = 'G'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'K1'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'K2'
          TYPRUP(5) = 'R'
          NOPRUP(6) = 'G_IRWIN'
          TYPRUP(6) = 'R'
      ELSE IF(OPTION.EQ.'CALC_K_G'.AND.CAS.EQ.'2D'.AND.
     &        TYPFIS.EQ.'FISSURE'.AND..NOT. LMODA) THEN
          NBPRUP = 7
          NOPRUP(1) = 'NUME_FOND'
          TYPRUP(1) = 'I'
          IF(LMELAS)THEN
            NOPRUP(2) = 'NUME_CAS'
            TYPRUP(2) = 'I'
            NOPRUP(3) = 'NOM_CAS'
            TYPRUP(3) = 'K16'
          ELSE
            NOPRUP(2) = 'NUME_ORDRE'
            TYPRUP(2) = 'I'
            NOPRUP(3) = 'INST'
            TYPRUP(3) = 'R'
          ENDIF
          NOPRUP(4) = 'G'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'K1'
          TYPRUP(5) = 'R'
          NOPRUP(6) = 'K2'
          TYPRUP(6) = 'R'
          NOPRUP(7) = 'G_IRWIN'
          TYPRUP(7) = 'R'
      ELSEIF((OPTION.EQ.'CALC_K_G'.OR.OPTION.EQ.'CALC_K_MAX')
     &        .AND.(CAS.EQ.'3D_LOCAL').AND.(.NOT.LMODA)) THEN
          NBPRUP = 11
          NOPRUP(1) = 'NUME_FOND'
          TYPRUP(1) = 'I'
          IF(LMELAS)THEN
            NOPRUP(2) = 'NUME_CAS'
            TYPRUP(2) = 'I'
            NOPRUP(3) = 'NOM_CAS'
            TYPRUP(3) = 'K16'
          ELSE
            NOPRUP(2) = 'NUME_ORDRE'
            TYPRUP(2) = 'I'
            NOPRUP(3) = 'INST'
            TYPRUP(3) = 'R'
          ENDIF
          NOPRUP(4) = 'NUM_PT'
          TYPRUP(4) = 'I'
          NOPRUP(5) = 'ABSC_CURV'
          TYPRUP(5) = 'R'
          NOPRUP(6) = 'K1'
          TYPRUP(6) = 'R'
          NOPRUP(7) = 'K2'
          TYPRUP(7) = 'R'
          NOPRUP(8) = 'K3'
          TYPRUP(8) = 'R'
          NOPRUP(9) = 'G'
          TYPRUP(9) = 'R'
          NOPRUP(10) = 'BETA'
          TYPRUP(10) = 'R'
          NOPRUP(11) = 'G_IRWIN'
          TYPRUP(11) = 'R'
      ELSEIF ( OPTION .EQ. 'CALC_K_G' .AND. LMODA) THEN
        IF(CAS.EQ.'3D_LOCAL')THEN
          NBPRUP = 9
          NOPRUP(1) = 'NUME_MODE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'NUM_PT'
          TYPRUP(2) = 'I'
          NOPRUP(3) = 'ABSC_CURV'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'K1'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'K2'
          TYPRUP(5) = 'R'
          NOPRUP(6) = 'K3'
          TYPRUP(6) = 'R'
          NOPRUP(7) = 'G'
          TYPRUP(7) = 'R'
          NOPRUP(8) = 'BETA'
          TYPRUP(8) = 'R'
          NOPRUP(9) = 'G_IRWIN'
          TYPRUP(9) = 'R'
        ELSE
          NBPRUP = 5
          NOPRUP(1) = 'NUME_MODE'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'G'
          TYPRUP(2) = 'R'
          NOPRUP(3) = 'K1'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'K2'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'G_IRWIN'
          TYPRUP(5) = 'R'
        ENDIF
      ELSEIF ( OPTION .EQ. 'G_BILI'
     &     .OR.OPTION .EQ. 'G_MAX') THEN
          NBPRUP = 6
          IF(LMELAS)THEN
            NOPRUP(1) = 'NOM_CAS'
            TYPRUP(1) = 'K16'
          ELSE
            NOPRUP(1) = 'INST'
            TYPRUP(1) = 'R'
          ENDIF
          NOPRUP(2) = 'NUME_CMP_I'
          TYPRUP(2) = 'I'
          NOPRUP(3) = 'NUME_CMP_J'
          TYPRUP(3) = 'I'
          NOPRUP(4) = 'NOEUD'
          TYPRUP(4) = 'K8'
          NOPRUP(5) = 'ABSC_CURV'
          TYPRUP(5) = 'R'
          NOPRUP(6) = 'G_BILI_LOCAL'
          TYPRUP(6) = 'R'
      ELSEIF ( OPTION .EQ. 'G_BILI_GLOB'
     &     .OR.OPTION .EQ. 'G_MAX_GLOB') THEN
          NBPRUP = 3
          NOPRUP(1) = 'NUME_CMP_I'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'NUME_CMP_J'
          TYPRUP(2) = 'I'
          NOPRUP(3) = 'G_BILIN'
          TYPRUP(3) = 'R'
      ENDIF
      CALL TBCRSD ( TABLE, 'G' )
      CALL TBAJPA ( TABLE, NBPRUP, NOPRUP, TYPRUP )
C
      END
