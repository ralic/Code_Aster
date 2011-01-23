      SUBROUTINE CGCRTB(LATABL,OPTIO1,DIME,LMELAS,TROIDL,NBPRUP,
     &                  NOPRUP,TYPRUP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 25/01/2011   AUTEUR MACOCCO K.MACOCCO 
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
C ----------------------------------------------
C      BUT: CREATION DE LA TABLE ISSUE DE CALC_G 
C           ET AFFECTATION DES PARAMETRES
C           
C      APPELE PAR: OP0100 (OPERATEUR 'CALC_G')
C ----------------------------------------------
C     IN  LATABL    :  NOM DE LA TABLE
C     IN  OPTIO1    :  OPTION DE CALCUL
C     IN  DIME      :  DIMENSION GEOMETRIQUE
C     IN  LMELAS    :  = .TRUE.  SI TYPE SD RESULTAT = MULT_ELAS
C                        .FALSE. SINON
C     IN  TROIDL    :  = .TRUE.  SI 3D LOCAL
C                        .FALSE. SI 2D OU 3D GLOBAL
C     IN/OUT NBPRUP :  NOMBRE DE PARAMETRES
C     OUT NOPRUP    :  NOMS DES PARAMETRES
C     OUT TYPRUP    :  TYPES DES PARAMETRES
C
      IMPLICIT NONE
      INTEGER       NBPRUP,DIME
      CHARACTER*8   LATABL,TYPRUP(NBPRUP)
      CHARACTER*16  OPTIO1,NOPRUP(NBPRUP)
      LOGICAL       TROIDL,LMELAS
C
      CALL JEMARQ()
C
      IF((  (OPTIO1.EQ.'CALC_G'.OR.
     &       OPTIO1.EQ.'CALC_DG') .AND. DIME.EQ.2)
     &  .OR. (OPTIO1.EQ.'CALC_G_GLOB')) THEN
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
      ELSEIF((OPTIO1.EQ.'CALC_G_X').AND. TROIDL)THEN
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
      ELSEIF((OPTIO1.EQ.'CALC_G_X').AND. DIME.EQ.2)THEN
          NBPRUP = 4
          NOPRUP(1) = 'NUME_FOND'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'NUME_ORDRE'
          TYPRUP(2) = 'I'
          NOPRUP(3) = 'INST'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'G'
          TYPRUP(4) = 'R'   
      ELSEIF((OPTIO1.EQ.'CALC_G').AND. TROIDL)THEN
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
      ELSEIF (OPTIO1.EQ.'CALC_DG_E'
     &   .OR. OPTIO1.EQ.'CALC_DGG_E') THEN
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
          NOPRUP(3) = 'DG/DE'
          TYPRUP(3) = 'R'
      ELSEIF (OPTIO1.EQ.'CALC_DG_FORC'
     &   .OR. OPTIO1.EQ.'CALC_DGG_FORC')THEN
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
          NOPRUP(3) = 'DG/DF'
          TYPRUP(3) = 'R'
      ELSEIF (OPTIO1 .EQ.'CALC_DK_DG_E')THEN
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
          NOPRUP(3) = 'DG/DE'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'DK1/DE'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'DK2/DE'
          TYPRUP(5) = 'R'
      ELSEIF (OPTIO1.EQ.'CALC_DK_DG_FORC')THEN
          NBPRUP = 4
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
          NOPRUP(3) = 'DK1/DF'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'DK2/DF'
          TYPRUP(4) = 'R'
      ELSE IF( OPTIO1.EQ.'CALC_K_G' .AND. DIME.EQ.2)THEN
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
      ELSE IF( OPTIO1.EQ.'CALC_K_X' .AND. DIME.EQ.2)THEN
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
      ELSEIF(OPTIO1(1:6).EQ.'CALC_K' .AND. TROIDL)THEN
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
      ELSEIF ( OPTIO1 .EQ. 'CALC_DK_DG_E' ) THEN
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
          NOPRUP(3) = 'DG/DE'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'DK1/DE'
          TYPRUP(4) = 'R'
          NOPRUP(5) = 'DK2/DE'
          TYPRUP(5) = 'R'
      ELSEIF ( OPTIO1 .EQ. 'CALC_DK_DG_FORC' ) THEN   
          NBPRUP = 4
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
          NOPRUP(3) = 'DK1/DF'
          TYPRUP(3) = 'R'
          NOPRUP(4) = 'DK2/DF'
          TYPRUP(4) = 'R'
      ELSEIF ( OPTIO1 .EQ. 'K_G_MODA' ) THEN
        IF(TROIDL)THEN
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
      ELSEIF ( OPTIO1 .EQ. 'G_BILI'
     &     .OR.OPTIO1 .EQ. 'G_MAX') THEN
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
      ELSEIF ( OPTIO1 .EQ. 'G_BILI_GLOB'
     &     .OR.OPTIO1 .EQ. 'G_MAX_GLOB') THEN
          NBPRUP = 3
          NOPRUP(1) = 'NUME_CMP_I'
          TYPRUP(1) = 'I'
          NOPRUP(2) = 'NUME_CMP_J'
          TYPRUP(2) = 'I'
          NOPRUP(3) = 'G_BILIN'
          TYPRUP(3) = 'R'
      ENDIF
      CALL TBCRSD ( LATABL, 'G' )
      CALL TBAJPA ( LATABL, NBPRUP, NOPRUP, TYPRUP )
C
      CALL JEDEMA()
C
      END
