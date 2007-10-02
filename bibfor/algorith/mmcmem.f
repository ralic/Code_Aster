      SUBROUTINE MMCMEM(OPTIOZ,
     &                  NOMA  ,MODELE,DEFICO,RESOCO,DEPMOI,
     &                  DEPDEL,VITMOI,VITPLU,ACCMOI)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/10/2007   AUTEUR KHAM M.KHAM 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*(*) OPTIOZ
      CHARACTER*8   NOMA
      CHARACTER*24  MODELE,RESOCO,DEFICO
      CHARACTER*24  DEPMOI,DEPDEL,ACCMOI,VITMOI,VITPLU
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
C
C CALCUL DES MATRICES ELEMENTAIRES ET DES SECONDS MEMBRES
C DES ELEMENTS DE CONTACT/FROTTEMENT METHODE CONTINUE
C      
C ----------------------------------------------------------------------
C 
C
C IN  OPTION : OPTION DE CALCUL
C               'PREDICTION'
C               'CORRECTION'
C IN  NOMA   : NOM DU MAILLAGE
C IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
C IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
C IN  DEPMOI : CHAM_NO DES DEPLACEMENTS A L'INSTANT PRECEDENT
C IN  MODELE : NOM DU MODELE
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C IN  ACCMOI : CHAM_NO DES ACCELERATIONS A L'INSTANT PRECEDENT
C IN  VITMOI : CHAM_NO DES VITESSES A L'INSTANT PRECEDENT
C IN  VITPLU : CHAM_NO DES VITESSES A L'INSTANT SUIVANT
C
C ----------------------------------------------------------------------
C
      CHARACTER*16 OPTION
C
C ----------------------------------------------------------------------
C
      OPTION = OPTIOZ
C
      IF (OPTION.EQ.'PREDICTION') THEN
        CALL MMCMAT(MODELE,RESOCO,DEPMOI,DEPDEL,VITMOI,
     &              VITPLU,ACCMOI) 
        CALL MMCVEC(MODELE,RESOCO,DEPMOI,DEPDEL,VITMOI,
     &              VITPLU,ACCMOI) 
      ELSEIF (OPTION.EQ.'CORRECTION') THEN
        CALL MAJUSU(NOMA  ,DEFICO,RESOCO,DEPMOI,DEPDEL)
        CALL MMCMAT(MODELE,RESOCO,DEPMOI,DEPDEL,VITMOI,
     &              VITPLU,ACCMOI) 
        CALL MMCVEC(MODELE,RESOCO,DEPMOI,DEPDEL,VITMOI,
     &              VITPLU,ACCMOI)      
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
