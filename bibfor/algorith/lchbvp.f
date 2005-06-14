      SUBROUTINE LCHBVP(SIGD,VP,VECP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/06/2005   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
      IMPLICIT NONE
      REAL*8 SIGD(6),VP(3),VECP(3,3)
C =====================================================================
C --- HOEK-BROWN : VALEURS ET VECTEURS PROPRES DU DEVIATEUR DE SIGD ---
C --- UTILISE POUR L OPTION RIGI_MECA AFIN DE CALCULER LES VALEURS ----
C --- ET VECTEURS PROPRES DU DEVIATEUR ELASTIQUE ----------------------
C =====================================================================
C IN  : SIGD   :  TENSEUR DES CONTRAINTES (ELASTIQUE) -----------------
C OUT : VP     :  VALEURS PROPRES ORDONNEES DU DEVIATEUR DE SIGD ------
C --- : VECP   :  VECTEURS PROPRES DU DEVIATEUR DE SIGD ---------------
C =====================================================================
      REAL*8        SEB(6),DEUX,SE(6)
      CHARACTER*10  CVP1,CVP2,CVP3 
      INTEGER       NDT,NDI
C ======================================================================
      PARAMETER   (DEUX = 2.0D0)
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
      CALL LCDEVI(SIGD,SE)
      SEB(1) = SE(1)
      SEB(2) = SE(4)/SQRT(DEUX)
      SEB(4) = SE(2)
      SEB(6) = SE(3)
      IF (NDT.EQ.4) THEN
          SEB(3) = 0.0D0
          SEB(5) = 0.0D0
      ELSE
          SEB(3) = SE(5) / SQRT(DEUX)
          SEB(5) = SE(6) / SQRT(DEUX)
      ENDIF
      CALL HBDIAG(SEB,VECP,VP)
      IF ((VP(2).LT.VP(1)) .OR. (VP(3).LT.VP(2))) THEN
          CALL CODREE(VP(1),'E',CVP1)
          CALL CODREE(VP(2),'E',CVP2)
          CALL CODREE(VP(3),'E',CVP3)
          CALL UTMESS('F','LCHOBR','VALEURS PROPRES NON//
     &               ORDONNEES'//CVP1//CVP2//CVP3)
      ENDIF                 
C ======================================================================
      END
