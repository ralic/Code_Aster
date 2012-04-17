      SUBROUTINE CCCHCI(CRITZ, QUESTZ, REPI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT NONE
      CHARACTER*(*) CRITZ, QUESTZ
      INTEGER      REPI
C RESPONSABLE COURTOIS M.COURTOIS
C ----------------------------------------------------------------------
C  CALC_CHAMP - TRAITEMENT DE CHAM_UTIL - CRITERE INFO
C  -    -                     --          -       -
C  RETOURNE DES INFOS SUR LE CRITERE CALCULE.
C  QUESTION :
C     'NBCMP' : RETOURNE LE NOMBRE DE COMPOSANTE A REMPLIR
C ----------------------------------------------------------------------
C IN  :
C   CRIT   K16  NOM DU CRITERE A CALCULER
C   QUEST  K16  QUESTION
C OUT :
C   REPI   I    VALEUR ENTIERE RETOURNEE
C ----------------------------------------------------------------------
      CHARACTER*5  QUEST
      CHARACTER*16 CRIT
C     ----- FIN  DECLARATIONS ------------------------------------------
      CRIT = CRITZ
      QUEST = QUESTZ
C
      IF (QUEST .EQ. 'NBCMP') THEN
        IF (CRIT.EQ.'VMIS' .OR. CRIT.EQ.'INVA_2' .OR.
     &      CRIT.EQ.'TRACE') THEN
          REPI = 1
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSE
C       QUESTION INVALIDE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
