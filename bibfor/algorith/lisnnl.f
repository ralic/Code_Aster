      SUBROUTINE LISNNL(PHENOZ,CHARGE,PREFOB)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2012   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT      NONE
      CHARACTER*(*) PHENOZ
      CHARACTER*8   CHARGE
      CHARACTER*13  PREFOB
C
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE (LISTE_CHARGES)
C
C NOM DU PREFIXE DE L'OBJET DE LA CHARGE
C
C ----------------------------------------------------------------------
C
C IN  PHENOM : TYPE DE PHENOMENE (MECANIQUE, THERMIQUE, ACOUSTIQUE)
C IN  CHARGE : NOM DE LA CHARGE (AFFE_CHAR_*)
C OUT PREFOB : PREFIXE DE L'OBJET DE LA CHARGE
C
C ----------------------------------------------------------------------
C
      CHARACTER*16 PHENOM
C
C ----------------------------------------------------------------------
C
      PHENOM = PHENOZ
      IF (PHENOM.EQ.'MECANIQUE') THEN
        PREFOB = CHARGE(1:8)//'.CHME'
      ELSEIF (PHENOM.EQ.'THERMIQUE') THEN
        PREFOB = CHARGE(1:8)//'.CHTH'
      ELSEIF (PHENOM.EQ.'ACOUSTIQUE') THEN
        PREFOB = CHARGE(1:8)//'.CHAC'
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      END
