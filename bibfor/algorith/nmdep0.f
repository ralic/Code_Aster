      SUBROUTINE NMDEP0(OPER  ,SOLALG)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/04/2013   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*3  OPER
      CHARACTER*19 SOLALG(*)
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
C
C MISE A ZERO DE DEPDEL
C
C ----------------------------------------------------------------------
C
C
C IN  OPER   : TYPE OPERATION
C               ON  - ON MET &&CNPART.ZERO
C               OFF - ON MET DEPDEL
C I/O SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
C      
C ----------------------------------------------------------------------
C
      CHARACTER*19 DEPDE0,DEPDEL
C      
C ----------------------------------------------------------------------
C 
      IF (OPER.EQ.'ON ') THEN
        DEPDE0 = '&&CNPART.ZERO' 
        CALL NMCHSO(SOLALG,'SOLALG','DEPDEL',DEPDE0,SOLALG)
      ELSEIF   (OPER.EQ.'OFF') THEN 
        DEPDEL = '&&NMCH2P.DEPDEL'
        CALL NMCHSO(SOLALG,'SOLALG','DEPDEL',DEPDEL,SOLALG)        
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      END
