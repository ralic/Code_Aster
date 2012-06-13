      SUBROUTINE LSPINI(SOLVEU)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*19 SOLVEU
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C ----------------------------------------------------------
C  BUT : REINITIALISATION DU PRECONDITIONNEUR LDLT_SP POUR 
C        PETSC OU GCPC
C
C IN K19 SOLVEU  : NOM DU SOLVEUR DONNE EN ENTREE
C ----------------------------------------------------------




      INTEGER      JSLVK,JSLVI
      CHARACTER*8  PRECON

C------------------------------------------------------------------
      CALL JEMARQ()

C --- LECTURES PARAMETRES DU SOLVEUR
      CALL JEVEUO(SOLVEU//'.SLVK','L',JSLVK)
      PRECON = ZK24(JSLVK-1+2) 

C --- REMISE A ZERO
      IF (PRECON.EQ.'LDLT_SP') THEN 
        CALL JEVEUO(SOLVEU//'.SLVI','E',JSLVI)  
        ZI(JSLVI-1+5)=0
      ENDIF

      CALL JEDEMA()
      END
