        SUBROUTINE HUJMEI (VIN)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/11/2007   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   ------------------------------------------------------------------
C   ENREGISTREMENT DES VARIABLES MEMOIRE POUR LE MECANISME ISOTROPE
C   IN  MATER  :  COEFFICIENTS MATERIAU A T+DT
C       VIN    :  VARIABLES INTERNES  
C       SIG    :  CONTRAINTE 
C
C   OUT VIN    :  VARIABLES INTERNES MODIFIEES 
C   ------------------------------------------------------------------
        REAL*8        UN, ZERO, VIN(*)
C ----------------------------------------------------------------------
        DATA      ZERO, UN  /0.D0, 1.D0/
        
C ==================================================================
C --- MISE A JOUR DES VARIABLES INTERNES DE MEMOIRE ----------------
C ==================================================================

        IF (VIN(21) .EQ. ZERO) THEN
          VIN(21) = VIN(4)
        ELSE
          VIN(21)= VIN(21)-VIN(22)*VIN(8)
        ENDIF

        IF(VIN(22).EQ.ZERO)THEN
          VIN(22) = UN
        ELSE
          VIN(22) = - VIN(22)
        ENDIF  
       
        END        
