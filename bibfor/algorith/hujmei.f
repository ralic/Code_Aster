        SUBROUTINE HUJMEI (MATER, VIN, SIG)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2007   AUTEUR KHAM M.KHAM 
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
        INTEGER       NDT, NDI, I
        REAL*8        I1, BETA, D, PC0, PC, UN 
        REAL*8        EPSVP, SIG(6), VIN(*)
        REAL*8        MATER(22,2), D13, ZERO
C ----------------------------------------------------------------------
        COMMON /TDIM/   NDT, NDI
C ----------------------------------------------------------------------
        DATA      D13, ZERO, UN  /0.333333333334D0, 0.D0, 1.D0/
        
C ==================================================================
C --- VARIABLES INTERNES -------------------------------------------
C ==================================================================
       
        EPSVP = VIN(23) 
C ==================================================================
C --- CARACTERISTIQUES MATERIAU ------------------------------------
C ==================================================================
        
        D      = MATER(3,2)
        PC0    = MATER(7,2)
        BETA   = MATER(2,2)        
        PC     = PC0*EXP(-BETA*EPSVP)
        
C ==================================================================
C --- PRESSION ISOTROPE DU MATERIAU A T ----------------------------
C ==================================================================

        I1 = ZERO
        DO 10 I = 1, NDI
          I1 = I1 + D13*SIG(I)
 10     CONTINUE
C ==================================================================
C --- MISE A JOUR DES VARIABLES INTERNES DE MEMOIRE ----------------
C ==================================================================

        IF(VIN(22).EQ.ZERO)THEN
          VIN(22) = UN
        ELSE
          VIN(22) = - VIN(22)
        ENDIF  
        IF (VIN(21) .EQ. ZERO) THEN
          VIN(21) = VIN(4)
        ELSE
          VIN(21)= I1/(D*PC)
        ENDIF
        
        END        
