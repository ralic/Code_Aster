      SUBROUTINE RANDOM (GRAINE)
! ----------------------------------------------------------------------
! ENGENDRE UN REEL PSEUDO-ALEATOIRE - ENROBE L'APPEL A RANDOM_NUMBER
!
! IN:OUT  GRAINE  : REAL*8 SERVANT DE GRAINE
!
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF RANDOM UTILITAI  DATE 15/03/2010   AUTEUR LEFEBVRE J-P.LEFEBVRE 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2010  EDF R&D              WWW.CODE-ASTER.ORG 
!                                                                    
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE 
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           
!                                                                    
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     
! ================================================================== 
! ================================================================== 
      IMPLICIT NONE
      REAL*8             GRAINE
!
      CALL RANDOM_NUMBER(GRAINE)

      END
