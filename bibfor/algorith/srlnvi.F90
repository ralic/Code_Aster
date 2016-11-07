subroutine srlnvi(mod, ndt, ndi, nvi)

!
! ===================================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG             
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY               
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY               
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR                  
! (AT YOUR OPTION) ANY LATER VERSION.                                                
!                                                                                    
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT                
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF                         
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU                   
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.                                           
!                                                                                    
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE                  
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,                      
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.                      
! ===================================================================================

!!!
!!! MODELE LKR : RECUPERATION DE NDT, NDI ET NVI
!!!

! ===================================================================================
! IN  : MOD : TYPE DE MODELISATION
! OUT : NDT : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR
!     : NDI : NOMBRE DE COMPOSANTES DIRECTES DU TENSEUR
!     : NVI : NB DE VARIABLES INTERNES
! ===================================================================================

    implicit none

#include "asterfort/utmess.h"

    !!!
    !!! Variables globales
    !!!
    
    integer :: ndt, ndi, nvi
    character(len=8) :: mod
    
    !!!
    !!! Modelisation 3d
    !!!
    
    if (mod(1:2) .eq. '3D') then
        ndt = 6
        ndi = 3
    
    !!!
    !!! Modelisation d_plan, axi ou c_plan
    !!!
    
    else if (mod(1:6).eq.'D_PLAN'.or. mod(1:4).eq.'AXIS' .or.&
             mod(1:6).eq.'C_PLAN' ) then
        ndt = 4
        ndi = 3
    
    !!!
    !!! Modelisation 1d non autorisee
    !!!
    
    else if (mod(1:2).eq.'1D') then
        call utmess('F', 'ALGORITH4_45')
    
    !!!
    !!! Modelisation inconnue
    !!!
    
    else
        call utmess('F', 'ALGORITH2_20')
    endif
    
    !!!
    !!! Nombre de variables internes
    !!!
    
    nvi = 12

end subroutine
