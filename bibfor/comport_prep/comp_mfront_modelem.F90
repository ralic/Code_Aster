subroutine comp_mfront_modelem(elem_type_name, l_mfront_cp ,&
                               model_dim     , model_mfront,&
                               codret        , type_cpla_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/teattr.h"
#include "asterfort/utmess.h"
!
! ======================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: elem_type_name
    aster_logical, intent(in) :: l_mfront_cp
    integer, intent(out) :: model_dim
    character(len=16), intent(out) :: model_mfront
    integer, intent(out) :: codret
    character(len=16), optional, intent(out) :: type_cpla_
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Select type of modelisation for MFront - On selected element
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_type_name   : type of finite element
! In  l_mfront_cp      : .true. if plane stress is possible for this MFront behaviour
! Out model_dim        : dimension of model 2 or 3
! Out model_mfront     : type of modelisation for MFront
! Out codret           : code for error 
!                        0 - OK
!                        1 - Error - Not same finite element
!                        2 - Error - No MFront modelisation allowed on this element
! Out type_cpla        : stress plane hypothesis (for Deborst)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret
    character(len=1) :: model_dim_s
    character(len=16) :: principal, model_thm, model_type, type_cpla
!
! --------------------------------------------------------------------------------------------------
!
    codret       = 0
    model_dim    = 0
    model_mfront = ' '
    type_cpla    = 'VIDE'
!
! - Get attributes on finite element
!
    call teattr('C', 'TYPMOD'         , model_type , iret, typel = elem_type_name)
    call teattr('C', 'PRINCIPAL'      , principal  , iret, typel = elem_type_name)
    call teattr('C', 'THM'            , model_thm  , iret, typel = elem_type_name)
    call teattr('C', 'DIM_TOPO_MODELI', model_dim_s, iret, typel = elem_type_name)
    read(model_dim_s,'(I1)') model_dim
!
! - Select modelisation for MFront
!
    if (principal .eq. 'OUI') then
        if ( model_type .eq. 'COMP3D' ) then
            model_mfront = '_Tridimensional'
        elseif ( model_type .eq. 'C_PLAN' ) then
! Deborst algorithm
            if (l_mfront_cp) then
                model_mfront = '_PlaneStress'
                type_cpla    = 'ANALYTIQUE'
            else
                model_mfront = '_Axisymmetrical'
                model_dim    = 2
                type_cpla    = 'DEBORST'
            endif
        elseif ( model_type .eq. 'D_PLAN' ) then
            model_mfront = '_PlaneStrain'
        elseif ( model_type .eq. 'AXIS' ) then
            model_mfront = '_Axisymmetrical'
        elseif ( model_type .eq. 'COMP1D' ) then
! Deborst algorithm
            model_mfront = '_Axisymmetrical'
            model_dim    = 2
            type_cpla    = 'DEBORST'
        elseif ( model_thm .eq. 'OUI' ) then
            model_mfront = '_Tridimensional'
        else
            model_mfront = model_type
            codret = 2
        endif
    endif
!
    if (model_dim .le. 1) then
        codret = 2
    endif
!
    if (present(type_cpla_)) then
        type_cpla_ = type_cpla
    endif
!
end subroutine
