subroutine comp_mfront_modelem(elem_type_name, l_mfront_cp     ,&
                               model_dim     , model_mfront    ,&
                               l_check_      , model_type_save_, codret_)
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
    aster_logical, optional, intent(in) :: l_check_
    character(len=16), optional, intent(inout) :: model_type_save_
    integer, optional, intent(out) :: codret_
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
! In  l_check          : .true. for check
! IO  model_type_save  : previous modelisation of finite element
! Out codret           : code for error 
!                        0 - OK
!                        1 - Error - Not same finite element
!                        2 - Error - No MFront modelisation allowed on this element
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, codret
    character(len=1) :: d1
    character(len=16) :: model_type, model_type_save
    character(len=16) :: principal, model_thm
    aster_logical :: l_check
!
! --------------------------------------------------------------------------------------------------
!
    codret          = 0
    model_dim       = 0
    model_mfront    = ' '
!
! - Activation of checks
!
    l_check = .false._1
    if (present(l_check_)) then
        l_check         = l_check_
        model_type_save = model_type_save_
    endif
!
! - Get attributes on finite element
!
    call teattr('C', 'TYPMOD'         , model_type, iret, typel = elem_type_name)
    call teattr('C', 'PRINCIPAL'      , principal , iret, typel = elem_type_name)
    call teattr('C', 'THM'            , model_thm , iret, typel = elem_type_name)
    call teattr('C', 'DIM_TOPO_MODELI', d1        , iret, typel = elem_type_name)
    read(d1,'(I1)') model_dim
!
! - Select modelisation for MFront
!
    if (principal .eq. 'OUI') then
        if (l_check) then
            if (model_type_save .eq. ' ') then
                model_type_save = model_type
            endif
            if (model_type_save .ne. model_type) then
                model_mfront = model_type
                codret = 1
                goto 99
            endif
        endif
        if ( model_type .eq. 'COMP3D' ) then
            model_mfront = '_Tridimensional'
        elseif ( model_type .eq. 'C_PLAN' ) then
! Deborst algorithm
            if (l_mfront_cp) then
                model_mfront = '_PlaneStress'
            else
                model_mfront = '_Axisymmetrical'
                model_dim    = 2
            endif
        elseif ( model_type .eq. 'D_PLAN' ) then
            model_mfront = '_PlaneStrain'
        elseif ( model_type .eq. 'AXIS' ) then
            model_mfront = '_Axisymmetrical'
        elseif ( model_type .eq. 'COMP1D' ) then
! Deborst algorithm
            model_mfront = '_Axisymmetrical'
            model_dim    = 2
        elseif ( model_thm .eq. 'OUI' ) then
            model_mfront = '_Tridimensional'
        else
            model_mfront = model_type
            if (l_check) then
                codret = 2
            else
                ASSERT(.false.)
            endif
        endif
    endif
!
99  continue
!
    if (present(l_check_)) then
        codret_          = codret
        model_type_save_ = model_type_save
    endif
!
end subroutine
