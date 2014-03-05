subroutine char_crea_neum(load, ligrmo, mesh, ndim, vale_type)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/cachre.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: load
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: ndim
    character(len=19), intent(in) :: ligrmo
    character(len=4), intent(in) :: vale_type
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Create Neumann loads
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh      : name of mesh
! In  load      : name of load
! In  ndim      : space dimension
! In  ligrmo    : list of elements in model
! In  vale_type : affected value type (real, complex or function)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: max_load_type
    parameter (max_load_type = 6)
    integer :: nbocc(max_load_type)
    character(len=5) :: param(max_load_type)
    character(len=16) :: keywordfact(max_load_type)
!
    integer :: i
    character(len=5) :: curr_para
    data keywordfact / 'FORCE_CONTOUR' , 'FORCE_INTERNE' , 'FORCE_ARETE' ,&
     &                 'FORCE_FACE'    , 'FORCE_POUTRE'  , 'FORCE_COQUE'  /
    data param       / 'F1D2D'         , ' '             , 'F1D3D'       ,&
     &                 'F2D3D'         , 'F1D1D'         , ' '            /
!
! --------------------------------------------------------------------------------------------------
!
!
!
! - Number of factor keywords
!
    do i = 1, max_load_type
        nbocc(i) = 0
        if (getexm(keywordfact(i),' ') .eq. 1) then
            call getfac(keywordfact(i), nbocc(i))
        endif
    end do
!
! - Some checks: FORCE_FACE and FORCE_POUTRE prohibited en 2D
!
    if (ndim .eq. 2) then
        if (nbocc(4) .ne. 0) then
            call utmess('F', 'CHARGES2_5', sk=keywordfact(4))
        endif
        if (nbocc(5) .ne. 0) then
            call utmess('F', 'CHARGES2_5', sk=keywordfact(5))
        endif
    endif
!
! - Load affectation
!
    do i = 1, max_load_type
        if (nbocc(i) .ne. 0) then
            curr_para = param(i)
! --------- FORCE_INTERNE#2D
            if (keywordfact(i) .eq. 'FORCE_INTERNE' .and. ndim .eq. 2) curr_para = 'F2D2D'
! --------- FORCE_INTERNE#3D
            if (keywordfact(i) .eq. 'FORCE_INTERNE' .and. ndim .eq. 3) curr_para = 'F3D3D'
! --------- FORCE_COQUE#2D
            if (keywordfact(i) .eq. 'FORCE_COQUE' .and. ndim .eq. 2) curr_para = 'FCO2D'
! --------- FORCE_COQUE#3D
            if (keywordfact(i) .eq. 'FORCE_COQUE' .and. ndim .eq. 3) curr_para = 'FCO3D'
!
            call cachre(load, ligrmo, mesh, ndim, vale_type,&
                        curr_para, keywordfact(i))
        endif
    end do
!
end subroutine
