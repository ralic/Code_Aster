subroutine romMultiParaRead(ds_multipara)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvc8.h"
#include "asterfort/infniv.h"
#include "asterfort/jelira.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(ROM_DS_MultiPara), intent(inout) :: ds_multipara
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Read data for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_multipara     : datastructure for multiparametric problems
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
!    character(len=1)  :: matr_type, vect_type, ktyp, matr_elem_type
!    character(len=16) :: keywfact
!    character(len=8) :: matr_asse, vect_asse, gran_name
!    aster_logical :: l_coef_cplx, l_coef_real
!    real(kind=8) :: coef_r
!    complex(kind=8) :: coef_c
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM2_20')
    endif
!!
!! - List of matrix
!!
!    keywfact  = 'MATR_ASSE'
!    matr_type = 'R'
!    call getfac(keywfact, nb_matr)
!    ASSERT(nb_matr .gt. 0)
!    do i_matr = 1, nb_matr
!        call getvid(keywfact, 'MATRICE', iocc=i_matr, scal=matr_asse, nbret=nbret)
!        ASSERT(nbret .gt. 0)
!        call jelira(matr_asse//'           .VALM', 'TYPE', cval=ktyp)
!        if (ktyp .eq. 'C') then
!            matr_elem_type = 'C'
!            matr_type      = 'C'
!        elseif (ktyp .eq. 'R') then
!            matr_elem_type = 'R'
!        else
!            ASSERT(.false.)
!        endif
!!
!        call getvr8(keywfact, 'COEF_R'   , iocc=i_matr, nbret=nbret)
!        if (nbret .eq. 0) then
!            call getvc8(keywfact, 'COEF_C', iocc=i_matr, nbret=nbret)
!            ASSERT(nbret .ne. 0)
!            l_coef_cplx = .true.
!            l_coef_real = .false.
!            call getvc8(keywfact, 'COEF_C', iocc=i_matr, scal = coef_c, nbret=nbret)
!            ASSERT(nbret .eq. 1)
!        else
!            l_coef_cplx = .false.
!            l_coef_real = .true.
!            call getvr8(keywfact, 'COEF_R', iocc=i_matr, scal = coef_r, nbret=nbret)
!            ASSERT(nbret .eq. 1)
!        endif
!        ds_multipara%matr_name(i_matr)    = matr_asse
!        ds_multipara%matr_type(i_matr)    = matr_elem_type
!        ds_multipara%l_coefm_cplx(i_matr) = l_coef_cplx
!        ds_multipara%l_coefm_real(i_matr) = l_coef_real
!        ds_multipara%coefm_cplx(i_matr)   = coef_c
!        ds_multipara%coefm_real(i_matr)   = coef_r
!    end do
!    ds_multipara%nb_matr      = nb_matr
!!
!! - Second member
!!
!    call getvid(' ', 'VECTEUR', iocc=0, scal=vect_asse, nbret=nbret)
!    call dismoi('NOM_GD', vect_asse, 'CHAM_NO', repk=gran_name)
!    if (gran_name .eq. 'DEPL_R') then
!        vect_type = 'R'
!    elseif (gran_name .eq. 'DEPL_C') then
!        vect_type = 'C'
!    else
!        ASSERT(.false.)
!    endif
!    call getvr8(' ', 'COEF_R'   , iocc=0, nbret=nbret)
!    if (nbret .eq. 0) then
!        call getvc8(' ', 'COEF_C', iocc=0, nbret=nbret)
!        ASSERT(nbret .ne. 0)
!        l_coef_cplx = .true.
!        l_coef_real = .false.
!        call getvc8(' ', 'COEF_C', iocc=0, scal = coef_c, nbret=nbret)
!        ASSERT(nbret .eq. 1)
!    else
!        l_coef_cplx = .false.
!        l_coef_real = .true.
!        call getvr8(' ', 'COEF_R', iocc=0, scal = coef_r, nbret=nbret)
!        ASSERT(nbret .eq. 1)
!    endif
!    ds_multipara%vect_name    = vect_asse
!    ds_multipara%vect_type    = vect_type
!    ds_multipara%l_coefv_cplx = l_coef_cplx
!    ds_multipara%l_coefv_real = l_coef_real
!    ds_multipara%coefv_cplx   = coef_c
!    ds_multipara%coefv_real   = coef_r
!!
!! - System
!!
!    ds_multipara%syst_type    = 'R'
!    if (vect_type .eq. 'C' .or. matr_type .eq. 'C') then
!        ds_multipara%syst_type    = 'C'
!    endif
!
end subroutine
