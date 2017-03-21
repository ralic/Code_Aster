subroutine get_tref(chmate, imate, tref, l_tref_is_nan, l_empty)
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: chmate
    integer, intent(in) :: imate
    real(kind=8), intent(out) :: tref
    aster_logical, intent(out) :: l_tref_is_nan
    aster_logical, intent(out) :: l_empty
!
! --------------------------------------------------------------------------------------------------
!
! Material - Coding
!
! Get TREF from external state variables
!
! --------------------------------------------------------------------------------------------------
!
! In  chmate           : name of material field (CHAM_MATER)
! In  imate            : index of current material
! Out tref             : temperature for reference
! Out l_tref_is_nan    : .true. if tref is a NaN
! Out l_empty          : .true. if TREF without field for temperature
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: ktref
    character(len=19) :: cartrf
    integer :: nb_cmp, nbec, ngdmax
    integer :: ec1, k, kk, iret
    character(len=8) :: phys_name
    integer, pointer :: v_chmate_desc(:) => null()
    character(len=8), pointer :: v_chmate_vale(:) => null()
    character(len=16), pointer :: v_vtrf(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    tref          = r8nnem()
    l_tref_is_nan = .false.
    l_empty       = .false.
!
! - Access to CHAM_MATER
!
    call jeveuo(chmate//'.CHAMP_MAT .DESC', 'L', vi =v_chmate_desc)
    call jeveuo(chmate//'.CHAMP_MAT .VALE', 'L', vk8=v_chmate_vale)

    cartrf = chmate//'.TEMP    .2'
    call jeexin(cartrf//'.DESC', iret)
    if (iret .ne. 0) then
        call jeveuo(cartrf//'.VALE', 'L', vk16=v_vtrf)
        l_empty = v_vtrf(1) .eq. 'CHAMP' .and. v_vtrf(4) .eq. 'VIDE'
    endif
!
! - Check physical quantity
!
    call jenuno(jexnum('&CATA.GD.NOMGD', v_chmate_desc(1)), phys_name)
    ASSERT(phys_name .eq. 'NOMMATER')
    call jelira(jexnom('&CATA.GD.NOMCMP', 'NOMMATER'), 'LONMAX', nb_cmp)
    call dismoi('NB_EC', phys_name, 'GRANDEUR', repi=nbec)
    ngdmax = v_chmate_desc(2)
!
! - Get TREF (SUR LE 1ER ENTIER CODE)
!
    ec1 = v_chmate_desc(3+2*ngdmax+nbec*(imate-1)+1)
    k   = 0
    do kk = 1, 30
        if (exisdg([ec1],kk)) then
            k=k+1
        endif
    end do
    if (v_chmate_vale(nb_cmp*(imate-1)+k-3) .ne. 'TREF=>') then
        call utmess('F', 'MATERIAL1_56', sk=chmate)
    endif
    ktref(1:8)   = v_chmate_vale(nb_cmp*(imate-1)+k-2)
    ktref(9:16)  = v_chmate_vale(nb_cmp*(imate-1)+k-1)
    ktref(17:24) = v_chmate_vale(nb_cmp*(imate-1)+k)
    if (ktref(1:3) .eq. 'NAN') then
        tref = r8nnem()
        l_tref_is_nan = .true.
    else
        read (ktref,'(1PE22.15)') tref
    endif
!
end subroutine
