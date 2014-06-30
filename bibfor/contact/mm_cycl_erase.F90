subroutine mm_cycl_erase(sd_cont_defi, sd_cont_solv, cycl_type, point_curr)
!
    implicit     none
!
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sd_cont_defi
    character(len=24), intent(in) :: sd_cont_solv
    integer, intent(in) :: cycl_type
    integer, intent(in) :: point_curr
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Erase cycling informations
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_cont_solv : data structure for contact solving
! In  sd_cont_defi : data structure from contact definition
! In  cycl_type    : type of cycling to erase
!                     0 - for erasing for all cycles
! In  point_curr   : contact point to erasing
!                     0 - when erasing all cycles
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sd_cycl_lis
    integer, pointer :: p_cycl_lis(:) => null()
    character(len=24) :: sd_cycl_nbr
    integer, pointer :: p_cycl_nbr(:) => null()
    character(len=24) :: sd_cycl_eta
    integer, pointer :: p_cycl_eta(:) => null()
    integer :: point_number, point_index
    integer :: cycl_index
    logical(kind=1) :: lctcc
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    lctcc = cfdisl(sd_cont_defi,'FORMUL_CONTINUE')
    if (.not.lctcc) then
        goto 99
    endif
!
! - Name of cycling objects
!
    sd_cycl_lis = sd_cont_solv(1:14)//'.CYCLIS'
    sd_cycl_nbr = sd_cont_solv(1:14)//'.CYCNBR'
    sd_cycl_eta = sd_cont_solv(1:14)//'.CYCETA'
!
! - Access to cycling objects
!
    call jeveuo(sd_cycl_lis, 'E', vi = p_cycl_lis)
    call jeveuo(sd_cycl_nbr, 'E', vi = p_cycl_nbr)
    call jeveuo(sd_cycl_eta, 'E', vi = p_cycl_eta)
!
! - Initializations
!
    point_number = cfdisi(sd_cont_defi,'NTPC' )
!
! - Erasing cycling information
!
    if (cycl_type.eq.0) then
        ASSERT(point_curr.eq.0)
        do cycl_index = 1, 4
            do point_index = 1, point_number
                p_cycl_lis(4*(point_index-1)+cycl_index) = 0
                p_cycl_nbr(4*(point_index-1)+cycl_index) = 0
                p_cycl_eta(4*(point_index-1)+cycl_index) = 0
            enddo
        end do
    else if (cycl_type.gt.0) then
        ASSERT(point_curr.le.point_number)
        ASSERT(point_curr.ge.1)
        ASSERT(cycl_type.ge.1)
        ASSERT(cycl_type.le.4)
        cycl_index = cycl_type
        point_index = point_curr
        p_cycl_lis(4*(point_index-1)+cycl_index) = 0
        p_cycl_nbr(4*(point_index-1)+cycl_index) = 0
        p_cycl_eta(4*(point_index-1)+cycl_index) = 0
    else
        ASSERT(.false.)
    endif
!
 99 continue
!
    call jedema()
end subroutine
