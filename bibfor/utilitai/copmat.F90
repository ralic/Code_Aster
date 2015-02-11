subroutine copmat(mat_in, mat_out)
    implicit none
#include "jeveux.h"
#include "asterf_types.h"
!
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!                              Function
!     _______________________________________________________________________
!    | Extract, inside a temporary work vector, all terms of a given matrix  |
!    |_______________________________________________________________________|
!
! person_in_charge: hassan.berro at edf.fr
!
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/matini.h"

    character(len=8), intent(in) :: mat_in
    real(kind=8), intent(out) :: mat_out(*)


    aster_logical :: lsym
    integer :: neq, jsmhc, nbnonz, nvale, jvale
    integer :: nlong, jval2, kterm, jcoll, iligl
    integer :: i, j, col

    character(len=1)  :: ktyp
    character(len=14) :: nonu
    character(len=19) :: mat19

    character(len=24), pointer :: refa(:) => null()
    integer, pointer           :: smde(:) => null()
    integer, pointer           :: smdi(:) => null()
!
#define m_out(i,j) mat_out((i-1)*neq+j)
!
    call jemarq()

    mat19 = mat_in

    call jeveuo(mat19//'.REFA', 'L', vk24=refa)
    nonu=refa(2)(1:14)

    call jeveuo(nonu//'.SMOS.SMDE', 'L', vi=smde)
    neq = smde(1)
    call matini(neq, neq, 0.d0, mat_out)

    call jeveuo(nonu//'.SMOS.SMDI', 'L', vi=smdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)

    nbnonz = smdi(neq)

    call jelira(mat19//'.VALM', 'NMAXOC', nvale)
    if (nvale .eq. 1) then
        lsym=.true.
    else if (nvale.eq.2) then
        lsym=.false.
    else
        ASSERT(.false.)
    endif

    call jeveuo(jexnum(mat19//'.VALM', 1), 'L', jvale)
    call jelira(jexnum(mat19//'.VALM', 1), 'LONMAX', nlong)
    ASSERT(nlong.eq.nbnonz)
    if (.not.lsym) then
        call jeveuo(jexnum(mat19//'.VALM', 2), 'L', jval2)
        call jelira(jexnum(mat19//'.VALM', 2), 'LONMAX', nlong)
        ASSERT(nlong.eq.nbnonz)
    endif

    call jelira(jexnum(mat19//'.VALM', 1), 'TYPE', cval=ktyp)
    ASSERT(ktyp.eq.'R')

    jcoll=1
    do  kterm = 1, nbnonz

        if (smdi(jcoll) .lt. kterm) jcoll = jcoll+1
        iligl = zi4(jsmhc-1+kterm)

        i = iligl
        j = jcoll

        if ((.not.lsym) .and. (i.ge.j)) then
            col = j
            j = i
            i = col
        endif

        m_out(i,j) = zr(jvale-1+kterm)
        m_out(j,i) = zr(jvale-1+kterm)

        if ((.not.lsym) .and. (i.ne.j)) then
            m_out(j,i) = zr(jval2-1+kterm)
        endif
      end do

    call jedema()
end subroutine