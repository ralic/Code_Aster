subroutine cbondp(load, ligrmo, mesh, ndim, vale_type)
!
    implicit   none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvr8.h"
#include "asterfort/assert.h"
#include "asterfort/char_crea_cart.h"
#include "asterfort/char_read_elem.h"
#include "asterfort/char_read_val.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/u2mess.h"
#include "asterfort/vetyma.h"
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
    character(len=8), intent(in)  :: load
    character(len=8), intent(in)  :: mesh
    integer, intent(in)  :: ndim
    character(len=19), intent(in) :: ligrmo
    character(len=4), intent(in)  :: vale_type
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Treatment of load ONDE_PLANE
!
! --------------------------------------------------------------------------------------------------
!
!
! In  mesh      : name of mesh
! In  load      : name of load
! In  ndim      : space dimension
! In  ligrmo    : list of elements in model
! In  vale_type : affected value type (real, complex or function)
!
! --------------------------------------------------------------------------------------------------
!
    complex(kind=8) :: c16dummy
    real(kind=8) :: r8dummy
    character(len=8) :: k8dummy
    character(len=16) :: k16dummy
    real(kind=8) :: wave_dire(3), wave_type_r
    character(len=8) :: signal
    character(len=16) :: wave_type
    integer :: iarg, jvalv
    integer :: iocc, ndir, val_nb, nondp, ibid, codret
    character(len=16) :: keywordfact
    character(len=19) :: carte(2)
    integer :: nb_carte, nb_cmp
    character(len=8) :: suffix
    character(len=24) :: list_elem
    integer :: j_elem
    integer :: nb_elem
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    keywordfact = 'ONDE_PLANE'
    call getfac(keywordfact, nondp)
    if (nondp .eq. 0) goto 99
!
! - Initializations
!
    ASSERT(vale_type.eq.'FONC')
!
! - Creation and initialization to zero of <CARTE>
!
    call char_crea_cart('MECANIQUE', keywordfact, load, mesh, ligrmo, &
                        vale_type, nb_carte, carte)
    ASSERT(nb_carte.eq.2)
!
! - Loop on factor keyword
!
    do iocc = 1, nondp
!
! ----- Read mesh affectation
!
        list_elem = '&&CBONDP.LIST_ELEM'
        suffix = ' '
        call char_read_elem(mesh, keywordfact, iocc, suffix, list_elem, &
                            nb_elem)
        call jeveuo(list_elem,'L',j_elem)
        if (nb_elem.eq.0) goto 100
!
! ----- Get wave function
!
        call char_read_val(keywordfact, iocc, 'FONC_SIGNAL', 'FONC', val_nb, &
                           r8dummy, signal, c16dummy, k16dummy)
        ASSERT(val_nb.eq.1)
!
! ----- Affectation of values in <CARTE> - Wave function
!
        call jeveuo(carte(1)//'.VALV', 'E', jvalv)
        nb_cmp = 1
        zk8(jvalv-1+1) = signal
        call nocart(carte(1), 3, k8dummy, 'NUM', nb_elem,&
                    k8dummy, zi(j_elem), ' ', nb_cmp)
!
! ----- Get direction
!
        wave_dire(1) = 0.d0
        wave_dire(2) = 0.d0
        wave_dire(3) = 0.d0
        call getvr8(keywordfact, 'DIRECTION', iocc, iarg, 0,   &
                    wave_dire, ndir)
        ndir = - ndir
        ASSERT(ndir.eq.3)
        call getvr8(keywordfact, 'DIRECTION', iocc, iarg, ndir,&
                    wave_dire, ibid)
!
! ----- Get wave type
!
        call char_read_val(keywordfact, iocc, 'TYPE_ONDE', 'TEXT', val_nb, &
                           r8dummy, k8dummy, c16dummy, wave_type)
        ASSERT(val_nb.eq.1)
        if (ndim .eq. 3) then
            if (wave_type .eq. 'P ') then
                wave_type_r = 0.d0
            else if (wave_type.eq.'SV') then
                wave_type_r = 1.d0
            else if (wave_type.eq.'SH') then
                wave_type_r = 2.d0
            else if (wave_type.eq.'S ') then
                call u2mess('F', 'CHARGES2_61')
            else
                ASSERT(.false.)
            endif
        elseif (ndim .eq. 2) then
            if (wave_type .eq. 'P ') then
                wave_type_r = 0.d0
            else if (wave_type.eq.'S ') then
                wave_type_r = 1.d0
            else if (wave_type.eq.'SV'.or.wave_type.eq.'SH') then
                call u2mess('A', 'CHARGES2_62')
                wave_type_r = 1.d0
            else
                ASSERT(.false.)
            endif
        else
            ASSERT(.false.)
        endif
!
! ----- Affectation of values in <CARTE> - Wave type and direction
!
        call jeveuo(carte(2)//'.VALV', 'E', jvalv)
        nb_cmp = 4
        zr(jvalv-1+1) = wave_dire(1)
        zr(jvalv-1+2) = wave_dire(2)
        zr(jvalv-1+3) = wave_dire(3)
        zr(jvalv-1+4) = wave_type_r
        call nocart(carte(2), 3, k8dummy, 'NUM', nb_elem,&
                    k8dummy, zi(j_elem), ' ', nb_cmp)

100     continue
!
! ----- Check elements
!
        call vetyma(mesh, ndim, keywordfact, list_elem, nb_elem, &
                    codret)
!
        call jedetr(list_elem)

    enddo
!
99  continue
    call jedema()
end subroutine
