subroutine cafond(load, ligrmo, mesh, ndim, vale_type)
!
    implicit   none
!
#include "jeveux.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/char_crea_cart.h"
#include "asterfort/getelem.h"
#include "asterfort/detrsd.h"
#include "asterfort/exlim1.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mesomm.h"
#include "asterfort/nocart.h"
#include "asterfort/peair1.h"
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
! Keyword = 'EFFE_FOND'
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
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=1)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: npres, iocc, nb_cmp
    integer :: ibid, ifm, niv, val_nb, jvalv, codret
    real(kind=8) :: r8dummy
    real(kind=8) :: hole_area, cara_geom(10), mate_area, coef_mult
    complex(kind=8) :: c16dummy
    character(len=8) :: k8dummy
    character(len=8) :: pres_fonc
    real(kind=8) :: pres_real
    character(len=16) :: keywordfact, option, k16dummy
    character(len=19) :: ligrel
    character(len=24) :: list_elem_hole, list_elem_sect
    integer :: j_elem_hole, j_elem_sect
    integer :: nb_elem_hole, nb_elem_sect
    character(len=8) :: suffix
    character(len=19) :: carte(2)
    integer :: nb_carte
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
    keywordfact = 'EFFE_FOND'
    call getfac(keywordfact, npres)
    if (npres.eq.0) goto 99
!
! - Creation and initialization to zero of <CARTE>
!
    call char_crea_cart('MECANIQUE', keywordfact, load, mesh, ligrmo, &
                        vale_type, nb_carte, carte)
    ASSERT(nb_carte.eq.2)
!
! - For computation of geometric caracteristics (elementary)
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
    option    = 'CARA_SECT_POUT3'
    ligrel    = '&&CAFOND.LIGREL'
    lpain(1)  = 'PGEOMER'
    lchin(1)  = mesh//'.COORDO'
    lpaout(1) = 'PCASECT'
    lchout(1) = '&&CAFOND.PSECT'
    list_elem_sect = '&&CAFOND.LISTSECT'
    list_elem_hole = '&&CAFOND.LISTHOLE'
!
    do iocc = 1, npres
!
! ----- Elements for hole
!
        suffix = '_INT'
        call getelem(mesh, keywordfact, iocc, suffix, 'F', &
                     list_elem_hole, nb_elem_hole)
        call jeveuo(list_elem_hole, 'L', j_elem_hole)
!
! ----- Elements for section
!
        suffix = ' '
        call getelem(mesh, keywordfact, iocc, suffix, 'F', &
                     list_elem_sect, nb_elem_sect)
        call jeveuo(list_elem_sect, 'L', j_elem_sect)
!
! ----- Create <LIGREL>
!
        call exlim1(zi(j_elem_sect), nb_elem_sect, ligrmo, 'V', ligrel)
!
! ----- Get pressure
!
        call char_read_val(keywordfact, iocc, 'PRES', vale_type, val_nb, &
                           pres_real, pres_fonc, c16dummy, k16dummy)
        ASSERT(val_nb.eq.1)
!
! ----- Area of hole
!
        call peair1(ligrmo, nb_elem_hole, zi(j_elem_hole), hole_area, r8dummy)
!
! ----- To compute area of material section
!
        call calcul('S', option, ligrel, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call mesomm(lchout(1), 10, ibid, cara_geom, c16dummy,&
                    0, ibid)
        call detrsd('LIGREL', ligrel)
!
! ----- Multiplicative ratio of pressure
!
        mate_area = cara_geom(1)
        coef_mult = -hole_area/mate_area
        if (niv .eq. 2) then
            write (ifm,*) 'SURFACE DU TROU    ',hole_area
            write (ifm,*) 'SURFACE DE MATIERE ',mate_area
        endif
!
! ----- Affectation of values in <CARTE> - Multiplicative ratio of pressure
!
        call jeveuo(carte(1)//'.VALV', 'E', jvalv)
        nb_cmp = 1
        zr(jvalv-1+1) = coef_mult
        call nocart(carte(1), 3, k8dummy, 'NUM', nb_elem_sect,&
                    k8dummy, zi(j_elem_sect), ' ', nb_cmp)
!
! ----- Affectation of values in <CARTE> - Pressure
!
        call jeveuo(carte(2)//'.VALV', 'E', jvalv)
        nb_cmp = 1
        if (vale_type .eq. 'REEL') then
            zr(jvalv-1+1)  = pres_real
        elseif (vale_type .eq. 'FONC') then
            zk8(jvalv-1+1) = pres_fonc
        else
            ASSERT(.false.)
        endif
        call nocart(carte(2), 3, k8dummy, 'NUM', nb_elem_sect,&
                    k8dummy, zi(j_elem_sect), ' ', nb_cmp)
!
! ----- Check elements
!
        call vetyma(mesh, ndim, keywordfact, list_elem_sect, nb_elem_sect, &
                    codret)
!
        call jedetr(list_elem_hole)
        call jedetr(list_elem_sect)
!
    end do
!
99  continue

    call jedema()
end subroutine
