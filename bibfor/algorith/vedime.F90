subroutine vedime(model     , lload_name, lload_info, time, typres,&
                  vect_elemz)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/inical.h"
#include "asterfort/load_list_info.h"
#include "asterfort/detrsd.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/megeom.h"
#include "asterfort/mecact.h"
#include "asterfort/gcnco2.h"
#include "asterfort/corich.h"
#include "asterfort/calcul.h"
!
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
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: lload_name
    character(len=24), intent(in) :: lload_info
    real(kind=8), intent(in) :: time
    character(len=*), intent(in) :: typres
    character(len=*), intent(inout) :: vect_elemz
!
! --------------------------------------------------------------------------------------------------
!
! Compute Dirichlet loads
!
! For Lagrange elements (AFFE_CHAR_MECA) - U(given)
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  lload_name     : name of object for list of loads name
! In  lload_info     : name of object for list of loads info
! In  time           : current time
! In  typres         : type of coefficeitns (real or complex)
! IO  vect_elem      : name of vect_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbin = 3 
    integer, parameter :: nbout = 1
    character(len=8) :: lpain(nbin), lpaout(nbout)
    character(len=19) :: lchin(nbin), lchout(nbout)
!
    character(len=8) :: load_name, newnom
    character(len=16) :: option
    character(len=19) :: vect_elem, resu_elem
    character(len=24) :: ligrch, chgeom, chtime
    integer :: ibid, load_nume, nb_load, i_load
    character(len=24), pointer :: v_load_name(:) => null()
    integer, pointer :: v_load_info(:) => null()
    aster_logical :: load_empty
    character(len=1) :: base
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    newnom    = '.0000000'
    chtime    = '&&VEDIME.CH_INST_R'
    resu_elem = '&&VEDIME.???????'
    base      = 'V'
!
! - Init fields
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! - Result name for vect_elem
!
    vect_elem = vect_elemz
    if (vect_elem .eq. ' ') then
        vect_elem = '&&VEDIME'
    endif
!
! - Loads
!
    call load_list_info(load_empty, nb_load  , v_load_name, v_load_info,&
                        lload_name, lload_info)
!
! - Allocate result
!
    call detrsd('VECT_ELEM', vect_elem)
    call memare(base, vect_elem, model, ' ', ' ',&
                'CHAR_MECA')
    call reajre(vect_elem, ' ', base)
    if (load_empty) then
        goto 99
    endif
!
! - Geometry field
!
    call megeom(model, chgeom)
!
! - Time field
! 
    call mecact('V', chtime, 'MODELE', model, 'INST_R  ',&
                ncmp=1, nomcmp='INST', sr=time)
!
! - Input fields
!
    lpain(1)  = 'PGEOMER'
    lchin(1)  = chgeom(1:19)
    lpain(2)  = 'PTEMPSR'
    lchin(2)  = chtime(1:19)
!
! - Output field
!
    if (typres .eq. 'R') then
        lpaout(1) = 'PVECTUR'
    else
        lpaout(1) = 'PVECTUC'
    endif
!
! - Computation
!
    do i_load = 1, nb_load
        load_name = v_load_name(i_load)(1:8)
        load_nume = v_load_info(i_load+1)  
        if ((load_nume.gt.0) .and. (load_nume.le.4)) then
            ligrch   = load_name//'.CHME.LIGRE'
!
! --------- Input field
!
            lchin(3) = load_name//'.CHME.CIMPO'
            if (load_nume .eq. 1) then
                if (typres .eq. 'R') then
                    option = 'MECA_DDLI_R'
                    lpain(3) = 'PDDLIMR'
                else
                    option = 'MECA_DDLI_C'
                    lpain(3) = 'PDDLIMC'
                endif
            else if (load_nume.eq.2) then
                option = 'MECA_DDLI_F'
                lpain(3) = 'PDDLIMF'
            else if (load_nume.eq.3) then
                option = 'MECA_DDLI_F'
                lpain(3) = 'PDDLIMF'
            else if (load_nume.eq.4) then
                ASSERT(typres.eq.'R')
                option = 'MECA_DDLI_R'
                lpain(3) = 'PDDLIMR'
            else
                ASSERT(.false.)
            endif
!
! --------- Generate new RESU_ELEM name
!
            call gcnco2(newnom)
            resu_elem(10:16) = newnom(2:8)
            call corich('E', resu_elem, i_load, ibid)
            lchout(1) = resu_elem
!
! --------- Computation
!
            call calcul('S'  , option, ligrch, nbin  , lchin,&
                        lpain, nbout , lchout, lpaout, base ,&
                        'OUI')
!
! --------- Copying output field
!
            call reajre(vect_elem, lchout(1), 'V')
!
        endif
    end do
!
 99 continue
    vect_elemz = vect_elem//'.RELR'
!
    call jedema()
end subroutine
