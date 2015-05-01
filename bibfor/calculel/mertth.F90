subroutine mertth(model, lload_name, lload_info, cara_elem, mate     ,&
                  time , time_move , temp_prev , temp_iter, matr_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/load_list_info.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
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
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: time
    character(len=24), intent(in) :: time_move
    character(len=24), intent(in) :: temp_prev
    character(len=24), intent(in) :: temp_iter
    character(len=19), intent(inout) :: matr_elem
!
! --------------------------------------------------------------------------------------------------
!
! Thermic - Matrix
! 
! Elementary matrix for transport (volumic and surfacic terms)
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of the model
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  lload_name       : name of object for list of loads name
! In  lload_info       : name of object for list of loads info
! In  time             : time (<CARTE>)
! In  time_move        : modified time (<CARTE>) for THER_NON_LINE_MO
! In  temp_prev        : previous temperature
! In  temp_iter        : temperature field at current Newton iteration
! IO  matr_elem        : name of matr_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbchmx
    parameter (nbchmx=4)
    integer :: nbopt(nbchmx), nligr(nbchmx)
    character(len=6) :: nomopr(nbchmx), nomopf(nbchmx), nomchp(nbchmx)
    character(len=7) :: nompar(nbchmx), nompaf(nbchmx)
!
    character(len=8) :: lpain(6), lpaout(1), load_name
    character(len=16) :: option
    character(len=24) :: ligrel(2), lchin(6), lchout(1)
    character(len=24) :: chgeom, chcara(18)
    integer :: iret, nb_load, i_load, ilires, k, load_nume
    aster_logical :: load_empty
    character(len=24), pointer :: v_load_name(:) => null()
    integer, pointer :: v_load_info(:) => null()    
    data nomchp/'.COEFH','.FLUNL','.HECHP','.COEFH'/
    data nomopr/'COEH_R','      ','PARO_R','COET_R'/
    data nomopf/'COEH_F','FLUTNL','PARO_F','COET_F'/
    data nompar/'PCOEFHR','       ','PHECHPR','PCOEFHR'/
    data nompaf/'PCOEFHF','PFLUXNL','PHECHPF','PCOEFHF'/
    data nbopt/3,4,5,3/
    data nligr/1,1,2,1/
!
! --------------------------------------------------------------------------------------------------
!
    
!
! - Loads
!
    call load_list_info(load_empty, nb_load   , v_load_name, v_load_info,&
                        lload_name, lload_info)
!
    call megeom(model, chgeom)
    call mecara(cara_elem, chcara)
!
    call jeexin(matr_elem(1:19)//'.RELR', iret)
    if (iret .eq. 0) then
        matr_elem = '&&METRIG'
        call memare('V', matr_elem, model(1:8), mate, cara_elem,&
                    'RIGI_THER')
    else
        call jedetr(matr_elem(1:19)//'.RELR')
    endif
!
    ligrel(1) = model(1:8)//'.MODELE'
!
    lpaout(1) = 'PMATTTR'
    lchout(1) = matr_elem(1:8)//'.ME001'
    ilires = 0
!
    if (model .ne. '        ') then
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PMATERC'
        lchin(2) = mate
        lpain(3) = 'PCACOQU'
        lchin(3) = chcara(7)
        lpain(4) = 'PTEMPSR'
        lchin(4) = time
        lpain(5) = 'PTEMPER'
        lchin(5) = temp_prev
        lpain(6) = 'PTEMPEI'
        lchin(6) = temp_iter
        option = 'RIGI_THER_TRANS'
        ilires = ilires + 1
        call codent(ilires, 'D0', lchout(1) (12:14))
        call calcul('S', option, ligrel(1), 6, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(matr_elem, lchout(1), 'V')
    endif
!
    if (nb_load .gt. 0) then
        do i_load = 1, nb_load
            load_name = v_load_name(i_load)(1:8)
            load_nume = v_load_info(nb_load+i_load+1)
            if (load_nume .gt. 0) then
                ligrel(2) = load_name//'.CHTH.LIGRE'
                lpain(1) = 'PGEOMER'
                lchin(1) = chgeom
                lpain(3) = 'PTEMPSR'
                lchin(3) = time
                lpain(4) = 'PTEMPER'
                lchin(4) = temp_iter
                lpain(5) = 'PDEPLAR'
                lchin(5) = '&&DEPPLU'
                lpaout(1) = 'PMATTTR'
                lchout(1) = matr_elem(1:8)//'.ME001'
                do k = 1, nbchmx
                    lchin(2) = load_name(1:8)//'.CHTH'// nomchp(k)// '.DESC'
                    call jeexin(lchin(2), iret)
                    if (iret .gt. 0) then
                        if (load_nume.eq. 1) then
                            option = 'RIGI_THER_'//nomopr(k)
                            lpain(2) = nompar(k)
                        else if (load_nume.eq.2 .or. load_nume.eq.3) then
                            option = 'RIGI_THER_'//nomopf(k)
                            lpain(2) = nompaf(k)
                        endif
                        if (option(11:14) .eq. 'PARO') then
                            lpain(3) = 'PTEMPSR'
                            lchin(3) = time_move
                        endif
                        if (k .eq. 2) lchin(4) = temp_iter
                        ilires = ilires + 1
                        call codent(ilires, 'D0', lchout(1) (12:14))
                        call calcul('S', option, ligrel(nligr(k)), nbopt( k), lchin,&
                                    lpain, 1, lchout, lpaout, 'V',&
                                    'OUI')
                        call reajre(matr_elem, lchout(1), 'V')
                    endif
                end do
            endif
        end do
    endif
!
end subroutine
