subroutine meamac(model_, nb_load, v_list_load8, mate, matr_elem_, base)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/calcul.h"
#include "asterfort/jeexin.h"
#include "asterfort/codent.h"
#include "asterfort/megeom.h"
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
!
    character(len=*), intent(in) :: model_
    integer, intent(in) :: nb_load
    character(len=8), pointer, intent(in) :: v_list_load8(:)
    character(len=*), intent(in) :: mate
    character(len=*), intent(in) :: matr_elem_
    character(len=1), intent(in) :: base
!
! --------------------------------------------------------------------------------------------------
!
! Acoustic
!
! Elementary matrix for AMOR_ACOU
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  nb_load          : number of loads
! In  v_list_load8     : pointer to list of loads (K8)
! In  mate             : name of material characteristics (field)
! In  matr_elem        : elementary matrix
! In  base             : JEVEUX base to create matr_elem
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbout = 1
    integer, parameter :: nbin = 3
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
    character(len=8) ::  model
    character(len=19) :: matr_elem
    character(len=16) :: option
    character(len=19) :: resu_elem, ligrmo
    character(len=24) :: chgeom, chimpe
    integer :: i_load, ilires, iret
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    model     = model_
    matr_elem = matr_elem_
    resu_elem = matr_elem(1:8)//'.???????'
    ligrmo    = model//'.MODELE'
    option    = 'AMOR_ACOU'
!
! - Init fields
!
    call inical(nbin, lpain, lchin, nbout, lpaout, lchout)
!
! - Create RERR object
!
    call jedetr(matr_elem//'.RELR')
    call memare(base, matr_elem, model, mate, ' ', 'AMOR_ACOU')
!
! - Geometry field
!
    call megeom(model, chgeom)
!
! - Input fields
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom(1:19)
    lpain(2) = 'PMATERC'
    lchin(2) = mate(1:19)
    lpain(3) = 'PIMPEDC'
!
! - Output field
!
    lpaout(1) = 'PMATTTC'
    ilires    = 0
!
    do i_load = 1, nb_load
        chimpe = v_list_load8(i_load)//'.CHAC.IMPED.DESC'
        call jeexin(chimpe, iret)
        if (iret .ne. 0) then
            lchin(3) = chimpe(1:19)
!
! --------- Generate new RESU_ELEM name
!
            ilires   = ilires + 1
            if (ilires .gt. 9999999) then
                call utmess('F', 'RESUELEM1_1', sk = option)
            endif
            call codent(ilires, 'D0', resu_elem(10:16))
            lchout(1) = resu_elem
!
! --------- Computation
!
            call calcul('S'  , option, ligrmo, nbin, lchin,&
                        lpain, nbout , lchout, lpaout, base,&
                        'OUI')
!
! --------- Copying resu_elem
!
            call reajre(matr_elem, lchout(1), base)
            ilires = ilires + 1
        endif
    end do
!
    call jedema()
end subroutine
