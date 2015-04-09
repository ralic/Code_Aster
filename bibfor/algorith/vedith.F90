subroutine vedith(model, list_load, time, vect_elem_)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=19), intent(in) :: list_load
    character(len=24), intent(in) :: time
    character(len=24), intent(inout) :: vect_elem_
!
! --------------------------------------------------------------------------------------------------
!
! Thermics - Load
!
! Elementary vector for Dirichlet BC
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  list_load        : name for list of loads
! In  time             : time (<CARTE>)
! IO  vect_elem        : elementary vector
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: nomch0, nomcha
    character(len=8) :: lpain(3), paout, newnom
    character(len=16) :: option
    character(len=19) :: vect_elem
    character(len=24) :: ligrch, lchin(3), resu_elem, chgeom
    integer :: iaux, iret, nb_load, jinf, jchar, i_load
    integer :: numdi
    aster_logical :: bidon
    character(len=24) :: lload_name, lload_info
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    newnom = '.0000000'
    bidon = .true.
    lload_name = list_load(1:19)//'.LCHA'
    lload_info = list_load(1:19)//'.INFC'
!
    call jeexin(lload_name, iret)
    if (iret .ne. 0) then
        call jelira(lload_name, 'LONMAX', nb_load)
        if (nb_load .ne. 0) then
            bidon = .false.
            call jeveuo(lload_name, 'L', jchar)
            call jeveuo(lload_info, 'L', jinf)
        endif
    endif
!
    vect_elem = '&&VEDITH'
    resu_elem = '&&VEDITH.???????'
!
!     -- ALLOCATION DU VECT_ELEM :
!     -----------------------------
    call detrsd('VECT_ELEM', vect_elem)
    call memare('V', vect_elem, model(1:8), ' ', ' ',&
                'CHAR_THER')
    call reajre(vect_elem, ' ', 'V')
    if (bidon) goto 40
!
    call megeom(model(1:8), chgeom)
!
    paout = 'PVECTTR'
    lpain(2) = 'PGEOMER'
    lchin(2) = chgeom
    lpain(3) = 'PTEMPSR'
!
    lchin(3) = time
!
    do i_load = 1, nb_load
        numdi = zi(jinf+i_load)
        if (numdi .gt. 0) then
            nomch0 = zk24(jchar+i_load-1) (1:8)
            ligrch = nomch0//'.CHTH.LIGRE'
            nomcha = nomch0
            lchin(1) = nomcha//'.CHTH.CIMPO.DESC'
            if (numdi .eq. 1) then
                option = 'THER_DDLI_R'
                lpain(1) = 'PDDLIMR'
            else if (numdi.eq.2) then
                option = 'THER_DDLI_F'
                lpain(1) = 'PDDLIMF'
            else if (numdi.eq.3) then
                option = 'THER_DDLI_F'
                lpain(1) = 'PDDLIMF'
            endif
!
            call gcnco2(newnom)
            resu_elem(10:16) = newnom(2:8)
            call corich('E', resu_elem, i_load, iaux)
!
            call calcul('S', option, ligrch, 3, lchin,&
                        lpain, 1, resu_elem, paout, 'V',&
                        'OUI')
            call reajre(vect_elem, resu_elem, 'V')
        endif
    end do
!
 40 continue
!
    vect_elem_ = vect_elem//'.RELR'
    call jedema()
end subroutine
