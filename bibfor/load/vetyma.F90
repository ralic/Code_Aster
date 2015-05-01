subroutine vetyma(mesh, ndim, load_type, list_elem, nb_elem,&
                  codret)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_elem
    character(len=24), intent(in) :: list_elem
    character(len=16), intent(in) :: load_type
    integer, intent(in) :: ndim
    integer, intent(out) :: codret
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Check element type
!
! --------------------------------------------------------------------------------------------------
!
!
! In  mesh      : name of mesh
! In  ndim      : space dimension
! In  load_type : type of load
! In  list_elem : list of elements read
! In  nb_elem   : number of elements read
! Out codret    : 0 if OK
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: valk(2)
    character(len=8) :: topo_2d, topo_3d, name_elem, type_elem, topo_elem, topo_elem2
    integer :: iatyma, iadtyp, nerr
    integer :: jelem, ielem, nume_elem
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    codret=0
    if (nb_elem .eq. 0) goto 99
!
! - Access to mesh
!
    call jeveuo(mesh//'.TYPMAIL', 'L', iatyma)
    nerr = 0
    codret = 0
    call jeveuo(list_elem, 'L', jelem)
!
! - Type of elements
!
    if (load_type .eq. 'FLUX_REP' .or. load_type .eq. 'PRES_REP' .or. load_type .eq.&
        'ECHANGE' .or. load_type .eq. 'FORCE_FACE' .or. load_type .eq. 'IMPE_FACE' .or.&
        load_type .eq. 'VITE_FACE' .or. load_type .eq. 'FORCE_CONTOUR' .or. load_type .eq.&
        'EFFE_FOND' .or. load_type .eq. 'ONDE_PLAN') then
        topo_2d = 'LINE'
        topo_3d = 'SURF'
    else if (load_type.eq.'SOURCE' .or.load_type.eq.'FORCE_INTERNE') then
        topo_2d = 'SURF'
        topo_3d = 'VOLU'
    else
        goto 99
    endif
!
    ASSERT(ndim.eq.2.or.ndim.eq.3.or.ndim.eq.23)
    if (ndim .eq. 2) then
        topo_elem = topo_2d
    else if (ndim.eq.3) then
        topo_elem = topo_3d
    else
        topo_elem = '23'
    endif
!
    do ielem = 1, nb_elem
        nume_elem = zi(jelem-1+ielem)
        call jenuno(jexnum(mesh//'.NOMMAI', nume_elem), name_elem)
        iadtyp = iatyma-1+nume_elem
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(iadtyp)), type_elem)

!       -- in the case ndim=23, we determine topo_elem from the first element :
        if (topo_elem .eq. '23') then
            ASSERT(ielem.eq.1)
            if (type_elem(1:3) .eq. 'SEG') then
                topo_elem2 = 'LINE'
            elseif ((type_elem(1:4) .eq. 'QUAD') .or. (type_elem(1:4) .eq. 'TRIA')) then
                topo_elem2 = 'SURF'
            elseif ((type_elem(1:4) .eq. 'HEXA') .or. (type_elem(1:4) .eq. 'PENT') .or.&
                (type_elem(1:4) .eq. 'PYRA') .or. (type_elem(1:4) .eq. 'TETR')) then
                topo_elem2 = 'VOLU'
            else
                ASSERT(.false.)
            endif
            topo_elem=topo_elem2

            if (topo_3d.eq.'VOLU') then
                valk(1) = name_elem
                valk(2) = load_type
                if (topo_elem.eq.'LINE') call utmess('F', 'CHARGES2_91', nk=2, valk=valk)
            elseif (topo_3d.eq.'SURF') then
                valk(1) = name_elem
                valk(2) = load_type
                if (topo_elem.eq.'VOLU') call utmess('F', 'CHARGES2_90', nk=2, valk=valk)
            else
                ASSERT(.false.)
            endif
        endif


        if (topo_elem .eq. 'LINE') then
            if (type_elem(1:3) .ne. 'SEG') then
                nerr = nerr+1
                valk(1) = name_elem
                valk(2) = load_type
                call utmess('A', 'CHARGES2_86', nk=2, valk=valk)
            endif
        else if (topo_elem.eq.'SURF') then
            if ((type_elem(1:4) .ne. 'QUAD') .and. (type_elem(1:4) .ne. 'TRIA')) then
                nerr = nerr+1
                valk(1) = name_elem
                valk(2) = load_type
                call utmess('A', 'CHARGES2_87', nk=2, valk=valk)
            endif
        else if (topo_elem.eq.'VOLU') then
            if ((type_elem(1:4) .ne. 'HEXA') .and. (type_elem(1:4) .ne. 'PENT') .and.&
                (type_elem(1:4) .ne. 'PYRA') .and. (type_elem(1:4) .ne. 'TETR')) then
                nerr = nerr+1
                valk(1) = name_elem
                valk(2) = load_type
                call utmess('A', 'CHARGES2_88', nk=2, valk=valk)
            endif
        else
            ASSERT(.false.)
        endif
    enddo
!
    if (nb_elem .eq. nerr) then
        call utmess('A', 'CHARGES2_89', sk=load_type)
    endif
!
    codret = nerr
99  continue
    call jedema()
end subroutine
