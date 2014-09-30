subroutine rvchn1(deplaz, nomjv, nbno, numnd, pgl)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/select_dof.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
    integer :: nbno, numnd(*)
    character(len=*) :: deplaz, nomjv
    real(kind=8) :: pgl(3, 3)
! ----------------------------------------------------------------------
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
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: ino
    integer :: iavald, numdx, numdy, numdz, numdrx
    integer :: numdry, numdrz, nb_node, nb_cmp
    real(kind=8) :: valed(3), vald(3), valer(3), valr(3)
    character(len=8) :: gran_name
    character(len=19) :: depla
    integer, pointer :: list_idx_dof(:) => null()
    integer, pointer :: list_node(:) => null()
    character(len=8), pointer :: list_cmp(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    depla = deplaz
    call dismoi('NOM_GD', deplaz, 'CHAM_NO', repk=gran_name)
    if (gran_name .ne. 'DEPL_R') then
        call utmess('F', 'POSTRELE_17')
    endif
!
! - Create list of components
!
    nb_cmp = 6
    AS_ALLOCATE(vk8=list_cmp, size = nb_cmp)
    list_cmp(1) = 'DX'
    list_cmp(2) = 'DY'
    list_cmp(3) = 'DZ'
    list_cmp(4) = 'DRX'
    list_cmp(5) = 'DRY'
    list_cmp(6) = 'DRZ'
!
! - Create list of equations
!
    AS_ALLOCATE(vi=list_idx_dof, size = nb_cmp)
!
! - Create list of equations
!
    nb_node = 1
    AS_ALLOCATE(vi=list_node, size = nb_node)
!
    call jedupo(depla//'.VALE', 'V', nomjv, .false._1)
    call jeveuo(nomjv, 'E', iavald)
!
    do ino = 1, nbno
        list_node(1) = numnd(ino)
        list_idx_dof(1:nb_cmp) = 0
        call select_dof(list_idx_dof = list_idx_dof,&
                        chamnoz  = depla,&
                        nb_nodez = nb_node, list_nodez = list_node,&
                        nb_cmpz  = nb_cmp , list_cmpz  = list_cmp)

        numdx  = list_idx_dof(1)
        numdy  = list_idx_dof(2)
        numdz  = list_idx_dof(3)
        numdrx = list_idx_dof(4)
        numdry = list_idx_dof(5)
        numdrz = list_idx_dof(6)
        valed(1) = 0.0d0
        valed(2) = 0.0d0
        valed(3) = 0.0d0
        valer(1) = 0.0d0
        valer(2) = 0.0d0
        valer(3) = 0.0d0
        if (numdx.ne.0) then
            valed(1) = zr(iavald-1+numdx)
        endif
        if (numdy.ne.0) then
            valed(2) = zr(iavald-1+numdy)
        endif
        if (numdz.ne.0) then
            valed(3) = zr(iavald-1+numdz)
        endif
        if (numdrx.ne.0) then
            valer(1) = zr(iavald-1+numdrx)
        endif
        if (numdry.ne.0) then
            valer(2) = zr(iavald-1+numdry)
        endif
        if (numdrz.ne.0) then
            valer(3) = zr(iavald-1+numdrz)
        endif
        if ((numdx+numdy+numdz) .ne. 0) then
            call utpvgl(1, 3, pgl, valed, vald)
            if (numdx .ne. 0) zr(iavald-1+numdx) = vald(1)
            if (numdy .ne. 0) zr(iavald-1+numdy) = vald(2)
            if (numdz .ne. 0) zr(iavald-1+numdz) = vald(3)
        endif
        if ((numdrx+numdry+numdrz) .ne. 0) then
            call utpvgl(1, 3, pgl, valer, valr)
            if (numdrx .ne. 0) zr(iavald-1+numdrx) = valr(1)
            if (numdry .ne. 0) zr(iavald-1+numdry) = valr(2)
            if (numdrz .ne. 0) zr(iavald-1+numdrz) = valr(3)
        endif
    end do
!
    AS_DEALLOCATE(vi=list_idx_dof)
    AS_DEALLOCATE(vi=list_node)
    AS_DEALLOCATE(vk8=list_cmp)
!
    call jedema()
end subroutine
