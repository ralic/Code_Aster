subroutine char_beam_lcs(mesh, model, connex_inv, keywordfact, iocc,&
                         node_nume, node_name, cmp_name_loc, cmp_valr_loc, cmp_name_glo,&
                         cmp_acti_glo, cmp_valr_glo)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/matloc.h"
#include "asterfort/reliem.h"
#include "asterfort/utpvlg.h"
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
!
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    character(len=19), intent(in) :: connex_inv
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    integer, intent(in) :: node_nume
    character(len=8), intent(in) :: node_name
    character(len=16), intent(in) :: cmp_name_loc(6)
    real(kind=8), intent(in) :: cmp_valr_loc(6)
    character(len=16), intent(out) :: cmp_name_glo(6)
    integer, intent(out) :: cmp_acti_glo(6)
    real(kind=8), intent(out) :: cmp_valr_glo(6)
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Change components with local coordinate system for beams at node
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh           : meshing
! In  model          : model
! In  connex_inv     : inverse connectivity of mesh (nodes -> elements)
! In  keywordfact    : factor cmp_name to read
! In  iocc           : factor cmp_name index in AFFE_CHAR_MECA
! In  node_name      : name of node
! In  node_nume      : number of node
! In  cmp_name_loc   : list of components in local coordinate system
! In  cmp_valr_loc   : values (if real) of components in local coordinate system
! Out cmp_name_glo   : list of components in global coordinate system
! Out cmp_acti_glo   : 1 if component affected, 0 else
! Out cmp_valr_glo   : values (if real) of components in global coordinate system
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: matr_glob_loca(3, 3)
    real(kind=8) :: rln1(3), rgn1(3)
    real(kind=8) :: dloc(3), dglo(3)
    integer :: i_direc, i_cmp
    character(len=16) :: keyw_name(2), keyw_type(2)
    character(len=16) :: cmp_name, list_cmp(6)
    character(len=24) :: list_repe_elem
    integer :: nb_repe_elem, j_repe_elem
!
    data list_cmp /'DX','DY','DZ','DRX','DRY','DRZ'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    do i_cmp = 1, 6
        cmp_valr_glo(i_cmp) = 0.d0
        cmp_acti_glo(i_cmp) = 0
        cmp_name_glo(i_cmp) = list_cmp(i_cmp)
    enddo
!
! - Mesh for local coordinate system
!
    keyw_name(1) = 'MAILLE_REPE'
    keyw_type(1) = 'MAILLE'
    keyw_name(2) = 'GROUP_MA_REPE'
    keyw_type(2) = 'GROUP_MA'
    list_repe_elem = '&&REPE.MAILLE'
    call reliem(model, mesh, 'NU_MAILLE', keywordfact, iocc,&
                2, keyw_name, keyw_type, list_repe_elem, nb_repe_elem)
    if (nb_repe_elem .ne. 0) then
        call jeveuo(list_repe_elem, 'L', j_repe_elem)
    else
        j_repe_elem = 1
    endif
!
! - Local coordinate system
!
    call matloc(mesh, connex_inv, keywordfact, iocc, node_nume,&
                node_name, nb_repe_elem, zi(j_repe_elem), matr_glob_loca)
!
! - Translation
!
    do i_direc = 1, 3
        dloc(i_direc) = 0.d0
        rln1(i_direc) = 0.d0
    enddo
    do i_cmp = 1, 6
        cmp_name = cmp_name_loc(i_cmp)
        if (cmp_name .eq. 'DX') then
            rln1(1) = 1.d0
            dloc(1) = cmp_valr_loc(i_cmp)
        endif
        if (cmp_name .eq. 'DY') then
            rln1(2) = 1.d0
            dloc(2) = cmp_valr_loc(i_cmp)
        endif
        if (cmp_name .eq. 'DZ') then
            rln1(3) = 1.d0
            dloc(3) = cmp_valr_loc(i_cmp)
        endif
    enddo
!
    call utpvlg(1, 3, matr_glob_loca, dloc, dglo)
    call utpvlg(1, 3, matr_glob_loca, rln1, rgn1)
!
    if (rgn1(1) .ne. 0.d0) then
        cmp_valr_glo(1) = dglo(1)
        cmp_acti_glo(1) = 1
    endif
    if (rgn1(2) .ne. 0.d0) then
        cmp_valr_glo(2) = dglo(2)
        cmp_acti_glo(2) = 1
    endif
    if (rgn1(3) .ne. 0.d0) then
        cmp_valr_glo(3) = dglo(3)
        cmp_acti_glo(3) = 1
    endif
!
! - Rotation
!
    do i_direc = 1, 3
        dloc(i_direc) = 0.d0
        rln1(i_direc) = 0.d0
    enddo
    do i_cmp = 1, 6
        cmp_name = cmp_name_loc(i_cmp)
        if (cmp_name .eq. 'DRX') then
            rln1(1) = 1.d0
            dloc(1) = cmp_valr_loc(i_cmp)
        endif
        if (cmp_name .eq. 'DRY') then
            rln1(2) = 1.d0
            dloc(2) = cmp_valr_loc(i_cmp)
        endif
        if (cmp_name .eq. 'DRZ') then
            rln1(3) = 1.d0
            dloc(3) = cmp_valr_loc(i_cmp)
        endif
    enddo
!
    call utpvlg(1, 3, matr_glob_loca, dloc, dglo)
    call utpvlg(1, 3, matr_glob_loca, rln1, rgn1)
!
    if (rgn1(1) .ne. 0.d0) then
        cmp_valr_glo(4) = dglo(1)
        cmp_acti_glo(4) = 1
    endif
    if (rgn1(2) .ne. 0.d0) then
        cmp_valr_glo(5) = dglo(2)
        cmp_acti_glo(5) = 1
    endif
    if (rgn1(3) .ne. 0.d0) then
        cmp_valr_glo(6) = dglo(3)
        cmp_acti_glo(6) = 1
    endif
!
    call jedema()
end subroutine
