subroutine lccmst(ds_contact, matr_asse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h" 
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/assert.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: matr_asse
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! LAC method - Modification of matrix for non-paired elements
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! In  matr_asse        : matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nt_patch
    integer :: i_patch, indi_cont, nume_equa
    integer :: jv_matr_sxdi, jv_matr_valm
    character(len=24) :: sdcont_stat
    integer, pointer :: v_sdcont_stat(:) => null()
    character(len=24) :: sdcont_ddlc
    integer, pointer :: v_sdcont_ddlc(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Get parameters
!
    nt_patch = ds_contact%nt_patch
!
! - Acces to contact objects
!
    sdcont_stat = ds_contact%sdcont_solv(1:14)//'.STAT'
    sdcont_ddlc = ds_contact%sdcont_solv(1:14)//'.DDLC'
    call jeveuo(sdcont_stat, 'L', vi = v_sdcont_stat)
    call jeveuo(sdcont_ddlc, 'L', vi = v_sdcont_ddlc)
!
! - Acces to matrix
!
    call mtdsc2(matr_asse, 'SXDI', 'L', jv_matr_sxdi)
    call jeveuo(jexnum(matr_asse//'.VALM', 1), 'E', jv_matr_valm)
!
! - Loop on patches
!
    do i_patch = 1, nt_patch
        indi_cont = v_sdcont_stat(i_patch)
        nume_equa = v_sdcont_ddlc(i_patch)
        if (indi_cont .eq. -1) then
            zr(jv_matr_valm-1+zi(jv_matr_sxdi-1+nume_equa)) = 1.d0
        end if
    end do
!
    call jedema()
end subroutine 

