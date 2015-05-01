subroutine mm_cycl_crsd(sd_cont_defi,sd_cont_solv)
!
    implicit     none
!
#include "asterfort/cfdisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sd_cont_defi
    character(len=24), intent(in) :: sd_cont_solv
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Creating data structures
!
! --------------------------------------------------------------------------------------------------
!
!
! In  sd_cont_solv : data structure for contact solving
! In  sd_cont_defi : data structure from contact definition 
!
! --------------------------------------------------------------------------------------------------
!
    integer :: point_number, zone_number
    character(len=24) :: sd_cycl_lis
    integer, pointer :: p_cycl_lis(:) => null()
    character(len=24) :: sd_cycl_nbr
    integer, pointer :: p_cycl_nbr(:) => null()
    character(len=24) :: sd_cycl_eta
    integer, pointer :: p_cycl_eta(:) => null()
    character(len=24) :: sd_cycl_his
    real(kind=8), pointer :: p_cycl_his(:) => null()
    character(len=24) :: sd_cycl_coe
    real(kind=8), pointer :: p_cycl_coe(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    point_number = cfdisi(sd_cont_defi,'NTPC' )
    zone_number  = cfdisi(sd_cont_defi,'NZOCO' )
!
! - Status saving (coded integer)
!
    sd_cycl_lis = sd_cont_solv(1:14)//'.CYCLIS'
!
! - Cycling length
!
    sd_cycl_nbr = sd_cont_solv(1:14)//'.CYCNBR'
!
! - Cycling state
!
    sd_cycl_eta = sd_cont_solv(1:14)//'.CYCETA'
!
! - Cycling history 
!
    sd_cycl_his = sd_cont_solv(1:14)//'.CYCHIS'
!
! - Informations about ratios 
!
    sd_cycl_coe = sd_cont_solv(1:14)//'.CYCCOE'
!
! - Creating cycling objects
!
    call wkvect(sd_cycl_lis, 'V V I', 4*point_number, vi = p_cycl_lis)
    call wkvect(sd_cycl_nbr, 'V V I', 4*point_number, vi = p_cycl_nbr)
    call wkvect(sd_cycl_eta, 'V V I', 4*point_number, vi = p_cycl_eta)
    call wkvect(sd_cycl_his, 'V V R', 25*point_number, vr = p_cycl_his)
    call wkvect(sd_cycl_coe, 'V V R', 6*zone_number, vr = p_cycl_coe)
!
    call jedema()
end subroutine
