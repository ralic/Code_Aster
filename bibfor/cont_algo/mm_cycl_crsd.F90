subroutine mm_cycl_crsd(sdcont_defi, sdcont_solv)
!
implicit none
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
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Creating data structures
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cont_poin, nb_cont_zone
    character(len=24) :: sdcont_cyclis
    integer, pointer :: p_sdcont_cyclis(:) => null()
    character(len=24) :: sdcont_cycnbr
    integer, pointer :: p_sdcont_cycnbr(:) => null()
    character(len=24) :: sdcont_cyceta
    integer, pointer :: p_sdcont_cyceta(:) => null()
    character(len=24) :: sdcont_cychis
    real(kind=8), pointer :: p_sdcont_cychis(:) => null()
    character(len=24) :: sdcont_cyccoe
    real(kind=8), pointer :: p_sdcont_cyccoe(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nb_cont_poin = cfdisi(sdcont_defi,'NTPC' )
    nb_cont_zone  = cfdisi(sdcont_defi,'NZOCO' )
!
! - Status saving (coded integer)
!
    sdcont_cyclis = sdcont_solv(1:14)//'.CYCLIS'
!
! - Cycling length
!
    sdcont_cycnbr = sdcont_solv(1:14)//'.CYCNBR'
!
! - Cycling state
!
    sdcont_cyceta = sdcont_solv(1:14)//'.CYCETA'
!
! - Cycling history
!
    sdcont_cychis = sdcont_solv(1:14)//'.CYCHIS'
!
! - Informations about ratios
!
    sdcont_cyccoe = sdcont_solv(1:14)//'.CYCCOE'
!
! - Creating cycling objects
!
    call wkvect(sdcont_cyclis, 'V V I', 4*nb_cont_poin, vi = p_sdcont_cyclis)
    call wkvect(sdcont_cycnbr, 'V V I', 4*nb_cont_poin, vi = p_sdcont_cycnbr)
    call wkvect(sdcont_cyceta, 'V V I', 4*nb_cont_poin, vi = p_sdcont_cyceta)
    call wkvect(sdcont_cychis, 'V V R', 25*nb_cont_poin, vr = p_sdcont_cychis)
    call wkvect(sdcont_cyccoe, 'V V R', 6*nb_cont_zone, vr = p_sdcont_cyccoe)
!
    call jedema()
end subroutine
