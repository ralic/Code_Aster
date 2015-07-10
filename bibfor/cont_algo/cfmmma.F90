subroutine cfmmma(sdcont_defi, sdcont_solv)
!
implicit none
!
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfmmci.h"
#include "asterfort/infdbg.h"
#include "asterfort/utmess.h"
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
! Contact - Solve
!
! Continue/Discrete method - Create datastructures for DISCRETE/CONTINUE methods
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_cont_poin, nb_cont_node_c, nb_cont_zone
    integer :: zeven, ztaco
    character(len=24) :: sdcont_evenco
    real(kind=8), pointer :: v_sdcont_evenco(:) => null()
    character(len=24) :: sdcont_evenpe
    real(kind=8), pointer :: v_sdcont_evenpe(:) => null()
    character(len=24) :: sdcont_jsupco
    real(kind=8), pointer :: v_sdcont_jsupco(:) => null()
    character(len=24) :: sdcont_tabcof
    real(kind=8), pointer :: v_sdcont_tabcof(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> . Create contact datastructures for DISCRETE/CONTINUE methods'
    endif
!
! - Get parameters
!
    nb_cont_poin   = cfdisi(sdcont_defi,'NTPC' )
    nb_cont_node_c = cfdisi(sdcont_defi,'NTNOEC')
    nb_cont_zone   = cfdisi(sdcont_defi,'NZOCO' )
    zeven          = cfmmvd('ZEVEN')
    ztaco          = cfmmvd('ZTACO')
!
! - Create datastructure for user's gaps
!
    sdcont_jsupco = sdcont_solv(1:14)//'.JSUPCO'
    call wkvect(sdcont_jsupco, 'V V R', nb_cont_poin, vr = v_sdcont_jsupco)
!
! - Create datastructure for event-driven management
!
    sdcont_evenco = sdcont_solv(1:14)//'.EVENCO'
    call wkvect(sdcont_evenco, 'V V R', zeven*nb_cont_poin, vr = v_sdcont_evenco)
    sdcont_evenpe = sdcont_solv(1:14)//'.EVENPE'
    call wkvect(sdcont_evenpe, 'V V R', 3*nb_cont_zone    , vr = v_sdcont_evenpe)
!
! - Print
!
    call utmess('I', 'MECANONLINE6_5', si=nb_cont_node_c)
!
! - Create datastructure for coefficients
! 
    sdcont_tabcof = sdcont_solv(1:14)//'.TABL.COEF'
    call wkvect(sdcont_tabcof, 'V V R', nb_cont_zone*ztaco, vr = v_sdcont_tabcof)
!
! - Init coefficients
!
    call cfmmci(sdcont_defi, sdcont_solv)
!
end subroutine
