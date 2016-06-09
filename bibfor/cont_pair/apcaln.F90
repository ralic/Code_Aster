subroutine apcaln(sdappa, mesh, sdcont_defi, newgeo)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/aptgen.h"
#include "asterfort/aptgno.h"
#include "asterfort/apverl.h"
#include "asterfort/infdbg.h"
#include "asterfort/sdmpic.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: sdappa
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
    character(len=19), intent(in) :: newgeo
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Compute tangents
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: one_proc
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('APPARIEMENT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> ... CALCUL DES TANGENTES SUR TOUS LES NOEUDS'
    endif
    one_proc=.false.
!
! - Compute tangents at each node for each element
!
    call aptgen(sdappa, mesh, sdcont_defi, newgeo)
!
! - All-reduce for tangents field by element
!
    if (.not. one_proc) then
        call sdmpic('SD_APPA_TGEL',sdappa)
    endif
!
! - Compute 
!
    call aptgno(sdappa, mesh, sdcont_defi)
!
! - All-reduce for tangents at each node field
!   
    if (.not. one_proc) then
        call sdmpic('SD_APPA_TGNO',sdappa)   
    endif 
!
! - Check normals discontinuity
!
    
    call apverl(sdappa, mesh, sdcont_defi)
    
!
end subroutine
