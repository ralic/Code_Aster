subroutine extrai(nin, lchin, lpain, opt, nute,&
                  ligrel, init)
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "asterfort/assert.h"
#include "asterfort/extra1.h"
#include "asterfort/nbelem.h"
#include "asterfort/typele.h"
    integer :: nin, opt, nute
    character(len=*) :: lchin(*), init
    character(len=8) :: lpain(*)
    character(len=19) :: ligrel
! ----------------------------------------------------------------------
!     BUT: PREPARER LES CHAMPS LOCAUX "IN"
!
! ----------------------------------------------------------------------
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
!
!
! DEB-------------------------------------------------------------------
!
    call assert(init.eq.' '.or.init.eq.'INIT')
!
    if (calvoi .eq. 0) then
        if (init .ne. 'INIT') then
            call extra1(nin, lchin, lpain, opt, nute,&
                        ligrel)
        endif
    else
!       -- ON PREPARE TOUT LA 1ERE FOIS :
        if (init .eq. 'INIT') then
            do 1, igr=1,nbgr
            nbelgr=nbelem(ligrel,igr)
            nute=typele(ligrel,igr)
            call extra1(nin, lchin, lpain, opt, nute,&
                        ligrel)
 1          continue
        endif
    endif
!
!
end subroutine
