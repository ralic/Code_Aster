subroutine fmater(nbfmax, nftab, tab)

use calcul_module, only : ca_jfpgl_, ca_nfpg_
implicit none

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
! person_in_charge: jacques.pellet at edf.fr

#include "jeveux.h"
#include "asterfort/assert.h"

    integer :: nbfmax, nftab
    character(len=8) :: tab(*)
!-----------------------------------------------------------------------
! but : recuperer les familles de points de gauss de la liste MATER
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------

    ASSERT(nbfmax .ge. ca_nfpg_)
    nftab=ca_nfpg_
    do i=1, ca_nfpg_
        tab(i)=zk8(ca_jfpgl_+i-1)
    end do

end subroutine
