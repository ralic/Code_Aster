subroutine pmfinfo(nbfibr,nbgrfi,tygrfi,nbcarm,nug)
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
!
! --------------------------------------------------------------------------------------------------
!
!           Informations sur les PMF
!
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
!   OUT
!       nbfibr  : nombre total de fibre
!       nbgrfi  : nombre de groupe de fibres
!       tygrfi  : type des groupes de fibres
!       nbcarm  : nombre de composantes dans la carte
!       nug     : numéro des groupes de fibres nug(1:nbgrfi)
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
!
    integer, intent(out) :: nbfibr, nbgrfi, tygrfi, nbcarm, nug(*)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jnbspi
!
! --------------------------------------------------------------------------------------------------
!
    call jevech('PNBSP_I', 'L', jnbspi)
    nbfibr = zi(jnbspi)
    nbgrfi = zi(jnbspi+1)
    tygrfi = zi(jnbspi+2)
    nbcarm = zi(jnbspi+3)
    nug(1:nbgrfi) = zi(jnbspi+3+1:jnbspi+3+nbgrfi)
!
end subroutine
