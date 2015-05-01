subroutine pmfdef(typfib, nf, ncarf, vf, dege, deff)
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
!                       DEFORMATION AXIALE DES FIBRES
!
! --------------------------------------------------------------------------------------------------
!
!   IN
!       typfib  : type des fibres : 1 ou 2
!       nf      : nombre de fibres
!       ncarf   : nombre de caractéristiques sur chaque fibre
!       vf(*)   : positions des fibres
!           Types 1 et 2
!               vf(1,*) : Y fibres
!               vf(2,*) : Z fibres
!               vf(3,*) : Aire fibres
!           Types 2
!               vf(4,*) : Yp groupes de fibres
!               vf(5,*) : Zp groupes de fibres
!               vf(6,*) : num du groupe
!       dege(6) : deformations generalisees sur l'element
!
!   out
!       deff(nf) : deformations axiales des fibres
!
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!

    implicit none
#include "asterfort/utmess.h"
!
    integer :: typfib, nf, ncarf
    real(kind=8) :: vf(ncarf, nf), dege(6), deff(nf)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ii
    real(kind=8) :: yy,zz
!
! --------------------------------------------------------------------------------------------------
!
    if ( typfib.eq.1 ) then
!       3 caractéristiques utiles par fibre : y z aire
        do ii = 1, nf
            yy   = vf(1,ii)
            zz   = vf(2,ii)
            deff(ii) = dege(1) - yy*dege(6) + zz*dege(5)
        enddo
    else if ( typfib.eq.2 ) then
!       6 caractéristiques utiles par fibre : y z aire yp zp numgr
        do ii = 1, nf
            yy   = vf(1,ii)
            zz   = vf(2,ii)
            deff(ii) = dege(1) - yy*dege(6) + zz*dege(5)
        enddo
    else
        call utmess('F', 'ELEMENTS2_40', si=typfib)
    endif

end subroutine
