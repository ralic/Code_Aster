subroutine pmfitg(typfib, nf, ncarf, vf, vs)
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
! --------------------------------------------------------------------------------------------------
!
!                           Intégrations sur la section des PMF
!
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
! IN
!   typfib  : type des fibres : 1 ou 2
!   nf      : nombre de fibres
!   ncarf   : nombre de caractéristiques par fibre
!           !!! Quand "pmfitg" est appelé par pmfd00 avec vf qui pointe sur la SD fibres
!               le nombre de composantes doit être en relation avec le type de groupe de fibres
!           !!! Quand "pmfitg" est appelé sous un TE avec PFIBRES des catalogues de poutres
!               le nombre de composantes est le maximum de tous les types (info dans PNBSP_I)
!   vf(*)   : positions des fibres
!       Types 1 et 2
!          vf(1,*) : Y fibres
!          vf(2,*) : Z fibres
!          vf(3,*) : Aire fibres
!       Types 2
!          vf(4,*) : Yp groupes de fibres
!          vf(5,*) : Zp groupes de fibres
!          vf(6,*) : num du groupe
!
! OUT
!   vs(1) : surface totale              : somme(ds)
!   vs(2) : moment statique / oz        : somme(y.ds)
!   vs(3) : moment statique / oy        : somme(z.ds)
!   vs(4) : moment quadratique / oz     : somme(y.y.ds)
!   vs(5) : moment quadratique / oy     : somme(z.z.ds)
!   vs(6) : moment produit              : somme(y.z.ds)
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "asterfort/utmess.h"
!
    integer :: typfib, nf, ncarf
    real(kind=8) :: vf(ncarf, nf), vs(6)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ii
    real(kind=8) :: yy, zz, aire
!
! --------------------------------------------------------------------------------------------------
!
    vs(:) = 0.0d0
!
    if ( typfib.eq.1 ) then
!       3 caractéristiques utiles par fibre : y  z  aire
        do ii = 1, nf
            yy   = vf(1,ii)
            zz   = vf(2,ii)
            aire = vf(3,ii)
!
            vs(1) = vs(1) + aire
            vs(2) = vs(2) + yy*aire
            vs(3) = vs(3) + zz*aire
            vs(4) = vs(4) + yy*yy*aire
            vs(5) = vs(5) + zz*zz*aire
            vs(6) = vs(6) + yy*zz*aire
        enddo
    else if ( typfib.eq.2 ) then
!       6 caractéristiques utiles par fibre : y  z  aire  yp  zp  numgr
        do ii = 1, nf
            yy   = vf(1,ii)
            zz   = vf(2,ii)
            aire = vf(3,ii)
!
            vs(1) = vs(1) + aire
            vs(2) = vs(2) + yy*aire
            vs(3) = vs(3) + zz*aire
            vs(4) = vs(4) + yy*yy*aire
            vs(5) = vs(5) + zz*zz*aire
            vs(6) = vs(6) + yy*zz*aire
        enddo
    else
        call utmess('F', 'ELEMENTS2_40', si=typfib)
    endif
!
end subroutine
