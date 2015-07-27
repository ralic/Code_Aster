subroutine calcul_cara_maille( coord, noeuds, topologie, surface, centre, normale )
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
!
! --------------------------------------------------------------------------------------------------
!
!     Calcul pour une maille : centre de gravité, surface
!       coord       : coordonnées des noeuds rangées suivant les conventions (x,y,z)
!       noeuds      : noeuds de l'élément
!       topologie   : topologie de la maille (1:ligne 2:surface)
!       surface     :   1       : surface totale de la maille,
!                       2       : surface totale de la maille / nombre de noeuds
!       cdg         : cdg des noeuds sommet de la maille
!       normale     : si topologie=2 ==> vecteur normal à la face, normé
!                                 =1 ==> vecteur tangent au segment, normé
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
implicit none
        real(kind=8),intent(in)    :: coord(*)
        integer,intent(in)         :: noeuds(:)
        integer,intent(in)         :: topologie
        real(kind=8),optional,intent(out)   :: surface(*)
        real(kind=8),optional,intent(out)   :: centre(*)
        real(kind=8),optional,intent(out)   :: normale(*)
!
#include "asterfort/provec.h"
#include "blas/ddot.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: ii, pta, ptb, ptc, ptd, inoe, nbnoeu
    real(kind=8) :: cdg(3), vectab(3), vectcd(3), vect(3), surf
! --------------------------------------------------------------------------------------------------
!   centre de gravité de la maille
    cdg(:) = 0.0
    do ii = lbound(noeuds,1), ubound(noeuds,1)
        inoe = 3*(noeuds(ii)-1)
        cdg(1:3) = cdg(1:3) + coord(inoe+1:inoe+3)
    enddo
    nbnoeu = size(noeuds)
    cdg(1:3) = cdg(1:3) / nbnoeu
!   Surface de la maille
    pta = 0; ptb = 0; ptc = 0; ptd = 0; surf = 0.0
    if (topologie.eq.1) then
        pta = 3*(noeuds(1)-1)
        ptb = 3*(noeuds(2)-1)
        vectab(1:3) = coord(ptb+1:ptb+3) - coord(pta+1:pta+3)
        surf = ddot(2,vectab,1,vectab,1)
        surf = sqrt(surf)
        vect(1:3) = vectab(1:3)/surf
    else if (topologie.eq.2) then
        pta = 3*(noeuds(1)-1)
        ptb = 3*(noeuds(3)-1)
        if (nbnoeu.eq.3 .or. nbnoeu.eq.6 .or. nbnoeu.eq.7) then
            ptc = 3*(noeuds(1)-1)
            ptd = 3*(noeuds(2)-1)
        else if (nbnoeu.eq.4 .or. nbnoeu.eq.8 .or. nbnoeu.eq.9) then
            ptc = 3*(noeuds(2)-1)
            ptd = 3*(noeuds(4)-1)
        endif
        vectab(1:3) = coord(ptb+1:ptb+3) - coord(pta+1:pta+3)
        vectcd(1:3) = coord(ptd+1:ptd+3) - coord(ptc+1:ptc+3)
        call provec(vectab, vectcd, vect)
        surf = ddot(3,vect,1,vect,1)
        vect = vect(1:3)/sqrt(surf)
        surf = sqrt(surf)*0.5d0
    endif
!   Resultat
    if ( present(surface) ) then
        surface(1)   = surf
        surface(2)   = surf / nbnoeu
    endif
    if ( present(centre) ) then
        centre(1:3) = cdg(1:3)
    endif
    if ( present(normale) ) then
        normale(1:3) = vect(1:3)
    endif
end subroutine
