subroutine pmfasseinfo(tygrfi, nbfibr, nbcarm, cara, mxfiass, nbfiass, gxjxpou)
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
!           Informations sur les PMF :  assemblage de groupes de fibres
!
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
!   OUT
!       tygrfi      : type des groupes de fibres
!       nbfibr      : nombre total de fibre
!       nbcarm      : nombre de composantes dans la carte
!       cara        : caractéristiques des fibres
!       mxfiass     : nombre maximum de fibre par assemblage
!       nbfiass     : nombre de fibre par assemblage
!       gxjxpou     : constante de torsion par assemblage        
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
!
    integer, intent(in) :: nbfibr, tygrfi, nbcarm
    real(kind=8), intent(in) :: cara(nbcarm,nbfibr)
    integer, intent(out):: mxfiass
    integer, pointer, intent(out) :: nbfiass(:)
    real(kind=8), pointer, intent(out) :: gxjxpou(:)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ii, numgr, nbassfi
!
! --------------------------------------------------------------------------------------------------
!
!
    if ( tygrfi .eq. 1 ) then
        mxfiass    = nbfibr
        ASSERT( size(nbfiass) .eq. 1 )
        nbfiass(1) = nbfibr
    else if ( tygrfi .eq. 2 ) then
        nbassfi = 0
        do ii = 1 , nbfibr
            numgr   = nint( cara(nbcarm,ii) )
            nbassfi = max( nbassfi , numgr )
        enddo
        ASSERT( nbassfi .ne. 0 )
!
        ASSERT( size(nbfiass) .eq. nbassfi )
        nbfiass(1:nbassfi) = 0
        do ii = 1 , nbfibr
            numgr = nint( cara(nbcarm,ii) )
            nbfiass(numgr) = nbfiass(numgr) + 1
            gxjxpou(numgr) = cara(nbcarm-1,ii)
        enddo
        mxfiass = 0
        do ii = 1 , nbassfi
            mxfiass = max( mxfiass , nbfiass(ii) )
        enddo
        ASSERT( mxfiass .ne. 0 )
    else
        call utmess('F', 'ELEMENTS2_40', si=tygrfi)
    endif
end subroutine
