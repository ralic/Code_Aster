subroutine te0245(option, nomte)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! --------------------------------------------------------------------------------------------------
!
!           CALCUL DES TERMES PROPRES A UNE STRUCTURE  (ELEMENT DE BARRE)
!
! --------------------------------------------------------------------------------------------------
!
!   IN
!       OPTION  : 'MASS_INER      : CALCUL DES CARACTERISTIQUES DE STRUCTURES
!       NOMTE   :
!        'MECA_BARRE'       : BARRE
!        'MECA_2D_BARRE'    : BARRE 2D
!
! --------------------------------------------------------------------------------------------------
!
implicit none
    character(len=*) :: option, nomte
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: lcastr, lmater, lsect, igeom
    integer :: codres(1)
    real(kind=8) :: rho(1), a, xl, r8b
    character(len=16) :: ch16, phenom
!
! --------------------------------------------------------------------------------------------------
!
     call jevech('PMATERC', 'L', lmater)
!
    call rccoma(zi(lmater), 'ELAS', 1, phenom, codres(1))
!
    r8b = 0.0d0
    if (phenom.eq.'ELAS' .or. phenom.eq.'ELAS_ISTR' .or. phenom.eq.'ELAS_ORTH') then
        call rcvalb('FPG1', 1, 1, '+', zi(lmater),' ', phenom, 0, ' ', [r8b],&
                    1, 'RHO', rho, codres, 1)
        if (rho(1) .le. r8prem()) then
            call utmess('F', 'ELEMENTS5_45')
        endif
    else
        call utmess('F', 'ELEMENTS_50')
    endif
!
!   recuperation des caracteristiques generales des sections
    call jevech('PCAGNBA', 'L', lsect)
    a = zr(lsect)
!
!   Longueur de l'élément
    if (nomte .eq. 'MECA_BARRE') then
        xl = lonele(igeom=igeom)
    else if (nomte.eq.'MECA_2D_BARRE') then
        xl = lonele(dime=2, igeom=igeom)
    else
        xl = 0.0d0
        ASSERT( ASTER_FALSE )
    endif
!
!   calcul des caracteristiques elementaires
    if (option .eq. 'MASS_INER') then
        call jevech('PMASSINE', 'E', lcastr)
!       masse et cdg de l'element
        if (nomte .eq. 'MECA_BARRE') then
            zr(lcastr) = rho(1) * a * xl
            zr(lcastr+1) =( zr(igeom+4) + zr(igeom+1) ) / 2.d0
            zr(lcastr+2) =( zr(igeom+5) + zr(igeom+2) ) / 2.d0
            zr(lcastr+3) =( zr(igeom+6) + zr(igeom+3) ) / 2.d0
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(lcastr) = rho(1) * a * xl
            zr(lcastr+1) =( zr(igeom+3) + zr(igeom+1) ) / 2.d0
            zr(lcastr+2) =( zr(igeom+4) + zr(igeom+2) ) / 2.d0
        endif
!       inertie de l'element
        zr(lcastr+4) = 0.d0
        zr(lcastr+5) = 0.d0
        zr(lcastr+6) = 0.d0
        zr(lcastr+7) = 0.d0
        zr(lcastr+8) = 0.d0
        zr(lcastr+9) = 0.d0
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!
end subroutine
