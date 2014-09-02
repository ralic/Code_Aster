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
implicit none
    character(len=*) :: option, nomte
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
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
    integer :: codres(1)
    character(len=16) :: ch16, phenom
    real(kind=8) :: rho(1), a, xl, r8b
!
! --------------------------------------------------------------------------------------------------
!
    integer :: lcastr, lmater, lsect, igeom
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
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
    call jevech('PCAGNBA', 'L', lsect)
    a = zr(lsect)
!
!   Longueur de l'élément
    if (nomte .eq. 'MECA_BARRE') then
        call lonele(3, igeom, xl)
    else if (nomte.eq.'MECA_2D_BARRE') then
        call lonele(2, igeom, xl)
    endif
!
!     --- CALCUL DES CARACTERISTIQUES ELEMENTAIRES ----
    if (option .eq. 'MASS_INER') then
        call jevech('PMASSINE', 'E', lcastr)
!        --- MASSE ET CDG DE L'ELEMENT ---
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
!        --- INERTIE DE L'ELEMENT ---
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
