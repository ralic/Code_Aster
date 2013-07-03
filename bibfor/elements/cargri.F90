subroutine cargri(lexc, densit, distn, dir11)
    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8vide.h"
#include "asterfort/jevech.h"
    logical :: lexc
    real(kind=8) :: densit, distn, dir11(3)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!         LECTURE DES CARACTERISTIQUES DES GRILLES
!
!  IN  LEXC : TRUE  SI GRILLE_EXCENTREE
!             FALSE SI GRILLE_MEMBRANE
!  OUT DENSIT : DENSITE D'ARMATURE
!  OUT DISTN  : EXCENTREMENT ( R8VIDE() SI LEXC = .FALSE.)
!  OUT DIR11  : DIRECTION DES ARMATURE
!
!     ------------------------------------------------------------------
    real(kind=8) :: alpha, beta
    integer :: icacoq
!
    call jevech('PCACOQU', 'L', icacoq)
!
    densit = zr(icacoq)
    alpha = zr(icacoq+1) * r8dgrd()
    beta = zr(icacoq+2) * r8dgrd()
    dir11(1) = cos(beta)*cos(alpha)
    dir11(2) = cos(beta)*sin(alpha)
    dir11(3) = - sin(beta)
!
    if (lexc) then
        distn = zr(icacoq+3)
    else
        distn = r8vide()
    endif
!
end subroutine
