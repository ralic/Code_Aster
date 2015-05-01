subroutine coqrep(pgl, alpha, beta, t2iu, t2ui,&
                  c, s)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"

    real(kind=8) :: pgl(3, 3), t2iu(*), t2ui(*), alpha, beta, c, s
!     ---------------------------------------------------
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
! person_in_charge: nicolas.sellenet at edf.fr
!     ------------------------------------------------------------------
!
!         CALCUL DE LA MATRICE DE PASSAGE DU REPERE INTRINSEQUE (ELEMENT) A CELUI
!         DE L'UTILISATEUR (VARIETE) (LE REPERE DE LA VARIETE EST OBTENU PAR LA MATRICE
!         DE PASSAGE GLOBAL -> LOCAL) AINSI QUE SON INVERSE
!
!         POUR TOUTES LES OPTIONS DE POST TRAITEMENT COQUE
!
!     ==> ALPHA, BETA EN RADIAN
!
!     ------------------------------------------------------------------
    real(kind=8) :: dx, dy, dz, norm
    real(kind=8) :: ps, pjdx, pjdy, pjdz
!     LE VECTEUR EST NORME
    dx = cos(beta)*cos(alpha)
    dy = cos(beta)*sin(alpha)
    dz = -sin(beta)
!   On vérifie que n = pgl(3,1:3) n'est pas de norme nulle
    norm = sqrt(dot_product(pgl(3,1:3),pgl(3,1:3)))
    ASSERT( norm.gt.r8prem() )
!   
    ps = dx*pgl(3,1) + dy*pgl(3,2) + dz*pgl(3,3)
    pjdx = dx - ps*pgl(3,1)
    pjdy = dy - ps*pgl(3,2)
    pjdz = dz - ps*pgl(3,3)
    norm = sqrt (pjdx*pjdx + pjdy*pjdy + pjdz*pjdz)
    if (norm .le. r8prem()) then
        call utmess('F', 'ELEMENTS_40')
    endif
!
    pjdx = pjdx/norm
    pjdy = pjdy/norm
    pjdz = pjdz/norm
    c = pjdx*pgl(1,1) + pjdy*pgl(1,2) + pjdz*pgl(1,3)
    s = pjdx*pgl(2,1) + pjdy*pgl(2,2) + pjdz*pgl(2,3)
!
    c=c/sqrt(c**2+s**2)
    s=s/sqrt(c**2+s**2)
!
    t2iu(1) = c
    t2iu(2) = s
    t2iu(3) = - s
    t2iu(4) = c
!
    t2ui(1) = c
    t2ui(2) = - s
    t2ui(3) = s
    t2ui(4) = c
!
!
end subroutine
