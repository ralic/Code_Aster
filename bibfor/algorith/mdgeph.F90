subroutine mdgeph(neq, nbmode, bmodal, xgene, u)
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
!-----------------------------------------------------------------------
!     CONVERTIT EN BASE PHYSIQUE MODE VECTEUR
!     REM U EST INITIALISE A 0.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
!        NOM        MODE                    ROLE
!  ________________ ____ ______________________________________________
!    NEQ            <--   NB D'EQUATIONS DU SYSTEME ASSEMBLE
!    NBMODE         <--   NB DE MODES NORMAUX CONSIDERES
!    BMODAL         <--   BASE MODALE CONSIDEREE
!    XGENE          <--   VECTEUR DES COORDONNEES GENERALISEES
!    U              <--   VECTEUR DES COORDONNEES PHYSIQUES
! .________________.____.______________________________________________.
    implicit none
    real(kind=8) :: bmodal(neq, nbmode), xgene(nbmode), u(neq)
    integer :: i, j, nbmode, neq
!-----------------------------------------------------------------------
    do 10 i = 1, neq
        u(i)=0.0d0
        do 10 j = 1, nbmode
            u(i) = u(i) + bmodal(i,j)*xgene(j)
10      continue
end subroutine
