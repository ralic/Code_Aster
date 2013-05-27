subroutine lcelin(mod, nmat, materd, materf, deps,&
                  sigd, sigf)
    implicit none
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------
! INTEGRATION ELASTIQUE LINEAIRE ISOTROPE SUR DT
! IN  MOD    :  MODELISATION
!     NMAT   :  DIMENSION MATER
!     MATERD :  COEFFICIENTS MATERIAU A T
!     MATERF :  COEFFICIENTS MATERIAU A T+DT
!     SIGD   :  CONTRAINTE  A T
! VAR DEPS   :  INCREMENT DE DEFORMATION
! OUT SIGF   :  CONTRAINTE A T+DT
! ----------------------------------------------------------------
    include 'asterfort/lcopil.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcsove.h'
    integer :: nmat
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: sigd(6), sigf(6)
    real(kind=8) :: dkooh(6, 6), hookf(6, 6)
    real(kind=8) :: epsed(6), epsef(6), deps(6)
    character(len=8) :: mod
! ----------------------------------------------------------------
    if (int(materf(nmat,1)) .eq. 0) then
!
! --     OPERATEUR ELASTIQUE LINEAIRE ISOTROPE
!
        call lcopli('ISOTROPE', mod, materf(1, 1), hookf)
        call lcopil('ISOTROPE', mod, materd(1, 1), dkooh)
!
    else if (int(materf(nmat,1)).eq.1) then
!
! --     OPERATEUR ELASTIQUE LINEAIRE ORTHOTROPE
!
        call lcopli('ORTHOTRO', mod, materf(1, 1), hookf)
        call lcopil('ORTHOTRO', mod, materd(1, 1), dkooh)
    endif
!
!                                                        -1
! --  DEFORMATION ELASTIQUE A T ET T+DT : EPSEF = HOOKD  SIGD + DEPS
!
    call lcprmv(dkooh, sigd, epsed)
    call lcsove(epsed, deps, epsef)
!
! --  CONTRAINTES PLANES
!     DEPS3 = - ( H31 EPSEF1 + H32 EPSEF2 + H34 EPSEF4 )/H33 - EPSED3
!
    if (mod(1:6) .eq. 'C_PLAN') then
        deps(3) = - (&
                  hookf(3, 1) * epsef(1) + hookf(3, 2) * epsef(2) + hookf(3, 4) * epsef(4) ) / ho&
                  &okf(3,&
                  3) - epsed(3&
                  )
        call lcsove(epsed, deps, epsef)
    endif
!
! --  INTEGRATION ELASTIQUE : SIGF = HOOKF EPSEF (EPSEF MODIFIE EN CP)
!
    call lcprmv(hookf, epsef, sigf)
!
end subroutine
