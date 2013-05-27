function mefac1(n, m)
    implicit none
!
    integer :: n, m
    real(kind=8) :: mefac1
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ----------------------------------------------------------------------
!     CALCUL DE L'EXPRESSION FACTORIELLE SUIVANTE :
!     N!/(M-1)!/(N-M)! = N(N-1)...(N-M+1)/(M-1)/.../1
!     OPERATEUR APPELANT : OP0144 , FLUST3, MEFIST, MEFMAT
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    mefac1 = n
    do 1 i = 1, m-1
        mefac1 = mefac1*(n-i)/(m-i)
 1  end do
!
end function
