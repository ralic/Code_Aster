subroutine damage(curvvp, bend, k, dmax, dam,&
                  tanmrp, alpha, beta, gamma)
!
    implicit  none
!-----------------------------------------------------------------------
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
!     CALCUL L EVOLUTION DES VARIABLES D ENDOMMAGEMENT AINSI QUE LES
!     TERMES DE LA MATRICE TANGENT LIEES A L EVOLUTION DE L ENDO
!
! IN  CURVVP : VALEURS PROPRES DU TENSEUR DES COURBURES ELASTIQUES
! IN  BEND : FLEXION POSITIVE (1) OU NEGATIVE (-1)
! IN  K : SEUIL D ENDOMMAGEMENT
! IN  DMAX : ENDOMMAGEMNT MAXIMAL
! IN  ALPHA : PARAMETRE  MATERIAU
! IN  BETA : PARAMETRE  MATERIAU
! IN  GAMMA : PARAMETRE  MATERIAU
!
! OUT DAM : ENDOMMAGEMENT
! OUT TANMRP : TERME DE LA MATRICE TANGENTE LIEE A L ENDOMMAGEMENT
!
!
#include "asterfort/r8inir.h"
    integer :: bend
!
    real(kind=8) :: curvvp(2), k, dmax, alpha, beta, gamma
    real(kind=8) :: dam, tanmrp(3, 3)
    real(kind=8) :: trcurv, xtr, xvp(2), w, damtst, coef
!
    trcurv = curvvp(1)+curvvp(2)
!
    xtr = 0.d0
!
    call r8inir(2, 0.0d0, xvp, 1)
!
!     EVALUATION DES FONCTIONS HEAVISIDE H(tr(k**e)), H(K_i**e)
    if (trcurv*bend .gt. 0.d0) xtr = alpha * trcurv
    if (curvvp(1)*bend .gt. 0.d0) xvp(1) = beta * curvvp(1)
    if (curvvp(2)*bend .gt. 0.d0) xvp(2) = beta * curvvp(2)
!
    w= xtr * trcurv + xvp(1) * curvvp(1) + xvp(2) * curvvp(2)
!
    damtst = (sqrt((1.d0-gamma)/k*w)-1.d0)
!
    call r8inir(3*3, 0.0d0, tanmrp, 1)
!
    if (damtst .gt. dam) then
!     CONDITION POUR QUE L EVOLUTION DE L ENDOMMAGEMENT SOIT POSITIVE
        if (damtst .ge. dmax) then
!     A T ON ATTEINT L ENDOMMAGEMENT MAX
            dam=dmax
        else
            dam=damtst
            coef = 2.d0*(gamma-1.d0)/((1.d0+dam)*w)
            tanmrp(1,1) = coef * (xtr + xvp(1))**2
            tanmrp(2,2) = coef * (xtr + xvp(2))**2
            tanmrp(1,2) = coef * (xtr + xvp(1)) * (xtr + xvp(2))
            tanmrp(2,1) = tanmrp(1,2)
        endif
    endif
!
end subroutine
