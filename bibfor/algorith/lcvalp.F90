subroutine lcvalp(t, valp)
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
    implicit none
    real(kind=8),intent(in) :: t(6)
    real(kind=8),intent(out):: valp(3)
! --------------------------------------------------------------------------------------------------
!  CALCUL DES VALEURS PROPRES D'UN TENSEUR SYMETRIQUE
! --------------------------------------------------------------------------------------------------
!  T      IN  TENSEUR (1:6) CODE AVEC RAC2 SUR LE CISAILLEMENT
!  VALP   OUT VALEURS PROPRES (1:3) DANS L'ORDRE DECROISSANT
! --------------------------------------------------------------------------------------------------
    real(kind=8),parameter,dimension(6):: kr=(/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/)
    real(kind=8),parameter:: pi = 4*atan(1.d0)
    real(kind=8),parameter:: rac2 = sqrt(2.d0)
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: p, d(6), s, s3, quatj3, ratio, th
! --------------------------------------------------------------------------------------------------
!
!  PREMIER INVARIANT
    p = (t(1)+t(2)+t(3))/3
!
!  DEVIATEUR ET SECOND INVARIANT (2/3 VON MISES)
    d = t - p*kr
    s = sqrt(2.d0*dot_product(d,d)/3.d0)
    s3 = s**3
!
!  TROISIEME INVARIANT (4 X DETERMINANT DE DEV)
    quatj3 = 4*d(1)*d(2)*d(3)-2*d(3)*d(4)**2+2*rac2*d(4)*d(5)*d(6)-2*d(1)*d(6)**2-2*d(2)*d(5)**2
!
!  ANGLE DE LODE
    if (abs(quatj3) .ge. s3) then
        ratio = sign(1.d0,quatj3)
    else
        ratio = quatj3/s3
        ratio = max(ratio,-1.d0)
        ratio = min(ratio, 1.d0)
    endif
    th = acos(ratio)/3
!
!  VALEURS PROPRES RANGEES DANS L'ORDRE DECROISSANT
    valp(1) = p + s*cos(th)
    valp(2) = p + s*cos(th-2*pi/3.d0)
    valp(3) = p + s*cos(th+2*pi/3.d0)
!
end subroutine
