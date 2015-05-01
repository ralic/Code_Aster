function calor(mdal, t, dt, deps, dp1,&
               dp2, signe, alp11, alp12, coeps,&
               ndim)
    implicit none
!
    integer :: i, ndim
    real(kind=8) :: t, dt, deps(6), dp1, dp2, alp11, alp12, signe, coeps
    real(kind=8) :: calor, mdal(6), calome, rac2
! ======================================================================
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
! ======================================================================
! --- CALCUL DE LA CHALEUR REDUITE Q' SELON FORMULE DOCR ---------------
! ======================================================================
    rac2 = sqrt(2.d0)
    calome = 0.d0
!
    do 10 i = 1, ndim
        calome=calome+mdal(i)*deps(i)*(t-dt/2.0d0)
10  end do
    do 20 i = ndim+1, 2*ndim
        calome=calome+mdal(i)*deps(i)*(t-dt/2.0d0)*rac2
20  end do
    calor = calome + 3.0d0*alp11*(t-dt/2.0d0)*signe*dp1 - 3.0d0*(alp11+alp12)*(t-dt/2.d0)*dp2 + c&
            &oeps*dt
! ======================================================================
end function
