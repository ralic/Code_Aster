function calor(alpha0, k0, t, dt, depsv,&
               dp1, dp2, signe, alp11, alp12,&
               coeps)
    implicit      none
    real(kind=8) :: alpha0, k0, t, dt, depsv, dp1, dp2, alp11, alp12, signe
    real(kind=8) :: coeps
    real(kind=8) :: calor
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
    calor = alpha0*3.0d0*k0*(t-dt/2.0d0)*depsv + 3.0d0*alp11*(t-dt/2.0d0)*signe*dp1 - 3.0d0*(alp1&
            &1+alp12)*(t-dt/2.d0)*dp2 + coeps*dt
! ======================================================================
end function
