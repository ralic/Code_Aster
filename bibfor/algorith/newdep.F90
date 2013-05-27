subroutine newdep(neq, c, dt, d0, v0,&
                  a0, d1, a1)
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
!**********************************************************************
    implicit none
!
!
!   INPUT:
!        NEQ   : NOMBRE D'EQUATIONS (D.D.L. ACTIFS)
!        C     : CONSTANTE DE CALCUL
!        DT    : PAS DE TEMPS DE CALCUL
!        D0    : VECTEUR DEPLACEMENT  INITIAL  (NEQ)
!        V0    : VECTEUR VITESSE      INITIALE (NEQ)
!        A0    : VECTEUR ACCELERATION INITIALE (NEQ)
!        A1    : VECTEUR ACCELERATION SOLUTION (NEQ)
!
!   OUTPUT:
!        D1    : VECTEUR DEPLACEMENT  SOLUTION (NEQ)
!
!  CALCUL DU DEPLACEMENT (WILSON)
!  ============================= DEPSOL = D0 + DT*V0 + C*(ACCSOL+2.A0)
!
!
!----------------------------------------------------------------------
!   E.D.F DER   JACQUART G. 47-65-49-41      LE 19 JUILLET 1990
!**********************************************************************
!
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    real(kind=8) :: d0(*), d1(*), v0(*), a0(*), a1(*), c, dt
!-----------------------------------------------------------------------
    integer :: neq
!-----------------------------------------------------------------------
    call dcopy(neq, d0, 1, d1, 1)
    call daxpy(neq, dt, v0, 1, d1,&
               1)
    call daxpy(neq, c, a1, 1, d1,&
               1)
    call daxpy(neq, 2*c, a0, 1, d1,&
               1)
end subroutine
