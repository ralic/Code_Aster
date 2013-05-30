subroutine regup2(x0, y0, y0p, y1, a,&
                  b, c)
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! REGUP2 :
!  CALCULE UN  POLYNOME P DU SECOND DEGRE TEL QUE
!  P(X)=AX**2 +BX +C
!  P(X0)=Y0, DPDX(X0)=Y0P, P(1)=Y1
    implicit none
    real(kind=8) :: x0, y0, y0p, y1, a, b, c
!
    a=(y1-y0+y0p*(x0-1.d0))/((1.d0-x0)**2)
    b= -2.d0*a*x0 + y0p
    c = y1 - a - b
end subroutine
