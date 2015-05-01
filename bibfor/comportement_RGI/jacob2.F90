subroutine jacob2(a, d, s)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!
!       DIAGONALISATION DE A      SYMETRIQUE
!   ENTREEES
!        A(3,3) = MATRICE A DIAGONALISER  A(1,3)=A(2,3)=0.
!                                         A(3,1)=A(3,2)=0.
!   SORTIES
!        D(3)   =  LES 3 VALEURS PROPRES D(1) > D(2) ET D(3)=A(3,3)
!        S(3,3) =  VECTEURS PROPRES    S(I,3)=0. 0. 1.
!
!        RECUPERATION INCA FEVRIER 85    EBERSOLT
!====================================================================
    implicit none
    real(kind=8) :: a(3, *)
    real(kind=8) :: d(*)
    real(kind=8) :: s(3, *)
    real(kind=8) :: x1, x2, x3, x4, x5, x6, x7
!
    x1  =2.*a(1,2)
    x2  =a(1,1)-a(2,2)
    x3  =sqrt(x2*x2+x1*x1)
    d(1)=0.5*(a(1,1)+a(2,2)+x3)
    d(2)=d(1)-x3
    d(3)=a(3,3)
    s(3,1)=0.
    s(3,2)=0.
    s(1,3)=0.
    s(2,3)=0.
    s(3,3)=1.
    if (x2 .eq. 0.) goto 70
    x4=x1/x2
    if (abs(x4) .lt. 1.e+10) goto 50
!
 70 continue
    x5    =sqrt(2.d0)*.5
    s(1,1)=x5
    s(2,1)=sign(x5,x1)
    s(1,2)=-s(2,1)
    s(2,2)=x5
    goto 100
!
 50 continue
    x5=1. + x4*x4
    x5=sign(1.d0,x2)/sqrt(x5)
    x6=(1.+x5)*.5
    x6=sqrt(x6)
    x7=(1.-x5)*.5
    x7=sign(1.d0,x1)*sqrt(x7)
    s(1,1)= x6
    s(2,1)= x7
    s(1,2)=-x7
    s(2,2)= x6
    100 end subroutine
