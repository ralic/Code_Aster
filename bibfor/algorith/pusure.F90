subroutine pusure(nbpt, fn, vt1, vt2, iadh,&
                  t, pusee)
!***********************************************************************
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
!
!       CALCUL DE LA PUISSANCE D USURE (LOI D ARCHARD)
!
!
!
    implicit none
    real(kind=8) :: fn(*), vt1(*), vt2(*), t(*), pusee
    integer :: iadh(*)
!
!-----------------------------------------------------------------------
    integer :: i, nbpt
!-----------------------------------------------------------------------
    pusee=0.00d00
!
    do 10 i = 1, nbpt-1
        if (iadh(i) .eq. 0) then
            pusee=pusee+ (abs(fn(i+1)*sqrt(vt1(i+1)**2+vt2(i+1)**2))+&
            abs(fn(i)*sqrt(vt1(i)**2+vt2(i)**2)))*(t(i+1)-t(i))
        endif
10  continue
!
    pusee = pusee / 2.d0
    pusee=pusee/(t(nbpt)-t(1))
!
end subroutine
