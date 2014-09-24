subroutine b3d_degre3(as0,as1,as2,xr1,xi1,xr2,xi2,xr3,xi3)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!     passage test en double precision a.sellier dim. 29 août 2010 07:48:35
!===================================================================
    implicit none
        real(kind=8) :: as0
        real(kind=8) :: as1
        real(kind=8) :: as2
        real(kind=8) :: xr1
        real(kind=8) :: xi1
        real(kind=8) :: xr2
        real(kind=8) :: xi2
        real(kind=8) :: xr3
        real(kind=8) :: xi3,som,tr,r,d,q,sd,s1,s2,ro,arg,dif

!-inc ccoptio
!
!
!        polynome de degre 3 sous la forme
!        x3 + as2*x2 + as1*x + as0 = 0
!
!
!
      tr=dsqrt(3.d0)
      q=as1/3.d0-as2*as2/9.d0
      r=(as1*as2-3.d0*as0)/6.d0-as2*as2*as2/27.d0
      d=q*q*q+r*r
!
!      if(iimpi.eq.9) then
!        write(6,*) 'q  ',q
!        write(6,*) 'r  ',r
!        write(6,*) 'd  ',d
!      endif
!
      if ( d < 0.d0 ) then
      sd=dsqrt(-d)
      ro=(r*r-d)**(1.d0/6.d0)
      arg=atan2(sd,r)/3.d0
      if (dabs(arg).lt. 1.d-7) arg=0.d0
      som=ro*dcos(arg)
      dif=ro*dsin(arg)
      xr1=som*2.d0-as2/3.d0
      xi1=0.d0
      xr2=-som-as2/3.d0-tr*dif
      xi2=0.d0
      xr3=-som-as2/3.d0+tr*dif
      xi3=0.d0
    else
      sd=dsqrt(d)
      s1=sign(1.d0,r+sd)*((dabs(r+sd))**(1.d0/3.d0))
      s2=sign(1.d0,r-sd)*((dabs(r-sd))**(1.d0/3.d0))
      xr1=s1+s2-as2/3.d0
      xi1=0.d0
      xr2=-(s1+s2)/2.d0-as2/3.d0
      xi2=tr*(s1-s2)/2.d0
      xr3=xr2
      xi3=-xi2
  endif
end subroutine
