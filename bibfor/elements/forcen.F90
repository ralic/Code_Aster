subroutine forcen(rnormc, intsn, nb1, xi, xr,&
                  rho, epais, vomega, vecl1, xa)
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
    implicit none
!
    integer :: intsn, nb1, intsx, ie(3, 3, 3)
    real(kind=8) :: wgt, rho
    real(kind=8) :: xi(3, *), xr(*), vomega(3), vecl1(42), xa(3)
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, ib, ik, ip, iq
    integer :: ir, j, jb, k, l1
    real(kind=8) :: epais, rnormc
!-----------------------------------------------------------------------
    wgt=xr(127-1+intsn)
!
    do 30 i = 1, 3
        do 20 j = 1, 3
            do 10 k = 1, 3
                ie(i,j,k)=0
10          continue
20      continue
30  end do
    ie(1,2,3)=1
    ie(1,3,2)=-1
    ie(2,1,3)=-1
    ie(2,3,1)=1
    ie(3,1,2)=1
    ie(3,2,1)=-1
!
    l1=135
    intsx=8*(intsn-1)
!
    i1=l1+intsx
!
    do 90 ib = 1, nb1
        i2=5*(ib-1)
        do 80 iq = 1, 3
            do 70 ip = 1, 3
                do 60 ik = 1, 3
                    do 50 ir = 1, 3
                        do 40 jb = 1, nb1
                            vecl1(i2+1)=vecl1(i2+1)-wgt*rho*epais*&
                            rnormc* ie(iq,ip,ik)*ie(1,ir,iq)*vomega(&
                            ir)* vomega(ip)*(xi(ik,jb)-xa(ik))*xr(i1+&
                            ib)* xr(i1+jb)
!
                            vecl1(i2+2)=vecl1(i2+2)-wgt*rho*epais*&
                            rnormc* ie(iq,ip,ik)*ie(2,ir,iq)*vomega(&
                            ir)* vomega(ip)*(xi(ik,jb)-xa(ik))*xr(i1+&
                            ib)* xr(i1+jb)
!
                            vecl1(i2+3)=vecl1(i2+3)-wgt*rho*epais*&
                            rnormc* ie(iq,ip,ik)*ie(3,ir,iq)*vomega(&
                            ir)* vomega(ip)*(xi(ik,jb)-xa(ik))*xr(i1+&
                            ib)* xr(i1+jb)
40                      continue
50                  continue
60              continue
70          continue
80      continue
90  end do
end subroutine
