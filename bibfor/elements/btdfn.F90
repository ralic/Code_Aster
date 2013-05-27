subroutine btdfn(ind, nb1, nb2, ksi3s2, intsn,&
                 xr, epais, vectpt, hsj1fx, btdf)
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
    implicit none
    integer :: nb1, nb2, intsn
    real(kind=8) :: xr(*), epais, vectpt(9, 2, 3)
    real(kind=8) :: hsj1fx(3, 9), btdf(3, 42)
    real(kind=8) :: dnsdsf(9, 42)
    real(kind=8) :: ksi3s2
    common/dnsf/dnsdsf
!
!-----------------------------------------------------------------------
    integer :: i, i3, i4, i5, ind, intsn1, j
    integer :: j1, jb, k, l1, l2, l3
!-----------------------------------------------------------------------
    if (ind .eq. 1) then
        l1=459
        l2=540
        l3=621
    else if (ind.eq.0) then
        l1=351
        l2=387
        l3=423
    endif
!
    do 15 i = 1, 9
        do 16 j = 1, 5*nb1+2
            dnsdsf(i,j)=0.d0
            if (i .le. 3) btdf(i,j)=0.d0
16      end do
15  end do
!
    intsn1=9*(intsn-1)
!
!                         DN
!     CONSTRUCTION DE   ------    AUX PTS DE GAUSS NORMAL
!                        DQSI  F
!
    i3=l1+intsn1
    i4=l2+intsn1
    i5=l3+intsn1
    do 30 j = 1, nb1
        j1=5*(j-1)
        dnsdsf(1,j1+4)=-ksi3s2*xr(i4+j)*epais*vectpt(j,2,1)
        dnsdsf(1,j1+5)= ksi3s2*xr(i4+j)*epais*vectpt(j,1,1)
!
        dnsdsf(2,j1+4)=-ksi3s2*xr(i5+j)*epais*vectpt(j,2,1)
        dnsdsf(2,j1+5)= ksi3s2*xr(i5+j)*epais*vectpt(j,1,1)
!
        dnsdsf(3,j1+4)=-xr(i3+j)/2*epais*vectpt(j,2,1)
        dnsdsf(3,j1+5)= xr(i3+j)/2*epais*vectpt(j,1,1)
!
        dnsdsf(4,j1+4)=-ksi3s2*xr(i4+j)*epais*vectpt(j,2,2)
        dnsdsf(4,j1+5)= ksi3s2*xr(i4+j)*epais*vectpt(j,1,2)
!
        dnsdsf(5,j1+4)=-ksi3s2*xr(i5+j)*epais*vectpt(j,2,2)
        dnsdsf(5,j1+5)= ksi3s2*xr(i5+j)*epais*vectpt(j,1,2)
!
        dnsdsf(6,j1+4)=-xr(i3+j)/2*epais*vectpt(j,2,2)
        dnsdsf(6,j1+5)= xr(i3+j)/2*epais*vectpt(j,1,2)
!
        dnsdsf(7,j1+4)=-ksi3s2*xr(i4+j)*epais*vectpt(j,2,3)
        dnsdsf(7,j1+5)= ksi3s2*xr(i4+j)*epais*vectpt(j,1,3)
!
        dnsdsf(8,j1+4)=-ksi3s2*xr(i5+j)*epais*vectpt(j,2,3)
        dnsdsf(8,j1+5)= ksi3s2*xr(i5+j)*epais*vectpt(j,1,3)
!
        dnsdsf(9,j1+4)=-xr(i3+j)/2*epais*vectpt(j,2,3)
        dnsdsf(9,j1+5)= xr(i3+j)/2*epais*vectpt(j,1,3)
30  end do
!
    dnsdsf(1,5*nb1+1)=-ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,2,1)
    dnsdsf(1,5*nb1+2)= ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,1,1)
!
    dnsdsf(2,5*nb1+1)=-ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,2,1)
    dnsdsf(2,5*nb1+2)= ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,1,1)
!
    dnsdsf(3,5*nb1+1)=-xr(i3+nb2)/2*epais*vectpt(nb2,2,1)
    dnsdsf(3,5*nb1+2)= xr(i3+nb2)/2*epais*vectpt(nb2,1,1)
!
    dnsdsf(4,5*nb1+1)=-ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,2,2)
    dnsdsf(4,5*nb1+2)= ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,1,2)
!
    dnsdsf(5,5*nb1+1)=-ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,2,2)
    dnsdsf(5,5*nb1+2)= ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,1,2)
!
    dnsdsf(6,5*nb1+1)=-xr(i3+nb2)/2*epais*vectpt(nb2,2,2)
    dnsdsf(6,5*nb1+2)= xr(i3+nb2)/2*epais*vectpt(nb2,1,2)
!
    dnsdsf(7,5*nb1+1)=-ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,2,3)
    dnsdsf(7,5*nb1+2)= ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,1,3)
!
    dnsdsf(8,5*nb1+1)=-ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,2,3)
    dnsdsf(8,5*nb1+2)= ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,1,3)
!
    dnsdsf(9,5*nb1+1)=-xr(i3+nb2)/2*epais*vectpt(nb2,2,3)
    dnsdsf(9,5*nb1+2)= xr(i3+nb2)/2*epais*vectpt(nb2,1,3)
!
!     CONSTRUCTION DE BTILDF = HFM * S * JTILD-1 * DNSDSF  : (3,5*NB1+2)
!
    do 40 i = 1, 3
        do 50 jb = 1, nb1
            do 60 j = 4, 5
                j1=j+5*(jb-1)
                btdf(i,j1)=0.d0
                do 70 k = 1, 9
                    btdf(i,j1)=btdf(i,j1)+hsj1fx(i,k)*dnsdsf(k,j1)
70              end do
60          end do
50      end do
!
        do 80 j = 1, 2
            j1=j+5*nb1
            btdf(i,j1)=0.d0
            do 90 k = 1, 9
                btdf(i,j1)=btdf(i,j1)+hsj1fx(i,k)*dnsdsf(k,j1)
90          end do
80      end do
40  end do
!
end subroutine
