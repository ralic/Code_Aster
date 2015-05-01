subroutine btdmsr(nb1, nb2, ksi3s2, intsr, xr,&
                  epais, vectpt, hsj1m, hsj1s, btdm,&
                  btds)
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
!
    implicit none
    integer :: nb1, nb2, intsr
    real(kind=8) :: xr(*), epais, vectpt(9, 2, 3)
    real(kind=8) :: hsj1m(3, 9), hsj1s(2, 9), btdm(4, 3, 42), btds(4, 2, 42)
    real(kind=8) :: dnsdsm(9, 42), dnsds(9, 42)
    real(kind=8) :: ksi3s2
    common/dnsms/dnsdsm,dnsds
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, i3, i4, i5, intsr1
    integer :: intsr2, j, j1, jb, k, k1, l1
    integer :: l2, l3, l4, l5
!-----------------------------------------------------------------------
    l1= 44
    l2= 76
    l3=351
    l4=387
    l5=423
!
    do 15 i = 1, 9
        do 16 j = 1, 5*nb1+2
            dnsdsm(i,j)=0.d0
            dnsds(i,j)=0.d0
            if (i .le. 3) btdm(intsr,i,j)=0.d0
            if (i .le. 2) btds(intsr,i,j)=0.d0
16      end do
15  end do
!
    intsr1=8*(intsr-1)
    intsr2=9*(intsr-1)
!
!                         DN             DN
!     CONSTRUCTION DE   ------    ET   ------   AUX PTS DE GAUSS REDUITS
!                        DQSI  M        DQSI
!
    i1=l1+intsr1
    i2=l2+intsr1
    i3=l3+intsr2
    i4=l4+intsr2
    i5=l5+intsr2
!
    do 30 j = 1, nb1
        j1=5*(j-1)
        dnsdsm(1,j1+1)=xr(i1+j)
        dnsdsm(2,j1+1)=xr(i2+j)
!
        dnsdsm(4,j1+2)=xr(i1+j)
        dnsdsm(5,j1+2)=xr(i2+j)
!
        dnsdsm(7,j1+3)=xr(i1+j)
        dnsdsm(8,j1+3)=xr(i2+j)
!
        dnsds(1,j1+1)=xr(i1+j)
        dnsds(1,j1+4)=-ksi3s2*xr(i4+j)*epais*vectpt(j,2,1)
        dnsds(1,j1+5)= ksi3s2*xr(i4+j)*epais*vectpt(j,1,1)
!
        dnsds(2,j1+1)=xr(i2+j)
        dnsds(2,j1+4)=-ksi3s2*xr(i5+j)*epais*vectpt(j,2,1)
        dnsds(2,j1+5)= ksi3s2*xr(i5+j)*epais*vectpt(j,1,1)
!
        dnsds(3,j1+4)=-xr(i3+j)/2*epais*vectpt(j,2,1)
        dnsds(3,j1+5)= xr(i3+j)/2*epais*vectpt(j,1,1)
!
        dnsds(4,j1+2)=xr(i1+j)
        dnsds(4,j1+4)=-ksi3s2*xr(i4+j)*epais*vectpt(j,2,2)
        dnsds(4,j1+5)= ksi3s2*xr(i4+j)*epais*vectpt(j,1,2)
!
        dnsds(5,j1+2)=xr(i2+j)
        dnsds(5,j1+4)=-ksi3s2*xr(i5+j)*epais*vectpt(j,2,2)
        dnsds(5,j1+5)= ksi3s2*xr(i5+j)*epais*vectpt(j,1,2)
!
        dnsds(6,j1+4)=-xr(i3+j)/2*epais*vectpt(j,2,2)
        dnsds(6,j1+5)= xr(i3+j)/2*epais*vectpt(j,1,2)
!
        dnsds(7,j1+3)=xr(i1+j)
        dnsds(7,j1+4)=-ksi3s2*xr(i4+j)*epais*vectpt(j,2,3)
        dnsds(7,j1+5)= ksi3s2*xr(i4+j)*epais*vectpt(j,1,3)
!
        dnsds(8,j1+3)=xr(i2+j)
        dnsds(8,j1+4)=-ksi3s2*xr(i5+j)*epais*vectpt(j,2,3)
        dnsds(8,j1+5)= ksi3s2*xr(i5+j)*epais*vectpt(j,1,3)
!
        dnsds(9,j1+4)=-xr(i3+j)/2*epais*vectpt(j,2,3)
        dnsds(9,j1+5)= xr(i3+j)/2*epais*vectpt(j,1,3)
30  end do
!
    dnsds(1,5*nb1+1)=-ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,2,1)
    dnsds(1,5*nb1+2)= ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,1,1)
!
    dnsds(2,5*nb1+1)=-ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,2,1)
    dnsds(2,5*nb1+2)= ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,1,1)
!
    dnsds(3,5*nb1+1)=-xr(i3+nb2)/2*epais*vectpt(nb2,2,1)
    dnsds(3,5*nb1+2)= xr(i3+nb2)/2*epais*vectpt(nb2,1,1)
!
    dnsds(4,5*nb1+1)=-ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,2,2)
    dnsds(4,5*nb1+2)= ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,1,2)
!
    dnsds(5,5*nb1+1)=-ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,2,2)
    dnsds(5,5*nb1+2)= ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,1,2)
!
    dnsds(6,5*nb1+1)=-xr(i3+nb2)/2*epais*vectpt(nb2,2,2)
    dnsds(6,5*nb1+2)= xr(i3+nb2)/2*epais*vectpt(nb2,1,2)
!
    dnsds(7,5*nb1+1)=-ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,2,3)
    dnsds(7,5*nb1+2)= ksi3s2*xr(i4+nb2)*epais*vectpt(nb2,1,3)
!
    dnsds(8,5*nb1+1)=-ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,2,3)
    dnsds(8,5*nb1+2)= ksi3s2*xr(i5+nb2)*epais*vectpt(nb2,1,3)
!
    dnsds(9,5*nb1+1)=-xr(i3+nb2)/2*epais*vectpt(nb2,2,3)
    dnsds(9,5*nb1+2)= xr(i3+nb2)/2*epais*vectpt(nb2,1,3)
!
!     CONSTRUCTION DE BTILDM = HFM * S * JTILD-1 * DNSDSM  : (3,5*NB1+2)
!
    do 40 i = 1, 3
        do 50 jb = 1, nb1
            do 60 j = 1, 3
                j1=j+5*(jb-1)
                btdm(intsr,i,j1)=0.d0
                do 70 k = 1, 2
                    k1=k+3*(j-1)
                    btdm(intsr,i,j1)=btdm(intsr,i,j1)+hsj1m(i,k1)*&
                    dnsdsm(k1,j1)
70              end do
60          end do
50      end do
        btdm(intsr,i,5*nb1+1)=0.d0
        btdm(intsr,i,5*nb1+2)=0.d0
40  end do
!
!     CONSTRUCTION DE BTILDS = HS * S * JTILD-1 * DNSDS  : (2,5*NB1+2)
!
    do 80 i = 1, 2
        do 90 jb = 1, nb1
            do 100 j = 1, 5
                j1=j+5*(jb-1)
                btds(intsr,i,j1)=0
                if (j .le. 3) then
                    do 110 k = 1, 2
                        k1=k+3*(j-1)
                        btds(intsr,i,j1)=btds(intsr,i,j1)+hsj1s(i,k1)*&
                        dnsds(k1,j1)
110                  end do
                else
                    do 120 k = 1, 9
                        btds(intsr,i,j1)=btds(intsr,i,j1)+hsj1s(i,k)*&
                        dnsds(k,j1)
120                  end do
                endif
100          end do
90      end do
        do 130 j = 1, 2
            j1=5*nb1+j
            btds(intsr,i,j1)=0.d0
            do 140 k = 1, 9
                btds(intsr,i,j1)=btds(intsr,i,j1)+hsj1s(i,k)*dnsds(k,&
                j1)
140          end do
130      end do
80  end do
end subroutine
