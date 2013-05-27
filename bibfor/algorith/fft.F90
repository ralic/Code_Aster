subroutine fft(s, n, ifft)
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
    include 'jeveux.h'
    include 'asterc/r8pi.h'
    include 'asterfort/veri32.h'
    complex(kind=8) :: s(n)
!-----------------------------------------------------------------------
! IN,OUT : S    FONCTION A TRANSFORMER
! IN     : N    NOMBRE DE POINTS DE LA FONCTION
! IN     : IFFT > 0 => FFT
!               < 0 => FFT INVERSE
!
    complex(kind=8) :: u, w, t
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ifft, ip, isgn, j, k, l
    integer :: le, le1, m, n, n2, nm1, nv2
!
    real(kind=8) :: pi
!-----------------------------------------------------------------------
    m= int(log(dble(n))/log(2.d0))
    if (m .gt. 30) call veri32()
    n2 = 2**m
    if (n2 .ne. n) then
        m = m+1
        if (m .gt. 30) call veri32()
        n2 = 2**m
        if (n2 .ne. n) then
            m = m-2
        endif
    endif
    isgn = 1
    if (ifft .lt. 0) isgn=-1
    pi= r8pi()*isgn
    nm1=n-1
    j = 1
    nv2=n/2
    do 8 i = 1, nm1
        if (i .ge. j) goto 5
        t=s(j)
        s(j)=s(i)
        s(i)=t
 5      continue
        k=nv2
 6      continue
        if (k .ge. j) goto 7
        j=j-k
        k=k/2
        goto 6
 7      continue
        j=j+k
 8  end do
    do 20 l = 1, m
        if (l .gt. 30) call veri32()
        le=2**l
        le1=le/2
        u=(1.d0,0.d0)
        w=dcmplx(cos(-pi/dble(le1)),sin(-pi/dble(le1)))
        do 20 j = 1, le1
            do 10 i = j, n, le
                ip=i+le1
                t=s(ip)*u
                s(ip)=s(i)-t
                s(i)=s(i)+t
10          continue
            u=u*w
20      continue
    if (ifft .lt. 0) then
        do 30 i = 1, n2
            s(i) = s(i)/n2
30      continue
    endif
end subroutine
