subroutine histog(nbpt, v, vmin, vmax, x,&
                  y, ndec)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!        CACUL DE L'HISTOGRAMME AMV
!
! IN  : NBPT   : NB DE POINTS DU SIGNAL
! IN  : V      : TABLEAU DU SIGNAL
! IN  : NDEC   : NOMBRE DE CLASSES DE L'HISTOGRAMME
! OUT : VMIN   : VALEUR MINIMALE DU SIGNAL
! OUT : VMAX   : VALEUR MAXIMALE DU SIGNAL
! OUT : X      : TABLEAU DE VALEUR DES ABSCISSES DE L'HISTOGRAMME
! OUT : Y      : TABLEAU DES DENSITES DE PROBABILITE DES CLASSES
! ----------------------------------------------------------------------
!
    implicit none
    real(kind=8) :: v(*), x(*), y(*)
    real(kind=8) :: vmin, vmax
    integer :: nbpt, ndec
    real(kind=8) :: dx
    integer :: i, icel
    real(kind=8) :: coef
!-----------------------------------------------------------------------
!
    do 5 i = 1, ndec
        x(i)=0.d0
        y(i)=0.d0
 5  end do
    do 10 i = 1, nbpt
        if (v(i) .ge. vmax) vmax = v(i)
        if (v(i) .le. vmin) vmin = v(i)
10  end do
    if (nbpt .ne. 0) then
        dx = (vmax-vmin)/ndec
        coef = 1.d0/nbpt
    else
        dx =0.d0
        coef = 1.d0
        vmin = 0.d0
        vmax = 0.d0
    endif
    do 20 i = 1, nbpt
        if (dx .ne. 0.d0) then
            icel = int((v(i)-vmin)/dx)+1
        else
            icel = 1
        endif
        if (icel .gt. ndec) icel = ndec
        y(icel)=y(icel)+1
20  end do
    do 30 i = 1, ndec
        x(i)=vmin+i*dx
        y(i)=y(i)*coef
30  end do
end subroutine
