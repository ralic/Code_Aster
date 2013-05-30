subroutine sh2ksi(npint, xxg, xyg, xzg, bksi)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!*
!*     -----------------------------------------------------------------
!*
!*         DERIVEE P/R VARIABLES CANONIQUE AUX POINTS D INTEGRATION
!*          POUR LES ELEMENTS CUBES A 20 NOEUDS
!*     -----------------------------------------------------------------
!*
!*
!*   ENTREES
!*     NPINT   : NBRE DE  PTS D INTEGRATION
!*     XXG,XYG,XZG      : COOR. CANONIQUES DES  PTS D INTEGRATION
!*
!*   SORTIES :
!*     BKSI(3,20,NPINT) : LES DERIVEES
!*
    implicit none
!*
!*---    VARIABLES GLOBALES
!*
    integer :: npint
    real(kind=8) :: xg(3), xxg(20), xyg(20), xzg(20)
    real(kind=8) :: bksi(3, 20, 20)
!*
!*---    VARIABLES LOCALES
    integer :: ip
!
    do 10 ip = 1, npint
        xg(1)=xxg(ip)
        xg(2)=xyg(ip)
        xg(3)=xzg(ip)
!
        bksi(1,1,ip)= -0.125d0*(1.d0-xg(2))*(1.d0-xg(3))* (-1.d0-2.d0*&
        xg(1)-xg(2)-xg(3))
        bksi(1,2,ip)= 0.125d0*(1.d0-xg(2))*(1.d0-xg(3))* (-1.d0+2.d0*&
        xg(1)-xg(2)-xg(3))
        bksi(1,3,ip)= 0.125d0*(1.d0+xg(2))*(1.d0-xg(3))* (-1.d0+2.d0*&
        xg(1)+xg(2)-xg(3))
        bksi(1,4,ip)= -0.125d0*(1.d0+xg(2))*(1.d0-xg(3))* (-1.d0-2.d0*&
        xg(1)+xg(2)-xg(3))
        bksi(1,5,ip)= -0.125d0*(1.d0-xg(2))*(1+xg(3))* (-1.d0-2.d0*xg(&
        1)-xg(2)+xg(3))
        bksi(1,6,ip)= 0.125d0*(1.d0-xg(2))*(1.d0+xg(3))* (-1.d0+2.d0*&
        xg(1)-xg(2)+xg(3))
        bksi(1,7,ip)= 0.125d0*(1.d0+xg(2))*(1.d0+xg(3))* (-1.d0+2.d0*&
        xg(1)+xg(2)+xg(3))
        bksi(1,8,ip)= -0.125d0*(1.d0+xg(2))*(1.d0+xg(3))* (-1.d0-2.d0*&
        xg(1)+xg(2)+xg(3))
        bksi(1,9,ip)= -0.5d0*xg(1)*(1.d0-xg(2))*(1.d0-xg(3))
        bksi(1,10,ip)= 0.25d0*(1.d0-xg(2)*xg(2))*(1.d0-xg(3))
        bksi(1,11,ip)= -0.5d0*xg(1)*(1.d0+xg(2))*(1.d0-xg(3))
        bksi(1,12,ip)= -0.25d0*(1.d0-xg(2)*xg(2))*(1.d0-xg(3))
        bksi(1,13,ip)= -0.25d0*(1.d0-xg(2))*(1.d0-xg(3)*xg(3))
        bksi(1,14,ip)= 0.25d0*(1.d0-xg(2))*(1.d0-xg(3)*xg(3))
        bksi(1,15,ip)= 0.25d0*(1.d0+xg(2))*(1.d0-xg(3)*xg(3))
        bksi(1,16,ip)= -0.25d0*(1.d0+xg(2))*(1.d0-xg(3)*xg(3))
        bksi(1,17,ip)= -0.5d0*xg(1)*(1.d0-xg(2))*(1.d0+xg(3))
        bksi(1,18,ip)= 0.25d0*(1.d0-xg(2)*xg(2))*(1.d0+xg(3))
        bksi(1,19,ip)= -0.5d0*xg(1)*(1.d0+xg(2))*(1.d0+xg(3))
        bksi(1,20,ip)= -0.25d0*(1.d0-xg(2)*xg(2))*(1.d0+xg(3))
!
        bksi(2,1,ip)= -0.125d0*(1.d0-xg(1))*(1.d0-xg(3))* (-1.d0-xg(1)&
        -2.d0*xg(2)-xg(3))
        bksi(2,2,ip)= -0.125d0*(1.d0+xg(1))*(1.d0-xg(3))* (-1.d0+xg(1)&
        -2.d0*xg(2)-xg(3))
        bksi(2,3,ip)= 0.125d0*(1.d0+xg(1))*(1.d0-xg(3))* (-1.d0+xg(1)+&
        2.d0*xg(2)-xg(3))
        bksi(2,4,ip)= 0.125d0*(1.d0-xg(1))*(1.d0-xg(3))* (-1.d0-xg(1)+&
        2.d0*xg(2)-xg(3))
        bksi(2,5,ip)= -0.125d0*(1.d0-xg(1))*(1+xg(3))* (-1.d0-xg(1)-&
        2.d0*xg(2)+xg(3))
        bksi(2,6,ip)= -0.125d0*(1.d0+xg(1))*(1.d0+xg(3))* (-1.d0+xg(1)&
        -2.d0*xg(2)+xg(3))
        bksi(2,7,ip)= 0.125d0*(1.d0+xg(1))*(1.d0+xg(3))* (-1.d0+xg(1)+&
        2.d0*xg(2)+xg(3))
        bksi(2,8,ip)= 0.125d0*(1.d0-xg(1))*(1.d0+xg(3))* (-1.d0-xg(1)+&
        2.d0*xg(2)+xg(3))
        bksi(2,9,ip)= -0.25d0*(1.d0-xg(1)*xg(1))*(1.d0-xg(3))
        bksi(2,10,ip)= -0.5d0*xg(2)*(1.d0+xg(1))*(1.d0-xg(3))
        bksi(2,11,ip)= 0.25d0*(1.d0-xg(1)*xg(1))*(1.d0-xg(3))
        bksi(2,12,ip)= -0.5d0*xg(2)*(1.d0-xg(1))*(1.d0-xg(3))
        bksi(2,13,ip)= -0.25d0*(1.d0-xg(1))*(1.d0-xg(3)*xg(3))
        bksi(2,14,ip)= -0.25d0*(1.d0+xg(1))*(1.d0-xg(3)*xg(3))
        bksi(2,15,ip)= 0.25d0*(1.d0+xg(1))*(1.d0-xg(3)*xg(3))
        bksi(2,16,ip)= 0.25d0*(1.d0-xg(1))*(1.d0-xg(3)*xg(3))
        bksi(2,17,ip)= -0.25d0*(1.d0-xg(1)*xg(1))*(1.d0+xg(3))
        bksi(2,18,ip)= -0.5d0*xg(2)*(1.d0+xg(1))*(1.d0+xg(3))
        bksi(2,19,ip)= 0.25d0*(1.d0-xg(1)*xg(1))*(1.d0+xg(3))
        bksi(2,20,ip)= -0.5d0*xg(2)*(1.d0-xg(1))*(1.d0+xg(3))
!
        bksi(3,1,ip)= -0.125d0*(1.d0-xg(1))*(1.d0-xg(2))* (-1.d0-xg(1)&
        -xg(2)-2.d0*xg(3))
        bksi(3,2,ip)= -0.125d0*(1.d0+xg(1))*(1.d0-xg(2))* (-1.d0+xg(1)&
        -xg(2)-2.d0*xg(3))
        bksi(3,3,ip)= -0.125d0*(1.d0+xg(1))*(1.d0+xg(2))* (-1.d0+xg(1)&
        +xg(2)-2.d0*xg(3))
        bksi(3,4,ip)= -0.125d0*(1.d0-xg(1))*(1.d0+xg(2))* (-1.d0-xg(1)&
        +xg(2)-2.d0*xg(3))
        bksi(3,5,ip)= 0.125d0*(1.d0-xg(1))*(1-xg(2))* (-1.d0-xg(1)-xg(&
        2)+2.d0*xg(3))
        bksi(3,6,ip)= 0.125d0*(1.d0+xg(1))*(1.d0-xg(2))* (-1.d0+xg(1)-&
        xg(2)+2.d0*xg(3))
        bksi(3,7,ip)= 0.125d0*(1.d0+xg(1))*(1.d0+xg(2))* (-1.d0+xg(1)+&
        xg(2)+2.d0*xg(3))
        bksi(3,8,ip)= 0.125d0*(1.d0-xg(1))*(1.d0+xg(2))* (-1.d0-xg(1)+&
        xg(2)+2.d0*xg(3))
        bksi(3,9,ip)= -0.25d0*(1.d0-xg(1)*xg(1))*(1.d0-xg(2))
        bksi(3,10,ip)= -0.25d0*(1.d0-xg(2)*xg(2))*(1.d0+xg(1))
        bksi(3,11,ip)= -0.25d0*(1.d0-xg(1)*xg(1))*(1.d0+xg(2))
        bksi(3,12,ip)= -0.25d0*(1.d0-xg(2)*xg(2))*(1.d0-xg(1))
        bksi(3,13,ip)= -0.5d0*xg(3)*(1.d0-xg(1))*(1.d0-xg(2))
        bksi(3,14,ip)= -0.5d0*xg(3)*(1.d0+xg(1))*(1.d0-xg(2))
        bksi(3,15,ip)= -0.5d0*xg(3)*(1.d0+xg(1))*(1.d0+xg(2))
        bksi(3,16,ip)= -0.5d0*xg(3)*(1.d0-xg(1))*(1.d0+xg(2))
        bksi(3,17,ip)= 0.25d0*(1.d0-xg(1)*xg(1))*(1.d0-xg(2))
        bksi(3,18,ip)= 0.25d0*(1.d0-xg(2)*xg(2))*(1.d0+xg(1))
        bksi(3,19,ip)= 0.25d0*(1.d0-xg(1)*xg(1))*(1.d0+xg(2))
        bksi(3,20,ip)= 0.25d0*(1.d0-xg(2)*xg(2))*(1.d0-xg(1))
!
10  end do
!
end subroutine
