subroutine sh1ksi(npint, xxg, xyg, xzg, bksi)
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
!*          POUR LES ELEMENTS PRISMATIQUES A 15 NOEUDS
!*                                                    V-D.TRINH     2006
!*     -----------------------------------------------------------------
!*
!*
!*   ENTREES
!*     NPINT   : NBRE DE  PTS D INTEGRATION
!*     XG  : COOR. CANONIQUES DES  PTS D INTEGRATION
!*
!*   SORTIES :
!*     BKSI(3,15,NPINT) : LES DERIVEES
!*
    implicit none
!*
!*---    VARIABLES GLOBALES
!*
    integer :: npint, ip
    real(kind=8) :: xg(3), xxg(15), xyg(15), xzg(15)
    real(kind=8) :: bksi(3, 15, 15)
!*
    do 10 ip = 1, npint
        xg(1)=xxg(ip)
        xg(2)=xyg(ip)
        xg(3)=xzg(ip)
!*
! CALCUL DE Ni,ksi
        bksi(1,1,ip)= -0.5d0*xg(2)*(2.d0*xg(2)-2.d0*xg(1)-1.d0)
        bksi(1,2,ip)= -0.5d0*xg(3)*(2.d0*xg(3)-2.d0*xg(1)-1.d0)
        bksi(1,3,ip)=(1.d0-xg(2)-xg(3))*(xg(1)+xg(2)+xg(3)-0.5d0)
        bksi(1,4,ip)= 0.5d0*xg(2)*(2.d0*xg(2)+2.d0*xg(1)-1.d0)
        bksi(1,5,ip)= 0.5d0*xg(3)*(2.d0*xg(3)+2.d0*xg(1)-1.d0)
        bksi(1,6,ip)= -(1.d0-xg(2)-xg(3))*(xg(2)+xg(3)-xg(1)-0.5d0)
        bksi(1,7,ip)= -2.d0*xg(2)*xg(3)
        bksi(1,8,ip)= -2.d0*xg(3)*(1.d0-xg(2)-xg(3))
        bksi(1,9,ip)= -2.d0*xg(2)*(1.d0-xg(2)-xg(3))
        bksi(1,10,ip)= -2.d0*xg(1)*xg(2)
        bksi(1,11,ip)= -2.d0*xg(1)*xg(3)
        bksi(1,12,ip)= -2.d0*xg(1)*(1.d0-xg(2)-xg(3))
        bksi(1,13,ip)= 2.d0*xg(2)*xg(3)
        bksi(1,14,ip)= 2.d0*xg(3)*(1.d0-xg(2)-xg(3))
        bksi(1,15,ip)= 2.d0*xg(2)*(1.d0-xg(2)-xg(3))
! CALUL DE Ni,eta
        bksi(2,1,ip)= 0.5d0*(1.d0-xg(1))*(4.d0*xg(2)-xg(1)-2.d0)
        bksi(2,2,ip)= 0.d0
        bksi(2,3,ip)=(1.d0-xg(1))*(2.d0*xg(2)+2.d0*xg(3)+0.5d0*xg(1)-&
        1.d0)
        bksi(2,4,ip)= 0.5d0*(1.d0+xg(1))*(4.d0*xg(2)+xg(1)-2.d0)
        bksi(2,5,ip)= 0.d0
        bksi(2,6,ip)=(1.d0+xg(1))*(2.d0*xg(2)+2.d0*xg(3)-0.5d0*xg(1)-&
        1.d0)
        bksi(2,7,ip)= 2.d0*xg(3)*(1.d0-xg(1))
        bksi(2,8,ip)= -2.d0*xg(3)*(1.d0-xg(1))
        bksi(2,9,ip)= 2.d0*(1.d0-xg(1))*(1.d0-2.d0*xg(2)-xg(3))
        bksi(2,10,ip)= 1.d0 -xg(1)*xg(1)
        bksi(2,11,ip)= 0.d0
        bksi(2,12,ip)= -1.d0 + xg(1)*xg(1)
        bksi(2,13,ip)= 2.d0*xg(3)*(1.d0+xg(1))
        bksi(2,14,ip)= -2.d0*xg(3)*(1.d0+xg(1))
        bksi(2,15,ip)= 2.d0*(1.d0+xg(1))*(1.d0-2.d0*xg(2)-xg(3))
! CALCUL DE Ni,zeta
        bksi(3,1,ip)= 0.d0
        bksi(3,2,ip)= 0.5d0*(1.d0-xg(1))*(4.d0*xg(3)-xg(1)-2.d0)
        bksi(3,3,ip)=(1.d0-xg(1))*(2.d0*xg(2)+2.d0*xg(3)+0.5d0*xg(1)-&
        1.d0)
        bksi(3,4,ip)= 0.d0
        bksi(3,5,ip)= 0.5d0*(1.d0+xg(1))*(4.d0*xg(3)+xg(1)-2.d0)
        bksi(3,6,ip)=(1.d0+xg(1))*(2.d0*xg(2)+2.d0*xg(3)-0.5d0*xg(1)-&
        1.d0)
        bksi(3,7,ip)= 2.d0*xg(2)*(1.d0-xg(1))
        bksi(3,8,ip)= 2.d0*(1.d0-xg(1))*(1.d0-2.d0*xg(3)-xg(2))
        bksi(3,9,ip)= -2.d0*xg(2)*(1.d0-xg(1))
        bksi(3,10,ip)= 0.d0
        bksi(3,11,ip)= 1.d0-xg(1)*xg(1)
        bksi(3,12,ip)= -1.d0+xg(1)*xg(1)
        bksi(3,13,ip)= 2.d0*xg(2)*(1.d0+xg(1))
        bksi(3,14,ip)= 2.d0*(1.d0+xg(1))*(1.d0-2.d0*xg(3)-xg(2))
        bksi(3,15,ip)= -2.d0*xg(2)*(1.d0+xg(1))
!*
10  end do
!*
end subroutine
