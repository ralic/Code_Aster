subroutine sh6ksi(npint, xxg, bksi)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ------------------------------------------------------------------
!         DERIVEE P/R VARIABLES CANONIQUE AUX POINTS D INTEGRATION
!          POUR LES ELEMENTS PRISMATIQUES A 6 NOEUDS
!                                                    TRINH V.D     2007
!     ------------------------------------------------------------------
!
!   ENTREES
!     NPINT   : NBRE DE  PTS D INTEGRATION
!     XG      : COOR. CANONIQUES DES  PTS D INTEGRATION
!
!   SORTIES :
!     BKSI(3,6,NPINT) : LES DERIVEES
!
    implicit none
!
!   VARIABLES GLOBALES
!
    integer :: npint
    real(kind=8) :: xg(3, 5), xxg(5)
    real(kind=8) :: bksi(3, 6, 5)
!
!   VARIABLES LOCALES
!
    real(kind=8) :: un, zero, uns2, uns3
    integer :: ip
    zero=0.d0
    un=1.d0
    uns2=1.d0/2.d0
    uns3=1.d0/3.d0
!
! ON REMPLIT LE TABLEAU XG
!
    do 10 ip = 1, npint
        xg(1,ip)=uns3
        xg(2,ip)=uns3
        xg(3,ip)=xxg(ip)
10  end do
!
    do 20 ip = 1, npint
!
! CALCUL DE Ni,ksi
!
        bksi(1,1,ip)=uns2*(xg(3,ip)-un)
        bksi(1,2,ip)=-bksi(1,1,ip)
        bksi(1,3,ip)=zero
        bksi(1,4,ip)=uns2*(-xg(3,ip)-un)
        bksi(1,5,ip)=uns2*(xg(3,ip)+un)
        bksi(1,6,ip)=bksi(1,3,ip)
!
! CALUL DE Ni,eta
!
        bksi(2,1,ip)=bksi(1,1,ip)
        bksi(2,2,ip)=bksi(1,3,ip)
        bksi(2,3,ip)=bksi(1,2,ip)
        bksi(2,4,ip)=bksi(1,4,ip)
        bksi(2,5,ip)=bksi(1,3,ip)
        bksi(2,6,ip)=bksi(1,5,ip)
!
! CALCUL DE Ni,zeta
!
        bksi(3,1,ip)=uns2*(-un+xg(2,ip)+xg(1,ip))
        bksi(3,2,ip)=uns2*(-xg(1,ip))
        bksi(3,3,ip)=uns2*(-xg(2,ip))
        bksi(3,4,ip)=-bksi(3,1,ip)
        bksi(3,5,ip)=-bksi(3,2,ip)
        bksi(3,6,ip)=-bksi(3,3,ip)
!
20  end do
!
end subroutine
