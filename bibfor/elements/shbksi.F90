subroutine shbksi(npint, xxg, bksi)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
!         DERIVEE P/R VARIABLES CANONIQUE AUX POINTS D INTEGRATION
!          POUR LES ELEMENTS CUBES A 8 NOEUDS
!   ENTREES
!     NPINT   : NBRE DE  PTS D INTEGRATION
!     XG      : COOR. CANONIQUES DES  PTS D INTEGRATION
!   SORTIES :
!     BKSI(3,8,NPINT) : LES DERIVEES
    implicit none
!
!---    VARIABLES GLOBALES
!
    integer :: npint
    real(kind=8) :: xg(3, 5), xxg(5)
    real(kind=8) :: bksi(3, 8, 5)
!
!---    VARIABLES LOCALES
!
    real(kind=8) :: xp(3), xm(3), aux
    real(kind=8) :: un, uns8
    integer :: ip, i
    un = 1.d0
    uns8 = 0.125d0
!
! ON REMPLIT LE TABLEAU XG
!
    do 10 ip = 1, npint
        xg(1,ip) = 0
        xg(2,ip) = 0
        xg(3,ip) = xxg(ip)
10  end do
!
    do 30 ip = 1, npint
!
        do 20 i = 1, 3
            xp(i) = un + xg(i,ip)
            xm(i) = un - xg(i,ip)
20      continue
!
        aux = -uns8*xm(3)
        bksi(1,1,ip) = aux*xm(2)
        bksi(1,2,ip) = -bksi(1,1,ip)
        bksi(1,3,ip) = -aux*xp(2)
        bksi(1,4,ip) = -bksi(1,3,ip)
        bksi(2,1,ip) = aux*xm(1)
        bksi(2,2,ip) = aux*xp(1)
        bksi(2,3,ip) = -bksi(2,2,ip)
        bksi(2,4,ip) = -bksi(2,1,ip)
        aux = -uns8*xp(3)
        bksi(1,5,ip) = aux*xm(2)
        bksi(1,6,ip) = -bksi(1,5,ip)
        bksi(1,7,ip) = -aux*xp(2)
        bksi(1,8,ip) = -bksi(1,7,ip)
        bksi(2,5,ip) = aux*xm(1)
        bksi(2,6,ip) = aux*xp(1)
        bksi(2,7,ip) = -bksi(2,6,ip)
        bksi(2,8,ip) = -bksi(2,5,ip)
        aux = -uns8*xm(2)
        bksi(3,1,ip) = aux*xm(1)
        bksi(3,2,ip) = aux*xp(1)
        bksi(3,5,ip) = -bksi(3,1,ip)
        bksi(3,6,ip) = -bksi(3,2,ip)
        aux = -uns8*xp(2)
        bksi(3,3,ip) = aux*xp(1)
        bksi(3,4,ip) = aux*xm(1)
        bksi(3,7,ip) = -bksi(3,3,ip)
        bksi(3,8,ip) = -bksi(3,4,ip)
30  end do
!
end subroutine
