subroutine s6ksib(bksi)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     ------------------------------------------------------------------
!
!    DERIVEE P/R VARIABLES CANONIQUE AU POINT D INTEGRATION
!    POUR LES ELEMENTS PRISMATIQUES A 6 NOEUDS (POINT D ORIGINE)
!     ------------------------------------------------------------------
!
!
!   ENTREES
!     NPINT   : NBRE DE  PTS D INTEGRATION
!     XG      : COOR. CANONIQUES DES  PTS D INTEGRATION
!
!   SORTIES :
!     BKSI(3,6) : LES DERIVEES
!
    implicit none
!
!   VARIABLES GLOBALES
!
    real(kind=8) :: xg(3)
    real(kind=8) :: bksi(3, 6)
!
!   VARIABLES LOCALES
!
    real(kind=8) :: un, zero, uns2
    zero=0.d0
    un=1.d0
    uns2=1.d0/2.d0
!
! ON REMPLIT LE TABLEAU XG
!
    xg(1)=zero
    xg(2)=zero
    xg(3)=zero
! CALCUL DE Ni,ksi
    bksi(1,1)=uns2*(xg(3)-un)
    bksi(1,2)=-bksi(1,1)
    bksi(1,3)=zero
    bksi(1,4)=uns2*(-xg(3)-un)
    bksi(1,5)=uns2*(xg(3)+un)
    bksi(1,6)=bksi(1,3)
!
! CALUL DE Ni,eta
    bksi(2,1)=bksi(1,1)
    bksi(2,2)=bksi(1,3)
    bksi(2,3)=bksi(1,2)
    bksi(2,4)=bksi(1,4)
    bksi(2,5)=bksi(1,3)
    bksi(2,6)=bksi(1,5)
!
! CALCUL DE Ni,zeta
    bksi(3,1)=uns2*(-un+xg(2)+xg(1))
    bksi(3,2)=uns2*(-xg(1))
    bksi(3,3)=uns2*(-xg(2))
    bksi(3,4)=-bksi(3,1)
    bksi(3,5)=-bksi(3,2)
    bksi(3,6)=-bksi(3,3)
!
end subroutine
