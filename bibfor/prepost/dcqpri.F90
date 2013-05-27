subroutine dcqpri(coorp1, coorp2, coori, sprim)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
!************************************
!          BUT DE CETTE ROUTINE :   *
! CALCUL LA SURFACE D UN QUADRANGLE *
!************************************
!
! IN   COOR*  : COORDONNEES DES NOEUDS DU QUADRANGLE
! OUT  SPRIM  : AIRE DU QUADRANGLE
!
    implicit none
!
! DECLARATION GLOBALE
!
    real(kind=8) :: coorp1(2), coorp2(2), coori(2, 2), sprim
!
! DECLARATION LOCALE
!
    real(kind=8) :: x1, y1, x2, y2, x3, y3, x4, y4
!
    x1 = coorp1(1)
    y1 = coorp1(2)
    x2 = coori(1,1)
    y2 = coori(2,1)
    x3 = coori(1,2)
    y3 = coori(2,2)
    x4 = coorp2(1)
    y4 = coorp2(2)
!
    sprim =((x2-x1)*(y4-y1)-(y2-y1)*(x4-x1))&
     &     +((x4-x3)*(y2-y3)-(y4-y3)*(x2-x3))
!
    sprim = abs(sprim)/2.d+0
!
end subroutine
