subroutine dcspri(coorp, coori, sprim)
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
!*******************************************************************
!              BUT DE CETTE ROUTINE :                              *
! CALCUL DE LA SURFACE D UN TRIANGLE                               *
!*******************************************************************
!
! IN   COOR*  : COORDONNEES DES NOEUDS DU TRIANGLE
! OUT  SPRIM  : AIRE DU TRIANGLE
!
    implicit none
!
! DECLARATION GLOBALE
!
    real(kind=8) :: coorp(2), coori(2, 2), sprim
!
! DECLARATION LOCALE
!
    real(kind=8) :: xc, yc, xi, yi, xj, yj
!
    xc = coorp(1)
    yc = coorp(2)
    xi = coori(1,1)
    yi = coori(2,1)
    xj = coori(1,2)
    yj = coori(2,2)
!
    sprim=abs((xj-xi)*(yc-yi)-(yj-yi)*(xc-xi))
    sprim=sprim/2.d+0
!
end subroutine
