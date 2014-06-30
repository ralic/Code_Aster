subroutine i3ctpv(epsi, noeud, nbn, coordo, pave,&
                  coupe)
    implicit none
!
    integer :: noeud(*), nbn
    real(kind=8) :: epsi, coordo(*), pave(*)
    logical(kind=1) :: coupe
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     REPONSE A LA QUESTION : UN PAVE DONNE COUPE-T-IL LE PLUS PETIT
!     PAVE CONTENANT UN ENSEMBLE DE NOEUDS
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  NOEUD  : I : TABLE DES NOEUDS
! IN  NBN    : I : NOMBRE DE NOEUDS
! IN  COORDO : R : TABLE DES COORDONNEES
! IN  PAVE   : R : DESCRIPTEUR DU PAVE (XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX)
! OUT COUPE  : L : REPONSE
!     ------------------------------------------------------------------
!
    integer :: i, j
    real(kind=8) :: xmin, xmax, ymin, ymax, zmin, zmax, a1, a2, b1, b2, c1, c2
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    xmin = 1.0d50
    xmax = -1.0d50
    ymin = 1.0d50
    ymax = -1.0d50
    zmin = 1.0d50
    zmax = -1.0d50
    do 100, i = 1, nbn, 1
    j = 3*(noeud(i)-1)
    xmin = min(xmin,coordo(j+1))
    xmax = max(xmax,coordo(j+1))
    ymin = min(ymin,coordo(j+2))
    ymax = max(ymax,coordo(j+2))
    zmin = min(zmin,coordo(j+3))
    zmax = max(zmax,coordo(j+3))
    100 end do
    a1 = max(pave(1),xmin)
    a2 = min(pave(4),xmax)
    b1 = max(pave(2),ymin)
    b2 = min(pave(5),ymax)
    c1 = max(pave(3),zmin)
    c2 = min(pave(6),zmax)
    coupe = (&
            ( (a1.le.a2) .or. (abs(a1-a2).le.epsi) ) .and.&
            ( (b1.le.b2) .or. (abs(b1-b2).le.epsi) ) .and.&
            ( (c1.le.c2) .or. (abs(c1-c2).le.epsi) )&
            )
end subroutine
