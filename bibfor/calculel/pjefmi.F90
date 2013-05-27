subroutine pjefmi(elrefp, nnop, coor, xg, ndim,&
                  x1, x2, lext, xmi)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/elrfvf.h'
    include 'asterfort/vecini.h'
    character(len=8) :: elrefp
    integer :: nnop, ndim
    real(kind=8) :: coor(ndim*nnop)
    real(kind=8) :: xg(ndim), x1(ndim), x2(ndim), xmi(ndim)
!
! ----------------------------------------------------------------------
! BUT :
!   DETERMINER LES MEILLEURES COORDONNEES BARYCENTRIQUES ENTRE X1 ET X2
!   PERMET DE VERIFIER QUE LA ROUTINE REEREG A AMELIORE LA PRECISION
!   DES COORDONNEES BARYCENTRIQUES GROSSIERES DE LA 1ERE ETAPE DE
!   PROJ_CHAMP.
! ----------------------------------------------------------------------
!
!
! IN  ELREFP : TYPE DE L'ELEMENT
! IN  NNOP   : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  COOR   : COORDONNEES DS ESPACE REEL DES NOEUDS DE L'ELEMENT
! IN  XG     : COORDONNEES DU POINT DANS L'ESPACE REEL
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  X1     : COORDONNEES DU POINT 1 DANS L'ESPACE PARA DE L'ELEMENT
!              (RESULTAT DE LA PROJECTION "GROSSIERE")
! IN  X2     : COORDONNEES DU POINT 2 DANS L'ESPACE PARA DE L'ELEMENT
!              (RESULTAT DU RAFFINEMENT REEREG.F)
! IN  LEXT   : LE POINT X2 EST "EXTERIEUR" A LA MAILLE
! OUT XMI    : "X MIEUX" : RECOPIE DE X1 OU X2 (SELON LE CAS)
!
! ----------------------------------------------------------------------
    integer :: nbnomx
    parameter(nbnomx=27)
    real(kind=8) :: xr1(3), xr2(3), zero, d1, d2
    real(kind=8) :: ff(nbnomx)
    integer :: k, idim, ino, nno
    logical :: lext
! ----------------------------------------------------------------------
    zero=0.d0
!
!     -- SI LE POINT EST EXTERIEUR, ON S'INTERDIT L'EXTRAPOLATION
!        => XMI=X1
    if (lext) then
        do 61 k = 1, ndim
            xmi(k)=x1(k)
61      continue
        goto 9999
    endif
!
!     -- CALCUL DE XR1 : GEOMETRIE REELLE DE X1 :
    call elrfvf(elrefp, x1, nbnomx, ff, nno)
    call assert(nno.eq.nnop)
    call vecini(ndim, zero, xr1)
    do 20 idim = 1, ndim
        do 10 ino = 1, nno
            xr1(idim)=xr1(idim)+ff(ino)*coor(ndim*(ino-1)+idim)
10      continue
20  end do
!
!
!     -- CALCUL DE XR2 : GEOMETRIE REELLE DE X2 :
    call elrfvf(elrefp, x2, nbnomx, ff, nno)
    call vecini(ndim, zero, xr2)
    do 40 idim = 1, ndim
        do 30 ino = 1, nno
            xr2(idim)=xr2(idim)+ff(ino)*coor(ndim*(ino-1)+idim)
30      continue
40  end do
!
!
!     -- QUELLE EST LA MEILLEURE APPROXIMATION DE XG ?
    d1=zero
    d2=zero
    do 50 k = 1, ndim
        d1=d1+(xr1(k)-xg(k))**2
        d2=d2+(xr2(k)-xg(k))**2
50  end do
!
    if (d1 .le. d2) then
        do 60 k = 1, ndim
            xmi(k)=x1(k)
60      continue
    else
        do 70 k = 1, ndim
            xmi(k)=x2(k)
70      continue
    endif
!
9999  continue
end subroutine
