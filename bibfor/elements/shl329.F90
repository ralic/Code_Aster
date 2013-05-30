subroutine shl329()
    implicit none
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
!....................................................................
!   CALCUL DES TERMES ELEMENTAIRES DE L'ACCEPTANCE
!     OPTION : ACCEPTANCE
!....................................................................
!
    include 'jeveux.h'
    include 'asterfort/codent.h'
    include 'asterfort/elref5.h'
    include 'asterfort/jevech.h'
    include 'asterfort/tecael.h'
    include 'asterfort/wkvect.h'
    character(len=7) :: ielem, imode
    character(len=24) :: vetel
    real(kind=8) :: sx(9, 9), sy(9, 9), sz(9, 9), jac(9)
    real(kind=8) :: nx(9), ny(9), nz(9), norm(3, 9), acc(3, 9)
    real(kind=8) :: flufn(9), acloc(3, 8), qsi, eta, zero, un
    real(kind=8) :: x(3, 9), ff(4, 4), dfdx(4, 4), dfdy(4, 4)
    integer :: igeom, ipg, iadzi, iazk24, ivetel
    integer :: iacce, iharm, ivectu, i, k, idim, ino, jno, j
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
!
! DEB ------------------------------------------------------------------
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
!
    call jevech('PACCELR', 'L', iacce)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PNUMMOD', 'L', iharm)
    call jevech('PVECTUR', 'E', ivectu)
!
    zero = 0.0d0
    un = 1.0d0
!
    do 1200 i = 1, nno
        acloc(1,i)=zero
        acloc(2,i)=zero
        acloc(3,i)=zero
1200  end do
!
    k=0
    do 1201 i = 1, nno
        do 20 idim = 1, 3
            k=k+1
            acloc(idim,i) = zr(iacce+k-1)
20      continue
1201  continue
!
    do 1052 ipg = 1, npg
        acc(1,ipg)=zero
        acc(2,ipg)=zero
        acc(3,ipg)=zero
1052  continue
!
    do 1061 ipg = 1, npg
!
        qsi = zr(icoopg-1+ndim*(ipg-1)+1)
        eta = zr(icoopg-1+ndim*(ipg-1)+2)
!
        ff(1,ipg)= (un-qsi)*(un-eta)/4.d0
        ff(2,ipg)= (un+qsi)*(un-eta)/4.d0
        ff(3,ipg)= (un+qsi)*(un+eta)/4.d0
        ff(4,ipg)= (un-qsi)*(un+eta)/4.d0
!
        dfdx(1,ipg)= -(un-eta)/4.d0
        dfdx(2,ipg)= (un-eta)/4.d0
        dfdx(3,ipg)= (un+eta)/4.d0
        dfdx(4,ipg)= -(un+eta)/4.d0
!
        dfdy(1,ipg)= -(un-qsi)/4.d0
        dfdy(2,ipg)= -(un+qsi)/4.d0
        dfdy(3,ipg)= (un+qsi)/4.d0
        dfdy(4,ipg)= (un-qsi)/4.d0
!
1061  continue
!
!     CALCUL DES PRODUITS VECTORIELS OMI X OMJ
!
    do 21 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 22 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
22      continue
21  end do
!
!     BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 ipg = 1, npg
        nx(ipg) = zero
        ny(ipg) = zero
        nz(ipg) = zero
        do 102 i = 1, nno
            do 104 j = 1, nno
                nx(ipg) = nx(ipg) + dfdx(i,ipg)*dfdy(j,ipg)*sx(i,j)
                ny(ipg) = ny(ipg) + dfdx(i,ipg)*dfdy(j,ipg)*sy(i,j)
                nz(ipg) = nz(ipg) + dfdx(i,ipg)*dfdy(j,ipg)*sz(i,j)
104          continue
102      continue
!
!      CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac(ipg) = sqrt (nx(ipg)*nx(ipg) + ny(ipg)*ny(ipg) + nz(ipg)* nz(ipg))
!
!       CALCUL DE LA NORMALE UNITAIRE
!
        norm(1,ipg) = nx(ipg)/jac(ipg)
        norm(2,ipg) = ny(ipg)/jac(ipg)
        norm(3,ipg) = nz(ipg)/jac(ipg)
101  continue
!
!
    do 1051 ipg = 1, npg
        do 105 i = 1, nno
            acc(1,ipg) = acc(1,ipg) + acloc(1,i)*ff(i,ipg)
            acc(2,ipg) = acc(2,ipg) + acloc(2,i)*ff(i,ipg)
            acc(3,ipg) = acc(3,ipg) + acloc(3,i)*ff(i,ipg)
105      continue
1051  continue
!
!    CALCUL DE COORDONNEES AUX POINTS DE GAUSS
!
    do 90 ipg = 1, npg
        x(1,ipg)=zero
        x(2,ipg)=zero
        x(3,ipg)=zero
        do 91 j = 1, nno
            x(1,ipg)= x(1,ipg) + zr(igeom+3*(j-1)-1+1)*ff(j,ipg)
            x(2,ipg)= x(2,ipg) + zr(igeom+3*(j-1)-1+2)*ff(j,ipg)
            x(3,ipg)= x(3,ipg) + zr(igeom+3*(j-1)-1+3)*ff(j,ipg)
91      continue
!
! CALCUL DU FLUX FLUIDE NORMAL AUX POINTS DE GAUSS
!
        flufn(ipg) = acc(1,ipg)*norm(1,ipg)+acc(2,ipg)* norm(2,ipg)+ acc(3,ipg)*norm(3,ipg)
!
90  continue
!
! STOCKAGE DU FLUX FLUIDE DANS UN VECTEUR INDEXE
! PAR LE MODE ET L'ELEMENT
!
    imode='CHBIDON'
    ielem ='CHBIDON'
    call codent(zi(iharm), 'D0', imode)
    call tecael(iadzi, iazk24)
    call codent(zi(iadzi), 'D0', ielem)
    vetel = '&&329.M'//imode//'.EL'//ielem
!        ON CONSERVE L'ALLOCATION DYNAMIQUE AU DETRIMENT DE L'ALLOCATION
!        STATIQUE, CAR VETEL EST UTILIE A L'EXTERIEUR DES ROUTINES
!        ELEMENTAIRES
    call wkvect(vetel, 'V V R8', 4*npg, ivetel)
    do 100 ipg = 0, npg-1
        zr(ivetel+4*ipg) = jac(ipg+1)*flufn(ipg+1)
        zr(ivetel+4*ipg+1) = x(1,ipg+1)
        zr(ivetel+4*ipg+2) = x(2,ipg+1)
        zr(ivetel+4*ipg+3) = x(3,ipg+1)
100  continue
!
end subroutine
