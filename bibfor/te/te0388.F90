subroutine te0388(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/elref2.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    character(len=16) :: nomte, option
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
!.......................................................................
!
!     BUT: CALCUL DES MATRICES ELEMENTAIRES EN THERMIQUE
!          CORRESPONDANT AU TERME D'ECHANGE ENTRE 2 PAROIS (FACE)
!          D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'MTAN_THER_PARO_R'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
    integer :: ipoids, ivf, idfdx, idfdy, igeom, nnos, jgano
    integer :: ndim, nno, ndi, ipg, npg1, imattt, ihechp
    integer :: idec, jdec, kdec, ldec, nbelr
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac, theta, h
    real(kind=8) :: mat(45)
    character(len=8) :: elrefe, lirefe(2)
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ij, ino, itemps, j, jno, k1
    integer :: k2, k3, k4
!-----------------------------------------------------------------------
    call elref2(nomte, 2, lirefe, nbelr)
    call assert(nbelr.eq.2)
    elrefe = lirefe(2)
!
    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
    ndi = nno*(nno+1)/2
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PHECHPR', 'L', ihechp)
    h = zr(ihechp)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PMATTTR', 'E', imattt)
!
    theta = zr(itemps+2)
!
    do 10 i = 1, ndi
        mat(i) = 0.0d0
10  end do
!
!    CALCUL DES PRODUITS VECTORIELS OMI * OMJ
!
    do 1 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 2 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
 2      continue
 1  end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 ipg = 1, npg1
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!   CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
!
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
!
102          continue
!
!   CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
        do 103 i = 1, nno
!CDIR$ IVDEP
            do 104 j = 1, i
                ij = (i-1)*i/2 + j
                mat(ij) = mat(ij) + jac * theta * zr(ipoids+ipg-1) * h * zr(ivf+ldec+i-1) * zr(iv&
                          &f+ldec+j-1)
!
104          continue
103      continue
!
101  end do
!
! --- CALCUL LA MATRICE SUR LA MAILLE DE COUPLAGE
!            ! MAT -MAT!
!     MATTT =!      MAT!
    k1 = 0
    k2= nno*(nno+1)/2
    do 200 i = 1, nno
        k3 = k2 + nno
!CDIR$ IVDEP
        do 201 j = 1, i
            k1=k1+1
            k2=k2+1
            k3=k3+1
            zr(imattt-1+k1) = mat(k1)
            zr(imattt-1+k2) = -mat(k1)
            zr(imattt-1+k3) = mat(k1)
201      continue
        do 202 j = i+1, nno
            k2=k2+1
            k4=i+j*(j-1)/2
            zr(imattt-1+k2) = -mat(k4)
202      continue
        k2 = k3
200  end do
end subroutine
