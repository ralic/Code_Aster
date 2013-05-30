subroutine te0128(option, nomte)
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
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8t0.h'
    include 'asterfort/assert.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS RESIDUS
!                          OPTION : 'RESI_THER_COEF_F'
!                          OPTION : 'RESI_THER_RAYO_F'
!                          ELEMENTS 3D
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
    character(len=8) :: nompar(4)
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac, tpg, theta
    real(kind=8) :: valpar(4), xx, yy, zz
    real(kind=8) :: echnp1, sigma, epsil, tz0
    integer :: ipoids, ivf, idfdx, idfdy, igeom, jgano
    integer :: ndim, nno, ipg, npg1, iveres, iech, iray, nnos
    integer :: idec, jdec, kdec, ldec
!-----------------------------------------------------------------------
    integer :: i, ier, ino, itemp, itemps, j, jno
!
!-----------------------------------------------------------------------
    data               nompar/'X','Y','Z','INST'/
! DEB ------------------------------------------------------------------
    tz0 = r8t0()
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!
    if (option(11:14) .eq. 'COEF') then
        call jevech('PCOEFHF', 'L', iech)
    else if (option(11:14).eq.'RAYO') then
        call jevech('PRAYONF', 'L', iray)
    endif
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPEI', 'L', itemp)
    call jevech('PRESIDU', 'E', iveres)
!
    theta = zr(itemps+2)
!
!    CALCUL DES PRODUITS VECTORIELS OMI   OMJ
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
    do 101 ipg = 1, npg1
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx = nx + zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sx(i,j)
                ny = ny + zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sy(i,j)
                nz = nz + zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sz(i,j)
102          continue
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
        tpg = 0.d0
        xx = 0.d0
        yy = 0.d0
        zz = 0.d0
        do 103 i = 1, nno
            tpg = tpg + zr(itemp+i-1) * zr(ivf+ldec+i-1)
            xx = xx + zr(igeom+3*i-3) * zr(ivf+ldec+i-1)
            yy = yy + zr(igeom+3*i-2) * zr(ivf+ldec+i-1)
            zz = zz + zr(igeom+3*i-1) * zr(ivf+ldec+i-1)
103      continue
        valpar(1) = xx
        valpar(2) = yy
        valpar(3) = zz
        valpar(4) = zr(itemps)
        if (option(11:14) .eq. 'COEF') then
            call fointe('A', zk8(iech), 4, nompar, valpar,&
                        echnp1, ier)
            call assert(ier .eq. 0)
            do 104 i = 1, nno
                zr(iveres+i-1) = zr(iveres+i-1) + jac* theta* zr( ipoids+ipg-1)* zr(ivf+ldec+i-1)&
                                 &* echnp1 * tpg
104          continue
        else if (option(11:14).eq.'RAYO') then
            call fointe('A', zk8(iray), 4, nompar, valpar,&
                        sigma, ier)
            call assert(ier .eq. 0)
            call fointe('A', zk8(iray+1), 4, nompar, valpar,&
                        epsil, ier)
            call assert(ier .eq. 0)
            do 105 i = 1, nno
                zr(iveres+i-1) = zr(iveres+i-1) + jac* theta* zr( ipoids+ipg-1)* zr(ivf+ldec+i-1)&
                                 &* sigma* epsil* ( tpg + tz0 )**4
105          continue
        endif
!
101  end do
! FIN ------------------------------------------------------------------
end subroutine
