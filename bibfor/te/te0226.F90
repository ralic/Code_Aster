subroutine te0226(option, nomte)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dfdm1d.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/pmavec.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/vecma.h'
    character(len=16) :: option, nomte
! ......................................................................
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          COQUE 1D
!                          OPTION : 'MASS_MECA       '
!                          ELEMENT: MECXSE3,METCSE3,METDSE3
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
    character(len=8) :: elrefe, fami, poum
    integer :: icodre, kpg, spt
    real(kind=8) :: dfdx(3), r, rm, rf, rmf, poids, cour, nx, ny, h, vfi, vfj
    real(kind=8) :: matp(9, 9), matv(45), r8b, rho
    integer :: nno, nnos, jgano, ndim, ipoids, ivf, idfdk, igeom, imate, icaco
    integer :: kp, npg, ii, jj, i, j, k, imatuu, kd1, kd2, kd3, ij1, ij2, ij3
    integer :: nddl, nvec, iacce, ivect
! ......................................................................
!
    call elref1(elrefe)
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdk, jgano)
    nddl = 3*nno
    nvec = nddl* (nddl+1)/2
!
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCACOQU', 'L', icaco)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', 0, ' ', r8b,&
                1, 'RHO', rho, icodre, 1)
    h = zr(icaco)
    rm = rho*h
    rf = rho*h**3/12.d0
!
    do 10 k = 1, nvec
        matv(k) = 0.0d0
10  end do
!
    do 60 kp = 1, npg
        k = (kp-1)*nno
        call dfdm1d(nno, zr(ipoids+kp-1), zr(idfdk+k), zr(igeom), dfdx,&
                    cour, poids, nx, ny)
        if (nomte .eq. 'MECXSE3') then
            r = 0.0d0
            do 20 i = 1, nno
                r = r + zr(igeom+2* (i-1))*zr(ivf+k+i-1)
20          continue
            poids = poids*r
            rmf = rf* (cour+nx/r)
        endif
!
        if (nomte .eq. 'METDSE3' .or. nomte .eq. 'METCSE3') then
            rmf = rf*cour
        endif
        kd1 = 5
        kd2 = 3
        kd3 = 2
        do 50 i = 1, 3*nno, 3
            kd1 = kd1 + 3*i - 6
            kd2 = kd2 + 3*i - 3
            kd3 = kd3 + 3*i
            ii = (i+2)/3
            do 30 j = 1, i, 3
                jj = (j+2)/3
                ij1 = kd1 + j - 2
                ij2 = kd2 + j - 2
                ij3 = kd3 + j - 2
                vfi = zr(ivf+k+ii-1)
                vfj = zr(ivf+k+jj-1)
                matv(ij1) = matv(ij1) + vfi*vfj*poids*rm
                matv(ij2) = 0.0d0
                matv(ij2+1) = matv(ij1)
                matv(ij3) = matv(ij3) + vfi*vfj*poids*rmf*ny
                matv(ij3+1) = matv(ij3+1) - vfi*vfj*poids*rmf*nx
                matv(ij3+2) = matv(ij3+2) + vfi*vfj*poids*rf
30          continue
!
            do 40 j = 1, i - 3, 3
                jj = (j+2)/3
                ij1 = kd1 + j - 2
                ij2 = kd2 + j - 2
                ij3 = kd3 + j - 2
                matv(ij1+1) = matv(ij2)
                matv(ij1+2) = matv(ij3)
                matv(ij2+2) = matv(ij3+1)
40          continue
50      continue
60  end do
!
    if (option .eq. 'MASS_MECA') then
!
        call jevech('PMATUUR', 'E', imatuu)
!
        do 70 i = 1, nvec
            zr(imatuu+i-1) = matv(i)
70      continue
!
    else if (option.eq.'M_GAMMA') then
!
        call jevech('PACCELR', 'L', iacce)
        call jevech('PVECTUR', 'E', ivect)
        call vecma(matv, nvec, matp, nddl)
        call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
!
    else
!C OPTION DE CALCUL INVALIDE
        call assert(.false.)
    endif
!
end subroutine
