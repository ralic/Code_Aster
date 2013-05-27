subroutine te0010(option, nomte)
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
!.......................................................................
    implicit none
!
!     BUT: CALCUL DES MATRICES ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UNE IMPEDANCE IMPOSEE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'IMPE_MECA'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/rcvalb.h'
    integer :: icodre(1)
    character(len=8) :: nomres(1), fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac
    real(kind=8) :: valres(1), rho, rho2
    integer :: ipoids, ivf, idfdx, idfdy, igeom, imate
    integer :: ndim, nno, ndi, ipg, npg2, imatuu, iimpe
    integer :: idec, jdec, kdec, ldec
    integer :: nnos, jgano, kpg, spt
!
!
!-----------------------------------------------------------------------
    integer :: i, ii, ij, ino, j, jj, jno
    integer :: mater
    real(kind=8) :: r8b
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg2, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
    ndi = nno*(2*nno+1)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    mater=zi(imate)
    nomres(1)='RHO'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'FLUIDE', 0, ' ', r8b,&
                1, nomres, valres, icodre, 1)
    rho = valres(1)
!
    call jevech('PIMPEDR', 'L', iimpe)
    call jevech('PMATUUR', 'E', imatuu)
!
! --- INITIALISATION DE LA MATRICE D'IMPEDANCE
    do 10 i = 1, ndi
        zr(imatuu+i-1) = 0.d0
10  end do
!
    if (zr(iimpe) .eq. 0.d0) then
        goto 999
    else
!
!        CALCUL DES PRODUITS VECTORIELS OMI X OMJ
!
        do 1 ino = 1, nno
            i = igeom + 3*(ino-1) -1
            do 2 jno = 1, nno
                j = igeom + 3*(jno-1) -1
                sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
                sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
                sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
 2          continue
 1      continue
!
!        BOUCLE SUR LES POINTS DE GAUSS
        rho2 = -rho*rho
!
        do 101 ipg = 1, npg2
            kdec = (ipg-1)*nno*ndim
            ldec = (ipg-1)*nno
!
            nx = 0.0d0
            ny = 0.0d0
            nz = 0.0d0
!
!           CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
!
            do 102 i = 1, nno
                idec = (i-1)*ndim
                do 102 j = 1, nno
                    jdec = (j-1)*ndim
!
                    nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sx(i,j)
                    ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sy(i,j)
                    nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+ jdec) * sz(i,j)
!
102              continue
!
!           CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
            jac = sqrt(nx*nx + ny*ny + nz*nz)
!
            do 103 i = 1, nno
!
                do 104 j = 1, i
                    ii=2*i
                    jj=2*j
                    ij = (ii-1)*ii/2 + jj
!
                    zr(imatuu+ij-1) = zr(imatuu+ij-1) + jac * rho2 / zr(iimpe+ipg-1) * zr(ipoids+&
                                      &ipg-1) * zr(ivf+ldec+ i-1) * zr(ivf+ldec+j-1)
!
104              continue
103          continue
!
101      continue
    endif
999  continue
end subroutine
