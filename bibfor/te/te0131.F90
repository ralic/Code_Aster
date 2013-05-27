subroutine te0131(option, nomte)
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
!     BUT: CALCUL DES MATRICES TANGENTES ELEMENTAIRES EN THERMIQUE
!          CORRESPONDANT AU TERME D'ECHANGE
!          (LE COEFFICIENT D'ECHANGE EST UNE FONCTION DU TEMPS ET DE
!           L'ESPACE)
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'MTAN_THER_COEF_F'
!          OPTION : 'MTAN_THER_RAYO_F'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    include 'jeveux.h'
!
    include 'asterc/r8t0.h'
    include 'asterfort/assert.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    character(len=8) :: nompar(4)
    character(len=16) :: nomte, option
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac, theta
    real(kind=8) :: valpar(4), echan, xx, yy, zz
    integer :: ipoids, ivf, idfdx, idfdy, igeom
    integer :: ier, ndim, nno, ndi, ipg, npg2, imattt, iech
    integer :: iray, itemp, nnos, jgano
    integer :: idec, jdec, kdec, ldec
    real(kind=8) :: sigma, epsil, tpg, tz0
!
!-----------------------------------------------------------------------
    integer :: i, ij, ino, itemps, j, jno
!-----------------------------------------------------------------------
    tz0 = r8t0()
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg2, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
    ndi = nno*(nno+1)/2
!
    if (option(11:14) .eq. 'COEF') then
        call jevech('PCOEFHF', 'L', iech)
    else if (option(11:14).eq.'RAYO') then
        call jevech('PRAYONF', 'L', iray)
        call jevech('PTEMPEI', 'L', itemp)
    endif
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PMATTTR', 'E', imattt)
!
    theta = zr(itemps+2)
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
    nompar(4) = 'INST'
!
    do 10 i = 1, ndi
        zr(imattt+i-1) = 0.0d0
10  end do
!
!    CALCUL DES PRODUITS VECTORIELS OMI X OMJ
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
    do 101 ipg = 1, npg2
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
        do 12 i = 1, nno
            idec = (i-1)*ndim
            do 12 j = 1, nno
                jdec = (j-1)*ndim
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
12          continue
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
        xx = 0.d0
        yy = 0.d0
        zz = 0.d0
        do 102 i = 1, nno
            xx = xx + zr(igeom+3*i-3) * zr(ivf+ldec+i-1)
            yy = yy + zr(igeom+3*i-2) * zr(ivf+ldec+i-1)
            zz = zz + zr(igeom+3*i-1) * zr(ivf+ldec+i-1)
102      continue
        valpar(1) = xx
        valpar(2) = yy
        valpar(3) = zz
        valpar(4) = zr(itemps)
        if (option(11:14) .eq. 'COEF') then
            call fointe('A', zk8(iech), 4, nompar, valpar,&
                        echan, ier)
            call assert(ier.eq.0)
!
            do 103 i = 1, nno
!CDIR$ IVDEP
                do 104 j = 1, i
                    ij = (i-1)*i/2 + j
!
                    zr(imattt+ij-1) = zr(imattt+ij-1) + jac * theta * zr(ipoids+ipg-1) * echan * &
                                      &zr(ivf+ldec+i-1) * zr( ivf+ldec+j-1)
!
104              continue
103          continue
        else if (option(11:14).eq.'RAYO') then
            call fointe('A', zk8(iray), 4, nompar, valpar,&
                        sigma, ier)
            call assert(ier .eq. 0)
            call fointe('A', zk8(iray+1), 4, nompar, valpar,&
                        epsil, ier)
            call assert(ier .eq. 0)
!
            tpg = 0.d0
            do 105 i = 1, nno
                tpg = tpg + zr(itemp+i-1) * zr(ivf+ldec+i-1)
105          continue
            do 106 i = 1, nno
!CDIR$ IVDEP
                do 107 j = 1, i
                    ij = (i-1)*i/2 + j
!
                    zr(imattt+ij-1) = zr(imattt+ij-1) + jac * theta * zr(ivf+ldec+i-1) * zr(ivf+l&
                                      &dec+j-1) * zr(ipoids+ ipg-1) * sigma * epsil * 4.d0 * (tpg&
                                      &+tz0)**3
!
107              continue
106          continue
        endif
!
101  continue
end subroutine
