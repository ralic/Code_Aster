subroutine te0304(option, nomte)
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
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN THERMIQUE
!          CORRESPONDANT AU TERME D'ECHANGE ENTRE 2 PAROIS (FACE)
!          D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_THER_PARO_F'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/elref2.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    character(len=8) :: elrefe, nompar(4), lirefe(2)
    character(len=16) :: nomte, option
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), xx, yy, zz, jac
    real(kind=8) :: tem, theta, hechp, valpar(4)
    integer :: ipoids, ivf, idfdx, idfdy, igeom, i, itemp, itemps, ndim, nno
    integer :: ipg, npg1, ivectt, ihechp, ino, jno, idec, jdec, kdec, ldec, ier
    integer :: j, nbelr, nnos, jgano
!
!====
! 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
!====
!
    call elref2(nomte, 2, lirefe, nbelr)
    call assert(nbelr.eq.2)
    elrefe = lirefe(2)
!
    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!
!====
! 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
!====
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PHECHPF', 'L', ihechp)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PVECTTR', 'E', ivectt)
!
!====
! 1.3 PREALABLES LIES AUX CALCULS
!====
    theta = zr(itemps+2)
    valpar(4) = zr(itemps)
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
    nompar(4) = 'INST'
    do 10 i = 1, 2*nno
        zr(ivectt+i-1) = 0.0d0
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
!====
! 2. CALCULS TERMES DE MASSE
!====
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 101 ipg = 1, npg1
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!    CALCUL DE HECHP
        xx = 0.d0
        yy = 0.d0
        zz = 0.d0
        do 202 i = 1, nno
            xx = xx + zr(igeom+3*i-3) * zr(ivf+ldec+i-1)
            yy = yy + zr(igeom+3*i-2) * zr(ivf+ldec+i-1)
            zz = zz + zr(igeom+3*i-1) * zr(ivf+ldec+i-1)
202      continue
        valpar(1) = xx
        valpar(2) = yy
        valpar(3) = zz
        call fointe('FM', zk8(ihechp), 4, nompar, valpar,&
                    hechp, ier)
!
!   CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx=nx+zr(idfdx+kdec+idec)*zr(idfdy+kdec+jdec)*sx(i,j)
                ny=ny+zr(idfdx+kdec+idec)*zr(idfdy+kdec+jdec)*sy(i,j)
                nz=nz+zr(idfdx+kdec+idec)*zr(idfdy+kdec+jdec)*sz(i,j)
102          continue
!
! --- CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
        tem = 0.d0
        do 104 i = 1, nno
            ldec = (ipg-1)*nno
            tem = tem + (zr(itemp+nno+i-1)- zr(itemp+i-1) ) * zr(ivf+ ldec+i-1)
104      continue
        do 103 i = 1, nno
            zr(ivectt+i-1) = zr(ivectt+i-1) + jac * hechp * zr(ipoids+ ipg-1) * zr(ivf+ldec+i-1) &
                             &* (1.0d0-theta)*tem
            zr(ivectt+nno+i-1) = zr(ivectt+nno+i-1) - jac * hechp * zr(ipoids+ipg-1) * zr(ivf+lde&
                                 &c+i-1) * (1.0d0-theta)*tem
103      continue
101  end do
end subroutine
