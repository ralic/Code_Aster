subroutine te0522(option, nomte)
    implicit none
!
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
    include 'jeveux.h'
!
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/ntfcma.h'
    include 'asterfort/rcfodi.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES DE
!                          CONVECTION POUR LES ELEMENTS ISOPARAMETRIQUES
!                          EN THERMIQUE 3D
!                          OPTION : 'RIGI_THER_CONV_T'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: npgmax
    parameter (npgmax=40)
!
    character(len=24) :: decent
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids
    real(kind=8) :: dni(3, 27, npgmax), uloc(3, 27), ul(3, npgmax)
    real(kind=8) :: jacob(npgmax), umi(3), um, aire, rr, cc
    real(kind=8) :: xr, xrr, xaux, rbid, s, xma, xm, coef, cmin, alfa, aksi
    integer :: jgano, nno, kp, npg1, i, j, k, ij, itemps, imattt
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: ifon(3), ivite, itempi
    integer :: ndim, iad, nbvf, jvalf, idim, jdim, nnos
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVITESR', 'L', ivite)
    call jevech('PMATERC', 'L', imate)
    call jevech('PNEUK24', 'L', iad)
    decent = zk24(iad-1+1)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PMATTTR', 'E', imattt)
!
    call ntfcma(zi(imate), ifon)
    nbvf = zi(ifon(1))
    jvalf = zi(ifon(1)+2)
    xr = 0.d0
    do 20 i = 1, nbvf
        xaux = zr(jvalf+i-1)
        call rcfodi(ifon(1), xaux, rbid, xrr)
        if (xrr .gt. xr) then
            xr = xrr
        endif
20  end do
    rr = 0.6d0/xr
!
    k = 0
    do 40 i = 1, nno
        do 30 idim = 1, 3
            k = k + 1
            uloc(idim,i) = zr(ivite+k-1)
30      continue
40  end do
!
    aire = 0.d0
    umi(1) = 0.d0
    umi(2) = 0.d0
    umi(3) = 0.d0
!
    do 70 kp = 1, npg1
        ul(1,kp) = 0.d0
        ul(2,kp) = 0.d0
        ul(3,kp) = 0.d0
        k = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, dfdz, poids)
        do 50 i = 1, nno
            ul(1,kp) = ul(1,kp) + uloc(1,i)*zr(ivf+k+i-1)
            ul(2,kp) = ul(2,kp) + uloc(2,i)*zr(ivf+k+i-1)
            ul(3,kp) = ul(3,kp) + uloc(3,i)*zr(ivf+k+i-1)
50      continue
!
        aire = aire + poids
        do 60 i = 1, nno
            dni(1,i,kp) = dfdx(i)
            dni(2,i,kp) = dfdy(i)
            dni(3,i,kp) = dfdz(i)
60      continue
!
        jacob(kp) = poids
!
        umi(1) = umi(1) + ul(1,kp)*poids
        umi(2) = umi(2) + ul(2,kp)*poids
        umi(3) = umi(3) + ul(3,kp)*poids
70  end do
!
    umi(1) = umi(1)/aire
    umi(2) = umi(2)/aire
    umi(3) = umi(3)/aire
!
    ij = imattt - 1
!
    do 100 i = 1, nno
        do 90 j = 1, nno
            s = 0.d0
            do 80 kp = 1, npg1
                k = (kp-1)*nno
                s = s + zr(ivf+k+i-1)*dni(1,j,kp)*ul(1,kp)*jacob(kp)* rr + zr(ivf+k+i-1)*dni(2,j,&
                    &kp)*ul(2,kp)*jacob(kp)*rr + zr(ivf+k+i-1)*dni(3,j,kp)*ul(3,kp)*jacob(kp)*rr
80          continue
            ij = ij + 1
            zr(ij) = zr(ij) + s
90      continue
100  end do
!
    if (decent .eq. 'OUI') then
!
!
!- DECENTREMENT HUGUES-BROOKS SU2
!
        um = umi(1)*umi(1) + umi(2)*umi(2) + umi(3)*umi(3)
        um = sqrt(um)
        if (um .lt. 1.d-10) goto 170
        umi(1) = umi(1)/um
        umi(2) = umi(2)/um
        umi(3) = umi(3)/um
!
        xma = aire** (1.d0/3)
!
        do 110 i = 2, nno
            xm = 0.d0
            xm = xm + (&
                 zr(igeom)-zr(igeom+3*i-3))* (zr(igeom)-zr( igeom+3*i-3)) + (zr(igeom+1)-zr(igeom&
                 &+3*i-2))* (zr(igeom+ 1)-zr(igeom+3*i-2)) + (zr(igeom+2)-zr(igeom+3*i-1))* (zr(i&
                 &geom+2)-zr(igeom+3*i-1)&
                 )
            xm = sqrt(xm)
            if (xm .gt. xma) xma = xm
110      continue
!
        ij = imattt - 1
!
        do 160 i = 1, nno
            do 150 j = 1, nno
                s = 0.d0
                do 140 kp = 1, npg1
                    do 130 idim = 1, 3
                        do 120 jdim = 1, 3
!
                            coef = rr
                            cmin = 1.d0
                            alfa = um*xma/cmin*coef
                            aksi = alfa/3.d0
                            if (alfa .gt. 3.d0) aksi = 1.d0
                            cc = aksi*um*xma
!
                            s = s + (&
                                dni(idim,i,kp)*dni(jdim,j,kp)*ul( idim,kp)* ul(jdim,kp)*jacob(kp)&
                                &/ (um*um)&
                                )* coef*cc
!
120                      continue
130                  continue
140              continue
                ij = ij + 1
                zr(ij) = zr(ij) + s
150          continue
160      continue
    endif
!
170  continue
end subroutine
