subroutine te0200(option, nomte)
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
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/dfdm2d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_ME_FF2D2D  '
!                          ELEMENTS AXISYMETRIQUES FOURIER
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: icode, k, nbres
!-----------------------------------------------------------------------
    parameter         ( nbres=3 )
!
    character(len=8) :: nompar(nbres)
    real(kind=8) :: valpar(nbres)
    real(kind=8) :: dfdx(9), dfdy(9), poids, r, z, tx, ty, tz
    integer :: nno, kp, npg1, i, itemps, ifr2d, ivectu, nnos, jgano
    integer :: ipoids, ivf, idfde, igeom, ndim
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PFF2D2D', 'L', ifr2d)
    call jevech('PTEMPSR', 'L', itemps)
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'INST'
    valpar(3) = zr(itemps)
    call jevech('PVECTUR', 'E', ivectu)
!
    do 101 kp = 1, npg1
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, poids)
        r = 0.d0
        z = 0.d0
        do 102 i = 1, nno
            r = r + zr(igeom+2*i-2)*zr(ivf+k+i-1)
            z = z + zr(igeom+2*i-1)*zr(ivf+k+i-1)
102      continue
        poids = poids*r
        valpar(1) = r
        valpar(2) = z
        call fointe('FM', zk8(ifr2d ), 3, nompar, valpar,&
                    tx, icode)
        call fointe('FM', zk8(ifr2d+1), 3, nompar, valpar,&
                    ty, icode)
        call fointe('FM', zk8(ifr2d+2), 3, nompar, valpar,&
                    tz, icode)
        do 103 i = 1, nno
            k=(kp-1)*nno
            zr(ivectu+3*i-3) = zr(ivectu+3*i-3) + poids * tx * zr(ivf+ k+i-1)
            zr(ivectu+3*i-2) = zr(ivectu+3*i-2) + poids * ty * zr(ivf+ k+i-1)
            zr(ivectu+3*i-1) = zr(ivectu+3*i-1) + poids * tz * zr(ivf+ k+i-1)
103      continue
101  end do
end subroutine
