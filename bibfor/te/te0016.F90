subroutine te0016(option, nomte)
    implicit     none
    include 'jeveux.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/tefrep.h'
    character(len=16) :: option, nomte
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
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_MECA_FORC_R '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
    integer :: ipoids, ivf, idfde, igeom, iforc
    integer :: jgano, nno, ndl, kp, npg, ii, i, l, ivectu, ndim, nnos
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, fx, fy, fz
!     ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)
    call tefrep(option, nomte, 'PFR3D3D', iforc)
!
    ndl = 3*nno
    do 20 i = 1, ndl
        zr(ivectu+i-1) = 0.0d0
20  end do
!
!
!     BOUCLE SUR LES POINTS DE GAUSS
    do 50 kp = 1, npg
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, dfdz, poids)
!
!       CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---
        fx = 0.0d0
        fy = 0.0d0
        fz = 0.0d0
        do 30 i = 1, nno
            ii = 3*(i-1)
            fx = fx + zr(ivf-1+l+i)*zr(iforc+ii )
            fy = fy + zr(ivf-1+l+i)*zr(iforc+ii+1)
            fz = fz + zr(ivf-1+l+i)*zr(iforc+ii+2)
30      continue
!
        do 40 i = 1, nno
            ii = 3* (i-1)
            zr(ivectu+ii ) = zr(ivectu+ii ) + poids*zr(ivf+l+i-1)*fx
            zr(ivectu+ii+1) = zr(ivectu+ii+1) + poids*zr(ivf+l+i-1)* fy
            zr(ivectu+ii+2) = zr(ivectu+ii+2) + poids*zr(ivf+l+i-1)* fz
40      continue
50  end do
!
!
end subroutine
