subroutine te0028(option, nomte)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/tefrep.h'
    character(len=16) :: option, nomte
!.......................................................................
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
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT FORCE_FACE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'CHAR_MECA_FR2D3D '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    integer :: ipoids, ivf, idfdx, idfdy, igeom, i, j
    integer :: ndim, nno, ipg, npg1, iforc, ino, jno
    integer :: idec, jdec, kdec, ldec, ires
    integer :: nnos, jgano
    real(kind=8) :: jac, nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), fx, fy, fz
!
!     ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ires)
    call tefrep(option, nomte, 'PFR2D3D', iforc)
!
    do 20 i = 1, 3*nno
        zr(ires+i-1) = 0.0d0
20  end do
!
!     --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
!
    do 30 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 32 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
32      continue
30  end do
!
!     --- BOUCLE SUR LES POINTS DE GAUSS ---
!
    do 100 ipg = 1, npg1
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!        --- CALCUL DE LA NORMALE AU POINT DE GAUSS IPG ---
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
102          continue
!
!        --- LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE ---
!
        jac = sqrt (nx*nx + ny*ny + nz*nz)
!
!        --- CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---
!
        fx = 0.0d0
        fy = 0.0d0
        fz = 0.0d0
        do 104 i = 1, nno
            fx = fx + zr(iforc-1+3*(i-1)+1)*zr(ivf+ldec+i-1)
            fy = fy + zr(iforc-1+3*(i-1)+2)*zr(ivf+ldec+i-1)
            fz = fz + zr(iforc-1+3*(i-1)+3)*zr(ivf+ldec+i-1)
104      continue
!
        do 106 i = 1, nno
            zr(ires+3*(i-1) ) = zr( ires+3*(i-1) ) + zr(ipoids+ipg-1)* fx*zr(ivf+ldec+i-1 )*jac
            zr(ires+3*(i-1)+1) = zr( ires+3*(i-1)+1) + zr(ipoids+ipg-1) *fy*zr(ivf+ldec+i-1 )*jac
            zr(ires+3*(i-1)+2) = zr( ires+3*(i-1)+2) + zr(ipoids+ipg-1) *fz*zr(ivf+ldec+i-1 )*jac
106      continue
100  end do
!
end subroutine
