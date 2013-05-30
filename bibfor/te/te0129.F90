subroutine te0129(option, nomte)
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
    include 'asterfort/elref4.h'
    include 'asterfort/foderi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS RESIDUS
!                          OPTION : 'RESI_THER_FLUXNL'
!                          ELEMENTS DE FACE 3D
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac
    real(kind=8) :: theta, tpg, alpha, rbid
    integer :: ndim, nno, npg1, ipoids, ivf, idfdx, idfdy
    integer :: igeom, iflux, itempi, itemps, ino, jno, iveres
    integer :: nnos, jgano
    character(len=8) :: coef
!
!-----------------------------------------------------------------------
    integer :: i, idec, j, jdec, kdec, kp, ldec
!
!-----------------------------------------------------------------------
    call jemarq()
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PFLUXNL', 'L', iflux)
    call jevech('PRESIDU', 'E', iveres)
!
    theta = zr(itemps+2)
    coef = zk8(iflux)
    if (coef(1:7) .eq. '&FOZERO') goto 999
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
    do 101 kp = 1, npg1
        kdec = (kp-1)*nno*ndim
        ldec = (kp-1)*nno
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!   CALCUL DE LA NORMALE AU POINT DE GAUSS KP
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx = nx+ zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sx( i,j)
                ny = ny+ zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sy( i,j)
                nz = nz+ zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sz( i,j)
102          continue
!
!   CALCUL DU JACOBIEN AU POINT DE GAUSS KP
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
        tpg = 0.d0
        do 103 i = 1, nno
            tpg = tpg + zr(itempi+i-1) * zr(ivf+ldec+i-1)
103      continue
        call foderi(coef, tpg, alpha, rbid)
!
! ----- ON RAJOUTE DANS LE RESIDU LE TERME (1-THETA)*ALPHAP QUI NE
! ----- FIGURE PAS DANS LE 2ND MEMBRE LINEAIRE
!
!CDIR$ IVDEP
        do 104 i = 1, nno
            zr(iveres+i-1) = zr(iveres+i-1) - zr(ipoids+kp-1)*jac* theta*alpha*zr(ivf+ldec+i-1)
104      continue
101  end do
999  continue
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
