subroutine te0197(option, nomte)
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
    include 'asterc/r8miem.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mess.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_ME_ROTAR   '
!                          CAS AXISYMETRIQUE FOURIER
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: icodre
    character(len=8) :: fami, poum
    real(kind=8) :: dfdx(9), dfdy(9), poids, r
    integer :: nno, kp, npg1, i, ivectu, irota, ndim, jgano, nnos
    integer :: ipoids, ivf, idfde, igeom, imate, kpg, spt
!
!
!-----------------------------------------------------------------------
    integer :: k
    real(kind=8) :: r8b, rho
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PROTATR', 'L', irota)
!
! VERIFICATIONS SUR LE CHARGEMENT ROTATION (AXE=Oy et CENTRE=ORIGINE)
!
    if (zr(irota+1) .gt. r8miem() .or. zr(irota+3) .gt. r8miem()) then
        call u2mess('F', 'MODELISA10_1')
    endif
    if (zr(irota+4) .gt. r8miem() .or. zr(irota+5) .gt. r8miem() .or. zr(irota+6) .gt.&
        r8miem()) then
        call u2mess('F', 'MODELISA10_2')
    endif
    call jevech('PVECTUR', 'E', ivectu)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'ELAS', 0, ' ', r8b,&
                1, 'RHO', rho, icodre, 1)
!
    do 101 kp = 1, npg1
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, poids)
        r = 0.d0
        do 102 i = 1, nno
            r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
102      continue
        poids = poids*rho*r
        r = r - zr(irota+4)
        do 103 i = 1, nno
            k=(kp-1)*nno
            zr(ivectu+3*i-3) = zr(ivectu+3*i-3) + poids * (zr(irota)* zr(irota+2))**2 * r * zr(iv&
                               &f+k+i-1)
103      continue
101  end do
end subroutine
