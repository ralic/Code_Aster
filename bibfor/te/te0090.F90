subroutine te0090(option, nomte)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/tefrep.h'
    include 'asterfort/vff2dn.h'
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
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_MECA_FR1D2D'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nno, nnos, jgano, ndim, kp, npg, ipoids, ivf, idfde, igeom
    integer :: ivectu, k, i, iforc, ii
    real(kind=8) :: poids, r, fx, fy, nx, ny
    logical :: laxi
!     ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)
    call tefrep(option, nomte, 'PFR1D2D', iforc)
!
!
    do 40 kp = 1, npg
        k = (kp-1)*nno
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
!
!        --- CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---
        fx = 0.0d0
        fy = 0.0d0
        do 10 i = 1, nno
            ii = 2* (i-1)
            fx = fx + zr(iforc+ii)*zr(ivf+k+i-1)
            fy = fy + zr(iforc+ii+1)*zr(ivf+k+i-1)
10      continue
!
        if (laxi) then
            r = 0.d0
            do 20 i = 1, nno
                r = r + zr(igeom+2* (i-1))*zr(ivf+k+i-1)
20          continue
            poids = poids*r
        endif
!
        do 30 i = 1, nno
            zr(ivectu+2* (i-1)) = zr(ivectu+2* (i-1)) + fx*zr(ivf+k+i- 1)*poids
            zr(ivectu+2* (i-1)+1) = zr(ivectu+2* (i-1)+1) + fy*zr(ivf+ k+i-1 )*poids
30      continue
!
40  end do
!
end subroutine
