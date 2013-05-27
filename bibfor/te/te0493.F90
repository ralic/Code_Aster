subroutine te0493(option, nomte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
    implicit none
    include 'jeveux.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/u2mesk.h'
    character(len=16) :: option, nomte
! ......................................................................
!
!     BUT: CALCUL DU FLUX HYDRAULIQUE NORMAL
!          SUR DES ELEMENTS DE BORD DE 3D (FACE8 ET FACE6)
!          OPTION : 'FLHN_ELGA'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: nx, ny, nz, flx, fly, flz, flun, s, t, u, jac
    real(kind=8) :: sx(4, 4), sy(4, 4), sz(4, 4)
    integer :: nno, kp, npg, ipoids, ivf, idfdx, idfdy, igeom
    integer :: iflux, ivectu, k, i, iad
    integer :: idec, jdec, kdec
    character(len=24) :: valkm(3)
    logical :: tria
!
!
!-----------------------------------------------------------------------
    integer :: ifl, ino, j, jgano, jno, nbflux, ndim
    integer :: nnos
!-----------------------------------------------------------------------
    tria = .false.
!  CALCUL DU NBRE DE CMP CALCULEES DU FLUX
    if (nomte .eq. 'HM_FACE8' .or. nomte .eq. 'THM_FACE8' .or. nomte .eq. 'H_FACE8') then
        nbflux=1
        elseif(nomte.eq.'HM_FACE6'.or.nomte.eq.'THM_FACE6'.or.&
     & nomte.eq.'H_FACE6') then
        nbflux=1
        tria = .true.
    else if (nomte.eq.'THV_FACE8') then
        nbflux=2
    else if (nomte.eq.'THV_FACE6') then
        nbflux=2
        tria = .true.
        elseif(nomte.eq.'HHM_FACE8'.or.nomte.eq.'THH_FACE8'&
     &.or.nomte.eq.'THHM_FACE8'.or.nomte.eq.'HH_FACE8') then
        nbflux=3
        elseif(nomte.eq.'HHM_FACE6'.or.nomte.eq.'THH_FACE6'&
     &.or.nomte.eq.'THHM_FACE6'.or.nomte.eq.'HH_FACE6') then
        nbflux=3
        tria = .true.
        elseif(nomte.eq.'HH2M_FACE8'.or.nomte.eq.'THH2_FACE8' .or.nomte&
    .eq.'THH2M_FACE8'.or.nomte.eq.'HH2_FACE8') then
        nbflux=4
        elseif(nomte.eq.'HH2M_FACE6'.or.nomte.eq.'THH2_FACE6' .or.nomte&
    .eq.'THH2M_FACE6'.or.nomte.eq.'HH2_FACE6') then
        nbflux=4
        tria = .true.
    else
        valkm(1)=option
        valkm(2)=nomte
        valkm(3)='TE0493'
        call u2mesk('F', 'CALCULEL7_2', 3, valkm)
    endif
!
    if (tria) then
        call elref4('TR3', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfdx, jgano)
    else
        call elref4('QU4', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, ivf, idfdx, jgano)
    endif
    idfdy=idfdx+1
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCONTR', 'L', iflux)
    call jevech('PFLHN', 'E', ivectu)
!
! --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
!
    do 20 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 22 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
22      continue
20  end do
!
!    BOUCLE SUR LES CMP
    do 100 ifl = 1, nbflux
!
!    BOUCLE SUR LES POINTS DE GAUSS
        do 40 kp = 1, npg
            k = (kp-1)*nno
! CALCUL DES FLUX AU POINT DE GAUSS KP A PARTIR DES FLUX AUX NOEUDS
            s = 0.d0
            t = 0.d0
            u = 0.d0
            do 10 i = 1, nno
                iad = iflux+3*(ifl-1)+3*nbflux*(i-1)
                s = s + zr(iad )*zr(ivf+k+i-1)
                t = t + zr(iad+1)*zr(ivf+k+i-1)
                u = u + zr(iad+2)*zr(ivf+k+i-1)
10          continue
            flx = s
            fly = t
            flz = u
! --- CALCUL DE LA NORMALE AU POINT DE GAUSS KP ---
            nx = 0.0d0
            ny = 0.0d0
            nz = 0.0d0
            kdec = (kp-1)*nno*ndim
            do 102 i = 1, nno
                idec = (i-1)*ndim
                do 102 j = 1, nno
                    jdec = (j-1)*ndim
                    nx = nx + zr(idfdx+kdec+idec)*zr(idfdy+kdec+jdec)* sx(i,j)
                    ny = ny + zr(idfdx+kdec+idec)*zr(idfdy+kdec+jdec)* sy(i,j)
                    nz = nz + zr(idfdx+kdec+idec)*zr(idfdy+kdec+jdec)* sz(i,j)
102              continue
            jac = sqrt(nx*nx+ny*ny+nz*nz)
            flun = (nx*flx + ny*fly + nz*flz)/jac
            zr(ivectu+nbflux*(kp-1)+ifl-1) = flun
40      continue
100  end do
!
end subroutine
