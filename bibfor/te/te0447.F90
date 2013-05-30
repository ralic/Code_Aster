subroutine te0447(option, nomte)
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
    include 'asterfort/jevech.h'
    include 'asterfort/nmgeom.h'
    include 'asterfort/r8inir.h'
    character(len=16) :: option, nomte
!
!
!     BUT: CALCUL DES DEFORMATIONS AUX POINTS DE GAUSS
!          DES ELEMENTS INCOMPRESSIBLES 2D
!
!          OPTION : 'EPSI_ELGA'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    logical :: axi, grand
    integer :: kpg, ksig, nno, nnos, npg, ipoids, ivf, ndim, ncmp
    integer :: idfde, idepl, igeom, idefo, kk, jgano
    real(kind=8) :: poids, dfdi(81), f(3, 3), r, eps(6), vpg(36)
    real(kind=8) :: tmp
! ......................................................................
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    ncmp = 2*ndim
    axi = nomte(3:4).eq.'AX'
    grand = .false.
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PDEFOPG', 'E', idefo)
!
    call r8inir(6, 0.d0, eps, 1)
    call r8inir(36, 0.d0, vpg, 1)
!
    do 10 kpg = 1, npg
!
        call nmgeom(ndim, nno, axi, grand, zr(igeom),&
                    kpg, ipoids, ivf, idfde, zr(idepl),&
                    .true., poids, dfdi, f, eps,&
                    r)
!
!       RECUPERATION DE LA DEFORMATION
        do 20 ksig = 1, ncmp
            if (ksig .le. 3) then
                tmp=1.d0
            else
                tmp = sqrt(2.d0)
            endif
            vpg(ncmp*(kpg-1)+ksig)=eps(ksig)/tmp
20      continue
10  end do
!
!     AFFECTATION DU VECTEUR EN SORTIE
    do 30 kk = 1, npg*ncmp
        zr(idefo+kk-1)= vpg(kk)
30  end do
!
end subroutine
