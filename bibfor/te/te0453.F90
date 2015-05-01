subroutine te0453(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nmgeom.h"
#include "asterfort/r8inir.h"
    character(len=16) :: option, nomte
!.......................................................................
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BUT: CALCUL DES DEFORMATIONS AUX POINTS DE GAUSS
!          DES ELEMENTS INCOMPRESSIBLES 3D
!
!          OPTION : 'EPSI_ELGA'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
!
    aster_logical :: axi, grand
    real(kind=8) :: eps(6), vpg(162), poids, dfdi(60), f(3, 3), rbid, tmp
    integer :: jgano, ndim, ncmp, nno, npg, kpg, kk, ksig, nnos
    integer :: ipoids, ivf, idfde, igeom, idepl, idefo
! ......................................................................
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
    grand = .false.
    axi = .false.
    ncmp = 6
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PDEFOPG', 'E', idefo)
!
    call r8inir(6, 0.d0, eps, 1)
    call r8inir(162, 0.d0, vpg, 1)
!
    do 20 kpg = 1, npg
        call nmgeom(ndim, nno, axi, grand, zr(igeom),&
                    kpg, ipoids, ivf, idfde, zr(idepl),&
                    .true._1, poids, dfdi, f, eps,&
                    rbid)
!       RECUPERATION DE LA DEFORMATION
        do 10 ksig = 1, ncmp
            if (ksig .le. ndim) then
                tmp = 1.d0
            else
                tmp = sqrt(2.d0)
            endif
            vpg(ncmp* (kpg-1)+ksig) = eps(ksig)/tmp
 10     continue
!
 20 end do
!
!      AFFECTATION DU VECTEUR EN SORTIE
    do 30 kk = 1, npg*ncmp
        zr(idefo+kk-1) = vpg(kk)
 30 end do
!
end subroutine
