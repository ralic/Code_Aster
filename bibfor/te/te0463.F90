subroutine te0463(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fmater.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/ppga1d.h"
#include "asterfort/utpvlg.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!     CALCUL DES COORDONNEES DES SOUS POINTS DE GAUSS SUR LES FAMILLE
!     DE LA LISTE MATER
!     POUR LES ELEMENTS : POU_D_EM ET POU_D_TGM
! ----------------------------------------------------------------------
!     NOMBRE MAX DE FAMILLE DANS MATER
    integer :: nfpgmx
!     NOMBRE MAX DE POINTS DE GAUSS
    integer :: nbpgmx
    parameter (nfpgmx=10,nbpgmx=3)
!
    integer :: ndim, nno, nnos, npg, jgano, icopg, idfde, ipoids, ivf, igeom
    integer :: ndim1
    integer :: inbf, nbfib, jacf, iorien
    integer :: ncarfi, nfpg
    integer :: ifpg, npgfa(nfpgmx)
    integer :: ig, ifi, decga, k, decfpg(nfpgmx)
    real(kind=8) :: copg(4, 4), copgfa(3, nbpgmx, nfpgmx), pgl(3, 3), gm1(3)
    real(kind=8) :: gm2(3)
    character(len=8) :: fami(nfpgmx)
!
! ----------------------------------------------------------------------
!
    if ((nomte.ne.'MECA_POU_D_EM') .and. (nomte.ne.'MECA_POU_D_TGM')) then
        ASSERT(.false.)
    endif
!
    call jevech('PGEOMER', 'L', igeom)
    ndim = 3
!
!     ZR(ICOPG) : COORDONNEES DE SOUS-POINTS DE GAUSS
    call jevech('PCOOPGM', 'E', icopg)
!
! ELEMENTS A SOUS POINTS : POUTRES MULTIFIBRES
!
    call jevech('PNBSP_I', 'L', inbf)
    nbfib = zi(inbf)
    call jevech('PFIBRES', 'L', jacf)
    ncarfi = 3
    call jevech('PCAORIE', 'L', iorien)
    call matrot(zr(iorien), pgl)
    decga=nbfib*ndim
!
    call fmater(nfpgmx, nfpg, fami)
    decfpg(1)=0
    do 200 ifpg = 1, nfpg
!
        call elrefe_info(fami=fami(ifpg),ndim=ndim1,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
        if (ifpg .lt. nfpg) decfpg(ifpg+1)=decfpg(ifpg)+npg
!
!       POSITION DES POINTS DE GAUSS SUR L'AXE POUR FAMI
        call ppga1d(ndim, nno, npg, zr(ipoids), zr(ivf),&
                    zr(idfde), zr( igeom), copg)
        npgfa(ifpg) = npg
        do 10 ig = 1, npg
            do 20 k = 1, ndim
                copgfa(k,ig,ifpg)=copg(k,ig)
20          continue
10      continue
!
200  end do
!
!
    gm1(1)=0.d0
!
    do 100 ifi = 1, nbfib
        gm1(2)=zr(jacf+(ifi-1)*ncarfi)
        gm1(3)=zr(jacf+(ifi-1)*ncarfi+1)
        call utpvlg(1, 3, pgl, gm1, gm2)
        do 120 ifpg = 1, nfpg
            do 110 ig = 1, npgfa(ifpg)
!
                zr(icopg+(decfpg(ifpg)+ig-1)*decga+(ifi-1)*ndim+0)&
                =copgfa(1,ig,ifpg)+gm2(1)
                zr(icopg+(decfpg(ifpg)+ig-1)*decga+(ifi-1)*ndim+1)&
                =copgfa(2,ig,ifpg)+gm2(2)
                zr(icopg+(decfpg(ifpg)+ig-1)*decga+(ifi-1)*ndim+2)&
                =copgfa(3,ig,ifpg)+gm2(3)
110          continue
120      continue
100  end do
!
!
!
!
end subroutine
