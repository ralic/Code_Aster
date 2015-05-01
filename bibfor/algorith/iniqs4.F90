subroutine iniqs4(nno, sdfde, sdfdk, poipg, coopg)
    implicit none
#include "jeveux.h"
#include "asterfort/elraga.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nno
    real(kind=8) :: sdfde(4, 4), sdfdk(4, 4), coopg(8), poipg(4)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! =============================================================
!   BUT : RECUPERER TOUS LES INDICES DU VECTEUR ZR DANS LEQUEL
!         SE TROUVE LES COORD., LES DFDE et DFDK, LE POIDS DE
!         LA DEUXIEMME FAMILLE DE PT DE GAUSS DE QUAS4.
!
!        IN   :  NNO  NOMBRE DE NOEUDS
!        OUT  :  DFDE   DERIVEE DES FF DANS REP DE REF
!        OUT  :  DFDK   DERIVEE DES FF DANS REP DE REF
!        OUT  :  POIDS  POIDS DES PTS DE GAUSS
!        OUT  :  COOPG  COORD.DES PTS DE GAUSS
!
! =============================================================
!
    integer :: i, j, k, ndim, nnos, npg, ipoids, ivf, idfde, jgano, nbpg
    character(len=8) :: elrefe, famil
!     ------------------------------------------------------------------
!
    call jemarq()
!
    elrefe = 'QU4     '
    famil = 'FPG4    '
!
    call elraga(elrefe, famil, ndim, nbpg, coopg,&
                poipg)
!
    call elrefe_info(elrefe=elrefe,fami='MASS',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    do 40 i = 1, npg
        k = 2*nno*(i-1)
        do 30 j = 1, nno
            sdfde(i,j) = zr(idfde+k+2*(j-1)-1+1)
            sdfdk(i,j) = zr(idfde+k+2*(j-1)-1+2)
30      continue
40  end do
!
    call jedema()
end subroutine
