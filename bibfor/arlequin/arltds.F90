subroutine arltds(nns   ,npgs  , &
                  ipoids,icoors,ivfs  ,idfdes, &
                  poijcs,fctfs    ,dfdxs ,dfdys ,dfdzs )



! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================


    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "blas/dcopy.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/jedema.h"

    integer :: nns,npgs
    integer :: ivfs,ipoids,idfdes,icoors
    real(kind=8) ::  poijcs(npgs)
    real(kind=8) ::  fctfs(npgs*nns)
    real(kind=8) ::  dfdxs(npgs*nns),dfdys(npgs*nns),dfdzs(npgs*nns)

! ----------------------------------------------------------------------

! CALCUL DES MATRICES DE COUPLAGE ARLEQUIN
! OPTION ARLQ_MATR

! CALCUL DES DERIVEES DES FF DE LA MAILLE SUPPORT S

! ----------------------------------------------------------------------


! IN  NNS    : NOMBRE DE NOEUDS DE LA MAILLE SUPPORT S
! IN  NPGS   : NOMBRE DE POINTS DE GAUSS DE LA MAILLE SUPPORT S
! IN  IPOIDS : POINTEUR VERS POIDS DE GAUSS DE LA MAILLE SUPPORT S
! IN  ICOORS : POINTEUR VERS COORD. NOEUDS DE LA MAILLE SUPPORT S
! IN  IVFS   : POINTEUR VERS FONCTIONS DE FORME DE LA MAILLE SUPPORT S
! IN  IDFDES : POINTEUR VERS DER. FONCTIONS DE FORME DE LA MAILLE S
! OUT POIJCS : POIDS DE GAUSS*JACOBIEN
! OUT FCTFS  : FONCTIONS DE FORME
! OUT DFDXS  : DER/X FONCTIONS DE FORME
! OUT DFDYS  : DER/Y FONCTIONS DE FORME
! OUT DFDZS  : DER/Z FONCTIONS DE FORME



    integer ::  mtl,kpgs

! ----------------------------------------------------------------------
    call jemarq()

! --- calcul des derivees de fct formes + jacobien transfo maille support

    do 10 kpgs = 1,npgs
        mtl  = nns*(kpgs-1)+1
        call dcopy(nns,zr(ivfs-1+mtl),1,fctfs(mtl)   ,1)
        call dfdm3d(nns       ,kpgs  ,ipoids,idfdes, &
                    zr(icoors), &
                    poijcs(kpgs)  , &
                    dfdxs(mtl)  , &
                    dfdys(mtl)   , &
                    dfdzs(mtl))
    10 end do

    call jedema()

end subroutine
