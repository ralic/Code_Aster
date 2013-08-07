subroutine mdsize(nomres, nbsauv, nbmode, nbchoc, nbrede,&
                  nbrevi)
!
    implicit none
#include "asterfort/jeecra.h"
    character(len=8) :: nomres
!-----------------------------------------------------------------------
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
!     DIMINUTION DES OBJETS D'UN TRAN_GENE DE NOM NOMRES
!-----------------------------------------------------------------------
! IN  : NOMRES : NOM DU TRAN_GENE RESULTAT
! IN  : NBSAUV : NOMBRE DE RESULTATS ARCHIVES (SELON ARCHIVAGE DES PAS
!                DE TEMPS).NOUVELLE TAILLE DE NOMRES
! IN  : NBMODE : NOMBRE DE MODES OU D'EQUATIONS GENERALISEES
! IN  : NBCHOC : NOMBRE DE CHOCS
! IN  : NBREDE : NOMBRE DE RELATION EFFORT DEPLACEMENT (RED)
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: nbchoc, nbmode, nbrede, nbsauv, nbsto1, nbstoc, nbrevi
!-----------------------------------------------------------------------
    nbstoc = nbsauv * nbmode
    call jeecra(nomres//'           .DEPL', 'LONUTI', nbstoc)
    call jeecra(nomres//'           .VITE', 'LONUTI', nbstoc)
    call jeecra(nomres//'           .ACCE', 'LONUTI', nbstoc)
    call jeecra(nomres//'           .ORDR', 'LONUTI', nbsauv)
    call jeecra(nomres//'           .DISC', 'LONUTI', nbsauv)
    call jeecra(nomres//'           .PTEM', 'LONUTI', nbsauv)
!
    if (nbchoc .gt. 0) then
        nbstoc = 3 * nbchoc * nbsauv
        nbsto1 = nbchoc * nbsauv
        call jeecra(nomres//'           .FCHO', 'LONUTI', nbstoc)
        call jeecra(nomres//'           .DLOC', 'LONUTI', nbstoc)
        call jeecra(nomres//'           .VCHO', 'LONUTI', nbstoc)
        call jeecra(nomres//'           .ICHO', 'LONUTI', nbsto1)
    endif
    if (nbrede .gt. 0) then
        nbstoc = nbrede * nbsauv
        call jeecra(nomres//'           .REDC', 'LONUTI', nbstoc)
        call jeecra(nomres//'           .REDD', 'LONUTI', nbstoc)
    endif
!
    if (nbrevi .gt. 0) then
        nbstoc = nbrevi * nbsauv
        call jeecra(nomres//'           .REVC', 'LONUTI', nbstoc)
        call jeecra(nomres//'           .REVD', 'LONUTI', nbstoc)
    endif
end subroutine
