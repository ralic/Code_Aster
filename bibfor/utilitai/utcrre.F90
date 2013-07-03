subroutine utcrre(result, nbval)
!
! ----------------------------------------------------------------------
!     UTILITAIRE : CREATION DES RESULTATS
!     **           **           **
! ----------------------------------------------------------------------
!
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterc/getres.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsrusd.h"
    integer :: nbval
    character(len=8) :: result
!
!
    integer :: iret
    integer :: nbav
!
    character(len=8) :: k8bid
    character(len=19) :: resu19
    character(len=16) :: typres, nomcmd
!
! ----------------------------------------------------------------------
!
    call getres(k8bid, typres, nomcmd)
!
! --- ALLOCATION (ON DIMENSIONNE PAR DEFAUT A 100)
!
    call jeexin(result(1:8)//'           .DESC', iret)
!
! --- SI LE RESULTAT N'EXISTE PAS, ON ALLOUE A NBVAL VALEURS
!
    if (iret .eq. 0) then
        call rscrsd('G', result, typres, nbval)
    else
! ----- SI LE RESULTAT N'EST PAS ASSEZ GRAND, ON L'AGRANDIT :
        resu19=result
        call jelira(resu19//'.ORDR', 'LONMAX', nbav, k8bid)
        if (nbval .gt. nbav) call rsagsd(result, nbval)
! ----- S'IL EXISTE, ON DETRUIT TOUT CE QUI SERAIT AU DELA DE NBVAL
        call rsrusd(result, nbval+1)
    endif
!
end subroutine
