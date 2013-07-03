subroutine lisnol(lischa, genchz, nomlis, nbch)
!
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
!
    implicit      none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lisdef.h"
#include "asterfort/lisnnb.h"
    character(len=19) :: lischa
    character(len=24) :: nomlis
    integer :: nbch
    character(len=*) :: genchz
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! CREATION OBJETS CONTENANT LA LISTE DES INDEX POUR LE GENRE DONNE
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE DES CHARGES
! IN  GENCHA : GENRE DE LA CHARGE (VOIR LISDEF)
! OUT NOMLIS : LISTE DES INDEX DES CHARGES
! OUT NBCH   : LONGUEUR DE NOMLIS
!
!
!
!
    integer :: nbchar
    integer :: ibid, iposit
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nbch = 0
    call lisnnb(lischa, nbchar)
!
    if (nbchar .ne. 0) then
!
! ----- POSITION DE L'ENTIER CODE POUR CE GENRE DE CHARGE
!
        call lisdef('POEC', genchz, ibid, k8bid, iposit)
!
! ----- LISTE DES INDEX DE CHARGE POUR CE GENRE
!
        call lisdef('IDNS', nomlis, iposit, k8bid, nbch)
    endif
!
    call jedema()
end subroutine
