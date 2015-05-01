subroutine ltcrsd(litab, base)
    implicit   none
#include "asterfort/assert.h"
#include "asterfort/jeecra.h"
#include "asterfort/wkvect.h"
    character(len=*) :: litab, base
!     ------------------------------------------------------------------
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
!      CREER UNE SD L_TABLE DE NOM "LITAB" SUR UNE BASE "BASE"
!      LA STRUCTURE D'UNE L_TABLE :
!       .LTNT : K16 : REPERTOIRE DES NOMS SYMBOLIQUES
!       .LTNS : K24 : NOMS DES TABLE
!
! IN  : LITAB  : NOM DE LA SD L_TABLE A CREER.
! IN  : BASE   : BASE SUR LAQUELLE ON CREE LA SD L_TABLE
!
!     REMARQUE :
!        LA L_TABLE EST ALLOUEE A LA LONGUEUR 7, PUIS ELLE S'AGRANDIT
!        DE 6 EN 6
!     ------------------------------------------------------------------
    character(len=1) :: baselt
    character(len=19) :: listab
    integer :: jbid
! DEB------------------------------------------------------------------
!
    baselt = base(1:1)
    ASSERT(baselt.eq.'V' .or. baselt.eq.'G')
!
    listab = litab
!
!     --- CREATION DU .LTNT ---
!
    call wkvect(listab//'.LTNT', baselt//' V K16', 7, jbid)
    call jeecra(listab//'.LTNT', 'LONUTI', 0)
!
!     --- CREATION DU .LTNS ---
!
    call wkvect(listab//'.LTNS', baselt//' V K24', 7, jbid)
    call jeecra(listab//'.LTNS', 'LONUTI', 0)
!
end subroutine
