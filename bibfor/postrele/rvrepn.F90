subroutine rvrepn(mailla, nlsnac, repere, sdnewr)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "jeveux.h"
!
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rvrlln.h"
    character(len=24) :: nlsnac
    character(len=19) :: sdnewr
    character(len=8) :: mailla, repere
!
!***********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     CALCUL  DU REPERE LOCAL OU POLAIRE LA LONG D' UNE LISTE DE NOEUDS
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     REPERE : VAUT 'LOCAL' OU 'POLAIRE'
!     COURBE : NOM DE LISTE DE NOEUDS
!     MAILLA : NOM DU MAILLAGE
!
!  ARGUMENTS EN SORTIE
!  -------------------
!
!     SDNEWR : NOM DE LA SD DU NOUVEAU REPERE
!              (DOC. C.F. RVCHGR)
!
!***********************************************************************
!
!  -----------------------------------------
!
!
!
!  ---------------------------------
!
!  VARIABLES LOCALES
!  -----------------
!
    integer :: acoord, avec1, avec2, nbn, alsnac
!
!====================== CORPS DE LA ROUTINE ===========================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    call jelira(nlsnac, 'LONMAX', nbn)
    call jeveuo(nlsnac, 'L', alsnac)
!
    call jeveuo(mailla//'.COORDO    .VALE', 'L', acoord)
!
    call jecrec(sdnewr//'.VEC1', 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                1)
    call jecrec(sdnewr//'.VEC2', 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                1)
    call jecroc(jexnum(sdnewr//'.VEC1', 1))
    call jecroc(jexnum(sdnewr//'.VEC2', 1))
    call jeecra(jexnum(sdnewr//'.VEC1', 1), 'LONMAX', 2*nbn)
    call jeecra(jexnum(sdnewr//'.VEC2', 1), 'LONMAX', 2*nbn)
    call jeveuo(jexnum(sdnewr//'.VEC1', 1), 'E', avec1)
    call jeveuo(jexnum(sdnewr//'.VEC2', 1), 'E', avec2)
!
    call rvrlln(zr(acoord), zi(alsnac), nbn, repere, zr(avec1),&
                zr(avec2))
!
    call jedema()
end subroutine
