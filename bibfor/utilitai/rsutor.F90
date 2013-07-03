subroutine rsutor(nomsd, champ, nomsym, iordr)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: iordr
    character(len=*) :: nomsd, champ, nomsym
! ----------------------------------------------------------------------
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
! person_in_charge: nicolas.sellenet at edf.fr
!      RECUPERATION DU NUMERO D'ORDRE ET DU NOM SYMBOLIQUE
!      CORRESPONDANT A UN NOM DE CHAMP D'UNE STRUCTURE DE DONNEES NOMSD.
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : CHAMP  : NOM DU CHAMP A RECHERCHER
! OUT : NOMSYM : NOM SYMBOLIQUE
! OUT : IORDR  : NUMERO D'ORDRE
! ----------------------------------------------------------------------
!
    character(len=3) :: chnoch, kbid
    character(len=6) :: chorch
    character(len=19) :: nomd2
    integer :: nunoch, jordr, irang, i1, nbordr
!
    nomd2 = nomsd
!
    chnoch = champ(10:12)
    chorch = champ(14:19)
!
    read(chnoch,'(I3)') nunoch
    read(chorch,'(I6)') i1
    irang=i1+1
!
    call jelira(nomd2//'.ORDR', 'LONUTI', nbordr, kbid)
    call assert(irang.ge.1 .and. irang.le.nbordr)
!
    call jeveuo(nomd2//'.ORDR', 'L', jordr)
    iordr = zi(jordr-1+irang)
    call jenuno(jexnum(nomd2//'.DESC', nunoch), nomsym)
!
end subroutine
