subroutine vrcin_elno(nomch, cesmod, chs)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsces.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=19), intent(in) :: nomch, cesmod, chs
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sam.cuvilliez at edf.fr
!-----------------------------------------------------------------------
!
!   Sous-routine de vrcin1. Cas particulier ou l'on souhaite produire 
!   un champ de varc ELNO.
!
!   in nomch  : nom du champ dont le support doit etre modifie
!               (cham_no -> cham_elem_s / ELNO)
!   in cesmod : nom du cham_elem_s "modele" pour appel a cnsces
!   in chs    : nom du cham_elem_s / ELNO a creer a partir de nomch
!
!-----------------------------------------------------------------------
!
    character(len=8) :: tych
    character(len=19) :: cns1
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
!   seul les cham_no sont autorises 
    call dismoi('TYPE_CHAMP', nomch, 'CHAMP', repk=tych)
    ASSERT(tych .eq. 'NOEU')
!
!   passage cham_no -> cham_elem_s / ELNO
    cns1='&&VRCINE.CNS1'
    call cnocns(nomch, 'V', cns1)
    call cnsces(cns1, 'ELNO', cesmod, ' ', 'V', chs)
!
!   menage
    call detrsd('CHAM_NO_S', cns1)
!
    call jedema()
!
end subroutine
