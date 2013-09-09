subroutine dierre(sddisc, sdcrit, iterat)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmlere.h"
    integer :: iterat
    character(len=19) :: sddisc, sdcrit
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! ENREGISTRE LES RESIDUS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION
! IN  SDCRIT : SD CRITERE
! IN  ITERAT : NUMERO ITERATION NEWTON
!
!
!
!
!
    character(len=24) :: critcr
    integer :: jcrr
    real(kind=8) :: vrela(1), vmaxi(1), vchar(1)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ENREGISTRE LES RESIDUS A CETTE ITERATION
!
    critcr = sdcrit(1:19)//'.CRTR'
    call jeveuo(critcr, 'L', jcrr)
    vrela(1) = zr(jcrr+3-1)
    vmaxi(1) = zr(jcrr+4-1)
    vchar(1) = zr(jcrr+6-1)
    call nmlere(sddisc, 'E', 'VRELA', iterat, vrela(1))
    call nmlere(sddisc, 'E', 'VMAXI', iterat, vmaxi(1))
    call nmlere(sddisc, 'E', 'VCHAR', iterat, vchar(1))
!
    call jedema()
end subroutine
