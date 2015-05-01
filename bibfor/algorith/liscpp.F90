subroutine liscpp(lischa, ichar, phase, npuis)
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lisnnb.h"
    character(len=19) :: lischa
    integer :: ichar
    real(kind=8) :: phase
    integer :: npuis
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! RETOURNE LA PHASE ET LA PUISSANCE POUR UNE FONCTION
! MULTIPLICATRICE COMPLEXE
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : NOM DE LA SD LISTE_CHARGES
! IN  ICHAR  : INDICE DE LA CHARGE
! OUT PHASE  : PHASE POUR LES FONCTIONS MULTIPLICATRICES COMPLEXES
! OUT NPUIS  : PUISSANCE POUR LES FONCTIONS MULTIPLICATRICES COMPLEXES
!
!
!
!
    character(len=24) :: valfon
    integer :: jvfon
    integer :: nbchar
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    phase = 0.d0
    npuis = 0
    call lisnnb(lischa, nbchar)
!
    if (nbchar .ne. 0) then
        valfon = lischa(1:19)//'.VFON'
        call jeveuo(valfon, 'L', jvfon)
        phase = zr(jvfon-1+2*(ichar-1)+1)
        npuis = nint(zr(jvfon-1+2*(ichar-1)+2))
    endif
!
    call jedema()
end subroutine
