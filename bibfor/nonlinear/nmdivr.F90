subroutine nmdivr(sddisc, sderro, iterat)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit     none
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmlere.h"
    integer :: iterat
    character(len=19) :: sddisc
    character(len=24) :: sderro
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE
!
! EVALUATION DE LA DIVERGENCE DU RESIDU
!
! ----------------------------------------------------------------------
!
! EVALUATION DE LA DIVERGENCE DU RESIDU :
!    ON DIT QU'IL Y A DIVERGENCE DU RESIDU SSI :
!       MIN[ R(I), R(I-1) ] > R(I-2), A PARTIR DE I=3 (COMME ABAQUS)
!
!    OU R(I)   EST LE RESIDU A L'ITERATION COURANTE
!       R(I-1) EST LE RESIDU A L'ITERATION MOINS 1
!       R(I-2) EST LE RESIDU A L'ITERATION MOINS 2
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : GESTION DES ERREURS
! IN  ITERAT : NUMERO ITERATION NEWTON
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: r(1), rm1(1), rm2(1)
    logical :: divres
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    divres = .false.
!
! --- DIVRES N'EST EVALUE QU'A PARTIR DE L'ITERATION 3
!
    if (iterat .lt. 3) goto 999
!
! --- RESIDU GLOBAL (RESI_GLOB_MAXI) A L'ITERATION COURANTE
!
    call nmlere(sddisc, 'L', 'VMAXI', iterat, r(1))
!
! --- RESIDU GLOBAL (RESI_GLOB_MAXI) A L'ITERATION "MOINS 1"
!
    call nmlere(sddisc, 'L', 'VMAXI', iterat-1, rm1(1))
!
! --- RESIDU GLOBAL (RESI_GLOB_MAXI) A L'ITERATION "MOINS 2"
!
    call nmlere(sddisc, 'L', 'VMAXI', iterat-2, rm2(1))
!
! --- SI LE RESIDU N'EST PAS DIMINUE SUR UNE DES 2 ITERATIONS : DIV
!
    if (min(r(1),rm1(1)) .gt. rm2(1)) divres = .true.
!
999  continue
!
! --- SAUVEGARDE DES EVENEMENTS
!
    call nmcrel(sderro, 'DIVE_RESI', divres)
!
    call jedema()
end subroutine
