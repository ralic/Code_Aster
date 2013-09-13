subroutine nmcrpp(motfaz, iocc, prec, criter, tole)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
    character(len=*) :: motfaz
    integer :: iocc
    character(len=8) :: criter
    real(kind=8) :: prec, tole
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (UTILITAIRE - SELEC. INST.)
!
! LECTURE PRECISION/CRITERE
!
! ----------------------------------------------------------------------
!
! NB: SI LE CRITERE EST RELATIF MAIS QUE _PRECISION_ N'EST PAS
!     PRECISEE, ALORS PRECISION VAUT PREDEF
!
! IN  MOTFAC : MOT-FACTEUR POUR LIRE (LIST_INST/INST)
! IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
! OUT PREC   : PRECISION DE RECHERCHE
! OUT CRITER : CRITERE DE SELECTION (RELATIF/ABSOLU)
! OUT TOLE   : TOLERANCE
!                +PREC POUR RELATIF
!                -PREC POUR ABSOLU
!
!
!
!
    integer :: n1, n2
    character(len=16) :: motfac
    real(kind=8) :: predef
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    prec = 0.d0
    tole = 0.d0
    criter = 'RELATIF'
    motfac = motfaz
    predef = 1.d-6
!
! --- LECTURE
!
    call getvr8(motfac, 'PRECISION', iocc=iocc, scal=prec, nbret=n1)
    call getvtx(motfac, 'CRITERE', iocc=iocc, scal=criter, nbret=n2)
    if (criter .eq. 'ABSOLU') then
        if (n1 .eq. 0) call u2mess('F', 'LISTINST_1')
    else if (criter.eq.'RELATIF') then
        if (n1 .eq. 0) then
            prec = predef
            call u2mesr('A', 'LISTINST_2', 1, predef)
        endif
    else
        ASSERT(.false.)
    endif
!
    if (prec .le. r8prem()) then
        call u2mess('F', 'LISTINST_3')
    endif
!
! --- TOLERANCE
!
    if (criter .eq. 'RELATIF') then
        tole = prec
    else if (criter.eq.'ABSOLU') then
        tole = -prec
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine
