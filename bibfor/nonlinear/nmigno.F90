function nmigno(jdiri, lndepl, ieq)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    aster_logical :: nmigno
    integer :: jdiri, ieq
    aster_logical :: lndepl
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! DIT SI ON DOIT IGNORER UN DDL LORS DU CALCUL DE L'EQUILIBRE
! POUR LA CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! IN  JDIRI  : POINTEUR SUR REACTIONS D'APPUI
! IN  IEQ    : NUMERO DE l'EQUATION COURANTE
! IN  LNDEPL : .TRUE. SI FORMULATION PAS EN DEPLACEMENT
!
!
!
!
!
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nmigno = .false.
    if (lndepl) then
        if (zr(jdiri+ieq-1) .ne. 0.d0) then
            nmigno = .true.
        endif
    endif
!
    call jedema()
end function
