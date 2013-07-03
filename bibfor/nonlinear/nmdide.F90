subroutine nmdide(lreuse, result, numder, insder)
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
    implicit     none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsorac.h"
    character(len=8) :: result
    logical :: lreuse
    integer :: numder
    real(kind=8) :: insder
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - DISCRETISATION)
!
! DERNIER NUMERO ARCHIVE DANS LA SD SI ETAT_INIT
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM DE LA SD RESULTAT
! IN  LREUSE : .TRUE. SI CONCEPT REENTRANT
! OUT NUMDER : DERNIER NUMERO ARCHIVE DANS RESULT
!               (OU 0 SI NON REENTRANT)
! OUT INSDER : DERNIER INSTANT ARCHIVE DANS RESULT
!               (R8VIDE SI NON REENTRANT)
!
!
!
!
    integer :: ibid, jinst
    character(len=8) :: k8bid
    complex(kind=8) :: c16bid
    real(kind=8) :: r8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    numder = 0
    insder = r8vide()
!
! --- ACCES SD RESULTAT (OU PAS)
!
    if (lreuse) then
        call rsorac(result, 'DERNIER', ibid, r8bid, k8bid,&
                    c16bid, 0.d0, 'ABSOLU', numder, 1,&
                    ibid)
        call rsadpa(result, 'L', 1, 'INST', numder,&
                    0, jinst, k8bid)
        insder = zr(jinst)
    else
        numder = 0
        insder = r8vide()
    endif
!
    call jedema()
end subroutine
