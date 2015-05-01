subroutine limsst(nomcmd)
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
    implicit none
!
!***********************************************************************
!  C. VARE     DATE 31/10/94
!-----------------------------------------------------------------------
!
!  BUT : VERIFIER LES DONNEES UTILISATEUR EN FONCTION DES POSSIBILITES
!        DU CALCUL TRANSITOIRE PAR SOUS-STRUCTURATION
!
!-----------------------------------------------------------------------
!
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/utmess.h"
!
    integer :: nbchoc, nbrede, nbrevi
    character(len=24) :: valk(2)
    character(len=16) :: nomcmd, method
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: n1, n2, namor
!-----------------------------------------------------------------------
    call getvtx('SCHEMA_TEMPS', 'SCHEMA', iocc=1, scal=method, nbret=n1)
    call getfac('ETAT_INIT', n2)
    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=0, nbret=namor)
    call getfac('CHOC', nbchoc)
    call getfac('RELA_EFFO_DEPL', nbrede)
    call getfac('RELA_EFFO_VITE', nbrevi)
!
    if (method .ne. 'EULER' .and. method(1:5) .ne. 'ADAPT' .and. method(1:5) .ne. 'RUNGE') then
        valk (1) = method
        valk (2) = 'EULER,RUNGE_..,ADAPT_..'
        call utmess('F', 'ALGORITH13_29', nk=2, valk=valk)
    endif
!
    if (n2 .ne. 0) then
        call utmess('F', 'ALGORITH13_30')
    endif
!
    if (nbchoc .ne. 0) then
        call utmess('F', 'ALGORITH13_31')
    endif
!
    if (nbrede .ne. 0) then
        call utmess('F', 'ALGORITH13_32')
    endif
!
    if (nbrevi .ne. 0) then
        call utmess('F', 'ALGORITH13_33')
    endif
!
    if (namor .ne. 0) then
        call utmess('F', 'ALGORITH13_34')
    endif
!
end subroutine
