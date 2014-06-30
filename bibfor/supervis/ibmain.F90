subroutine ibmain(lldbg)
! aslint: disable=
    implicit none
#include "asterc/faster.h"
#include "asterc/gtopti.h"
#include "asterc/inisig.h"
#include "asterfort/ib0mai.h"
#include "asterfort/lxinit.h"
    logical(kind=1) :: lldbg
!     ------------------------------------------------------------------
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
!     ENSEMBLE DES INITIALISATIONS POUR L'EXECUTION D'UN JOB
!     ------------------------------------------------------------------
!     ------- COMMUN DEBUG SUPERVISEUR ---------------------------------
    logical(kind=1) :: ldbg
    integer :: ifv
    common /cxsu00/ ldbg , ifv
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: issuiv, iret
!
    ldbg=lldbg
!     --- BUFFERISATION EN CAS DE SUIVI INTERACTIF
    call gtopti('suivi_batch', issuiv, iret)
    if (issuiv .gt. 0 .and. iret .eq. 0) then
        call faster()
    endif
!
!     --- INITIALISATION DE L'ANALYSEUR LEXICAL ET DE L'UNITE DE LECTURE
    call lxinit()
!
!     --- INITIALISATION DE L"INTERCEPTION DE CERTAINS SIGNAUX
    call inisig()
!
!     --- INITIALISATION DE JEVEUX   ---
    call ib0mai()
!
end subroutine
