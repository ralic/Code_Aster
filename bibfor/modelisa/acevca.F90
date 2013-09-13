subroutine acevca(nbocc, nlm, nlg, ier)
    implicit none
#include "asterc/getres.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/utmess.h"
    integer :: nbocc, nlm, nlg, ier
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     VERIFICATION DES MOTS CLES POUR L'ELEMENT CABLE
! ----------------------------------------------------------------------
! IN  : NBOCC  : NOMBRE D'OCCURENCE
! OUT : NLM    : NOMBRE TOTAL DE MAILLE
! OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
! ----------------------------------------------------------------------
    character(len=8) :: nomu
    character(len=16) :: concep, cmd
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ioc, ne, ng, nm, nsom
!-----------------------------------------------------------------------
    call getres(nomu, concep, cmd)
!
    nlm = 0
    nlg = 0
    do 10 ioc = 1, nbocc
        call getvtx('CABLE', 'GROUP_MA', iocc=ioc, nbval=0, nbret=ng)
        call getvtx('CABLE', 'MAILLE', iocc=ioc, nbval=0, nbret=nm)
        call getvr8('CABLE', 'SECTION', iocc=ioc, nbval=0, nbret=ne)
!
        if (ioc .eq. 1 .and. ne .eq. 0) then
            call utmess('E', 'MODELISA_52')
            ier = ier + 1
        endif
!
        nsom = ng + nm
        if (nsom .eq. ng .or. nsom .eq. nm) then
            nlm = max(nlm,-nm)
            nlg = max(nlg,-ng)
        endif
!
10  end do
!
end subroutine
