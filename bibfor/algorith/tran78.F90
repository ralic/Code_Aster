subroutine tran78(nomres, typres, nomin)
    implicit none
#include "asterfort/bamo78.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/macr78.h"
#include "asterfort/titre.h"
    character(len=8) :: nomres, nomin
    character(len=16) :: typres
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
! IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_COND_TRAN
! IN  : NOMIN  : NOM UTILISATEUR DU CONCEPT TRAN_GENE AMONT
! IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_TRANS'
!
    character(len=8) :: macrel
    character(len=19) :: trange
!
!-----------------------------------------------------------------------
    integer :: nmc
!-----------------------------------------------------------------------
    call jemarq()
    trange = nomin
    call getvid(' ', 'MACR_ELEM_DYNA', scal=macrel, nbret=nmc)
!
    if (nmc .ne. 0) then
        call macr78(nomres, trange, typres)
    else
        call bamo78(nomres, trange, typres)
    endif
    call titre()
!
    call jedema()
end subroutine
