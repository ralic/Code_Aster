function lisico(genchz, codcha)
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
!
    implicit none
    logical :: lisico
#include "jeveux.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lisdef.h"
    character(len=*) :: genchz
    integer :: codcha
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! RETOURNE .TRUE. SI LA CHARGE EST DU GENRE DONNE
!
! ----------------------------------------------------------------------
!
! IN  GENCHA : GENRE DE LA CHARGE (VOIR LISDEF)
! IN  CODCHA : CODE POUR LE GENRE DE LA CHARGE
!
!
!
!
    integer :: tabcod(30), iposit, ibid
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    lisico = .false.
    call lisdef('POEC', genchz, ibid, k8bid, iposit)
    call isdeco(codcha, tabcod, 30)
    if (tabcod(iposit) .eq. 1) lisico = .true.
!
    call jedema()
end function
