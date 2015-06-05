subroutine mginfo(modmec, numddl, nbmode, neq)
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
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: modmec
    integer :: nbmode, neq
    character(len=14) :: numddl
!
!
! ----------------------------------------------------------------------
!
! UTILITAIRE
!
! INFORMATIONS SUR MATRICE MODES MECANIQUES
!
! ----------------------------------------------------------------------
!
!
! IN  MODMEC : NOM DE LA MATRICE DES MODES MECANIQUES
! OUT NUMDDL : NOM DU DDL
! OUT NBMODE : NOMBRE DE MODES
! OUT NEQ    : NOMBRE D'EQUATIONS
!
!
!
!
    character(len=24) :: matric
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INFORMATIONS SUR MATRICE DES MODES MECANIQUES
!
!
!
    call dismoi('NUME_DDL', modmec, 'RESU_DYNA', repk=numddl)
    if (numddl(1:1) .ne. ' ') then
        call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
    else
        call dismoi('REF_RIGI_PREM', modmec, 'RESU_DYNA', repk=matric)
        call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=numddl)
        call dismoi('NB_EQUA', matric, 'MATR_ASSE', repi=neq)
    endif
!
! --- NOMBRE DE MODES
!
    call jelira(modmec//'           .ORDR', 'LONMAX', nbmode)
!
    call jedema()
end subroutine
