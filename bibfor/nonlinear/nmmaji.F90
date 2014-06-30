subroutine nmmaji(numedd, lgrot, lendo, sdnume, coef,&
                  incmoz, ddincz, incplz, ordre)
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
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/majour.h"
    logical(kind=1) :: lgrot, lendo
    real(kind=8) :: coef
    character(len=*) :: incplz, incmoz, ddincz
    character(len=24) :: numedd
    character(len=19) :: sdnume
    integer :: ordre
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MISE A JOUR DE L'INCONNUE EN DEPLACEMENT
!
! ----------------------------------------------------------------------
!
! INCPLU = INCMOI + COEF*DDINNC
!
! IN  NUMEDD : NOM DU NUME_DDL
! IN  COEF   : COEFFICIENT MULTIPLICATEUR
! IN  INCMOI : CHAMP DE DEPL. INITIAL
! OUT INCPLU : CHAMP DE DEPL. CORRIGE
! IN  DDINNC : INCREMENT CHAMP DE DEPL.
! IN  LGROT  : .TRUE.  S'IL Y A DES DDL DE GRDE ROTATION
! IN  SDNUME : SD NUMEROTATION
! IN  ORDRE  : 0 -> MAJ DES INCREMENTS
!              1 -> MAJ DES DEPL
!
!
!
!
    integer :: neq
    character(len=24) :: incplu, incmoi
    character(len=24) :: ddincc
    real(kind=8), pointer :: ddepl(:) => null()
    real(kind=8), pointer :: depm(:) => null()
    real(kind=8), pointer :: depp(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    incplu = incplz
    incmoi = incmoz
    ddincc = ddincz
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    call jeveuo(incmoi(1:19)//'.VALE', 'L', vr=depm)
    call jeveuo(incplu(1:19)//'.VALE', 'E', vr=depp)
    call jeveuo(ddincc(1:19)//'.VALE', 'E', vr=ddepl)
!
! --- MISE A JOUR
!
    call majour(neq, lgrot, lendo, sdnume, depm,&
                ddepl, coef, depp, ordre)
!
    call jedema()
end subroutine
