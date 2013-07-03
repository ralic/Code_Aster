subroutine mmelty(noma, numa, alias, nno, ndim)
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
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=8) :: noma
    integer :: numa
    character(len=8) :: alias
    integer :: nno
    integer :: ndim
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! RETOURNE UN ALIAS POUR UN TYPE D'ELEMENT, LE NOMBRE DE NOEUDS
! DE CET ELEMENT ET SA DIMENSION
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMA   : NUMERO ABSOLU DE LA MAILLE
! OUT ALIAS  : TYPE DE L'ELEMENT
!               'PO1'
!               'SE2'
!               'SE3'
!               'TR3'
!               'TR6'
!               'TR7'
!               'QU4'
!               'QU8'
!               'QU9'
! OUT NNO    : NOMBRE DE NOEUDS DE CET ELEMENT
! OUT NDIM   : DIMENSION DE LA MAILLE
!
!
!
!
    integer :: iatyma, ityp, nutyp
    character(len=8) :: nomtm
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nomtm = ' '
!
! --- CODE TYPE DE LA MAILLE
!
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
    ityp = iatyma - 1 + numa
    nutyp = zi(ityp)
!
! --- NOM CATALOGUE
!
    call jenuno(jexnum('&CATA.TM.NOMTM', nutyp), nomtm)
!
    if (nomtm .eq. 'POI1') then
        alias = 'PO1'
        nno = 1
        ndim = 1
    else if (nomtm .eq. 'SEG2') then
        alias = 'SE2'
        nno = 2
        ndim = 2
    else if (nomtm .eq. 'SEG3') then
        alias = 'SE3'
        nno = 3
        ndim = 2
    else if (nomtm .eq. 'TRIA3') then
        alias = 'TR3'
        nno = 3
        ndim = 3
    else if (nomtm .eq. 'TRIA6') then
        alias = 'TR6'
        nno = 6
        ndim = 3
    else if (nomtm .eq. 'TRIA7') then
        alias = 'TR7'
        nno = 7
        ndim = 3
    else if (nomtm .eq. 'QUAD4') then
        alias = 'QU4'
        nno = 4
        ndim = 3
    else if (nomtm .eq. 'QUAD8') then
        alias = 'QU8'
        nno = 8
        ndim = 3
    else if (nomtm .eq. 'QUAD9') then
        alias = 'QU9'
        nno = 9
        ndim = 3
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
