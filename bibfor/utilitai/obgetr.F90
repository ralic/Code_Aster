subroutine obgetr(nomstr, nompaz, valr)
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
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/obpara.h"
    character(len=24) :: nomstr
    character(len=*) :: nompaz
    real(kind=8) :: valr
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT)
!
! LECTURE D'UN REEL
!
! ----------------------------------------------------------------------
!
!
! IN  NOMSTR : NOM DU STRUCT
! IN  NOMPAR : NOM DU PARAMETRE
! OUT VALR   : VALEUR DU PARAMETRE DE TYPE REEL
!
! ----------------------------------------------------------------------
!
    character(len=24) :: sdvalr
    integer :: jsvalr
    character(len=24) :: nompar
    character(len=1) :: typpar
    integer :: indice
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    indice = 0
    nompar = nompaz
    typpar = ' '
!
! --- VALEURS PARAMETRES REELS
!
    sdvalr = nomstr(1:19)//'.VALE'
!
! --- REPERAGE PARAMETRE DANS LA SD
!
    call obpara(nomstr, nompar, indice, typpar)
!
! --- LECTURE
!
    if (typpar .eq. 'R') then
        call jeveuo(sdvalr, 'L', jsvalr)
        valr = zr(jsvalr-1+indice)
    else
        write(6,*) 'TYPE INCORRECT: ',typpar
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
