subroutine liscn2(lisold, nbchar, ichar, typapp)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: lisold
    character(len=16) :: typapp
    integer :: ichar, nbchar
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! CONVERTISSEUR ANCIENNE LISTE_CHARGES -> NOUVELLE LISTE_CHARGES
!
! TYPE D'APPLICATION
!
! ----------------------------------------------------------------------
!
!
! IN  TYPAPP : TYPE D'APPLICATION DE LA CHARGE
!              FIXE_CSTE
!              FIXE_PILO
!              SUIV
!              DIDI
!
! ----------------------------------------------------------------------
!
    character(len=24) :: infcha
    integer :: jinfch
    integer :: iinf1, iinf2, iinf3
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES ANCIENNE SD
!
    infcha = lisold(1:19)//'.INFC'
    call jeveuo(infcha, 'L', jinfch)
!
! --- INITIALISATIONS
!
    typapp = 'FIXE_CSTE'
!
    iinf1 = zi(jinfch+ichar)
    if ((iinf1.eq.5) .or. (iinf1.eq.6)) then
        typapp = 'FIXE_PILO'
    endif
!
    iinf2 = zi(jinfch+3*nbchar+2+ichar)
    if ((iinf2.eq.1)) then
        typapp = 'DIDI'
    endif
!
    iinf3 = zi(jinfch+nbchar+ichar)
    if ((iinf3.eq.5)) then
        typapp = 'FIXE_PILO'
    endif
    if ((iinf3.eq.4)) then
        typapp = 'SUIV'
    endif
!
    call jedema()
end subroutine
