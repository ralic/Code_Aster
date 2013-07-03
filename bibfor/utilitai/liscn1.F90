subroutine liscn1(lisold, nbchar, ichar, nomfct, typfct,&
                  phase, npuis)
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
    character(len=16) :: typfct
    character(len=8) :: nomfct
    real(kind=8) :: phase
    integer :: npuis, ichar, nbchar
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! CONVERTISSEUR ANCIENNE LISTE_CHARGES -> NOUVELLE LISTE_CHARGES
!
! TYPE DE FONC_MULT
!
! ----------------------------------------------------------------------
!
!
!
!
! ----------------------------------------------------------------------
!
    character(len=24) :: infcha, fomult
    integer :: jinfch, jalifc
    integer :: iinf1, iinf2
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! - ACCES ANCIENNE SD
!
    infcha = lisold(1:19)//'.INFC'
    fomult = lisold(1:19)//'.FCHA'
    call jeveuo(infcha, 'L', jinfch)
    call jeveuo(fomult, 'L', jalifc)
!
! - INITIALISATIONS
!
    phase = 0.d0
    npuis = 0
    typfct = 'CONST_REEL'
    nomfct = zk24(jalifc+ichar-1)(1:8)
!
    iinf1 = zi(jinfch+ichar)
    if ((iinf1.eq.-2) .or. (iinf1.eq.-3) .or. (iinf1.eq.6) .or. (iinf1.eq.2) .or.&
        (iinf1.eq.3)) then
        typfct = 'FONCT_REEL'
    endif
!
    iinf2 = zi(jinfch+nbchar+ichar)
    if ((iinf2.eq.2) .or. (iinf2.eq.3)) then
        typfct = 'FONCT_REEL'
    endif
!
! - Cas des fonctions constantes
!
    if (typfct.eq.'FONCT_REEL') then
      if (nomfct(1:2).eq.'&&') typfct = 'CONST_REEL'
    endif
    
!
    call jedema()
end subroutine
