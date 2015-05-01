subroutine mecgm2(lischa, instan, mesuiv)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxliis.h"
#include "asterfort/wkvect.h"
    character(len=19) :: mesuiv, lischa
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL DE LA LISTE DES COEFFICIENTS POUR MATR_ELEM CHARGEMENTS
! SUIVEURS
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE_CHARGES
! IN  INSTAN : INSTANT COURANT
! I/O MESUIV : MATR_ELEM SUIVEUR
!               OUT - MESUIV(1:15)//'.COEF'
!
!
!
!
    integer :: nbchme, nchar
    integer :: iret, ichar, icha, ier
    aster_logical :: fct
    character(len=24) :: licoef, fomult
    integer :: jlicoe, jfonct
    real(kind=8) :: valres
    aster_logical :: bidon
    character(len=24), pointer :: relr(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    fomult = lischa(1:19)//'.FCHA'
    licoef = mesuiv(1:15)//'.COEF'
    bidon = .false.
!
! --- NOMBRE DE CHARGEMENTS SUIVEURS
!
    call jeexin(mesuiv(1:19)//'.RELR', iret)
    if (iret .ne. 0) then
        call jelira(mesuiv(1:19)//'.RELR', 'LONUTI', nbchme)
        if (nbchme .eq. 0) then
            bidon = .true.
        else
            call jeveuo(mesuiv(1:19)//'.RELR', 'L', vk24=relr)
            if (relr(1)(7:8) .eq. '00') then
                bidon = .true.
            endif
        endif
    else
        ASSERT(.false.)
    endif
    if (bidon) then
        goto 9999
    endif
!
! --- ACCES AUX FONCTIONS MULTIPLICATRICES
!
    call jeexin(fomult, iret)
    if (iret .eq. 0) then
        fct = .false.
    else
        fct = .true.
        call jelira(fomult, 'LONMAX', nchar)
        if (nchar .eq. 0) then
            ASSERT(.false.)
        endif
        call jeveuo(fomult, 'L', jfonct)
    endif
!
! --- CREATION SD COEF
!
    call jedetr(licoef)
    call wkvect(licoef, 'V V R', nbchme, jlicoe)
    do 1 ichar = 1, nbchme
        if (fct) then
!
! ------- ON RECUPERE LE NUMERO DE LA CHARGE ICHA STOCKEE DANS LE NOM
! ------- DU VECTEUR ASSEMBLE
!
            call lxliis(relr(ichar)(7:8), icha, ier)
            if (icha .gt. 0) then
                call fointe('F ', zk24(jfonct+icha-1)(1:8), 1, ['INST'], [instan],&
                            valres, ier)
            else
                ASSERT(.false.)
            endif
        else
            valres = 1.d0
        endif
        zr(jlicoe+ichar-1) = valres
  1 end do
!
9999 continue
!
    call jedema()
!
end subroutine
