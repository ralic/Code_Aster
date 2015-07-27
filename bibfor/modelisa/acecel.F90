subroutine acecel(noma, nomo, nbocc, ele_sup_num, ele_sup_typ, nb_ty_el, zjdlm, ier)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!     AFFE_CARA_ELEM
!     COMPTEUR D'ELEMENTS
!
! --------------------------------------------------------------------------------------------------
!
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
    use cara_elem_parameter_module
    implicit none
    character(len=8) :: noma, nomo
    integer :: nbocc(*), ele_sup_num(*), ele_sup_typ(*), nb_ty_el(*), zjdlm(*), ier
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: ii, ifm, ixma, tt, jdme, nbmail, nummai, nutyel
!
    character(len=24) :: mlgnma, modmai
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    modmai = nomo//'.MAILLE'
    mlgnma = noma//'.NOMMAI'
    call jeexin(modmai, ixma)
    call jelira(mlgnma, 'NOMMAX', nbmail)
    ASSERT( ixma .ne. 0 )
    call jeveuo(modmai, 'L', jdme)
!
    ifm = iunifi('MESSAGE')
!
    nb_ty_el(1:ACE_NB_ELEMENT) = 0
!
    do nummai = 1, nbmail
        nutyel = zi(jdme+nummai-1)
        zjdlm(nummai) = nutyel
!
        do ii = 1 , ACE_NB_TYPE_ELEM
            if ( nutyel .eq. ele_sup_num(ii) ) then
                tt = ele_sup_typ(ii)
                nb_ty_el(tt) = nb_ty_el(tt) + 1
                exit
            endif
        enddo
    enddo
!
    write(ifm,100) nomo
    do ii=1 ,ACE_NB_ELEMENT
        if ( nb_ty_el(ii) .gt. 0 ) then
            write(ifm,110) nb_ty_el(ii),ACE_NM_ELEMENT(ii)
        endif
    enddo
100 format(/,5x,'LE MODELE ',a8,' CONTIENT : ')
110 format(35x,i6,' ELEMENT(S) ',A16)
!
!   Vérification de la cohérence des affectations
    if (nbocc(ACE_POUTRE).ne.0 .and. nb_ty_el(ACE_NU_POUTRE).eq.0) then
        call utmess('E', 'MODELISA_29', sk=nomo)
        ier = ier + 1
    endif
    if (nbocc(ACE_COQUE).ne.0 .and. nb_ty_el(ACE_NU_COQUE).eq.0) then
        call utmess('E', 'MODELISA_30', sk=nomo)
        ier = ier + 1
    endif
    if ((nbocc(ACE_DISCRET)+nbocc(ACE_DISCRET_2D)).ne.0 .and. &
         nb_ty_el(ACE_NU_DISCRET).eq.0) then
        call utmess('E', 'MODELISA_31', sk=nomo)
        ier = ier + 1
    endif
    if (nbocc(ACE_ORIENTATION).ne.0 .and. (nb_ty_el(ACE_NU_POUTRE) + &
        nb_ty_el(ACE_NU_DISCRET)+nb_ty_el(ACE_NU_BARRE)).eq.0) then
        call utmess('E', 'MODELISA_32', sk=nomo)
        ier = ier + 1
    endif
    if (nbocc(ACE_DEFI_ARC).ne.0 .and. nb_ty_el(ACE_NU_POUTRE).eq.0) then
        call utmess('E', 'MODELISA_29', sk=nomo)
        ier = ier + 1
    endif
    if (nbocc(ACE_CABLE).ne.0 .and. nb_ty_el(ACE_NU_CABLE).eq.0) then
        call utmess('E', 'MODELISA_33', sk=nomo)
        ier = ier + 1
    endif
    if (nbocc(ACE_BARRE).ne.0 .and. nb_ty_el(ACE_NU_BARRE).eq.0) then
        call utmess('E', 'MODELISA_34', sk=nomo)
        ier = ier + 1
    endif
    if (nbocc(ACE_MASSIF).ne.0 .and. (nb_ty_el(ACE_NU_MASSIF)+nb_ty_el(ACE_NU_THHMM)).eq.0) then
        call utmess('E', 'MODELISA_35', sk=nomo)
        ier = ier + 1
    endif
    if (nbocc(ACE_GRILLE).ne.0 .and. nb_ty_el(ACE_NU_GRILLE).eq.0) then
        call utmess('E', 'MODELISA_36', sk=nomo)
        ier = ier + 1
    endif
    if (nbocc(ACE_MEMBRANE).ne.0 .and. nb_ty_el(ACE_NU_MEMBRANE).eq.0) then
        call utmess('E', 'MODELISA_55', sk=nomo)
        ier = ier + 1
    endif
    if (nbocc(ACE_MASS_REP).ne.0 .and. nb_ty_el(ACE_NU_DISCRET).eq.0 ) then
        call utmess('E', 'MODELISA_31', sk=nomo)
        ier = ier + 1
    endif
!
    call jedema()
end subroutine
