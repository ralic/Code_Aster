subroutine xmchex(noma, nbma, chpmod, chelex)
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
#include "asterfort/celces.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=19) :: chelex, chpmod
    integer :: nbma
    character(len=8) :: noma
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - CREATION CHAM_ELEM)
!
! CREATION D'UN CHAM_ELEM_S VIERGE POUR ETENDRE LE CHAM_ELEM
! A PARTIR DE LA STRUCTURE D UN CHAMP EXISTANT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NBMA   : NOMBRE DE MAILLES
! IN  CHPMOD : CHAMP DONT LA STRUCTURE SERT DE MODELE
! OUT CHELEX : CHAM_ELEM_S PERMETTANT DE CREER UN CHAM_ELEM "ETENDU"
!
!
!
!
!
    integer :: nbcmp
    parameter     (nbcmp = 2)
    character(len=8) :: licmp(nbcmp)
    character(len=19) :: valk(2), chelsi
    integer :: vali(1)
!
    integer :: iad, ima
    integer :: jcesl,  jcesd
    integer, pointer :: cesd2(:) => null()
    integer, pointer :: cesv(:) => null()
!
    data licmp    / 'NPG_DYN', 'NCMP_DYN'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CREATION DU CHAM_ELEM_S VIERGE
!
    call cescre('V', chelex, 'ELEM', noma, 'DCEL_I',&
                nbcmp, licmp, [-1], [-1], [-nbcmp])
!
! --- ACCES AU CHAM_ELEM_S
!
    call jeveuo(chelex(1:19)//'.CESD', 'L', jcesd)
    call jeveuo(chelex(1:19)//'.CESL', 'E', jcesl)
    call jeveuo(chelex(1:19)//'.CESV', 'E', vi=cesv)
!
! --- TRANSFORMATION CHAMP MODELE EN CHAMP SIMPLE
    chelsi = '&&XMCHEX.CHELSI'
    call celces(chpmod, 'V', chelsi)
    call jeveuo(chelsi(1:19)//'.CESD', 'L', vi=cesd2)
!
! --- AFFECTATION DES COMPOSANTES DU CHAM_ELEM_S
!
    do 100 ima = 1, nbma
        call cesexi('C', jcesd, jcesl, ima, 1,&
                    1, 1, iad)
        if (iad .ge. 0) then
            vali(1) = 1
            valk(1) = chelex(1:19)
            valk(2) = 'ELEM'
            call utmess('F', 'CATAELEM_20', nk=2, valk=valk, si=vali(1))
        endif
        zl(jcesl-1-iad) = .true.
        cesv(1-1-iad) = cesd2(5+4*(ima-1)+2)
        call cesexi('C', jcesd, jcesl, ima, 1,&
                    1, 2, iad)
        if (iad .ge. 0) then
            vali(1) = 1
            valk(1) = chelex(1:19)
            valk(2) = 'ELEM'
            call utmess('F', 'CATAELEM_20', nk=2, valk=valk, si=vali(1))
        endif
        zl(jcesl-1-iad) = .true.
        cesv(1-1-iad) = cesd2(5+4*(ima-1)+3)
100  end do
!
    call detrsd('CHAM_ELEM_S', chelsi)
!
    call jedema()
!
end subroutine
