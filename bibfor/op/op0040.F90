subroutine op0040()
    implicit none
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
! ----------------------------------------------------------------------
! OPERATEUR : INFO_RESU
! BUT       : FOURNIR LES COMPOSANTES DES CHAMPS PRESENTS DANS UNE SD
!             DE DONNEES RESULTAT
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
#include "jeveux.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/cmpcha.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
!
    integer :: ifm, niv, ibid, nb_cmp, icmp, jatach, nbcham
    integer :: isy, iord, ifi, n2
    character(len=16) :: nomsym, nomfi
    character(len=19) :: resuin, nomcha
    integer, pointer :: cata_to_field(:) => null()
    integer, pointer :: field_to_cata(:) => null()
    character(len=8), pointer :: cmp_name(:) => null()
!
    call infniv(ifm, niv)
!
    ifi = 0
    nomfi = ' '
    call getvis(' ', 'UNITE', scal=ifi, nbret=n2)
    if (.not. ulexis( ifi )) then
        call ulopen(ifi, ' ', nomfi, 'NEW', 'O')
    endif
!
    call jemarq()
!
!     RECUPERATION DU NOM DU RESULTAT
    call getvid(' ', 'RESULTAT', scal=resuin, nbret=ibid)
!
    write(ifi,*) '-----------------------------------------------',&
     &                '------------'
    write(ifi,*)&
     & 'COMPOSANTES DES CHAMPS PRESENTS DANS LE RESULTAT : ',resuin
!
!     LECTURE DU NOMBRE DE CHAMPS PRESENTS ET DU NOMBRE D'ORDRE
    call jelira(resuin//'.DESC', 'NOMMAX', nbcham)
!
    do isy = 1, nbcham
        call jenuno(jexnum(resuin//'.DESC', isy), nomsym)
        call jenonu(jexnom(resuin//'.DESC', nomsym), ibid)
        call jeveuo(jexnum(resuin//'.TACH', ibid), 'L', jatach)
        iord = 1
        if (zk24(jatach-1+iord)(1:1) .ne. ' ') then
            write(ifi,*) '   - CHAMP ',nomsym,' :'
            nomcha = zk24(jatach-1+iord)(1:19)
            call cmpcha(nomcha, cmp_name, cata_to_field, field_to_cata, nb_cmp)
            do icmp = 1,nb_cmp
                write(ifi,*) '      * ',cmp_name(icmp)
            end do
            AS_DEALLOCATE(vi = cata_to_field)
            AS_DEALLOCATE(vi = field_to_cata)
            AS_DEALLOCATE(vk8 = cmp_name)
        endif
    end do
!
    write(ifi,*) '-----------------------------------------------',&
     &                '------------'
!
    call jedema()
end subroutine
