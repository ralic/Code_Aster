subroutine pjtyco(isole, resuin, cham1, lnoeu, lelno,&
                  lelem, lelga)
! person_in_charge: jacques.pellet at edf.fr
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
! COMMANDE:  PROJ_CHAMP
! BUT : DETERMINER LES TYPES DE CHAMP A PROJETER
!
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutc4.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
    character(len=8) :: resuin
    character(len=19) :: cham1
    aster_logical :: isole
    aster_logical :: lnoeu, lelno, lelem, lelga
!
!
!  LNOEU  : .TRUE.  : IL Y A UN CHAM_NO A PROJETER
!  LELNO  : .TRUE.  : IL Y A UN CHAM_ELEM DE TYPE ELNO A PROJETER
!  LELEM  : .TRUE.  : IL Y A UN CHAM_ELEM DE TYPE ELEM A PROJETER
!  LELGA  : .TRUE.  : IL Y A UN CHAM_ELEM DE TYPE ELGA A PROJETER
!
!
!
!
! 0.2. ==> COMMUNS
! ----------------------------------------------------------------------
!
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: i, ie, iret
    integer :: nbordr
    integer :: iordr, isym, nbsym
    aster_logical :: acceno
    real(kind=8) :: prec
    character(len=4) :: tych
    character(len=8) :: crit
    character(len=16) :: nomsym(200)
    integer, pointer :: nume_ordre(:) => null()
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    lnoeu=.false.
    lelno=.false.
    lelem=.false.
    lelga=.false.
!
!
!
!       1- CAS CHAMP ISOLE :
!       =====================
    if (isole) then
        call dismoi('TYPE_CHAMP', cham1, 'CHAMP', repk=tych)
        if (tych .eq. 'NOEU') then
            lnoeu=.true.
        else if (tych.eq.'ELNO') then
            lelno=.true.
        else if (tych.eq.'ELEM') then
            lelem=.true.
        else if (tych.eq.'ELGA') then
            lelga=.true.
        endif
!
!
!       2- CAS SD_RESULTAT :
!       =====================
    else
        call getvr8(' ', 'PRECISION', scal=prec, nbret=ie)
        call getvtx(' ', 'CRITERE', scal=crit, nbret=ie)
        call rsutnu(resuin, ' ', 0, '&&PJXXCO.NUME_ORDRE', nbordr,&
                    prec, crit, iret)
!
        if (iret .ne. 0) then
            call utmess('F', 'CALCULEL4_61', sk=resuin)
        endif
        if (nbordr .eq. 0) then
            call utmess('F', 'CALCULEL4_62', sk=resuin)
        endif
!
        call jeveuo('&&PJXXCO.NUME_ORDRE', 'L', vi=nume_ordre)
        call rsutc4(resuin, ' ', 1, 200, nomsym,&
                    nbsym, acceno)
!
!
!       -- DETERMINATION DE LNOEU
        do isym = 1, nbsym
            do i = 1, nbordr
                iordr=nume_ordre(i)
                call rsexch(' ', resuin, nomsym(isym), iordr, cham1,&
                            iret)
!
                if (iret .eq. 0) then
                    call dismoi('TYPE_CHAMP', cham1, 'CHAMP', repk=tych)
                    if (tych .eq. 'NOEU') then
                        lnoeu=.true.
                        goto 20
!
                    endif
                endif
!
            end do
 20         continue
        end do
!
!       -- DETERMINATION DE LELNO
        do isym = 1, nbsym
            do i = 1, nbordr
                iordr=nume_ordre(i)
                call rsexch(' ', resuin, nomsym(isym), iordr, cham1,&
                            iret)
!
                if (iret .eq. 0) then
                    call dismoi('TYPE_CHAMP', cham1, 'CHAMP', repk=tych)
                    if (tych .eq. 'ELNO') then
                        lelno=.true.
                        goto 40
!
                    endif
                endif
!
            end do
 40         continue
        end do
!
!       -- DETERMINATION DE LELEM
        do isym = 1, nbsym
            do i = 1, nbordr
                iordr=nume_ordre(i)
                call rsexch(' ', resuin, nomsym(isym), iordr, cham1,&
                            iret)
!
                if (iret .eq. 0) then
                    call dismoi('TYPE_CHAMP', cham1, 'CHAMP', repk=tych)
                    if (tych .eq. 'ELEM') then
                        lelem=.true.
                        goto 60
!
                    endif
                endif
!
            end do
 60         continue
        end do
!
!       -- DETERMINATION DE LELGA
        do isym = 1, nbsym
            do i = 1, nbordr
                iordr=nume_ordre(i)
                call rsexch(' ', resuin, nomsym(isym), iordr, cham1,&
                            iret)
!
                if (iret .eq. 0) then
                    call dismoi('TYPE_CHAMP', cham1, 'CHAMP', repk=tych)
                    if (tych .eq. 'ELGA') then
                        lelga=.true.
                        goto 80
!
                    endif
                endif
!
            end do
 80         continue
        end do
    endif
!
    call jedema()
end subroutine
