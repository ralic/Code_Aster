subroutine barych(ch1z, ch2z, r1, r2, chz,&
                  base, nomsdz)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/idenob.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeimpo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/vrrefe.h"
#include "asterfort/vtcopy.h"

    character(len=*), intent(in) :: ch1z, ch2z, chz
    character(len=1), intent(in) :: base
    character(len=*), intent(in), optional :: nomsdz
    real(kind=8), intent(in) :: r1, r2
!----------------------------------------------------------------------
! but :   faire la combinaison lineraire de 2 champ :
!         ch = r1*ch1+ r2*ch2 (champ = cham_no ou cham_elem)

! in:  ch1    : nom du 1er champ
!      ch2    : nom du 2em champ
!      r1,r2  : coefficients multiplicateurs.
!      base   : 'G' ou 'V' (globale ou volatile)
!      ch     : nom du champ resultat.

! out: ch est rempli.
!----------------------------------------------------------------------
    character(len=19) :: ch1, ch2, ch
    character(len=4) :: docu, scal
    character(len=24) :: valk(2)
    character(len=8) :: nomsd
    integer :: i, jvale, jvale1, jvale2, ibid, ier
    integer :: lon1
    aster_logical :: iden
!-----------------------------------------------------------------------
    call jemarq()
    ch1=ch1z
    ch2=ch2z
    ch=chz

    nomsd='XXXX'
    if (present(nomsdz)) nomsd=nomsdz

    call copisd('CHAMP_GD', base, ch1, ch)

    call jeexin(ch//'.DESC', ibid)
    if (ibid .gt. 0) then
        call jelira(ch//'.DESC', 'DOCU', cval=docu)
    else
        call jelira(ch//'.CELD', 'DOCU', cval=docu)
    endif


    if (docu(1:4).eq.'CART') then
!   -----------------------------------
        valk(1)=nomsd
        call utmess('F', 'CALCULEL_30',nk=1,valk=valk)


    else if (docu(1:4).eq.'CHNO') then
!   -----------------------------------
        call jelira(ch1//'.VALE', 'LONMAX', lon1)
        call jelira(ch1//'.VALE', 'TYPE', cval=scal)
        call vrrefe(ch1, ch2, ier)
        if (ier .eq. 0) then

!           -- recopie brutale des .VALE
            call jeveuo(ch//'.VALE', 'E', jvale)
            call jeveuo(ch1//'.VALE', 'L', jvale1)
            call jeveuo(ch2//'.VALE', 'L', jvale2)
            if (scal(1:1) .eq. 'R') then
                do i = 1,lon1
                    zr(jvale-1+i) = r1*zr(jvale1-1+i) + r2*zr(jvale2-1+i)
                enddo
            else if (scal(1:1).eq.'C') then
                do i = 1,lon1
                    zc(jvale-1+i) = r1*zc(jvale1-1+i) + r2*zc(jvale2-1+i)
                enddo
            endif
        else
            call vtcopy(ch2, ch, ' ', ier)
            if ( ier.ne.0 ) then
                valk(1) = ch1
                valk(2) = ch2
                call utmess('F', 'ALGELINE7_21', nk=2, valk=valk)
            endif
            call jeveuo(ch//'.VALE', 'E', jvale)
            call jeveuo(ch1//'.VALE', 'L', jvale1)
            if (scal(1:1) .eq. 'R') then
                do i = 1,lon1
                    zr(jvale-1+i) = r1*zr(jvale1-1+i) + r2*zr(jvale-1+i)
                enddo
            else if (scal(1:1).eq.'C') then
                do i = 1,lon1
                    zc(jvale-1+i) = r1*zc(jvale1-1+i) + r2*zc(jvale-1+i)
                enddo
            endif
        endif


    else if (docu(1:4).eq.'CHML') then
!   -----------------------------------
!       -- on ne sait traiter que le cas tres simple ou les
!          objets '.CELV' ont exactement la meme organisation.
!          (memes nombres de sous-points et/ou de VARI)
        call vrrefe(ch1, ch2, ier)
        iden=(ier.eq.0)
        iden=iden .and. idenob(ch1//'.CELD',ch2//'.CELD')

        if (iden) then
            call jelira(ch1//'.CELV', 'TYPE', cval=scal)
            call jelira(ch//'.CELV', 'LONMAX', lon1)

            call jeveuo(ch//'.CELV', 'E', jvale)
            call jeveuo(ch1//'.CELV', 'L', jvale1)
            call jeveuo(ch2//'.CELV', 'L', jvale2)
            if (scal(1:1) .eq. 'R') then
                do i = 1,lon1
                    zr(jvale-1+i) = r1*zr(jvale1-1+i) + r2*zr(jvale2-1+i)
                enddo
            else if (scal(1:1).eq.'C') then
                do i = 1,lon1
                    zc(jvale-1+i) = r1*zc(jvale1-1+i) + r2*zc(jvale2-1+i)
                enddo
            endif
        else
            if (.false.) then
                call jeimpo(6, ch1//'.CELK', 'CELK_1:')
                call jeimpo(6, ch2//'.CELK', 'CELK_2:')
                call jeimpo(6, ch1//'.CELD', 'CELD_1:')
                call jeimpo(6, ch2//'.CELD', 'CELD_2:')
            endif
            call utmess('F', 'CALCULEL_27')
        endif
    else
        ASSERT(.false.)
    endif


    call jedema()
end subroutine
