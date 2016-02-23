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
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
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
! ----------------------------------------------------------------------
!     BUT :   FAIRE LA COMBINAISON LINERAIRE DE 2 CHAMP :
!             CH = R1*CH1+ R2*CH2 (CHAMP = CHAM_NO OU CHAM_ELEM)
!
! IN:  CH1    : NOM DU 1ER CHAMP
!      CH2    : NOM DU 2EM CHAMP
!      R1,R2  : COEFFICIENTS MULTIPLICATEURS.
!      BASE   : 'G' OU 'V' (GLOBALE OU VOLATILE)
!      CH     : NOM DU CHAMP RESULTAT.
!
! OUT: CH EST REMPLI.
! ----------------------------------------------------------------------
    character(len=19) :: ch1, ch2, ch
    character(len=5) :: vale
    character(len=4) :: docu, scal
    character(len=24) :: valk(2)
    character(len=8) :: nomsd
!-----------------------------------------------------------------------
    integer :: i, iach, iach1, iach2, ibid, ier
    integer :: lon1, lon2, long
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
        vale='.VALE'
        call jelira(ch1//vale, 'LONMAX', lon1)
        call jelira(ch1//vale, 'TYPE', cval=scal)
        call vrrefe(ch1, ch2, ier)
        if (ier .eq. 0) then

!           -- recopie brutale des .VALE
            call jeveuo(ch//vale, 'E', iach)
            call jeveuo(ch1//vale, 'L', iach1)
            call jeveuo(ch2//vale, 'L', iach2)
            if (scal(1:1) .eq. 'R') then
                do i = 1,lon1
                    zr(iach-1+i) = r1*zr(iach1-1+i) + r2*zr(iach2-1+i)
                enddo
            else if (scal(1:1).eq.'C') then
                do i = 1,lon1
                    zc(iach-1+i) = r1*zc(iach1-1+i) + r2*zc(iach2-1+i)
                enddo
            endif
        else
            call vtcopy(ch2, ch, ' ', ier)
            if ( ier.ne.0 ) then
                valk(1) = ch1
                valk(2) = ch2
                call utmess('F', 'ALGELINE7_21', nk=2, valk=valk)
            endif
            call jeveuo(ch//vale, 'E', iach)
            call jeveuo(ch1//vale, 'L', iach1)
            if (scal(1:1) .eq. 'R') then
                do i = 1,lon1
                    zr(iach-1+i) = r1*zr(iach1-1+i) + r2*zr(iach-1+i)
                enddo
            else if (scal(1:1).eq.'C') then
                do i = 1,lon1
                    zc(iach-1+i) = r1*zc(iach1-1+i) + r2*zc(iach-1+i)
                enddo
            endif
        endif
!
!
    else if (docu(1:4).eq.'CHML') then
!   -----------------------------------
        call vrrefe(ch1, ch2, ier)
        if (ier .eq. 0) then
            vale='.CELV'
            call jelira(ch1//vale, 'TYPE', cval=scal)
            call jelira(ch1//vale, 'LONMAX', lon1)
            call jelira(ch2//vale, 'LONMAX', lon2)
            call jelira(ch//vale, 'LONMAX', long)
            ASSERT((lon1.eq.lon2).and.(lon1.eq.long))
!
            call jeveuo(ch//vale, 'E', iach)
            call jeveuo(ch1//vale, 'L', iach1)
            call jeveuo(ch2//vale, 'L', iach2)
            if (scal(1:1) .eq. 'R') then
                do i = 1,lon1
                    zr(iach-1+i) = r1*zr(iach1-1+i) + r2*zr(iach2-1+i)
                enddo
            else if (scal(1:1).eq.'C') then
                do i = 1,lon1
                    zc(iach-1+i) = r1*zc(iach1-1+i) + r2*zc(iach2-1+i)
                enddo
            endif
        else
            call utmess('F', 'CALCULEL_27')
        endif
    else
        ASSERT(.false.)
    endif
!
!
    call jedema()
end subroutine
