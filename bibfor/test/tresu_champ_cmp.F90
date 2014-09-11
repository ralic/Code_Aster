subroutine tresu_champ_cmp(chamgd, typtes, typres, nbref, tbtxt,&
                           refi, refr, refc, epsi, lign1,&
                           lign2, crit, ific, nbcmp, nocmp,&
                           llab, ssigne, ignore, compare)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesred.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/tresu_print_all.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*), intent(in) :: chamgd
    character(len=8), intent(in) :: typtes
    character(len=*), intent(in) :: typres
    integer, intent(in) :: nbref
    character(len=16), intent(in) :: tbtxt(2)
    integer, intent(in) :: refi(nbref)
    real(kind=8), intent(in) :: refr(nbref)
    complex(kind=8), intent(in) :: refc(nbref)
    real(kind=8), intent(in) :: epsi
    character(len=200), intent(inout) :: lign1
    character(len=200), intent(inout) :: lign2
    character(len=*), intent(in) :: crit
    integer, intent(in) :: ific
    integer, intent(in) :: nbcmp
    character(len=8), intent(in) :: nocmp(*)
    aster_logical, intent(in) :: llab
    character(len=*), intent(in) :: ssigne
    aster_logical, intent(in), optional :: ignore
    real(kind=8), intent(in), optional :: compare
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
! IN  : CHAMGD : NOM DU CHAM_GD
! IN  : TYPTES : TYPE DE TEST A EFFECTUER SUR LE CHAMP
! IN  : REFI   : VALEUR REELLE ENTIERE ATTENDUE
! IN  : REFR   : VALEUR REELLE ATTENDUE
! IN  : REFC   : VALEUR COMPLEXE ATTENDUE
! IN  : CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
! IN  : EPSI   : PRECISION ESPEREE
! IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
! IN  : TBTXT  : (1)=REFERENCE, (2)=LEGENDE
! IN  : NODDL  : NOM DU DDL A TRAITER
! IN  : LLAB   : FLAG D IMPRESSION DES LABELS
! IN/OUT  : LIGN1  : PREMIERE LIGNE D'IMPRESSION DU RESULTAT
! IN/OUT  : LIGN2  : DEUXIEME LIGNE D'IMPRESSION DU RESULTAT
! OUT : IMPRESSION SUR LISTING
! ----------------------------------------------------------------------
    integer :: vali, neq, i, j, k, iret1, valii, icmp
    integer :: ncmp, vnocmp, jcsd, jcsc, jcsv, jcsl, jcmp, ind
    integer :: nl1, nl11, nl2, nl22
    real(kind=8) :: valr, valrr
    complex(kind=8) :: valc
    character(len=1) :: typrez
    character(len=24) :: valk(3)
    character(len=4) :: type
    character(len=8) :: tych, noddl
    character(len=19) :: cham19, cnsinr, cnsin1
    aster_logical :: skip
    real(kind=8) :: ordgrd
!     ------------------------------------------------------------------
!
    skip = .false.
    if (present(ignore)) then
        skip = ignore
    endif
!
    ordgrd = 1.d0
    if (present(compare)) then
        ordgrd = compare
    endif
!
    call jemarq()
!
    cham19 = chamgd
    typrez = typres(1:1)
!
    call wkvect('&&TRESU_CH.CMP', 'V V I', nbcmp, jcmp)
!
!     -- LE CHAMP EXISTE-T-IL ?
!     =========================
    call dismoi('TYPE_CHAMP', cham19, 'CHAMP', repk=tych, arret='C',&
                ier=iret1)

    ASSERT(nbcmp.eq.1)


    if (tych(1:4) .eq. 'NOEU') then
!   --------------------------------
        cnsinr = '&&TRESU_CH.CNSINR'
        call cnocns(cham19, 'V', cnsinr)
        call jeveuo(cnsinr//'.CNSV', 'L', jcsv)
        call jeveuo(cnsinr//'.CNSC', 'L', jcsc)
        call jeveuo(cnsinr//'.CNSL', 'L', jcsl)
        call jeveuo(cnsinr//'.CNSD', 'L', jcsd)
        ncmp = zi(jcsd-1+2)
        do i = 1, nbcmp
            noddl = nocmp(i)
            do j = 1, ncmp
                if (zk8(jcsc-1+j) .eq. noddl) then
                    zi(jcmp-1+i) = j
                    goto 10
                endif
            end do
            call utmess('F', 'CHAMPS_3', sk=noddl)
 10         continue
        end do
        call jelira(cnsinr//'.CNSV', 'TYPE', cval=type)
        call jelira(cnsinr//'.CNSV', 'LONMAX', neq)
        neq = neq / ncmp
        if (type(1:1) .ne. typrez) then
            write(ific,*) 'NOOK '
            valk(1) = cham19
            valk(2) = type
            valk(3) = typrez
            call utmess('A', 'CALCULEL5_13', nk=3, valk=valk)
            goto 999
        endif

!
    else if (tych(1:2).eq.'EL') then
!   -----------------------------------
        cnsin1 = '&&TRESU_CH.CNSIN1'
        cnsinr = '&&TRESU_CH.CNSINR'
        call celces(cham19, 'V', cnsin1)
!       -- tres important : cesred avec nbcmp permet la division neq=neq/ncmp
        call cesred(cnsin1, 0, [0], 1, nocmp(1),&
                  'V', cnsinr)
        call detrsd('CHAM_ELEM_S', cnsin1)

        call jeveuo(cnsinr//'.CESV', 'L', jcsv)
        call jeveuo(cnsinr//'.CESC', 'L', jcsc)
        call jeveuo(cnsinr//'.CESL', 'L', jcsl)
        call jeveuo(cnsinr//'.CESD', 'L', jcsd)
        ncmp = zi(jcsd-1+2)
        ASSERT(ncmp.eq.1)
        do i = 1, nbcmp
            noddl = nocmp(i)
            do j = 1, ncmp
                if (zk8(jcsc-1+j) .eq. noddl) then
                    zi(jcmp-1+i) = j
                    goto 20
                endif
            end do
            call utmess('F', 'CHAMPS_3', sk=noddl)
 20         continue
        end do
        call jelira(cnsinr//'.CESV', 'TYPE', cval=type)
        call jelira(cnsinr//'.CESV', 'LONMAX', neq)
        neq = neq / ncmp
        if (type(1:1) .ne. typrez) then
            write(ific,*) 'NOOK '
            valk(1) = cham19
            valk(2) = type
            valk(3) = typrez
            call utmess('A', 'CALCULEL5_13', nk=3, valk=valk)
            goto 999
        endif
    else
        write(ific,*) 'NOOK '
        call utmess('A', 'CALCULEL5_14', sk=cham19)
    endif



    nl1 = lxlgut(lign1)
    lign1(1:nl1+16)=lign1(1:nl1-1)//' NOM_CMP'
    lign1(nl1+17:nl1+17)='.'


!    ================================================================================
    if (type .eq. 'I') then
        if (typtes .eq. 'SOMM_ABS') then
            vali = 0
            do i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        vali = vali + abs( zi(jcsv-1+ind) )
                    endif
                end do
            end do
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
!
        else if (typtes .eq. 'SOMM') then
            vali = 0
            do i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        vali = vali + zi(jcsv-1+ind)
                    endif
                end do
            end do
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
!
        else if (typtes .eq. 'MAX') then
            do i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valii = zi(jcsv-1+ind)
                        goto 124
                    endif
                end do
124             continue
                do k = j+1, neq
                    ind = ncmp*(k-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valii = max( valii , zi(jcsv-1+ind) )
                    endif
                end do
                if (i .eq. 1) then
                    vali=valii
                    icmp=1
                else
                    if (valii .gt. vali) then
                        vali=valii
                        icmp=i
                    endif
                endif
            end do
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp-&
            1+icmp))
            lign2(nl2+17:nl2+17)='.'
!
        else if (typtes .eq. 'MIN') then
            do i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valii = zi(jcsv-1+ind)
                        goto 134
                    endif
                end do
134             continue
                do k = j+1, neq
                    ind = ncmp*(k-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valii = min( valii , zi(jcsv-1+ind) )
                        icmp=vnocmp
                    endif
                end do
                if (i .eq. 1) then
                    vali=valii
                    icmp=1
                else
                    if (valii .lt. vali) then
                        vali=valii
                        icmp=i
                    endif
                endif
            end do
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp-&
            1+icmp))
            lign2(nl2+17:nl2+17)='.'
        else
            write(ific,*) 'NOOK '
            call utmess('A', 'CALCULEL5_12')
            goto 999
        endif
!

!    ================================================================================
    else if (type .eq. 'R') then
        if (typtes .eq. 'SOMM_ABS') then
            valr = 0.d0
            do i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valr = valr + abs( zr(jcsv-1+ind) )
                    endif
                end do
            end do
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
        else if (typtes .eq. 'SOMM') then
            valr = 0.d0
            do i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valr = valr + zr(jcsv-1+ind)
                    endif
                end do
            end do
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'

        else if (typtes .eq. 'MAX') then
            valrr=-1.d+300
            do i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do k = 1, neq
                    ind = ncmp*(k-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valrr = max( valrr , zr(jcsv-1+ind) )
                    endif
                end do
                if (i .eq. 1) then
                    valr=valrr
                    icmp=1
                else
                    if (valrr .gt. valr) then
                        valr=valrr
                        icmp=i
                    endif
                endif
            end do
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp-1+icmp))
            lign2(nl2+17:nl2+17)='.'

        else if (typtes .eq. 'MIN') then
            valrr=1.d+300
            do i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do k = 1, neq
                    ind = ncmp*(k-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valrr = min( valrr , zr(jcsv-1+ind) )
                    endif
                end do
                if (i .eq. 1) then
                    valr=valrr
                    icmp=1
                else
                    if (valrr .lt. valr) then
                        valr=valrr
                        icmp=i
                    endif
                endif
            end do
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp-&
            1+icmp))
            lign2(nl2+17:nl2+17)='.'
        else
            write(ific,*) 'NOOK '
            call utmess('A', 'CALCULEL5_12')
            goto 999
        endif
!

!    ================================================================================
    else if (type .eq. 'C') then
        if (typtes .eq. 'SOMM_ABS') then
            valr = 0.d0
            do i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valr = valr + abs( zc(jcsv-1+ind) )
                    endif
                end do
            end do
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
        else if (typtes .eq. 'SOMM') then
            valc = dcmplx(0.d0,0.d0)
            do i = 1, nbcmp
                vnocmp = zi(jcmp+i-1)
                do j = 1, neq
                    ind = ncmp*(j-1)+(vnocmp-1)+1
                    if (zl(jcsl-1+ind)) then
                        valc = valc + zc(jcsv-1+ind)
                    endif
                end do
            end do
            nl2 = lxlgut(lign2)
            lign2(1:nl2+16)=lign2(1:nl2-1)//' '// zk8(jcsc-1+zi(jcmp))
            lign2(nl2+17:nl2+17)='.'
        else
            write(ific,*) 'NOOK '
            call utmess('A', 'CALCULEL5_12')
            goto 999
        endif
    endif



    nl1 = lxlgut(lign1)
    nl11 = lxlgut(lign1(1:nl1-1))
    nl2 = lxlgut(lign2)
    nl22 = lxlgut(lign2(1:nl2-1))

    if (llab) then
        if (nl11 .lt. 80) then
            write (ific,*) lign1(1:nl11)
        else if (nl11.lt.160) then
            write (ific,1160) lign1(1:80),lign1(81:nl11)
        else
            write (ific,1200) lign1(1:80),lign1(81:160),lign1(161:&
            nl11)
        endif
!
        if (nl22 .lt. 80) then
            write (ific,*) lign2(1:nl22)
        else if (nl22.lt.160) then
            write (ific,1160) lign2(1:80),lign2(81:nl22)
        else
            write (ific,1200) lign2(1:80),lign2(81:160),lign2(161:&
            nl22)
        endif
    endif
!
    call tresu_print_all(tbtxt(1), tbtxt(2), llab, typres, nbref,&
                         crit, epsi, ssigne, refr, valr,&
                         refi, vali, refc, valc, ignore=skip,&
                         compare=ordgrd)
!
    if (tych(1:4).eq.'NOEU') then
        call detrsd('CHAM_NO_S', cnsinr)
    else if (tych(1:2).eq.'EL') then
        call detrsd('CHAM_ELEM_S', cnsinr)
    endif

999 continue
    call jedetr('&&TRESU_CH.CMP')
!
    1160 format(1x,a80,a)
    1200 format(1x,2(a80),a)
!
    call jedema()
end subroutine
