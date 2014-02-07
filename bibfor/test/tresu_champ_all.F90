subroutine tresu_champ_all(chamgd, typtes, typres, nbref, tbtxt,&
                           refi, refr, refc, epsi, crit,&
                           ific, llab, ssigne, ignore, compare)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tresu_print_all.h"
#include "asterfort/utmess.h"
    character(len=*), intent(in) :: chamgd
    character(len=8), intent(in) :: typtes
    character(len=*), intent(in) :: typres
    integer, intent(in) :: nbref
    character(len=16), intent(in) :: tbtxt(2)
    integer, intent(in) :: refi(nbref)
    real(kind=8), intent(in) :: refr(nbref)
    complex(kind=8), intent(in) :: refc(nbref)
    real(kind=8), intent(in) :: epsi
    character(len=*), intent(in) :: crit
    integer, intent(in) :: ific
    logical, intent(in) :: llab
    character(len=*), intent(in) :: ssigne
    logical, intent(in), optional :: ignore
    real(kind=8), intent(in), optional :: compare
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
! IN  : CHAMGD : NOM DU CHAM_GD
! IN  : TYPTES : TYPE DE TEST A EFFECTUER SUR LE CHAMP
! IN  : NBREF  : NOMBRE DE VALEURS DE REFERENCE
! IN  : TBTXT  : (1)=REFERENCE, (2)=LEGENDE
! IN  : REFI   : VALEUR REELLE ENTIERE ATTENDUE
! IN  : REFR   : VALEUR REELLE ATTENDUE
! IN  : REFC   : VALEUR COMPLEXE ATTENDUE
! IN  : CRIT   : 'RELATIF' OU 'ABSOLU'(PRECISION RELATIVE OU ABSOLUE).
! IN  : EPSI   : PRECISION ESPEREE
! IN  : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
! IN  : LLAB   : FLAG D IMPRESSION DES LABELS
! OUT : IMPRESSION SUR LISTING
! ----------------------------------------------------------------------
    integer :: vali, jvale, neq, i, iret1, iret2
    real(kind=8) :: valr, ordgrd
    complex(kind=8) :: valc
    character(len=1) :: typrez
    character(len=24) :: valk(3)
    character(len=4) :: type
    character(len=5) :: sufv
    character(len=19) :: cham19
    logical :: skip
!     ------------------------------------------------------------------
    if (present(ignore)) then
        skip = ignore
    else
        skip = .false.
    endif
    if (present(compare)) then
        ordgrd = compare
    else
        ordgrd = 1.d0
    endif
!
    call jemarq()
!
    cham19 = chamgd
    typrez = typres(1:1)
!
!     -- LE CHAMP EXISTE-T-IL ?
!     =========================
    call jeexin(cham19//'.VALE', iret1)
    if (iret1 .gt. 0) then
        sufv='.VALE'
    else
        call jeexin(cham19//'.CELV', iret2)
        if (iret2 .gt. 0) then
            sufv='.CELV'
        else
            write(ific,*) 'NOOK '
            goto 9999
        endif
    endif
!
!
    call jelira(cham19//sufv, 'TYPE', cval=type)
    if (type(1:1) .ne. typrez) then
        write(ific,*) 'NOOK '
        valk(1) = cham19
        valk(2) = type
        valk(3) = typrez
        call utmess('A', 'CALCULEL5_13', nk=3, valk=valk)
        goto 9999
    endif
!
    call jelira(cham19//sufv, 'LONMAX', neq)
    call jeveuo(cham19//sufv, 'L', jvale)
!
!
    if (type .eq. 'I') then
        if (typtes .eq. 'SOMM_ABS') then
            vali = 0
            do 100 i = 1, neq
                vali = vali + abs( zi(jvale+i-1) )
100          continue
        else if (typtes .eq. 'SOMM') then
            vali = 0
            do 102 i = 1, neq
                vali = vali + zi(jvale+i-1)
102          continue
        else if (typtes .eq. 'MAX') then
            vali = zi(jvale-1+1)
            do 104 i = 2, neq
                vali = max( vali , zi(jvale+i-1) )
104          continue
        else if (typtes .eq. 'MIN') then
            vali = zi(jvale-1+1)
            do 106 i = 2, neq
                vali = min( vali , zi(jvale+i-1) )
106          continue
        else
            write(ific,*) 'NOOK '
            call utmess('A', 'CALCULEL5_12')
            goto 9999
        endif
!
!
    else if (type .eq. 'R') then
        if (typtes .eq. 'SOMM_ABS') then
            valr = 0.d0
            do 200 i = 1, neq
                valr = valr + abs( zr(jvale+i-1) )
200          continue
        else if (typtes .eq. 'SOMM') then
            valr = 0.d0
            do 202 i = 1, neq
                valr = valr + zr(jvale+i-1)
202          continue
        else if (typtes .eq. 'MAX') then
            valr = zr(jvale)
            do 204 i = 2, neq
                valr = max( valr , zr(jvale+i-1) )
204          continue
        else if (typtes .eq. 'MIN') then
            valr = zr(jvale)
            do 206 i = 2, neq
                valr = min( valr , zr(jvale+i-1) )
206          continue
        else
            write(ific,*) 'NOOK '
            call utmess('A', 'CALCULEL5_12')
            goto 9999
        endif
!
!
    else if (type .eq. 'C') then
        if (typtes .eq. 'SOMM_ABS') then
            valc = dcmplx(0.d0,0.d0)
            do 300 i = 1, neq
                valc = valc + abs( zc(jvale+i-1) )
300          continue
        else if (typtes .eq. 'SOMM') then
            valc = dcmplx(0.d0,0.d0)
            do 302 i = 1, neq
                valc = valc + zc(jvale+i-1)
302          continue
        else
            write(ific,*) 'NOOK '
            call utmess('A', 'CALCULEL5_12')
            goto 9999
        endif
    endif
!
    call tresu_print_all(tbtxt(1), tbtxt(2), llab, typres, nbref, &
                crit, epsi, ssigne, refr, valr, &
                refi, vali, refc, valc, &
                ignore=skip, compare=ordgrd)
!
9999  continue
!
    call jedema()
end subroutine
