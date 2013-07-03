subroutine speph2(movrep, napexc, nbmode, nbpf, intmod,&
                  table, specmr, specmi)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: napexc, nbmode, nbpf
    real(kind=8) :: specmr(nbpf, *), specmi(nbpf, *)
    logical :: intmod
    character(len=8) :: table
    character(len=16) :: movrep
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
    integer :: ival(2), ideb1, ifin1, i, j, imi, imj, ideb, isj, ifon, if1
    integer :: mxval, lnumi, lnumj, i1
!
    character(len=8) :: k8b
    character(len=24) :: chnumi, chnumj, chvale
!
!     ------------------------------------------------------------------
!
    if (movrep .eq. 'ABSOLU') then
        ideb1 = 1
        ifin1 = napexc+nbmode
    else if (movrep .eq. 'RELATIF') then
        ideb1 = napexc+1
        ifin1 = napexc+nbmode
    else if (movrep .eq. 'DIFFERENTIEL') then
        ideb1 = 1
        ifin1 = napexc
    endif
!
    chnumi = table//'.NUMI'
    chnumj = table//'.NUMJ'
    chvale = table//'.VALE'
    call jeveuo(chnumi, 'L', lnumi)
    call jeveuo(chnumj, 'L', lnumj)
    call jelira(chnumi, 'LONMAX', mxval, k8b)
!
    j = 0
    do 30 imj = ideb1, ifin1
        j = j + 1
!
        ival(2) = imj
!
        ideb = imj
        if (intmod) ideb = ideb1
!
        i = 0
        do 40 imi = ideb, imj
            i = i + 1
!
            ival(1) = imi
!
            do 200 i1 = 1, mxval
                if ((zi(lnumi-1+i1) .eq. ival(1)) .and. (zi(lnumj-1+ i1) .eq. ival(2))) then
                    call jeveuo(jexnum(chvale, i1), 'L', ifon)
                endif
200          continue
!
            isj = j * ( j - 1 ) / 2 + i
!
            do 50 if1 = 1, nbpf
                if (ival(1) .eq. ival(2)) then
                    specmr(if1,isj) = zr(ifon-1 + if1)
                    specmi(if1,isj) = 0.d0
                else
                    specmr(if1,isj) = zr(ifon+ (if1-1)*2)
                    specmi(if1,isj) = zr(ifon+ (if1-1)*2+1)
                endif
50          continue
40      continue
!
30  end do
end subroutine
