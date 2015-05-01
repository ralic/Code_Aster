subroutine mltalc(local, global, adress, sn, lgsn,&
                  place, sni, supnd, nbass)
! person_in_charge: olivier.boiteau at edf.fr
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
! aslint: disable=W1304
    implicit none
    integer(kind=4) :: local(*), global(*)
    integer :: sn, lgsn(*), place(*), adress(*)
    integer :: sni, supnd(*), nbass
    integer :: k, longsn, is
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    longsn = lgsn(sn)
    do 110 k = adress(sn), adress(sn) + longsn - 1
        local(k) = 0
110  end do
    do 120 k = adress(sn) + longsn, adress(sn+1) - 1
        local(k) = int(place(global(k)), 4)
120  end do
    nbass = 0
    is = supnd(sni+1)
    k = adress(sn) + longsn
!      DO WHILE (K.LT.ADRESS(SN+1).AND.GLOBAL(K).LT.IS)
130  continue
    if (k .lt. adress(sn+1) .and. global(k) .lt. is) then
        nbass = nbass + 1
        k = k + 1
        goto 130
! FIN DO WHILE
    endif
end subroutine
