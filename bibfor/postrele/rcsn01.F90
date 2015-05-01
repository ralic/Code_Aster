subroutine rcsn01(nbm, adrm, ipt, sn3, sn4,&
                  alphaa, alphab, nbth, iocs, sn6)
    implicit none
#include "jeveux.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: nbm, adrm(*), ipt, nbth, iocs
    real(kind=8) :: sn3, sn4, alphaa, alphab, sn6
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!     CALCUL DU SN
!
!     ------------------------------------------------------------------
    integer :: vali(3)
!
    integer ::  iad, icmp, nbcmp, decal,   jcesl, nbinst, i
    integer :: jmoye, jmoy2
    real(kind=8) :: ta, tb, tab, dt1, dt1max, tabmax
    character(len=24) :: chtemp
    integer, pointer :: cesd(:) => null()
    character(len=24), pointer :: cham_ther(:) => null()
    character(len=24), pointer :: cesv(:) => null()
!
! DEB ------------------------------------------------------------------
!
    sn6 = 0.d0
    if (nbth .eq. 0) goto 9999
!
    call jeveuo('&&RC3600.CHAM_THER', 'L', vk24=cham_ther)
!
    chtemp = cham_ther(iocs)
!
    call jeveuo(chtemp(1:19)//'.CESD', 'L', vi=cesd)
    call jeveuo(chtemp(1:19)//'.CESV', 'L', vk24=cesv)
    call jeveuo(chtemp(1:19)//'.CESL', 'L', jcesl)
!
    nbcmp = cesd(2)
    decal = cesd(5+4*(adrm(1)-1)+4)
    icmp = 2
    iad = decal + (ipt-1)*nbcmp + icmp
    if (.not.zl(jcesl-1+iad)) then
        vali(1) = iocs
        vali(2) = adrm(1)
        call utmess('F', 'POSTRCCM_15', sk='RESU_THER', ni=2, vali=vali)
    endif
    call jeveuo(cesv(iad), 'L', jmoye)
    call jelira(cesv(iad), 'LONMAX', nbinst)
    nbinst = nbinst / 2
!
    if (nbm .gt. 1) then
        decal = cesd(5+4*(adrm(2)-1)+4)
        icmp = 2
        iad = decal + (ipt-1)*nbcmp + icmp
        if (.not.zl(jcesl-1+iad)) then
            vali(1) = iocs
            vali(2) = adrm(2)
            call utmess('F', 'POSTRCCM_15', sk='RESU_THER_MOYE', ni=2, vali=vali)
        endif
        call jeveuo(cesv(iad), 'L', jmoy2)
    endif
!
! --- ON BOUCLE SUR LES INSTANTS :
!
    dt1max = 0.d0
    tabmax = 0.d0
!
    do 10 i = 1, nbinst
!
! ------ DT1: AMPLITUDE DE LA VARIATION ENTRE LES 2 ETATS STABILISES
!             DE LA DIFFERENCE DE TEMPERATURE ENTRE LES PAROIS
!             INTERNE ET EXTERNE
!
        dt1 = zr(jmoye-1+2*(i-1)+2)
!
! ------ TA : AMPLITUDE DE VARIATION ENTRE LES 2 ETATS STABILISES
!             DES TEMPERATURES MOYENNES A GAUCHE D'UNE DISCONTINUITE
!
        ta = zr(jmoye-1+2*(i-1)+1)
!
! ------ TB : AMPLITUDE DE VARIATION ENTRE LES 2 ETATS STABILISES
!             DES TEMPERATURES MOYENNES A DROITE D'UNE DISCONTINUITE
!
        dt1max = max ( dt1max, abs( dt1 ) )
!
        if (nbm .gt. 1) then
            tb = zr(jmoy2-1+2*(i-1)+1)
            tab = ( alphaa * ta ) - ( alphab * tb )
            tabmax = max ( tabmax, abs( tab ) )
        endif
!
10  end do
!
    sn6 = sn6 + ( sn3 * dt1max )
    if (nbm .gt. 1) sn6 = sn6 + ( sn4 * tabmax )
!
9999  continue
!
end subroutine
