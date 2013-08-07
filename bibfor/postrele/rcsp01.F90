subroutine rcsp01(nbm, adrm, ipt, sp3, sp4,&
                  sp5, alphaa, alphab, nbth, iocs,&
                  sp6)
    implicit   none
#include "jeveux.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesg.h"
    integer :: nbm, adrm(*), ipt, nbth, iocs
    real(kind=8) :: sp3, sp4, sp5, alphaa, alphab, sp6
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!     CALCUL DU SP
!
!     ------------------------------------------------------------------
!
    integer :: jchth, iad, icmp, nbcmp, decal, jcesd, jcesv, jcesl, nbinst, i
    integer :: jmoye, jmoy2, jther, vali(2)
    real(kind=8) :: tint, text, ta, tb, tab, dt1, dt2, term1, term2, dt1max
    real(kind=8) :: dt2max, tabmax
    character(len=24) :: chtemp
!
! DEB ------------------------------------------------------------------
!
    sp6 = 0.d0
    if (nbth .eq. 0) goto 9999
!
    call jeveuo('&&RC3600.CHAM_THER', 'L', jchth)
!
    chtemp = zk24(jchth+iocs-1)
!
    call jeveuo(chtemp(1:19)//'.CESD', 'L', jcesd)
    call jeveuo(chtemp(1:19)//'.CESV', 'L', jcesv)
    call jeveuo(chtemp(1:19)//'.CESL', 'L', jcesl)
!
    nbcmp = zi(jcesd-1+2)
    decal = zi(jcesd-1+5+4*(adrm(1)-1)+4)
!
    icmp = 1
    iad = decal + (ipt-1)*nbcmp + icmp
    if (.not.zl(jcesl-1+iad)) then
        vali(1) = iocs
        vali(2) = adrm(1)
        call u2mesg('F', 'POSTRCCM_15', 1, 'RESU_THER', 2,&
                    vali, 0, 0.d0)
    endif
    call jeveuo(zk24(jcesv-1+iad), 'L', jther)
    call jelira(zk24(jcesv-1+iad), 'LONMAX', nbinst)
    nbinst = nbinst / 2
!
    icmp = 2
    iad = decal + (ipt-1)*nbcmp + icmp
    if (.not.zl(jcesl-1+iad)) then
        vali(1) = iocs
        vali(2) = adrm(1)
        call u2mesg('F', 'POSTRCCM_15', 1, 'RESU_THER_MOYE', 2,&
                    vali, 0, 0.d0)
    endif
    call jeveuo(zk24(jcesv-1+iad), 'L', jmoye)
!
    if (nbm .gt. 1) then
        decal = zi(jcesd-1+5+4*(adrm(2)-1)+4)
        icmp = 2
        iad = decal + (ipt-1)*nbcmp + icmp
        if (.not.zl(jcesl-1+iad)) then
            vali(1) = iocs
            vali(2) = adrm(2)
            call u2mesg('F', 'POSTRCCM_15', 1, 'RESU_THER_MOYE', 2,&
                        vali, 0, 0.d0)
        endif
        call jeveuo(zk24(jcesv-1+iad), 'L', jmoy2)
    endif
!
! --- ON BOUCLE SUR LES INSTANTS :
!
    dt1max = 0.d0
    dt2max = 0.d0
    tabmax = 0.d0
!
    do 10 i = 1, nbinst
!
! ------ TEMP_INT, TEMP_EXT
!
        tint = zr(jther-1+2*(i-1)+1)
        text = zr(jther-1+2*(i-1)+2)
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
! ------ DT2: PARTIE NON LINEAIRE DE LA DISTRIBUTION DANS L'EPAISSEUR
!             DE PAROI DE L'AMPLITUDE DE VARIATION DE LA TEMPERATURE
!             ENTRE LES 2 ETATS STABILISES
!
        term1 = abs(text-ta) - abs(0.5d0*dt1)
        term2 = abs(tint-ta) - abs(0.5d0*dt1)
        dt2 = max( term1, term2, 0.d0 )
!
        dt1max = max ( dt1max, abs( dt1 ) )
!
        dt2max = max ( dt2max, abs( dt2 ) )
!
        if (nbm .gt. 1) then
            tb = zr(jmoy2-1+2*(i-1)+1)
            tab = ( alphaa * ta ) - ( alphab * tb )
            tabmax = max ( tabmax, abs( tab ) )
        endif
!
10  end do
!
    sp6 = sp6 + ( sp3 * dt1max )
    sp6 = sp6 + ( sp5 * dt2max )
    if (nbm .gt. 1) sp6 = sp6 + ( sp4 * tabmax )
!
9999  continue
!
end subroutine
