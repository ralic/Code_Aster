subroutine mechpo(souche, charge, modele, chdep2, chdynr,&
                  suropt, lpain, lchin, nbopt, typcoe,&
                  alpha, calpha)
    implicit none
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/fozero.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecact.h"
    character(len=*) :: souche, charge, modele, chdep2, chdynr, suropt, lpain(*)
    character(len=*) :: lchin(*), typcoe
    integer :: nbopt
    real(kind=8) :: alpha
    complex(kind=8) :: calpha
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CREE UNE CARTE SPECFIQUE POUTRE A LA POUX
!     ------------------------------------------------------------------
! IN  : MODELE : NOM DU MODELE
! IN  : TYPCOE : TYPE DU COEFFICIENT MULTIPLICATIF DE LA CHARGE REPARTIE
!                SI TYPE = R ON CREE UNE CARTE AVEC LE COEFFICIENT REEL
!                   ALPHA
!                SI TYPE = C ALORS ON CREE UNE CARTE DE COEFFICIENT
!                    COMPLEXE CALPHA
!     ------------------------------------------------------------------
!
!
    real(kind=8) :: tps(11)
    character(len=5) :: ch5
    character(len=8) :: k8b, ncmppe(4), ncmpfo(11), tpf(11)
    character(len=19) :: ch19
    character(len=24) :: ligrmo, chdepl
    complex(kind=8) :: tpc(11)
!-----------------------------------------------------------------------
    integer :: i, iret
!-----------------------------------------------------------------------
    data         ncmppe/ 'G' , 'AG' , 'BG' , 'CG' /
    data         ncmpfo/ 'FX' , 'FY' , 'FZ' , 'MX' , 'MY' , 'MZ' ,&
     &                     'BX' , 'REP' , 'ALPHA' , 'BETA' , 'GAMMA' /
!    -------------------------------------------------------------------
    call jemarq()
    do 10 i = 1, 11
        tps(i) = 0.d0
        tpf(i) = '&FOZERO'
        tpc(i) = ( 0.d0 , 0.d0 )
 10 end do
    ligrmo = modele(1:8)//'.MODELE'
    chdepl = chdep2
    ch5 = '.    '
!
    nbopt = 0
    if (typcoe .eq. 'R') then
        nbopt = nbopt+1
        lpain(nbopt) = 'PCOEFFR'
        lchin(nbopt) = souche(1:8)//ch5//'.COEFF'
        call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'IMPE_R',&
                    ncmp=1, nomcmp='IMPE', sr=alpha)
    else if (typcoe.eq.'C') then
        nbopt = nbopt+1
        lpain(nbopt) = 'PCOEFFC'
        lchin(nbopt) = souche(1:8)//ch5//'.COEFF'
        call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'IMPE_C',&
                    ncmp=1, nomcmp='IMPE', sc=calpha)
    endif
!
    nbopt = nbopt+1
    lpain(nbopt) = 'PPESANR'
    lchin(nbopt) = charge(1:8)//'.CHME.PESAN.DESC'
    call jeexin(lchin(nbopt), iret)
    if (iret .eq. 0) then
        call codent(nbopt, 'D0', ch5(2:5))
        lchin(nbopt) = souche(1:8)//ch5//'.PESAN.DESC'
        call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'PESA_R  ',&
                    ncmp=4, lnomcmp=ncmppe, vr=tps)
    endif
!
    nbopt = nbopt+1
    lchin(nbopt) = charge(1:8)//'.CHME.F1D1D.DESC'
    call jeexin(lchin(nbopt), iret)
    if (iret .eq. 0) then
        lpain(nbopt) = 'PFF1D1D'
        call codent(nbopt, 'D0', ch5(2:5))
        lchin(nbopt) = souche(1:8)//ch5//'.P1D1D.DESC'
        call fozero(tpf(1))
        call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'FORC_F  ',&
                    ncmp=11, lnomcmp=ncmpfo, vk=tpf)
!
        nbopt = nbopt+1
        lpain(nbopt) = 'PFR1D1D'
        call codent(nbopt, 'D0', ch5(2:5))
        lchin(nbopt) = souche(1:8)//ch5//'.P1D1D.DESC'
        call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'FORC_R  ',&
                    ncmp=11, lnomcmp=ncmpfo, vr=tps)
!
        nbopt = nbopt+1
        lpain(nbopt) = 'PFC1D1D'
        call codent(nbopt, 'D0', ch5(2:5))
        lchin(nbopt) = souche(1:8)//ch5//'.P1D1D.DESC'
        call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'FORC_C  ',&
                    ncmp=11, lnomcmp=ncmpfo, vc=tpc)
!
    else
        call dismoi('TYPE_CHARGE', charge, 'CHARGE', repk=k8b)
        if (k8b(5:7) .eq. '_FO') then
            lpain(nbopt) = 'PFF1D1D'
!
            nbopt = nbopt+1
            lpain(nbopt) = 'PFR1D1D'
            call codent(nbopt, 'D0', ch5(2:5))
            lchin(nbopt) = souche(1:8)//ch5//'.P1D1D.DESC'
            call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'FORC_R  ',&
                        ncmp=11, lnomcmp=ncmpfo, vr=tps)
!
            nbopt = nbopt+1
            lpain(nbopt) = 'PFC1D1D'
            call codent(nbopt, 'D0', ch5(2:5))
            lchin(nbopt) = souche(1:8)//ch5//'.P1D1D.DESC'
            call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'FORC_C  ',&
                        ncmp=11, lnomcmp=ncmpfo, vc=tpc)
        else if (k8b(5:6) .eq. '_RI') then
            lpain(nbopt) = 'PFC1D1D'
!
            nbopt = nbopt+1
            lpain(nbopt) = 'PFR1D1D'
            call codent(nbopt, 'D0', ch5(2:5))
            lchin(nbopt) = souche(1:8)//ch5//'.P1D1D.DESC'
            call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'FORC_R  ',&
                        ncmp=11, lnomcmp=ncmpfo, vr=tps)
!
            nbopt = nbopt+1
            lpain(nbopt) = 'PFF1D1D'
            call codent(nbopt, 'D0', ch5(2:5))
            lchin(nbopt) = souche(1:8)//ch5//'.P1D1D.DESC'
            call fozero(tpf(1))
            call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'FORC_F  ',&
                        ncmp=11, lnomcmp=ncmpfo, vk=tpf)
        else
            lpain(nbopt) = 'PFR1D1D'
!
            nbopt = nbopt+1
            lpain(nbopt) = 'PFF1D1D'
            call codent(nbopt, 'D0', ch5(2:5))
            lchin(nbopt) = souche(1:8)//ch5//'.P1D1D.DESC'
            call fozero(tpf(1))
            call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'FORC_F  ',&
                        ncmp=11, lnomcmp=ncmpfo, vk=tpf)
!
            nbopt = nbopt+1
            lpain(nbopt) = 'PFC1D1D'
            call codent(nbopt, 'D0', ch5(2:5))
            lchin(nbopt) = souche(1:8)//ch5//'.P1D1D.DESC'
            call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'FORC_C  ',&
                        ncmp=11, lnomcmp=ncmpfo, vc=tpc)
        endif
    endif
!
    nbopt = nbopt+1
    lpain(nbopt) = 'PCHDYNR'
    ch19 = chdynr
    lchin(nbopt) = ch19//'.VALE'
    call jeexin(lchin(nbopt), iret)
    if (iret .eq. 0) then
        call codent(nbopt, 'D0', ch5(2:5))
        lchin(nbopt) = souche(1:8)//ch5//'.PCHDY'
!
        call copisd('CHAMP_GD', 'V', chdepl, lchin(nbopt))
    endif
!
    nbopt = nbopt+1
    lpain(nbopt) = 'PSUROPT'
    call codent(nbopt, 'D0', ch5(2:5))
    lchin(nbopt) = souche(1:8)//ch5//'.SUR_OPTION'
    call mecact('V', lchin(nbopt), 'MODELE', ligrmo, 'NEUT_K24',&
                ncmp=1, nomcmp='Z1', sk=suropt)
!
    call jedema()
end subroutine
