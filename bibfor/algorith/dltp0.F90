subroutine dltp0(t0, nume)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsorac.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    real(kind=8) :: t0
    integer :: nume
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
!     ------------------------------------------------------------------
! OUT : T0   : INSTANT INITIAL
! OUT : NUME : NUMERO D'ORDRE DE REPRISE
!     ------------------------------------------------------------------
    integer :: vali
    real(kind=8) :: valr
    character(len=8) :: k8b, nomres, dyna, li, crit, ctype
    character(len=16) :: typres, nomcmd
    character(len=24) :: valk
    complex(kind=8) :: c16b
    integer :: iarg
!     -----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, jadr, jbint, jordr, n1, nbordr
    integer :: nbtrou, nc, ndy, nni, np, nt
    real(kind=8) :: prec, r8b, temps
!-----------------------------------------------------------------------
    call jemarq()
    call getres(nomres, typres, nomcmd)
!
!     --- EST-ON EN REPRISE ? ---
!
!     INITIALISATION DE T0 PAR DEFAUT
!
    call getvid('ETAT_INIT', 'RESULTAT', 1, iarg, 1,&
                dyna, ndy)
    if (ndy .ne. 0) then
        call getvis('ETAT_INIT', 'NUME_ORDRE', 1, iarg, 1,&
                    nume, nni)
        if (nni .eq. 0) then
            call getvr8('ETAT_INIT', 'INST_INIT', 1, iarg, 1,&
                        temps, nt)
            if (nt .eq. 0) then
                call rsorac(dyna, 'DERNIER', ibid, temps, k8b,&
                            c16b, prec, crit, nume, 1,&
                            nbtrou)
                if (nbtrou .ne. 1) then
                    call u2mess('F', 'ALGORITH3_24')
                endif
            else
                call getvr8('ETAT_INIT', 'PRECISION', 1, iarg, 1,&
                            prec, np)
                call getvtx('ETAT_INIT', 'CRITERE', 1, iarg, 1,&
                            crit, nc)
                call rsorac(dyna, 'INST', ibid, temps, k8b,&
                            c16b, prec, crit, nume, 1,&
                            nbtrou)
                if (nbtrou .lt. 0) then
                    valk = dyna
                    valr = temps
                    vali = -nbtrou
                    call u2mesg('F', 'ALGORITH12_83', 1, valk, 1,&
                                vali, 1, valr)
                else if (nbtrou.eq.0) then
                    valk = dyna
                    valr = temps
                    call u2mesg('F', 'ALGORITH12_84', 1, valk, 0,&
                                0, 1, valr)
                endif
            endif
        else
!           --- VERIFICATION QUE NUME EXISTE ---
            call rsorac(dyna, 'LONUTI', ibid, r8b, k8b,&
                        c16b, r8b, k8b, nbordr, 1,&
                        ibid)
            call wkvect('&&OP0048.NUME_ORDRE', 'V V I', nbordr, jordr)
            call rsorac(dyna, 'TOUT_ORDRE', ibid, r8b, k8b,&
                        c16b, r8b, k8b, zi(jordr), nbordr,&
                        ibid)
            do 10 i = 1, nbordr
                if (zi(jordr+i-1) .eq. nume) goto 12
10          continue
            call u2mesk('F', 'ALGORITH3_36', 1, dyna)
12          continue
        endif
!
!        --- RECUPERATION DE L'INSTANT ---
        call rsadpa(dyna, 'L', 1, 'INST', nume,&
                    1, jadr, ctype)
        t0 = zr(jadr)
    else
!
!     --- DEFINITION DES INSTANTS DE CALCUL A PARTIR DE "LIST_INST" ---
!
        call getvid('INCREMENT', 'LIST_INST', 1, iarg, 1,&
                    li, n1)
        if (n1 .ne. 0) then
            call jeveuo(li//'           .BINT', 'L', jbint)
            t0 = zr (jbint)
        else
!
!
!     --- DEFINITION DE L'INSTANT INITIAL AVEC "INST_INIT" ---
!
            t0 = 0.d0
            call getvr8('INCREMENT', 'INST_INIT', 1, iarg, 1,&
                        t0, np)
            if (np .eq. 0) call u2mess('I', 'ALGORITH5_62')
        endif
    endif
!
    call jedema()
end subroutine
