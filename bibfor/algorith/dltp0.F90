subroutine dltp0(t0, nume)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/rs_getlast.h"
    real(kind=8) :: t0
    integer :: nume
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
!     -----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, jadr,  jordr, n1, nbordr(1), tnume(1)
    integer :: nbtrou, nc, ndy, nni, np, nt
    real(kind=8) :: prec, r8b, temps
    real(kind=8), pointer :: bint(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    call getres(nomres, typres, nomcmd)
!
!     --- EST-ON EN REPRISE ? ---
!
!     INITIALISATION DE T0 PAR DEFAUT
!
    call getvid('ETAT_INIT', 'RESULTAT', iocc=1, scal=dyna, nbret=ndy)
    if (ndy .ne. 0) then
        call getvis('ETAT_INIT', 'NUME_ORDRE', iocc=1, scal=nume, nbret=nni)
        if (nni .eq. 0) then
            call getvr8('ETAT_INIT', 'INST_INIT', iocc=1, scal=temps, nbret=nt)
            if (nt .eq. 0) then    
                call rs_getlast(dyna, nume)     
            else
                call getvr8('ETAT_INIT', 'PRECISION', iocc=1, scal=prec, nbret=np)
                call getvtx('ETAT_INIT', 'CRITERE', iocc=1, scal=crit, nbret=nc)
                call rsorac(dyna, 'INST', ibid, temps, k8b,&
                            c16b, prec, crit, tnume, 1,&
                            nbtrou)
                nume=tnume(1)            
                if (nbtrou .lt. 0) then
                    valk = dyna
                    valr = temps
                    vali = -nbtrou
                    call utmess('F', 'ALGORITH12_83', sk=valk, si=vali, sr=valr)
                else if (nbtrou.eq.0) then
                    valk = dyna
                    valr = temps
                    call utmess('F', 'ALGORITH12_84', sk=valk, sr=valr)
                endif
            endif
        else
!           --- VERIFICATION QUE NUME EXISTE ---
            call rsorac(dyna, 'LONUTI', 0, r8b, k8b,&
                        c16b, r8b, k8b, nbordr, 1,&
                        ibid)
            call wkvect('&&COMDLT.NUME_ORDRE', 'V V I', nbordr(1), jordr)
            call rsorac(dyna, 'TOUT_ORDRE', 0, r8b, k8b,&
                        c16b, r8b, k8b, zi(jordr), nbordr(1),&
                        ibid)
            do 10 i = 1, nbordr(1)
                if (zi(jordr+i-1) .eq. nume) goto 12
10          continue
            call utmess('F', 'ALGORITH3_36', sk=dyna)
12          continue
        endif
!
!        --- RECUPERATION DE L'INSTANT ---
        call rsadpa(dyna, 'L', 1, 'INST', nume,&
                    1, sjv=jadr, styp=ctype)
        t0 = zr(jadr)
    else
!
!     --- DEFINITION DES INSTANTS DE CALCUL A PARTIR DE "LIST_INST" ---
!
        call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=li, nbret=n1)
        if (n1 .ne. 0) then
            call jeveuo(li//'           .BINT', 'L', vr=bint)
            t0 = bint(1)
        else
!
!
!     --- DEFINITION DE L'INSTANT INITIAL AVEC "INST_INIT" ---
!
            t0 = 0.d0
            call getvr8('INCREMENT', 'INST_INIT', iocc=1, scal=t0, nbret=np)
        endif
    endif
!
    call jedema()
end subroutine
