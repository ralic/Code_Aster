subroutine op0178()
    implicit none
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
!     COMMANDE:  ENGENDRE_TEST
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/engtce.h"
#include "asterfort/engtcn.h"
#include "asterfort/engtrs.h"
#include "asterfort/engttb.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelstc.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/tstobj.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/wkvect.h"
    integer :: resume, sommi, lonuti, lonmax, ni
    real(kind=8) :: sommr
    character(len=3) :: type
    character(len=8) :: kbid, form0, typtes
    character(len=10) :: formr, preci
    character(len=19) :: nomsd
    character(len=24) :: nomfi, obj
    character(len=140) :: form1
!
!
!-----------------------------------------------------------------------
    integer :: i, ialico, ialiob, ico, ific, iret
    integer :: n1, nbobj, nbval, nco
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
!
!     -- RECUPERATION DES DONNEES :
!     ----------------------------
!
    ific = 0
    nomfi = ' '
    call getvis(' ', 'UNITE', scal=ific, nbret=n1)
    if (.not. ulexis( ific )) then
        call ulopen(ific, ' ', nomfi, 'NEW', 'O')
    endif
!
    form0 = 'ASTER'
    call getvtx(' ', 'FORMAT', scal=form0)
!
    call getvtx(' ', 'FORMAT_R', scal=formr)
    call getvtx(' ', 'PREC_R', scal=preci)
    call getvtx(' ', 'TYPE_TEST', scal=typtes)
!
! ---- FORMAT TEST_RESU / STANDARD
!
    if (form0 .eq. 'ASTER') then
        call getvid(' ', 'CO', nbval=0, nbret=n1)
        nco =-n1
        call wkvect('&&OP0178.LCO', 'V V K8', nco, ialico)
        call getvid(' ', 'CO', nbval=nco, vect=zk8(ialico))
!
        do ico = 1, nco
            nomsd = zk8(ialico+ico-1)(1:8)//'           '
!
            call exisd('RESULTAT', nomsd, iret)
            if (iret .eq. 1) then
                call engtrs(ific, nomsd, typtes, preci, formr)
                goto 100
            endif
!
            call exisd('TABLE', nomsd, iret)
            if (iret .eq. 1) then
                call engttb(ific, nomsd, typtes, preci, formr)
                goto 100
            endif
!
            call exisd('CHAM_ELEM', nomsd, iret)
            if (iret .eq. 1) then
                call engtce(ific, nomsd, typtes, preci, formr)
                goto 100
            endif
!
            call exisd('CHAM_NO', nomsd, iret)
            if (iret .eq. 1) then
                call engtcn(ific, nomsd, typtes, preci, formr)
                goto 100
            endif
100         continue
        end do
        call jedetr('&&OP0178.LCO')
!
! ---- FORMAT TEST_RESU / OBJET
!
    else
!
        form1 = "('_F(NOM=''', a24, ''', VALE_CALC=', " //formr//&
            ", ',TOLE_MACHINE="//preci(1:lxlgut(preci)) //"),')"
902     format('_F(NOM=''',a24,''',VALE_CALC_I=',i15,',TOLE_MACHINE=0.,),')
!
!     -- CAS : TOUT:'OUI'
!    -----------------------------------------
        call getvtx(' ', 'TOUT', scal=kbid, nbret=n1)
        if (n1 .eq. 1) then
            call jelstc('G', ' ', 0, 0, kbid,&
                        nbval)
            nbobj = -nbval
            call wkvect('&&OP0178.LISTE', 'V V K24', nbobj, ialiob)
            call jelstc('G', ' ', 0, nbobj, zk24(ialiob),&
                        nbval)
!
            do 10 i = 1, nbobj
                obj = zk24(ialiob-1+i)
                if (obj(1:1) .eq. '&') goto 10
                call tstobj(obj, 'NON', resume, sommi, sommr,&
                            lonuti, lonmax, type, iret, ni)
                if (iret .eq. 0) then
!             -- TEST_RESU/VALE_CALC_I (OU VALE_CALC) :
                    if ((type.eq.'R') .or. (type.eq.'C')) then
                        write (ific,form1) obj,sommr
                    else if (type.eq.'I') then
                        write (ific,902) obj,sommi
                    endif
                endif
10          continue
        endif
!
!     -- CAS : CO: L_CO
!    -----------------------------------------
        call getvid(' ', 'CO', nbval=0, nbret=n1)
        if (n1 .lt. 0) then
            nco = -n1
            call wkvect('&&OP0178.LCO', 'V V K8', nco, ialico)
            call getvid(' ', 'CO', nbval=nco, vect=zk8(ialico))
!
            do 30 ico = 1, nco
                call jelstc('G', zk8(ialico-1+ico), 1, 0, kbid,&
                            nbval)
                if (nbval .eq. 0) goto 30
                nbobj = -nbval
                call wkvect('&&OP0178.LISTE', 'V V K24', nbobj, ialiob)
                call jelstc('G', zk8(ialico-1+ico), 1, nbobj, zk24(ialiob),&
                            nbval)
!
                do 20 i = 1, nbobj
                    obj = zk24(ialiob-1+i)
                    call tstobj(obj, 'NON', resume, sommi, sommr,&
                                lonuti, lonmax, type, iret, ni)
                    if (iret .eq. 0) then
!               -- TEST_RESU/VALE_CALC_I (OU VALE_CALC) :
                        if ((type.eq.'R') .or. (type.eq.'C')) then
                            write (ific,form1) obj,sommr
                        else if (type.eq.'I') then
                            write (ific,902) obj,sommi
                        endif
                    endif
20              continue
                call jedetr('&&OP0178.LISTE')
30          continue
        endif
    endif
!
!
    call jedema()
!
end subroutine
