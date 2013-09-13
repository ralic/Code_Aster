subroutine acevba(nbocc, nlm, nlg, ier)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8maem.h"
#include "asterfort/acedat.h"
#include "asterfort/codent.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbocc, nlm, nlg, ier
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
!     AFFE_CARA_ELEM
!     VERIFICATION DES MOTS CLES POUR L'ELEMENT BARRE
! ----------------------------------------------------------------------
! IN  : NBOCC  : NOMBRE D'OCCURENCE
! OUT : NLM    : NOMBRE TOTAL DE MAILLE
! OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
! ----------------------------------------------------------------------
!     NSECBA : NOMBRE DE SECTIONS PAR BARRE
!     NTYPSE : NOMBRE DE TYPE DE SECTION
! ----------------------------------------------------------------------
    real(kind=8) :: tst
    character(len=8) :: k8b, kioc, ki, nomu
    character(len=24) :: valk(3)
    character(len=16) :: k16b, sec, concep, cmd
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ioc, irece, irech, jcar, jcara, jexp
    integer :: jpara, jsect, jtab, jtype, jvale, l, nbcar
    integer :: nbo, nbval, nc, ncar, ncara, ncmax, ndim
    integer :: ng, nm, ns, nsec, nsecba, nsom, ntypse
    integer :: nv, nval
!-----------------------------------------------------------------------
    call jemarq()
    call getres(nomu, concep, cmd)
!
    call wkvect('&&ACEVBA.TAB_PARA', 'V V I', 10, jpara)
    call acedat('BARRE', 0, zi(jpara), k16b, k8b,&
                k8b, k8b)
    nsecba = zi(jpara )
    ntypse = zi(jpara+1)
    nbo = zi(jpara+2)
    nbcar = zi(jpara+3)
    nbval = zi(jpara+4)
    call wkvect('&&ACEVBA.NCP', 'V V I', ntypse, jtype)
    do 2 i = 1, ntypse
        zi(jtype+i-1) = zi(jpara+4+i)
 2  end do
    ndim = zi(jtype+1) * ntypse
    call wkvect('&&ACEVBA.TYP_SECT', 'V V K16', ntypse, jsect)
    call wkvect('&&ACEVBA.EXPBAR', 'V V K8 ', nbo, jexp)
    call wkvect('&&ACEVBA.TABBAR', 'V V K8 ', nbo, jtab)
    call wkvect('&&ACEVBA.CARBAR', 'V V K8 ', ndim, jcar)
    call acedat('BARRE', 1, zi(jpara), zk16(jsect), zk8(jexp),&
                zk8(jtab), zk8(jcar))
    call wkvect('&&ACEVBA.CARA', 'V V K8', nbcar, jcara)
    call wkvect('&&ACEVBA.VALE', 'V V R8', nbval, jvale)
!
    tst = r8maem()
    nlm = 0
    nlg = 0
    do 10 ioc = 1, nbocc
        call codent(ioc, 'G', kioc)
        call getvtx('BARRE', 'GROUP_MA', iocc=ioc, nbval=0, nbret=ng)
        call getvtx('BARRE', 'MAILLE', iocc=ioc, nbval=0, nbret=nm)
        call getvtx('BARRE', 'SECTION', iocc=ioc, nbval=0, nbret=ns)
        call getvtx('BARRE', 'SECTION', iocc=ioc, scal=sec, nbret=nsec)
        call getvtx('BARRE', 'CARA', iocc=ioc, nbval=0, nbret=nc)
        call getvtx('BARRE', 'CARA', iocc=ioc, nbval=nbcar, vect=zk8(jcara),&
                    nbret=ncar)
        call getvr8('BARRE', 'VALE', iocc=ioc, nbval=0, nbret=nv)
        call getvr8('BARRE', 'VALE', iocc=ioc, nbval=nbval, vect=zr(jvale),&
                    nbret=nval)
!
! -- CARA
        if (ncar .gt. 0) then
            ncara = ncar
            do 20 l = 1, ntypse
                if (sec .eq. zk16(jsect+l-1)) then
                    ncmax = zi(jtype+l-1)*nsecba
                    call codent(ncmax, 'G', ki)
                    if (ncar .gt. ncmax .and. l .ne. 2) then
                        valk(1) = kioc
                        valk(2) = ki
                        valk(3) = zk16(jsect+l-1)
                        call utmess('E', 'MODELISA_44', nk=3, valk=valk)
                        ier = ier + 1
                    endif
                    if (l .eq. 2) then
                        if (ncar .gt. 4) then
                            valk(1) = kioc
                            valk(2) = zk16(jsect+l-1)
                            call utmess('E', 'MODELISA_45', nk=2, valk=valk)
                            ier = ier + 1
                        endif
                        irech = 0
                        irece = 0
                        do 30 i = 1, ncar
                            if (zk8(jcara+i-1)(1:2) .eq. 'H ') then
                                if (irech .eq. 2) then
                                    valk(1) = kioc
                                    valk(2) = zk16(jsect+l-1)
                                    call utmess('E', 'MODELISA_46', nk=2, valk=valk)
                                    ier = ier + 1
                                endif
                                irech = 1
                            endif
                            if (zk8(jcara+i-1)(1:2) .eq. 'HY' .or. zk8(jcara+i-1)(1:2) .eq.&
                                'HZ') then
                                if (irech .eq. 1) then
                                    valk(1) = kioc
                                    valk(2) = zk16(jsect+l-1)
                                    call utmess('E', 'MODELISA_47', nk=2, valk=valk)
                                    ier = ier + 1
                                endif
                                irech = 2
                            endif
                            if (zk8(jcara+i-1)(1:3) .eq. 'EP ') then
                                if (irece .eq. 1) then
                                    valk(1) = kioc
                                    valk(2) = zk16(jsect+l-1)
                                    call utmess('E', 'MODELISA_48', nk=2, valk=valk)
                                    ier = ier + 1
                                endif
                                irece = 2
                            endif
                            if (zk8(jcara+i-1)(1:3) .eq. 'EPX' .or. zk8(jcara+i-1)(1:3)&
                                .eq. 'EPY') then
                                if (irece .eq. 2) then
                                    valk(1) = kioc
                                    valk(2) = zk16(jsect+l-1)
                                    call utmess('E', 'MODELISA_49', nk=2, valk=valk)
                                    ier = ier + 1
                                endif
                                irece = 1
                            endif
30                      continue
                    endif
                endif
20          continue
        endif
!
! -- VALE
        if (nval .gt. 0) then
            if (nval .ne. ncara) then
                call codent(ncara, 'G', ki)
                valk(1) = kioc
                valk(2) = ki
                call utmess('E', 'MODELISA_50', nk=2, valk=valk)
                ier = ier + 1
            else
                do 70 i = 1, nval
                    call codent(i, 'G', ki)
                    if (zr(jvale+i-1) .eq. tst) then
                        valk(1) = kioc
                        valk(2) = zk16(jsect+l-1)
                        valk(3) = ki
                        call utmess('E', 'MODELISA_51', nk=3, valk=valk)
                        ier = ier + 1
                    endif
70              continue
            endif
        endif
!
! ---    GROUP_MA + GROUP_NO + NOEUD + MAILLE
        nsom = ng + nm
        if (nsom .eq. ng .or. nsom .eq. nm) then
            nlm = max(nlm,-nm)
            nlg = max(nlg,-ng)
        endif
!
10  end do
!
    call jedetr('&&ACEVBA.TAB_PARA')
    call jedetr('&&ACEVBA.NCP')
    call jedetr('&&ACEVBA.TYP_SECT')
    call jedetr('&&ACEVBA.EXPBAR')
    call jedetr('&&ACEVBA.TABBAR')
    call jedetr('&&ACEVBA.CARBAR')
    call jedetr('&&ACEVBA.CARA')
    call jedetr('&&ACEVBA.VALE')
!
    call jedema()
end subroutine
