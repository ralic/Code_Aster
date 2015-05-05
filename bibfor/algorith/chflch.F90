subroutine chflch(rigthe, vec2nd)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: hassan.berro at edf.fr
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/asasve.h"
#include "asterfort/ascavc.h"
#include "asterfort/ascova.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/vechth.h"
#include "asterfort/vedith.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
!   ------------------------------------------------------------------------------------
!   *** Subroutine arguments
!   ------------------------------------------------------------------------------------
!       --> Input variables
    character(len=8) :: rigthe
!       <-- Output variables
    character(len=24) :: vec2nd
!
!   ------------------------------------------------------------------------------------
!   *** Definition of local variables
!   ------------------------------------------------------------------------------------
    integer :: neq, nchar, ialich, jinf, ialifc, nchci, ich, iret, jnchtp, j2nd, lonch, k
    integer :: n1, jpro, jval, loncm1, jndirp
    real(kind=8) :: tpsthe(1)
    logical :: fmult, coecst
    character(len=8) :: typch, parcha, mate, carele, numedd
    character(len=19) :: infcha, nomcha
    character(len=24) :: charge, fomult, vechtp, vechtn, infoch, ligrch, lchin, cnchci, modele
    character(len=24) :: vachtp, cnchtp, nomfct, vediri, vadirp, cndirp, inst
!   ------------------------------------------------------------------------------------
    integer :: nbtych
    parameter   (nbtych = 11)
    character(len=6) :: nomlig(nbtych)
    nomlig=['.CIMPO'  ,'.SOURE'  ,'.FLURE'  ,'.FLUR2'  ,&
     &     '.T_EXT'  ,'.COEFH'  ,'.HECHP'  ,'.GRAIN'  ,'.FLUNL'  ,&
     &     '.SOUNL'  , '.RAYO '   ]
!   ------------------------------------------------------------------------------------
    vediri ='&&VETDIR           .RELR'
    vechtp ='&&VETCHA           .RELR'
    vechtn ='&&VENCHA           .RELR'
    infcha ='&&OP0116_INF_CHARGE'
    cndirp ='    '
    cnchtp ='    '
    tpsthe(1) = 0.0d0
    inst= ' '

    call jemarq()

    call dismoi('NOM_MODELE',rigthe, 'MATR_ASSE', repk=modele)
    call dismoi('CHAM_MATER',rigthe, 'MATR_ASSE', repk=mate)
    call dismoi('CARA_ELEM',rigthe, 'MATR_ASSE', repk=carele)
    call dismoi('NOM_NUME_DDL',rigthe, 'MATR_ASSE', repk=numedd)

    call vtcreb(vec2nd, 'V', 'R', nume_ddlz=numedd, nb_equa_outz=neq)

    charge = infcha//'.LCHA'
    infoch = infcha//'.INFC'
    fomult = infcha//'.FCHA'

    call getfac('EXCIT', nchar)

    if (nchar .ne. 0) then
        call wkvect(charge, 'V V K24', nchar, ialich)
        call wkvect(infoch, 'V V IS', 2*nchar+1, jinf)
        call wkvect(fomult, 'V V K24', nchar, ialifc)
        zi(jinf) = nchar
        nchci = 0
        do 32 , ich = 1 , nchar
            call getvid('EXCIT', 'CHARGE', iocc=ich, scal=nomcha)
            zk24(ialich+ich-1) = nomcha

            call dismoi('TYPE_CHARGE', nomcha, 'CHARGE', repk=typch)
            if ((typch(1:5).ne.'THER_') .and. (typch(1:5).ne.'CITH_')) then
                call utmess('E', 'CHARGES_21', sk=nomcha(1:8))
            endif

            ligrch = nomcha(1:8)//'.CHTH.LIGRE'

            if (typch(1:5) .eq. 'CITH_') then
                call jeexin(nomcha(1:19)//'.AFCK', iret)
                ASSERT(iret.ne.0)
                if (typch(5:7) .eq. '_FT') then
                    zi(jinf+ich) = -3
                else if (typch(5:7).eq.'_FO') then
                    zi(jinf+ich) = -2
                else
                    zi(jinf+ich) = -1
                endif
            endif

            lchin = ligrch(1:13)//'.CIMPO.DESC'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (typch(5:7) .eq. '_FO') then
                    zi(jinf+ich) = 2
                    call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=parcha)
                    if (parcha(1:3) .eq. 'OUI') then
                        zi(jinf+ich) = 3
                    endif
                else
                    zi(jinf+ich) = 1
                endif
            endif

            fmult = .false.
            call getvid('EXCIT', 'FONC_MULT', iocc=ich, scal=zk24( ialifc+ich-1), nbret=n1)

            if (n1 .eq. 0) then
                nomfct = '&&OP0116'
                call jeexin(nomfct(1:19)//'.PROL', iret)
                if (iret .eq. 0) then
                    ASSERT(lxlgut(nomfct).le.24)
                    call wkvect(nomfct(1:19)//'.PROL', 'V V K24', 6, jpro)
                    zk24(jpro) = 'CONSTANT'
                    zk24(jpro+1) = 'CONSTANT'
                    zk24(jpro+2) = 'TOUTPARA'
                    zk24(jpro+3) = 'TOUTRESU'
                    zk24(jpro+4) = 'CC      '
                    zk24(jpro+5) = nomfct
!
                    call wkvect(nomfct(1:19)//'.VALE', 'V V R', 2, jval)
                    zr(jval) = 1.0d0
                    zr(jval+1)= 1.0d0
                endif
                zk24(ialifc+ich-1) = '&&OP0116'
            else
                fmult = .true.
            endif

            do 326 , k = 2 ,nbtych
                lchin = ligrch(1:13)//nomlig(k)//'.DESC'
                call exisd('CHAMP_GD', lchin, iret)
                if (iret .ne. 0) then
                    if ((k.ge.7) .and. fmult) then
                        call utmess('F', 'CHARGES_20', sk=nomcha(1:8))
                    endif
                    if (typch(5:7) .eq. '_FO') then
                        zi(jinf+nchar+ich) = max(2,zi(jinf+nchar+ich))
                        call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=parcha)
                        if (parcha(1:3) .eq. 'OUI') then
                            if (nomlig(k) .ne. '.T_EXT') then
                                coecst = .false.
                            endif
                            zi(jinf+nchar+ich) = max(3,zi(jinf+nchar+ ich))
                        endif
                    else
                        zi(jinf+nchar+ich) = max(1,zi(jinf+nchar+ich))
                    endif
                endif
326         continue
32      continue
    endif
!
!   --- Dirichlet
    call vedith(modele, infcha, inst, vediri)
    call asasve(vediri, numedd, 'R', vadirp)
    call ascova('D', vadirp, fomult, 'INST', tpsthe(1),&
                'R', cndirp)
    call jeveuo(cndirp(1:19)//'.VALE', 'L', jndirp)
!   --- Cinematique
    cnchci = ' '
    call ascavc(charge, infoch, fomult, numedd, tpsthe(1),&
                cnchci)
!   --- Other
    call vechth('STAT',modele, charge, infoch, carele, mate,&
                tpsthe(1), ' ', ' ', vechtp)
    call asasve(vechtp, numedd, 'R', vachtp)
    call ascova('D', vachtp, fomult, 'INST', tpsthe(1),&
                'R', cnchtp)
    call jeveuo(cnchtp(1:19)//'.VALE', 'L', jnchtp)
    call jedetr(vechtp)
    call jedetr(vechtn)
!
    call jeveuo(vec2nd(1:19)//'.VALE', 'E', j2nd)
    call jelira(vec2nd(1:19)//'.VALE', 'LONMAX', lonch)
    loncm1 = lonch - 1
    do k = 0, loncm1
        zr(j2nd+k) = zr(jnchtp+k) + zr(jndirp+k)
    enddo

    call jedema()

end subroutine
