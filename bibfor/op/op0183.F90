subroutine op0183()
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
!-----------------------------------------------------------------------
!     COMMANDE :  CALC_FORC_NONL
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/asasve.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infbav.h"
#include "asterfort/infmaj.h"
#include "asterfort/infmue.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdome.h"
#include "asterfort/nmdocc.h"
#include "asterfort/onerrf.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
#include "asterfort/vefnme.h"
#include "asterfort/vrcins.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
    integer :: ibid
    integer :: i, iachar, iad, ichar
    integer :: iordr, iret, iret2, j
    integer :: jfo, jfono, jinfc
    integer :: jnoch, jordr
    integer :: lonch, lvafon, n0, n2, nbchar
    integer :: nbddl, nbordr, nc, neq, nh, np
    integer :: ii, ltps, ltps2
    real(kind=8) :: time, prec, partps(3)
!
    character(len=2) :: codret
    character(len=6) :: nompro
    character(len=8) :: k8bid, ctyp, crit, materi
    character(len=8) :: kiord
    character(len=16) :: option, type, oper, k16bid
    character(len=16) :: compex
    character(len=19) :: resuco, knum, infcha, ligrel, resuc1, chdep2
    character(len=24) :: modele, mater, carac, charge, infoch, chamno
    character(len=24) :: nume, vfono, vafono, sigma, chdepl, k24bid
    character(len=24) :: vreno, compor, chvive, chacve, raide
    character(len=24) :: bidon, chvarc
    character(len=24) :: numref, valk(3)
    logical :: l_etat_init
!     ------------------------------------------------------------------
    parameter(nompro='OP0183')
!     ------------------------------------------------------------------
!
    logical :: exitim
!
!     ------------------------------------------------------------------
    data infcha/'&&INFCHA.INFCHA'/
    data k24bid/' '/
    data chvarc/'&&OP0183.CHVARC'/
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
! --- ON STOCKE LE COMPORTEMENT EN CAS D'ERREUR AVANT MNL : COMPEX
! --- PUIS ON PASSE DANS LE MODE "VALIDATION DU CONCEPT EN CAS D'ERREUR"
    call onerrf(' ', compex, ibid)
    call onerrf('EXCEPTION+VALID', k16bid, ibid)
!
    call infmue()
!
! --- OPTIONS A CALCULER
!
    call getres(resuc1, type, oper)
    ASSERT(type.eq.'DYNA_TRANS')
    call getvid(' ', 'RESULTAT', scal=resuco, nbret=n0)
    ASSERT(resuco.ne.resuc1)
!
!
    call getvtx(' ', 'OPTION', scal=option, nbret=n2)
    ASSERT(n2.eq.1 .and. option.eq.'FONL_NOEU')
!
!
    knum='&&'//nompro//'.NUME_ORDRE'
!
!=======================================================================
    k8bid='&&'//nompro
    ibid=1
!=======================================================================
!
    call getvr8(' ', 'PRECISION', scal=prec, nbret=np)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
!
    call rsutnu(resuco, ' ', 0, knum, nbordr,&
                prec, crit, iret)
    if (iret .eq. 10) then
        call utmess('F', 'CALCULEL4_8', sk=resuco)
        goto 60
!
    endif
    if (iret .ne. 0) then
        call utmess('F', 'ALGORITH3_41')
        goto 60
!
    endif
    call jeveuo(knum, 'L', jordr)
    bidon='&&'//nompro//'.BIDON'
!
!
    exitim=.true.
    carac=' '
    charge=' '
    mater=' '
    call rscrsd('G', resuc1, type, nbordr)
    call getvid(' ', 'MODELE', scal=modele, nbret=n0)
    ligrel=modele(1:8)//'.MODELE'
    ASSERT(n0.eq.1)
    call getvid(' ', 'CHAM_MATER', scal=materi, nbret=n0)
    if (n0 .gt. 0) then
        call rcmfmc(materi, mater)
    else
        mater=' '
    endif
    carac=' '
    call getvid(' ', 'CARA_ELEM', scal=carac, nbret=n0)
!
! INFO. RELATIVE AUX CHARGES
    charge=infcha//'.LCHA'
    infoch=infcha//'.INFC'
    call jeexin(infoch, iret)
    ASSERT(iret.eq.0)
    nbchar=0
    ichar=1
!
!
!
    time=0.d0
    partps(1)=0.d0
    partps(2)=0.d0
    partps(3)=0.d0
    l_etat_init = .false.
!
!
    numref=' '
    call refdcp(resuco, resuc1)
    call dismoi('F', 'REF_RIGI_PREM', resuc1, 'RESU_DYNA', ibid,&
                raide, iret)
    if (raide .ne. ' ') then
        call dismoi('F', 'NOM_NUME_DDL', raide, 'MATR_ASSE', ibid,&
                    numref, iret)
    endif
!
!
    do i = 1, nbordr
        call jemarq()
        iordr=zi(jordr+i-1)
        charge=infcha//'.LCHA'
        infoch=infcha//'.INFC'
        call jeexin(infoch, iret)
        if (iret .ne. 0) then
            call jeveuo(infoch, 'L', jinfc)
            nbchar=zi(jinfc)
            if (nbchar .ne. 0) then
                call jeveuo(charge, 'L', iachar)
                call jedetr('&&'//nompro//'.L_CHARGE')
                call wkvect('&&'//nompro//'.L_CHARGE', 'V V K8', nbchar, ichar)
                do 20 ii = 1, nbchar
                    zk8(ichar-1+ii)=zk24(iachar-1+ii)(1:8)
20              continue
            else
                ichar=1
            endif
        else
            nbchar=0
            ichar=1
        endif
!
!
!
        vfono=' '
        vafono=' '
        vreno='&&'//nompro//'           .RELR'
        nh=0
!
        call rsexch(' ', resuco, 'SIEF_ELGA', iordr, sigma,&
                    iret)
        if (iret .ne. 0) then
            call rsexch(' ', resuco, 'SIEF_ELGA', iordr, sigma,&
                        iret2)
            if (iret2 .ne. 0 .and. option .ne. 'FONL_NOEU') then
                call codent(iordr, 'G', kiord)
                valk(1)=kiord
                valk(2)=option
                call utmess('A', 'PREPOST5_2', nk=2, valk=valk)
                goto 40
!
            endif
            if (iret2 .ne. 0 .and. option .eq. 'FONL_NOEU') then
                sigma=' '
            endif
        endif
!
        call rsexch(' ', resuco, 'DEPL', iordr, chdepl,&
                    iret)
        if (iret .ne. 0) then
            call codent(iordr, 'G', kiord)
            valk(1)=kiord
            valk(2)=option
            call utmess('A', 'PREPOST5_3', nk=2, valk=valk)
            goto 40
        else
!         CREATION D'UN VECTEUR ACCROISSEMENT DE DEPLACEMENT NUL
!         POUR LE CALCUL DE FORC_NODA DANS LES POU_D_T_GD
!
            chdep2='&&'//nompro//'.CHDEP_NUL'
            call copisd('CHAMP_GD', 'V', chdepl, chdep2)
            call jelira(chdep2//'.VALE', 'LONMAX', nbddl)
            call jerazo(chdep2//'.VALE', nbddl, 1)
        endif
!
!       -- CALCUL D'UN NUME_DDL "MINIMUM" POUR ASASVE :
        nume=numref(1:14)//'.NUME'
!
        call rsexch(' ', resuco, 'VITE', iordr, chvive,&
                    iret)
        if (iret .eq. 0) then
            chvive='&&'//nompro//'.CHVIT_NUL'
            call copisd('CHAMP_GD', 'V', chdepl, chvive)
            call jelira(chvive(1:19)//'.VALE', 'LONMAX', nbddl)
            call jerazo(chvive(1:19)//'.VALE', nbddl, 1)
        endif
        call rsexch(' ', resuco, 'ACCE', iordr, chacve,&
                    iret)
        if (iret .eq. 0) then
            chacve='&&'//nompro//'.CHACC_NUL'
            call copisd('CHAMP_GD', 'V', chdepl, chacve)
            call jelira(chacve(1:19)//'.VALE', 'LONMAX', nbddl)
            call jerazo(chacve(1:19)//'.VALE', nbddl, 1)
        endif
!
        if (exitim) then
            call rsadpa(resuco, 'L', 1, 'INST', iordr,&
                        0, sjv=iad, styp=ctyp)
            time=zr(iad)
        endif
!
        call vrcins(modele, mater, carac, time, chvarc(1:19),&
                    codret)
!
!       --- CALCUL DES VECTEURS ELEMENTAIRES ---
        if (i .eq. 1) then
            compor='&&OP0183.COMPOR'
            call nmdocc(modele(1:8), materi, l_etat_init, compor)
        endif
!
        call vefnme(option, 'V'   , modele, mater , carac ,&
                    compor, partps, nh    , ligrel, chvarc,&
                    sigma , ' '   , chdepl, chdep2, vfono)    
!
!       --- ASSEMBLAGE DES VECTEURS ELEMENTAIRES ---
        call asasve(vfono, nume, 'R', vafono)
!
        call rsexch(' ', resuc1, 'DEPL', iordr, chamno,&
                    iret)
        call rsadpa(resuc1, 'E', 1, 'INST', iordr,&
                    0, sjv=ltps2, styp=k8bid)
        call rsadpa(resuco, 'L', 1, 'INST', iordr,&
                    0, sjv=ltps, styp=k8bid)
        zr(ltps2)=zr(ltps)
!
!
        call jeexin(chamno(1:19)//'.REFE', iret)
        if (iret .ne. 0) then
            call codent(iordr, 'G', kiord)
            valk(1)=option
            valk(2)=kiord
            call utmess('A', 'PREPOST5_1', nk=2, valk=valk)
            call detrsd('CHAM_NO', chamno(1:19))
        endif
        call vtcreb(chamno, nume, 'G', 'R', neq)
        call jeveuo(chamno(1:19)//'.VALE', 'E', jnoch)
!
        call jeveuo(vafono, 'L', jfo)
        call jeveuo(zk24(jfo)(1:19)//'.VALE', 'L', jfono)
        call jelira(zk24(jfo)(1:19)//'.VALE', 'LONMAX', lvafon)
        call jelira(chamno(1:19)//'.VALE', 'LONMAX', lonch)
!
        do 30 j = 0, lonch-1
            zr(jnoch+j)=zr(jfono+j)
30      continue
!
        call rsnoch(resuc1, 'DEPL', iordr)
        call nmdome(modele, mater, carac, infcha, resuc1(1:8),&
                    iordr)
!
        call detrsd('CHAMP_GD', '&&'//nompro//'.SIEF')
        call detrsd('VECT_ELEM', vfono(1:8))
        call detrsd('VECT_ELEM', vreno(1:8))
40      continue
        call jedema()
    end do
!
    call jedetr(knum)
    call detrsd('CHAMP_GD', bidon)
!
! --- ON REMET LE MECANISME D'EXCEPTION A SA VALEUR INITIALE
    call onerrf(compex, k16bid, ibid)
!
60  continue
    call infbav()
    call jedema()
!
end subroutine
