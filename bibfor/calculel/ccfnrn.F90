subroutine ccfnrn(option, resuin, resuou, lisord, nbordr,&
                  lischa, ncharg, chtype, typesd)
    implicit none
!     --- ARGUMENTS ---
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asasve.h"
#include "asterfort/ascova.h"
#include "asterfort/calcop.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlima.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mcmult.h"
#include "asterfort/memam2.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/nmdome.h"
#include "asterfort/ntdoth.h"
#include "asterfort/numecn.h"
#include "asterfort/pteddl.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
#include "asterfort/vecgme.h"
#include "asterfort/vechme.h"
#include "asterfort/vefnme.h"
#include "asterfort/vefpme.h"
#include "asterfort/vrcins.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
#include "asterfort/verif_bord.h"
    integer :: nbordr, ncharg
    character(len=4) :: chtype
    character(len=8) :: resuin, resuou
    character(len=16) :: option, typesd
    character(len=19) :: lischa, lisord
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
! ----------------------------------------------------------------------
!  CALC_CHAMP - CALCUL DES FORCES NODALES ET DES REACTIONS NODALES
!  -    -                  -      -              -         -
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
    integer :: jordr, iret, iordr, i, jinfc, nbchar, ic, jref
    integer :: iachar, ichar, ii, nuord, nh, jnmo, nbddl, lmat, iad, ind
    integer :: neq, jfo, lonch
    integer :: lonc2, ltrav, j, inume, jddl, jddr, lacce
    integer :: cret
    real(kind=8) :: etan, time, partps(3), omega2, coef(3)
    character(len=1) :: stop
    character(len=2) :: codret
    character(len=6) :: nompro
    character(len=8) :: k8bid, kiord, ctyp, nomcmp(3)
    character(len=16) :: typmo, optio2
    character(len=19) :: ligrel, chdep2, infcha
    character(len=24) :: numref, fomult, charge, infoch, vechmp, vachmp, cnchmp
    character(len=24) :: vecgmp, vacgmp, cncgmp, vefpip, vafpip, cnfpip, vfono
    character(len=24) :: carac
    character(len=24) :: vafono, vreno, vareno, sigma, chdepl, valk(3), nume
    character(len=24) :: mater
    character(len=24) :: chvive, chacve, masse, chvarc, compor, k24bid, chamno
    character(len=24) :: strx
    character(len=24) :: bidon, chacce, modele, kstr
    aster_logical :: exitim, lstr, lstr2
    real(kind=8), pointer :: cgmp(:) => null()
    real(kind=8), pointer :: chmp(:) => null()
    real(kind=8), pointer :: fono(:) => null()
    real(kind=8), pointer :: fpip(:) => null()
    real(kind=8), pointer :: noch(:) => null()
    real(kind=8), pointer :: reno(:) => null()
    real(kind=8), pointer :: nldepl(:) => null()
    parameter(nompro='CCFNRN')
    data chvarc/'&&CCFNRN.CHVARC'/
    data infcha/'&&INFCHA.INFCHA'/
    data k24bid/' '/
    data nomcmp/'DX','DY','DZ'/
!
    call jemarq()
    bidon='&&'//nompro//'.BIDON'
!
    call jeveuo(lisord, 'L', jordr)
!
! ----ON VERIFIE SI DERRIERE UN CONCEPT MODE_MECA SE TROUVE UN MODE_DYN
    if (typesd(1:9) .eq. 'MODE_MECA') then
        call rsadpa(resuin, 'L', 1, 'TYPE_MODE', 1,&
                    0, sjv=iad, styp=k8bid)
        typmo=zk16(iad)
    else
        typmo=' '
    endif
!
! TRI DES OPTIONS SUIVANT TYPESD
    lmat=0
    exitim=.false.
    if (typesd .eq. 'EVOL_ELAS' .or. typesd .eq. 'EVOL_NOLI') then
        exitim=.true.
    else if (typesd.eq.'MODE_MECA' .or. typesd.eq.'DYNA_TRANS') then
        call jeexin(resuin//'           .REFD', iret)
        if (iret .ne. 0) then
            call dismoi('REF_MASS_PREM', resuin, 'RESU_DYNA', repk=masse, arret='C')
            if (masse .ne. ' ') then
                call mtdscr(masse)
                call jeveuo(masse(1:19)//'.&INT', 'E', lmat)
            endif
        endif
        if (typesd .eq. 'DYNA_TRANS') exitim=.true.
    else if (typesd.eq.'DYNA_HARMO') then
        call jeexin(resuin//'           .REFD', iret)
        if (iret .ne. 0) then
            call dismoi('REF_MASS_PREM', resuin, 'RESU_DYNA', repk=masse, arret='C')
            if (masse .ne. ' ') then
                call mtdscr(masse)
                call jeveuo(masse(1:19)//'.&INT', 'E', lmat)
            endif
        endif
    endif
    if (typesd .eq. 'MODE_MECA' .or. typesd .eq. 'DYNA_TRANS') then
        call dismoi('NUME_DDL', resuin, 'RESU_DYNA', repk=numref)
    endif
    carac=' '
    charge=' '
    mater=' '
    modele=' '
    nuord=zi(jordr)
    if (typesd .eq. 'EVOL_THER') then
        call ntdoth(modele, mater, carac, infcha,& 
                    result = resuou, nume_store = nuord)
    else
        call nmdome(modele, mater, carac, infcha, resuou(1:8),&
                    nuord)
    endif
    if (modele(1:2) .eq. '&&') then
        call utmess('F', 'CALCULEL3_50')
    endif
!
    fomult=infcha//'.FCHA'
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
            do ii = 1, nbchar
                zk8(ichar-1+ii)=zk24(iachar-1+ii)(1:8)
            end do
        else
            ichar=1
        endif
    else
        nbchar=0
        ichar=1
    endif
    call exlima(' ', 0, 'V', modele, ligrel)
!     ON REGARDE S'IL Y A DES ELEMENTS DE STRUCTURE UTILISANT LE CHAMP
!     STRX_ELGA
    strx=' '
    call dismoi('EXI_STRX', modele, 'MODELE', repk=kstr)
    lstr=(kstr(1:3).eq.'OUI')
!     Y A-T-IL DES ELEMENTS SACHANT CALCULER L'OPTION STRX_ELGA
    call dismoi('EXI_STR2', modele, 'MODELE', repk=kstr)
    lstr2=(kstr(1:3).eq.'OUI')
!
    time=0.d0
    do i = 1, nbordr
        call jemarq()
        iordr=zi(jordr+i-1)
!
        vechmp=' '
        vachmp=' '
        cnchmp=' '
        vecgmp=' '
        vacgmp=' '
        cncgmp=' '
        vefpip=' '
        vafpip=' '
        cnfpip=' '
        etan=0.d0
        vfono=' '
        vafono=' '
        vreno='&&'//nompro//'           .RELR'
        vareno='&&'//nompro//'           .RELR'
!
        nh=0
        if (typesd(1:8) .eq. 'FOURIER_') then
            call rsadpa(resuin, 'L', 1, 'NUME_MODE', iordr,&
                        0, sjv=jnmo, styp=k8bid)
            nh=zi(jnmo)
        endif
! ICI
        call rsexch(' ', resuin, 'SIEF_ELGA', iordr, sigma,&
                    iret)
        if (iret .ne. 0) then
            optio2 = 'SIEF_ELGA'
            call calcop(optio2, ' ', resuin, resuou, lisord,&
                        nbordr, lischa, ncharg, chtype, typesd,&
                        cret)
        endif
        if (lstr) then
            call rsexch(' ', resuin, 'STRX_ELGA', iordr, strx,&
                        iret)
            if (iret .ne. 0 .and. lstr2) then
                optio2 = 'STRX_ELGA'
                call calcop(optio2, ' ', resuin, resuou, lisord,&
                            nbordr, lischa, ncharg, chtype, typesd,&
                            cret)
            endif
        endif
!
        call rsexch(' ', resuin, 'DEPL', iordr, chdepl,&
                    iret)
        if (iret .ne. 0) then
            call codent(iordr, 'G', kiord)
            valk(1)=kiord
            valk(2)=option
            call utmess('A', 'PREPOST5_3', nk=2, valk=valk)
            goto 280
!
        else
!
!         CREATION D'UN VECTEUR ACCROISSEMENT DE DEPLACEMENT NUL
!         POUR LE CALCUL DE FORC_NODA DANS LES POU_D_T_GD
            chdep2='&&'//nompro//'.CHDEP_NUL'
            call copisd('CHAMP_GD', 'V', chdepl, chdep2)
            call jelira(chdep2//'.VALE', 'LONMAX', nbddl)
            call jerazo(chdep2//'.VALE', nbddl, 1)
        endif
!
!       -- CALCUL D'UN NUME_DDL "MINIMUM" POUR ASASVE :
        if (typesd .eq. 'MODE_MECA' .or. typesd .eq. 'DYNA_TRANS') then
            nume=numref(1:14)//'.NUME'
        else
            call numecn(modele, chdepl, nume)
        endif
!
        call rsexch(' ', resuin, 'VITE', iordr, chvive,&
                    iret)
        if (iret .eq. 0) then
            chvive='&&'//nompro//'.CHVIT_NUL'
            call copisd('CHAMP_GD', 'V', chdepl, chvive)
            call jelira(chvive(1:19)//'.VALE', 'LONMAX', nbddl)
            call jerazo(chvive(1:19)//'.VALE', nbddl, 1)
        endif
        call rsexch(' ', resuin, 'ACCE', iordr, chacve,&
                    iret)
        if (iret .eq. 0) then
            chacve='&&'//nompro//'.CHACC_NUL'
            call copisd('CHAMP_GD', 'V', chdepl, chacve)
            call jelira(chacve(1:19)//'.VALE', 'LONMAX', nbddl)
            call jerazo(chacve(1:19)//'.VALE', nbddl, 1)
        endif
!
        if (exitim) then
            call rsadpa(resuin, 'L', 1, 'INST', iordr,&
                        0, sjv=iad, styp=ctyp)
            time=zr(iad)
        endif
!
        call vrcins(modele, mater, carac, time, chvarc(1:19),&
                    codret)
        call rsexch(' ', resuin, 'COMPORTEMENT', iordr, compor,&
                    iret)
!
        call vefnme(option, 'V', modele, mater, carac,&
                    compor, partps, nh, ligrel, chvarc,&
                    sigma, strx, chdepl, chdep2, vfono)
!
!       --- ASSEMBLAGE DES VECTEURS ELEMENTAIRES ---
        call asasve(vfono, nume, 'R', vafono)
!
!       --- CREATION DE LA STRUCTURE CHAM_NO ---
        call rsexch(' ', resuou, option, iordr, chamno,&
                    iret)
!
        call jeexin(chamno(1:19)//'.REFE', iret)
        if (iret .ne. 0) then
            call codent(iordr, 'G', kiord)
            valk(1)=option
            valk(2)=kiord
            call utmess('A', 'PREPOST5_1', nk=2, valk=valk)
            call detrsd('CHAM_NO', chamno(1:19))
        endif
        call vtcreb(chamno, 'G', 'R',&
                    nume_ddlz = nume,&
                    nb_equa_outz = neq)
        call jeveuo(chamno(1:19)//'.VALE', 'E', vr=noch)
!
!       --- REMPLISSAGE DE L'OBJET .VALE DU CHAM_NO ---
        call jeveuo(vafono, 'L', jfo)
        call jeveuo(zk24(jfo)(1:19)//'.VALE', 'L', vr=fono)
        call jelira(chamno(1:19)//'.VALE', 'LONMAX', lonch)
!
!       --- STOCKAGE DES FORCES NODALES ---
        if (option .eq. 'FORC_NODA') then
            do j = 0, lonch-1
                noch(1+j)=fono(1+j)
            end do
            goto 270
        endif
!
!       --- CALCUL DES FORCES NODALES DE REACTION

        if (charge .ne. ' ') then
            partps(1)=time
!
! --- CHARGES NON PILOTEES (TYPE_CHARGE: 'FIXE_CSTE')
!
            if (ligrel(1:8) .ne. modele) then
                stop = 'C'
!               -- on verifie que le ligrel contient bien les mailles de bord
                call verif_bord(modele,ligrel)
            else
                stop = 'S'
            endif
            call vechme(stop, modele, charge, infoch, partps,&
                        carac, mater, vechmp, varc_currz = chvarc, ligrel_calcz = ligrel)
!
            call asasve(vechmp, nume, 'R', vachmp)
            call ascova('D', vachmp, fomult, 'INST', time,&
                        'R', cnchmp)
!
! --- CHARGES SUIVEUSE (TYPE_CHARGE: 'SUIV')
            call detrsd('CHAMP_GD', bidon)
            call vtcreb(bidon, 'G', 'R',&
                        nume_ddlz = nume,&
                        nb_equa_outz = neq)
            call vecgme(modele, carac, mater, charge, infoch,&
                        partps(1), chdepl, bidon, vecgmp, partps(1),&
                        compor, k24bid, ligrel, chvive, k24bid)
            call asasve(vecgmp, nume, 'R', vacgmp)
            call ascova('D', vacgmp, fomult, 'INST', time,&
                        'R', cncgmp)
!
! --- POUR UN EVOL_NOLI, PRISE EN COMPTE DES FORCES PILOTEES
            if (typesd .eq. 'EVOL_NOLI') then
! - CHARGES PILOTEES (TYPE_CHARGE: 'FIXE_PILO')
                call vefpme(modele, carac, mater, charge, infoch,&
                            partps, k24bid, vefpip, ligrel)
                call asasve(vefpip, nume, 'R', vafpip)
                call ascova('D', vafpip, fomult, 'INST', time,&
                            'R', cnfpip)
! - RECUPERATION DU PARAMETRE DE CHARGE ETAN DANS LA SD EVOL_NOLI
                call rsadpa(resuin, 'L', 1, 'ETA_PILOTAGE', iordr,&
                            0, sjv=iad, styp=ctyp)
                etan=zr(iad)
            endif
!
! --- CALCUL DU CHAMNO DE REACTION PAR DIFFERENCE DES FORCES NODALES
! --- ET DES FORCES EXTERIEURES MECANIQUES NON SUIVEUSES
            call jeveuo(cnchmp(1:19)//'.VALE', 'L', vr=chmp)
            call jeveuo(cncgmp(1:19)//'.VALE', 'L', vr=cgmp)
            do j = 0, lonch-1
                noch(1+j)=fono(1+j)-chmp(1+j)-cgmp(1+j)
            end do
            if (typesd.eq.'EVOL_NOLI') then
                call jeveuo(cnfpip(1:19)//'.VALE', 'L', vr=fpip)
                do j = 0, lonch-1
                    noch(1+j)=noch(1+j)-etan*fpip(1+j)
                end do
            endif
        else
!         --- CALCUL DU CHAMNO DE REACTION PAR RECOPIE DE FORC_NODA
            do j = 0, lonch-1
                noch(1+j)=fono(1+j)
            end do
        endif
!
!       --- TRAITEMENT DES MODE_MECA ---
        if (typesd .eq. 'MODE_MECA' .and. typmo(1:8) .eq. 'MODE_DYN') then
            call rsadpa(resuin, 'L', 1, 'OMEGA2', iordr,&
                        0, sjv=iad, styp=ctyp)
            omega2=zr(iad)
            call jeveuo(chdepl(1:19)//'.VALE', 'L', vr=nldepl)
            call jelira(chdepl(1:19)//'.VALE', 'LONMAX', lonc2)
            call wkvect('&&'//nompro//'.TRAV', 'V V R', lonc2, ltrav)
            if (lmat .eq. 0) then
                call utmess('F', 'PREPOST3_81', sk=option)
            endif
            call mrmult('ZERO', lmat, nldepl, zr(ltrav), 1,&
                        .true._1)
            do j = 0, lonch-1
                noch(1+j)=noch(1+j)-omega2*zr(ltrav+j)
            end do
            call jedetr('&&'//nompro//'.TRAV')
!
!       --- TRAITEMENT DES MODE_STAT ---
            elseif (typesd.eq.'MODE_MECA' .and. typmo(1:8).eq.'MODE_STA')&
        then
            call rsadpa(resuin, 'L', 1, 'TYPE_DEFO', iordr,&
                        0, sjv=iad, styp=ctyp)
            if (zk16(iad)(1:9) .eq. 'FORC_IMPO') then
                call rsadpa(resuin, 'L', 1, 'NUME_DDL', iordr,&
                            0, sjv=iad, styp=ctyp)
                inume=zi(iad)
                noch(inume)=noch(inume)-1.d0
            else if (zk16(iad)(1:9).eq.'ACCE_IMPO') then
                call jelira(chdepl(1:19)//'.VALE', 'LONMAX', lonc2)
                call rsadpa(resuin, 'L', 1, 'COEF_X', iordr,&
                            0, sjv=iad, styp=ctyp)
                coef(1)=zr(iad)
                call rsadpa(resuin, 'L', 1, 'COEF_Y', iordr,&
                            0, sjv=iad, styp=ctyp)
                coef(2)=zr(iad)
                call rsadpa(resuin, 'L', 1, 'COEF_Z', iordr,&
                            0, sjv=iad, styp=ctyp)
                coef(3)=zr(iad)
                call wkvect('&&'//nompro//'.POSI_DDL', 'V V I', 3*lonc2, jddl)
                call pteddl('NUME_DDL', nume, 3, nomcmp, lonc2,&
                            tabl_equa = zi(jddl))
                call wkvect('&&'//nompro//'.POSI_DDR', 'V V R', lonc2, jddr)
                do ic = 1, 3
                    ind=lonc2*(ic-1)
                    do j = 0, lonc2-1
                        zr(jddr+j)=zr(jddr+j)+zi(jddl+ind+j)*coef(ic)
                    end do
                end do
                call wkvect('&&'//nompro//'.TRAV', 'V V R', lonc2, ltrav)
                if (lmat .eq. 0) then
                    call utmess('F', 'PREPOST3_81', sk=option)
                endif
                call mrmult('ZERO', lmat, zr(jddr), zr(ltrav), 1,&
                            .true._1)
                do j = 0, lonch-1
                    noch(1+j)=noch(1+j)-zr(ltrav+j)
                end do
                call jedetr('&&'//nompro//'.POSI_DDR')
                call jedetr('&&'//nompro//'.POSI_DDL')
                call jedetr('&&'//nompro//'.TRAV')
            endif
!
!       --- TRAITEMENT DE DYNA_TRANS ---
        else if (typesd.eq.'DYNA_TRANS') then
            call rsexch(' ', resuin, 'ACCE', iordr, chacce,&
                        iret)
            if (iret .eq. 0) then
                call jeveuo(chacce(1:19)//'.VALE', 'L', lacce)
                call wkvect('&&'//nompro//'.TRAV', 'V V R', lonch, ltrav)
                if (lmat .eq. 0) then
                    call utmess('F', 'PREPOST3_81', sk=option)
                endif
                call mrmult('ZERO', lmat, zr(lacce), zr(ltrav), 1,&
                            .true._1)
                do j = 0, lonch-1
                    noch(1+j)=noch(1+j)+zr(ltrav+j)
                end do
                call jedetr('&&'//nompro//'.TRAV')
            else
                call utmess('A', 'CALCULEL3_1')
            endif
!
!       --- TRAITEMENT DE DYNA_HARMO ---
        else if (typesd.eq.'DYNA_HARMO') then
            call rsexch(' ', resuin, 'ACCE', iordr, chacce,&
                        iret)
            if (iret .eq. 0) then
                call jeveuo(chacce(1:19)//'.VALE', 'L', lacce)
                call wkvect('&&'//nompro//'.TRAV', 'V V C', lonch, ltrav)
                if (lmat .eq. 0) then
                    call utmess('F', 'PREPOST3_81', sk=option)
                endif
                call mcmult('ZERO', lmat, zc(lacce), zc(ltrav), 1,&
                            .true._1)
                do j = 0, lonch-1
                    noch(1+j)=noch(1+j)+dble(zc(ltrav+j))
                end do
                call jedetr('&&'//nompro//'.TRAV')
            else
                call utmess('A', 'CALCULEL3_1')
            endif
!
!       --- TRAITEMENT DE EVOL_NOLI ---
        else if (typesd.eq.'EVOL_NOLI') then
            call rsexch(' ', resuin, 'ACCE', iordr, chacce,&
                        iret)
            if (iret .eq. 0) then
                optio2='M_GAMMA'
!
!           --- CALCUL DES MATRICES ELEMENTAIRES DE MASSE
                call memam2(optio2, modele, nbchar, zk8(ichar), mater,&
                            carac, compor, exitim, time, chacce,&
                            vreno, 'V', ligrel)
!
!           --- ASSEMBLAGE DES VECTEURS ELEMENTAIRES ---
                call asasve(vreno, nume, 'R', vareno)
                call jeveuo(vareno, 'L', jref)
                call jeveuo(zk24(jref)(1:19)//'.VALE', 'L', vr=reno)
                do j = 0, lonch-1
                    noch(1+j)=noch(1+j)+reno(1+j)
                end do
            endif
        endif
!
270     continue
        call rsnoch(resuou, option, iordr)
!
        if (typesd .eq. 'EVOL_THER') then
            call ntdoth(modele, mater, carac, infcha, &
                        result = resuou, nume_store = iordr)
        else
            call nmdome(modele, mater, carac, infcha, resuou(1:8),&
                        iordr)
        endif
        call detrsd('CHAMP_GD', '&&'//nompro//'.SIEF')
        call detrsd('VECT_ELEM', vfono(1:8))
        call detrsd('VECT_ELEM', vreno(1:8))
        call detrsd('VECT_ELEM', vechmp(1:8))
        call detrsd('VECT_ELEM', vecgmp(1:8))
        call detrsd('VECT_ELEM', vefpip(1:8))
        call detrsd('CHAMP_GD', cnchmp(1:8)//'.ASCOVA')
        call detrsd('CHAMP_GD', cncgmp(1:8)//'.ASCOVA')
        call detrsd('CHAMP_GD', cnfpip(1:8)//'.ASCOVA')
        call jedetr(vachmp(1:8))
        call jedetr(vacgmp(1:8))
        call jedetr(vafpip(1:8))
        call jedetr(vachmp(1:6)//'00.BIDON')
        call jedetr(vacgmp(1:6)//'00.BIDON')
        call jedetr(vafpip(1:6)//'00.BIDON')
        call jedetr(vachmp(1:6)//'00.BIDON     .VALE')
        call jedetr(vacgmp(1:6)//'00.BIDON     .VALE')
        call jedetr(vafpip(1:6)//'00.BIDON     .VALE')
        call jedetr(vachmp(1:6)//'00.BIDON     .DESC')
        call jedetr(vacgmp(1:6)//'00.BIDON     .DESC')
        call jedetr(vafpip(1:6)//'00.BIDON     .DESC')
        call jedetr(vachmp(1:6)//'00.BIDON     .REFE')
        call jedetr(vacgmp(1:6)//'00.BIDON     .REFE')
        call jedetr(vafpip(1:6)//'00.BIDON     .REFE')
        call jedetr(vachmp(1:8)//'.ASCOVA')
        call jedetr(vacgmp(1:8)//'.ASCOVA')
        call jedetr(vafpip(1:8)//'.ASCOVA')
280     continue
        call jedema()
    end do
    call detrsd('CHAMP_GD', bidon)
    call jedema()
end subroutine
