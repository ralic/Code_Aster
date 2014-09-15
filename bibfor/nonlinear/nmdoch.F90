subroutine nmdoch(list_load, l_load_user, list_load_resu)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liscad.h"
#include "asterfort/lisccr.h"
#include "asterfort/liscli.h"
#include "asterfort/lisexp.h"
#include "asterfort/lislfc.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    aster_logical, intent(in) :: l_load_user
    character(len=19), intent(in) :: list_load
    character(len=19), intent(in) :: list_load_resu
!
! --------------------------------------------------------------------------------------------------
!
! Mechanics - Read parameters
!
! Get loads information and create datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  list_load_resu : name of datastructure for list of loads from result datastructure
! In  l_load_user    : .true. if loads come from user (EXCIT)
! In  list_load      : name of datastructure for list of loads
!
! --------------------------------------------------------------------------------------------------
!
! --- NOMBRE MAXIMUM DE TYPE_INFO
    integer :: nb_info_maxi
    parameter   (nb_info_maxi=99)
    character(len=24) :: list_info_type(nb_info_maxi)
! --- NOMBRE MAXIMUM DE RESUELEM POUR LES FORCES DE LAPLACE : NBCHMX
    integer :: nbchmx
    parameter   (nbchmx=99)
! --- NOMBRE MAXIMUM DE TYPESD DE CHARGE                    : NBTYCH
    integer :: nbtych
    parameter    (nbtych=18)
    character(len=6) :: nomlig(nbtych)
!
    integer :: itych
    integer :: n1, nocc, iexc, iret2
    integer :: npilo, nexci, nchar, nchar1, nchar2, nchar3
    integer :: infmax, indic, i_load, iret, infc, j, i_load_new
    integer :: jlisdb, ichd
    character(len=5) :: suffix
    character(len=8) :: k8bid, affcha, parcha, typcha
    character(len=8) :: fctcsr
    character(len=16) :: nomcmd, typesd
    character(len=8) :: nomcha, nomfct, nomch1, nomfc1
    character(len=24) :: info_type
    character(len=19) :: list_load_new, lisdbl
    character(len=24) :: ligrch, lchin, k24bid
    integer :: i_neum_lapl
    aster_logical :: lfcplx, lacce, l_new_list
    integer :: nb_info_type
    integer, pointer :: infc2(:) => null()
    integer, pointer :: infch(:) => null()
    integer, pointer :: vinfc(:) => null()
    character(len=24), pointer :: lcha2(:) => null()
    character(len=24), pointer :: lcha(:) => null()
!
    data nomlig  /'.FORNO','.F3D3D','.F2D3D','.F1D3D',&
                  '.F2D2D','.F1D2D','.F1D1D','.PESAN',&
                  '.ROTAT','.PRESS','.FELEC','.FCO3D',&
                  '.FCO2D','.EPSIN','.FLUX' ,'.VEASS',&
                  '.ONDPL','.SIINT'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call getres(k8bid, typesd, nomcmd)
!
! --- INITIALISATIONS
!
    nchar = 0
    infmax = 0
    fctcsr = '&&NMDOME'
    lisdbl = '&&NMDOME.LISDBL'
    lfcplx = .false.
    lacce = .false.
    info_type = 'RIEN'
    npilo = 0
    indic = 0
    nexci = 0
    i_load_new = 0
    k24bid = ' '
    l_new_list = .false.
!
! --- NOMBRE DE CHARGES
!
    if (l_load_user) then
        if (getexm('EXCIT','CHARGE') .eq. 1) then
            call getfac('EXCIT', nexci)
        else
            nexci=0
        endif
        if (nexci .gt. 0) then
            do iexc = 1, nexci
                call getvid('EXCIT', 'CHARGE', iocc=iexc, scal=k24bid, nbret=nocc)
!
! --- GLUTE POUR LE CAS DEFI_CABLE_BP: MELANGE
! --- CINEMATIQUE/NEUMANN
!
                call jeexin(k24bid(1:8)//'.CHME.SIGIN.VALE', iret)
                if (iret .ne. 0) then
                    call jeexin(k24bid(1:8)//'.CHME.CIMPO.DESC', iret2)
                else
                    iret2 = 1
                endif
!
                if ((nocc.eq.1) .and. (iret2.ne.0)) then
                    nchar = nchar + 1
                endif
            end do
        else
! --- CAS OU LE CHARGEMENT PEUT NE PAS ETRE OBLIGATOIRE (DYNA_NON_LINE)
!     ON CREE UNE SD CHARGE CONTENANT 1 CHARGE FICTIVE
            if (nomcmd .eq. 'DYNA_NON_LINE') then
                call lisccr(list_load, 1, 'V')
                call jeveuo(list_load(1:19)//'.INFC', 'E', vi=infch)
                nchar=0
                infch(1) = nchar
            else if (nomcmd.eq.'STAT_NON_LINE') then
                call utmess('F', 'CHARGES_2')
            endif
        endif
    else
        call jeveuo(list_load_resu(1:19)//'.INFC', 'L', vi=vinfc)
        nchar = vinfc(1)
!
! ----- Create new list of loads fro CALC_CHAMP
!
        if (nomcmd .eq. 'CALC_CHAMP') then
            nchar1 = nchar
            nchar2 = 0
            call jeveuo(list_load_resu(1:19)//'.LCHA', 'L', vk24=lcha)
            do i_load = 1, nchar1
                nomcha = lcha(i_load)(1:8)
                if (nomcha .ne. ' ') then
                    call dismoi('TYPE_CHARGE', nomcha, 'CHARGE', repk=affcha)
                    if (affcha(1:5) .ne. 'CIME_') then
                        nchar2 = nchar2 + 1
                    endif
                endif
            end do
!
! --------- Create new list of loads without AFFE_CHAR_CINE loads
!
            if (nchar1 .ne. nchar2) then
                nchar3        = max(nchar2,1)
                list_load_new = '&&NMDOME.CHARGES'
                call lisccr(list_load_new, nchar3, 'V')
                i_load_new = 0
                do i_load = 1, nchar1
                    nb_info_type = 1
                    call liscli(list_load_resu, i_load      , nb_info_maxi, list_info_type, nomch1,&
                                nomfc1        , nb_info_type, i_neum_lapl)
                    if (list_info_type(1)(1:5) .ne. 'CINE_' .and. nomch1 .ne. ' ') then
                        i_load_new = i_load_new + 1
                        call liscad(list_load_new , i_load_new, nomch1, nomfc1, nb_info_type,&
                                    list_info_type, i_neum_laplz = i_neum_lapl)
                    endif
                end do
                nchar      = nchar2
                l_new_list = .true.
            endif
        endif
!
        if (l_new_list) then
            call jeveuo(list_load_new(1:19)//'.INFC', 'L', vi=infc2)
            call jeveuo(list_load_new(1:19)//'.LCHA', 'L', vk24=lcha2)
        else
            call jeveuo(list_load_resu(1:19)//'.INFC', 'L', vi=infc2)
            call jeveuo(list_load_resu(1:19)//'.LCHA', 'L', vk24=lcha2)
        endif
    endif
!
    i_load_new = 0
!
    if (nchar .ne. 0) then
!
! ----- CREATION LA SD L_CHARGES
!
        call lisccr(list_load, nchar, 'V')
!
! ----- LISTE DOUBLE
!
        call wkvect(lisdbl, 'V V K8', nchar, jlisdb)
!
! ----- BOUCLE SUR LES CHARGES
!
        do i_load = 1, nchar
            if (l_load_user) then
                indic = indic + 1
 30             continue
                call getvid('EXCIT', 'CHARGE', iocc=indic, nbval=0, nbret=n1)
                if (n1 .ne. 0) then
                    call getvid('EXCIT', 'CHARGE', iocc=indic, scal=nomcha, nbret=n1)
                    do ichd = 1, nchar
                        if (nomcha .eq. zk8(jlisdb+ichd-1)) then
                            call utmess('F', 'CHARGES_1', sk=nomcha)
                        endif
                    end do
                else
                    indic = indic + 1
                    goto 30
                endif
            else
                nomcha = lcha2(i_load)(1:8)
            endif
            zk8(jlisdb+i_load-1) = nomcha(1:8)
!
! ------- LIGREL DE LA CHARGE
!
            ligrch = nomcha(1:8)//'.CHME.LIGRE'
!
! ------- TYPE DE LA CHARGE
!
            if (l_load_user) then
                if (nomcmd .eq. 'DYNA_LINE_TRAN' .or. nomcmd .eq. 'DYNA_LINE_HARM') then
                    typcha='FIXE'
                else
                    call getvtx('EXCIT', 'TYPE_CHARGE', iocc=indic, scal=typcha, nbret=n1)
                endif
            else
                typcha = 'FIXE_CST'
                if (nomcmd .eq. 'CALC_CHAMP') then
                    if (infc2(i_load+1) .eq. 4 .or. infc2(1+nchar+i_load) .eq. 4) then
                        typcha = 'SUIV'
                        elseif (infc2(i_load+1).eq.5 .or. infc2(1+nchar+&
                    i_load).eq.5) then
                        typcha = 'FIXE_PIL'
                    else if (infc2(1+3*nchar+2+i_load).eq.1) then
                        typcha = 'DIDI'
                    endif
                endif
            endif
!
! ------- NOMBRE DE CHARGES PILOTEES
!
            if (typcha .eq. 'FIXE_PIL') then
                npilo = npilo + 1
            endif
!
! ------- CONTROLE DU CARACTERE MECANIQUE DE LA CHARGE
!
            call dismoi('TYPE_CHARGE', nomcha, 'CHARGE', repk=affcha)
            if ((affcha(1:5).ne.'MECA_') .and. (affcha(1:5) .ne.'CIME_')) then
                call utmess('F', 'CHARGES_22', sk=nomcha(1:8))
            endif
!
! ------- FONCTIONS MULTIPLICATIVES DES CHARGES
!
            lfcplx = (&
                     nomcmd .eq. 'DYNA_LINE_HARM' .or.&
                     ( nomcmd.eq.'LIRE_RESU' .and. typesd.eq.'DYNA_HARMO' )&
                     )
            lacce = (nomcmd.eq.'DYNA_NON_LINE'.or. nomcmd.eq.'LIRE_RESU')
            call lislfc(list_load_resu, i_load, indic, l_load_user, nexci,&
                        lfcplx, lacce, fctcsr, nomfct)
            if (nomfct .ne. fctcsr) then
                if (typcha .eq. 'FIXE_PIL') then
                    call utmess('F', 'CHARGES_38', sk=nomcha(1:8))
                endif
            endif
!
! ------- CHARGE DE TYPE DIRICHLET PROVENANT D'UN AFFE_CHAR_CINE
!
            nb_info_type = 0
            info_type = 'RIEN'
            if (affcha(1:5) .eq. 'CIME_') then
                if (typcha(1:4) .eq. 'SUIV') then
                    call utmess('F', 'CHARGES_23', sk=nomcha(1:8))
                else if (typcha.eq.'FIXE_PIL') then
                    call utmess('F', 'CHARGES_27', sk=nomcha(1:8))
                else if (typcha(1:4).eq.'DIDI') then
                    call utmess('F', 'CHARGES_24', sk=nomcha(1:8))
                else
                    if (affcha(5:7) .eq. '_FT') then
                        info_type = 'CINE_FT'
                    else if (affcha(5:7).eq.'_FO') then
                        info_type = 'CINE_FO'
                    else
                        info_type = 'CINE_CSTE'
                    endif
                endif
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! -------- CHARGE DE TYPE DIRICHLET PROVENANT DE AFFE_CHAR_MECA
!
            info_type = 'RIEN'
            lchin = ligrch(1:13)//'.CIMPO.DESC'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (typcha(1:4) .eq. 'SUIV') then
                    call utmess('F', 'CHARGES_23', sk=nomcha(1:8))
!
                else if (typcha.eq.'FIXE_PIL') then
                    call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=parcha)
                    if (parcha(1:3) .eq. 'OUI') then
                        call utmess('F', 'CHARGES_28', sk=nomcha(1:8))
                    endif
!
                    if (affcha(5:7) .eq. '_FT') then
                        call utmess('F', 'CHARGES_28', sk=nomcha(1:8))
                    else if (affcha(5:7).eq.'_FO') then
                        info_type = 'DIRI_PILO_F'
                    else
                        info_type = 'DIRI_PILO'
                    endif
                else
                    if (affcha(5:7) .eq. '_FO') then
                        info_type = 'DIRI_FO'
                        call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=parcha)
                        if (parcha(1:3) .eq. 'OUI') then
                            info_type = 'DIRI_FT'
                        endif
                    else
                        info_type = 'DIRI_CSTE'
                    endif
                    if (typcha(1:4) .eq. 'DIDI') then
                        info_type = info_type(1:9)//'_DIDI'
                    endif
                endif
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! ------- CHARGE DE TYPE NEUMANN
!
            do itych = 1, nbtych
                if (nomlig(itych) .eq. '.VEASS') then
                    suffix = '     '
                else
                    suffix = '.DESC'
                endif
                lchin = ligrch(1:13)//nomlig(itych)//suffix
                call jeexin(lchin, iret)
                info_type = 'RIEN'
                if (iret .ne. 0) then
                    if (nomlig(itych) .eq. '.ONDPL') then
                        info_type = 'NEUM_ONDE'
!
                    else if (nomlig(itych).eq.'.SIINT') then
                        info_type = 'NEUM_SIGM_INT'
!
                    else if (typcha.eq.'FIXE_PIL') then
                        info_type = 'NEUM_PILO'
                        if (nomlig(itych) .ne. '.VEASS') then
                            call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=parcha)
                            if (parcha(1:3) .eq. 'OUI') then
                                call utmess('F', 'CHARGES_28')
                            endif
                        endif
!
                    else if (typcha(1:4).eq.'SUIV') then
                        info_type = 'NEUM_SUIV'
!
                    else if (affcha(5:7).eq.'_FO') then
                        call dismoi('PARA_INST', lchin(1:19), 'CARTE', repk=parcha)
!
                        if (parcha(1:3) .eq. 'OUI') then
                            info_type = 'NEUM_FT'
                        else
                            info_type = 'NEUM_FO'
                        endif
                    else
                        info_type = 'NEUM_CSTE'
                    endif
                endif
                if (info_type .ne. 'RIEN') then
                    nb_info_type = nb_info_type + 1
                    ASSERT(nb_info_type.lt.nb_info_maxi)
                    list_info_type(nb_info_type) = info_type
                endif
            end do
!
! ------- CHARGE DE TYPE EVOL_CHAR
!
            info_type = 'RIEN'
            lchin = ligrch(1:13)//'.EVOL.CHAR'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (typcha(1:4) .eq. 'SUIV') then
                    info_type = 'NEUM_SUIV'
                else
                    info_type = 'NEUM_CSTE'
                endif
                if (typcha .eq. 'FIXE_PIL') then
                    call utmess('F', 'CHARGES_34', sk=nomcha(1:8))
                endif
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! ------- CHARGE DE TYPE EXCIT_SOL
!
            info_type = 'RIEN'
            lchin = ligrch(1:13)//'.VEISS'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (nomcmd .eq. 'STAT_NON_LINE') then
                    call utmess('F', 'CHARGES_50', sk=nomcha(1:8))
                endif
                if (typcha .eq. 'SUIV') then
                    call utmess('F', 'CHARGES_51', sk=nomcha(1:8))
                endif
                if (typcha .eq. 'DIDI') then
                    call utmess('F', 'CHARGES_52', sk=nomcha(1:8))
                endif
                if (affcha(5:6) .eq. '_F') then
                    call utmess('F', 'CHARGES_53', sk=nomcha(1:8))
                endif
                if (nomfct .ne. fctcsr) then
                    call utmess('F', 'CHARGES_54', sk=nomcha(1:8))
                endif
                info_type = 'EXCIT_SOL'
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! -------- CHARGES DE TYPE FORCE DE LAPLACE
!
            infc = 0
            info_type = 'RIEN'
            do j = 1, nbchmx
                lchin(1:17) = ligrch(1:13)//'.FL1'
                call codent(j, 'D0', lchin(18:19))
                lchin = lchin(1:19)//'.DESC'
                call jeexin(lchin, iret)
                if (iret .ne. 0) then
                    infc = infc + 1
                else
                    goto 90
                endif
            end do
 90         continue
            if (infc .ne. 0) then
                i_neum_lapl = max(infmax,infc)
                info_type = 'NEUM_LAPL'
            endif
            if (info_type .ne. 'RIEN') then
                nb_info_type = nb_info_type + 1
                ASSERT(nb_info_type.lt.nb_info_maxi)
                list_info_type(nb_info_type) = info_type
            endif
!
! --- AJOUT DE LA CHARGE
!
            if (nb_info_type .gt. 0) then
                i_load_new = i_load_new+1
                call liscad(list_load     , i_load_new, nomcha, nomfct, nb_info_type,&
                            list_info_type, i_neum_laplz = i_neum_lapl)
            endif
!
        end do
!
! ---- PILOTAGE POSSIBLE SI IL YA DES CHARGES PILOTEES !
!
        if (nomcmd .ne. 'LIRE_RESU') then
            if (nomcmd .eq. 'STAT_NON_LINE') then
                call getvtx('PILOTAGE', 'TYPE', iocc=1, scal=k24bid, nbret=n1)
                if (n1 .ne. 0 .and. npilo .eq. 0) then
                    call utmess('F', 'CHARGES_39')
                endif
                if (npilo .gt. 1) then
                    call utmess('F', 'CHARGES_40')
                endif
            endif
        endif
!
! ----- Some loads are prohibited with PILOTAGE
!
        if (npilo .ge. 1) then
            call lisexp(list_load)
        endif
!
    endif
    call jedetr(lisdbl)
    call jedema()
end subroutine
