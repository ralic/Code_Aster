subroutine rbph01(trange, nbcham, typea, itresu, nfonct,&
                  basemo, typref, typbas, tousno, multap)
    implicit   none
    include 'asterc/getvtx.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: nbcham, itresu(*), nfonct
    character(len=8) :: basemo
    character(len=16) :: typea(*), typbas(*)
    character(len=19) :: trange, typref(*)
    logical :: tousno, multap
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
!     OPERATEUR REST_BASE_PHYS
!               TRAITEMENT DES MOTS CLES "TOUT_CHAM" ET "NOM_CHAM"
!     ------------------------------------------------------------------
    integer :: n1, i, iret
    character(len=8) :: blanc, mode
    character(len=16) :: champ(8)
    character(len=19) :: nomcha
    integer :: iarg
!     ------------------------------------------------------------------
    data blanc    /'        '/
!     ------------------------------------------------------------------
!
    mode = basemo
!
    champ(1)=' '
    call getvtx(' ', 'TOUT_CHAM', 1, iarg, 1,&
                champ, n1)
!
    if (champ(1) .eq. 'OUI') then
        nbcham = 3
        typea(1) = 'DEPL            '
        typea(2) = 'VITE            '
        typea(3) = 'ACCE            '
        call jeexin(trange//'.DEPL', iret)
        if (iret .eq. 0) then
            call u2mess('F', 'ALGORITH10_11')
        else
            call jeveuo(trange//'.DEPL', 'L', itresu(1))
!
        endif
!
        call jeexin(trange//'.VITE', iret)
        if (iret .eq. 0) then
            call u2mess('F', 'ALGORITH10_12')
        else
            call jeveuo(trange//'.VITE', 'L', itresu(2))
        endif
        call jeexin(trange//'.ACCE', iret)
        if (iret .eq. 0) then
            call u2mess('F', 'ALGORITH10_13')
        else
            call jeveuo(trange//'.ACCE', 'L', itresu(3))
        endif
        if (nfonct .ne. 0) then
            nbcham = 4
            typea(4) = 'ACCE_ABSOLU     '
            itresu(4) = itresu(3)
        endif
        if (mode .eq. blanc) then
            typref(1) = ' '
            typref(2) = ' '
            typref(3) = ' '
            typref(4) = ' '
        else
            call rsexch(' ', basemo, 'DEPL', 1, nomcha,&
                        iret)
            typref(1) = nomcha
            typref(2) = nomcha
            typref(3) = nomcha
            typref(4) = nomcha
        endif
        typbas(1) = 'DEPL'
        typbas(2) = 'DEPL'
        typbas(3) = 'DEPL'
        typbas(4) = 'DEPL'
!
    else
!
        call getvtx(' ', 'NOM_CHAM', 1, iarg, 0,&
                    champ, n1)
        nbcham = -n1
        call getvtx(' ', 'NOM_CHAM', 1, iarg, nbcham,&
                    champ, n1)
!
        do 10 i = 1, nbcham
            if (champ(i) .eq. 'DEPL') then
                typea(i) = 'DEPL'
                call jeexin(trange//'.DEPL', iret)
                if (iret .eq. 0) then
                    call u2mess('F', 'ALGORITH10_11')
                else
                    call jeveuo(trange//'.DEPL', 'L', itresu(i))
                endif
                if (mode .eq. blanc) then
                    typref(i) = ' '
                else
                    call rsexch(' ', basemo, typea(i), 1, nomcha,&
                                iret)
                    typref(i) = nomcha
                endif
                typbas(i) = 'DEPL'
!
            else if (champ(i) .eq. 'VITE') then
                typea(i) = 'VITE'
                call jeexin(trange//'.VITE', iret)
                if (iret .eq. 0) then
                    call u2mess('F', 'ALGORITH10_12')
                else
                    call jeveuo(trange//'.VITE', 'L', itresu(i))
                endif
                if (mode .eq. blanc) then
                    typref(i) = ' '
                else
                    call rsexch(' ', basemo, 'DEPL', 1, nomcha,&
                                iret)
                    typref(i) = nomcha
                endif
                typbas(i) = 'DEPL'
!
            else if (champ(i) .eq. 'ACCE') then
                typea(i) = 'ACCE'
                call jeexin(trange//'.ACCE', iret)
                if (iret .eq. 0) then
                    call u2mess('F', 'ALGORITH10_13')
                else
                    call jeveuo(trange//'.ACCE', 'L', itresu(i))
                endif
                if (mode .eq. blanc) then
                    typref(i) = ' '
                else
                    call rsexch(' ', basemo, 'DEPL', 1, nomcha,&
                                iret)
                    typref(i) = nomcha
                endif
                typbas(i) = 'DEPL'
!
            else if (champ(i) .eq. 'ACCE_ABSOLU') then
                typea(i) = 'ACCE_ABSOLU'
                call jeexin(trange//'.ACCE', iret)
                if (iret .eq. 0) then
                    call u2mess('F', 'ALGORITH10_13')
                else
                    call jeveuo(trange//'.ACCE', 'L', itresu(i))
                endif
                if (mode .eq. blanc) then
                    typref(i) = ' '
                else
                    call rsexch(' ', basemo, 'DEPL', 1, nomcha,&
                                iret)
                    typref(i) = nomcha
                endif
                typbas(i) = 'DEPL'
!
                elseif ( champ(i) .eq. 'FORC_NODA' .or. champ(i) .eq.&
            'REAC_NODA' ) then
                typea(i) = champ(i)
                call jeexin(trange//'.DEPL', iret)
                if (iret .eq. 0) then
                    call u2mess('F', 'ALGORITH10_11')
                else
                    call jeveuo(trange//'.DEPL', 'L', itresu(i))
                endif
                if (multap) then
                    call u2mess('F', 'ALGORITH10_14')
                endif
                if (mode .eq. blanc) then
                    call u2mess('F', 'ALGORITH10_15')
                else
                    call rsexch('F', basemo, typea(i), 1, nomcha,&
                                iret)
                    typref(i) = nomcha
                endif
                typbas(i) = typea(i)
!
            else
                typea(i) = champ(i)
                call jeexin(trange//'.DEPL', iret)
                if (iret .eq. 0) then
                    call u2mess('F', 'ALGORITH10_11')
                else
                    call jeveuo(trange//'.DEPL', 'L', itresu(i))
                endif
                if (.not. tousno) then
                    call u2mesk('F', 'ALGORITH10_17', 1, typea(i))
                endif
                if (multap) then
                    call u2mess('F', 'ALGORITH10_14')
                endif
                if (mode .eq. blanc) then
                    call u2mess('F', 'ALGORITH10_15')
                else
                    call rsexch('F', basemo, typea(i), 1, nomcha,&
                                iret)
                    typref(i) = nomcha
                endif
                typbas(i) = typea(i)
!
            endif
10      continue
    endif
!
end subroutine
