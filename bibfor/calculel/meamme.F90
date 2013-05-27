subroutine meamme(optioz, modele, nchar, lchar, mate,&
                  cara, exitim, time, base, merigi,&
                  memass, meamor)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/calcul.h'
    include 'asterfort/codent.h'
    include 'asterfort/dbgcal.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/inical.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecham.h'
    include 'asterfort/memare.h'
    include 'asterfort/reajre.h'
    include 'asterfort/redetr.h'
    include 'asterfort/vrcins.h'
    integer :: nchar
    real(kind=8) :: time
    character(len=*) :: modele, optioz, cara, mate
    character(len=*) :: merigi, memass, meamor
    character(len=8) :: lchar(*)
    logical :: exitim
    character(len=1) :: base
!
! ----------------------------------------------------------------------
!
! CALCUL DES MATRICES ELEMENTAIRES D'AMOR_MECA
! OU DES MATRICES ELEMENTAIRES DE RIGI_MECA_HYST
!
! ----------------------------------------------------------------------
!
!
! IN  OPTION : 'AMOR_MECA' OU 'RIGI_MECA_HYST'
! IN  MODELE : NOM DU MODELE
! IN  NCHAR  : NOMBRE DE CHARGES
! IN  LCHAR  : LISTE DES CHARGES
! IN  MATE   : CHAM_MATER
! IN  CARA   : CARA_ELEM
! OUT MEAMOR : MATR_ELEM AMORTISSEMENT
! IN  EXITIM : VRAI SI L'INSTANT EST DONNE
! IN  TIME   : INSTANT DE CALCUL
! IN  MERIGI : MATR_ELEM_DEPL_R DE RIGI_MECA
! IN  MEMASS : MATR_ELEM_DEPL_R DE MASS_MECA
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=3, nbin=12)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: icode, iret, ibid, i, ied, icha
    integer :: ialir1, ilires, iarefe
    integer :: nh, nop
    integer :: nbres1
    character(len=2) :: codret
    character(len=8) :: k8b, nomgd
    character(len=19) :: ligre1, chvarc
    character(len=24) :: rigich, massch, ligrmo, ligrch
    character(len=24) :: chgeom, chcara(18), chharm, argu
    character(len=16) :: option
    logical :: debug
    integer :: ifmdbg, nivdbg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    if (modele(1:1) .eq. ' ') then
        call assert(.false.)
    endif
    option = optioz
    ligrmo = modele(1:8)//'.MODELE'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- CREATION DES CHAMPS DE GEOMETRIE, CARA_ELEM ET FOURIER
!
    nh = 0
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, icode)
!
! --- CREATION CHAMP DE VARIABLES DE COMMANDE CORRESPONDANT
!
    chvarc = '&&MEAMME.CHVARC'
    call vrcins(modele, mate, cara, time, chvarc,&
                codret)
!
! --- NOM DES RESUELEM DE RIGIDITE
!
    rigich = ' '
    if (merigi(1:1) .ne. ' ') then
        call jeexin(merigi(1:19)//'.RELR', iret)
        if (iret .gt. 0) then
            call jeveuo(merigi(1:19)//'.RELR', 'L', ialir1)
            call jelira(merigi(1:19)//'.RELR', 'LONUTI', nbres1, k8b)
            do 10 i = 1, nbres1
                rigich = zk24(ialir1-1+i)
                call dismoi('F', 'NOM_LIGREL', rigich(1:19), 'RESUELEM', ibid,&
                            ligre1, ied)
                if (ligre1(1:8) .eq. modele(1:8)) goto 20
10          continue
            call assert(.false.)
20          continue
!
!
!
        endif
    endif
!
! --- NOM DES RESUELEM DE MASSE
!
    massch = ' '
    if (memass(1:1) .ne. ' ') then
        call jeexin(memass(1:19)//'.RELR', iret)
        if (iret .gt. 0) then
            call jeveuo(memass(1:19)//'.RELR', 'L', ialir1)
            call jelira(memass(1:19)//'.RELR', 'LONUTI', nbres1, k8b)
            do 30 i = 1, nbres1
                massch = zk24(ialir1-1+i)
                call dismoi('F', 'NOM_LIGREL', massch(1:19), 'RESUELEM', ibid,&
                            ligre1, ied)
                if (ligre1(1:8) .eq. modele(1:8)) goto 40
30          continue
            call assert(.false.)
40          continue
        endif
    endif
!
! --- CREATION DU .RERR DES MATR_ELEM D'AMORTISSEMENT
!
    call jeexin(meamor(1:19)//'.RERR', iret)
    if (iret .gt. 0) then
        call jedetr(meamor(1:19)//'.RERR')
        call jedetr(meamor(1:19)//'.RELR')
    endif
    call memare(base, meamor(1:19), modele(1:8), mate, cara(1:8),&
                'AMOR_MECA')
!     SI LA MATRICE EST CALCULEE SUR LE MODELE, ON ACTIVE LES S_STRUC:
    call jeveuo(meamor(1:19)//'.RERR', 'E', iarefe)
    zk24(iarefe-1+3) (1:3) = 'OUI'
!
! --- REMPLISSAGE DES CHAMPS D'ENTREE
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PCAORIE'
    lchin(3) = chcara(1)
    lpain(4) = 'PCADISA'
    lchin(4) = chcara(4)
    lpain(5) = 'PCAGNPO'
    lchin(5) = chcara(6)
    lpain(6) = 'PCACOQU'
    lchin(6) = chcara(7)
    lpain(7) = 'PVARCPR'
    lchin(7) = chvarc
    lpain(8) = 'PRIGIEL'
    lchin(8) = rigich
!
    if (rigich .ne. ' ') then
        call dismoi('F', 'NOM_GD', rigich, 'RESUELEM', ibid,&
                    nomgd, ied)
        if (nomgd .eq. 'MDNS_R') lpain(8) = 'PRIGINS'
    endif
!
    lpain(9) = 'PMASSEL'
    lchin(9) = massch
    lpain(10) = 'PCADISK'
    lchin(10) = chcara(2)
    lpain(11) = 'PCINFDI'
    lchin(11) = chcara(15)
!
! --- REMPLISSAGE DES CHAMPS DE SORTIE
!
    if (option(1:9) .eq. 'AMOR_MECA') then
        lpaout(1) = 'PMATUUR'
        lpaout(2) = 'PMATUNS'
    else if (option.eq.'RIGI_MECA_HYST') then
        lpaout(1) = 'PMATUUC'
    else
        call assert(.false.)
    endif
    lpaout(3) = 'PMATUUR'
    lchout(1) = meamor(1:8)//'.ME001'
    lchout(2) = meamor(1:8)//'.ME002'
    lchout(3) = meamor(1:8)//'.ME003'
!
! --- APPEL A CALCUL
!
!       IF (OPTION.EQ.'AMOR_MECA_ABSO') THEN
!         NOP = 3
!       ELSE
!         NOP = 11
!       ENDIF
    nop = 11
    call calcul('S', option, ligrmo, nop, lchin,&
                lpain, 2, lchout, lpaout, base,&
                'OUI')
!
    if (debug) then
        call dbgcal(option, ifmdbg, nop, lpain, lchin,&
                    2, lpaout, lchout)
    endif
!
    call reajre(meamor, lchout(1), base)
    call reajre(meamor, lchout(2), base)
!
! --- PRISE EN COMPTE DES MATRICES DE BLOCAGE DANS LE CAS D'UNE
! --- RIGIDITE HYSTERETIQUE :
!
    if (option .eq. 'RIGI_MECA_HYST') then
        do 50 icha = 1, nchar
            ligrch = lchar(icha) (1:8)//'.CHME.LIGRE'
            argu = lchar(icha) (1:8)//'.CHME.LIGRE.LIEL'
            call jeexin(argu, iret)
            if (iret .le. 0) goto 50
            lchin(1) = lchar(icha) (1:8)//'.CHME.CMULT'
            argu = lchar(icha) (1:8)//'.CHME.CMULT.DESC'
            call jeexin(argu, iret)
            if (iret .le. 0) goto 50
!
            lpain(1) = 'PDDLMUR'
            call jelira(meamor(1:19)//'.RELR', 'LONUTI', ilires, k8b)
            call codent(ilires+1, 'D0', lchout(3) (12:14))
            option = 'MECA_DDLM_R'
            call calcul('S', option, ligrch, 1, lchin,&
                        lpain, 1, lchout(3), lpaout(3), base,&
                        'OUI')
            call reajre(meamor, lchout(3), base)
50      continue
    endif
!
!
!     -- DESTRUCTION DES RESUELEM NULS :
    call redetr(meamor)
!
    call detrsd('CHAMP_GD', chvarc)
!
    call jedema()
end subroutine
