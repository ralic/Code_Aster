subroutine chpchd(chin, type, celmod, prol0, base,&
                  chou)
    implicit  none
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterfort/assert.h'
    include 'asterfort/carces.h'
    include 'asterfort/celces.h'
    include 'asterfort/celfpg.h'
    include 'asterfort/cescel.h'
    include 'asterfort/cesces.h'
    include 'asterfort/cescns.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/cnsces.h'
    include 'asterfort/cnscno.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/manopg.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: chin, chou, base, celmod, type
!     -----------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: jacques.pellet at edf.fr
! -----------------------------------------------------------------
!  BUT : CHANGER LE SUPPORT GEOMETRIQUE D'UN CHAMP
! -----------------------------------------------------------------
!
! CHIN IN/JXIN  K19 : NOM DU CHAMP A CHANGER
!                     TYPE AUTORISE POUR CHIN :
!        NOEU, CART, ELGA, ELNO, CESE
!
! CHOU IN/JXOUT K19 : NOM DU CHAMP RESULTAT
! BASE IN       K1  : BASE DE CREATION DE CHOU : /'G' / 'V'
! TYPE IN       K19 : TYPE DE SUPPORT GEOMETRIQUE VOULU POUR CHOU
!                     /'NOEU' /'CART' /'ELNO' /ELGA' /'ELEM'
!
! ARGUMENTS UTILISES SI TYPE=ELNO/ELGA/ELEM :
!   PROL0 IN   K3  :
!        /'OUI' : LE CHAM_ELEM CHOU EST PROLONGE
!         PAR DES VALEURS NULLES LA OU IL N'EST PAS DEFINI.
!        /'NON' : ERREUR <F> SI IL EXISTE DES
!         DES VALEURS DE CHOU QUI NE SONT PAS AFFECTEES DANS CHIN
!   CELMOD IN/JXIN  K19 : NOM D'UN CHAM_ELEM "MODELE" SI TYPE='EL..'
!
!  LES CAS TRAITES AUJOURD'HUI SONT :
!
!         /'NOEU->ELNO'   : CHAM_NO -> CHAM_ELEM/ELNO
!         /'NOEU->ELGA'   : CHAM_NO -> CHAM_ELEM/ELGA
!
!         /'CART->ELEM'   : CARTE   -> CHAM_ELEM/ELEM
!         /'CART->ELGA'   : CARTE   -> CHAM_ELEM/ELGA
!         /'CART->ELNO'   : CARTE   -> CHAM_ELEM/ELNO
!         /'CART->NOEU'   : CARTE   -> CHAM_NO
!
!         /'ELGA->NOEU'   : CHAM_ELEM/ELGA    -> CHAM_NO
!         /'ELGA->ELNO'   : CHAM_ELEM/ELGA    -> CHAM_ELEM/ELNO
!         /'ELNO->NOEU'   : CHAM_ELEM/ELNO    -> CHAM_NO
!         /'ELNO->ELGA'   : CHAM_ELEM/ELNO    -> CHAM_ELEM/ELGA
!
!         /'CESE->ELNO'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELNO
!         /'CESE->ELGA'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELGA
!         /'CESE->ELEM'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELEM
! -----------------------------------------------------------------
!
    integer :: ib, iret, nncp, ibid
    character(len=3) :: prol0
    character(len=8) :: ma, ma2, tychi, nomgd, param, moin
    character(len=16) :: cas, option, nomcmd, kbid
    character(len=19) :: cesmod, ces1, cns1, mnoga, ligrel, ces2
    character(len=24) :: valk(4)
!
!     ------------------------------------------------------------------
    mnoga = '&&CHPCHD.MANOGA'
    cesmod = '&&CHPCHD.CESMOD'
!
!
! 1- CALCUL DE:
!      MA    : MAILLAGE ASSOCIE A CHIN
!      TYCHI : TYPE DU CHAMP CHIN (CART/NOEU/ELNO/ELGA/CESE)
!      NOMGD : NOM DE LA GRANDEUR ASSOCIEE A CHIN
! ------------------------------------------------------------------
!
    call dismoi('F', 'NOM_MAILLA', chin, 'CHAMP', ib,&
                ma, ib)
    call dismoi('F', 'TYPE_CHAMP', chin, 'CHAMP', ib,&
                tychi, ib)
    call dismoi('F', 'NOM_GD', chin, 'CHAMP', ib,&
                nomgd, ib)
    call assert(tychi .eq. 'NOEU' .or. tychi&
                .eq. 'CART' .or. tychi .eq.&
                'ELNO' .or. tychi .eq. 'ELGA'&
                .or. tychi .eq. 'CESE')
!
!
! 2.  -- SI TYPE = 'EL..' : ON CREE UN CHAM_ELEM_S "MODELE" : CESMOD
!         LIGREL: NOM DU LIGREL ASSOCIE A CHOU
! ---------------------------------------------------------------
    if (type(1:2) .eq. 'EL') then
        call assert(celmod.ne.' ')
        call dismoi('F', 'NOM_LIGREL', celmod, 'CHAM_ELEM', ib,&
                    ligrel, ib)
        call dismoi('F', 'NOM_OPTION', celmod, 'CHAM_ELEM', ib,&
                    option, ib)
        call dismoi('F', 'NOM_PARAM', celmod, 'CHAM_ELEM', ib,&
                    param, ib)
        call dismoi('F', 'NOM_MAILLA', ligrel, 'LIGREL', ib,&
                    ma2, ib)
        if (ma .ne. ma2) then
            call dismoi('F', 'NOM_MODELE', ligrel, 'LIGREL', ib,&
                        moin, ib)
            valk(1) = chin
            valk(2) = moin
            valk(3) = ma
            valk(4) = ma2
            call u2mesk('F', 'CALCULEL4_59', 4, valk)
        endif
        call celces(celmod, 'V', cesmod)
    endif
!
!
! 3.  -- CALCUL DE CAS :
! ---------------------------------------
!     SONT TRAITES AUJOURD'HUI :
!
!         /'NOEU->ELNO'   : CHAM_NO -> CHAM_ELEM/ELNO
!         /'NOEU->ELGA'   : CHAM_NO -> CHAM_ELEM/ELGA
!
!         /'CART->ELEM'   : CARTE   -> CHAM_ELEM/ELEM
!         /'CART->ELGA'   : CARTE   -> CHAM_ELEM/ELGA
!         /'CART->ELNO'   : CARTE   -> CHAM_ELEM/ELNO
!         /'CART->NOEU'   : CARTE   -> CHAM_NO
!
!         /'ELGA->NOEU'   : CHAM_ELEM/ELGA    -> CHAM_NO
!         /'ELGA->ELNO'   : CHAM_ELEM/ELGA    -> CHAM_ELEM/ELNO
!         /'ELNO->NOEU'   : CHAM_ELEM/ELNO    -> CHAM_NO
!         /'ELNO->ELGA'   : CHAM_ELEM/ELNO    -> CHAM_ELEM/ELGA
!
!         /'CESE->ELNO'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELNO
!         /'CESE->ELGA'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELGA
!         /'CESE->ELEM'   : CHAM_ELEM_S/ELEM  -> CHAM_ELEM/ELEM
!
    cas = ' '
    cas(1:4) = tychi
    cas(5:6) = '->'
    cas(7:10) = type
!
!
! 4.  TRAITEMENT DES DIFFERENTS CAS DE FIGURE :
! ----------------------------------------------
    nncp = 0
    if (cas .eq. 'NOEU->ELGA') then
!     ----------------------------------
        cns1 = '&&CHPCHD.CNS1'
        ces1 = '&&CHPCHD.CES1'
        call manopg(ligrel, option, param, mnoga)
!
        call cnocns(chin, 'V', cns1)
        call cnsces(cns1, 'ELGA', cesmod, mnoga, 'V',&
                    ces1)
        call detrsd('CHAM_NO_S', cns1)
        call detrsd('CHAM_ELEM_S', mnoga)
!
        call cescel(ces1, ligrel, option, param, prol0,&
                    nncp, base, chou, 'F', ibid)
        call detrsd('CHAM_ELEM_S', ces1)
!
!
    else if (cas.eq.'ELNO->ELGA') then
!     ----------------------------------
        ces1 = '&&CHPCHD.CES1'
        ces2 = '&&CHPCHD.CES2'
        call manopg(ligrel, option, param, mnoga)
!
        call celces(chin, 'V', ces1)
        call cesces(ces1, 'ELGA', cesmod, mnoga, ' ',&
                    'V', ces2)
        call detrsd('CHAM_ELEM_S', ces1)
        call detrsd('CHAM_ELEM_S', mnoga)
!
        call cescel(ces2, ligrel, option, param, prol0,&
                    nncp, base, chou, 'F', ibid)
        call detrsd('CHAM_ELEM_S', ces2)
!
!
    else if (cas.eq.'NOEU->ELNO') then
!     ----------------------------------------------------------------
        cns1 = '&&CHPCHD.CNS1'
        ces1 = '&&CHPCHD.CES1'
!
        call cnocns(chin, 'V', cns1)
        call cnsces(cns1, 'ELNO', cesmod, ' ', 'V',&
                    ces1)
        call detrsd('CHAM_NO_S', cns1)
!
        call cescel(ces1, ligrel, option, param, prol0,&
                    nncp, base, chou, 'F', ibid)
        call detrsd('CHAM_ELEM_S', ces1)
!
!
        elseif ((cas.eq.'ELNO->NOEU') .or. (cas.eq.'ELGA->NOEU') .or.&
    (cas.eq.'CART->NOEU')) then
!     ----------------------------------------------------------------
        cns1 = '&&CHPCHD.CNS1'
        ces1 = '&&CHPCHD.CES1'
!
        if (cas(1:4) .eq. 'ELNO') then
            call celces(chin, 'V', ces1)
!
        else if (cas(1:4).eq.'ELGA') then
            call celces(chin, 'V', ces1)
            call celfpg(chin, '&&CHPCHD.CELFPG', iret)
!
        else if (cas(1:4).eq.'CART') then
            call carces(chin, 'ELNO', ' ', 'V', ces1,&
                        'A', iret)
!
        else
            call assert(.false.)
        endif
!
        call cescns(ces1, '&&CHPCHD.CELFPG', 'V', cns1, ' ',&
                    iret)
        call cnscno(cns1, ' ', 'NON', base, chou,&
                    'F', ibid)
!
        call detrsd('CHAM_NO_S', cns1)
        call detrsd('CHAM_ELEM_S', ces1)
        call jedetr('&&CHPCHD.CELFPG')
!
!
    else if (cas(1:8).eq.'CART->EL') then
!     ----------------------------------------------------------------
        call assert(ligrel.ne.' ')
        ces1 = '&&CHPCHD.CES1'
        call carces(chin, cas(7:10), cesmod, 'V', ces1,&
                    'A', ib)
!
        call cescel(ces1, ligrel, option, param, prol0,&
                    nncp, base, chou, 'F', ibid)
        call detrsd('CHAM_ELEM_S', ces1)
!
        if (nncp .ne. 0) then
            call getres(kbid, kbid, nomcmd)
            if (nomcmd .eq. 'CREA_CHAMP') then
                valk(1) = chou(1:8)
                valk(2) = option
                valk(3) = param
                call u2mesk('A', 'CALCULEL6_77', 3, valk)
            endif
        endif
!
!
    else if (cas(1:8).eq.'CESE->EL') then
!     ----------------------------------------------------------------
        call assert(ligrel.ne.' ')
        ces1 = '&&CHPCHD.CES1'
        call cesces(chin, cas(7:10), cesmod, ' ', ' ',&
                    'V', ces1)
        call cescel(ces1, ligrel, option, param, prol0,&
                    nncp, base, chou, 'F', ibid)
        call detrsd('CHAM_ELEM_S', ces1)
!
        if (nncp .ne. 0) then
            call getres(kbid, kbid, nomcmd)
            if (nomcmd .eq. 'CREA_CHAMP') then
                valk(1) = chou(1:8)
                valk(2) = option
                valk(3) = param
                call u2mesk('A', 'CALCULEL6_77', 3, valk)
            endif
        endif
!
!
    else if (cas.eq.'ELGA->ELNO') then
!     ----------------------------------------------------------------
        ces1 = '&&CHPCHD.CES1'
        ces2 = '&&CHPCHD.CES2'
!
        call celces(chin, 'V', ces1)
        call celfpg(chin, '&&CHPCHD.CELFPG', iret)
        call assert(iret.eq.0)
        call cesces(ces1, 'ELNO', cesmod, ' ', '&&CHPCHD.CELFPG',&
                    'V', ces2)
        call cescel(ces2, ligrel, option, param, prol0,&
                    nncp, base, chou, 'F', ibid)
!
        call detrsd('CHAM_ELEM_S', ces1)
        call detrsd('CHAM_ELEM_S', ces2)
        call jedetr('&&CHPCHD.CELFPG')
!
!
    else if (cas.eq.'NOEU->ELEM') then
!     ----------------------------------------------------------------
        cns1 = '&&CHPCHD.CNS1'
        ces1 = '&&CHPCHD.CES1'
!
        call cnocns(chin, 'V', cns1)
        call cnsces(cns1, 'ELEM', cesmod, ' ', 'V',&
                    ces1)
        call detrsd('CHAM_NO_S', cns1)
!
        call cescel(ces1, ligrel, option, param, prol0,&
                    nncp, base, chou, 'F', ibid)
        call detrsd('CHAM_ELEM_S', ces1)
!
!
    else
!       CAS NON ENCORE PROGRAMME
        call u2mesk('F', 'CALCULEL_5', 1, cas)
    endif
!
!
!
!
!     -- MENAGE :
!     ------------
    if (type(1:2) .eq. 'EL') call detrsd('CHAM_ELEM_S', cesmod)
!
end subroutine
