subroutine nmdoct(lischa, defico, deficu, lcont, lunil,&
                  ligrcf, ligrxf)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/focste.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/liscad.h'
    include 'asterfort/lisccr.h'
    include 'asterfort/liscli.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: defico
    character(len=24) :: deficu
    character(len=19) :: lischa
    logical :: lcont, lunil
    character(len=19) :: ligrcf, ligrxf
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE
!
! SAISIE ET VERIFICATION DE LA COHERENCE DU CHARGEMENT CONTACT
!
! ----------------------------------------------------------------------
!
!
! OUT DEFICO : NOM DE LA SD DEFINITION DU CONTACT
! OUT DEFICU : NOM DE LA SD DEFINITION DE LIAISON_UNILATER
! I/O LISCHA : LISTE DES CHARGES
! OUT LCONT  : IL Y A DU CONTACT
! OUT LUNIL  : IL Y A LIAISON_UNILATER
! OUT LIGRCF : NOM DU LIGREL TARDIF POUR ELEMENTS DE CONTACT CONTINUE
! OUT LIGRXF : NOM DU LIGREL TARDIF POUR ELEMENTS DE CONTACT XFEM GG
!
!
!
!
    character(len=16) :: motcle
    integer :: nocc, nchar1, nchar2, ich, ier, iatype
    character(len=8) :: charco
    integer :: iform, ival1, ival2
    character(len=8) :: ligret, ligrel, ligrxt
    character(len=19) :: lisch2
    character(len=24) :: infch1
    integer :: jinfc1
    character(len=8) :: nomch1, nomfc1, fctcst
    character(len=24) :: infoc2
    real(kind=8) :: coef
    logical :: ltfcm
! --- NOMBRE MAXIMUM DE TYPE_INFO
    integer :: nbinmx, nbinfo
    parameter   (nbinmx=99)
    character(len=24) :: lisinf(nbinmx)
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    motcle = 'CONTACT'
    lisch2 = '&&NMDOCT.LISCHA'
    ligrcf = ' '
    ligrxf = ' '
    lcont = .false.
    lunil = .false.
!
! --- ANCIENNE SD L_CHARGES
!
    infch1 = lischa(1:19)//'.INFC'
    call jeveuo(infch1, 'L', jinfc1)
    nchar1 = zi(jinfc1)
    nchar2 = nchar1
!
! --- FONCTION CONSTANTE
!
    fctcst = '&&NMDOCT'
    coef = 1.d0
    call focste(fctcst, 'TOUTRESU', coef, 'V')
!
! --- RECUPERATION DU NOM DU CHARGEMENT DE CONTACT
!
    call getvid(' ', motcle, 1, iarg, 1,&
                charco, nocc)
    if (nocc .le. 0) then
        defico = '&&OP0070.DEFIC'
        goto 999
    endif
!
! --- NOM DE LA SD DE DEFINITION DU CONTACT
!
    defico = charco(1:8)//'.CONTACT'
    deficu = charco(1:8)//'.UNILATE'
!
! --- FORMULATION
!
    iform = cfdisi(defico,'FORMULATION')
    if (iform .eq. 4) then
        lunil = .true.
    else
        lcont = .true.
    endif
    ltfcm = cfdisl(defico,'CONT_XFEM_GG')
!
! --- LIGREL ELEMENTS TARDIFS - METHODE CONTINUE
!
    if (iform .eq. 2) then
        ligret = charco(1:8)
        ligrcf = '&&LIGRCF.CHME.LIGRE'
        call wkvect(ligrcf(1:8)//'.TYPE', 'V V K8', 1, iatype)
        zk8(iatype) = 'ME'
        nchar2 = nchar2+2
    endif
!
! --- LIGREL ELEMENTS TARDIFS - XFEM GRANDS GLISSEMENTS
!
    if (ltfcm) then
        ligrxt = charco(1:8)
        ligrxf = '&&LIGRXF.CHME.LIGRE'
        call wkvect(ligrxf(1:8)//'.TYPE', 'V V K8', 1, iatype)
        zk8(iatype) = 'ME'
        nchar2 = nchar2+2
    endif
!
! --- EVENTUELLES RELATIONS LINEAIRES - METHODES DISCRETES
!
    if (iform .eq. 1) then
        ligrel = charco(1:8)
        call jeexin(ligrel//'.CHME.LIGRE.LGRF', ier)
        if (ier .ne. 0) then
            nchar2 = nchar2+1
        endif
    endif
!
    if (nchar2 .ne. nchar1) then
        call lisccr(lisch2, nchar2, 'V')
!
        do 24 ich = 1, nchar1
            nbinfo = nbinmx
            call liscli(lischa, ich, nomch1, nomfc1, nbinfo,&
                        lisinf, ival1)
            call liscad(lisch2, ich, nomch1, nomfc1, nbinfo,&
                        lisinf, ival1)
24      continue
!
        if (iform .eq. 2) then
            infoc2 = 'ELEM_TARDIF'
            ival2 = 0
            nbinfo = 1
            call liscad(lisch2, nchar1+1, ligret, fctcst, nbinfo,&
                        infoc2, ival2)
!
            call liscad(lisch2, nchar1+2, ligrcf(1:8), fctcst, nbinfo,&
                        infoc2, ival2)
        endif
!
        if (iform .eq. 1) then
            if (ier .ne. 0) then
                infoc2 = 'DIRI_CSTE'
                ival2 = 0
                nbinfo = 1
                call liscad(lisch2, nchar1+1, ligrel, fctcst, nbinfo,&
                            infoc2, ival2)
            endif
        endif
!
        if (iform .eq. 3) then
            if (ltfcm) then
                infoc2 = 'ELEM_TARDIF'
                ival2 = 0
                nbinfo = 1
                call liscad(lisch2, nchar1+1, ligrxt, fctcst, nbinfo,&
                            infoc2, ival2)
                call liscad(lisch2, nchar1+2, ligrxf(1:8), fctcst, nbinfo,&
                            infoc2, ival2)
            endif
        endif
!
!
        call lisccr(lischa, nchar2, 'V')
        call copisd(' ', 'V', lisch2, lischa)
        call detrsd('LISTE_CHARGES', lisch2)
!
    endif
!
999  continue
!
    call jedema()
end subroutine
