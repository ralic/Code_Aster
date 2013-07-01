subroutine lisimp(lischa, ifm)
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
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/isdeco.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/liscpp.h'
    include 'asterfort/lisdef.h'
    include 'asterfort/lislch.h'
    include 'asterfort/lislco.h'
    include 'asterfort/lisllc.h'
    include 'asterfort/lislnf.h'
    include 'asterfort/lislta.h'
    include 'asterfort/lisltc.h'
    include 'asterfort/lisltf.h'
    include 'asterfort/lisnnb.h'
    character(len=19) :: lischa
    integer :: ifm
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! IMPRESSION DU CONTENU DE LA SD LISTE_CHARGES
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : NOM DE LA SD LISTE_CHARGES
! IN  IFM    : NUMERO UNITE LOGIQUE IMPRESSION
!
! ----------------------------------------------------------------------
!
    integer :: ichar, nbchar, ibid
    character(len=8) :: charge, typech, nomfct, k8bid
    character(len=16) :: typapp, typfct
    integer :: genrec, tabcod(30)
    character(len=24) :: lisgen, nomlis, gencha
    integer :: jlisg, nbgenr, igenr, iposit
    character(len=13) :: prefob
    real(kind=8) :: phase
    integer :: npuis
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call lisnnb(lischa, nbchar)
    if (nbchar .eq. 0) then
        write(ifm,*) '<LISCHA> PAS DE CHARGES'
        goto 99
    else
        write(ifm,*) '<LISCHA> NOMBRE DE CHARGES: ',nbchar
    endif
!
! --- LISTE DES GENRES DISPONIBLES
!
    lisgen = '&&LISIMP.LISGEN'
    call lisdef('LISG', lisgen, ibid, k8bid, nbgenr)
    call jeveuo(lisgen, 'L', jlisg)
!
! --- AFFICHAGE
!
    do 10 ichar = 1, nbchar
!
! ----- INFORMATIONS GENERALES
!
        call lislch(lischa, ichar, charge)
        call lisltc(lischa, ichar, typech)
        call lislta(lischa, ichar, typapp)
        call lislco(lischa, ichar, genrec)
        call lisltf(lischa, ichar, typfct)
        call lisllc(lischa, ichar, prefob)
        write(6,*) 'CHARGE NUMERO : ',ichar
        write(6,*) '  * NOM DE LA CHARGE                  : ',charge
        write(6,*) '  * TYPE DE LA CHARGE                 : ',typech
        write(6,*) '  * TYPE D APPLICATION                : ',typapp
        write(6,*) '  * CODE DE LA CHARGE                 : ',genrec
        write(6,*) '  * PREFIXE DE L''OBJET DE LA CHARGE   : ',prefob
        write(6,*) '  * FONCTION MULTIPLICATRICE:'
        write(6,*) '  ** TYPE                : ',typfct
        if (typfct(1:5) .eq. 'FONCT') then
            call lislnf(lischa, ichar, nomfct)
            write(6,*) '  ** NOM (DEFI_FONCTION) : ',nomfct
        else
            call lislnf(lischa, ichar, nomfct)
            write(6,*) '  ** NOM (INTERNE)       : ',nomfct
        endif
!
        if (typfct(7:10) .eq. 'COMP') then
            call liscpp(lischa, ichar, phase, npuis)
            write(6,*) '  ** PHASE               : ',phase
            write(6,*) '  ** PUISSANCE           : ',npuis
        endif
!
! ----- BOUCLE SUR LES GENRES
!
        write(6,*) '  * GENRES DE LA CHARGE:'
        do 15 igenr = 1, nbgenr
            gencha = zk24(jlisg-1+igenr)
            nomlis = '&&LISIMP.NOMLIS'
!
! ------- POSITION ENTIER CODE POUR CE GENRE
!
            call lisdef('POSG', gencha, ibid, k8bid, iposit)
            call isdeco(genrec, tabcod, 30)
            if (tabcod(iposit) .eq. 1) then
!
! --------- GENRE PRESENT DANS CETTE CHARGE
!
                write(6,*) '  ** GENRE        : ',gencha
            endif
15      continue
10  continue
!
    call jedetr(lisgen)
!
99  continue
!
    call jedema()
end subroutine
