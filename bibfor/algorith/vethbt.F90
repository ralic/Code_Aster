subroutine vethbt(modele, charge, infcha, carele, mate,&
                  chtni, vebtla)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/calcul.h'
    include 'asterfort/corich.h'
    include 'asterfort/gcncon.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/memare.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: modele, charge, infcha, carele, mate, chtni, vebtla
! ----------------------------------------------------------------------
! CALCUL DES TERMES DE DIRICHLET EN THERMIQUE NON LINEAIRE
!
! IN  MODELE  : NOM DU MODELE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGEMENTS
! IN  CARELE  : CHAMP DE CARA_ELEM
! IN  MATE    : MATERIAU CODE
! IN  CHTNI   : IEME ITEREE DU CHAMP DE TEMPERATURE
! OUT VEBTLA  : VECTEURS ELEMENTAIRES PROVENANT DE BT LAMBDA
!
!
!
    character(len=8) :: nomcha, lpain(2), lpaout(1), k8bid, newnom
    character(len=16) :: option
    character(len=19) :: vecel
    character(len=24) :: lchin(2), lchout(1), ligrch
    integer :: iret, nchar, icha, jchar, jinf
!-----------------------------------------------------------------------
    integer :: ibid, jdir, ndir
!-----------------------------------------------------------------------
    call jemarq()
    call jeexin(charge, iret)
    if (iret .ne. 0) then
        call jelira(charge, 'LONMAX', nchar, k8bid)
        call jeveuo(charge, 'L', jchar)
    else
        nchar = 0
    endif
!
    call jeexin(vebtla, iret)
    vecel = '&&VETBTL           '
    if (iret .eq. 0) then
        vebtla = vecel//'.RELR'
        call memare('V', vecel, modele(1:8), mate, carele,&
                    'CHAR_THER')
        call wkvect(vebtla, 'V V K24', nchar, jdir)
    else
        call jeveuo(vebtla, 'E', jdir)
    endif
    call jeecra(vebtla, 'LONUTI', 0, k8bid)
!
    if (nchar .gt. 0) then
        call jeveuo(infcha, 'L', jinf)
        ndir = 0
        do 10 icha = 1, nchar
!
! ------- CALCUL DES TERMES DE DIRICHLET SOUS-OPTION BT LAMBDA
!
            option = 'THER_BTLA_R'
            if (zi(jinf+icha) .gt. 0) then
                nomcha = zk24(jchar+icha-1) (1:8)
                ligrch = nomcha//'.CHTH.LIGRE'
                lpain(1) = 'PDDLMUR'
                lchin(1) = nomcha//'.CHTH.CMULT'
                lpain(2) = 'PLAGRAR'
                lchin(2) = chtni
                lpaout(1) = 'PVECTTR'
                lchout(1) = '&&VETHBT.???????'
                call gcncon('.', newnom)
                lchout(1) (10:16) = newnom(2:8)
                call corich('E', lchout(1), -1, ibid)
                call calcul('S', option, ligrch, 2, lchin,&
                            lpain, 1, lchout, lpaout, 'V',&
                            'OUI')
                ndir = ndir + 1
                zk24(jdir+ndir-1) = lchout(1)
            endif
!
10      continue
        call jeecra(vebtla, 'LONUTI', ndir, k8bid)
!
    endif
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
