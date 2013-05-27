subroutine vethbu(modele, matasz, charge, infcha, carele,&
                  mate, chtni, vebtem)
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
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/calcul.h'
    include 'asterfort/conlag.h'
    include 'asterfort/corich.h'
    include 'asterfort/gcncon.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mecact.h'
    include 'asterfort/memare.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: modele, charge, infcha, carele, mate, chtni, vebtem
    character(len=*) :: matasz
! ----------------------------------------------------------------------
! CALCUL DES VECTEURS ELEMENTAIRES B * TEMPERATURE
!
! IN  MODELE  : NOM DU MODELE
! IN  MATASZ  : SD MATRICE ASSEMBLEE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGEMENTS
! IN  CARELE  : CHAMP DE CARA_ELEM
! IN  MATE    : MATERIAU CODE
! IN  CHTNI   : IEME ITEREE DU CHAMP DE TEMPERATURE
! OUT VEBTEM  : VECTEURS ELEMENTAIRES PROVENANT DE B * TMOINS
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=3)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    real(kind=8) :: alpha
    character(len=8) :: nomcha, k8bid, newnom
    character(len=16) :: option
    character(len=19) :: vecel, matass
    character(len=24) :: ligrch, chalph
    integer :: iret, nchar, icha, jchar, jinf, ibid, jdir, ndir
    complex(kind=8) :: cbid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES AUX CHARGES
!
    call jeexin(charge, iret)
    if (iret .ne. 0) then
        call jelira(charge, 'LONMAX', nchar, k8bid)
        call jeveuo(charge, 'L', jchar)
    else
        nchar = 0
    endif
!
! --- ALLOCATION DU VECT_ELEM RESULTAT :
!
    call jeexin(vebtem, iret)
    vecel = '&&VEBUEE           '
    if (iret .eq. 0) then
        vebtem = vecel//'.RELR'
        call memare('V', vecel, modele(1:8), mate, carele,&
                    'CHAR_THER')
        call wkvect(vebtem, 'V V K24', nchar, jdir)
    else
        call jeveuo(vebtem, 'E', jdir)
    endif
    call jeecra(vebtem, 'LONUTI', 0, k8bid)
!
! --- ALLOCATION DE LA CARTE DU CONDITIONNEMENT DES LAGRANGES
!
    matass = matasz
    call conlag(matass, alpha)
    chalph = '&&VETHBU.CH_NEUT_R'
    call mecact('V', chalph, 'MODELE', modele, 'NEUT_R  ',&
                1, 'X1', ibid, alpha, cbid,&
                k8bid)
!
! --- CALCUL DE L'OPTION B.T
!
    option = 'THER_BU_R'
    if (nchar .gt. 0) then
        call jeveuo(infcha, 'L', jinf)
        do 10 icha = 1, nchar
            ndir = 0
            if (zi(jinf+icha) .gt. 0) then
                nomcha = zk24(jchar+icha-1) (1:8)
                ligrch = nomcha//'.CHTH.LIGRE'
                lpain(1) = 'PDDLMUR'
                lchin(1) = nomcha//'.CHTH.CMULT'
                lpain(2) = 'PDDLIMR'
                lchin(2) = chtni
                lpain(3) = 'PALPHAR'
                lchin(3) = chalph
                lpaout(1) = 'PVECTTR'
                lchout(1) = '&&VETHBU.???????'
                call gcncon('.', newnom)
                lchout(1) (10:16) = newnom(2:8)
                call corich('E', lchout(1), -1, ibid)
                call calcul('S', option, ligrch, nbin, lchin,&
                            lpain, nbout, lchout, lpaout, 'V',&
                            'OUI')
                ndir = ndir + 1
                zk24(jdir+ndir-1) = lchout(1)
            endif
            call jeecra(vebtem, 'LONUTI', ndir, k8bid)
!
10      continue
!
    endif
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
