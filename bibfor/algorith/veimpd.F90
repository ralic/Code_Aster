subroutine veimpd(modele, mate, vitini, sddyna, vecelz)
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
    include 'asterfort/calcul.h'
    include 'asterfort/corich.h'
    include 'asterfort/dbgcal.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/inical.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ndynkk.h'
    include 'asterfort/reajre.h'
    character(len=*) :: vecelz
    character(len=24) :: modele, mate
    character(len=19) :: vitini
    character(len=19) :: sddyna
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES VECTEURS ELEMENTAIRES DES IMPEDANCES DE SOL
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  MATE   : CHAMP DE MATERIAU
! IN  VITINI : VITESSE INITIALE
! IN  SDDYNA : SD DYNAMIQUE
! OUT VECELE : VECTEURS ELEMENTAIRES IMPEDANCES DE SOL
!
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=4)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: ibid
    character(len=19) :: vecele
    character(len=16) :: option
    character(len=24) :: chgeom, ligrmo
    character(len=19) :: vitent
    logical :: debug
    integer :: jnoma
    integer :: ifmdbg, nivdbg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    vecele = vecelz
    ligrmo = modele(1:8)//'.MODELE'
    call ndynkk(sddyna, 'VITENT', vitent)
    call jeveuo(ligrmo(1:19)//'.LGRF', 'L', jnoma)
    chgeom = zk8(jnoma)//'.COORDO'
    option = 'IMPE_ABSO'
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
! --- CHAMPS D'ENTREE
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom(1:19)
    lpain(2) = 'PMATERC'
    lchin(2) = mate(1:19)
    lpain(3) = 'PVITPLU'
    lchin(3) = vitini(1:19)
    lpain(4) = 'PVITENT'
    lchin(4) = vitent
!
! --- CHAMPS DE SORTIE
!
    lpaout(1) = 'PVECTUR'
    lchout(1) = vecele
!
! --- ALLOCATION DU VECT_ELEM RESULTAT :
!
    call jedetr(vecele//'.RELR')
!
! --- CALCUL
!
    call corich('E', lchout(1), -1, ibid)
    call calcul('S', option, ligrmo, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
!
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
    call reajre(vecele, lchout(1), 'V')
!
    call jedema()
end subroutine
