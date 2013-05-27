subroutine vechmx(nomo, lischa, ichar, nbch, nomlis,&
                  nbin, lpain, lchin, lastin, vecele)
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
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/calcul.h'
    include 'asterfort/corich.h'
    include 'asterfort/exisd.h'
    include 'asterfort/gcnco2.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lisllc.h'
    include 'asterfort/lisltc.h'
    include 'asterfort/lisopt.h'
    include 'asterfort/reajre.h'
    integer :: nbin, lastin
    character(len=8) :: lpain(nbin)
    character(len=19) :: lchin(nbin)
    character(len=19) :: lischa
    character(len=24) :: nomlis
    integer :: ichar, nbch
    character(len=8) :: nomo
    character(len=19) :: vecele
!
! ----------------------------------------------------------------------
!
! CALCUL DES VECTEURS ELEMENTAIRES DES CHARGEMENTS MECANIQUES
! DE NEUMANN (VOIR DEFINITION DANS LISDEF)
!
! CALCUL EFFECTIF - BOUCLE SUR LES TYPES DE CHARGEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO   : NOM DU MODELE
! IN  LISCHA : SD LISTE DES CHARGES
! IN  ICHAR  : INDICE DE LA CHARGE
! IN  NOMLIS : LISTE DES INDEX DES CHARGES
! IN  NBCH   : LONGUEUR DE NOMLIS
! IN  NBIN   : NOMBRE MAXI DE CHAMPS D'ENTREE
! IN  LPAIN  : LISTE DES PARAMETRES IN
! IN  LCHIN  : LISTE DES CHAMPS IN
! IN  LASTIN : NOMBRE EFFECTIF DE CHAMPS IN
! OUT VECELE : VECT_ELEM RESULTAT
!
! ----------------------------------------------------------------------
!
    integer :: nbout
    parameter    (nbout=1)
    character(len=8) :: lpaout(nbout)
    character(len=19) :: lchout(nbout)
!
    integer :: jlisci, ich, ibid
    integer :: iret
    integer :: indxch
    character(len=16) :: option
    character(len=8) :: parain, paraou, newnom
    character(len=8) :: typech
    character(len=19) :: carte
    character(len=19) :: ligcal
    character(len=13) :: prefob
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    newnom = '.0000000'
!
! --- PREFIXE DE L'OBJET DE LA CHARGE
!
    call lisllc(lischa, ichar, prefob)
!
! --- TYPE DE LA CHARGE
!
    call lisltc(lischa, ichar, typech)
!
! --- CHAMP DE SORTIE
!
    call gcnco2(newnom)
    lchout(1) = '&&VECHMX.'//newnom(2:8)
    call corich('E', lchout(1), ichar, ibid)
!
! --- LISTE DES INDEX DES CHARGES
!
    call jeveuo(nomlis, 'L', jlisci)
!
! --- CALCUL
!
    do 70 ich = 1, nbch
        indxch = zi(jlisci-1+ich)
        call lisopt(prefob, nomo, typech, indxch, option,&
                    parain, paraou, carte, ligcal)
        call jeexin(carte(1:19)//'.DESC', iret)
        if (iret .ne. 0) then
!
! ------- CARTE D'ENTREE
!
            lastin = lastin + 1
            lchin(lastin) = carte
            lpain(lastin) = parain
!
! ------- CARTE DE SORTIE
!
            lpaout(1) = paraou
!
! ------- CALCUL
!
            call assert(lastin.le.nbin)
            call calcul('S', option, ligcal, lastin, lchin,&
                        lpain, nbout, lchout, lpaout, 'V',&
                        'OUI')
!
! ------- RESU_ELEM DANS LE VECT_ELEM
!
            call exisd('CHAMP_GD', lchout(1), iret)
            call assert(iret.gt.0)
            call reajre(vecele, lchout(1), 'V')
        endif
70  end do
!
    call jedema()
end subroutine
