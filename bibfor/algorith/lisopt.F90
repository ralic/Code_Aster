subroutine lisopt(prefob, nomo, typech, indxch, option,&
                  parain, paraou, carte, ligcal)
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
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lisdef.h'
    include 'asterfort/lislic.h'
    integer :: indxch
    character(len=8) :: nomo
    character(len=16) :: option
    character(len=19) :: carte, ligcal
    character(len=8) :: parain, paraou
    character(len=8) :: typech
    character(len=13) :: prefob
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! CREATION OBJETS CONTENANT LA LISTE DES CHARGES POUR LE GENRE DONNE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO   : NOM DU MODELE
! IN  PREFOB : PREFIXE DE L'OBJET DE LA CHARGE
! IN  TYPECH : TYPE DE LA CHARGE
! IN  INDXCH : INDICE DU TYPE DE CHARGE
! OUT OPTION : NOM DE L'OPTION DE CALCUL
! OUT CARTE  : NOM DE LA CARTE
! OUT PARAIN : NOM DU PARAMETRE D'ENTREE
! OUT PARAOU : NOM DU PARAMETRE DE SORTIE
! OUT LIGCAL : NOM DU LIGREL SUR LEQUEL ON FAIT LE CALCUL
!
!
!
!
    integer :: ibid
    character(len=24) :: nomobj
    integer :: itypob
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    option = ' '
    parain = ' '
    paraou = ' '
    carte = ' '
    ligcal = ' '
!
! --- OPTION DE CALCUL
!
    call lisdef('OPTI', typech, indxch, option, ibid)
!
! --- NOM DE LA CARTE
!
    call lisdef('OBJE', prefob, indxch, nomobj, itypob)
!
! --- ON ATTEND UNE CARTE !
!
    if (itypob .eq. 1) then
        carte = nomobj(1:19)
    else
        call assert(.false.)
    endif
!
! --- NOM DU PARAMETRE D'ENTREE
!
    call lisdef('PARA', typech, indxch, parain, ibid)
    if (typech .eq. 'COMP') then
        paraou = 'PVECTUC'
    else
        paraou = 'PVECTUR'
    endif
!
! --- LIGREL DE CALCUL
!
    call lislic(nomo, prefob, indxch, ligcal)
!
    call jedema()
end subroutine
