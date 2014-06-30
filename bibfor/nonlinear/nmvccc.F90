subroutine nmvccc(modele, nbin, nbout, lpain, lchin,&
                  lpaout, lchout, exitem, exihyd, exipto,&
                  exisec, exiepa, exipha, vecel)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/reajre.h"
    integer :: nbout, nbin
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
    character(len=8) :: modele
    logical(kind=1) :: exitem, exihyd, exipto, exisec, exiepa, exipha
    character(len=19) :: vecel
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES VECT_ELEM DES VARIABLES DE COMMANDES
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NBIN   : NOMBRE MAXI DE CHAMPS IN POUR CALCUL
! IN  LPAIN  : NOM DES TYPES DE CHAMP D'ENTREE
! IN  LCHIN  : NOM DES CHAMPS D'ENTREE
! IN  NBOUT  : NOMBRE MAXI DE CHAMPS OUT POUR CALCUL
! IN  LPAOUT : NOM DES TYPES DE CHAMP DE SORTIE
! IN  LCHOUT : NOM DES CHAMPS DE SORTIE
! IN  EXITEM : VARIABLE DE COMMANDE TEMPERATURE PRESENTE OU NON
! IN  EXIHYD : VARIABLE DE COMMANDE HYDRATATION PRESENTE OU NON
! IN  EXIPTO : VARIABLE DE COMMANDE PRESSION TOTALE DE FLUIDE
!              PRESENTE OU NON
! IN  EXISEC : VARIABLE DE COMMANDE SECHAGE PRESENTE OU NON
! IN  EXIEPA : VARIABLE DE COMMANDE DEFO. ANEL. PRESENTE OU NON
! IN  EXIPHA : VARIABLE DE COMMANDE PHASES META. PRESENTE OU NON
! OUT VECEL  : NOM DU VECT_ELEM
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=6) :: masque
    character(len=16) :: option
    character(len=24) :: ligrmo
    integer :: iret, nbr
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
!
    endif
!
! --- INITIALISATIONS
!
    nbr = 0
    masque = '.VEXXX'
    ligrmo = modele(1:8)//'.MODELE'
!
! --- THERMIQUE
!
    if (exitem) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vecel(1:8)// masque
        option = 'CHAR_MECA_TEMP_R'
        call calcul('C', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call jeexin(lchout(1)//'.RESL', iret)
        call reajre(vecel, lchout(1), 'V')
    endif
!
! --- HYDRATATION
!
    if (exihyd) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vecel(1:8)// masque
        option = 'CHAR_MECA_HYDR_R'
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vecel, lchout(1), 'V')
    endif
!
! --- THM - CHAINAGE HYDRAULIQUE VERS LA MECANIQUE
!
    if (exipto) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vecel(1:8)// masque
        option = 'CHAR_MECA_PTOT_R'
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vecel, lchout(1), 'V')
    endif
!
! --- SECHAGE
!
    if (exisec) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vecel(1:8)// masque
        option = 'CHAR_MECA_SECH_R'
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vecel, lchout(1), 'V')
    endif
!
! --- DEFORMATION ANELASTIQUE
!
    if (exiepa) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vecel(1:8)// masque
        option = 'CHAR_MECA_EPSA_R'
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vecel, lchout(1), 'V')
    endif
!
! --- PHASES METALLURGIQUES
!
    if (exipha) then
        nbr = nbr+1
        call codent(nbr, 'D0', masque(4:6))
        lchout(1) = vecel(1:8)// masque
        option = 'CHAR_MECA_META_Z'
        call calcul('S', option, ligrmo, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(vecel, lchout(1), 'V')
    endif
!
    call jedema()
end subroutine
