subroutine oblcre(sdlist, typstz, idnpaz, nbstru)
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
    implicit      none
    include       'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/obcrea.h'
    include 'asterfort/obseti.h'
    include 'asterfort/obsetk.h'
    include 'asterfort/obseto.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: sdlist
    character(len=*) :: typstz
    character(len=*) :: idnpaz
    integer :: nbstru
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (GESTION STRUCT - LISTE DE STRUCTS)
!
! CREATION
!
! ----------------------------------------------------------------------
!
!
! IN  SDLIST : NOM DE LA LISTE
! IN  TYPSTR : TYPE DES STRUCT DE LA LISTE
! IN  IDNPAR : NOM DU PARAMETRE IDENTIFIANT LE STRUCT
! IN  NBSTRU : NOMBRE DE STRUCTS DANS LA LISTE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: lisnom, lisact
    integer :: jlisac
    character(len=24) :: typstr, idnpar
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    typstr = typstz
    idnpar = idnpaz
!
! --- LISTE DE LISTES INTERDIT
!
    if (typstr .eq. 'LISTE_STRUCT') then
        call assert(.false.)
    endif
    call assert(nbstru.gt.0)
!
! --- LISTE DES NOMS DE STRUCTS
!
    lisnom = sdlist(1:19)//'.NOMS'
    call jecrec(lisnom, 'V V K24', 'NOM', 'CONTIG', 'CONSTANT',&
                nbstru)
    call jeecra(lisnom, 'LONMAX', 1, k8bid)
!
! --- LISTE DES STRUCTS ACTIFS
!
    lisact = sdlist(1:19)//'.ACTI'
    call wkvect(lisact, 'V V I  ', nbstru, jlisac)
!
! --- CREATION STRUCTS
!
    call obcrea('LISTE_STRUCTS', sdlist)
!
! --- VALEURS DES PARAMETRES
!
    call obseti(sdlist, 'NBRE_STRUCTS', nbstru)
    call obseto(sdlist, 'NOM_STRUCTS', lisnom)
    call obseto(sdlist, 'ACT_STRUCTS', lisact)
    call obsetk(sdlist, 'TYPE_STRUCTS', typstr)
    call obsetk(sdlist, 'PARA_IDEN_STRUCT', idnpar)
!
    call jedema()
end subroutine
