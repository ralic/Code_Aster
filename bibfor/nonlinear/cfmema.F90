subroutine cfmema(defico, nsuco, nmaco0, listma, poinsm,&
                  nmaco)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: nsuco
    integer :: nmaco0, nmaco
    character(len=24) :: defico
    character(len=24) :: listma
    character(len=24) :: poinsm
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES - ELIMINATION)
!
! MISE A JOUR DE LA LISTE DES MAILLES APRES ELIMINATION
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : NOM SD CONTACT DEFINITION
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! IN  NMACO0 : NOMBRE TOTAL DE MAILLES DES SURFACES
! IN  POINSM : POINTEUR MISE A JOUR POUR PSURMA
! IN  LISTMA : LISTE DES MAILLES RESTANTES (LONGUEUR NMACO
! IN  NMACO  : NOMBRE DE MAILLES AU FINAL
!
!
!
!
    character(len=24) :: contma, psurma
    integer :: jmaco, jsuma
    integer :: jelima, jma
    integer :: isuco, i
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    call jeveuo(listma, 'L', jma)
    call jeveuo(poinsm, 'L', jelima)
!
    contma = defico(1:16)//'.MAILCO'
    psurma = defico(1:16)//'.PSUMACO'
    call jeveuo(psurma, 'E', jsuma)
    call jeveuo(contma, 'E', jmaco)
!
! --- MODIFICATION DU POINTEUR PSURMA
!
    do 160 isuco = 1, nsuco
        zi(jsuma+isuco) = zi(jsuma+isuco) - zi(jelima+isuco)
160  end do
!
! --- TRANSFERT DES VECTEURS DE TRAVAIL DANS CONTMA
!
    do 170 i = 1, nmaco
        zi(jmaco+i-1) = zi(jma+i-1)
170  end do
!
! --- MAZ ET MODIFICATION DE L'ATTRIBUT LONUTI
!
    do 180 i = nmaco + 1, nmaco0
        zi(jmaco+i-1) = 0
180  end do
    call jeecra(contma, 'LONUTI', nmaco, k8bid)
!
    call jedema()
end subroutine
