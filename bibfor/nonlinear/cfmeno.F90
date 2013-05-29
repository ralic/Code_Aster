subroutine cfmeno(defico, nsuco, nnoco0, listno, poinsn,&
                  nnoco)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: nsuco
    integer :: nnoco0, nnoco
    character(len=24) :: defico
    character(len=24) :: listno
    character(len=24) :: poinsn
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES NOEUDES - LECTURE DONNEES - ELIMINATION)
!
! MISE A JOUR DE LA LISTE DES NOEUDS APRES ELIMINATION
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : NOM SD CONTACT DEFINITION
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! IN  NNOCO0 : NOMBRE TOTAL DE NOEUDS DES SURFACES
! IN  POINSN : POINTEUR MISE A JOUR POUR PSURNO
! IN  LISTNO : LISTE DES NOEUDS RESTANTES (LONGUEUR NNOCO
! IN  NNOCO  : NOMBRE DE NOEUDS AU FINAL
!
!
!
!
    character(len=24) :: contno, psurno
    integer :: jnoco, jsuno
    integer :: jelino, jno
    integer :: isuco, i
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    call jeveuo(listno, 'L', jno)
    call jeveuo(poinsn, 'L', jelino)
!
    contno = defico(1:16)//'.NOEUCO'
    psurno = defico(1:16)//'.PSUNOCO'
    call jeveuo(psurno, 'E', jsuno)
    call jeveuo(contno, 'E', jnoco)
!
! --- MODIFICATION DU POINTEUR PSURNO
!
    do 160 isuco = 1, nsuco
        zi(jsuno+isuco) = zi(jsuno+isuco) - zi(jelino+isuco)
160  end do
!
! --- TRANSFERT DES VECTEURS DE TRAVAIL DANS CONTNO
!
    do 170 i = 1, nnoco
        zi(jnoco+i-1) = zi(jno+i-1)
170  end do
!
! --- MAZ ET MODIFICATION DE L'ATTRIBUT LONUTI
!
    do 180 i = nnoco + 1, nnoco0
        zi(jnoco+i-1) = 0
180  end do
    call jeecra(contno, 'LONUTI', nnoco, k8bid)
!
    call jedema()
end subroutine
