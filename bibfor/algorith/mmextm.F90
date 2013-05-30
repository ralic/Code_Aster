subroutine mmextm(defico, cnsmul, posmae, mlagr)
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
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfnumn.h'
    include 'asterfort/cfposn.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: posmae
    character(len=19) :: cnsmul
    character(len=24) :: defico
    real(kind=8) :: mlagr(9)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! EXTRACTION D'UN MULTIPLICATEUR DE LAGRANGE SUR LES NOEUDS D'UNE MAILLE
! ESCLAVE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  CNSMUL : CHAM_NO_SIMPLE REDUIT AUX DDLS DU MULTIPLICATEUR EXTRAIT
! IN  POSMAE : INDICE DE LA MAILLE ESCLAVE
! OUT MLAGR  : MULTIPLICATEURS SUR LES NOEUDS ESCLAVES
!
!
!
!
    integer :: nbnmax
    parameter    (nbnmax = 9)
!
    integer :: ino, nnomai
    integer :: numnno(nbnmax), posnno(nbnmax)
    integer :: jcnslb
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    do 10,ino = 1,nbnmax
    mlagr(ino) = 0.d0
    10 end do
!
! --- NUMEROS DANS SD CONTACT DES NOEUDS DE LA MAILLE ESCLAVE
!
    call cfposn(defico, posmae, posnno, nnomai)
    call assert(nnomai.le.nbnmax)
!
! --- NUMEROS ABSOLUS DES NOEUDS DE LA MAILLE ESCLAVE
!
    call cfnumn(defico, nnomai, posnno, numnno)
!
! --- EXTRACTION DU MULTIPLICATEUR
!
    call jeveuo(cnsmul//'.CNSV', 'L', jcnslb)
    do 20 ino = 1, nnomai
        mlagr(ino) = zr(jcnslb-1+numnno(ino))
20  end do
!
    call jedema()
end subroutine
