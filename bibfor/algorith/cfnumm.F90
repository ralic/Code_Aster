subroutine cfnumm(defico, nma, posnma, numnma)
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
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: defico
    integer :: nma
    integer :: posnma(nma)
    integer :: numnma(nma)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - UTILITAIRE)
!
! DONNE LES NUMEROS ABSOLUS DES MAILLES DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE CONTACT (DEFINITION)
! IN  NMA    : NOMBRE DE MAILLES
! IN  POSNMA : INDICE DANS CONTNO DES MAILLES
! OUT NUMNMA : INDICE ABSOLUS DES MAILLES DANS LE MAILLAGE
!
!
!
!
    integer :: ima, posma
    character(len=24) :: contma
    integer :: jmaco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONTACT
!
    contma = defico(1:16)//'.MAILCO'
    call jeveuo(contma, 'L', jmaco)
!
! --- NUMERO DES MAILLES
!
    do 10 ima = 1, nma
        posma = posnma(ima)
        numnma(ima) = zi(jmaco+posma-1)
10  end do
!
    call jedema()
!
end subroutine
