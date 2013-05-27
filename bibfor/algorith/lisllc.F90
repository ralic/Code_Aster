subroutine lisllc(lischa, ichar, prefob)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lisnnb.h'
    character(len=19) :: lischa
    integer :: ichar
    character(len=13) :: prefob
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! RETOURNE LE PREFIXE DE L'OBJET DE LA CHARGE
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE DES CHARGES
! IN  ICHAR  : INDICE DE LA CHARGE
! OUT PREFOB : PREFIXE DE L'OBJET DE LA CHARGE
!
!
!
!
    character(len=24) :: precha
    integer :: jprec
    integer :: nbchar
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    prefob = ' '
    call lisnnb(lischa, nbchar)
!
    if (nbchar .ne. 0) then
        precha = lischa(1:19)//'.PREO'
        call jeveuo(precha, 'L', jprec)
        prefob = zk24(jprec-1+ichar)(1:13)
    endif
!
    call jedema()
end subroutine
