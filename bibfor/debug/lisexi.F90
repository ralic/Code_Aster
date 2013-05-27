function lisexi(prefob, indxch)
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
    implicit none
    logical :: lisexi
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/exisd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lisdef.h'
    character(len=13) :: prefob
    integer :: indxch
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! RETOURNE TRUE SI LE CHARGEMENT EXISTE
!
! ----------------------------------------------------------------------
!
!
! IN  PREFOB : PREFIXE DE LA CHARGE
! IN  INDXCH : INDEX DE LA CHARGE
!
!
!
!
    character(len=24) :: nomobj
    integer :: itypob
    character(len=19) :: carte
    integer :: iret
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    lisexi = .false.
!
! --- RECUPERATION OBJET LIE A CETTE CHARGE
!
    call lisdef('OBJE', prefob, indxch, nomobj, itypob)
!
! --- VERIFICATION EXISTENCE
!
    if (itypob .eq. 1) then
        carte = nomobj(1:19)
        call exisd('CARTE', carte, iret)
        if (iret .eq. 1) lisexi = .true.
    else if (itypob.eq.0) then
        call jeexin(nomobj, iret)
        if (iret .ne. 0) lisexi = .true.
    else
        call assert(.false.)
    endif
!
    call jedema()
end function
