function liscft(lischa, ichar)
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
    logical :: liscft
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lisltc.h'
    include 'asterfort/lisltf.h'
    character(len=19) :: lischa
    integer :: ichar
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! LA CHARGE EST UNE FONCTION DU TEMPS ?
!
! ----------------------------------------------------------------------
!
! RETOURNE .TRUE. SI LA CHARGE EST FONCTION DU TEMPS:
!  * AFFE_*_F : SI C'EST UNE FONCTION DU TEMPS
!  * AFFE_*   : S'IL Y A UNE FONC_MULT
!
! IN  LISCHA : SD LISTE DES CHARGES
! IN  ICHAR  : INDICE DE LA CHARGE
!
!
!
!
    character(len=8) :: typech
    character(len=16) :: typfct
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    liscft = .false.
!
! --- TYPE DE LA CHARGE
!
    call lisltc(lischa, ichar, typech)
!
! --- TYPE DE LA FONCTION MULTIPLICATRICE
!
    call lisltf(lischa, ichar, typfct)
!
    if (typech .eq. 'FONC_FT') liscft = .true.
    if (typfct(1:5) .eq. 'FONCT') liscft = .true.
!
    call jedema()
end function
