subroutine xmdeco(resoco)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/copisd.h'
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONSTINUE - POST-TRAITEMENT)
!
! GESTION DE LA DECOUPE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    character(len=19) :: xindco, xmemco, xindcp, xmemcp
    character(len=19) :: xseuco, xseucp, xcohes, xcohep
!
! ----------------------------------------------------------------------
!
!
! --- REACTUALISATION DES STRUCTURES DE DONNEES DE CONTACT
!
    xindco = resoco(1:14)//'.XFIN'
    xmemco = resoco(1:14)//'.XMEM'
    xindcp = resoco(1:14)//'.XFIP'
    xmemcp = resoco(1:14)//'.XMEP'
    xseuco = resoco(1:14)//'.XFSE'
    xseucp = resoco(1:14)//'.XFSP'
    xcohes = resoco(1:14)//'.XCOH'
    xcohep = resoco(1:14)//'.XCOP'
    call copisd('CHAMP_GD', 'V', xindco, xindcp)
    call copisd('CHAMP_GD', 'V', xmemco, xmemcp)
    call copisd('CHAMP_GD', 'V', xseuco, xseucp)
    call copisd('CHAMP_GD', 'V', xcohep, xcohes)
!
end subroutine
