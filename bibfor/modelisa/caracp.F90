subroutine caracp(char)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: char
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - SD)
!
! CREATION DES SDS DE DEFINITION DU CONTACT DEDIEES AUX
! PARAMETRES GENERAUX (NE DEPENDANT PAS DE LA ZONE DE CONTACT)
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
!
!
!
!
    character(len=24) :: defico
    character(len=24) :: ndimco, paracr, paraci
    integer :: jdim, jparcr, jparci
    integer :: zparr, zpari, zdime
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
!
! --- NOMS SDS
!
    paracr = defico(1:16)//'.PARACR'
    paraci = defico(1:16)//'.PARACI'
    ndimco = defico(1:16)//'.NDIMCO'
!
    zparr = cfmmvd('ZPARR')
    zpari = cfmmvd('ZPARI')
    zdime = cfmmvd('ZDIME')
!
    call wkvect(paracr, 'G V R', zparr, jparcr)
    call wkvect(paraci, 'G V I', zpari, jparci)
    call wkvect(ndimco, 'G V I', zdime, jdim)
!
    call jedema()
!
end subroutine
