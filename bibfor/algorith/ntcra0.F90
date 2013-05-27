subroutine ntcra0(sddisc)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/nmcrpx.h'
    include 'asterfort/wkvect.h'
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE THER_* (STRUCTURES DE DONNES)
!
! CREATION SD ARCHIVAGE SPECIAL SI CALCUL NON TRANSITOIRE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: arcinf, arcexc
    integer :: jarinf, jarexc
    character(len=19) :: sdarch
    integer :: iocc
    character(len=16) :: motfac, motpas
    character(len=1) :: base
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    motfac = ' '
    motpas = ' '
    iocc = 0
    base = 'V'
!
! --- NOM SD ARCHIVAGE
!
    sdarch = sddisc(1:14)//'.ARCH'
    arcinf = sdarch(1:19)//'.AINF'
    arcexc = sdarch(1:19)//'.AEXC'
!
! --- CREATION DES SDS
!
    call wkvect(arcexc, 'V V K16', 1, jarexc)
    call wkvect(arcinf, 'V V I', 3, jarinf)
!
! --- LECTURE LISTE INSTANTS D'ARCHIVAGE
!
    call nmcrpx(motfac, motpas, iocc, sdarch, base)
!
    call jedema()
!
end subroutine
