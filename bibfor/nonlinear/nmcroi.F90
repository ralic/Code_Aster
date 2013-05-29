subroutine nmcroi(sdobse, motfac, nbocc)
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
    include 'asterfort/impfoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmcrpx.h'
    character(len=19) :: sdobse
    integer :: nbocc
    character(len=16) :: motfac
!
! --------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - OBSERVATION)
!
! LECTURE LISTE DES INSTANTS
!
! ----------------------------------------------------------------------
!
!
! IN  SDOBSE : NOM DE LA SD POUR OBSERVATION
! IN  MOTFAC : MOT-FACTEUR POUR LIRE
! IN  MOTPAS : MOT-FACTEUR POUR LIRE PAS
!
!
!
!
    integer :: iocc
    character(len=1) :: base
    character(len=19) :: listli
    character(len=2) :: chaine
    character(len=16) :: motpas
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    base = 'V'
    motpas = 'PAS_OBSE'
!
! --- LECTURE LISTE INSTANTS
!
    do 10 iocc = 1, nbocc
        call impfoi(0, 2, iocc, chaine)
        listli = sdobse(1:14)//chaine(1:2)//'.LI'
        call nmcrpx(motfac, motpas, iocc, listli, base)
10  end do
!
    call jedema()
end subroutine
