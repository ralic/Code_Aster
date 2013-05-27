subroutine lrmmeq(fid, nomamd, infmed)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     LECTURE DU MAILLAGE -  FORMAT MED - LES EQUIVALENCES
!     -    -     -                  -         --
!-----------------------------------------------------------------------
!     ENTREES :
!       FID    : IDENTIFIANT DU FICHIER MED
!       NOMAMD : NOM DU MAILLAGE MED
!       INFMED : NIVEAU DES INFORMATIONS A IMPRIMER
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jemarq.h'
    integer :: fid
    integer :: infmed
!
    character(len=*) :: nomamd
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRMMEQ' )
!
!     ------------------------------------------------------------------
    call jemarq()
!
!====
! 1. PREALABLES
!====
!
!
    call jedetc('V', '&&'//nompro, 1)
!
    call jedema()
!
end subroutine
