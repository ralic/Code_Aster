subroutine sanscc(char, motfac, noma)
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
! REPONSABLE
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/sansno.h'
    character(len=8) :: char, noma
    character(len=16) :: motfac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - LECTURE DONNEES)
!
! TRAITEMENT MOT-CLEFS SANS_GROUP_NO_FR/SANS_NOEUD_FR
!
! ----------------------------------------------------------------------
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NOMA   : NOM DU MAILLAGE
!
!
!
!
    character(len=24) :: defico
    character(len=24) :: frotno, pfrot
    integer :: nbmocl
    character(len=16) :: limocl(2), tymocl(2)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nbmocl = 2
    tymocl(1) = 'GROUP_NO'
    tymocl(2) = 'NOEUD'
!
! --- TRAITEMENT MOT-CLEF SANS_GROUP_NO_FR/SANS_NOEUD_FR
!
    frotno = defico(1:16)//'.SANOFR'
    pfrot = defico(1:16)//'.PSANOFR'
    limocl(1) = 'SANS_GROUP_NO_FR'
    limocl(2) = 'SANS_NOEUD_FR'
    call sansno(char, motfac, noma, frotno, pfrot,&
                nbmocl, tymocl, limocl)
!
    call jedema()
end subroutine
