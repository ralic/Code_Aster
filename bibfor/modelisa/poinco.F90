subroutine poinco(char, motfac, noma, nzoco, nsuco)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nbzoco.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: char
    character(len=16) :: motfac
    character(len=8) :: noma
    integer :: nzoco
    integer :: nsuco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! DETERMINATION DU NOMBRE DE ZONES DE CONTACT ET DU NOMBRE TOTAL DE
! MAILLES ET DE NOEUDS DE CONTACT.
! REMPLISSAGE DES POINTEURS ASSOCIES: PZONE,PSURMA,PSURNO,PNOQUA
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR
! IN  NOMA   : NOM DU MAILLAGE
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! OUT NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
!
!
!
!
    integer :: izone
    character(len=24) :: pzone, psurma, psurno
    integer :: jzone, jsuma, jsuno
    character(len=24) :: defico
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    pzone = defico(1:16)//'.PZONECO'
    psurma = defico(1:16)//'.PSUMACO'
    psurno = defico(1:16)//'.PSUNOCO'
!
! --- INITIALISATIONS
!
    nsuco = 0
!
! --- DETERMINATION DU NOMBRE DE SURFACES
!
    call wkvect(pzone, 'G V I', nzoco+1, jzone)
    do 2 izone = 1, nzoco
        call nbzoco(motfac, noma, izone, jzone, nsuco)
 2  end do
!
! --- TABLEAUX DU NOMBRE DE MAILLES ET DE NOEUDS DE CONTACT
!
    call wkvect(psurma, 'G V I', nsuco+1, jsuma)
    call wkvect(psurno, 'G V I', nsuco+1, jsuno)
!
    call jedema()
end subroutine
