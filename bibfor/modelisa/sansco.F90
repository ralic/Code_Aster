subroutine sansco(char, motfac, noma)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit      none
#include "jeveux.h"
#include "asterfort/sansno.h"
    character(len=8) :: char
    character(len=16) :: motfac
    character(len=8) :: noma
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! LECTURE: DES NOEUDS DANS LE MOT-CLEF SANS_*
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NOMA   : NOM DU MAILLAGE
!
!
!
!
    character(len=24) :: sansn, psans
    character(len=24) :: defico
    integer :: nbmocl
    character(len=16) :: limocl(4), tymocl(4)
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nbmocl = 4
    tymocl(1) = 'GROUP_NO'
    tymocl(2) = 'NOEUD'
    tymocl(3) = 'GROUP_MA'
    tymocl(4) = 'MAILLE'
    limocl(1) = 'SANS_GROUP_NO'
    limocl(2) = 'SANS_NOEUD'
    limocl(3) = 'SANS_GROUP_MA'
    limocl(4) = 'SANS_MAILLE'
!
! --- TRAITEMENT MOT-CLEF SANS_GROUP_NO/SANS_NOEUD
!
    sansn = defico(1:16)//'.SSNOCO'
    psans = defico(1:16)//'.PSSNOCO'
!
    call sansno(char, motfac, noma, sansn, psans,&
                nbmocl, tymocl, limocl)
!
end subroutine
