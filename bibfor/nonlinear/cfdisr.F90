function cfdisr(deficz, questz)
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
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfr.h"
    real(kind=8) :: cfdisr
    character(len=*) :: deficz
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES)
!
! RETOURNE DES INFOS DIVERSES POUR LE CONTACT (REEL)
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO  : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  QUESTI  : QUESTION (PARAMETRE INTERROGE)
!   'RESI_ABSO'     PARAMETRE METHODE GCP
!   'COEF_RESI'     PARAMETRE METHODE GCP
!
!
!
!
    character(len=24) :: defico, questi
    character(len=24) :: paracr
    integer :: jparcr, izone
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    defico = deficz
    questi = questz
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    paracr = defico(1:16)//'.PARACR'
    call jeveuo(paracr, 'L', jparcr)
!
    if (questi .eq. 'RESI_GEOM') then
        cfdisr = zr(jparcr+1-1)
!
    else if (questi.eq.'RESI_FROT') then
        cfdisr = zr(jparcr+2-1)
!
    else if (questi.eq.'RESI_ABSO') then
        cfdisr = zr(jparcr+4-1)
!
    else if (questi.eq.'COEF_RESI') then
        cfdisr = zr(jparcr+5-1)
!
    else if (questi.eq.'ALARME_JEU') then
        izone = 1
        cfdisr = mminfr(defico,'ALARME_JEU',izone )
!
    else if (questi.eq.'PROJ_NEWT_RESI') then
        cfdisr = 1d-4
!
    else
        call assert(.false.)
    endif
!
end function
