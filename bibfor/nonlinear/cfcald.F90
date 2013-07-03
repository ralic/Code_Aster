function cfcald(defico, izone, typnoe)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    logical :: cfcald
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
    character(len=4) :: typnoe
    character(len=24) :: defico
    integer :: izone
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - APPARIEMENT)
!
! DIT SI LA NORMALE DOIT ETRE CALCULEE SUR LE TYPE
! DE NOEUD DONNE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  IZONE  : ZONE DE CONTACT
! IN  TYPNOE : TYPE DU NOEUD 'MAIT' OU 'ESCL'
! OUT CFCALD : .TRUE. SI NORMALE A CALCULER
!
! ----------------------------------------------------------------------
!
    integer :: iappa
    logical :: lliss, lmait, lescl, lmaes
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    cfcald = .false.
!
! --- FONCTIONNALITES ACTIVEES
!
    lliss = cfdisl(defico,'LISSAGE')
    lmait = mminfl(defico,'MAIT' ,izone)
    lescl = mminfl(defico,'ESCL' ,izone)
    lmaes = mminfl(defico,'MAIT_ESCL' ,izone)
    iappa = mminfi(defico,'APPARIEMENT' ,izone)
!
! --- CALCUL OU NON ?
!
    if (typnoe .eq. 'MAIT') then
        if (lliss) then
            if (lmait .or. lmaes) then
                cfcald = .true.
            endif
        endif
        if (iappa .eq. 0) then
            if (lmait .or. lmaes) then
                cfcald = .true.
            endif
        endif
    else if (typnoe.eq.'ESCL') then
        if (lescl .or. lmaes) then
            cfcald = .true.
        endif
    else
        call assert(.false.)
    endif
!
    call jedema()
end function
