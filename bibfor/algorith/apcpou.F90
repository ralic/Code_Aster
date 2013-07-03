subroutine apcpou(sdappa, izone, nommai, typzon, tau1,&
                  tau2)
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
#include "jeveux.h"
#include "asterfort/apzoni.h"
#include "asterfort/apzonv.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/u2mesk.h"
#include "blas/dcopy.h"
    character(len=19) :: sdappa
    character(len=4) :: typzon
    integer :: izone
    real(kind=8) :: tau1(3), tau2(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! ORIENTATION DES TANGENTES DANS LE CAS DES POUTRES
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  IZONE  : NUMERO DE LA ZONE
! IN  NOMMAI : NOM DE LA MAILLE
! IN  TYPZON : TYPE DE LA ZONE 'MAIT' OU 'ESCL'
! OUT TAU1   : PREMIERE TANGENTE (NON NORMALISEE)
! OUT TAU2   : SECONDE TANGENTE (NON NORMALISEE)
!
!
!
!
    integer :: itype
    character(len=8) :: nommai
    real(kind=8) :: vector(3), norme
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- TYPE DE NORMALE
!
    if (typzon .eq. 'ESCL') then
        call apzoni(sdappa, izone, 'TYPE_NORM_ESCL', itype)
        call apzonv(sdappa, izone, 'VECT_ESCL', vector)
    else if (typzon.eq.'MAIT') then
        call apzoni(sdappa, izone, 'TYPE_NORM_MAIT', itype)
        call apzonv(sdappa, izone, 'VECT_MAIT', vector)
    else
        call assert(.false.)
    endif
!
! --- REDEFINITION SI BASE LOCALE DANS LE CAS DES POUTRES
!
    if (itype .eq. 0) then
        call u2mesk('F', 'APPARIEMENT_61', 1, nommai)
    else if (itype.eq.1) then
        call normev(vector, norme)
        call provec(vector, tau1, tau2)
    else if (itype.eq.2) then
        call normev(vector, norme)
        call dcopy(3, vector, 1, tau2, 1)
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
