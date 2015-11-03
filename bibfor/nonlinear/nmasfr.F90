subroutine nmasfr(ds_contact, matass)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infbav.h"
#include "asterfort/infdbg.h"
#include "asterfort/infmue.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtcmbl.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19) :: matass
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - ALGORITHME)
!
! CREATION DE LA MATRICE DE CONTACT/FROTTEMENT
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! I/O MATASS  : IN  MATR_ASSE TANGENTE
!               OUT MATR_ASSE TANGENTE + MATRICE CONTACT/FROTTEMENT
!                  (EVENTUELLE)
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=14) :: numedf
    integer :: nbliac
    character(len=19) :: matrcf
    character(len=24) :: limat(2)
    real(kind=8) :: coefmu(2)
    character(len=1) :: typcst(2)
    aster_logical :: lmodim
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    lmodim = cfdisl(ds_contact%sdcont_defi,'MODI_MATR_GLOB')
    nbliac = cfdisd(ds_contact%sdcont_solv,'NBLIAC')
    if (nbliac .eq. 0) then
        goto 999
    endif
    if (.not.lmodim) then
        goto 999
    endif
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> AJOUT MATRICE CONTACT/FROTTEMENT'
    endif
!
    matrcf = ds_contact%sdcont_solv(1:14)//'.MATR'
    limat(1) = matass
    limat(2) = matrcf
    coefmu(1) = 1.d0
    coefmu(2) = 1.d0
    typcst(1) = 'R'
    typcst(2) = 'R'
!
! - Get numbering object for discrete friction methods
! 
    numedf = ds_contact%nume_dof_frot
    call detrsd('NUME_DDL', numedf)
!
    call infmue()
    call mtcmbl(2, typcst, coefmu, limat, matass,&
                ' ', numedf, 'ELIM1')
    call infbav()
    call dismoi('NOM_NUME_DDL', matrcf, 'MATR_ASSE', repk=numedf)
    call detrsd('MATR_ASSE', matrcf)
    call detrsd('NUME_DDL', numedf)
!
999 continue
!
    call jedema()
!
end subroutine
