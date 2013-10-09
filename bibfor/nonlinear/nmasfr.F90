subroutine nmasfr(defico, resoco, matass)
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
    implicit none
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
    character(len=24) :: resoco
    character(len=19) :: matass
    character(len=24) :: defico
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - ALGORITHME)
!
! CREATION DE LA MATRICE DE CONTACT/FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO  : SD DE DEFINITION DU CONTACT
! IN  RESOCO  : SD CONTACT
! I/O MATASS  : IN  MATR_ASSE TANGENTE
!               OUT MATR_ASSE TANGENTE + MATRICE CONTACT/FROTTEMENT
!                  (EVENTUELLE)
!
!
!
!
    integer :: ifm, niv
    character(len=14) :: numedf
    character(len=24) :: nosdco
    integer :: jnosdc
    integer :: nbliac
    character(len=19) :: matrcf
    character(len=24) :: limat(2)
    real(kind=8) :: coefmu(2)
    character(len=1) :: typcst(2)
    logical :: lmodim
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    lmodim = cfdisl(defico,'MODI_MATR_GLOB')
    nbliac = cfdisd(resoco,'NBLIAC')
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
    matrcf = resoco(1:14)//'.MATR'
    nosdco = resoco(1:14)//'.NOSDCO'
    call jeveuo(nosdco, 'L', jnosdc)
    limat(1) = matass
    limat(2) = matrcf
    coefmu(1) = 1.d0
    coefmu(2) = 1.d0
    typcst(1) = 'R'
    typcst(2) = 'R'
    numedf = zk24(jnosdc+1-1)(1:14)
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
