subroutine op0071()
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!
!     CALCUL PROJECTION MATRICE SUR BASE DE RITZ
!
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/gettco.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/projmc.h"
#include "asterfort/projmr.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
    integer :: ibid, n1, n2, n3, n4, nbmode(1), neq
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    character(len=1) :: typmat
    character(len=8) :: k8b, nomres, basemo, matras, numgen
    character(len=14) :: nu, numdd1, numdd2
    character(len=16) :: typres, nomcom, typbas
    character(len=14) :: nugene
    character(len=24) :: matric
    integer, pointer :: smde(:) => null()
!-----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    call getres(nomres, typres, nomcom)
!
! --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getvid(' ', 'MATR_ASSE', scal=matras, nbret=n1)
    call getvid(' ', 'MATR_ASSE_GENE', scal=matras, nbret=n3)
    call getvid(' ', 'BASE', scal=basemo, nbret=n4)
    call getvid(' ', 'NUME_DDL_GENE', scal=numgen, nbret=n2)
    nugene=numgen
!
    call gettco(basemo, typbas)
    typmat= typres(16:16)
!
    if (n2 .ne. 0) then
    endif
!
!
!==================================================
!
    call rsorac(basemo, 'LONUTI', 0, rbid, k8b,&
                cbid, rbid, 'ABSOLU', nbmode, 1,&
                ibid)
!
! RECUPERATION DU NOMBRE DE MODES REDUIT,
! NB_VECT DONNE PAR NUME_DDL_GENE
    call jeveuo(nugene//'.SMOS.SMDE', 'L', vi=smde)
    nbmode(1) = smde(1)
!
!
    call dismoi('NOM_NUME_DDL', matras, 'MATR_ASSE', repk=numdd1)
    call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric, arret='C')
    if (matric .ne. ' ') then
        call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=numdd2)
    else
        call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=numdd2)
    endif
    if (numdd1 .ne. numdd2) then
        call utmess('I', 'ALGORITH9_39')
    endif
    nu = numdd1(1:14)
    call dismoi('NB_EQUA', matras, 'MATR_ASSE', repi=neq)
!
    if (typmat .eq. 'R') then
        call projmr(matras, nomres, basemo, nugene, nu,&
                    neq, nbmode(1))
    else if (typmat.eq.'C') then
        call projmc(matras, nomres, basemo, nugene, nu,&
                    neq, nbmode(1))
    else
        call utmess('F', 'ALGORITH9_40', sk=typmat)
    endif
!
!
    call jedema()
end subroutine
