subroutine cnomax(cnoz, ncmp, licmp, rmax, numno)
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
    implicit      none
#include "jeveux.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsred.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: cnoz
    integer :: ncmp
    character(len=8) :: licmp(ncmp)
    real(kind=8) :: rmax
    integer :: numno
!
! ======================================================================
! ROUTINE APPELEE PAR : CVGCNT
! ======================================================================
!
! CALCULER LE MAX DE LA NORME DU DEPL. (DX DY DZ) DE CNO
!
! IN  CNO    : SD CHAM_NO
! IN  NCMP   : NOMBRE DE COMPOSANTES DANS LICMP
! IN  LICMP  : COMPOSANTES SUR LESQUELLES LE MAX EST CALCULE
! OUT RMAX   : MAX DE LA NORME DU DEPL.
! OUT NUMNO  : NUMERO DU NOEUD REALISANT LE MAX DE DEPL.
!
!
!
!
    integer :: jcnsd, jcnsv, jcnsl
    integer :: nbno, k, ino
    character(len=19) :: cno, cns1, cns
    real(kind=8) :: norme
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    cno = cnoz
    rmax = 0.d0
    cns1 = '&&CNOMAX.CNS1'
    cns = '&&CNOMAX.CNS'
    numno = 0
    call cnocns(cno, 'V', cns1)
    call cnsred(cns1, 0, 0, ncmp, licmp,&
                'V', cns)
!
    call jeveuo(cns//'.CNSD', 'L', jcnsd)
    call jeveuo(cns//'.CNSV', 'L', jcnsv)
    call jeveuo(cns//'.CNSL', 'L', jcnsl)
!
    nbno = zi(jcnsd-1+1)
    do 10,ino = 1,nbno
    norme=0.d0
    do 30,k = 1,ncmp
    if (zl(jcnsl-1+ (ino-1)*ncmp+k)) then
        norme=norme+zr(jcnsv-1+ (ino-1)*ncmp+k)**2
    endif
30  continue
    if (sqrt(norme) .ge. rmax) then
        rmax = sqrt(norme)
        numno = ino
    endif
    10 end do
!
!
    call detrsd('CHAM_NO_S', cns1)
    call detrsd('CHAM_NO_S', cns)
    call jedema()
end subroutine
