subroutine op0013()
    implicit none
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
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/getvid.h"
#include "asterfort/assvec.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: nu, vecas, k8, vprof
    character(len=16) :: typv, oper
    integer :: type
!
!     FONCTIONS JEVEUX
!
!
!
!
    character(len=19) :: ch19
    integer :: iarg
!-----------------------------------------------------------------------
    integer :: i, ibid, ifm, ilicoe, ilivec, nbvec, niv
!
!-----------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
    call getres(vecas, typv, oper)
!
    call getvid(' ', 'VECT_ELEM', 0, iarg, 0,&
                k8, nbvec)
    nbvec = -nbvec
!
!
    call jecreo(vecas//'.LI2VECEL', 'V V K8 ')
    call jeecra(vecas//'.LI2VECEL', 'LONMAX', nbvec, ' ')
    call jeveuo(vecas//'.LI2VECEL', 'E', ilivec)
    call getvid(' ', 'VECT_ELEM', 0, iarg, nbvec,&
                zk8(ilivec), nbvec)
    call gettco(zk8(ilivec), typv)
    if (typv(16:16) .eq. 'R') type=1
    if (typv(16:16) .eq. 'C') type=2
!
!
    call jecreo(vecas//'.LICOEF', 'V V R ')
    call jeecra(vecas//'.LICOEF', 'LONMAX', nbvec, ' ')
    call jeveuo(vecas//'.LICOEF', 'E', ilicoe)
    do 5 i = 1, nbvec
        zr(ilicoe-1+i) = 1.0d0
 5  end do
!
    call getvid(' ', 'NUME_DDL', 0, iarg, 1,&
                nu, ibid)
    vprof = '        '
    call assvec('G', vecas, nbvec, zk8(ilivec), zr(ilicoe),&
                nu, vprof, 'ZERO', type)
    ch19 = vecas
    call jedetr(ch19//'.LILI')
    call jedetr(ch19//'.ADNE')
    call jedetr(ch19//'.ADLI')
!
    call jedema()
end subroutine
