subroutine te0064(option, nomte)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/rcadma.h"
#include "asterfort/zacier.h"
#include "asterfort/zedgar.h"
    character(len=16) :: option, nomte
! ......................................................................
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
!
!    - FONCTION REALISEE:  CALCUL DE Z EN 3D
!                          CHGT DE PHASE METALLURGIQUE
!                          OPTION : 'META_ELGA_TEMP'ET'META_ELNO'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
!
    character(len=16) :: compor
    integer :: icodre
    real(kind=8) :: dt10, dt21, instp
    real(kind=8) :: tno1, tno0, tno2
    real(kind=8) :: metaac(189), metazi(108)
    integer :: jgano, nno, kn, i, itempe, itempa, itemps, iadtrc
    integer :: ipoids, ivf, imate, ndim, npg
    integer :: nbhist, itempi, nbtrc, iadckm, nnos
    integer :: ipftrc, jftrc, jtrc, iphasi, iphasn, icompo
    integer :: matos, nbcb1, nbcb2, nblexp, iadexp, idfde
!
    call jemarq()
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPAR', 'L', itempa)
    call jevech('PTEMPER', 'L', itempe)
    call jevech('PTEMPIR', 'L', itempi)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PPHASIN', 'L', iphasi)
    call jevech('PCOMPOR', 'L', icompo)
!
    call jevech('PPHASNOU', 'E', iphasn)
!
    compor = zk16(icompo)
    matos = zi(imate)
!
    if (compor .eq. 'ACIER') then
!
        call jevech('PFTRC', 'L', ipftrc)
        jftrc = zi(ipftrc)
        jtrc = zi(ipftrc+1)
!
        call rcadma(matos, 'META_ACIER', 'TRC', iadtrc, icodre,&
                    1)
!
        nbcb1 = nint(zr(iadtrc+1))
        nbhist = nint(zr(iadtrc+2))
        nbcb2 = nint(zr(iadtrc+1+2+nbcb1*nbhist))
        nblexp = nint(zr(iadtrc+1+2+nbcb1*nbhist+1))
        nbtrc = nint(zr(iadtrc+1+2+nbcb1*nbhist+2+nbcb2*nblexp+1))
        iadexp = 5 + nbcb1*nbhist
        iadckm = 7 + nbcb1*nbhist + nbcb2*nblexp
!
        do 10 kn = 1, nno
!         -- ATTENTION: ZACIER MODIFIE PARFOIS DT10 ET DT21 :
            dt10 = zr(itemps+1)
            dt21 = zr(itemps+2)
!
            tno1 = zr(itempe+kn-1)
            tno0 = zr(itempa+kn-1)
            tno2 = zr(itempi+kn-1)
            call zacier(matos, nbhist, zr(jftrc), zr(jtrc), zr(iadtrc+3),&
                        zr(iadtrc+iadexp), zr(iadtrc+iadckm), nbtrc, tno0, tno1,&
                        tno2, dt10, dt21, zr(iphasi+7*(kn-1)), metaac(1+7*(kn-1)))
!
            do 20 i = 1, 7
                zr(iphasn+7*(kn-1)+i-1) = metaac(1+7*(kn-1)+i-1)
20          continue
10      continue
!
    else if (compor(1:4).eq.'ZIRC') then
!
        dt10 = zr(itemps+1)
        dt21 = zr(itemps+2)
        instp= zr(itemps)+dt21
!
        do 30 kn = 1, nno
            tno1 = zr(itempe+kn-1)
            tno2 = zr(itempi+kn-1)
            call zedgar(matos, tno1, tno2, instp, dt21,&
                        zr(iphasi+4*(kn-1) ), metazi(1+4*(kn-1)))
!
            do 40 i = 1, 4
                zr(iphasn+4*(kn-1)+i-1) = metazi(1+4*(kn-1)+i-1)
40          continue
30      continue
!
    endif
!
    call jedema()
end subroutine
