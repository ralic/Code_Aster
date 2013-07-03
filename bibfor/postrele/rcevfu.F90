subroutine rcevfu(cnoc, cfat, fut)
    implicit     none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    real(kind=8) :: fut
    character(len=24) :: cnoc, cfat
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
!     CALCUL DU FACTEUR D'USAGE TOTAL (FUT)
!
!     ------------------------------------------------------------------
!
    integer :: nbinst, jnocr, jfu, i1, i2, ind, noc1, noc2, i1m, i2m, noc1m
    integer :: noc2m, nbcycl
    integer :: jnock, jnocl, jfukl, indi, inds, k, l, ifm, niv
    real(kind=8) :: fum, fukl
    logical :: encore
    character(len=8) :: k8b
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    call jelira(cnoc, 'LONMAX', nbinst, k8b)
    call jeveuo(cnoc, 'L', jnocr)
    call jeveuo(cfat, 'L', jfu)
!
    fut = 0.d0
!
    call wkvect('&&RCEVFU.NB_OCC_K', 'V V I', nbinst, jnock)
    call wkvect('&&RCEVFU.NB_OCC_L', 'V V I', nbinst, jnocl)
    do 10 i1 = 1, nbinst
        zi(jnock+i1-1) = zi(jnocr+i1-1)
        zi(jnocl+i1-1) = zi(jnocr+i1-1)
10  end do
!
    call wkvect('&&RCEVFU.MATR_FU', 'V V R', nbinst*nbinst, jfukl)
    ind = 0
    do 20 i1 = 1, nbinst
        indi = nbinst*(i1-1) + i1
        ind = ind + 1
        zr(jfukl-1+indi) = zr(jfu-1+5*(ind-1)+4)
        do 22 i2 = i1+1, nbinst
            inds = nbinst*(i1-1) + i2
            indi = nbinst*(i2-1) + i1
            ind = ind + 1
            zr(jfukl-1+inds) = zr(jfu-1+5*(ind-1)+4)
            zr(jfukl-1+indi) = zr(jfu-1+5*(ind-1)+4)
22      continue
20  end do
!
    ifm = 6
    ind = 0
!
100  continue
    ind = ind + 1
!
    if (niv .eq. 2) then
        if (ind .eq. 1) then
            write(ifm,*) 'MATRICE FACTEUR D''USAGE INITIALE'
        else
            write(ifm,*) 'MATRICE FACTEUR D''USAGE MODIFIEE'
        endif
        write(ifm,1010) ( zi(jnocl+l-1), l=1,nbinst )
        do 700 k = 1, nbinst
            i1 = nbinst*(k-1)
            write(ifm,1000) zi(jnock+k-1), (zr(jfukl-1+i1+l), l=1,&
            nbinst)
700      continue
    endif
!
    fum = 0.d0
!
    do 110 i1 = 1, nbinst
        noc1 = zi(jnock-1+i1)
        if (noc1 .eq. 0) goto 110
        k = nbinst*(i1-1)
!
        do 112 i2 = 1, nbinst
            noc2 = zi(jnocl-1+i2)
            if (noc2 .eq. 0) goto 112
            l = i2
!
            fukl = zr(jfukl-1+k+l)
            if (fukl .lt. r8prem()) goto 112
            if (fukl .gt. fum) then
                noc1m = noc1
                noc2m = noc2
                i1m = i1
                i2m = i2
                fum = fukl
            endif
!
112      continue
!
110  end do
    nbcycl = min( noc1m , noc2m )
!
    if (fum .lt. r8prem()) goto 999
    if (niv .eq. 2) then
        write(ifm,1020)'=> FACTEUR D''USAGE MAXI: ',fum,i1m,i2m
        write(ifm,1030)'   NB_OCCUR = ', nbcycl
    endif
!
! --- ON CUMULE
!
    fut = fut + fum*dble(nbcycl)
!
! --- ON MET A ZERO LES FACTEURS D'USAGE INCRIMINES
!
    if (noc1m .eq. noc2m) then
        zi(jnocl-1+i1m) = 0
        zi(jnocl-1+i2m) = 0
        zi(jnock-1+i1m) = 0
        zi(jnock-1+i2m) = 0
        do 120 k = 1, nbinst
            zr(jfukl-1+(k-1)*nbinst+i2m) = 0.d0
            zr(jfukl-1+(i2m-1)*nbinst+k) = 0.d0
            zr(jfukl-1+(k-1)*nbinst+i1m) = 0.d0
            zr(jfukl-1+(i1m-1)*nbinst+k) = 0.d0
120      continue
    else if (noc1m .lt. noc2m) then
        zi(jnocl-1+i2m) = zi(jnocl-1+i2m) - noc1m
        zi(jnock-1+i2m) = zi(jnock-1+i2m) - noc1m
        zi(jnock-1+i1m) = 0
        zi(jnocl-1+i1m) = 0
        do 122 k = 1, nbinst
            zr(jfukl-1+(i1m-1)*nbinst+k) = 0.d0
            zr(jfukl-1+(k-1)*nbinst+i1m) = 0.d0
122      continue
    else
        zi(jnocl-1+i2m) = 0
        zi(jnock-1+i2m) = 0
        zi(jnocl-1+i1m) = zi(jnocl-1+i1m) - noc2m
        zi(jnock-1+i1m) = zi(jnock-1+i1m) - noc2m
        do 124 k = 1, nbinst
            zr(jfukl-1+(k-1)*nbinst+i2m) = 0.d0
            zr(jfukl-1+(i2m-1)*nbinst+k) = 0.d0
124      continue
    endif
!
! --- EXISTE-T-IL DES ETATS TELS QUE NB_OCCUR > 0
!
    encore = .false.
    do 200 i1 = 1, nbinst
        if (zi(jnock-1+i1) .gt. 0) then
            encore = .true.
        endif
200  continue
    if (encore) goto 100
!
999  continue
!
    if (niv .eq. 2) write(ifm,*)'-->> FACTEUR D''USAGE CUMULE = ', fut
!
    call jedetr('&&RCEVFU.NB_OCC_K')
    call jedetr('&&RCEVFU.NB_OCC_L')
    call jedetr('&&RCEVFU.MATR_FU')
!
    1000 format(1p,i10,'|',40(e10.3,'|'))
    1010 format(1p,'   NB_OCCUR ','|',40(i10,'|'))
    1020 format(1p,a28,e12.5,', LIGNE:',i4,', COLONNE:',i4)
    1030 format(1p,a15,i8)
!
    call jedema()
end subroutine
