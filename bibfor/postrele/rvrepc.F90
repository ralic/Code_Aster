subroutine rvrepc(courbe, repere, sdnewr)
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
#include "jeveux.h"
!
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rvrthe.h"
    character(len=19) :: sdnewr
    character(len=8) :: courbe, repere
!
!***********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     CALCUL  DU REPERE LOCAL OU POLAIRE LA LONG D' UNE COURBE
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     COURBE : NOM DU CONCEPT COURBE
!     REPERE : VAUT 'LOCAL' OU 'POLAIRE'
!
!  ARGUMENTS EN SORTIE
!  -------------------
!
!     SDNEWR : NOM DE LA SD DU REPERE CALCULE
!              (DOC. C.F. RVCHGR)
!
!***********************************************************************
!
!  -----------------------------------------
!
!
!
!  ---------------------------------
!
!  VARIABLES LOCALES
!  -----------------
!
    character(len=14) :: nabscr, nabsce, ncnxor, ncnxex
    character(len=24) :: nvec1, nvec2
    integer :: aabscr, avec1, avec2, aasgt, absgt, acarc, asarc, ararc
    integer :: nbpart, nbsgt, nbarc, ipart, nbpt, ipt, aabsce, acnxe, acnxo
    integer :: nbcx, icx, pt, dcx, fcx
    real(kind=8) :: x, y, xa, xb, xc, ya, yb, yc, r, s, t1, t2, n1, n2
!
!====================== CORPS DE LA ROUTINE ===========================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    nvec1 = sdnewr//'.VEC1'
    nvec2 = sdnewr//'.VEC2'
    nabscr = courbe//'.ORSGT'
    nabsce = courbe//'.EXSGT'
    ncnxor = courbe//'.CNXOR'
    ncnxex = courbe//'.CNXEX'
!
    call jelira(courbe//'.XYASGT', 'LONMAX', nbsgt)
    call jelira(courbe//'.XYCARC', 'LONMAX', nbarc)
    call jeveuo(courbe//'.XYASGT', 'L', aasgt)
    call jeveuo(courbe//'.XYBSGT', 'L', absgt)
    call jeveuo(courbe//'.XYCARC', 'L', acarc)
    call jeveuo(courbe//'.XRARC', 'L', ararc)
    call jeveuo(courbe//'.XSARC', 'L', asarc)
!
    nbsgt = (nbsgt/2) - 1
    nbarc = (nbarc/2) - 1
    nbpart = nbarc + nbsgt
!
    call jecrec(nvec1, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
    call jecrec(nvec2, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbpart)
!
    do 100, ipart = 1, nbpart, 1
!
    call jelira(jexnum(ncnxor, ipart), 'LONMAX', nbcx)
    call jeveuo(jexnum(ncnxor, ipart), 'L', acnxo)
    call jeveuo(jexnum(ncnxex, ipart), 'L', acnxe)
    call jelira(jexnum(nabscr, ipart), 'LONMAX', nbpt)
    call jeveuo(jexnum(nabscr, ipart), 'L', aabscr)
    call jeveuo(jexnum(nabsce, ipart), 'L', aabsce)
    call jecroc(jexnum(nvec1, ipart))
    call jeecra(jexnum(nvec1, ipart), 'LONMAX', 2*(nbpt+nbcx))
    call jeveuo(jexnum(nvec1, ipart), 'E', avec1)
    call jecroc(jexnum(nvec2, ipart))
    call jeecra(jexnum(nvec2, ipart), 'LONMAX', 2*(nbpt+nbcx))
    call jeveuo(jexnum(nvec2, ipart), 'E', avec2)
!
    xa = zr(aasgt + 2*ipart+1 -1)
    ya = zr(aasgt + 2*ipart+2 -1)
    xb = zr(absgt + 2*ipart+1 -1)
    yb = zr(absgt + 2*ipart+2 -1)
    xc = zr(acarc + 2*ipart+1 -1)
    yc = zr(acarc + 2*ipart+2 -1)
    r = zr(ararc + ipart+1 -1)
!
    pt = 1
!
    do 110, icx = 1, nbcx, 1
!
    dcx = zi(acnxo + icx-1)
    fcx = zi(acnxe + icx-1)
!
    do 120, ipt = dcx, fcx + 1, 1
!
    if (ipt .le. fcx) then
!
        s = zr(aabscr + ipt -1)
!
    else
!
        s = zr(aabsce + ipt-1 -1)
!
    endif
!
    if (ipart .le. nbsgt) then
!
        x = xb - xa
        y = yb - ya
!
        if (repere .eq. 'POLAIRE') then
!
            x = xa + s*x
            y = ya + s*y
!
            call rvrthe(x, y, t1, t2, n1,&
                        n2)
!
        else
!
            if (ipt .eq. 1) then
!
                call rvrthe(x, y, t1, t2, n1,&
                            n2)
!
            endif
!
        endif
!
    else
!
        n1 = cos(s)
        n2 = sin(s)
!
        if (repere .eq. 'LOCAL') then
!
            t1 = -n2
            t2 = n1
!
        else
!
            x = xc + r*n1
            y = yc + r*n2
!
            call rvrthe(x, y, n1, n2, t1,&
                        t2)
!
        endif
!
    endif
!
    zr(avec1 + 2*pt-1 -1) = t1
    zr(avec1 + 2*pt -1) = t2
    zr(avec2 + 2*pt-1 -1) = n1
    zr(avec2 + 2*pt -1) = n2
!
    pt = pt + 1
!
120  continue
!
110  continue
!
    100 end do
!
    call jedema()
end subroutine
