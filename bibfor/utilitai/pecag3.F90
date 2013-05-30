subroutine pecag3(ndim, nsymx, nsymy, noma, motcle,&
                  nbmail, noment, valpar)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8dgrd.h'
    include 'asterc/r8maem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    integer :: ndim, nbmail
    real(kind=8) :: valpar(*)
    character(len=*) :: noment(*), noma
    character(len=*) :: motcle
    logical :: nsymx, nsymy
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "CARA_GEOM"
!     ------------------------------------------------------------------
!
!
    character(len=8) :: k8b, noma8
    character(len=24) :: mlggma, mlgval, mlgcox
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ibid, ig, im, in, jcoor, jdes
    integer :: jgro, nbma, nbno, nbnoeu, numail, nuno
    real(kind=8) :: alpha, cdx, cdy, cosa, r
    real(kind=8) :: rmax, rx, ry, sina, tamp, x, x0
    real(kind=8) :: xmax, xmin, y, y0, ymax, ymin, zmax
    real(kind=8) :: zmin
!-----------------------------------------------------------------------
    call jemarq()
    noma8=noma
    mlggma = noma8//'.GROUPEMA'
    mlgcox = noma8//'.CONNEX'
    mlgval = noma8//'.COORDO    .VALE'
    call jeveuo(mlgval, 'L', jcoor)
    call jelira(mlgval, 'LONMAX', nbnoeu, k8b)
    nbnoeu = nbnoeu / 3
!
    if (ndim .eq. 2) then
        cdx = valpar(13)
        cdy = valpar(14)
        alpha = r8dgrd() * valpar(20)
        cosa = cos ( alpha )
        sina = sin ( alpha )
    else
        call u2mess('F', 'UTILITAI3_48')
        cdx = valpar(19)
        cdy = valpar(20)
    endif
    xmax = -r8maem()
    xmin = r8maem()
    ymax = -r8maem()
    ymin = r8maem()
    zmax = -r8maem()
    zmin = r8maem()
    rmax = -r8maem()
!
!
    if (motcle(1:4) .eq. 'TOUT') then
        do 10 i = 1, nbnoeu
            x0 = zr(jcoor-1+3*(i-1)+1) - cdx
            y0 = zr(jcoor-1+3*(i-1)+2) - cdy
            x = x0*cosa + y0*sina
            y = y0*cosa - x0*sina
            r = sqrt ( x*x + y*y )
            xmax = max ( xmax , x )
            xmin = min ( xmin , x )
            ymax = max ( ymax , y )
            ymin = min ( ymin , y )
            zmax = 0.d0
            zmin= 0.d0
            rmax = max ( rmax , r )
10      continue
!
    else if (motcle(1:6) .eq. 'MAILLE') then
        do 20 im = 1, nbmail
            call jenonu(jexnom(noma8//'.NOMMAI', noment(im)), ibid)
            call jeveuo(jexnum(mlgcox, ibid), 'L', jdes)
            call jelira(jexnum(mlgcox, ibid), 'LONMAX', nbno, k8b)
            do 22 in = 1, nbno
                nuno = zi(jdes+in-1)
                x0 = zr(jcoor-1+3*(nuno-1)+1)-cdx
                y0 = zr(jcoor-1+3*(nuno-1)+2)-cdy
                x = x0*cosa + y0*sina
                y = y0*cosa - x0*sina
                r = sqrt ( x*x + y*y )
                xmax = max ( xmax , x )
                xmin = min ( xmin , x )
                ymax = max ( ymax , y )
                ymin = min ( ymin , y )
                zmax = 0.d0
                zmin = 0.d0
                rmax = max ( rmax , r )
22          continue
20      continue
!
    else if (motcle(1:8) .eq. 'GROUP_MA') then
        do 30 ig = 1, nbmail
            call jeveuo(jexnom(mlggma, noment(ig)), 'L', jgro)
            call jelira(jexnom(mlggma, noment(ig)), 'LONUTI', nbma, k8b)
            do 32 im = 1, nbma
                numail = zi(jgro+im-1)
                call jeveuo(jexnum(mlgcox, numail), 'L', jdes)
                call jelira(jexnum(mlgcox, numail), 'LONMAX', nbno, k8b)
                do 34 in = 1, nbno
                    nuno = zi(jdes+in-1)
                    x0 = zr(jcoor-1+3*(nuno-1)+1)-cdx
                    y0 = zr(jcoor-1+3*(nuno-1)+2)-cdy
                    x = x0*cosa + y0*sina
                    y = y0*cosa - x0*sina
                    r = sqrt ( x*x + y*y )
                    xmax = max ( xmax , x )
                    xmin = min ( xmin , x )
                    ymax = max ( ymax , y )
                    ymin = min ( ymin , y )
                    zmax = 0.d0
                    zmin = 0.d0
                    rmax = max ( rmax , r )
34              continue
32          continue
30      continue
    endif
    rx=max(abs(xmax),(abs(xmin)))
    ry=max(abs(ymax),(abs(ymin)))
!
    if (nsymx) then
        x0 = 1.d0
        y0 = 0.d0
        x = x0*cosa + y0*sina
        y = y0*cosa - x0*sina
        if (abs(abs(x)-1.d0) .le. 1.d-5) then
            tamp = max( abs(ymin) , abs(ymax) )
            ymin = -tamp
            ymax = tamp
        else
            tamp = max( abs(xmin) , abs(xmax) )
            xmin = -tamp
            xmax = tamp
        endif
    endif
    if (nsymy) then
        x0 = 0.d0
        y0 = 1.d0
        x = x0*cosa + y0*sina
        y = y0*cosa - x0*sina
        if (abs(abs(y)-1.d0) .le. 1.d-5) then
            tamp = max( abs(xmin) , abs(xmax) )
            xmin = -tamp
            xmax = tamp
        else
            tamp = max( abs(ymin) , abs(ymax) )
            ymin = -tamp
            ymax = tamp
        endif
    endif
    if (ndim .eq. 2) then
        valpar( 7) = xmax
        valpar( 8) = ymax
        valpar( 9) = xmin
        valpar(10) = ymin
        valpar(11) = rmax
        valpar(42) = rx
        valpar(43) = ry
    else
        valpar(11) = xmax
        valpar(12) = ymax
        valpar(13) = zmax
        valpar(14) = xmin
        valpar(15) = ymin
        valpar(16) = zmin
        valpar(17) = rmax
    endif
!
    call jedema()
!
end subroutine
