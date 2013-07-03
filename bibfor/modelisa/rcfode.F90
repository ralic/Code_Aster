subroutine rcfode(ifon, temp, f, df)
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
    implicit none
#include "jeveux.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
    integer :: ifon
    real(kind=8) :: temp, f, df
! ......................................................................
!     OBTENTION DE LA VALEUR DE LA FONCTION ET DE SA DERIVEE POUR UNE
!     FONCTION DE LA TEMPERATURE LINEAIRE PAR MORCEAU
! IN   IFON   : ADRESSE DANS LE MATERIAU CODE DE LA FONCTION
! IN   TEMP   : TEMPERATURE AU POINT DE GAUSS CONSIDERE
! OUT  F      : VALEUR DE LA FONCTION
! OUT  DF     : VALEUR DE LA DERIVEE DE LA FONCTION
!
!
    real(kind=8) :: valr
!
!
    integer :: jpro, jvalf, jv, jp, nbvf
    logical :: tesinf, tessup, entre, deja, avant
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
!
!-----------------------------------------------------------------------
    integer :: ideb, ifin, incr, indfct, isave
!-----------------------------------------------------------------------
    parameter  ( indfct = 7 )
! DEB ------------------------------------------------------------------
    jpro = zi(ifon+1)
    jvalf = zi(ifon+2)
    if (zk24(jpro)(1:1) .eq. 'C') then
        f = zr(jvalf+1)
        df = 0.d0
        goto 101
    else if (zk24(jpro)(1:1).eq.'I') then
        call u2mess('F', 'MODELISA6_31')
    endif
    isave = zi(ifon+indfct)
    if (zk24(jpro)(1:1) .eq. 'N') then
!
!---- NAPPE - IMPOSSIBLE
!
        call u2mess('F', 'MODELISA6_58')
    endif
    nbvf = zi(ifon)
!
    deja = temp.ge.zr(jvalf+isave-1) .and. temp.le.zr(jvalf+isave)
    avant = temp.lt.zr(jvalf+isave-1)
    tesinf = temp.lt.zr(jvalf)
    tessup = temp.gt.zr(jvalf+nbvf-1)
    entre = .not. tesinf .and. .not. tessup
    if (deja) then
        jp = jvalf + isave
        jv = jp + nbvf
        df = (zr(jv)-zr(jv-1))/(zr(jp)-zr(jp-1))
        f = df*(temp-zr(jp-1))+zr(jv-1)
        goto 100
    else
        if (avant) then
            ifin = jvalf
            ideb = jvalf+isave-1
            incr = -1
        else
            ideb = jvalf+isave
            ifin = jvalf+nbvf-1
            incr = 1
        endif
    endif
    if (entre) then
        if (incr .gt. 0) then
            do 8 jp = ideb, ifin, incr
                jv = jp + nbvf
                if (zr(jp) .ge. temp) then
                    df = (zr(jv)-zr(jv-1))/(zr(jp)-zr(jp-1))
                    f = df*(temp-zr(jp-1))+zr(jv-1)
                    isave = jp-jvalf
                    goto 5
                endif
 8          continue
 5          continue
        else
            do 9 jp = ideb, ifin, incr
                jv = jp + nbvf
                if (zr(jp) .le. temp) then
                    df = (zr(jv+1)-zr(jv))/(zr(jp+1)-zr(jp))
                    f = df*(temp-zr(jp))+zr(jv)
                    isave = jp-jvalf+1
                    goto 6
                endif
 9          continue
 6          continue
        endif
    else if (tesinf) then
        jv = jvalf+nbvf
        jp = jvalf
        if (zk24(jpro+4)(2:2) .eq. 'C') then
            df = 0.0d0
            f = zr(jv)
        else if (zk24(jpro+4)(1:1).eq.'L') then
            df = (zr(jv+1)-zr(jv))/(zr(jp+1)-zr(jp))
            f = df*(temp-zr(jp))+zr(jv)
        else if (zk24(jpro+4)(1:1).eq.'E') then
            valr = temp
            call u2mesg('F', 'MODELISA8_93', 0, ' ', 0,&
                        0, 1, valr)
        endif
        isave = 1
    else if (tessup) then
        jv = jvalf + 2*nbvf - 1
        jp = jvalf + nbvf - 1
        if (zk24(jpro+4)(2:2) .eq. 'C') then
            df = 0.0d0
            f = zr(jv)
        else if (zk24(jpro+4)(2:2).eq.'L') then
            df = (zr(jv)-zr(jv-1))/(zr(jp)-zr(jp-1))
            f = df*(temp-zr(jp-1))+zr(jv-1)
        else if (zk24(jpro+4)(2:2).eq.'E') then
            valr = temp
            call u2mesg('F', 'MODELISA8_94', 0, ' ', 0,&
                        0, 1, valr)
        endif
        isave = nbvf - 1
    endif
100  continue
    zi(ifon+indfct) = isave
101  continue
! FIN ------------------------------------------------------------------
end subroutine
