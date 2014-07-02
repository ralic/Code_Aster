subroutine rcfodi(ifon, beta, f, df)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/utmess.h"
    integer :: ifon
    real(kind=8) :: beta, f, df
! ......................................................................
!     ROUTINE INVERSE DE RCFODE :
!     UTILISEE UNIQUEMENT POUR LA FONCTION ENTHALPIE DANS
!     L OPERATEUR THER_NON_LINE_MO (REPERE MOBILE)
!     LA FONCTION FOURNIE PAR LE MATERIAU CODE EST BETA=F(TEMPERATURE)
!     ON CALCULE ICI TEMPERATURE=F-1(BETA), DE MEME QUE LA DERIVEE DE
!     CETTE FONCTION.
! IN   IFON   : ADRESSE DANS LE MATERIAU CODE DE LA FONCTION
! IN   BETA   : ENTHALPIE AU POINT DE GAUSS CONSIDERE
! OUT  F      : VALEUR DE LA FONCTION (TEMPERATURE)
! OUT  DF     : VALEUR DE LA DERIVEE DE LA FONCTION
!
!
!
!
    integer :: jpro, jvalf, jv, jp, nbvf
    integer :: isave, ideb, ifin, incr, indfct
    aster_logical :: tesinf, tessup, entre, deja, avant
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
!
    parameter  ( indfct = 7 )
! DEB ------------------------------------------------------------------
    nbvf = zi(ifon)
    jpro = zi(ifon+1)
    jvalf = zi(ifon+2)
    if (zk24(jpro)(1:1) .eq. 'C') then
        f = zr(jvalf+nbvf+1)
        df = 0.d0
        goto 101
    endif
    isave = zi(ifon+indfct)
    if (zk24(jpro)(1:1) .eq. 'N') then
!
!---- NAPPE - IMPOSSIBLE
!
        call utmess('F', 'MODELISA6_58')
    endif
!
    deja = beta.ge.zr(jvalf+isave+nbvf-1) .and. beta.le.zr(jvalf+isave+nbvf)
    avant = beta.lt.zr(jvalf+isave+nbvf-1)
    tesinf = beta.lt.zr(jvalf+nbvf)
    tessup = beta.gt.zr(jvalf+nbvf+nbvf-1)
    entre = .not. tesinf .and. .not. tessup
    if (deja) then
        jp = jvalf + isave
        jv = jp + nbvf
        df = (zr(jp)-zr(jp-1))/(zr(jv)-zr(jv-1))
        f = df*(beta-zr(jv-1))+zr(jp-1)
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
                if (zr(jv) .ge. beta) then
                    df = (zr(jp)-zr(jp-1))/(zr(jv)-zr(jv-1))
                    f = df*(beta-zr(jv-1))+zr(jp-1)
                    isave = jp-jvalf
                    goto 5
                endif
  8         continue
  5         continue
        else
            do 9 jp = ideb, ifin, incr
                jv = jp + nbvf
                if (zr(jv) .le. beta) then
                    df = (zr(jp+1)-zr(jp))/(zr(jv+1)-zr(jv))
                    f = df*(beta-zr(jv))+zr(jp)
                    isave = jp-jvalf+1
                    goto 6
                endif
  9         continue
  6         continue
        endif
    else if (tesinf) then
        jv = jvalf+nbvf
        jp = jvalf
        if (zk24(jpro+4)(2:2) .eq. 'C') then
            df = 0.0d0
            f = zr(jp)
        else if (zk24(jpro+4)(1:1).eq.'L') then
            df = (zr(jp+1)-zr(jp))/(zr(jv+1)-zr(jv))
            f = df*(beta-zr(jv))+zr(jp)
        else if (zk24(jpro+4)(1:1).eq.'E') then
            call utmess('F', 'MODELISA4_63')
        endif
        isave = 1
    else if (tessup) then
        jv = jvalf + 2*nbvf - 1
        jp = jvalf + nbvf - 1
        if (zk24(jpro+4)(2:2) .eq. 'C') then
            df = 0.0d0
            f = zr(jp)
        else if (zk24(jpro+4)(2:2).eq.'L') then
            df = (zr(jp)-zr(jp-1))/(zr(jv)-zr(jv-1))
            f = df*(beta-zr(jv-1))+zr(jp-1)
        else if (zk24(jpro+4)(2:2).eq.'E') then
            call utmess('F', 'MODELISA4_65')
        endif
        isave = nbvf - 1
    endif
100 continue
    zi(ifon+indfct) = isave
101 continue
! FIN ------------------------------------------------------------------
end subroutine
