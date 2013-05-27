subroutine foderi(nomfon, temp, f, df)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveut.h'
    include 'asterfort/u2mess.h'
    character(len=*) :: nomfon
    real(kind=8) :: temp, f, df
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ......................................................................
!     OBTENTION DE LA VALEUR DE LA FONCTION ET DE SA DERIVEE POUR UNE
!     FONCTION LINEAIRE PAR MORCEAU
! IN   NOMFON : NOM DE LA FONCTION
! IN   TEMP   : TEMPERATURE AU POINT DE GAUSS CONSIDERE
! OUT  F      : VALEUR DE LA FONCTION
! OUT  DF     : VALEUR DE LA DERIVEE DE LA FONCTION
! ......................................................................
!
!
!
!
!     ------------------------------------------------------------------
    integer :: mxsave, isvnxt, nextsv
    integer :: iaprol, iavale, luvale
    character(len=2) :: svprgd
    character(len=8) :: svnomf
    common /ifdsav/ mxsave, isvnxt, nextsv(5)
    common /jfdsav/ iaprol(5),iavale(5),luvale(5)
    common /kfdsav/ svnomf(5), svprgd(5)
!     ------------------------------------------------------------------
    logical :: tesinf, tessup
    integer :: isave, kk, jpro, jvalf, jv, jp, nbvf
    character(len=8) :: k8bid
    character(len=19) :: ch19
    character(len=24) :: chpro, chval
!     ------------------------------------------------------------------
!
    call jemarq()
!
    do 100 kk = 1, mxsave
        if (nomfon(1:8) .eq. svnomf(kk)) then
            isave = kk
            jpro = iaprol(isave)
            jvalf= iavale(isave)
            nbvf = luvale(isave)
            goto 101
        endif
100  end do
!
    ch19 = nomfon(1:8)
    chpro = ch19//'.PROL'
    chval = ch19//'.VALE'
    call jeveut(chpro, 'L', jpro)
    if (zk24(jpro)(1:1) .eq. 'I') then
!
! --- FONCTION INTERPRETEE NON-UTILISABLE
!
        call u2mess('F', 'MODELISA4_61')
    else if (zk24(jpro)(1:1).eq.'N') then
!
! --- NAPPE - IMPOSSIBLE
!
        call u2mess('F', 'MODELISA4_62')
    endif
!
    call jeveut(chval, 'L', jvalf)
    call jelira(chval, 'LONMAX', nbvf, k8bid)
    nbvf = nbvf/2
!
    isvnxt = nextsv(isvnxt)
    isave = isvnxt
    iaprol(isave) = jpro
    iavale(isave) = jvalf
    luvale(isave) = nbvf
    svnomf(isave) = nomfon(1:8)
    svprgd(isave) = zk24(jpro+4)(1:2)
!
101  continue
!
    tesinf = temp.lt.zr(jvalf)
    tessup = temp.gt.zr(jvalf+nbvf-1)
!
    if (tesinf) then
        jv = jvalf+nbvf
        jp = jvalf
        if (svprgd(isave)(1:1) .eq. 'C') then
            df = 0.0d0
            f = zr(jv)
        else if (svprgd(isave)(1:1).eq.'L') then
            df = (zr(jv+1)-zr(jv))/(zr(jp+1)-zr(jp))
            f = df*(temp-zr(jp))+zr(jv)
        else if (svprgd(isave)(1:1).eq.'E') then
            call u2mess('F', 'MODELISA4_63')
        else
            call u2mess('F', 'MODELISA4_64')
        endif
!
    else if (tessup) then
        jv = jvalf + 2*nbvf - 1
        jp = jvalf + nbvf - 1
        if (svprgd(isave)(2:2) .eq. 'C') then
            df = 0.0d0
            f = zr(jv)
        else if (svprgd(isave)(2:2).eq.'L') then
            df = (zr(jv)-zr(jv-1))/(zr(jp)-zr(jp-1))
            f = df*(temp-zr(jp-1))+zr(jv-1)
        else if (svprgd(isave)(2:2).eq.'E') then
            call u2mess('F', 'MODELISA4_65')
        else
            call u2mess('F', 'MODELISA4_66')
        endif
!
    else
        do 8 jp = jvalf+1, jvalf+nbvf-1
            jv = jp + nbvf
            if (zr(jp) .ge. temp) then
                df = (zr(jv)-zr(jv-1))/(zr(jp)-zr(jp-1))
                f = df*(temp-zr(jp-1))+zr(jv-1)
                goto 5
            endif
 8      continue
        call u2mess('F', 'MODELISA4_67')
 5      continue
!
    endif
! FIN ------------------------------------------------------------------
    call jedema()
!
end subroutine
