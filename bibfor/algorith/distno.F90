subroutine distno(xlocal, signe, typeob, xjeu, dist1,&
                  dist2, dnorm, cost, sint)
    implicit none
    include 'jeveux.h'
    include 'asterc/r8depi.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/tbliva.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: xlocal(6), signe(*)
    character(len=8) :: typeob
!---------------------------------------------------------------------
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
!
!     CALCULE LA DISTANCE NORMALE A L'OBSTACLE (<0 SI CHOC)
!---------------------------------------------------------------------
! IN  : XLOCAL : COORDONNES DANS LE REPERE LOCAL
! IN  : SIGNE  : POUR UN BI_PLAN_Y  SIGNE= SIGNE DE Y20LOC-Y10LOC
!                POUR UN BI_PLAN_Z  SIGNE= SIGNE DE Z20LOC-Z10LOC
! IN  : TYPEOB : NOM DECRIVANT LE TYPE D'OBSTACLE CERCLE,PLAN.
! IN  : XJEU   : VALEUR DU JEU POUR OBSTACLE CERCLE OU PLAN
! IN  : DIST1  : VALEUR DE EPAISSEUR DE DIST1 POUR BIPLAN
! IN  : DIST2  : VALEUR DE EPAISSEUR DE DIST2 POUR BIPLAN
! OUT : DNORM  : DISTANCE NORMALE A L'OBSTACLE
! OUT : COST   : DIRECTION NORMALE A L'OBSTACLE
! OUT : SINT   : DIRECTION NORMALE A L'OBSTACLE
!---------------------------------------------------------------------
    character(len=8) :: kbid, k8typ
    integer :: ibid, irett, lval, lfon, nbval, nbpair
    real(kind=8) :: r8bid
    complex(kind=8) :: cbid
    character(len=24) :: nomfon
!
!-----------------------------------------------------------------------
    integer :: i
    real(kind=8) :: cos2, cost, costno, depi, dist1, dist2, dnorm
    real(kind=8) :: dy, dz, r1, r2, sin2, sint
    real(kind=8) :: sintno, t1, t2, tetano, un, xjeu, xlg
    real(kind=8) :: xls, y1, y2, z1, z2, zero
!-----------------------------------------------------------------------
    call jemarq()
    zero = 0.d0
    un = 1.d0
    depi = r8depi()
!
!     --- OBSTACLE CIRCULAIRE ---
    if (typeob .eq. 'CERCLE  ') then
        xlg = sqrt( xlocal(2)**2 + xlocal(3)**2 )
        dnorm = xjeu - xlg
        if (xlg .ne. 0.d0) then
            sint = -xlocal(3) / xlg
            cost = -xlocal(2) / xlg
        else
            sint = zero
            cost = -un
        endif
!
!     --- OBSTACLES CIRCULAIRES ---
    else if (typeob.eq.'BI_CERCL') then
        xlg = sqrt( (xlocal(2)-xlocal(5))**2+(xlocal(3)-xlocal(6))**2 )
        dnorm = xlg - dist1 - dist2
        cost = ( xlocal(2) - xlocal(5) ) / xlg
        sint = ( xlocal(3) - xlocal(6) ) / xlg
!     --- OBSTACLES CIRCULAIRES CONCENTRIQUES ---
    else if (typeob.eq.'BI_CERCI') then
        xlg = sqrt( (xlocal(2)-xlocal(5))**2+(xlocal(3)-xlocal(6))**2 )
        dnorm = dist2 - dist1 -xlg
        if (xlg .gt. 0.d0) then
            cost = ( xlocal(5) - xlocal(2) ) / xlg
            sint = ( xlocal(6) - xlocal(3) ) / xlg
        else
            cost = -un
            sint = zero
        endif
!
!     --- OBSTACLE PLAN PARALLELE A YLOCAL ---
    else if (typeob .eq. 'PLAN_Y  ') then
        dnorm = xjeu - abs(xlocal(2))
        sint = zero
        cost = -sign( un,xlocal(2) )
!
!     --- OBSTACLE PLANS PARALLELES A YLOCAL ---
    else if (typeob.eq.'BI_PLANY') then
        dnorm = ( xlocal(5)-xlocal(2) ) * signe(1)- dist1 - dist2
        sint = zero
!        COST  = -SIGN(UN,(XLOCAL(5)-XLOCAL(2)))
        cos2 = -sign(un,(xlocal(5)-xlocal(2)))
        cost = -signe(1)
        if (cos2 .ne. cost) call u2mess('A', 'ALGORITH3_10')
!
!     --- OBSTACLE PLAN PARALLELE A ZLOCAL ---
    else if (typeob .eq. 'PLAN_Z  ') then
        dnorm = xjeu - abs(xlocal(3))
        cost = zero
        sint = -sign( un,xlocal(3) )
!
!     --- OBSTACLE PLANS PARALLELES A ZLOCAL ---
    else if (typeob .eq. 'BI_PLANZ') then
        dnorm = ( xlocal(6) - xlocal(3) ) * signe(2) - dist1 - dist2
        cost = zero
!        SINT  = -SIGN(UN,(XLOCAL(6)-XLOCAL(3)))
        sin2 = -sign(un,(xlocal(6)-xlocal(3)))
        sint = -signe(2)
        if (sin2 .ne. sint) call u2mess('A', 'ALGORITH3_10')
!
!     --- OBSTACLE DISCRETISE ---
    else
        call tbliva(typeob, 1, 'LIEU', ibid, r8bid,&
                    cbid, 'DEFIOBST', kbid, r8bid, 'FONCTION',&
                    k8typ, ibid, r8bid, cbid, nomfon,&
                    irett)
        call assert(irett.eq.0)
        call jeveuo(nomfon(1:19)//'.VALE', 'L', lval)
        call jelira(nomfon(1:19)//'.VALE', 'LONMAX', nbval, kbid)
        nbpair = nbval/2
        lfon = lval + nbpair
        xlg = sqrt(xlocal(2)**2+xlocal(3)**2)
        if (xlg .ne. 0.d0) then
            sintno = xlocal(3) / xlg
            costno = xlocal(2) / xlg
        else
            sintno = zero
            costno = un
        endif
        tetano = atan2(sintno,costno)
        if (tetano .lt. zero) tetano = tetano + depi
        do 10 i = 1, nbpair-1
            r1 = zr(lfon+i-1)
            r2 = zr(lfon+i)
            t1 = zr(lval+i-1)
            t2 = zr(lval+i)
            if (tetano .ge. t1 .and. tetano .le. t2) then
                y1 = r1*cos(t1)
                y2 = r2*cos(t2)
                z1 = r1*sin(t1)
                z2 = r2*sin(t2)
                dy = y2-y1
                dz = z2-z1
                xls = sqrt(dy*dy+dz*dz)
                if (xls .ne. 0.d0) then
                    cost = -dz / xls
                    sint = dy / xls
                else
                    sint = zero
                    cost = -un
                endif
                dnorm = (xlocal(2)-y1)*cost+(xlocal(3)-z1)*sint
                goto 9999
            endif
10      continue
    endif
!
9999  continue
    call jedema()
end subroutine
