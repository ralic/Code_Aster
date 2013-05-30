subroutine chveri(np1, np2, np3, nbm, nbmcd,&
                  nbnl, typch, nbseg, phii, noecho,&
                  alpha, beta, gamma, orig, rc,&
                  theta, depg)
!
    implicit none
!
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! VERIFICATION DE LA BONNE CONFIGURATION AUX NOEUX DE CHOC:
!  - NORMALE AU PLAN DE CHOC ET DIRECTION DU TUBE,
!  - POSITION INITIALE DU NOEUD DE CHOC, PAR RAPPORT AU JEU.
!-----------------------------------------------------------------------
!
!
! ARGUMENTS
    include 'jeveux.h'
!
    include 'asterfort/disbut.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/gloloc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/projmg.h'
    include 'asterfort/u2mesk.h'
    integer :: np1, np2, np3
    integer :: nbm, nbmcd
    integer :: nbnl, typch(*), nbseg(*)
    real(kind=8) :: phii(np2, np1, 3), depg(*)
    real(kind=8) :: rc(np3, *), theta(np3, *)
    real(kind=8) :: orig(6, *)
    real(kind=8) :: alpha(2, *), beta(2, *), gamma(2, *)
    character(len=8) :: noecho(np2, *)
    character(len=24) :: valk(2)
!
! VARIABLES LOCALES
    integer :: ic, typobs, nbs, ino1, jcoor1, i, j, k, nbno, ier
    real(kind=8) :: xloc(3), xglo(3)
    real(kind=8) :: xjeu, sint, cost, dnorm
    real(kind=8) :: xorig(3), xoriv(3), sina, cosa, sinb, cosb, sing, cosg
    real(kind=8) :: epsi, coor(3, 3), vect(3), tot
    character(len=3) :: inum
    character(len=8) :: k8b, mailla, nomnoe
!
! ****************** DEBUT DU CODE EXECUTABLE ************************
!
    call jemarq()
    epsi = 1.d-8
    xoriv(1) = 0.d0
    xoriv(2) = 0.d0
    xoriv(3) = 0.d0
!
    do 10 i = 1, nbm
        depg(i) = 0.d0
10  end do
!
! 1. BOUCLE SUR LES NON-LINEARITES.
!    ------------------------------
!
    do 20 ic = 1, nbnl
!
!  1.1.  CONVERSION DDLS GENERALISES -> DDLS PHYSIQUES.
!        ----------------------------------------------
        call projmg(np1, np2, ic, nbmcd, phii,&
                    depg, xglo)
!
!  1.2.  PASSAGE REPERE GLOBAL -> LOCAL.
!        -------------------------------
        xorig(1) = orig(1,ic)
        xorig(2) = orig(2,ic)
        xorig(3) = orig(3,ic)
        xglo(1) = xglo(1) + orig(4,ic)
        xglo(2) = xglo(2) + orig(5,ic)
        xglo(3) = xglo(3) + orig(6,ic)
        sina = alpha(1,ic)
        cosa = alpha(2,ic)
        sinb = beta(1,ic)
        cosb = beta(2,ic)
        sing = gamma(1,ic)
        cosg = gamma(2,ic)
!
        call gloloc(xglo, xorig, sina, cosa, sinb,&
                    cosb, sing, cosg, xloc)
!
        typobs = typch(ic)
        nbs = nbseg(ic)
        mailla = noecho(ic,4)
        nomnoe = noecho(ic,1)
!
        if (typobs .eq. 0 .or. typobs .eq. 1 .or. typobs .eq. 2) then
            xjeu = rc(1,ic)
        endif
!        TEST DE LA POSITION INITIALES ET DE L'ORIGINE
        call disbut(np3, ic, xloc, typobs, xjeu,&
                    rc, theta, nbs, cost, sint,&
                    dnorm)
        if (dnorm .lt. 0.d0) then
            write(inum,'(I3.3)') ic
            valk(1) = inum
            valk(2) = nomnoe
            call u2mesk('A', 'ALGELINE_9', 2, valk)
        endif
!
!        TEST DE LA POSITION INITIALES ET DE L'ORIGINE DANS LES
!        DIRECTIONS NORMALES AU PLAN DE CHOC
!
        if (typobs .eq. 0) then
!
            if (abs(xloc(1)) .gt. epsi .or. abs(xloc(3)) .gt. epsi) then
                write(inum,'(I3.3)') ic
                valk(1) = inum
                valk(2) = nomnoe
                call u2mesk('A', 'ALGELINE_10', 2, valk)
            endif
!
!     --- OBSTACLE PLAN PARALLELE A ZLOCAL ---
!
        else if (typobs .eq. 1) then
!
            if (abs(xloc(1)) .gt. epsi .or. abs(xloc(2)) .gt. epsi) then
                write(inum,'(I3.3)') ic
                valk(1) = inum
                valk(2) = nomnoe
                call u2mesk('A', 'ALGELINE_10', 2, valk)
            endif
!
!     --- OBSTACLE CIRCULAIRE OU DISCRETISE---
!
        else if (typobs .eq. 2 .or. typobs .eq. 3) then
!
            if (abs(xloc(1)) .gt. epsi) then
                write(inum,'(I3.3)') ic
                valk(1) = inum
                valk(2) = nomnoe
                call u2mesk('A', 'ALGELINE_10', 2, valk)
            endif
!
        endif
!
        call jeveuo(mailla//'.COORDO    .VALE', 'L', jcoor1)
        call jenonu(jexnom(mailla//'.NOMNOE', nomnoe), ino1)
        call dismoi('F', 'NB_NO_MAILLA', mailla, 'MAILLAGE', nbno,&
                    k8b, ier)
        if (ino1 .gt. 1) then
            ino1 = ino1 - 1
        endif
        if (ino1 .ge. (nbno-1)) then
            ino1 = nbno - 2
        endif
        do 12 k = 1, 3
            do 11 j = 1, 3
                coor(k,j) = zr(jcoor1+3*(ino1+k-2)+j-1)
11          continue
12      continue
        tot = 0.d0
        do 13 j = 1, 3
            vect(j) = coor(3,j) - coor(1,j)
            tot = tot + vect(j)*vect(j)
13      continue
        tot = dble(sqrt(tot))
        do 14 j = 1, 3
            vect(j) = vect(j) / tot
14      continue
!
        call gloloc(vect, xoriv, sina, cosa, sinb,&
                    cosb, sing, cosg, xloc)
        if (dble(abs(xloc(1))) .lt. epsi) then
            write(inum,'(I3.3)') ic
            call u2mesk('A', 'ALGELINE_11', 1, inum)
!
        else if (dble(abs(xloc(1))).lt.0.17364818D0) then
            write(inum,'(I3.3)') ic
            call u2mesk('A', 'ALGELINE_12', 1, inum)
!
        else if (dble(abs(xloc(1))).lt.0.70710678D0) then
            write(inum,'(I3.3)') ic
            call u2mesk('A', 'ALGELINE_13', 1, inum)
        endif
!
20  end do
    call jedema()
end subroutine
