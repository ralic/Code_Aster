subroutine afdi2d(irep, eta, car, val, jdc,&
                  jdv, ivr, iv, kma, ncmp,&
                  ntp, jdcinf, jdvinf, isym, ifm)
    implicit       none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/impmv.h'
    include 'asterfort/infdis.h'
    integer :: irep, jdv(3), jdc(3), ivr(*), iv, ncmp, ntp, ifm
    integer :: isym, jdcinf, jdvinf
    real(kind=8) :: eta, val(*)
    character(len=1) :: kma(3)
    character(len=*) :: car
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! --- ---------------------------------------------------------------
!     AFFECTATION DES VALEURS DES MATRICES A TOUS LES ELEMENTS
!     DEMANDES PAR L UTILISATEUR DANS LES CARTES CORRESPONDANTES
!     LES ELEMENTS CONCERNES SONT LES ELEMENTS DISCRETS 2D
! --- ---------------------------------------------------------------
!
!
! --- ---------------------------------------------------------------
    integer :: j, l, dimmat, ibid
    real(kind=8) :: r8bid
    character(len=7) :: ki
    logical :: nonsym
    character(len=8) :: k8bid
    character(len=13) :: carbid
! --- ---------------------------------------------------------------
!
    call infdis('DMXM', dimmat, r8bid, k8bid)
!
!     BOUCLE SUR LES TYPES DE MATRICE (K,M,A)
    do 200 j = 1, 3
        if (car(1:1) .ne. kma(j)) goto 200
!        REPERE GLOBAL OU REPERE LOCAL)
        zk8(jdcinf+j-1) = 'REP'//kma(j)//'    '
        zr (jdvinf+j-1) = irep
!        MATRICE SYMETRIQUE OU PAS
        zk8(jdcinf+j+2) = 'SYM'//kma(j)//'    '
        zr (jdvinf+j+2) = isym
!        TYPE DE LA MATRICE
        zk8(jdcinf+j+5) = 'DIS'//kma(j)//'    '
        zr (jdvinf+j+5) = 1.0d0
!        COEFFICIENT AMORTISSEMENT HYSTERETIQUE
        if (car(1:1) .eq. 'K') then
            zk8(jdcinf+9) = 'ETAK    '
            zr (jdvinf+9) = eta
        endif
        zk8(jdcinf+10) = 'TYDI    '
!
        nonsym = ( isym .eq. 2 )
        ntp = j
!
!        NCMP : NOMBRE DE COMPOSANTES DE LA MATRICE NON-SYMETRIQUE
!
        do 100 l = 1, dimmat
            call codent(l, 'G', ki)
            zk8(jdc(j)+l-1) = kma(j)//ki
            zr (jdv(j)+l-1) = 0.d0
100      continue
! --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_T_D_N
        if (car(3:7) .eq. 'T_D_N') then
            carbid = '2D_DIS_'//car(3:7)
            call assert(isym .eq. 1)
            ncmp = 4
            if (car(1:1) .eq. 'M') then
                zr (jdv(j) ) = val(iv)
                zr (jdv(j)+2) = val(iv)
                iv = iv + 1
            else
                zr (jdv(j) ) = val(iv)
                zr (jdv(j)+2) = val(iv+1)
                iv = iv + 2
            endif
! --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_T_D_L
        else if (car(3:7).eq.'T_D_L') then
            carbid = '2D_DIS_'//car(3:7)
            call assert(isym .eq. 1)
            ncmp = 16
            if (car(1:1) .eq. 'M') then
                zr (jdv(j)) = val(iv)
                zr (jdv(j)+2) = val(iv)
                zr (jdv(j)+5) = val(iv)
                zr (jdv(j)+9 ) = val(iv)
                iv = iv + 1
            else
                zr (jdv(j)) = val(iv)
                zr (jdv(j)+2) = val(iv+1)
                zr (jdv(j)+3) = -val(iv)
                zr (jdv(j)+5) = val(iv)
                zr (jdv(j)+7) = -val(iv+1)
                zr (jdv(j)+9 ) = val(iv+1)
                iv = iv + 2
            endif
! --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_TR_D_N
        else if (car(3:8).eq.'TR_D_N') then
            carbid = '2D_DIS_'//car(3:8)
            call assert(isym .eq. 1)
            ncmp = 9
            if (car(1:1) .eq. 'M') then
                zr (jdv(j)) = val(iv)
                zr (jdv(j)+2) = val(iv)
                zr (jdv(j)+3) = val(iv)*val(iv+3)
                zr (jdv(j)+4) = -val(iv)*val(iv+2)
                zr (jdv(j)+5) = val(iv+1)+ val(iv)*(val(iv+2)*val(iv+&
                2)+val(iv+3)*val(iv+3))
                iv = iv + 4
            else
                zr (jdv(j)) = val(iv)
                zr (jdv(j)+2) = val(iv+1)
                zr (jdv(j)+5) = val(iv+2)
                iv = iv + 3
            endif
! --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_TR_D_L
        else if (car(3:8).eq.'TR_D_L') then
            carbid = '2D_DIS_'//car(3:8)
            call assert(isym .eq. 1)
            ncmp = 36
            if (car(1:1) .eq. 'M') then
                zr (jdv(j)) = val(iv)
                zr (jdv(j)+2) = val(iv)
                zr (jdv(j)+5) = val(iv+1)
                zr (jdv(j)+9) = val(iv)
                zr (jdv(j)+14) = val(iv)
                zr (jdv(j)+20) = val(iv+1)
                iv = iv + 2
            else
                zr (jdv(j)) = val(iv)
                zr (jdv(j)+2) = val(iv+1)
                zr (jdv(j)+5) = val(iv+2)
                zr (jdv(j)+6) = -val(iv)
                zr (jdv(j)+9) = val(iv)
                zr (jdv(j)+11) = -val(iv+1)
                zr (jdv(j)+14) = val(iv+1)
                zr (jdv(j)+17) = -val(iv+2)
                zr (jdv(j)+20) = val(iv+2)
                iv = iv + 3
            endif
! --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_T_N
        else if (car(3:5).eq.'T_N') then
            carbid = '2D_DIS_'//car(3:5)
            if (nonsym) then
                ncmp = 4
            else
                ncmp = 3
            endif
            do 30 l = 1, ncmp
                call codent(l, 'G', ki)
                zk8(jdc(j)+l-1) = kma(j)//ki
                zr (jdv(j)+l-1) = val(iv)
                iv = iv + 1
30          continue
            ncmp = 4
! --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_T_L
        else if (car(3:5).eq.'T_L') then
            carbid = '2D_DIS_'//car(3:5)
            if (nonsym) then
                ncmp = 16
            else
                ncmp = 10
            endif
            do 35 l = 1, ncmp
                call codent(l, 'G', ki)
                zk8(jdc(j)+l-1) = kma(j)//ki
                zr (jdv(j)+l-1) = val(iv)
                iv = iv + 1
35          continue
            ncmp = 16
! --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_TR_N
        else if (car(3:6).eq.'TR_N') then
            carbid = '2D_DIS_'//car(3:6)
            if (nonsym) then
                ncmp = 9
            else
                ncmp = 6
            endif
            do 40 l = 1, ncmp
                call codent(l, 'G', ki)
                zk8(jdc(j)+l-1) = kma(j)//ki
                zr (jdv(j)+l-1) = val(iv)
                iv = iv + 1
40          continue
            ncmp = 9
! --- --- --- --- --- --- --- --- --- --- --- --- --- [K|M|A]_TR_L
        else if (car(3:6).eq.'TR_L') then
            carbid = '2D_DIS_'//car(3:6)
            if (nonsym) then
                ncmp = 36
            else
                ncmp = 21
            endif
            do 45 l = 1, ncmp
                call codent(l, 'G', ki)
                zk8(jdc(j)+l-1) = kma(j)//ki
                zr (jdv(j)+l-1) = val(iv)
                iv = iv + 1
45          continue
            ncmp = 36
        else
            call assert(.false.)
        endif
        call infdis('CODE', ibid, zr(jdvinf+10), carbid)
!
        if (ivr(3) .eq. 1) call impmv(ifm, car(1:8), zr(jdv(j)), ncmp, isym)
200  end do
!
end subroutine
