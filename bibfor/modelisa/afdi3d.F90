subroutine afdi3d(irep, eta, car, val, jdc,&
                  jdv, ivr, iv, kma, ncmp,&
                  ntp, jdcinf, jdvinf, isym )
!
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
! --------------------------------------------------------------------------------------------------
!
!           Affectation des valeurs des matrices à tous les éléments
!           demandés par l'utilisateur dans les cartes correspondantes
!           les éléments concernés sont les éléments discrets 3D
!
! --------------------------------------------------------------------------------------------------
    implicit none
    integer :: irep, jdv(3), jdc(3), ivr(*), iv, ncmp, ntp, ifm
    integer :: isym, jdcinf, jdvinf
    real(kind=8) :: eta, val(*)
    character(len=1) :: kma(3)
    character(len=*) :: car
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/impmv.h"
#include "asterfort/infdis.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: jj, ll, dimmat, ibid
    real(kind=8) :: r8bid
    aster_logical :: nonsym
    character(len=7) :: ki
    character(len=8) :: k8bid
    character(len=11) :: carbid
! --------------------------------------------------------------------------------------------------
!
    call infdis('DMXM', dimmat, r8bid, k8bid)
    ifm = ivr(4)
!
!   Boucle sur les types de matrice (K,M,A)
    do jj = 1, 3
        if (car(1:1) .ne. kma(jj)) cycle
!       Repère GLOBAL ou repère LOCAL
        zk8(jdcinf+jj-1) = 'REP'//kma(jj)//'    '
        zr (jdvinf+jj-1) = irep
!       Matrice symétrique ou pas
        zk8(jdcinf+jj+2) = 'SYM'//kma(jj)//'    '
        zr (jdvinf+jj+2) = isym
!       Affectation de la matrice
        zk8(jdcinf+jj+5) = 'DIS'//kma(jj)//'    '
        zr (jdvinf+jj+5) = 1.0d0
!       Coefficient amortissement hystérétique
        if (car(1:1) .eq. 'K') then
            zk8(jdcinf+9) = 'ETAK    '
            zr (jdvinf+9) = eta
        endif
        zk8(jdcinf+10) = 'TYDI    '
!
        nonsym = ( isym .eq. 2 )
        ntp = jj
!
!       NCMP : nombre de composantes de la matrice
        do ll = 1, dimmat
            call codent(ll, 'G', ki)
            zk8(jdc(jj)+ll-1) = kma(jj)//ki
            zr (jdv(jj)+ll-1) = 0.d0
        enddo
! ------------------------------------------------------ [K|M|A]_T_D_N
        if (car(3:7) .eq. 'T_D_N') then
            carbid = '_DIS_'//car(3:7)
            ASSERT(isym .eq. 1)
            ncmp = 9
            if (car(1:1) .eq. 'M') then
                zr(jdv(jj) )  = val(iv)
                zr(jdv(jj)+2) = val(iv)
                zr(jdv(jj)+5) = val(iv)
                iv = iv + 1
            else
                zr(jdv(jj) )  = val(iv)
                zr(jdv(jj)+2) = val(iv+1)
                zr(jdv(jj)+5) = val(iv+2)
                iv = iv + 3
            endif
! ------------------------------------------------------ [K|M|A]_T_D_L
        else if (car(3:7).eq.'T_D_L') then
            carbid = '_DIS_'//car(3:7)
            ASSERT(isym .eq. 1)
            ncmp = 36
            if (car(1:1) .eq. 'M') then
                zr(jdv(jj))    = val(iv)
                zr(jdv(jj)+2)  = val(iv)
                zr(jdv(jj)+5)  = val(iv)
                zr(jdv(jj)+9)  = val(iv)
                zr(jdv(jj)+14) = val(iv)
                zr(jdv(jj)+20) = val(iv)
                iv = iv + 1
            else
                zr(jdv(jj))    =  val(iv)
                zr(jdv(jj)+2)  =  val(iv+1)
                zr(jdv(jj)+5)  =  val(iv+2)
                zr(jdv(jj)+9)  =  val(iv)
                zr(jdv(jj)+14) =  val(iv+1)
                zr(jdv(jj)+20) =  val(iv+2)
                zr(jdv(jj)+6)  = -val(iv)
                zr(jdv(jj)+11) = -val(iv+1)
                zr(jdv(jj)+17) = -val(iv+2)
                iv = iv + 3
            endif
! ------------------------------------------------------ [K|M|A]_TR_D_N
        else if (car(3:8).eq.'TR_D_N') then
            carbid = '_DIS_'//car(3:8)
            ASSERT(isym .eq. 1)
            ncmp = 36
            if (car(1:1) .eq. 'M') then
                zr(jdv(jj))    =  val(iv)
                zr(jdv(jj)+2)  =  val(iv)
                zr(jdv(jj)+5)  =  val(iv)
                zr(jdv(jj)+7)  = -val(iv)*val(iv+9)
                zr(jdv(jj)+8)  =  val(iv)*val(iv+8)
                zr(jdv(jj)+10) =  val(iv)*val(iv+9)
                zr(jdv(jj)+12) = -val(iv)*val(iv+7)
                zr(jdv(jj)+15) = -val(iv)*val(iv+8)
                zr(jdv(jj)+16) =  val(iv)*val(iv+7)
                zr(jdv(jj)+9)  =  val(iv+1) + val(iv)*(val(iv+9)*val(iv+ 9)+val(iv+8)*val(iv+8))
                zr(jdv(jj)+14) =  val(iv+2) + val(iv)*(val(iv+7)*val(iv+7)+val(iv+9)*val(iv+9))
                zr(jdv(jj)+20) =  val(iv+3) + val(iv)*(val(iv+8)*val(iv+8)+val(iv+7)*val(iv+7))
                zr(jdv(jj)+13) =  val(iv+4) - val(iv)*val(iv+7)*val(iv+8)
                zr(jdv(jj)+19) =  val(iv+5) - val(iv)*val(iv+8)*val(iv+9)
                zr(jdv(jj)+18) =  val(iv+6) - val(iv)*val(iv+7)*val(iv+9)
                iv = iv + 10
            else
                zr(jdv(jj))    = val(iv)
                zr(jdv(jj)+2)  = val(iv+1)
                zr(jdv(jj)+5)  = val(iv+2)
                zr(jdv(jj)+9)  = val(iv+3)
                zr(jdv(jj)+14) = val(iv+4)
                zr(jdv(jj)+20) = val(iv+5)
                iv = iv + 6
            endif
! ------------------------------------------------------ [K|M|A]_TR_D_L
        else if (car(3:8).eq.'TR_D_L') then
            carbid = '_DIS_'//car(3:8)
            ASSERT(isym .eq. 1)
            ncmp = 144
            if (car(1:1) .eq. 'M') then
                zr(jdv(jj) )   = val(iv)
                zr(jdv(jj)+ 2) = val(iv)
                zr(jdv(jj)+ 5) = val(iv)
                zr(jdv(jj)+ 9) = val(iv+1)
                zr(jdv(jj)+14) = val(iv+2)
                zr(jdv(jj)+20) = val(iv+3)
                zr(jdv(jj)+27) = val(iv)
                zr(jdv(jj)+35) = val(iv)
                zr(jdv(jj)+44) = val(iv)
                zr(jdv(jj)+54) = val(iv+1)
                zr(jdv(jj)+65) = val(iv+2)
                zr(jdv(jj)+77) = val(iv+3)
                iv = iv + 4
            else
                zr(jdv(jj) )   = val(iv)
                zr(jdv(jj)+ 2) = val(iv+1)
                zr(jdv(jj)+ 5) = val(iv+2)
                zr(jdv(jj)+ 9) = val(iv+3)
                zr(jdv(jj)+14) = val(iv+4)
                zr(jdv(jj)+20) = val(iv+5)
                zr(jdv(jj)+27) = val(iv)
                zr(jdv(jj)+35) = val(iv+1)
                zr(jdv(jj)+44) = val(iv+2)
                zr(jdv(jj)+54) = val(iv+3)
                zr(jdv(jj)+65) = val(iv+4)
                zr(jdv(jj)+77) = val(iv+5)
                zr(jdv(jj)+21) = -val(iv)
                zr(jdv(jj)+29) = -val(iv+1)
                zr(jdv(jj)+38) = -val(iv+2)
                zr(jdv(jj)+48) = -val(iv+3)
                zr(jdv(jj)+59) = -val(iv+4)
                zr(jdv(jj)+71) = -val(iv+5)
                iv = iv + 6
            endif
! ------------------------------------------------------ [K|M|A]_T_N
        else if (car(3:5).eq.'T_N') then
            carbid = '_DIS_'//car(3:5)
            if (nonsym) then
                ncmp = 9
            else
                ncmp = 6
            endif
            do ll = 1, ncmp
                call codent(ll, 'G', ki)
                zk8(jdc(jj)+ll-1) = kma(jj)//ki
                zr (jdv(jj)+ll-1) = val(iv)
                iv = iv + 1
            enddo
            ncmp = 9
! ------------------------------------------------------ [K|M|A]_T_L
        else if (car(3:5).eq.'T_L') then
            carbid = '_DIS_'//car(3:5)
            if (nonsym) then
                ncmp = 36
            else
                ncmp = 21
            endif
            do ll = 1, ncmp
                call codent(ll, 'G', ki)
                zk8(jdc(jj)+ll-1) = kma(jj)//ki
                zr (jdv(jj)+ll-1) = val(iv)
                iv = iv + 1
            enddo
            ncmp = 36
! ------------------------------------------------------ [K|M|A]_TR_N
        else if (car(3:6).eq.'TR_N') then
            carbid = '_DIS_'//car(3:6)
            if (nonsym) then
                ncmp = 36
            else
                ncmp = 21
            endif
            do ll = 1, ncmp
                call codent(ll, 'G', ki)
                zk8(jdc(jj)+ll-1) = kma(jj)//ki
                zr (jdv(jj)+ll-1) = val(iv)
                iv = iv + 1
            enddo
            ncmp = 36
! ------------------------------------------------------ [K|M|A]_TR_L
        else if (car(3:6).eq.'TR_L') then
            carbid = '_DIS_'//car(3:6)
            if (nonsym) then
                ncmp = 144
            else
                ncmp = 78
            endif
            do ll = 1, ncmp
                call codent(ll, 'G', ki)
                zk8(jdc(jj)+ll-1) = kma(jj)//ki
                zr (jdv(jj)+ll-1) = val(iv)
                iv = iv + 1
            enddo
            ncmp = 144
        endif
        call infdis('CODE', ibid, zr(jdvinf+10), carbid)
!
        if (ivr(3) .eq. 2) call impmv(ifm, car(1:8), zr(jdv(jj)), ncmp, isym)
    enddo
!
end subroutine
