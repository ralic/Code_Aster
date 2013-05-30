subroutine utpplg(nn, nc, p, sl, sg)
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
    implicit none
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lcdi2m.h'
    include 'asterfort/lcps2m.h'
    include 'asterfort/lcso2m.h'
    include 'asterfort/lctr2m.h'
    include 'asterfort/mapvec.h'
    include 'asterfort/mavec.h'
    include 'asterfort/upletr.h'
    include 'asterfort/uplstr.h'
    include 'asterfort/utpalg.h'
    include 'asterfort/utpslg.h'
    include 'asterfort/vecmap.h'
    real(kind=8) :: p(3, 3), sl(*), sg(*)
    integer :: nn, nc, n, n1, nddl
    real(kind=8) :: matsy1(12, 12), matsy2(12, 12)
    real(kind=8) :: matas2(12, 12), matsym(12, 12)
    real(kind=8) :: matasy(12, 12)
    real(kind=8) :: parsym(78), parasy(78)
    real(kind=8) :: parsmg(12, 12), parayg(12, 12)
    real(kind=8) :: matril(12, 12), matrig(12, 12)
    real(kind=8) :: vecsym(78), vecasy(78)
!     ------------------------------------------------------------------
!     PASSAGE D'UNE MATRICE TRIANGULAIRE ANTISYMETRIQUE DE NN*NC LIGNES
!     DU REPERE LOCAL AU REPERE GLOBAL (3D)
!     ------------------------------------------------------------------
!IN   I   NN   NOMBRE DE NOEUDS
!IN   I   N    NOMBRE DE NOEUDS
!IN   I   NC   NOMBRE DE COMPOSANTES
!IN   R   P    MATRICE DE PASSAGE 3D DE GLOBAL A LOCAL
!IN   R   SL   NN*NC COMPOSANTES DE LA TRIANGULAIRE SL DANS LOCAL
!OUT  R   SG   NN*NC COMPOSANTES DE LA TRIANGULAIRE SG DANS GLOBAL
!     ------------------------------------------------------------------
    call jemarq()
!
    nddl = nn * nc
    n = nddl*nddl
    n1 = (nddl+1)*nddl/2
!
    call vecmap(sl, n, matril, nddl)
    call lctr2m(nddl, matril, matsy1)
    call lcso2m(nddl, matril, matsy1, matsy2)
    call lcdi2m(nddl, matril, matsy1, matas2)
    call lcps2m(nddl, 0.5d0, matsy2, matsym)
    call mavec(matsym, nddl, vecsym, n1)
    call lcps2m(nddl, 0.5d0, matas2, matasy)
    call mavec(matasy, nddl, vecasy, n1)
    call utpslg(nn, nc, p, vecsym, parsym)
    call uplstr(nddl, parsmg, parsym)
    call utpalg(nn, nc, p, vecasy, parasy)
    call upletr(nddl, parayg, parasy)
    call lcso2m(nddl, parsmg, parayg, matrig)
    call mapvec(matrig, nddl, sg, n)
!
    call jedema()
!
end subroutine
