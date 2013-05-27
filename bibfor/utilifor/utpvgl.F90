subroutine utpvgl(nn, nc, p, vg, vl)
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
    implicit none
    real(kind=8) :: p(3, 3), vg(*), vl(*)
!     ------------------------------------------------------------------
!     PASSAGE EN 3D D'UN VECTEUR NN*NC DU REPERE GLOBAL AU REPERE LOCAL
!     ------------------------------------------------------------------
!IN   I   NN   NOMBRE DE NOEUDS
!IN   I   NC   NOMBRE DE COMPOSANTES
!IN   R   P    MATRICE DE PASSAGE 3D DE GLOBAL A LOCAL
!IN   R   VG   NN*NC COMPOSANTES DU VECTEUR DANS GLOBAL
!OUT  R   VL   NN*NC COMPOSANTES DU VECTEUR DANS LOCAL
! ATTENTION :
!  IL NE FAUT PAS UTILISER CETTE ROUTINE AVEC VL=VG
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, nc, nn
!-----------------------------------------------------------------------
    if (mod(nc,3) .eq. 0) then
        do 10 i = 1, nn * nc, 3
            vl(i ) = p(1,1)*vg(i) + p(1,2)*vg(i+1) + p(1,3)*vg(i+2)
            vl(i+1) = p(2,1)*vg(i) + p(2,2)*vg(i+1) + p(2,3)*vg(i+2)
            vl(i+2) = p(3,1)*vg(i) + p(3,2)*vg(i+1) + p(3,3)*vg(i+2)
10      continue
!
    else if (mod(nc,3) .eq. 1) then
        do 20 i = 1, nn * nc, 7
            vl(i ) = p(1,1)*vg(i) + p(1,2)*vg(i+1) + p(1,3)*vg(i+2)
            vl(i+1) = p(2,1)*vg(i) + p(2,2)*vg(i+1) + p(2,3)*vg(i+2)
            vl(i+2) = p(3,1)*vg(i) + p(3,2)*vg(i+1) + p(3,3)*vg(i+2)
            vl(i+3) = p(1,1)*vg(i+3) + p(1,2)*vg(i+4) + p(1,3)*vg(i+5)
            vl(i+4) = p(2,1)*vg(i+3) + p(2,2)*vg(i+4) + p(2,3)*vg(i+5)
            vl(i+5) = p(3,1)*vg(i+3) + p(3,2)*vg(i+4) + p(3,3)*vg(i+5)
            vl(i+6) = vg(i+6)
20      continue
!
    else if (mod(nc,3) .eq. 2) then
        do 30 i = 1, nn * nc, 8
            vl(i ) = p(1,1)*vg(i) + p(1,2)*vg(i+1) + p(1,3)*vg(i+2)
            vl(i+1) = p(2,1)*vg(i) + p(2,2)*vg(i+1) + p(2,3)*vg(i+2)
            vl(i+2) = p(3,1)*vg(i) + p(3,2)*vg(i+1) + p(3,3)*vg(i+2)
            vl(i+3) = p(1,1)*vg(i+3) + p(1,2)*vg(i+4) + p(1,3)*vg(i+5)
            vl(i+4) = p(2,1)*vg(i+3) + p(2,2)*vg(i+4) + p(2,3)*vg(i+5)
            vl(i+5) = p(3,1)*vg(i+3) + p(3,2)*vg(i+4) + p(3,3)*vg(i+5)
            vl(i+6) = vg(i+6)
            vl(i+7) = vg(i+7)
30      continue
    endif
!
end subroutine
