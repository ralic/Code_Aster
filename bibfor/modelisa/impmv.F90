subroutine impmv(ifm, txt, mv, nn, isym)
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
!       Impression d'une matrice stockée en colonne
!
!           mv    =  matrice stockée colonne
!           nn    =  longeur du vecteur de la matrice
!           txt   =  texte à afficher
!           ifm   =  unité d'impression
!           isym  =  (1) matrice symétrique
!                    (2) matrice non-symétrique
!
! --------------------------------------------------------------------------------------------------
!
    implicit   none
    integer :: ifm, nn, isym
    real(kind=8) :: mv(nn)
    character(len=8) :: txt
! --------------------------------------------------------------------------------------------------
    integer :: ii, jj, kk, dimmp
    real(kind=8) :: mp(12, 12)
! --------------------------------------------------------------------------------------------------
!
    if (nn .eq. 0) goto 999
!
    if (nn .eq. 4) dimmp = 2
    if (nn .eq. 9) dimmp = 3
    if (nn .eq. 16) dimmp = 4
    if (nn .eq. 36) dimmp = 6
    if (nn .eq. 144) dimmp = 12
!
    if (isym .eq. 1) then
!       Symétrique par colonne
        kk = 0
        do jj = 1, dimmp
            do  ii = 1, jj
                kk = kk + 1
                mp(ii,jj) = mv(kk)
                mp(jj,ii) = mv(kk)
            enddo
        enddo
        write(ifm,100) txt,'SYMETRIQUE'
    else
!      Non-symétrique par colonne
        kk = 0
        do jj = 1, dimmp
            do ii = 1, dimmp
                kk = kk + 1
                mp(ii,jj) = mv(kk)
            enddo
        enddo
        write(ifm,100) txt,'NON SYMETRIQUE'
    endif
!
    do ii = 1, dimmp
        write(ifm,201) (mp(ii,jj),jj=1,dimmp)
    enddo
!
100 format(3x,a8,3x,a20)
201 format(12(2x,1pd10.3))
!
999  continue
end subroutine
