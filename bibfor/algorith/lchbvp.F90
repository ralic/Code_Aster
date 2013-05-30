subroutine lchbvp(sigd, vp, vecp)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =====================================================================
    implicit none
    include 'asterfort/codree.h'
    include 'asterfort/jacobi.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/u2mesk.h'
    real(kind=8) :: sigd(6), vp(3), vecp(3, 3)
! =====================================================================
! --- HOEK-BROWN : VALEURS ET VECTEURS PROPRES DU DEVIATEUR DE SIGD ---
! --- UTILISE POUR L OPTION RIGI_MECA AFIN DE CALCULER LES VALEURS ----
! --- ET VECTEURS PROPRES DU DEVIATEUR ELASTIQUE ----------------------
! =====================================================================
! IN  : SIGD   :  TENSEUR DES CONTRAINTES (ELASTIQUE) -----------------
! OUT : VP     :  VALEURS PROPRES ORDONNEES DU DEVIATEUR DE SIGD ------
! --- : VECP   :  VECTEURS PROPRES DU DEVIATEUR DE SIGD ---------------
! =====================================================================
    real(kind=8) :: seb(6), deux, se(6), tu(6), tol, toldyn, jacaux(3)
    character(len=10) :: cvp1, cvp2, cvp3
    character(len=24) :: valk(3)
    integer :: ndt, ndi, nperm, ttrij, otrij, nitjac
! ======================================================================
    parameter   (deux = 2.0d0)
! ======================================================================
    common /tdim/   ndt, ndi
! ======================================================================
    data   nperm ,tol,toldyn    /12,1.d-10,1.d-2/
    data   ttrij,otrij  /0,0/
! ======================================================================
    call lcdevi(sigd, se)
    seb(1) = se(1)
    seb(2) = se(4)/sqrt(deux)
    seb(4) = se(2)
    seb(6) = se(3)
    if (ndt .eq. 4) then
        seb(3) = 0.0d0
        seb(5) = 0.0d0
    else
        seb(3) = se(5) / sqrt(deux)
        seb(5) = se(6) / sqrt(deux)
    endif
! -- MATRICE UNITE POUR JACOBI ----------------------------------------
    tu(1) = 1.d0
    tu(2) = 0.d0
    tu(3) = 0.d0
    tu(4) = 1.d0
    tu(5) = 0.d0
    tu(6) = 1.d0
    call jacobi(3, nperm, tol, toldyn, seb,&
                tu, vecp, vp, jacaux, nitjac,&
                ttrij, otrij)
    if ((vp(2).lt.vp(1)) .or. (vp(3).lt.vp(2))) then
        call codree(vp(1), 'E', cvp1)
        call codree(vp(2), 'E', cvp2)
        call codree(vp(3), 'E', cvp3)
        valk(1) = cvp1
        valk(2) = cvp2
        valk(3) = cvp3
        call u2mesk('F', 'ALGORITH3_89', 3, valk)
    endif
! ======================================================================
end subroutine
