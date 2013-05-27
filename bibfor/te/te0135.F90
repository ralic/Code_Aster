subroutine te0135(option, nomte)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/matrot.h'
    character(len=16) :: option, nomte
!----------------------------------------------------------------------
!
!   - FONCTION REALISEE: CALCUL DU REPERE LOCAL DONNE PAR L'UTILISATEUR
!   - POUTRES, TUYAUX, DISCRETS
!
!----------------------------------------------------------------------
!
    integer :: jorie, jrepl1, jrepl2, jrepl3
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: i
    real(kind=8) :: pgl(3, 3)
    real(kind=8) :: ux(3), uy(3), uz(3)
    real(kind=8) :: ang(3)
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
    call assert(nnos.le.4)
!
    call jevech('PCAORIE', 'L', jorie)
    call jevech('PREPLO1', 'E', jrepl1)
    call jevech('PREPLO2', 'E', jrepl2)
    call jevech('PREPLO3', 'E', jrepl3)
!
    ang(1) = zr(jorie)
    ang(2) = zr(jorie+1)
    ang(3) = zr(jorie+2)
    call matrot(ang, pgl)
!     (UX,UY,UZ) : VECTEUR LOCAUX UTILISATEUR DANS LE BASE GLOBAL
!     UX EST LA PREMIERE LIGNE DE PGL
!     UY LA DEUXIEME
!     UZ LA TROISIEME
!
    ux(1) = pgl(1,1)
    ux(2) = pgl(1,2)
    ux(3) = pgl(1,3)
!
    uy(1) = pgl(2,1)
    uy(2) = pgl(2,2)
    uy(3) = pgl(2,3)
!
    uz(1) = pgl(3,1)
    uz(2) = pgl(3,2)
    uz(3) = pgl(3,3)
!
!
    do 12 i = 1, 3
        zr(jrepl1-1+i)=ux(i)
        zr(jrepl2-1+i)=uy(i)
        zr(jrepl3-1+i)=uz(i)
12  end do
!
end subroutine
