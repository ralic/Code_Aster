subroutine mpglcp(typecp, nbnolo, coordo, alpha, beta,&
                  gamma, pgl, iret)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/angvx.h'
    include 'asterfort/assert.h'
    include 'asterfort/coqrep.h'
    include 'asterfort/dxqpgl.h'
    include 'asterfort/dxtpgl.h'
    include 'asterfort/matrot.h'
    include 'asterfort/pmat.h'
    include 'asterfort/vdiff.h'
    character(len=1) :: typecp
    integer :: nbnolo, iret
    real(kind=8) :: coordo(*), alpha, beta, gamma, pgl(3, 3)
!     --- ARGUMENTS ---
! ----------------------------------------------------------------------
!  CALCUL DE LA MATRICE DE PASSAGE GLOBAL -> LOCAL COQUES ET POUTRES
!               -          -       -         -     -         -
! ----------------------------------------------------------------------
!
!  ROUTINE CALCUL DE LA MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE
!    LOCAL DANS LE CAS DES COQUES ET DES POUTRES
!
! IN  :
!   TYPECP  K1   'P' OU 'C' POUR POUTRES OU COQUES
!   NBNOLO  I    NOMBRE DE NOEUDS, CLASSIQUEMENT 2 POUR LES POUTRES
!                  ET 3 OU 4 POUR LES COQUES
!   COORDO  R*   TABLEAU CONTENANT LES COORDOONEES DES NOEUDS
!                  DIMENSIONNE A NBNOLO*3
!   ALPHA   R    PREMIER ANGLE NAUTIQUE
!   BETA    R    DEUXIEME ANGLE NAUTIQUE
!   GAMMA   R    TROISIEME ANGLE NAUTIQUE
!
!  POUR LES POUTRES, SEUL GAMMA EST A FOURNIR (CAR ALPHA ET BETA SONT
!    RECALCULES A PARTIR DES COORDONNEES)
!  POUR LES COQUES, SEULS ALPHA ET BETA SONT A FOURNIR
!
! OUT :
!   PGL     R*   LA MATRICE DE PASSAGE DE DIMENSION 3*3
!   IRET    I    CODE RETOUR
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
!
    real(kind=8) :: xd(3), angl(3), alphal, betal, t2ev(2, 2), t2ve(2, 2), c, s
    real(kind=8) :: mat1(3, 3), mat2(3, 3)
!
    if (typecp .eq. 'P') then
!
        call assert(nbnolo.eq.2)
!
!       CALCUL DE ALPHA ET BETA
        call vdiff(3, coordo(1), coordo(4), xd)
        call angvx(xd, alphal, betal)
!
        angl(1) = alphal
        angl(2) = betal
        angl(3) = gamma
        call matrot(angl, pgl)
!
    else if (typecp.eq.'C') then
!
!       CALCUL DE LA MATRICE DE PASSAGE GLOBAL -> INTRINSEQUE
        if (nbnolo .eq. 3) then
            call dxtpgl(coordo, pgl)
        else if (nbnolo.eq.4) then
            call dxqpgl(coordo, pgl, 'C', iret)
        else
            call assert(.false.)
        endif
!
!       MODIFICATION DE LA MATRICE POUR PRENDRE EN COMPTE
!       LA CARCOQUE UTILISATEUR
        call coqrep(pgl, alpha, beta, t2ev, t2ve,&
                    c, s)
        mat1(1,1) = pgl(1,1)
        mat1(1,2) = pgl(2,1)
        mat1(1,3) = pgl(3,1)
        mat1(2,1) = pgl(1,2)
        mat1(2,2) = pgl(2,2)
        mat1(2,3) = pgl(3,2)
        mat1(3,1) = pgl(1,3)
        mat1(3,2) = pgl(2,3)
        mat1(3,3) = pgl(3,3)
        mat2(1,1) = t2ve(1,1)
        mat2(1,2) = t2ve(2,1)
        mat2(1,3) = 0.d0
        mat2(2,1) = t2ve(1,2)
        mat2(2,2) = t2ve(2,2)
        mat2(2,3) = 0.d0
        mat2(3,1) = 0.d0
        mat2(3,2) = 0.d0
        mat2(3,3) = 1.d0
        call pmat(3, mat1, mat2, pgl)
!
    endif
!
end subroutine
