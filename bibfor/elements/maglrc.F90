subroutine maglrc(zimat, matr, delas, ecr)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/jevech.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "asterfort/utmess.h"
!
    integer :: i, jcoqu, icacoq, zimat
!
    real(kind=8) :: matr(*), delas(6, 6), r8b
    real(kind=8) :: valres(15), vglob(3), epais
    real(kind=8) :: ecr(*), alph, beta, vel
!
    integer :: codres(15)
    character(len=16) :: nomres(15)
    character(len=32) :: phenom
!
    r8b = 0.d0
    call r8inir(6*6, 0.0d0, delas, 1)
    phenom = 'GLRC_DAMAGE'

!     EPAISSEUR
    call jevech('PCACOQU', 'L', jcoqu)
    epais = zr(jcoqu)
    nomres(1) = 'EPAIS'
    call rcvala(zimat, ' ', phenom, 0, ' ', [r8b], 1, nomres, valres, codres, 1)
    if (valres(1) .ne. epais) then
        valres(2) = epais
        call utmess('F', 'ELEMENTS5_42', nr=2, valr=valres)
    endif
!
!     ELAS
!     ATTENTION PARAMETRES EQUIVALENTS EN FLEXION
!     EF ET NUEF
!
    nomres(1) = 'E_F'
    nomres(2) = 'NU_F'
!
    call rcvala(zimat, ' ', 'ELAS_GLRC       ', 0, ' ', [r8b], 2, nomres, valres, codres, 1)
    matr(6) = valres(1)
    matr(7) = valres(2)
!
!     GLRC_DAMAGE
!
!     MATRICE ELASTIQUE MEMBRANE/CISAILLEMENT
!
    nomres(1)  = 'BN11'
    nomres(2)  = 'BN12'
    nomres(3)  = 'BN22'
    nomres(4)  = 'BN33'
    nomres(5)  = 'BT1'
    nomres(6)  = 'BT2'
    nomres(7)  = 'BM11'
    nomres(8)  = 'BM12'
    nomres(9)  = 'BM22'
    nomres(10) = 'BM33'
!
    call rcvala(zimat, ' ', phenom, 0, ' ', [r8b], 10, nomres, valres, codres, 1)
!
    matr(1) = 1.0d0
    matr(2) = valres(1)
    matr(3) = valres(2)
    matr(4) = valres(3)
    matr(5) = valres(4)
    delas(4,4) = valres(7)
    delas(4,5) = valres(8)
    delas(5,4) = delas(4,5)
    delas(5,5) = valres(9)
    delas(6,6) = valres(10)
    matr(14) = valres(5)
    matr(15) = valres(6)
!
!     SEUILS ET PENTES
!
    nomres(1) = 'MF1'
    nomres(2) = 'MF2'
    nomres(3) = 'QP1'
    nomres(4) = 'QP2'
    nomres(5) = 'GAMMA'
!
    call rcvala(zimat, ' ', phenom, 0, ' ', [r8b], 5, nomres, valres, codres, 1)
!
    matr(8) = valres(1)
    matr(9) = valres(2)
    matr(10) = valres(3)
    matr(11) = valres(4)
    matr(12) = valres(5)
!
!     PARAMETRES TENSEUR DE PRAGER
!     MEMBRANE
    nomres(1) = 'C1N1'
    nomres(2) = 'C1N2'
    nomres(3) = 'C1N3'
    nomres(4) = 'C2N1'
    nomres(5) = 'C2N2'
    nomres(6) = 'C2N3'
!
    call rcvala(zimat, ' ', phenom, 0, ' ', [r8b], 6, nomres, valres, codres, 1)
!
    matr(16) = valres(1)
    matr(17) = valres(2)
    matr(18) = valres(3)
    matr(22) = valres(4)
    matr(23) = valres(5)
    matr(24) = valres(6)
!
!     FLEXION
!
    nomres(1) = 'C1M1'
    nomres(2) = 'C1M2'
    nomres(3) = 'C1M3'
    nomres(4) = 'C2M1'
    nomres(5) = 'C2M2'
    nomres(6) = 'C2M3'
!
    call rcvala(zimat, ' ', phenom, 0, ' ', [r8b], 6, nomres, valres, codres, 1)
!
    matr(19) = valres(1)
    matr(20) = valres(2)
    matr(21) = valres(3)
    matr(25) = valres(4)
    matr(26) = valres(5)
    matr(27) = valres(6)
!
    delas(1,1) = matr(2)
    delas(1,2) = matr(3)
    delas(2,1) = delas(1,2)
    delas(2,2) = matr(4)
    delas(3,3) = matr(5)
!
    call jevech('PCACOQU', 'L', icacoq)
    alph = zr(icacoq+1)*r8dgrd()
    beta = zr(icacoq+2)*r8dgrd()
!
    vglob(1) = cos(beta)*cos(alph)
    vglob(2) = cos(beta)*sin(alph)
    vglob(3) = -sin(beta)
    vel = vglob(1)*vglob(1) + vglob(2)*vglob(2)
    vel = vel + vglob(3)*vglob(3)
    vel = sqrt(vel)
    do i = 1, 3
        ecr(10 + i) = vglob(i)/vel
    end do
!
end subroutine
