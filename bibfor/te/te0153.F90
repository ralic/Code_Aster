subroutine te0153(option, nomte)
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
    implicit none
    character(len=*) :: option, nomte
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/matrot.h"
#include "asterfort/pmavec.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/vecma.h"
!
! --------------------------------------------------------------------------------------------------
!
!     CALCULE LES MATRICES ELEMENTAIRES DES ELEMENTS DE BARRE
!
! --------------------------------------------------------------------------------------------------
!
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'RIGI_MECA'      : CALCUL DE LA MATRICE DE RAIDEUR
!        'MASS_MECA'      : CALCUL DE LA MATRICE DE MASSE
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_BARRE' : ELEMENT BARRE
!        'MECA_2D_BARRE' : ELEMENT BARRE
!
! --------------------------------------------------------------------------------------------------
!
    integer :: codres(1)
    integer :: i, iacce, imate, lmat, lorien, lsect
    integer :: lvec, nc, nno
    real(kind=8) :: e(1), rho(1), pgl(3, 3), mat(21), matr(21)
    real(kind=8) :: a, xl, xrig, xmas, matp(6, 6), mat2dm(4, 4), mat2dv(10)
    real(kind=8) :: r8b
    character(len=16) :: ch16
!
! --------------------------------------------------------------------------------------------------
!
    r8b = 0.d0
    call jevech('PCAGNBA', 'L', lsect)
    a = zr(lsect)
    nno = 2
    nc = 3
!
!   Longueur de l'élément
    if (nomte .eq. 'MECA_BARRE') then
        xl =  lonele()
    else if (nomte.eq.'MECA_2D_BARRE') then
        xl = lonele(dime=2)
    else
        xl = 0.0d0
        ASSERT( ASTER_FALSE )
    endif
!
!   recuperation des orientations alpha,beta,gamma
    call jevech('PCAORIE', 'L', lorien)
    if (option .eq. 'M_GAMMA') then
        call jevech('PVECTUR', 'E', lvec)
        call jevech('PACCELR', 'L', iacce)
    else
        call jevech('PMATUUR', 'E', lmat)
    endif
!
!   calcul des matrices elementaires
    mat(:) = 0.d0
    call jevech('PMATERC', 'L', imate)
    if (option .eq. 'RIGI_MECA') then
        call rcvalb('FPG1', 1, 1, '+', zi(imate), ' ', 'ELAS', 0, ' ', [r8b],&
                    1, 'E', e, codres, 1)
        xrig = e(1) * a / xl
        mat( 1) =  xrig
        mat( 7) = -xrig
        mat(10) =  xrig
!
    else if (option.eq.'MASS_MECA' .or. option.eq.'M_GAMMA') then
        call rcvalb('FPG1', 1, 1, '+', zi(imate), ' ', 'ELAS', 0, ' ', [r8b],&
                    1, 'RHO', rho, codres, 1)
        matr(:) = 0.d0
!
        xmas = rho(1) * a * xl / 6.d0
        mat( 1) = xmas * 2.d0
        mat( 3) = xmas * 2.d0
        mat( 6) = xmas * 2.d0
        mat( 10) = xmas * 2.d0
        mat( 15) = xmas * 2.d0
        mat( 21) = xmas * 2.d0
!
        mat( 7) = xmas
        mat( 12) = xmas
        mat( 18) = xmas
!
    else if ( (option.eq.'MASS_MECA_DIAG').or.(option.eq.'MASS_MECA_EXPLI') ) then
        call rcvalb('FPG1', 1, 1, '+', zi(imate), ' ', 'ELAS', 0, ' ', [r8b],&
                    1, 'RHO', rho, codres, 1)
        xmas = rho(1) * a * xl / 2.d0
        mat( 1) = xmas
        mat( 3) = xmas
        mat( 6) = xmas
        mat(10) = xmas
        mat(15) = xmas
        mat(21) = xmas
!
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!
!   passage du repere local au repere global
    call matrot(zr(lorien), pgl)
    call utpslg(nno, nc, pgl, mat, matr)
!
    if (option .eq. 'M_GAMMA') then
        if (nomte .eq. 'MECA_BARRE') then
            matp(:,:)=0.d+0
            call vecma(matr, 21, matp, 6)
            call pmavec('ZERO', 6, matp, zr(iacce), zr(lvec))
        else
            mat2dv(1) = matr(1)
            mat2dv(2) = matr(2)
            mat2dv(3) = matr(3)
            mat2dv(4) = matr(7)
            mat2dv(5) = matr(8)
            mat2dv(6) = matr(10)
            mat2dv(7) = matr(11)
            mat2dv(8) = matr(12)
            mat2dv(9) = matr(14)
            mat2dv(10) = matr(15)
            mat2dm(:,:)=0.d+0
            call vecma(mat2dv, 10, mat2dm, 4)
            call pmavec('ZERO', 4, mat2dm, zr(iacce), zr(lvec))
        endif
    else
!       ecriture dans le vecteur pmattur suivant l'element
        if (nomte .eq. 'MECA_BARRE') then
            do i = 1, 21
                zr(lmat+i-1) = matr(i)
            enddo
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(lmat)   = matr(1)
            zr(lmat+1) = matr(2)
            zr(lmat+2) = matr(3)
            zr(lmat+3) = matr(7)
            zr(lmat+4) = matr(8)
            zr(lmat+5) = matr(10)
            zr(lmat+6) = matr(11)
            zr(lmat+7) = matr(12)
            zr(lmat+8) = matr(14)
            zr(lmat+9) = matr(15)
        endif
    endif
!
end subroutine
