subroutine pomass(nomte, e, xnu, rho, kanl, mlv)
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
!
!
! --------------------------------------------------------------------------------------------------
!
!     calcul de la matrice de masse des elements de poutre
!
! --------------------------------------------------------------------------------------------------
!
!   nomte   : nom du type element 'meca_pou_d_e'  'meca_pou_d_t'
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
!
    integer :: kanl
    character(len=*) :: nomte
    real(kind=8) :: mlv(*), e, xnu, rho
!
#include "jeveux.h"
#include "asterfort/lonele.h"
#include "asterfort/masstg.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/ptma01.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: istruc, itype
    real(kind=8) :: g, a, a2, alfay, alfay2, alfaz, alfaz2, ey, ez
    real(kind=8) :: xiy, xjx, xjx2, xiy2, xiz, xiz2, xl
    real(kind=8) :: mlv2(78)
    character(len=16) :: ch16
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cara = 17
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1','EY1','EZ1','JX1',&
                    'A2','IY2','IZ2','AY2','AZ2','EY2','EZ2','JX2','TVAR'/
!
! --------------------------------------------------------------------------------------------------
!
    g = e/(2.0d0* (1.0d0+xnu))
    mlv2(:) = 0.0d0
!
!   recuperation des caracteristiques generales des sections
    xl = lonele()
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
    a      = vale_cara(1)
    xiy    = vale_cara(2)
    xiz    = vale_cara(3)
    alfay  = vale_cara(4)
    alfaz  = vale_cara(5)
    xjx    = vale_cara(8)
    a2     = vale_cara(9)
    xiy2   = vale_cara(10)
    xiz2   = vale_cara(11)
    alfay2 = vale_cara(12)
    alfaz2 = vale_cara(13)
    xjx2   = vale_cara(16)
    ey = (vale_cara(6) +vale_cara(14))/2.d0
    ez = (vale_cara(7) +vale_cara(15))/2.d0
    itype = nint(vale_cara(17))
!
    istruc = 1
    if (nomte .eq. 'MECA_POU_D_E') then
!       POUTRE DROITE D'EULER A 6 DDL
        alfay = 0.d0
        alfaz = 0.d0
        alfay2 = 0.d0
        alfaz2 = 0.d0
    else if (nomte.eq.'MECA_POU_D_T') then
!       POUTRE DROITE DE TIMOSKENKO A 6 DDL
!       remarque : dans les poutres courbes maintenant supprimées,
!       la correction de IY et IZ avec COEF_FLEX n'était pas faite pour
!       le calcul de la matrice de masse. On a gardé cette logique.
    else if (nomte.eq.'MECA_POU_D_TG') then
!       poutre droite de timoskenko a 7 ddl
        a2 = a
    else
        ch16 = nomte
        call utmess('F', 'ELEMENTS2_42', sk=ch16)
    endif
!
!   calcul de la matrice de masse locale
    call ptma01(kanl, itype, mlv2, istruc, rho, e, a, a2, xl, xiy,&
                xiy2, xiz, xiz2, g, alfay, alfay2, alfaz, alfaz2, ey, ez)
    if (nomte.eq. 'MECA_POU_D_TG')then
        call masstg(mlv2, mlv)
    else
        mlv(1:78) = mlv2(1:78)
    endif
!
end subroutine
