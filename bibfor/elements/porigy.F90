subroutine porigy(nomte, e, rho, xnu, icdmat,&
                  klv, nl)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/pmfitx.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/ptgy02.h"
#include "asterfort/utmess.h"
#include "asterfort/lteatt.h"
!
    integer :: icdmat
    character(len=*) :: nomte
    real(kind=8) :: e, rho, xnu, klv(*)
! ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!     CALCULE LA MATRICE GYROSCOPIQUE DES ELEMENTS DE POUTRE
!
! IN  NOMTE : NOM DU TYPE ELEMENT
!             'MECA_POU_D_E'  'MECA_POU_D_T'  'MECA_POU_D_TG'
!             'MECA_POU_D_EM' 'MECA_POU_D_TGM'
!     ------------------------------------------------------------------
    integer :: nl
!
    character(len=16) :: ch16
    integer :: lx, istruc, itype
    real(kind=8) :: zero, deux, rbid, casect(6)
    real(kind=8) :: ey, ez, xl
    real(kind=8) :: a, xiy, xiz, xjx, alfay, alfaz, alfinv
    real(kind=8) :: a2, xiy2, xiz2, xjx2, alfay2, alfaz2
    aster_logical :: euler
!     ------------------------------------------------------------------
    integer, parameter :: nb_cara = 17
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1','EY1','EZ1','JX1',&
                    'A2','IY2','IZ2','AY2','AZ2','EY2','EZ2','JX2','TVAR'/
!-----------------------------------------------------------------------
!
    zero = 0.d0
    deux = 2.d0
    euler=lteatt('EULER','OUI')
!
!
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!
    call lonele(3, lx, xl)
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
    a = vale_cara(1)
    xiy = vale_cara(2)
    xiz = vale_cara(3)
    alfay = vale_cara(4)
    alfaz = vale_cara(5)
    xjx = vale_cara(8)
    a2 = vale_cara(9)
    xiy2 = vale_cara(10)
    xiz2 = vale_cara(11)
    alfay2 = vale_cara(12)
    alfaz2 = vale_cara(13)
    xjx2 = vale_cara(16)
    ey = (vale_cara(6) +vale_cara(14))/2.d0
    ez = (vale_cara(7) +vale_cara(15))/2.d0
    itype = nint(vale_cara(17))
!
    if (nomte .eq. 'MECA_POU_D_E') then
!        --- POUTRE DROITE D'EULER A 6 DDL ---
        istruc = 1
        alfinv = zero
    else if (nomte.eq.'MECA_POU_D_T') then
!        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
        istruc = 1
        alfinv = deux/(alfay+alfaz)
    else if (nomte.eq.'MECA_POU_D_EM' .or. nomte.eq.'MECA_POU_D_TGM') then
!        --- POUTRE DROITE MULTI-FIBRES---
        itype = 0
        istruc = 1
        alfinv = zero
!       ON MET RHO=1, il est utilisé dans PMFITX
        rho = 1.d0
        call pmfitx(icdmat, 2, casect, rbid)
        a = casect(1)
        xiy = casect(5)
        xiz = casect(4)
        xjx = 0.d0
    else
        ch16 = nomte
        call utmess('F', 'ELEMENTS2_42', sk=ch16)
    endif
!
!
    if (itype .eq. 1 .or. itype .eq. 2) then
!     ---- MOYENNAGE -------------------------------------
        a=(a+a2)/deux
        xiy=(xiy+xiy2)/deux
        xiz=(xiz+xiz2)/deux
        alfay=(alfay+alfay2)/deux
        alfaz=(alfaz+alfaz2)/deux
        xjx=(xjx+xjx2)/deux
        if (euler) then
            alfinv = zero
        else
            alfinv = deux/(alfay+alfaz)
        endif
    endif
    call ptgy02(klv, nl, xnu, rho, a,&
                xl, xiy, xiz, xjx, alfinv,&
                ey, ez, istruc)
!
end subroutine
