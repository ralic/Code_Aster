subroutine te0048(nomopt, nomte)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/matro2.h"
#include "asterfort/jevech.h"
#include "asterfort/terefe.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvlg.h"
    character(len=16) :: nomte, nomopt
! ----------------------------------------------------------------------
! IN OPTION    : K16 :  OPTION DE CALCUL
!     'FORC_NODA'  'REFE_FORC_NODA'
! IN NOMTE     : K16 : NOM DU TYPE ELEMENT
!     POUTRE COURBE DE TIMOSHENKO
!        'MECA_POU_C_T'   : SECTION CONSTANTE
!     ------------------------------------------------------------------
    integer :: ivectu, in, nc, i, icontg, lorien, lx, lrcou
    real(kind=8) :: forref, momref, xl, pgl1(3, 3), pgl2(3, 3)
    real(kind=8) :: fs(14), rad, angarc, angs2
    character(len=16) :: ch16
!     ------------------------------------------------------------------
    ASSERT(nomte .eq. 'MECA_POU_C_T')
    nc = 6
    call jevech('PVECTUR', 'E', ivectu)
    if (nomopt(1:14) .eq. 'REFE_FORC_NODA') then
        call terefe('EFFORT_REFE', 'MECA_POUTRE', forref)
        call terefe('MOMENT_REFE', 'MECA_POUTRE', momref)
!
        do  in = 1, 2
            do  i = 1, 3
                zr(ivectu+(in-1)*nc+i-1)=forref
            enddo
            do  i = 4, nc
                zr(ivectu+(in-1)*nc+i-1)=momref
            enddo
        enddo
    elseif (nomopt .eq. 'FORC_NODA') then
        call jevech('PCONTMR', 'L', icontg)
        do  in = 1, nc
            fs(in) = -zr(icontg+in-1)
            fs(in+nc) = zr(icontg+in+nc-1)
        enddo
        call jevech('PCAORIE', 'L', lorien)
!        MATRICE DE ROTATION MGL
!        POUTRE COURBE DE TIMOSKENKO A 6 DDL: COORDONNEES DES NOEUDS
        call jevech('PGEOMER', 'L', lx)
        lx = lx - 1
        xl = sqrt(( zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))** 2 + (zr(lx+6)-zr(lx+3) )**2&
                )
        if (xl .eq. 0.d0) then
            ch16 = ' ?????????'
            call utmess('F', 'ELEMENTS2_43', sk=ch16(:8))
        endif
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        angs2 = trigom('ASIN',xl/ (2.0d0*rad))
        call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
        call utpvlg(1, nc, pgl1, fs, zr(ivectu))
        call utpvlg(1, nc, pgl2, fs(7), zr(ivectu+6))
    else
        ASSERT(.false.)
    endif
end subroutine
