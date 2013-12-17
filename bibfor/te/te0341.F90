subroutine te0341(option, nomte)
!
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
    implicit none
#include "jeveux.h"
#include "asterfort/cgfono.h"
#include "asterfort/cgfore.h"
#include "asterfort/cginit.h"
#include "asterfort/cgtang.h"
#include "asterfort/elref2.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/terefe.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  FORC_NODA ET REFE_FORC_NODA
!                          POUR ELEMENTS GAINE/CABLE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!      INSPIRE DE TE0361
! ......................................................................
!
    character(len=8) :: lielrf(10)
    integer :: nno1, nno2, npg, ivf2, idf2, nnos, jgn
    integer :: iw, ivf1, idf1, igeom, icontm, ivectu, ndim, ntrou
    integer :: npgn, iwn, ivf1n, idf1n, jgnn
    integer :: iu(3, 3), iuc(3), im(3), isect
    real(kind=8) :: tang(3, 3), forref, sigref, depref, a
!
!
    call elref2(nomte, 2, lielrf, ntrou)
    call elref4(lielrf(1), 'RIGI', ndim, nno1, nnos,&
                npg, iw, ivf1, idf1, jgn)
    call elref4(lielrf(1), 'NOEU', ndim, nno1, nnos,&
                npgn, iwn, ivf1n, idf1n, jgnn)
    call elref4(lielrf(2), 'RIGI', ndim, nno2, nnos,&
                npg, iw, ivf2, idf2, jgn)
    ndim=3
!
! - DECALAGE D'INDICE POUR LES ELEMENTS D'INTERFACE
    call cginit(nomte, iu, iuc, im)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)

!     DEFINITION DES TANGENTES
!
    call cgtang(3, nno1, npgn, zr(igeom), zr(idf1n),&
                tang)
!
!      OPTIONS FORC_NODA ET REFE_FORC_NODA
!
    if (option .eq. 'FORC_NODA') then
!
        call jevech('PCONTMR', 'L', icontm)
        call cgfono(ndim, nno1, nno2, npg, zr(iw),&
                    zr(ivf1), zr(ivf2), zr(idf1), zr(igeom), tang,&
                    iu, iuc, im, zr(icontm), zr(ivectu))
!
    else
!
        call jevech('PCAGNBA', 'L', isect)
        a = zr(isect)
!
        call terefe('SIGM_REFE', 'MECA_CG', sigref)
        call terefe('DEPL_REFE', 'MECA_CG', depref)
        call terefe('EFFORT_REFE', 'MECA_CG', forref)
!
        call cgfore(ndim, nno1, nno2, npg, zr(iw),&
                    zr(ivf1), zr(ivf2), zr(idf1), a, zr(igeom),&
                    tang, iu, iuc, im, forref,&
                    sigref, depref, zr(ivectu))
!
    endif
!
end subroutine
