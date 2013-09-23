subroutine te0359(option, nomte)
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
! person_in_charge: jerome.laverne at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/eiangl.h"
#include "asterfort/eiinit.h"
#include "asterfort/elref2.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/pipeei.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
!
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  PILOTAGE PRED_ELAS
!                          POUR LES ELEMENTS D'INTERFACE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
    character(len=8) :: lielrf(10)
    logical :: axi
    integer :: nno1, nno2, npg, lgpg, ndim, iret, ntrou, iu(3, 18), im(3, 9)
    integer :: it(18)
    integer :: iw, ivf1, idf1, igeom, imate, ivf2, idf2, nnos, jgn, jtab(7)
    integer :: ivarim, icopil, ictau, iddlm, iddld, iddl0, iddl1, icompo, icamas
    real(kind=8) :: ang(24)
!
!
!
!
! - FONCTIONS DE FORME
!
    call elref2(nomte, 2, lielrf, ntrou)
    call elref4(lielrf(1), 'RIGI', ndim, nno1, nnos,&
                npg, iw, ivf1, idf1, jgn)
    call elref4(lielrf(2), 'RIGI', ndim, nno2, nnos,&
                npg, iw, ivf2, idf2, jgn)
    ndim = ndim + 1
    axi = lteatt(' ','AXIS','OUI')
!
! - DECALAGE D'INDICE POUR LES ELEMENTS D'INTERFACE
    call eiinit(nomte, iu, im, it)
!
! --- ORIENTATION DE L'ELEMENT D'INTERFACE : REPERE LOCAL
!     RECUPERATION DES ANGLES NAUTIQUES DEFINIS PAR AFFE_CARA_ELEM
!
    call jevech('PCAMASS', 'L', icamas)
    if (zr(icamas) .eq. -1.d0) then
        call utmess('F', 'ELEMENTS5_47')
    endif
!
!     DEFINITION DES ANGLES NAUTIQUES AUX NOEUDS SOMMETS : ANG
!
    call eiangl(ndim, nno2, zr(icamas+1), ang)
!
! - PARAMETRES
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PDEPLMR', 'L', iddlm)
    call jevech('PDDEPLR', 'L', iddld)
    call jevech('PDEPL0R', 'L', iddl0)
    call jevech('PDEPL1R', 'L', iddl1)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PCDTAU', 'L', ictau)
    call jevech('PCOPILO', 'E', icopil)
!
!    NOMBRE DE VARIABLES INTERNES
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=jtab)
    lgpg = max(jtab(6),1)*jtab(7)
!
!
! - PILOTAGE PRED_ELAS
!
    call pipeei(ndim, axi, nno1, nno2, npg,&
                zr(iw), zr(ivf1), zr(ivf2), zr(idf2), zr(igeom),&
                ang, zi(imate), zk16(icompo), lgpg, zr(iddlm),&
                zr(iddld), zr(iddl0), zr(iddl1), zr(ictau), zr(ivarim),&
                iu, im, zr(icopil))
!
end subroutine
