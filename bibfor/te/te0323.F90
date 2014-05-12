subroutine te0323(option, nomte)
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
!
#include "asterfort/ejfono.h"
#include "asterfort/ejfore.h"
#include "asterfort/ejinit.h"
#include "asterfort/elref2.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/terefe.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!    OPTION FORC_NODA ET REFE_FORC_NODA POUR LES JOINTS QUADRA ET HYME
! ----------------------------------------------------------------------
!
    character(len=8) :: lielrf(10)
    logical :: axi
    integer :: nno1, nno2, npg, ivf2, idf2, nnos, jgn, nddl
    integer :: iw, ivf1, idf1, igeom, ivectu, icontm, ndim, ntrou
    integer :: iu(3, 16), ip(4)
    real(kind=8) :: sigref, fhyref
!
    call elref2(nomte, 2, lielrf, ntrou)
    call elrefe_info(elrefe=lielrf(1),fami='RIGI',ndim=ndim,nno=nno1,nnos=nnos,&
  npg=npg,jpoids=iw,jvf=ivf1,jdfde=idf1,jgano=jgn)
    call elrefe_info(elrefe=lielrf(2),fami='RIGI',ndim=ndim,nno=nno2,nnos=nnos,&
  npg=npg,jpoids=iw,jvf=ivf2,jdfde=idf2,jgano=jgn)
    ndim = ndim + 1
    nddl = 2*ndim*nno1 + nno2
    axi = lteatt('AXIS','OUI')
!
! - DECALAGE D'INDICE POUR LES ELEMENTS DE JOINT
    call ejinit(nomte, iu, ip)
!
    call jevech('PVECTUR', 'E', ivectu)
    call jevech('PGEOMER', 'L', igeom)
!
!      OPTIONS FORC_NODA ET REFE_FORC_NODA
!
    if (option .eq. 'FORC_NODA') then
!
        call jevech('PCONTMR', 'L', icontm)
!
        call ejfono(ndim, nddl, axi, nno1, nno2,&
                    npg, iw, zr(iw), zr(ivf1), zr(ivf2),&
                    idf2, zr(idf2), zr(igeom), iu, ip,&
                    zr(icontm), zr(ivectu))
!
    else if (option .eq. 'REFE_FORC_NODA') then
!
        call terefe('SIGM_REFE', 'THM_JOINT', sigref)

!      EN MECA PURE ON IMPOSE LA VALEUR DE FLUX DE REFERENCE A 1
        if (lteatt('TYPMOD2','EJ_HYME')) then      
            call terefe('FLUX_HYD1_REFE', 'THM_JOINT', fhyref)
        else if (lteatt('TYPMOD2','ELEMJOIN')) then
            fhyref = 1.D0
        endif
!
        call ejfore(ndim, nddl, axi, nno1, nno2,&
                    npg, iw, zr(iw), zr(ivf1), zr(ivf2),&
                    idf2, zr(idf2), zr(igeom), iu, ip,&
                    sigref, fhyref, zr( ivectu))
    endif
!
!
end subroutine
