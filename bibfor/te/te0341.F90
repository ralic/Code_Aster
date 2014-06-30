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
#include "asterfort/elrefe_info.h"
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
    integer :: npgn, iwn, ivf1n, idf1n, jgnn, ino, i, nddl1
    integer :: iu(3, 3), iuc(3), im(3), isect, iddlm, icompo
    real(kind=8) :: tang(3, 3), forref, sigref, depref, a
    real(kind=8) :: geom(3, 3)
    logical(kind=1) :: reactu
!
!
    call elref2(nomte, 2, lielrf, ntrou)
    call elrefe_info(elrefe=lielrf(1),fami='RIGI',ndim=ndim,nno=nno1,nnos=nnos,&
  npg=npg,jpoids=iw,jvf=ivf1,jdfde=idf1,jgano=jgn)
    call elrefe_info(elrefe=lielrf(1),fami='NOEU',ndim=ndim,nno=nno1,nnos=nnos,&
  npg=npgn,jpoids=iwn,jvf=ivf1n,jdfde=idf1n,jgano=jgnn)
    call elrefe_info(elrefe=lielrf(2),fami='RIGI',ndim=ndim,nno=nno2,nnos=nnos,&
  npg=npg,jpoids=iw,jvf=ivf2,jdfde=idf2,jgano=jgn)
    ndim=3
    nddl1 = 5
!
! - DECALAGE D'INDICE POUR LES ELEMENTS D'INTERFACE
    call cginit(nomte, iu, iuc, im)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)
!
!     MISE A JOUR EVENTUELLE DE LA GEOMETRIE
!
    reactu = .false.
    if (option .eq. 'FORC_NODA')then
        call jevech('PCOMPOR', 'L', icompo)
        if (zk16(icompo+2) .eq. 'PETIT_REAC') reactu = .true.
    endif
    if (.not. reactu)then
        do ino = 1, nno1
            do i = 1, ndim
                geom(i,ino) = zr(igeom-1+(ino-1)*ndim+i)
            enddo
        enddo
    else
        call jevech('PDEPLMR', 'L', iddlm)
        do ino = 1, nno1
            do i = 1, ndim
                geom(i,ino) = zr(igeom-1+(ino-1)*ndim+i)&
                            + zr(iddlm-1+(ino-1)*nddl1+i)
            enddo
        enddo
    endif
!     DEFINITION DES TANGENTES
!
    call cgtang(3, nno1, npgn, geom, zr(idf1n),&
                tang)
!
!      OPTIONS FORC_NODA ET REFE_FORC_NODA
!
    if (option .eq. 'FORC_NODA') then
!
        call jevech('PCONTMR', 'L', icontm)
        call cgfono(ndim, nno1, nno2, npg, zr(iw),&
                    zr(ivf1), zr(ivf2), zr(idf1), geom, tang,&
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
                    zr(ivf1), zr(ivf2), zr(idf1), a, geom,&
                    tang, iu, iuc, im, forref,&
                    sigref, depref, zr(ivectu))
!
    endif
!
end subroutine
