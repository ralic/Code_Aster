subroutine te0543(option, nomte)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lcegeo.h"
#include "asterfort/lteatt.h"
#include "asterfort/pipeed.h"
#include "asterfort/pipepe.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES COEFFICIENTS A0 ET A1
!                          POUR LE PILOTAGE PAR CRITERE ELASTIQUE
!                          OU PAR INCREMENT DE DEFORMATION POUR LES
!                          ELEMENTS A VARIABLES LOCALES
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: typmod(2)
    character(len=16) :: compor, pilo
!
    integer :: jgano, ndim, nno, nnos, npg, lgpg, jtab(7), itype
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: icontm, ivarim, icopil, iborne, ictau
    integer :: ideplm, iddepl, idepl0, idepl1, icompo, iret
    real(kind=8) :: dfdi(2187), elgeom(10, 27)
!
!
!
!
!
! - TYPE DE MODELISATION
!
    if (lteatt('DIM_TOPO_MODELI','3')) then
        typmod(1) = '3D'
    else if (lteatt('AXIS','OUI')) then
        typmod(1) = 'AXIS'
    else if (lteatt('C_PLAN','OUI')) then
        typmod(1) = 'C_PLAN'
    else if (lteatt('D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN'
    endif
!
    typmod(2) = 'DEPLA'
!
    if (lteatt('TYPMOD2','ELEMDISC')) then
        typmod(2)='ELEMDISC'
    endif
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    ASSERT(nno .le. 27)
    ASSERT(npg .le. 27)
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PDDEPLR', 'L', iddepl)
    call jevech('PDEPL0R', 'L', idepl0)
    call jevech('PDEPL1R', 'L', idepl1)
    call jevech('PTYPEPI', 'L', itype)
!
    pilo = zk16(itype)
    compor = zk16(icompo)
    if (pilo .eq. 'PRED_ELAS') then
        call jevech('PCDTAU', 'L', ictau)
        call jevech('PBORNPI', 'L', iborne)
    endif
!
!
! -- NOMBRE DE VARIABLES INTERNES
!
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=jtab)
    lgpg = max(jtab(6),1)*jtab(7)
!
! - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES LOIS DE COMPORTEMENT
!
    if (compor .eq. 'BETON_DOUBLE_DP') then
        call lcegeo(nno, npg, ipoids, ivf, idfde,&
                    zr(igeom), typmod, compor, ndim, dfdi,&
                    zr(ideplm), zr(iddepl), elgeom)
    endif
!
! PARAMETRES EN SORTIE
!
    call jevech('PCOPILO', 'E', icopil)
!
    if (typmod(2) .eq. 'ELEMDISC') then
        call pipeed(nno, npg, ipoids, ivf, idfde,&
                    zr(igeom), typmod, zi(imate), lgpg, zr(ideplm),&
                    zr(ivarim), zr(iddepl), zr(idepl0), zr(idepl1), dfdi,&
                    zr(ictau), zr(icopil))
    else
        call pipepe(pilo, ndim, nno, npg, ipoids,&
                    ivf, idfde, zr( igeom), typmod, zi(imate),&
                    zk16(icompo), lgpg, zr(ideplm), zr( icontm), zr(ivarim),&
                    zr(iddepl), zr(idepl0), zr(idepl1), zr( icopil), dfdi,&
                    elgeom, iborne, ictau)
    endif
!
end subroutine
