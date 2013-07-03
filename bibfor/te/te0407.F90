subroutine te0407(option, nomte)
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
!
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/nmas3d.h"
#include "asterfort/rcangm.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "blas/dcopy.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          ELEMENT HEXAS8
!          => 1 POINT DE GAUSS + STABILISATION ASSUMED STRAIN
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    character(len=8) :: typmod(2)
    integer :: jgano, nno, npg, i, imatuu, lgpg, ndim, lgpg1, iret
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: icontm, ivarim
    integer :: iinstm, iinstp, ideplm, ideplp, icompo, icarcr
    integer :: ivectu, icontp, ivarip, jcret, codret
    integer :: ivarix
    integer :: jtab(7), nnos, idim
    real(kind=8) :: def(6, 3, 8), dfdi(8, 3)
    real(kind=8) :: angmas(7), bary(3)
!
    icontp=1
    ivarip=1
    imatuu=1
    ivectu=1
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
! - TYPE DE MODELISATION
    typmod(1) = '3D      '
    typmod(2) = '        '
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PCARCRI', 'L', icarcr)
    call tecach('OON', 'PVARIMR', 'L', 7, jtab,&
                iret)
    lgpg1 = max(jtab(6),1)*jtab(7)
    lgpg = lgpg1
    bary(1) = 0.d0
    bary(2) = 0.d0
    bary(3) = 0.d0
    do 150 i = 1, nno
        do 140 idim = 1, ndim
            bary(idim) = bary(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
140      continue
150  end do
    call rcangm(ndim, bary, angmas)
!
!
! - VARIABLES DE COMMANDE
!
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
!
!
! - PARAMETRES EN SORTIE
!
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PMATUUR', 'E', imatuu)
    endif
!
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
!
!      ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
        call jevech('PVARIMP', 'L', ivarix)
        call dcopy(npg*lgpg, zr(ivarix), 1, zr(ivarip), 1)
!
    endif
!
!
    if (zk16(icompo+3) (1:9) .eq. 'COMP_ELAS') then
!
! - LOIS DE COMPORTEMENT ECRITES EN CONFIGURATION DE REFERENCE
!                          COMP_ELAS
!
        call u2mess('F', 'ELEMENTS4_73')
!
    else
!
! - LOIS DE COMPORTEMENT ECRITE EN CONFIGURATION ACTUELLE
!                          COMP_INCR
!      PETITES DEFORMATIONS (AVEC EVENTUELLEMENT REACTUALISATION)
        if (zk16(icompo+2) (1:5) .eq. 'PETIT') then
            if (zk16(icompo+2) (6:10) .eq. '_REAC') then
                do 20 i = 1, 3*nno
                    zr(igeom+i-1) = zr(igeom+i-1) + zr(ideplm+i-1) + zr(ideplp+i-1)
20              continue
            endif
            call nmas3d('RIGI', nno, npg, ipoids, ivf,&
                        idfde, zr(igeom), typmod, option, zi(imate),&
                        zk16(icompo), lgpg, zr(icarcr), zr(iinstm), zr(iinstp),&
                        zr(ideplm), zr(ideplp), angmas, zr(icontm), zr(ivarim),&
                        dfdi, def, zr(icontp), zr(ivarip), zr(imatuu),&
                        zr(ivectu), codret)
!
!
        else
            call u2mesk('F', 'ELEMENTS3_16', 1, zk16(icompo+2))
        endif
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
end subroutine
