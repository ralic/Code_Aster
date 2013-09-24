subroutine te0350(option, nomte)
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
! ======================================================================
!    CALCUL DES OPTIONS MECANIQUES POUR LES ELEMENTS QUAS4
!    => 1 POINT DE GAUSS + STABILISATION ASSUMED STRAIN
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nmas2d.h"
#include "asterfort/rcangm.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
!
    character(len=16) :: option, nomte
!
    character(len=8) :: typmod(2)
    character(len=4) :: fami
    integer :: nno, npg1, i, imatuu, lgpg, lgpg1
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: icontm, ivarim
    integer :: iinstm, iinstp, ideplm, ideplp, icompo, icarcr
    integer :: ivectu, icontp, ivarip
    integer :: ivarix, iret, idim
    integer :: jtab(7), jcret, codret, ndim, nnos, jgano
    real(kind=8) :: vect1(54), vect3(4*27*2), xyz(3)
    real(kind=8) :: angmas(7)
!
!
!
!
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
!     MATNS MAL DIMENSIONNEE
    ASSERT(nno.le.9)
! - TYPE DE MODELISATION
!
    if (lteatt(' ','AXIS','OUI')) then
        typmod(1) = 'AXIS    '
    else if (lteatt(' ','C_PLAN','OUI')) then
        typmod(1) = 'C_PLAN  '
    else if (lteatt(' ','D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN  '
    else
!       NOM D'ELEMENT ILLICITE
        ASSERT(lteatt(' ', 'C_PLAN', 'OUI'))
    endif
!
    typmod(2) = 'ASSU    '
!
    codret = 0
!
!
! - PARAMETRES EN ENTREE
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PCARCRI', 'L', icarcr)
!
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=jtab)
    lgpg1 = max(jtab(6),1)*jtab(7)
    lgpg = lgpg1
!
!     ORIENTATION DU MASSIF
!     COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
!
    xyz(1) = 0.d0
    xyz(2) = 0.d0
    xyz(3) = 0.d0
    do 150 i = 1, nno
        do 140 idim = 1, ndim
            xyz(idim) = xyz(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
140      continue
150  end do
    call rcangm(ndim, xyz, angmas)
!
! - VARIABLES DE COMMANDE
!
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
!
! PARAMETRES EN SORTIE
!
    imatuu=1
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PMATUUR', 'E', imatuu)
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
!
!      ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
        call jevech('PVARIMP', 'L', ivarix)
        call dcopy(npg1*lgpg, zr(ivarix), 1, zr(ivarip), 1)
    else
        ivectu=1
        icontp=1
        ivarip=1
    endif
!
!
!
! - HYPER-ELASTICITE
!
    if (zk16(icompo+3) (1:9) .eq. 'COMP_ELAS') then
        if (zk16(icompo).ne.'ELAS') then
            call utmess('F', 'ALGORITH10_88')
        endif
    endif
!
!
! - HYPO-ELASTICITE
!
        if (zk16(icompo+2) (6:10) .eq. '_REAC') then
!CDIR$ IVDEP
            do 20 i = 1, 2*nno
                zr(igeom+i-1) = zr(igeom+i-1) + zr(ideplm+i-1) + zr(ideplp+i-1)
20          continue
        endif
!
        if (zk16(icompo+2) (1:5) .eq. 'PETIT') then
!
            call nmas2d(fami, nno, npg1, ipoids, ivf,&
                        idfde, zr(igeom), typmod, option, zi(imate),&
                        zk16(icompo), lgpg, zr(icarcr), zr(iinstm), zr(iinstp),&
                        zr(ideplm), zr(ideplp), angmas, zr(icontm), zr(ivarim),&
                        vect1, vect3, zr(icontp), zr(ivarip), zr(imatuu),&
                        zr(ivectu), codret)
!
        else
            call utmess('F', 'ELEMENTS3_16', sk=zk16(icompo+2))
        endif
!

    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
end subroutine
