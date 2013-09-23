subroutine te0560(option, nomte)
!
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
#include "asterfort/assert.h"
#include "asterfort/elrefv.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/massup.h"
#include "asterfort/nmgvno.h"
#include "asterfort/nmtstm.h"
#include "asterfort/rcangm.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          EN 2D (CPLAN ET DPLAN) ET AXI
!                          POUR LES ELEMNTS GRAD_VARI
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    logical :: matsym
!
    integer :: dlns
    integer :: nno, npg1, i, imatuu, lgpg, lgpg1, ndim
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: nnob, ivfb, idfdeb, jganob
    integer :: icontm, ivarim
    integer :: iinstm, iinstp, ideplm, ideplp, icompo, icarcr
    integer :: ivectu, icontp, ivarip, nnos, jgano
    integer :: ivarix, iret, idim
    integer :: jtab(7), jcret, codret
!
    real(kind=8) :: xyz(3)
    real(kind=8) :: angmas(7)
!
    integer :: icodr1
    character(len=8) :: typmod(2)
    character(len=16) :: phenom
!
!
    if (option(1:9) .eq. 'MASS_MECA') then
        call elrefv(nomte, 'MASS', ndim, nno, nnob,&
                    nnos, npg1, ipoids, ivf, ivfb,&
                    idfde, idfdeb, jgano, jganob)
    else
        call elrefv(nomte, 'RIGI', ndim, nno, nnob,&
                    nnos, npg1, ipoids, ivf, ivfb,&
                    idfde, idfdeb, jgano, jganob)
    endif
!
!     TYPE DE MODELISATION
    if (lteatt(' ','AXIS','OUI')) then
        typmod(1) = 'AXIS    '
    else if (lteatt(' ','C_PLAN','OUI')) then
        typmod(1) = 'C_PLAN  '
    else if (lteatt(' ','D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN  '
    else if (nomte(1:4).eq.'MNVG') then
        typmod(1) = '3D      '
    else
!     NOM D'ELEMENT ILLICITE
        ASSERT(nomte(1:4).eq.'MNVG')
    endif
!
    typmod(2) = 'GDVARINO'
    codret = 0
!
!     PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    if (option(1:9) .eq. 'MASS_MECA') then
! ---------------- CALCUL MATRICE DE MASSE ------------------------
!
        call jevech('PMATUUR', 'E', imatuu)
!
        if (ndim .eq. 2) then
!     2 DEPLACEMENT + VARI
            dlns = 3
        else if (ndim.eq.3) then
!     3 DEPLACEMENT + VARI
            dlns = 4
        else
            ASSERT(ndim .eq. 3)
        endif
!
        call massup(option, ndim, dlns, nno, nnob,&
                    zi(imate), phenom, npg1, ipoids, idfde,&
                    zr(igeom), zr(ivf), imatuu, icodr1, igeom,&
                    ivf)
!
! --------------- FIN CALCUL MATRICE DE MASSE -----------------------
    else
!
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
!
        do 150 i = 1, nno
            do 140 idim = 1, ndim
                xyz(idim) = xyz(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
140          continue
150      continue
        call rcangm(ndim, xyz, angmas)
!
!     VARIABLES DE COMMANDE
!
        call jevech('PINSTMR', 'L', iinstm)
        call jevech('PINSTPR', 'L', iinstp)
!
!     PARAMETRES EN SORTIE
!
        if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
            call nmtstm(zk16(icompo), imatuu, matsym)
        endif
!
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call jevech('PVECTUR', 'E', ivectu)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
!
!     ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
            call jevech('PVARIMP', 'L', ivarix)
            call dcopy(npg1*lgpg, zr(ivarix), 1, zr(ivarip), 1)
        else
            ivectu=1
            icontp=1
            ivarip=1
        endif
!
        if (option .eq. 'RIGI_MECA_ELAS' .or. option .eq. 'FULL_MECA_ELAS' .or. option .eq.&
            'RAPH_MECA') then
!
            call nmgvno('ELAS', ndim, nno, nnob, npg1,&
                        ipoids, zr(ivf), zr(ivfb), idfde, idfdeb,&
                        zr(igeom), typmod, option, zi(imate), zk16(icompo),&
                        lgpg, zr(icarcr), zr(iinstm), zr(iinstp), zr(ideplm),&
                        zr(ideplp), angmas, zr(icontm), zr(ivarim), zr( icontp),&
                        zr(ivarip), zr(imatuu), zr(ivectu), codret)
        else
            call nmgvno('RIGI', ndim, nno, nnob, npg1,&
                        ipoids, zr(ivf), zr(ivfb), idfde, idfdeb,&
                        zr(igeom), typmod, option, zi(imate), zk16(icompo),&
                        lgpg, zr(icarcr), zr(iinstm), zr(iinstp), zr(ideplm),&
                        zr(ideplp), angmas, zr(icontm), zr(ivarim), zr( icontp),&
                        zr(ivarip), zr(imatuu), zr(ivectu), codret)
        endif
!
    endif
!
    if (zk16(icompo+2) .ne. 'PETIT') then
        call utmess('F', 'ELEMENTS3_16', sk=zk16(icompo+2))
    endif
!
!
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
end subroutine
