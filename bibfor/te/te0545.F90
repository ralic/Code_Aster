subroutine te0545(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "jeveux.h"
#include "asterfort/elrefv.h"
#include "asterfort/jevech.h"
#include "asterfort/ngfint.h"
#include "asterfort/nmgvmb.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcangm.h"
#include "asterfort/teattr.h"
#include "asterfort/tecach.h"
#include "blas/dcopy.h"
#include "blas/dgemv.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          EN 2D (CPLAN ET DPLAN) ET AXI
!                          POUR LES ELEMNTS GRAD_VARI
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    integer :: nnomax, npgmax, epsmax, ddlmax
    parameter (nnomax=27,npgmax=27,epsmax=20,ddlmax=15*nnomax)
! ......................................................................
    character(len=8) :: typmod(2)
    logical :: resi, rigi, axi
    integer :: nno, nnob, npg, ndim, nddl, neps, lgpg
    integer :: ipoids, ivf, idfde, ivfb, idfdeb
    integer :: imate, icontm, ivarim, iinstm, iinstp, ideplm, ideplp, icompo
    integer :: ivectu, icontp, ivarip, imatuu, icarcr, ivarix, igeom, icoret
    integer :: iret, nnos, jgano, jganob, jtab(7)
    real(kind=8) :: xyz(3), unit(nnomax), angmas(7)
    real(kind=8) :: b(epsmax, npgmax, ddlmax), w(npgmax), ni2ldc(epsmax)
!
!
! - INITIALISATION
!
    resi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RAPH_MECA'
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
!
    call teattr('S', 'TYPMOD', typmod(1), iret)
    typmod(2) = 'GRADVARI'
    axi = typmod(1).eq.'AXIS'
!
    call elrefv(nomte, 'RIGI', ndim, nno, nnob,&
                nnos, npg, ipoids, ivf, ivfb,&
                idfde, idfdeb, jgano, jganob)
!
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
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
!
!
! - PARAMETRES EN SORTIE
!
    if (rigi) then
        call jevech('PMATUNS', 'E', imatuu)
    else
        imatuu=1
    endif
!
    if (resi) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PCODRET', 'E', icoret)
    else
        ivectu=1
        icontp=1
        ivarip=1
        icoret=1
    endif
!
!
!    NOMBRE DE VARIABLES INTERNES
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=jtab)
    lgpg = max(jtab(6),1)*jtab(7)
!
!
!    ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
    if (resi) then
        call jevech('PVARIMP', 'L', ivarix)
        call dcopy(npg*lgpg, zr(ivarix), 1, zr(ivarip), 1)
    endif
!
!
!    BARYCENTRE ET ORIENTATION DU MASSIF
    call r8inir(nno, 1.d0/nno, unit, 1)
    call dgemv('N', ndim, nno, 1.d0, zr(igeom),&
               ndim, unit, 1, 0.d0, xyz,&
               1)
    call rcangm(ndim, xyz, angmas)
!
!
! - CALCUL DES ELEMENTS CINEMATIQUES
!
    call nmgvmb(ndim, nno, nnob, npg, axi,&
                zr(igeom), zr(ivf), zr(ivfb), idfde, idfdeb,&
                ipoids, nddl, neps, b, w,&
                ni2ldc)
!
! - CALCUL DES FORCES INTERIEURES ET MATRICES TANGENTES
!
    call ngfint(option, typmod, ndim, nddl, neps,&
                npg, w, b, zk16(icompo), 'RIGI',&
                zi(imate), angmas, lgpg, zr(icarcr), zr(iinstm),&
                zr(iinstp), zr(ideplm), zr(ideplp), ni2ldc, zr(icontm),&
                zr(ivarim), zr(icontp), zr(ivarip), zr(ivectu), zr(imatuu),&
                zi(icoret))
!
end subroutine
