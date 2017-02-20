subroutine te0477(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
! 'RIGI_MECA_TANG', 'FULL_MECA' & 'RAPH_MECA' options for SHB elements
!
!
! 'RIGI_MECA_TANG', 'FULL_MECA' & 'RAPH_MECA' options for solid-shell elements
! SHB6, SHB8, SHB15 & SHB20.
! Computation of 3D elementary matrix.
!
!
! IN  option   'RIGI_MECA_TANG', 'FULL_MECA' or 'RAPH_MECA'
! IN  nomte    elment type name
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/jevech.h"
#include "asterfort/r8inir.h"
#include "blas/dcopy.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/nmssgr.h"
#include "asterfort/nmsspl.h"
#include "asterfort/nmtstm.h"
#include "asterfort/sshini.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option
    character(len=16) :: nomte
!
    integer :: codret, i, icarcr, icompo, icontm, icontp, icoopg
    integer :: ideplm, ideplp, idfde, igeom, iinstm, iinstp, imate
    integer ::  imatuu, ipoids, iret, ivarim, ivarix
    integer :: ivarip, ivectu, ivf, jcret, jgano, jtab(7), lgpg
    integer :: ndim, nno, nnos, npg
    character(len=4) :: fami
    character(len=8) :: typmod(2)
    aster_logical :: matsym, shb6, shb8, hexa
    real(kind=8) :: angmas(3)
!
! ......................................................................
!
    parameter(fami='RIGI')
    parameter(ndim=3)
!
    icontp=1
    ivarip=1
    imatuu=1
    ivectu=1
    iinstm=1
    iinstp=1
!
! - Finite element informations
!
    call elrefe_info(fami=fami, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfde,&
                     jgano=jgano)
!
!   Initialization of specific SHB variables
!
    call sshini(nno, nnos, hexa, shb6, shb8)
!
!   Model
!
    typmod(1) = 'C_PLAN  '
    typmod(2) = '        '
!
!   Input parameters
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PCARCRI', 'L', icarcr)

    call tecach('OOO', 'PVARIMR', 'L', iret, nval=7, itab=jtab)
!   'lgpg' is the number of internal variables per Gauss points
    lgpg = max(jtab(6),1)
!
    call r8inir(3, 0.d0, angmas, 1)
!
!   Command variables
!
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
!
!   Output parameters
!
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
            call nmtstm(zr(icarcr), imatuu, matsym)
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
       call jevech('PVECTUR', 'E', ivectu)
       call jevech('PCONTPR', 'E', icontp)
       call jevech('PVARIPR', 'E', ivarip)
!
!      Internal variable estimation from previous iteration
       call jevech('PVARIMP', 'L', ivarix)
       call dcopy(npg*lgpg, zr(ivarix), 1, zr(ivarip), 1)
    else
       ivarix=1
    endif
!
!
    if (zk16(icompo+2) (1:5) .eq. 'PETIT') then
!
!      'PETIT' deformation model (small transformations / small strains)
       call nmsspl(        hexa,       shb6,       shb8,     icoopg,&
                           fami,        nno,        npg,     ipoids,        ivf,&
                          idfde,  zr(igeom),     typmod,     option,  zi(imate),&
                   zk16(icompo),       lgpg, zr(icarcr), zr(iinstm), zr(iinstp),&
                     zr(ideplm), zr(ideplp),     angmas, zr(icontm), zr(ivarim),&
                     zr(icontp), zr(ivarip), zr(imatuu), zr(ivectu),&
                         codret)
!
    else if (zk16(icompo+2) .eq.'GROT_GDEP') then
!
!      'GROT_GDEP' deformation model (large transformations / small strains)
!
!      Geometrical update :
!           Geometry at last converged increment
!           + displacement at beginning of new increment
       do 40 i = 1, 3*nno
          zr(igeom+i-1) = zr(igeom+i-1) + zr(ideplm+i-1)
40     continue
!
       call nmssgr(        hexa,       shb6,       shb8,     icoopg,&
                           fami,        nno,        npg,     ipoids,        ivf,&
                          idfde,  zr(igeom),     typmod,     option,  zi(imate),&
                   zk16(icompo),       lgpg, zr(icarcr), zr(iinstm), zr(iinstp),&
                     zr(ideplm), zr(ideplp),     angmas, zr(icontm), zr(ivarim),&
                         matsym, zr(icontp), zr(ivarip), zr(imatuu), zr(ivectu),&
                         codret)
    else
!
!      Warning to indicate that only 'PETIT' & 'GROT_GDEP' deformation models
!      are implemented for SHB elements
       call utmess('F', 'COMPOR1_69', sk=zk16(icompo+2))
!
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
       call jevech('PCODRET', 'E', jcret)
       zi(jcret) = codret
    endif
!
end subroutine
