subroutine te0139(option, nomte)
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
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nmdlog.h"
#include "asterfort/nmel3d.h"
#include "asterfort/nmgpfi.h"
#include "asterfort/nmgr3d.h"
#include "asterfort/nmgz3d.h"
#include "asterfort/nmpl3d.h"
#include "asterfort/nmsh1.h"
#include "asterfort/nmtstm.h"
#include "asterfort/rcangm.h"
#include "asterfort/tecach.h"
#include "asterfort/tgveri.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          ELEMENTS 3D
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    character(len=8) :: typmod(2)
    character(len=4) :: fami
    integer :: jgano, nno, npg, i, imatuu, lgpg, ndim, lgpg1, iret
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: icontm, ivarim
    integer :: iinstm, iinstp, ideplm, ideplp, icompo, icarcr
    integer :: ivectu, icontp, ivarip, li, jcret, codret
    integer :: ivarix
    logical(kind=1) :: matsym
    integer :: jtab(7), nnos, idim
    real(kind=8) :: bary(3)
    real(kind=8) :: pff(6*27*27), def(6*27*3), dfdi(3*27)
    real(kind=8) :: angmas(7)
!     POUR TGVERI
    real(kind=8) :: sdepl(3*27), svect(3*27), scont(6*27), smatr(3*27*3*27)
    real(kind=8) :: epsilo
    real(kind=8) :: varia(2*3*27*3*27)
!
    icontp=1
    ivarip=1
    imatuu=1
    ivectu=1
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
!     MATNS MAL DIMENSIONNEE
    ASSERT(nno.le.27)
!
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
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=jtab)
    lgpg1 = max(jtab(6),1)*jtab(7)
    lgpg = lgpg1
!
! --- ORIENTATION DU MASSIF
!     COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
!
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
! - VARIABLES DE COMMANDE
!
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
!
! - PARAMETRES EN SORTIE
!
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
        call nmtstm(zk16(icompo), imatuu, matsym)
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
    else
        ivarix=1
    endif
!
! - PARAMETRES EN SORTIE SUPPLEMENTAIE POUR LA METHODE IMPLEX
    if (option(1:16) .eq. 'RIGI_MECA_IMPLEX') then
        if (zk16(icompo) .ne. 'VMIS_ISOT_LINE' .and. zk16(icompo) .ne. 'ELAS' .and.&
            zk16(icompo) .ne. 'ENDO_FRAGILE' .and. zk16( icompo) .ne. 'ENDO_ISOT_BETON') then
            call utmess('F', 'ELEMENTS5_50')
        endif
        call jevech('PCONTXR', 'E', icontp)
!       INITIALISATION DE LA CONTRAINTE EXTRAPOLEE CONTXR=CONTMR
        call dcopy(npg*6, zr(icontm), 1, zr(icontp), 1)        
    endif
!
    if (zk16(icompo+3) (1:9) .eq. 'COMP_ELAS') then
!
! - LOIS DE COMPORTEMENT ECRITES EN CONFIGURATION DE REFERENCE
!                          COMP_ELAS
!
        if (option(1:10) .eq. 'RIGI_MECA_') then
!
!        OPTION RIGI_MECA_TANG :         ARGUMENTS EN T-
            call nmel3d(fami, '-', nno, npg, ipoids,&
                        ivf, idfde, zr(igeom), typmod, option,&
                        zi(imate), zk16(icompo), lgpg, zr(icarcr), zr(ideplm),&
                        angmas, dfdi, pff, def, zr(icontm),&
                        zr(ivarim), zr( imatuu), zr(ivectu), codret)
!
        else
!
!        OPTION FULL_MECA OU RAPH_MECA : ARGUMENTS EN T+
!
            do 10 li = 1, 3*nno
                zr(ideplp+li-1) = zr(ideplm+li-1) + zr(ideplp+li-1)
10          continue
!
            call nmel3d(fami, '+', nno, npg, ipoids,&
                        ivf, idfde, zr(igeom), typmod, option,&
                        zi(imate), zk16(icompo), lgpg, zr(icarcr), zr(ideplp),&
                        angmas, dfdi, pff, def, zr(icontp),&
                        zr(ivarip), zr( imatuu), zr(ivectu), codret)
        endif
!
    else
!
! - LOIS DE COMPORTEMENT ECRITE EN CONFIGURATION ACTUELLE
!                          COMP_INCR
!
!       Pour le calcul de la matrice tangente par perrturbation
1000      continue
!
!      PETITES DEFORMATIONS (AVEC EVENTUELLEMENT REACTUALISATION)
        if (zk16(icompo+2) (1:5) .eq. 'PETIT') then
            if (zk16(icompo+2) (6:10) .eq. '_REAC') then
                do 20 i = 1, 3*nno
                    zr(igeom+i-1) = zr(igeom+i-1) + zr(ideplm+i-1) + zr(ideplp+i-1)
20              continue
            endif
            call nmpl3d(fami, nno, npg, ipoids, ivf,&
                        idfde, zr(igeom), typmod, option, zi(imate),&
                        zk16(icompo), lgpg, zr(icarcr), zr(iinstm), zr(iinstp),&
                        zr(ideplm), zr(ideplp), angmas, zr(icontm), zr(ivarim),&
                        matsym, dfdi, def, zr(icontp), zr( ivarip),&
                        zr(imatuu), zr(ivectu), codret)
!
!
!      GRANDES DEFORMATIONS : FORMULATION SIMO - MIEHE
!
        else if (zk16(icompo+2) (1:10).eq.'SIMO_MIEHE') then
            call nmgpfi(fami, option, typmod, ndim, nno,&
                        npg, ipoids, zr( ivf), idfde, zr(igeom),&
                        dfdi, zk16(icompo), zi(imate), lgpg, zr( icarcr),&
                        angmas, zr(iinstm), zr(iinstp), zr(ideplm), zr( ideplp),&
                        zr(icontm), zr(ivarim), zr(icontp), zr(ivarip), zr( ivectu),&
                        zr(imatuu), codret)
!
! 7.3 - CO-ROTATIONNELLE ZMAT
!
            else if (((zk16(icompo).eq.'ZMAT').and. zk16(icompo+2)&
        .eq.'GDEF_HYPO_ELAS') ) then
!
            do 51 li = 1, 3*nno
                zr(ideplp+li-1) = zr(ideplm+li-1) + zr(ideplp+li-1)
51          continue
!
            call nmgz3d(fami, nno, npg, ipoids, ivf,&
                        idfde, zr(igeom), typmod, option, zi(imate),&
                        zk16(icompo), lgpg, zr(icarcr), zr(iinstm), zr(iinstp),&
                        zr(ideplm), zr(ideplp), angmas, zr(icontm), zr(ivarim),&
                        dfdi, pff, def, zr(icontp), zr(ivarip),&
                        zr(imatuu), zr(ivectu), codret)
!
! 7.3 - GRANDES ROTATIONS ET PETITES DEFORMATIONS
        else if (zk16(icompo+2) .eq.'GROT_GDEP') then
!
            do 50 li = 1, 3*nno
                zr(ideplp+li-1) = zr(ideplm+li-1) + zr(ideplp+li-1)
50          continue
!
            call nmgr3d(nno, npg, ipoids, ivf, idfde,&
                        zr(igeom), typmod, option, zi(imate), zk16(icompo),&
                        lgpg, zr(icarcr), zr(iinstm), zr(iinstp), zr(ideplm),&
                        zr(ideplp), angmas, zr(icontm), zr( ivarim), matsym,&
                        dfdi, pff, def, zr(icontp), zr(ivarip),&
                        zr(imatuu), zr(ivectu), codret)
!
! 7.3 - GRANDES DEFORMATIONS FORMULATION GDEF_HYPO_ELAS
!       ou SIMO_HUGHES1 (A DEBUGGER)
!
        else if (zk16(icompo+2).eq.'GDEF_HYPO_ELAS') then
!
            call nmsh1(fami, option, typmod, zk16(icompo+2), ndim,&
                       nno, npg, ipoids, ivf, zr(ivf),&
                       idfde, zr(igeom), dfdi, zk16(icompo), zi(imate),&
                       lgpg, zr(icarcr), angmas, zr(iinstm), zr(iinstp),&
                       zr(ideplm), zr(ideplp), zr(icontm), zr(ivarim), zr(icontp),&
                       zr(ivarip), zr(ivectu), zr(imatuu), codret)
!
! 7.3 - GRANDES DEFORMATIONS FORMULATION MIEHE-APEL-LAMBRECHT
!
        else if (zk16(icompo+2).eq.'GDEF_LOG') then
!
            call nmdlog(fami, option, typmod, ndim, nno,&
                        npg, ipoids, ivf, zr(ivf), idfde,&
                        zr(igeom), dfdi, zk16(icompo), zi(imate), lgpg,&
                        zr(icarcr), angmas, zr(iinstm), zr(iinstp), matsym,&
                        zr( ideplm), zr(ideplp), zr(icontm), zr(ivarim), zr(icontp),&
                        zr( ivarip), zr(ivectu), zr(imatuu), codret)
!
        else
            call utmess('F', 'ELEMENTS3_16', sk=zk16(icompo+2))
        endif
!
        if (codret .ne. 0) goto 2000
!       Calcul eventuel de la matrice TGTE par PERTURBATION
        call tgveri(option, zr(icarcr), zk16(icompo), nno, zr(igeom),&
                    ndim, ndim*nno, zr(ideplp), sdepl, zr(ivectu),&
                    svect, 6*npg, zr(icontp), scont, npg*lgpg,&
                    zr(ivarip), zr(ivarix), zr(imatuu), smatr, matsym,&
                    epsilo, varia, iret)
        if (iret .ne. 0) goto 1000
!
    endif
!
2000  continue
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
end subroutine
