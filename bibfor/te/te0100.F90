subroutine te0100(option, nomte)
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
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nmdlog.h"
#include "asterfort/nmed2d.h"
#include "asterfort/nmel2d.h"
#include "asterfort/nmgpfi.h"
#include "asterfort/nmgr2d.h"
#include "asterfort/nmgz2d.h"
#include "asterfort/nmpl2d.h"
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
!                          EN 2D (CPLAN ET DPLAN) ET AXI
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    character(len=8) :: typmod(2)
    character(len=4) :: fami
    integer :: nno, npg1, i, imatuu, lgpg, lgpg1
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: icontm, ivarim
    integer :: iinstm, iinstp, ideplm, ideplp, icompo, icarcr
    integer :: ivectu, icontp, ivarip, li
    integer :: ivarix, iret
    integer :: jtab(7), jcret, codret
    integer :: ndim, nnos, jgano, idim
    real(kind=8) :: vect1(54), vect2(4*27*27), vect3(4*27*2), dfdi(4*9)
    real(kind=8) :: angmas(7), bary(3)
    logical :: matsym
!     POUR TGVERI
    real(kind=8) :: sdepl(3*9), svect(3*9), scont(6*9), smatr(3*9*3*9), epsilo
    real(kind=8) :: varia(2*3*9*3*9)
!
    icontp=1
    ivarip=1
    imatuu=1
    ivectu=1
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
!
!
! - TYPE DE MODELISATION
!
    if (lteatt('AXIS','OUI')) then
        typmod(1) = 'AXIS    '
    else if (lteatt('C_PLAN','OUI')) then
        typmod(1) = 'C_PLAN  '
    else if (lteatt('D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN  '
    else
!       NOM D'ELEMENT ILLICITE
        ASSERT(lteatt('C_PLAN', 'OUI'))
    endif
!
    if (lteatt('TYPMOD2','ELEMDISC')) then
        typmod(2) = 'ELEMDISC'
    else
        typmod(2) = '        '
    endif
    codret = 0
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
!
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=jtab)
    lgpg1 = max(jtab(6),1)*jtab(7)
    lgpg = lgpg1
!
!     ORIENTATION DU MASSIF
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
! PARAMETRES EN SORTIE
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
!      ESTIMATION VARIABLES INTERNES A L'ITERATION PRECEDENTE
        call jevech('PVARIMP', 'L', ivarix)
        call dcopy(npg1*lgpg, zr(ivarix), 1, zr(ivarip), 1)
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
    endif
!
!
! - HYPER-ELASTICITE
!
    if (zk16(icompo+3) (1:9) .eq. 'COMP_ELAS') then
!
        if (option(1:10) .eq. 'RIGI_MECA_') then
!
!        OPTION RIGI_MECA_TANG :         ARGUMENTS EN T-
            call nmel2d(fami, '-', nno, npg1, ipoids,&
                        ivf, idfde, zr(igeom), typmod, option,&
                        zi(imate), zk16(icompo), lgpg, zr(icarcr), ideplm,&
                        angmas, vect1, vect2, vect3, zr(icontm),&
                        zr(ivarim), zr(imatuu), ivectu, codret)
!
        else
!
!        OPTION FULL_MECA OU RAPH_MECA : ARGUMENTS EN T+
            do 10 li = 1, 2*nno
                zr(ideplp+li-1) = zr(ideplm+li-1) + zr(ideplp+li-1)
10          continue
!
            call nmel2d(fami, '+', nno, npg1, ipoids,&
                        ivf, idfde, zr(igeom), typmod, option,&
                        zi(imate), zk16(icompo), lgpg, zr(icarcr), ideplp,&
                        angmas, vect1, vect2, vect3, zr(icontp),&
                        zr(ivarip), zr(imatuu), ivectu, codret)
        endif
!
    else
!
! - HYPO-ELASTICITE
!
!      Pour le calcul de la matrice tangente par perturbation
1000      continue
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
! -       ELEMENT A DISCONTINUITE INTERNE
            if (typmod(2) .eq. 'ELEMDISC') then
!
                call nmed2d(nno, npg1, ipoids, ivf, idfde,&
                            zr(igeom), typmod, option, zi(imate), zk16(icompo),&
                            lgpg, zr(icarcr), ideplm, ideplp, zr(icontm),&
                            zr(ivarim), vect1, vect3, zr( icontp), zr(ivarip),&
                            zr(imatuu), ivectu, codret)
!
            else
!
                call nmpl2d(fami, nno, npg1, ipoids, ivf,&
                            idfde, zr(igeom), typmod, option, zi(imate),&
                            zk16(icompo), lgpg, zr(icarcr), zr(iinstm), zr(iinstp),&
                            ideplm, ideplp, angmas, zr( icontm), zr(ivarim),&
                            matsym, vect1, vect3, zr(icontp), zr( ivarip),&
                            zr(imatuu), ivectu, codret)
!
            endif
!
!      GRANDES DEFORMATIONS : FORMULATION SIMO - MIEHE
!
        else if (zk16(icompo+2) (1:10).eq.'SIMO_MIEHE') then
            call nmgpfi(fami, option, typmod, ndim, nno,&
                        npg1, ipoids, zr( ivf), idfde, zr(igeom),&
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
            do 46 li = 1, 2*nno
                zr(ideplp+li-1) = zr(ideplm+li-1) + zr(ideplp+li-1)
46          continue
!
            call nmgz2d(fami, nno, npg1, ipoids, ivf,&
                        idfde, zr(igeom), typmod, option, zi(imate),&
                        zk16(icompo), lgpg, zr(icarcr), zr(iinstm), zr(iinstp),&
                        ideplm, ideplp, angmas, zr(icontm), zr(ivarim),&
                        vect1, vect2, vect3, zr(icontp), zr(ivarip),&
                        zr(imatuu), ivectu, codret)
!
! 7.3 - CO-ROTATIONNELLE ZMAT SUPPRIME  ATTENTE CORRECTION FICHE 14063
!
! 7.3 - GRANDES ROTATIONS ET PETITES DEFORMATIONS
        else if (zk16(icompo+2) .eq.'GROT_GDEP') then
!
            do 45 li = 1, 2*nno
                zr(ideplp+li-1) = zr(ideplm+li-1) + zr(ideplp+li-1)
45          continue
!
            call nmgr2d(fami, nno, npg1, ipoids, ivf,&
                        zr(ivf), idfde, zr(igeom), typmod, option,&
                        zi(imate), zk16(icompo), lgpg, zr( icarcr), zr(iinstm),&
                        zr(iinstp), zr(ideplm), zr(ideplp), angmas, zr(icontm),&
                        zr(ivarim), matsym, vect1, vect2, vect3,&
                        zr(icontp), zr(ivarip), zr(imatuu), zr(ivectu), codret)
!
        else if (zk16(icompo+2).eq.'GDEF_HYPO_ELAS') then
!
            call nmsh1(fami, option, typmod, zk16(icompo+2), ndim,&
                       nno, npg1, ipoids, ivf, zr(ivf),&
                       idfde, zr(igeom), dfdi, zk16(icompo), zi(imate),&
                       lgpg, zr(icarcr), angmas, zr(iinstm), zr(iinstp),&
                       zr(ideplm), zr(ideplp), zr(icontm), zr(ivarim), zr(icontp),&
                       zr(ivarip), zr(ivectu), zr(imatuu), codret)
        else if (zk16(icompo+2).eq.'GDEF_LOG') then
!
            call nmdlog(fami, option, typmod, ndim, nno,&
                        npg1, ipoids, ivf, zr(ivf), idfde,&
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
!
!       Calcul eventuel de la matrice TGTE par PERTURBATION
        call tgveri(option, zr(icarcr), zk16(icompo), nno, zr(igeom),&
                    ndim, ndim*nno, zr(ideplp), sdepl, zr(ivectu),&
                    svect, 4*npg1, zr(icontp), scont, npg1*lgpg,&
                    zr(ivarip), zr(ivarix), zr(imatuu), smatr, matsym,&
                    epsilo, varia, iret)
        if (iret .ne. 0) goto 1000
!
    endif
!
2000  continue
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
end subroutine
