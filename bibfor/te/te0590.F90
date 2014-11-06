subroutine te0590(option, nomte)
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
! person_in_charge: sebastien.fayolle at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref2.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nbfilg.h"
#include "asterfort/nbfism.h"
#include "asterfort/nifilg.h"
#include "asterfort/nifipd.h"
#include "asterfort/nifism.h"
#include "asterfort/niinit.h"
#include "asterfort/nmtstm.h"
#include "asterfort/rcangm.h"
#include "asterfort/tecach.h"
#include "asterfort/tgverm.h"
#include "asterfort/utmess.h"
#include "blas/dcopy.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! FONCTION REALISEE:  CALCUL DES FORCES INTERNES POUR LES ELEMENTS
!                     INCOMPRESSIBLES A 3 CHAMPS UGP
!                     EN 3D/D_PLAN/AXI
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    aster_logical :: rigi, resi, matsym
    integer :: ndim, nno1, nno2, nno3, npg, nnos, jgn, ntrou
    integer :: icoret, codret, iret
    integer :: iw, ivf1, ivf2, ivf3, idf1, idf2, idf3
    integer :: jtab(7), lgpg, i, idim
    integer :: vu(3, 27), vg(27), vp(27), vpi(3, 27)
    integer :: igeom, imate, icontm, ivarim
    integer :: iinstm, iinstp, iddlm, iddld, icompo, icarcr, ivarix
    integer :: ivectu, icontp, ivarip, imatuu
    integer :: idbg, nddl, ia, ja
    real(kind=8) :: angmas(7), bary(3)
    character(len=8) :: lielrf(10), typmod(2)
!
!     POUR TGVERI
    real(kind=8) :: sdepl(135), svect(135), scont(6*27), smatr(18225)
    real(kind=8) :: epsilo, epsilp, epsilg
    real(kind=8) :: varia(2*135*135)
    real(kind=8) :: tab_out(27*3*27*3)
    integer      :: na, os, nb, ib, kk
! ----------------------------------------------------------------------
!
    idbg=0
!
! - FONCTIONS DE FORME
    call elref2(nomte, 10, lielrf, ntrou)
    ASSERT(ntrou.ge.3)
    call elrefe_info(elrefe=lielrf(3), fami='RIGI', ndim=ndim, nno=nno3, nnos=nnos,&
                     npg=npg, jpoids=iw, jvf=ivf3, jdfde=idf3, jgano=jgn)
    call elrefe_info(elrefe=lielrf(2), fami='RIGI', ndim=ndim, nno=nno2, nnos=nnos,&
                     npg=npg, jpoids=iw, jvf=ivf2, jdfde=idf2, jgano=jgn)
    call elrefe_info(elrefe=lielrf(1), fami='RIGI', ndim=ndim, nno=nno1, nnos=nnos,&
                     npg=npg, jpoids=iw, jvf=ivf1, jdfde=idf1, jgano=jgn)
    nddl = nno1*ndim + nno2 + nno3
    matsym = .true.
!
! - TYPE DE MODELISATION
    if (ndim .eq. 2 .and. lteatt('AXIS','OUI')) then
        typmod(1) = 'AXIS  '
    else if (ndim.eq.2 .and. lteatt('D_PLAN','OUI')) then
        typmod(1) = 'D_PLAN  '
    else if (ndim .eq. 3) then
        typmod(1) = '3D'
    else
        call utmess('F', 'ELEMENTS_34', sk=nomte)
    endif
    typmod(2) = '        '
    codret = 0
!
! - ACCES AUX COMPOSANTES DU VECTEUR DDL
    call niinit(nomte, typmod, ndim, nno1, nno2, nno3, 0, vu, vg, vp, vpi)
!
! - OPTION
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
!
! - PARAMETRES EN ENTREE
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PDEPLMR', 'L', iddlm)
    call jevech('PDEPLPR', 'L', iddld)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PCARCRI', 'L', icarcr)
!
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
!
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7, itab=jtab)
    lgpg = max(jtab(6),1)*jtab(7)
!
! - ORIENTATION DU MASSIF
! - COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
    bary(1) = 0.d0
    bary(2) = 0.d0
    bary(3) = 0.d0
    do i = 1, nno1
        do idim = 1, ndim
            bary(idim) = bary(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno1
        end do
    end do
    call rcangm(ndim, bary, angmas)
!
! - PARAMETRES EN SORTIE
    if (resi) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PVARIMP', 'L', ivarix)
        call dcopy(npg*lgpg, zr(ivarix), 1, zr(ivarip), 1)
    else
        ivectu=1
        icontp=1
        ivarip=1
        ivarix=1
    endif
!
100 continue
! - PETITES DEFORMATIONS
    if (zk16(icompo+2) (1:6) .eq. 'PETIT ') then
        if (lteatt('INCO','C3B')) then
            call utmess('F', 'MODELISA10_17', sk=zk16(icompo+2))
        endif
!
! - PARAMETRES EN SORTIE
        if (rigi) then
            call jevech('PMATUUR', 'E', imatuu)
        else
            imatuu=1
        endif
        call nifipd(ndim, nno1, nno2, nno3, npg,&
                    iw, zr(ivf1), zr(ivf2), zr(ivf3), idf1,&
                    vu, vg, vp, zr(igeom), typmod,&
                    option, zi(imate), zk16(icompo), lgpg, zr(icarcr),&
                    zr(iinstm), zr(iinstp), zr(iddlm), zr(iddld), angmas,&
                    zr(icontm), zr(ivarim), zr(icontp), zr(ivarip), resi,&
                    rigi, zr(ivectu), zr(imatuu), codret)
    else if (zk16(icompo+2) (1:8).eq.'GDEF_LOG') then
!
! - PARAMETRES EN SORTIE
        if (rigi) then
            call nmtstm(zk16(icompo), imatuu, matsym)
        else
            imatuu=1
        endif
!
        if (lteatt('INCO','C3B')) then
            call nbfilg(ndim, nno1, nno2, nno3, npg,&
                        iw, zr(ivf1), zr(ivf2), zr(ivf3), idf1,&
                        vu, vg, vp, zr(igeom), typmod,&
                        option, zi(imate), zk16(icompo), lgpg, zr(icarcr),&
                        zr(iinstm), zr(iinstp), zr(iddlm), zr(iddld), angmas,&
                        zr(icontm), zr(ivarim), zr(icontp), zr(ivarip), resi,&
                        rigi, zr(ivectu), zr(imatuu), matsym, codret)
        else
            call nifilg(ndim, nno1, nno2, nno3, npg,&
                        iw, zr(ivf1), zr(ivf2), zr(ivf3), idf1,&
                        vu, vg, vp, zr(igeom), typmod,&
                        option, zi(imate), zk16(icompo), lgpg, zr(icarcr),&
                        zr(iinstm), zr(iinstp), zr(iddlm), zr(iddld), angmas,&
                        zr(icontm), zr(ivarim), zr(icontp), zr(ivarip), resi,&
                        rigi, zr(ivectu), zr(imatuu), matsym, codret)
        endif
    else if (zk16(icompo+2) (1:10).eq.'SIMO_MIEHE') then
!
! - PARAMETRES EN SORTIE
        typmod(2) = 'INCO'
        if (rigi) then
            call nmtstm(zk16(icompo), imatuu, matsym)
        else
            imatuu=1
        endif
!
        if (lteatt('INCO','C3B')) then
            call nbfism(ndim, nno1, nno2, nno3, npg,&
                        iw, zr(ivf1), zr(ivf2), zr(ivf3), idf1,&
                        idf2, vu, vg, vp, zr(igeom),&
                        typmod, option, zi(imate), zk16(icompo), lgpg,&
                        zr(icarcr), zr(iinstm), zr(iinstp), zr(iddlm), zr(iddld),&
                        angmas, zr(icontm), zr(ivarim), zr(icontp), zr(ivarip),&
                        resi, rigi, zr(ivectu), zr(imatuu), codret)
        else
            call nifism(ndim, nno1, nno2, nno3, npg,&
                        iw, zr(ivf1), zr(ivf2), zr(ivf3), idf1,&
                        idf2, vu, vg, vp, zr(igeom),&
                        typmod, option, zi(imate), zk16(icompo), lgpg,&
                        zr(icarcr), zr(iinstm), zr(iinstp), zr(iddlm), zr(iddld),&
                        angmas, zr(icontm), zr(ivarim), zr(icontp), zr(ivarip),&
                        resi, rigi, zr(ivectu), zr(imatuu), codret)
        endif
    else
        call utmess('F', 'ELEMENTS3_16', sk=zk16(icompo+2))
    endif
!
    if (codret .ne. 0) goto 200
!       Calcul eventuel de la matrice TGTE par PERTURBATION
    call tgverm(option, zr(icarcr), zk16(icompo), nno1, nno2,&
                nno3, zr(igeom), ndim, nddl, zr(iddld),&
                sdepl, vu, vg, vp, zr(ivectu),&
                svect, ndim*2*npg, zr(icontp), scont, npg*lgpg,&
                zr(ivarip), zr(ivarix), zr(imatuu), smatr, matsym,&
                epsilo, epsilp, epsilg, varia, iret)
    if (iret .ne. 0) goto 100
!
200 continue
!
    if (resi) then
        call jevech('PCODRET', 'E', icoret)
        zi(icoret) = codret
    endif
!
    if (idbg .eq. 1) then
        if (rigi) then
            write(6,*) 'MATRICE TANGENTE'
            if (matsym) then
                do ia = 1, nddl
                    write(6,'(108(1X,E11.4))') (zr(imatuu+(ia*(ia-1)/2)+ja-1),ja=1,ia)
                enddo
            else
!
! - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
!
                write(6,*) 'KUU'
                ja = 1
                do na = 1, nno1
                    do ia = 1, ndim
                        os = (vu(ia,na)-1)*nddl
                        do nb = 1, nno1
                            do ib = 1, ndim
                                kk = os+vu(ib,nb)
                                tab_out(ja)=zr(imatuu+kk-1)
                                ja=ja+1
                            enddo
                        enddo
                    enddo
                enddo
                do ia = 1, nno1*ndim
                    write(6,'(108(1X,E11.4))') (tab_out((ia-1)*nno1*ndim+ja),ja=1,nno1*ndim)
                enddo
!
! - TERME K:GG      KGG(NNO2,NNO2)
!
                write(6,*) 'KGG'
                ja = 1
                do na = 1, nno2
                    os = (vg(na)-1)*nddl
                    do ia = 1, nno2
                        kk = os + vg(ia)
                        tab_out(ja)=zr(imatuu+kk-1)
                        ja=ja+1
                    end do
                end do
                do ia = 1, nno2
                    write(6,'(108(1X,E11.4))') (tab_out((ia-1)*nno2+ja),ja=1,nno2)
                enddo
!
! - TERME K:UP      KUP(NDIM,NNO1,NNO3)
!
                write(6,*) 'KUP'
                ja = 1
                do na = 1, nno1
                    do ia = 1, ndim
                        os = (vu(ia,na)-1)*nddl
                        do nb = 1, nno3
                            kk = os + vp(nb)
                            tab_out(ja)=zr(imatuu+kk-1)
                            ja=ja+1
                        end do
                    enddo
                enddo
                do ia = 1, nno1*ndim
                    write(6,'(108(1X,E11.4))') (tab_out((ia-1)*nno3+ja),ja=1,nno3)
                enddo
!
! - TERME K:PU      KPU(NDIM,NNO3,NNO1)
!
                write(6,*) 'KPU'
                ja = 1
                do ia = 1, nno3
                    os = (vp(ia)-1)*nddl
                    do nb = 1, nno1
                        do ib = 1, ndim
                            kk = os + vu(ib,nb)
                            tab_out(ja)=zr(imatuu+kk-1)
                            ja=ja+1
                        end do
                    end do
                end do
                do ia = 1, nno3
                    write(6,'(108(1X,E11.4))') (tab_out((ia-1)*nno1*ndim+ja),ja=1,nno1*ndim)
                enddo
!
! - TERME K:UG      KUG(NDIM,NNO1,NNO2)
!
                write(6,*) 'KUG'
                ja = 1
                do na = 1, nno1
                    do ia = 1, ndim
                        os = (vu(ia,na)-1)*nddl
                        do nb = 1, nno2
                            kk = os + vg(nb)
                            tab_out(ja)=zr(imatuu+kk-1)
                            ja=ja+1
                        end do
                    enddo
                enddo
                do ia = 1, nno1*ndim
                    write(6,'(108(1X,E11.4))') (tab_out((ia-1)*nno2+ja),ja=1,nno2)
                enddo
!
! - TERME K:GU      KGU(NDIM,NNO2,NNO1)
!
                write(6,*) 'KGU'
                ja = 1
                do ia = 1, nno2
                    os = (vg(ia)-1)*nddl
                    do nb = 1, nno1
                        do ib = 1, ndim
                            kk = os + vu(ib,nb)
                            tab_out(ja)=zr(imatuu+kk-1)
                            ja=ja+1
                        end do
                    enddo
                enddo
                do ia = 1, nno2
                    write(6,'(108(1X,E11.4))') (tab_out((ia-1)*nno1*ndim+ja),ja=1,nno1*ndim)
                enddo
!
! - TERME K:PG      KPG(NNO3,NNO2)
!
                write(6,*) 'KPG'
                ja = 1
                do ia = 1, nno3
                    os = (vp(ia)-1)*nddl
                    do ib = 1, nno2
                        kk = os + vg(ib)
                        tab_out(ja)=zr(imatuu+kk-1)
                        ja=ja+1
                    enddo
                enddo
                do ia = 1, nno3
                    write(6,'(108(1X,E11.4))') (tab_out((ia-1)*nno2+ja),ja=1,nno2)
                enddo
!
! - TERME K:GP      KPG(NNO2,NNO3)
!
                write(6,*) 'KGP'
                ja = 1
                do ia = 1, nno2
                    os = (vg(ia)-1)*nddl
                    do ib = 1, nno3
                        kk = os + vp(ib)
                        tab_out(ja)=zr(imatuu+kk-1)
                        ja=ja+1
                    enddo
                enddo
                do ia = 1, nno2
                    write(6,'(108(1X,E11.4))') (tab_out((ia-1)*nno3+ja),ja=1,nno3)
                enddo
            endif
        endif
        if (resi) then
            write(6,*) 'FORCE INTERNE'
            write(6,'(108(1X,E11.4))') (zr(ivectu+ja-1),ja=1,nddl)
        endif
    endif
!
end subroutine
