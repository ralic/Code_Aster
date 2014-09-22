subroutine te0031(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/cosiro.h"
#include "asterfort/dkqmas.h"
#include "asterfort/dkqrig.h"
#include "asterfort/dktmas.h"
#include "asterfort/dktnli.h"
#include "asterfort/dktrig.h"
#include "asterfort/dsqmas.h"
#include "asterfort/dsqrig.h"
#include "asterfort/dstmas.h"
#include "asterfort/dstrig.h"
#include "asterfort/dxbsig.h"
#include "asterfort/dxeffi.h"
#include "asterfort/dxiner.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxroep.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/q4gmas.h"
#include "asterfort/q4grig.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/t3grig.h"
#include "asterfort/tecach.h"
#include "asterfort/terefe.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vecma.h"
!
#include "asterc/r8prem.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!     CALCUL DES OPTIONS DES ELEMENTS DE PLAQUE
!          -----------------------------------------------------------
!                                              TRIANGLE  QUADRANGLE
!        LINEAIRE          KIRCHOFF  (MINCE)        DKT       DST
!                 AVEC CISAILLEMENT  (EPAISSE)      DST       DSQ
!                                                   Q4G       T3G
!
!        RIGI_MECA       MASS_MECA
!        EPOT_ELEM  ECIN_ELEM
!        MASS_INER
!          -----------------------------------------------------------
!                                              TRIANGLE
!        LINEAIRE          KIRCHOFF  (MINCE)        DKT
!
!        FORC_NODA
!        REFE_FORC_NODA
!          -----------------------------------------------------------
!                                              TRIANGLE
!        NON LINEAIRE      KIRCHOFF  (MINCE)        DKT
!
!        FULL_MECA       RAPH_MECA     RIGI_MECA_TANG
!
!
!
    integer :: npge
    parameter(npge=3)
!
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano, ind
    integer :: multic, codret(1), jdepm, jdepr
    integer :: icompo, i1, i2, j, jvect, kpg, spt
    integer :: k, jcret, jfreq, iacce
    integer :: jmate, jgeom, jmatr, jener, i, jcara
    integer :: ivect, nddl, nvec, iret, icontp
    integer :: icou, nbcou, jnbspi, iret1, vali(2), itab(7), nbsp
    integer :: ibid, n1, n2, ni
!
    real(kind=8) :: pgl(3, 3), xyzl(3, 4), bsigma(24), effgt(32)
    real(kind=8) :: effref, momref
    real(kind=8) :: vecloc(24), ener(3), matp(24, 24), matv(300)
    real(kind=8) :: epi(1), eptot, r8bid=0.d0, valr(2)
    real(kind=8) :: foref, moref
!
    character(len=2) :: val
    character(len=3) :: num
    character(len=16) :: nomres
    character(len=8) :: fami, poum
    character(len=32) :: phenom
!
    aster_logical :: lcqhom
!
!     ---> POUR DKT/DST MATELEM = 3 * 6 DDL = 171 TERMES STOCKAGE SYME
!     ---> POUR DKQ/DSQ MATELEM = 4 * 6 DDL = 300 TERMES STOCKAGE SYME
    real(kind=8) :: matloc(300), rho, epais
!
!     --->   UML : DEPLACEMENT A L'INSTANT T- (REPERE LOCAL)
!     --->   DUL : INCREMENT DE DEPLACEMENT   (REPERE LOCAL)
    real(kind=8) :: uml(6, 4), dul(6, 4)
!
! DEB ------------------------------------------------------------------
!
    r8bid=0.d0
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfdx, jgano=jgano)
!
    call r8inir(32, 0.d0, effgt, 1)
    if (option .ne. 'REFE_FORC_NODA') then
! --- PASSAGE DES CONTRAINTES DANS LE REPERE INTRINSEQUE :
        call cosiro(nomte, 'PCONTMR', 'L', 'UI', 'G',&
                    ibid, 'S')
        call cosiro(nomte, 'PCONTRR', 'L', 'UI', 'G',&
                    ibid, 'S')
!
        jnbspi=0
        call tecach('NNN', 'PNBSP_I', 'L', iret1, iad=jnbspi)
    endif
!
!
    lcqhom=.false.
    if ((option.eq.'FULL_MECA') .or. (option.eq.'RAPH_MECA') .or.&
        (option(1:9).eq.'RIGI_MECA')) then
        call jevech('PMATERC', 'L', jmate)
! ---   COQUE HOMOGENEISEE ?
        if ((option.eq.'FULL_MECA') .or. (option.eq.'RAPH_MECA') .or.&
            (option.eq.'RIGI_MECA_TANG')) then
            call rccoma(zi(jmate), 'ELAS', 1, phenom, codret(1))
!
            if ((phenom.eq.'ELAS_COQUE') .or. (phenom.eq.'ELAS_COQMU') .or.&
                (phenom.eq.'ELAS_ORTH')) then
                lcqhom=.true.
            endif
        endif
!
! ---   VERIFICATION DE LA COHERENCE DES INFORMATIONS
! ---   PROVENANT DE DEFI_COQU_MULT ET DE AFFE_CARA_ELEM
!       ----------------------------------
        fami='FPG1'
        kpg=1
        spt=1
        poum='+'
        if (iret1 .eq. 0) then
            nbcou=zi(jnbspi)
            icou=0
            eptot=0.d0
            epi(1)=0.d0
            call jevech('PCACOQU', 'L', jcara)
            epais=zr(jcara)
 10         continue
            icou=icou+1
            call codent(icou, 'G', num)
            call codent(1, 'G', val)
            nomres='C'//num//'_V'//val
            r8bid=0.d0
            call rcvalb(fami, kpg, spt, poum, zi(jmate),&
                        ' ', 'ELAS_COQMU', 0, ' ', [r8bid],&
                        1, nomres, epi, codret, 0)
            if (codret(1) .eq. 0) then
                eptot=eptot+epi(1)
                goto 10
            endif
            if (eptot .ne. 0.d0) then
                if ((icou-1) .ne. nbcou) then
                    vali(1)=icou-1
                    vali(2)=nbcou
                    call utmess('F', 'ELEMENTS3_51', ni=2, vali=vali)
                endif
                if (abs(epais-eptot)/epais .gt. 1.d-2) then
                    valr(1)=eptot
                    valr(2)=epais
                    call utmess('F', 'ELEMENTS3_52', nr=2, valr=valr)
                endif
            endif
        endif
    endif
!
    call jevech('PGEOMER', 'L', jgeom)
    if (nno .eq. 3) then
        call dxtpgl(zr(jgeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(jgeom), pgl, 'S', iret)
    endif
    call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
    if (option .eq. 'RIGI_MECA' .or. option .eq. 'EPOT_ELEM') then
!     --------------------------------------
!
        if (nomte .eq. 'MEDKTR3') then
            call dktrig(nomte, xyzl, option, pgl, matloc,&
                        ener, multic)
        else if (nomte.eq.'MEDSTR3') then
            call dstrig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        else if (nomte.eq.'MEDKQU4') then
            call dkqrig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        else if (nomte.eq.'MEDSQU4') then
            call dsqrig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        else if (nomte.eq.'MEQ4QU4') then
            call q4grig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        else if (nomte.eq.'MET3TR3') then
            call t3grig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        endif
!
        if (option .eq. 'RIGI_MECA') then
            call jevech('PMATUUR', 'E', jmatr)
            call utpslg(nno, 6, pgl, matloc, zr(jmatr))
!
        else if (option.eq.'EPOT_ELEM') then
            call jevech('PENERDR', 'E', jener)
            do i = 1, 3
                zr(jener-1+i)=ener(i)
            enddo
        endif
!
!
        else if ( (option.eq.'MASS_MECA') .or. (option.eq.'MASS_MECA_DIAG') .or. &
              (option.eq.'MASS_MECA_EXPLI') .or. (option.eq.'M_GAMMA') .or. &
              (option.eq.'ECIN_ELEM') ) then
!
        if (nomte .eq. 'MEDKTR3' .or. nomte .eq. 'MET3TR3') then
            call dktmas(xyzl, option, pgl, matloc, ener)
        else if (nomte.eq.'MEDSTR3') then
            call dstmas(xyzl, option, pgl, matloc, ener)
        else if (nomte.eq.'MEDKQU4') then
            call dkqmas(xyzl, option, pgl, matloc, ener)
        else if (nomte.eq.'MEDSQU4') then
            call dsqmas(xyzl, option, pgl, matloc, ener)
        else if (nomte.eq.'MEQ4QU4') then
            call q4gmas(xyzl, option, pgl, matloc, ener)
        endif
        if (option .eq. 'MASS_MECA') then
            call jevech('PMATUUR', 'E', jmatr)
            call utpslg(nno, 6, pgl, matloc, zr(jmatr))
        else if (option.eq.'ECIN_ELEM') then
            call jevech('PENERCR', 'E', jener)
            call jevech('POMEGA2', 'L', jfreq)
            do i = 1, 3
                zr(jener-1+i)=zr(jfreq)*ener(i)
            enddo
        else if (option.eq.'M_GAMMA') then
            call jevech('PACCELR', 'L', iacce)
            call jevech('PVECTUR', 'E', ivect)
            nddl=6*nno
            nvec=nddl*(nddl+1)/2
            call utpslg(nno, 6, pgl, matloc, matv)
            call vecma(matv, nvec, matp, nddl)
            call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivect))
            else if (option.eq.'MASS_MECA_DIAG' .or. &
                 option.eq.'MASS_MECA_EXPLI') then
            call jevech('PMATUUR', 'E', jmatr)
            nddl=6*nno
            ndim=nddl*(nddl+1)/2
            do i = 1, ndim
                zr(jmatr-1+i)=matloc(i)
            enddo
            if (option .eq. 'MASS_MECA_EXPLI') then
!               CORRECTION DES TERMES CORRESPONDANT AU DDL 6
!               NON PREVU PAR LA THEORIE DKT. ON RAJOUTE
!               UN TERME DIAGONAL NON ZERO EGAL A CELUI DU DDL 5.
!               CETTE CORRECTION A ETE INSPIRE PAR LA DEMARCHE DANS EUROPLEXUS
                do j = 1, nno
                    n1=6*(j-1)+5
                    n2=6*(j-1)+4
                    ni=6*j
                    ndim=(ni+1)*ni/2
                    n1=(n1+1)*n1/2
                    n2=(n2+1)*n2/2
                    zr(jmatr-1+ndim)=(zr(jmatr-1+n1)+zr(jmatr-1+n2))*0.5d0
                enddo
            endif
        endif
!
!
    else if (option.eq.'MASS_INER') then
!     -----------------------------------
        call jevech('PMASSINE', 'E', jmatr)
        call dxroep(rho, epais)
        call dxiner(nno, zr(jgeom), rho, epais, zr(jmatr),&
                    zr(jmatr+1), zr(jmatr+4))
!
!     -- OPTIONS NON-LINEAIRES :
!     --------------------------
        else if (option(1:9).eq.'FULL_MECA'.or. option.eq.'RAPH_MECA'.or. &
                 option(1:10).eq.'RIGI_MECA_') then
!
        call jevech('PDEPLMR', 'L', jdepm)
        call jevech('PDEPLPR', 'L', jdepr)
        call jevech('PCOMPOR', 'L', icompo)
        if (lcqhom) then
            call utmess('F', 'ELEMENTS2_75')
        endif
        if ((zk16(icompo+2)(6:10).eq.'_REAC') .or. (zk16(icompo+2) .eq.'GROT_GDEP')) then
!           GROT_GDEP CORRESPOND ICI A EULER_ALMANSI
            if (zk16(icompo+2)(6:10) .eq. '_REAC') then
                call utmess('A', 'ELEMENTS2_72')
            endif
            do i = 1, nno
                i1=3*(i-1)
                i2=6*(i-1)
                zr(jgeom+i1) = zr(jgeom+i1) + zr(jdepm+i2) + zr(jdepr+i2)
                zr(jgeom+i1+1) = zr(jgeom+i1+1) + zr(jdepm+i2+1) + zr(jdepr+i2+1)
                zr(jgeom+i1+2) = zr(jgeom+i1+2) + zr(jdepm+i2+2) + zr(jdepr+i2+2)
            enddo
!
            if (nno .eq. 3) then
                call dxtpgl(zr(jgeom), pgl)
            else if (nno.eq.4) then
                call dxqpgl(zr(jgeom), pgl, 'S', iret)
            endif
            call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
        endif
!
        call utpvgl(nno, 6, pgl, zr(jdepm), uml)
        call utpvgl(nno, 6, pgl, zr(jdepr), dul)
!
        if (nomte .eq. 'MEDKTR3') then
            call dktnli(nomte, option, xyzl, uml, dul,&
                        vecloc, matloc, codret(1))
        else if (nomte.eq.'MEDKQU4 ') then
            call dktnli(nomte, option, xyzl, uml, dul,&
                        vecloc, matloc, codret(1))
        else
            call utmess('F', 'ELEMENTS2_74', sk=nomte)
        endif
!
        if (option(1:9) .eq. 'FULL_MECA') then
            call jevech('PMATUUR', 'E', jmatr)
            call jevech('PVECTUR', 'E', jvect)
            call utpslg(nno, 6, pgl, matloc, zr(jmatr))
            call utpvlg(nno, 6, pgl, vecloc, zr(jvect))
        else if (option.eq.'RAPH_MECA') then
            call jevech('PVECTUR', 'E', jvect)
            call utpvlg(nno, 6, pgl, vecloc, zr(jvect))
        else if (option(1:10).eq.'RIGI_MECA_') then
            call jevech('PMATUUR', 'E', jmatr)
            call utpslg(nno, 6, pgl, matloc, zr(jmatr))
        endif
!
!
    else if (option.eq.'FORC_NODA') then
!     -------------------------------------
        call tecach('OOO', 'PCONTMR', 'L', iret, nval=7,&
                    itab=itab)
        icontp=itab(1)
        nbsp=itab(7)
        nbcou=zi(jnbspi)
!
        if (nbsp .ne. npge*nbcou) then
            call utmess('F', 'ELEMENTS_4')
        endif
!
        ind=8
        call dxeffi(option, nomte, pgl, zr(icontp), ind,&
                    effgt)
!
        call tecach('NNN', 'PCOMPOR', 'L', iret, iad=icompo)
        if (icompo .ne. 0) then
            if ((zk16(icompo+2)(6:10).eq.'_REAC') .or. (zk16(icompo+2) .eq.'GROT_GDEP')) then
                if (zk16(icompo+2)(6:10) .eq. '_REAC') then
                    call utmess('A', 'ELEMENTS2_72')
                endif
                call jevech('PDEPLMR', 'L', jdepm)
                call jevech('PDEPLPR', 'L', jdepr)
                do i = 1, nno
                    i1 = 3* (i-1)
                    i2 = 6* (i-1)
                    zr(jgeom+i1) = zr(jgeom+i1) + zr(jdepm+i2) + zr(jdepr+i2)
                    zr(jgeom+i1+1) = zr(jgeom+i1+1) + zr(jdepm+i2+1) + zr(jdepr+i2+1)
                    zr(jgeom+i1+2) = zr(jgeom+i1+2) + zr(jdepm+i2+2) + zr(jdepr+i2+2)
                end do
                if (nno .eq. 3) then
                    call dxtpgl(zr(jgeom), pgl)
                else if (nno.eq.4) then
                    call dxqpgl(zr(jgeom), pgl, 'S', iret)
                endif
!
                call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
            endif
        endif
!
! ------ CALCUL DES EFFORTS INTERNES (I.E. SOMME_VOL(BT_SIG))
        call dxbsig(nomte, xyzl, pgl, effgt, bsigma,&
                    option)
!
! ------ AFFECTATION DES VALEURS DE BSIGMA AU VECTEUR EN SORTIE
        call jevech('PVECTUR', 'E', jvect)
        k=0
        do i = 1, nno
            do j = 1, 6
                k=k+1
                zr(jvect+k-1)=bsigma(k)
            enddo
        enddo
!
    else if (option.eq.'REFE_FORC_NODA') then
!     -------------------------------------
        call terefe('EFFORT_REFE', 'MECA_COQUE', foref)
        call terefe('MOMENT_REFE', 'MECA_COQUE', moref)
!
        ind=8
        do i = 1, nno
            do j = 1, 3
                effgt((i-1)*ind+j) = foref
                effgt((i-1)*ind+3+j) = moref
            enddo
        enddo
!
! ------ CALCUL DES EFFORTS INTERNES (I.E. SOMME_VOL(BT_SIG))
        call dxbsig(nomte, xyzl, pgl, effgt, bsigma,&
                    option)
!
! ------ AFFECTATION DES VALEURS DE BSIGMA AU VECTEUR EN SORTIE
        call jevech('PVECTUR', 'E', jvect)
        k=0
        do i = 1, nno
            effref=(abs(bsigma(k+1))+abs(bsigma(k+2))+abs(bsigma(k+3)))/3.d0
            momref=(abs(bsigma(k+4))+abs(bsigma(k+5))+abs(bsigma(k+6)))/3.d0
            do j = 1, 6
                k=k+1
                if (j .lt. 4) then
                    zr(jvect+k-1) = effref
                else
                    zr(jvect+k-1) = momref
                endif
            enddo
        enddo
    else
!       OPTION DE CALCUL INVALIDE
        ASSERT(.false.)
    endif
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret)=codret(1)
    endif
!
    if (option .ne. 'REFE_FORC_NODA') then
! --- PASSAGE DES CONTRAINTES DANS LE REPERE UTILISATEUR :
        call cosiro(nomte, 'PCONTPR', 'E', 'IU', 'G',&
                    ibid, 'R')
    endif
!
end subroutine
