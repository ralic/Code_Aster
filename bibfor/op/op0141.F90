subroutine op0141()
    implicit none
!     ------------------------------------------------------------------
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
!     OPERATEUR DE CALCUL DU MAC DE DEUX BASES MODALES
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/copmod.h"
#include "asterfort/dcapno.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/idensd.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mcmult.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/rsorac.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbimex.h"
#include "asterfort/tbimpr.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vdiff.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/zcopy.h"
!
    integer :: n1, n2, n3, ibid, nbmod1, nbmod2, neq, idbas1
    integer :: idbas2, idbas3, idvec3, i, j, nbpara, inom, ityp, ind, imatra
    integer :: idvec1, iddeeq, idvec2, ifm, niv, llneq1, neq1, llneq2, iret
    integer :: iddl, indv, tmod(1)
    real(kind=8) :: rbid, pij, pii, pjj
    complex(kind=8) :: cbid, dcmplx, ztemp, dconjg
    character(len=1) :: typsca
    character(len=8) :: table, base1, base2, k8b, matras, rep
    character(len=14) :: nu, numdd1, numdd2, numdda
    character(len=16) :: nomcmd, typcon, typba1, typba2, matri1, matri2, depl
    character(len=19) :: matr, pronu1, pronu2, pronua
    character(len=24) :: chamol
    logical :: c1, c2, zcmplx, ieri
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
    data depl /'DEPL            '/
!
!     --- RECUPERATION DU RESULTAT ET DU MODE A TRAITER ---
    call jemarq()
!
    call getres(table, typcon, nomcmd)
! CREATION DE LA TABLE CONTENANT LE MAC
    call tbcrsd(table, 'G')
    call titre()
!     ---RECUPERATION DU NIVEAU D'IMPRESSION---
!
    call infmaj()
    call infniv(ifm, niv)
!
! RECUPERATION DE LA MATRICE ASSEMBLEE SI ELLE EXISTE
    call getvid(' ', 'MATR_ASSE', scal=matras, nbret=n1)
    if (n1 .ne. 0) then
! COOL ELLE EXISTE
        call mtdscr(matras)
        matr=matras
        call jeveuo(matr//'.&INT', 'E', imatra)
        call dismoi('NOM_NUME_DDL', matras, 'MATR_ASSE', repk=numdda)
        call dismoi('NB_EQUA', matras, 'MATR_ASSE', repi=neq)
    else
! PAS COOL ELLE EXISTE PAS
        matr=' '
    endif
!
    ieri = .false.
    call getvtx(' ', 'IERI', scal=rep, nbret=n1)
    if (n1 .eq. 1) then
        if (rep .eq. 'OUI') ieri = .true.
    endif
!
! RECUPERATION DES BASES DE MODES
    call getvid(' ', 'BASE_1', scal=base1, nbret=n2)
    call getvid(' ', 'BASE_2', scal=base2, nbret=n3)
!
    c1 = .false.
    c2 = .false.
!
    call dcapno(base1, depl, 1, chamol)
    call jelira(chamol, 'TYPE', cval=typsca)
    if (typsca .eq. 'C') c1 = .true.
!
    call dcapno(base2, depl, 1, chamol)
    call jelira(chamol, 'TYPE', cval=typsca)
    if (typsca .eq. 'C') c2 = .true.
!
    if (c1 .and. c2) then
        zcmplx = .true.
    else
        zcmplx = .false.
        if (c1 .or. c2) then
            call utmess('F', 'ALGELINE5_71')
        endif
    endif
!
! RECUPERATION DU TYPE ET DU NBRE DE MODES DES BASES
    call gettco(base1, typba1)
    call rsorac(base1, 'LONUTI', 0, rbid, k8b,&
                cbid, rbid, 'ABSOLU', tmod, 1,&
                ibid)
    nbmod1=tmod(1)
    call gettco(base2, typba2)
    call rsorac(base2, 'LONUTI', 0, rbid, k8b,&
                cbid, rbid, 'ABSOLU', tmod, 1,&
                ibid)
    nbmod2=tmod(1)
!
! RECUPERATION DE LA NUMEROTATION DES BASES
!
    if ((typba1(1:9).eq.'MODE_MECA') .or. (typba1(1:9).eq.'MODE_GENE')) then
        call dismoi('REF_RIGI_PREM', base1, 'RESU_DYNA', repk=matri1)
        call exisd('MATR_ASSE', matri1, iret)
        if (iret .ne. 0) then
            call dismoi('NOM_NUME_DDL', matri1, 'MATR_ASSE', repk=numdd1)
        else
            call dismoi('NUME_DDL', base1, 'RESU_DYNA', repk=numdd1)
        endif
    else
!       On passe par la numerotation du REFD
        call dismoi('NUME_DDL', base1, 'RESU_DYNA', repk=numdd1)
    endif
    call exisd('NUME_DDL', numdd1, iret)
    if (iret .ne. 1) then
        call utmess('F', 'CALCESSAI0_14', sk=base1)
    endif
!
    call jeveuo(numdd1//'.NUME.NEQU', 'L', llneq1)
    neq1 = zi(llneq1)
!
!
    if ((typba2(1:9).eq.'MODE_MECA') .or. (typba2(1:9).eq.'MODE_GENE')) then
        call dismoi('REF_RIGI_PREM', base2, 'RESU_DYNA', repk=matri2)
        call exisd('MATR_ASSE', matri2, iret)
        if (iret .ne. 0) then
            call dismoi('NOM_NUME_DDL', matri2, 'MATR_ASSE', repk=numdd2)
        else
            call dismoi('NUME_DDL', base2, 'RESU_DYNA', repk=numdd2)
        endif
    else
        call dismoi('NUME_DDL', base2, 'RESU_DYNA', repk=numdd2)
    endif
    call exisd('NUME_DDL', numdd2, iret)
    if (iret .ne. 1) then
        call utmess('F', 'CALCESSAI0_14', sk=base2)
    endif
    call jeveuo(numdd2//'.NUME.NEQU', 'L', llneq2)
!
! ---- Verification : les deux nume_ddl doivent etre identiques
    pronu1=(numdd1//'.NUME')
    pronu2=(numdd2//'.NUME')
    if (.not.idensd('PROF_CHNO',pronu1,pronu2)) then
        call utmess('F', 'ALGELINE2_80')
    endif
!
! --- Verification : le nume_ddl doit etre celui de la MATR_ASSE
    if (matr .ne. ' ') then
        pronua=(numdda//'.NUME')
        if (.not.idensd('PROF_CHNO',pronu1,pronua)) then
            call utmess('F', 'ALGELINE2_81')
        endif
        nu = numdda(1:14)
        call jeveuo(nu//'.NUME.DEEQ', 'L', iddeeq)
    else
        nu = numdd1(1:14)
        neq=neq1
    endif
!
! INITIALISATION DE LA TABLE DES MACS
    if (zcmplx) then
        nbpara=3
    else
        nbpara=4
    endif
    call wkvect('&&OP0141.TYPE_PARA', 'V V K8 ', nbpara, ityp)
    call wkvect('&&OP0141.NOM_PARA', 'V V K16', nbpara, inom)
    do i = 1, 2
        zk8(ityp+i-1)='I'
    end do
    if (zcmplx) then
        call wkvect('&&OP0141.MAC', 'V V R', 1, indv)
    else
        call wkvect('&&OP0141.MAC', 'V V R', 2, indv)
! MATRICE GENERALISEE EN PLUS POUR LES MODES REELS
        zk16(inom+3)='Y1_W_Y2'
        zk8(ityp+3)='R'
    endif
    zk8(ityp+2)='R'
    zk16(inom)='NUME_MODE_1'
    zk16(inom+1)='NUME_MODE_2'
    if (ieri) then
        zk16(inom+2)='IERI'
    else
        zk16(inom+2)='MAC'
    endif
    call tbajpa(table, nbpara, zk16(inom), zk8(ityp))
!
    call wkvect('&&OP0141.IJ', 'V V I', 2, ind)
!
    if (zcmplx) then
!
        call wkvect('&&OP0141.BASE1', 'V V C', nbmod1*neq, idbas1)
        call wkvect('&&OP0141.BASE2', 'V V C', nbmod2*neq, idbas2)
        call wkvect('&&OP0141.BASE3', 'V V C', neq, idbas3)
        call wkvect('&&OP0141.TEMP1', 'V V C', neq, idvec1)
        call wkvect('&&OP0141.TEMP2', 'V V C', neq, idvec2)
        call wkvect('&&OP0141.TEMP3', 'V V C', neq, idvec3)
!
        call copmod(base1, numer=nu, bmodz=zc(idbas1))
        call copmod(base2, numer=nu, bmodz=zc(idbas2))
!
! BOUCLE DE CALCUL DES MACS
        do i = 1, nbmod1
            pii=0.d0
            if (matr .ne. ' ') then
                call mcmult('ZERO', imatra, zc(idbas1+(i-1)*neq), zc( idvec1), 1,&
                            .true.)
!
                do iddl = 1, neq
                    if (zi(iddeeq-1+2*iddl) .le. 0) zc(idvec1-1+iddl) = dcmplx(0.d0,0.d0)
                end do
!
            else
                call zcopy(neq, zc(idbas1+(i-1)*neq), 1, zc(idvec1), 1)
            endif
!
! PB AVEC ZDOTC DE BLAS POUR CERTAIN COMPILO -> CALCUL DIRECT
            ztemp = dcmplx(0.0d0,0.0d0)
            do iddl = 1, neq
                ztemp = ztemp + zc( idbas1+(i-1)*neq-1+iddl)*dconjg(zc( idvec1-1+iddl))
            end do
            pii = abs(ztemp)
!
            zi(ind)=i
!
            do j = 1, nbmod2
                pij=0.d0
                pjj=0.d0
                if (matr .ne. ' ') then
                    call mcmult('ZERO', imatra, zc(idbas2+(j-1)*neq), zc(idvec2), 1,&
                                .true.)
!
                    do iddl = 1, neq
                        if (zi(iddeeq-1+2*iddl) .le. 0) zc(idvec2-1+iddl) = dcmplx(0.d0,0.d0)
                    end do
!
                else
                    call zcopy(neq, zc(idbas2+(j-1)*neq), 1, zc(idvec2), 1)
                endif
!
                ztemp = dcmplx(0.0d0,0.0d0)
                do iddl = 1, neq
                    ztemp = ztemp + zc( idbas2+(j-1)*neq-1+iddl)* dconjg(zc(idvec2-1+iddl))
                end do
                pjj = abs(ztemp)
!
                if (ieri) then
                    do iddl = 1, neq
                        zc(idbas3-1+iddl)=zc(idbas1+(i-1)*neq-1+iddl)&
                        -zc(idbas2+(j-1)*neq-1+iddl)
                    end do
                    call mcmult('ZERO', imatra, zc(idbas3), zc(idvec3), 1,&
                                .true.)
                    do iddl = 1, neq
                        if (zi(iddeeq-1+2*iddl) .le. 0) zc(idvec3-1+iddl) = dcmplx(0.d0,0.d0)
                    end do
!
                    ztemp = dcmplx(0.0d0,0.0d0)
                    do iddl = 1, neq
                        ztemp = ztemp + zc(idbas3-1+iddl)*dconjg(zc( idvec3-1+iddl))
                    end do
                    pij = abs(ztemp)
!
                    pij = (pij**2) / (pii**2 + pjj**2)
                else
                    ztemp = dcmplx(0.0d0,0.0d0)
                    do iddl = 1, neq
                        ztemp = ztemp + zc( idbas1+(i-1)*neq-1+iddl)* dconjg(zc(idvec2-1+iddl) )
                    end do
                    pij = abs(ztemp)
                    pij = (pij**2) / (pii * pjj)
                endif
!
                zi(ind+1)=j
                zr(indv)=pij
                call tbajli(table, nbpara, zk16(inom), zi(ind), zr( indv),&
                            [cbid], k8b, 0)
            end do
        end do
!
    else
!
        call wkvect('&&OP0141.BASE1', 'V V R', nbmod1*neq, idbas1)
        call wkvect('&&OP0141.BASE2', 'V V R', nbmod2*neq, idbas2)
        call wkvect('&&OP0141.BASE3', 'V V R', neq, idbas3)
        call wkvect('&&OP0141.TEMP1', 'V V R', neq, idvec1)
        call wkvect('&&OP0141.TEMP2', 'V V R', neq, idvec2)
        call wkvect('&&OP0141.TEMP3', 'V V R', neq, idvec3)
!
        call copmod(base1, numer=nu, bmodr=zr(idbas1))
        call copmod(base2, numer=nu, bmodr=zr(idbas2))
!
! BOUCLE DE CALCUL DES MACS
        do i = 1, nbmod1
            pii=0.d0
            if (matr .ne. ' ') then
                call mrmult('ZERO', imatra, zr(idbas1+(i-1)*neq), zr(idvec1), 1,&
                            .true.)
                call zerlag(neq, zi(iddeeq), vectr=zr(idvec1))
            else
                call dcopy(neq, zr(idbas1+(i-1)*neq), 1, zr(idvec1), 1)
            endif
!
            pii = abs(ddot( neq, zr(idbas1+(i-1)*neq),1, zr(idvec1),1) )
!
            zi(ind)=i
!
            do j = 1, nbmod2
                pij=0.d0
                pjj=0.d0
                if (matr .ne. ' ') then
                    call mrmult('ZERO', imatra, zr(idbas2+(j-1)*neq), zr(idvec2), 1,&
                                .true.)
                    call zerlag(neq, zi( iddeeq), vectr=zr(idvec2))
                else
                    call dcopy(neq, zr(idbas2+(j-1)*neq), 1, zr(idvec2), 1)
                endif
!
                pjj = abs(ddot( neq, zr(idbas2+(j-1)*neq),1, zr( idvec2),1))
!
                if (ieri) then
                    call vdiff(neq, zr(idbas1+(i-1)*neq), zr(idbas2+(j- 1)*neq), zr(idbas3))
                    call mrmult('ZERO', imatra, zr(idbas3), zr(idvec3), 1,&
                                .true.)
                    call zerlag(neq, zi( iddeeq), vectr=zr(idvec3))
!
                    pij = abs(ddot( neq,zr(idbas3) ,1, zr(idvec3),1))
!
                    pij = (pij**2) / (pii**2 + pjj**2)
!  POUR LA MATRICE GENERALISEE : Y1_W_Y2
                    rbid = abs(ddot( neq,zr(idbas1+(i-1)*neq) ,1, zr(idvec2),1) )
!
                    rbid = (rbid**2) / (pii * pjj)
                    zr(indv+1)=sqrt(rbid*pii*pjj)
                else
                    pij = abs(ddot( neq,zr(idbas1+(i-1)*neq) ,1, zr(idvec2),1))
!
                    pij = (pij**2) / (pii * pjj)
                    zr(indv+1)=sqrt(pij*pii*pjj)
                endif
!
                zi(ind+1)=j
                zr(indv)=pij
                call tbajli(table, nbpara, zk16(inom), zi(ind), zr( indv),&
                            [cbid], k8b, 0)
            end do
        end do
!
    endif
!  FIN TEST SUR TYPE DE VECTEURS (C/R)
!
    if (niv .ge. 2) then
        call tbimpr(table, 'TABLEAU', ifm, 3, zk16(inom),&
                    0, '1PE12.5')
        if (nbpara .eq. 4) then
            write(ifm,*) ' '
            write(ifm,1000) zk16(inom+2)
            call tbimex(table, ifm, 4, zk16(inom), 'EXCEL',&
                        '1PE12.5')
            write(ifm,*) ' '
        endif
    endif
    1000 format('AFFICHAGE ',a4,' ET MATRICE GENERALISEE : ')
!
    call jedema()
end subroutine
