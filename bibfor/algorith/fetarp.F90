subroutine fetarp(infofe, ifm, niter, nbi, nbreor,&
                  lrigid, dimgi, sdfeti, ipiv, nomggt,&
                  nbsd, ifetf, ifeth, nomgi, lstogi,&
                  irex, iprj, ir2, ifiv, matas,&
                  nbproc, rang)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  TEST DEFINIE POSITIVITE DE P*FI*P VIA UN
!      CALCUL DE LA PARTIE BASSE DE SON SPECTRE AVEC ARPACK
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/dnaupd.h"
#include "asterfort/dneupd.h"
#include "asterfort/fetfiv.h"
#include "asterfort/fetprj.h"
#include "asterfort/jedetr.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: ifm, niter, nbi, nbreor, dimgi, ipiv, nbsd, ifetf, ifeth, irex
    integer :: iprj, ir2, ifiv, rang, nbproc
    logical :: lrigid, lstogi
    character(len=19) :: sdfeti, matas
    character(len=24) :: infofe, nomggt, nomgi
!
!
! DECLARATION VARIABLES LOCALES
    integer :: ido, info, nfreq, nbvect, itest, itest1, i, iparam(11), itest2
    integer :: itest3, itest4, itest5, itest6, itest7, itest8, ipntr(14), lonwl
    real(kind=8) :: tol
    character(len=24) :: k24b
    logical :: lpara
!
! COMMON ARPACK
    integer :: logfil, ndigit, mgetv0, mnaupd, mnaup2, mnaitr, mneigh, mnapps
    integer :: mngets, mneupd
    common /debug/&
     &  logfil,ndigit,mgetv0,&
     &  mnaupd,mnaup2,mnaitr,mneigh,mnapps,mngets,mneupd
!
! INITS DIVERSES
    if (nbproc .eq. 1) then
        lpara=.false.
    else
        lpara=.true.
    endif
!
    if ((infofe(7:7).eq.'T') .and. (.not.lpara)) then
! NIVEAU D'IMPRESSION ARPACK
        ndigit=-3
        logfil=ifm
        mgetv0=0
        mnaupd=0
        mnaup2=0
        mnaitr=0
        mneigh=0
        mnapps=0
        mngets=0
        mneupd=0
        ido=0
        info=0
        nfreq=min(niter,nbi-2)
        nbvect=min(nbreor,nbi)
        call wkvect('&&FETI.TESTA', 'V V R', nbi, itest)
        call wkvect('&&FETI.TEST1', 'V V R', nbi*nbvect, itest1)
        call wkvect('&&FETI.TEST2', 'V V R', 3*nbi, itest2)
        iparam(1) = 1
        iparam(3) = 10
        iparam(4) = 1
        iparam(7) = 1
        lonwl = 3*nbvect**2+6*nbvect
        call wkvect('&&FETI.TEST3', 'V V R', lonwl, itest3)
        call wkvect('&&FETI.TEST4', 'V V L', nbvect, itest4)
        call wkvect('&&FETI.TEST5', 'V V R', 2*(nfreq+1), itest5)
        call wkvect('&&FETI.TEST6', 'V V R', 3*nbvect, itest6)
        call wkvect('&&FETI.TEST7', 'V V R', nbi, itest7)
        call wkvect('&&FETI.TEST8', 'V V R', nbi, itest8)
        tol=1.d-15
!
31      continue
        call dnaupd(ido, 'I', nbi, 'SR', nfreq,&
                    tol, zr(itest), nbvect, zr(itest1), nbi,&
                    iparam, ipntr, zr(itest2), zr(itest3), lonwl,&
                    info, nbi, 0.717d0)
        if (abs(ido) .eq. 1) then
            call fetprj(nbi, zr(itest2+ipntr(1)-1), zr(itest7), nomggt, lrigid,&
                        dimgi, 1, sdfeti, ipiv, nbsd,&
                        zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                        infofe, irex, iprj, nbproc, rang,&
                        k24b)
            call fetfiv(nbsd, nbi, zr(itest7), zr(ir2), zr(itest8),&
                        matas, zi(ifetf), zi(ifeth), infofe, irex,&
                        ifiv, nbproc, rang, k24b, sdfeti)
            call fetprj(nbi, zr(itest8), zr(itest2+ipntr(2)-1), nomggt, lrigid,&
                        dimgi, 1, sdfeti, ipiv, nbsd,&
                        zi(ifetf), zi(ifeth), matas, nomgi, lstogi,&
                        infofe, irex, iprj, nbproc, rang,&
                        k24b)
            goto 31
        else if (ido.eq.2) then
            call dcopy(nbi, zr(itest2+ipntr(1)-1), 1, zr(itest2+ipntr(2) -1), 1)
            goto 31
        endif
        if ((info.ne.0) .or. ((ido.eq.99).and.(iparam(5).lt.nfreq))) then
            call u2mess('A', 'ALGORITH3_62')
        endif
        info=0
        call dneupd(.false., 'A', zl(itest4), zr(itest5), zr(itest5+nfreq),&
                    zr(itest1), nbi, 0.d0, 0.d0, zr(itest6),&
                    'I', nbi, 'SR', nfreq, 0.d0,&
                    zr(itest), nbvect, zr(itest1), nbi, iparam,&
                    ipntr, zr(itest2), zr(itest3), lonwl, info)
        if (info .ne. 0) call u2mess('A', 'ALGORITH3_63')
        write(ifm,*)
        write(ifm,*)'******* NMAX_ITER VP PLUS BASSES DE P*FI*P *******'
        do 33 i = 1, iparam(5)
            write(ifm,1070)i,zr(itest5+i-1),zr(itest5+nfreq+i-1)
33      continue
        write(ifm,*)&
        '**************************************************'
        write(ifm,*)
        call jedetr('&&FETI.TESTA')
        call jedetr('&&FETI.TEST1')
        call jedetr('&&FETI.TEST2')
        call jedetr('&&FETI.TEST3')
        call jedetr('&&FETI.TEST4')
        call jedetr('&&FETI.TEST5')
        call jedetr('&&FETI.TEST6')
        call jedetr('&&FETI.TEST7')
        call jedetr('&&FETI.TEST8')
    endif
    1070 format('VALEUR PROPRE N ',i3,' ',d11.4,' + I ',d11.4)
!
end subroutine
