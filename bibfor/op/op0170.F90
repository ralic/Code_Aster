subroutine op0170()
    implicit none
!     -----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CALCUL FATIGUE ALEATOIRE
!
!     -----------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8vide.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pdadom.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbexve.h"
#include "asterfort/titre.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: ibid, nbtab, nbmom, n1, nbpfat, ivmom, i, ilign, nbl0, inbl0
    integer :: nbl2, inbl2, nbl4, inbl4
    parameter     ( nbpfat = 5 )
    real(kind=8) :: xm0, xm2, xm4, rundf, dom, rduree, valer(5)
    complex(kind=8) :: c16b
    character(len=8) :: k8b, nomres, table, typfat(nbpfat)
    character(len=16) :: nomcmd, concep, nopfat(nbpfat), nopfa2(4)
    character(len=24) :: nomob1, nomob2, nomob3
    integer :: iarg
!     -----------------------------------------------------------------
    data  nopfat / 'MOMENT_SPEC_0' , 'MOMENT_SPEC_2' ,&
     &               'MOMENT_SPEC_4' ,&
     &               'DUREE'         , 'DOMMAGE'       /
    data  nopfa2 / 'MOMENT_SPEC_0' , 'MOMENT_SPEC_2' ,&
     &               'DUREE'         , 'DOMMAGE'       /
    data  typfat / 'R' , 'R' , 'R' , 'R' , 'R' /
!     -----------------------------------------------------------------
!
    call infmaj()
    rundf = r8vide()
    xm4 = rundf
    ivmom = 0
!
    call getres(nomres, concep, nomcmd)
!
    call getvr8(' ', 'DUREE', scal=rduree, nbret=n1)
!
    call getvid(' ', 'TABL_POST_ALEA', nbval=0, nbret=nbtab)
!
    if (nbtab .ne. 0) then
        call getvid(' ', 'TABL_POST_ALEA', scal=table, nbret=n1)
!        CALL TBEXP2(TABLE,'GRANDEUR')
        call tbexp2(table, 'LAMBDA_00')
        call tbexp2(table, 'LAMBDA_02')
        call tbexp2(table, 'LAMBDA_04')
!        CALL TBLIVA ( TABLE, 0, K8B, IBID, R8B, C16B, K8B, K8B, R8B,
!     &                'GRANDEUR', K8B, IBID, R8B, C16B, K8B, IRET )
!        IF ( IRET.NE.0 .AND. IRET.NE.3 )
!     &                CALL U2MESS('F','MODELISA2_89')
!        IF ( K8B .NE. 'DSP_SIPO' .OR. K8B .NE. 'DSP_SIGM' .OR.
!     &       K8B .NE. 'DSP_EFGE'  ) THEN
!              CALL U2MESS('A','PREPOST4_16')
!        ENDIF
        nomob1 = '&&OP0170.LAMBDA_0'
        call tbexve(table, 'LAMBDA_00', nomob1, 'V', nbl0,&
                    k8b)
        call jeveuo(nomob1, 'L', inbl0)
        nomob2 = '&&OP0170.LAMBDA_2'
        call tbexve(table, 'LAMBDA_02', nomob2, 'V', nbl2,&
                    k8b)
        if (nbl2 .ne. nbl0) call u2mess('F', 'MODELISA2_89')
        call jeveuo(nomob2, 'L', inbl2)
        nomob3 = '&&OP0170.LAMBDA_4'
        call tbexve(table, 'LAMBDA_04', nomob3, 'V', nbl4,&
                    k8b)
        if (nbl4 .ne. nbl0) call u2mess('F', 'ALGELINE_7')
        call jeveuo(nomob3, 'L', inbl4)
        nbmom = nbl0
        call wkvect('&&OP0170.MOMENT', 'V V R', 3*nbmom, ivmom)
        do 10 i = 1, nbl0
            zr(ivmom+(i-1)*3 ) = zr(inbl0+i-1)
            zr(ivmom+(i-1)*3+1) = zr(inbl2+i-1)
            zr(ivmom+(i-1)*3+2) = zr(inbl4+i-1)
10      continue
!
    else
!
        call getvr8(' ', 'MOMENT_SPEC_0', scal=xm0, nbret=n1)
        call getvr8(' ', 'MOMENT_SPEC_2', scal=xm2, nbret=n1)
        call getvr8(' ', 'MOMENT_SPEC_4', scal=xm4, nbret=n1)
        nbmom = 1
        call wkvect('&&OP0170.MOMENT', 'V V R', 3*nbmom, ivmom)
        zr(ivmom ) = xm0
        zr(ivmom+1) = xm2
        zr(ivmom+2) = xm4
!
    endif
!
    if (nbmom .eq. 0) call u2mess('A', 'PREPOST4_17')
!
    call tbcrsd(nomres, 'G')
    call tbajpa(nomres, nbpfat, nopfat, typfat)
!
    ilign = 0
    do 20 i = 1, nbmom
        xm0 = zr(ivmom+(i-1)*3 )
        xm2 = zr(ivmom+(i-1)*3+1)
        xm4 = zr(ivmom+(i-1)*3+2)
        call pdadom(xm0, xm2, xm4, dom)
        dom = dom * rduree
        if (xm4 .eq. rundf) then
            valer(1) = xm0
            valer(2) = xm2
            valer(3) = rduree
            valer(4) = dom
            call tbajli(nomres, 4, nopfa2, ibid, valer,&
                        c16b, k8b, ilign)
        else
            valer(1) = xm0
            valer(2) = xm2
            valer(3) = xm4
            valer(4) = rduree
            valer(5) = dom
            call tbajli(nomres, nbpfat, nopfat, ibid, valer,&
                        c16b, k8b, ilign)
        endif
20  end do
!
    call titre()
!
!
end subroutine
