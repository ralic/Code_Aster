subroutine rcZ2sn(ze200, lieu, numsip, numsiq,iocs, mse,&
                  propi, propj, proqi, proqj, instsn, sn, sp3, spmeca3)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/getvtx.h"
#include "asterfort/rcZ2sn1a.h"
#include "asterfort/rcZ2sn1b.h"
#include "asterfort/jedema.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcveri.h"
#include "asterfort/tbexip.h"
#include "asterfort/utmess.h"
#include "asterfort/tbexv1.h"
#include "asterfort/rcver1.h"
#include "asterfort/getvid.h"
#include "asterfort/wkvect.h"
#include "asterfort/tbliva.h"
#include "asterfort/rcZ2s2.h"
#include "asterfort/rc32my.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterc/getfac.h"
#include "asterfort/jedetr.h"
    aster_logical :: ze200
    character(len=4) :: lieu
    integer :: numsip, numsiq, iocs
    real(kind=8) :: sn, instsn(2), sp3, spmeca3
    real(kind=8) :: mse(12), propi(20), propj(20), proqi(20), proqj(20)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200
!     CALCUL DU SP
!
!     ------------------------------------------------------------------
! IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
! IN  : NUMSIP : NUMERO SITUATION DE L'ETAT STABILISE P
! IN  : NUMSIQ : NUMERO SITUATION DE L'ETAT STABILISE Q
! OUT : SN1    : PARTIE B3200 du SP
!
    character(len=8) :: methode, tabfm(6), crit(1), k8b, nocmp(6)
    integer :: nb, n1, n0, i, nbabsc, jabsc, ndim, nbchar
    real(kind=8) :: prec(1), vale(1), momen0, momen1
    character(len=16) :: typmec, valek(1)
    aster_logical :: exist, seismeb32, seismeunit, seismeze200
    character(len=24) :: valk(3)
    integer :: jseis, j, ibid, iret, k
    complex(kind=8) :: cbid
    real(kind=8), pointer :: contraintes(:) => null()
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
!-------------------------------------
!     SEISME ZE200 ?
!-------------------------------------
    seismeze200 = .false.
    call getvtx(' ', 'TYPE_RESU_MECA', scal=typmec, nbret=n1)
    if (iocs .ne. 0 .and. typmec .eq. 'ZE200a') seismeze200=.true.
    if (iocs .ne. 0 .and. typmec .eq. 'ZE200b') seismeze200=.true.
!
!-------------------------------------
!     SI SEISME AVEC B3200_T
!-------------------------------------
    seismeb32 = .false.
    seismeunit = .false.
!
    call getfac('CHAR_MECA', nbchar)
    if (iocs .ne. 0 .and. typmec .eq. 'B3200' .and. nbchar .ne. 0) seismeunit=.true.
    if (iocs .ne. 0 .and. typmec .eq. 'B3200' .and.  nbchar .eq. 0) seismeb32=.true.
!
    if (seismeb32) then
        valek(1) = 'ABSC_CURV       '
        prec(1) = 1.0d-06
        crit(1) = 'RELATIF'
        nocmp(1) = 'SIXX'
        nocmp(2) = 'SIYY'
        nocmp(3) = 'SIZZ'
        nocmp(4) = 'SIXY'
        nocmp(5) = 'SIXZ'
        nocmp(6) = 'SIYZ'
!-- on récupère les tables correspondantes
        call getvid('SEISME', 'TABL_FX', iocc=iocs, scal=tabfm(1), nbret=n0)
        call getvid('SEISME', 'TABL_FY', iocc=iocs, scal=tabfm(2), nbret=n0)
        call getvid('SEISME', 'TABL_FZ', iocc=iocs, scal=tabfm(3), nbret=n0)
        call getvid('SEISME', 'TABL_MX', iocc=iocs, scal=tabfm(4), nbret=n0)
        call getvid('SEISME', 'TABL_MY', iocc=iocs, scal=tabfm(5), nbret=n0)
        call getvid('SEISME', 'TABL_MZ', iocc=iocs, scal=tabfm(6), nbret=n0)
! ----  on verifie l'ordre des noeuds de la table
        do 20 i = 1, 6
            call rcveri(tabfm(i))
 20     continue
! ----- on recupere les abscisses curvilignes de la table
        call tbexip(tabfm(1), valek(1), exist, k8b)
        if (.not. exist) then
            valk (1) = tabfm(1)
            valk (2) = valek(1)
            call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
        endif
        call tbexv1(tabfm(1), valek(1), 'RC.ABSC', 'V', nbabsc,&
                   k8b)
        call jeveuo('RC.ABSC', 'L', jabsc)
! ----- on vérifie la cohérence des tables
        do 30 i = 1, 5
            call rcver1('MECANIQUE', tabfm(1), tabfm(1+i))
 30     continue
! ----- on crée un vecteur qui contiendra les contraintes linéarisées dues au séisme
        ndim = 6*6
        call wkvect('&&RC3200.SIGSEIS', 'V V R', ndim, jseis)
        AS_ALLOCATE(vr=contraintes,  size=nbabsc)
! ----- on vient lire les tables
        do 40 i = 1, 6
            do 50 j = 1, 6
                do 60 k = 1, nbabsc
                  vale(1) = zr(jabsc+k-1)
!
                  call tbliva(tabfm(i), 1, valek, [ibid], vale,&
                             [cbid], k8b, crit, prec, nocmp(j),&
                             k8b, ibid, contraintes(k), cbid, k8b,&
                             iret)
                  if (iret .ne. 0) then
                    valk (1) = tabfm(i)
                    valk (2) = nocmp(j)
                    valk (3) = valek(1)
                    call utmess('F', 'POSTRCCM_2', nk=3, valk=valk, nr=1,&
                                valr=vale(1))
                  endif
 60             continue
                call rc32my(nbabsc, zr(jabsc), contraintes, momen0, momen1)
                if (lieu .eq. 'ORIG') then
                    zr(jseis+(i-1)*6+j-1) = momen0 - 0.5d0*momen1
                else
                    zr(jseis+(i-1)*6+j-1) = momen0 + 0.5d0*momen1
                endif
 50         continue
 40     continue
    endif
!
    call getvtx(' ', 'METHODE', scal=methode, nbret=nb)
    if (methode .eq. 'TRESCA') then
        call rcZ2sn1a(ze200, lieu, numsip, numsiq, seismeb32,&
                      seismeunit, seismeze200, mse, propi, propj, proqi, proqj,&
                      instsn, sn, sp3, spmeca3)
    else
        call rcZ2sn1b(ze200, lieu, numsip, numsiq, seismeb32,&
                      seismeunit, seismeze200, mse, propi, propj, proqi, proqj,&
                      instsn, sn, sp3, spmeca3)
    endif
!
    if (seismeb32) then
        call jedetr('RC.ABSC')
        call jedetr('&&RC3200.SIGSEIS')
        AS_DEALLOCATE(vr=contraintes)
    endif
!
    call jedema()
end subroutine
