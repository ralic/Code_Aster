subroutine acevor(nbocc, nlm, nlg, nln, nlj, ier)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: nbocc, nlm, nlg, nln, nlj, ier
!
! --------------------------------------------------------------------------------------------------
!
!     AFFE_CARA_ELEM
!     VERIFICATION DES MOTS CLES POUR LES ORIENTATIONS
!
! --------------------------------------------------------------------------------------------------
!
! IN  : NBOCC  : NOMBRE D'OCCURENCE
! OUT : NLM    : NOMBRE TOTAL DE MAILLE
! OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
! OUT : NLN    :
! OUT : NLJ    :
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
#include "asterc/getres.h"
#include "asterfort/codent.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/utmess.h"
! --------------------------------------------------------------------------------------------------
    integer :: ioc, jj, kk, nbcar, nbval, nc, ncar
    integer :: nco, ng, nj, nm, nn, nsom, nv
    integer :: nval
! --------------------------------------------------------------------------------------------------
    parameter ( nbcar = 100 , nbval = 1000 , nco = 4 )
    real(kind=8) :: val(nbval)
    character(len=6) :: kioc
    character(len=8) :: car(nbcar), nomu, carori(nco)
    character(len=16) :: cmd, concep
    character(len=24) :: valk(2)
    data carori  /'VECT_Y  ','VECT_X_Y','ANGL_NAU','ANGL_VRI'/
! --------------------------------------------------------------------------------------------------
!
    call getres(nomu, concep, cmd)
    nlm = 0
    nlg = 0
    nln = 0
    nlj = 0
!
    nj = 0
    nn = 0
    do ioc = 1, nbocc
        call codent(ioc, 'G', kioc)
        call getvtx('ORIENTATION', 'GROUP_MA', iocc=ioc, nbval=0, nbret=ng)
        call getvtx('ORIENTATION', 'MAILLE', iocc=ioc, nbval=0, nbret=nm)
        call getvtx('ORIENTATION', 'CARA', iocc=ioc, nbval=0, nbret=nc)
        call getvtx('ORIENTATION', 'CARA', iocc=ioc, nbval=nbcar, vect=car, nbret=ncar)
        call getvr8('ORIENTATION', 'VALE', iocc=ioc, nbval=0, nbret=nv)
        call getvr8('ORIENTATION', 'VALE', iocc=ioc, nbval=nbval, vect=val, nbret=nval)
!
        if (ioc .eq. 1) then
            if (nv .eq. 0) then
                call utmess('E', 'MODELISA_57')
                ier = ier + 1
            endif
            if (nc .eq. 0) then
                call utmess('E', 'MODELISA_58')
                ier = ier + 1
            endif
        endif
!       CARA
        kk = 0
        if (ncar .gt. 0) then
            if (nval .eq. 0) then
                call utmess('E', 'MODELISA_59', sk=kioc)
                ier = ier + 1
            endif
            do jj = 1, nco
                if (car(1) .eq. carori(jj)) kk = jj
            enddo
        endif
!       VALE
        if (nval .gt. 0) then
            if ((kk.eq.1.and.nval.ne.3) .or. (kk.eq.2.and.nval.ne.6) .or.&
                (kk.eq.3.and.nval.ne.3) .or. (kk.eq.4.and.nval.ne.1)) then
                valk(1) = kioc
                valk(2) = carori(kk)
                call utmess('E', 'MODELISA_60', nk=2, valk=valk)
                ier = ier + 1
            endif
        endif
!
        nsom = ng + nm + nj + nn
        if (nsom .eq. ng .or. nsom .eq. nm .or. nsom .eq. nj .or. nsom .eq. nn) then
            nlm = max(nlm,-nm)
            nlg = max(nlg,-ng)
            nln = max(nln,-nn)
            nlj = max(nlj,-nj)
        endif
    enddo
!
end subroutine
