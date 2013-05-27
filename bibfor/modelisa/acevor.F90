subroutine acevor(nbocc, nlm, nlg, nln, nlj,&
                  ier)
    implicit none
    include 'asterc/getres.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/codent.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: nbocc, nlm, nlg, nln, nlj, ier
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     VERIFICATION DES MOTS CLES POUR LES ORIENTATIONS
! ----------------------------------------------------------------------
! IN  : NBOCC  : NOMBRE D'OCCURENCE
! OUT : NLM    : NOMBRE TOTAL DE MAILLE
! OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
! OUT : NLN    :
! OUT : NLJ    :
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ioc, j, k, nbcar, nbval, nc, ncar
    integer :: nco, ng, nj, nm, nn, nsom, nv
    integer :: nval
!-----------------------------------------------------------------------
    parameter ( nbcar = 100 , nbval = 1000 , nco = 4 )
    real(kind=8) :: r8b, val(nbval)
    character(len=6) :: kioc
    character(len=8) :: k8b, car(nbcar), nomu, carori(nco)
    character(len=16) :: cmd, concep
    character(len=24) :: valk(2)
    integer :: iarg
    data carori  /'VECT_Y  ','VECT_X_Y','ANGL_NAU','ANGL_VRI'/
!     ------------------------------------------------------------------
!
    call getres(nomu, concep, cmd)
    nlm = 0
    nlg = 0
    nln = 0
    nlj = 0
!
    do 10 ioc = 1, nbocc
        call codent(ioc, 'G', kioc)
        call getvtx('ORIENTATION', 'GROUP_MA', ioc, iarg, 0,&
                    k8b, ng)
        call getvtx('ORIENTATION', 'MAILLE', ioc, iarg, 0,&
                    k8b, nm)
        call getvtx('ORIENTATION', 'GROUP_NO', ioc, iarg, 0,&
                    k8b, nj)
        call getvtx('ORIENTATION', 'NOEUD', ioc, iarg, 0,&
                    k8b, nn)
        call getvtx('ORIENTATION', 'CARA', ioc, iarg, 0,&
                    k8b, nc)
        call getvtx('ORIENTATION', 'CARA', ioc, iarg, nbcar,&
                    car, ncar)
        call getvr8('ORIENTATION', 'VALE', ioc, iarg, 0,&
                    r8b, nv)
        call getvr8('ORIENTATION', 'VALE', ioc, iarg, nbval,&
                    val, nval)
!
! -- IOC = 1
        if (ioc .eq. 1) then
            if (nv .eq. 0) then
                call u2mess('E', 'MODELISA_57')
                ier = ier + 1
            endif
            if (nc .eq. 0) then
                call u2mess('E', 'MODELISA_58')
                ier = ier + 1
            endif
        endif
!
! -- CARA
        if (ncar .gt. 0) then
!-DEL       NCARAC = NCAR
            if (nval .eq. 0) then
                call u2mesk('E', 'MODELISA_59', 1, kioc)
                ier = ier + 1
            endif
            k = 0
            do 20 j = 1, nco
                if (car(1) .eq. carori(j)) k = j
20          continue
        endif
!
! -- VALE
        if (nval .gt. 0) then
            if ((k.eq.1.and.nval.ne.3) .or. (k.eq.2.and.nval.ne.6) .or.&
                (k.eq.3.and.nval.ne.3) .or. (k.eq.4.and.nval.ne.1)) then
                valk(1) = kioc
                valk(2) = carori(k)
                call u2mesk('E', 'MODELISA_60', 2, valk)
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
10  end do
!
end subroutine
