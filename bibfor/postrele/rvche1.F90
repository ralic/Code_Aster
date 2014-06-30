subroutine rvche1(chelez, nomjv, nbel, numail, pgl)
    implicit none
#include "jeveux.h"
#include "asterfort/dgmode.h"
#include "asterfort/digdel.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
#include "asterfort/utpsgl.h"
!
    integer :: nbel, numail(*)
    character(len=*) :: chelez, nomjv
    real(kind=8) :: pgl(3, 3)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: debugr
    integer ::  gd, iad, ncmpmx, nec, tabec(10), iavale
    integer ::  im, imail, igrel, ielg, mode, nscal, icoef, nsca, nnoe
    integer :: ncmpp, icmp, npcalc, iel, ncou, iachml, icou, ino, icmpt, nbgrel
    integer :: numxx, numyy, numzz, numxy, numxz, numyz, nuddl, jlongr
    integer ::  jpnt, ipoin,  imodel, ilong
    real(kind=8) :: sg(6), sl(6)
    character(len=8) :: nomcmp, nomma
    character(len=24) :: valk(2)
    character(len=16) :: option
    character(len=19) :: chelm, noligr
    character(len=8), pointer :: lgrf(:) => null()
    integer, pointer :: celd(:) => null()
    integer, pointer :: liel(:) => null()
    character(len=24), pointer :: celk(:) => null()
    integer, pointer :: repe(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    chelm = chelez
!
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
!
    call jeveuo(chelm//'.CELD', 'L', vi=celd)
    gd = celd(1)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
    call jeveuo('&CATA.TE.MODELOC', 'L', imodel)
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', ilong)
!
    nec = nbec( gd )
    if (nec .gt. 10) then
        call utmess('F', 'POSTRELE_53')
    endif
!
    call dismoi('NOM_OPTION', chelez, 'CHAM_ELEM', repk=option)
    if (option .eq. 'SIGM_ELNO' .or. option .eq. 'SIEF_ELNO') then
!         COMPOSANTE:  SIXX SIYY SIZZ SIXY SIXZ SIYZ
        elseif ( option .eq. 'EPSI_ELNO' .or. option .eq. 'EPSG_ELNO'&
    .or. option .eq. 'EPME_ELNO' .or. option .eq. 'EPMG_ELNO' )then
!         COMPOSANTE:  EPXX EPYY EPZZ EPXY EPXZ EPYZ
    else if (option .eq. 'EFGE_ELNO') then
!         COMPOSANTE:  NXX NYY NXY MXX MYY MXY
    else if (option .eq. 'DEGE_ELNO') then
!         COMPOSANTE:  N  VY VZ MT MFY MFZ
    else
        valk (1) = chelm
        valk (2) = option
        call utmess('F', 'POSTRELE_26', nk=2, valk=valk)
    endif
!
    call jedupo(chelm//'.CELV', 'V', nomjv, .false._1)
    call jeveuo(nomjv, 'E', iavale)
!
    call jeveuo(chelm//'.CELK', 'L', vk24=celk)
    noligr = celk(1)(1:19)
    call jeveuo(noligr//'.REPE', 'L', vi=repe)
    call jeveuo(noligr//'.LIEL', 'L', vi=liel)
    call jeveuo(jexatr(noligr//'.LIEL', 'LONCUM'), 'L', jlongr)
    call jelira(noligr//'.LIEL', 'NUTIOC', nbgrel)
    call jeveuo(noligr//'.LGRF', 'L', vk8=lgrf)
    nomma = lgrf(1)
    call jeveuo(jexatr(nomma//'.CONNEX', 'LONCUM'), 'L', jpnt)
!
    do im = 1, nbel
        imail = numail(im)
        igrel = repe(2*(imail-1)+1)
        ielg = repe(2*(imail-1)+2)
        if (igrel .eq. 0) goto 20
        mode=celd(celd(4+igrel) +2)
        if (mode .eq. 0) goto 20
        call dgmode(mode, imodel, ilong, nec, tabec)
        nscal = digdel( mode )
        icoef=max(1,celd(4))
        if (icoef .gt. 1) then
            call utmess('F', 'POSTRELE_15')
        endif
        nsca = nscal*icoef
        ipoin = zi(jlongr-1+igrel)
        iel = liel(ipoin+ielg-1)
        nnoe = zi(jpnt-1+iel+1) - zi(jpnt-1+iel)
        ncmpp = 0
        do icmp = 1, ncmpmx
            if (exisdg( tabec, icmp )) then
                ncmpp = ncmpp + 1
            endif
        end do
        npcalc = nscal / ncmpp
        ncou = npcalc / nnoe
        debugr=celd(celd(4+igrel)+8)
        iachml = debugr + nsca*(ielg-1)
        do icou = 1, ncou
            do ino = 1, nnoe
                numxx = 0
                numyy = 0
                numzz = 0
                numxy = 0
                numxz = 0
                numyz = 0
                sg(1) = 0.0d0
                sg(2) = 0.0d0
                sg(3) = 0.0d0
                sg(4) = 0.0d0
                sg(5) = 0.0d0
                sg(6) = 0.0d0
                nuddl = iachml-1+ncmpp*icoef*(ino-1) +(icou-1)*ncmpp* icoef*nnoe
                icmpt = 0
                do icmp = 1, ncmpmx
                    if (exisdg( tabec, icmp )) then
                        icmpt = icmpt + 1
                        nomcmp = zk8(iad-1+icmp)
                        if (nomcmp .eq. 'SIXX' .or. nomcmp .eq. 'EPXX' .or. nomcmp .eq.&
                            'NXX' .or. nomcmp .eq. 'N') then
                            numxx = nuddl + icmpt
                            sg(1) = zr(iavale-1+numxx)
                            elseif ( nomcmp .eq. 'SIYY' .or. nomcmp .eq.&
                        'EPYY' .or. nomcmp .eq. 'NYY' .or. nomcmp&
                        .eq. 'VY' ) then
                            numyy = nuddl + icmpt
                            sg(3) = zr(iavale-1+numyy)
                            elseif ( nomcmp .eq. 'SIZZ' .or. nomcmp .eq.&
                        'EPZZ' .or. nomcmp .eq. 'NXY' .or. nomcmp&
                        .eq. 'VZ' ) then
                            numzz = nuddl + icmpt
                            sg(6) = zr(iavale-1+numzz)
                            elseif ( nomcmp .eq. 'SIXY' .or. nomcmp .eq.&
                        'EPXY' .or. nomcmp .eq. 'MXX' .or. nomcmp&
                        .eq. 'MT' ) then
                            numxy = nuddl + icmpt
                            sg(2) = zr(iavale-1+numxy)
                            elseif ( nomcmp .eq. 'SIXZ' .or. nomcmp .eq.&
                        'EPXZ' .or. nomcmp .eq. 'MYY' .or. nomcmp&
                        .eq. 'MFY' ) then
                            numxz = nuddl + icmpt
                            sg(4) = zr(iavale-1+numxz)
                            elseif ( nomcmp .eq. 'SIYZ' .or. nomcmp .eq.&
                        'EPYZ' .or. nomcmp .eq. 'MXY' .or. nomcmp&
                        .eq. 'MFZ' ) then
                            numyz = nuddl + icmpt
                            sg(5) = zr(iavale-1+numyz)
                        endif
                    endif
                end do
!
                call utpsgl(1, 3, pgl, sg, sl)
!
                if (numxx .ne. 0) zr(iavale-1+numxx) = sl(1)
                if (numyy .ne. 0) zr(iavale-1+numyy) = sl(3)
                if (numzz .ne. 0) zr(iavale-1+numzz) = sl(6)
                if (numxy .ne. 0) zr(iavale-1+numxy) = sl(2)
                if (numxz .ne. 0) zr(iavale-1+numxz) = sl(4)
                if (numyz .ne. 0) zr(iavale-1+numyz) = sl(5)
!
            end do
!
        end do
!
 20     continue
    end do
!
    call jedema()
end subroutine
