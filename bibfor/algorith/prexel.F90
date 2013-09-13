subroutine prexel(champ, ioc, mamax, nomax, ispmax,&
                  cmpmax, valmax, mamin, nomin, ispmin,&
                  cmpmin, valmin, maamax, noamax, isamax,&
                  cmamax, vaamax, maamin, noamin, isamin,&
                  cmamin, vaamin)
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterc/r8vide.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    integer :: ioc, ispmax, ispmin, isamax, isamin
    real(kind=8) :: valmin, valmax, vaamin, vaamax
    character(len=8) :: mamax, nomax, cmpmax, mamin, nomin, cmpmin
    character(len=8) :: maamax, noamax, cmamax, maamin, noamin, cmamin
    character(len=*) :: champ
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     COMMANDE : POST_RELEVE_T
!                DETERMINE LES EXTREMA POUR UN CHAM_ELNO
!
! ----------------------------------------------------------------------
!
    integer :: jcesk, jcesd, jcesv, jcesl, jcesc, nbma, ncmp, nbm
    integer :: ibid, nbmail, idmail, nbc, nbcmp, jcmp
    integer :: i100, i110, icp, imai, nbpt, nbsp, ipt, isp, iad
    integer :: imamax, iptmax, imamin, iptmin, jcone
    integer :: imaaax, ipamax, imaain, ipamin, ier1, ier2
    real(kind=8) :: x
    character(len=8) ::  nocmp, ma
    character(len=16) :: motcle(2), typmcl(2)
    character(len=19) :: chams1
    character(len=24) :: mesmai
! ---------------------------------------------------------------------
!
    motcle(1) = 'GROUP_MA'
    typmcl(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(2) = 'MAILLE'
    mesmai = '&&PREXEL.MES_MAILLES'
!
    chams1 = '&&PREXEL.CHAMS1'
    call celces(champ, 'V', chams1)
!
    call jeveuo(chams1//'.CESK', 'L', jcesk)
    call jeveuo(chams1//'.CESD', 'L', jcesd)
    call jeveuo(chams1//'.CESV', 'L', jcesv)
    call jeveuo(chams1//'.CESL', 'L', jcesl)
    call jeveuo(chams1//'.CESC', 'L', jcesc)
    ma = zk8(jcesk-1+1)
    nbma = zi(jcesd-1+1)
    ncmp = zi(jcesd-1+2)
!
    call reliem(' ', ma, 'NU_MAILLE', 'ACTION', ioc,&
                2, motcle, typmcl, mesmai, nbm)
    if (nbm .gt. 0) then
        nbmail = nbm
        call jeveuo(mesmai, 'L', idmail)
    else
        nbmail = nbma
    endif
!
    call getvtx('ACTION', 'NOEUD', iocc=ioc, nbval=0, nbret=ier1)
    call getvtx('ACTION', 'GROUP_NO', iocc=ioc, nbval=0, nbret=ier2)
    if (ier1 .ne. 0 .or. ier2 .ne. 0) call u2mess('F', 'POSTRELE_66')
!
    call getvtx('ACTION', 'NOM_CMP', iocc=ioc, nbval=0, nbret=nbc)
    if (nbc .ne. 0) then
        nbcmp = -nbc
        call wkvect('&&PREXEL.NOM_CMP', 'V V K8', nbcmp, jcmp)
        call getvtx('ACTION', 'NOM_CMP', iocc=ioc, nbval=nbcmp, vect=zk8(jcmp),&
                    nbret=ibid)
    else
        nbcmp = ncmp
    endif
!
    imamax = 0
    iptmax = 0
    ispmax = 0
    valmax = -r8vide()
    imamin = 0
    iptmin = 0
    ispmin = 0
    valmin = r8vide()
!
    imaaax = 0
    ipamax = 0
    isamax = 0
    vaamax = -r8vide()
    imaain = 0
    ipamin = 0
    isamin = 0
    vaamin = r8vide()
!
    do 100 i100 = 1, nbcmp
        if (nbc .ne. 0) then
            nocmp = zk8(jcmp+i100-1)
            icp = indik8( zk8(jcesc), nocmp, 1, ncmp )
            if (icp .eq. 0) goto 100
        else
            icp = i100
            nocmp = zk8(jcesc+i100-1)
        endif
!
        do 110 i110 = 1, nbmail
            if (nbm .ne. 0) then
                imai = zi(idmail+i110-1)
            else
                imai = i110
            endif
            nbpt = zi(jcesd-1+5+4*(imai-1)+1)
            nbsp = zi(jcesd-1+5+4*(imai-1)+2)
            call jeveuo(jexnum(ma//'.CONNEX', imai), 'L', jcone)
            do 120,ipt = 1,nbpt
            do 130,isp = 1,nbsp
            call cesexi('C', jcesd, jcesl, imai, ipt,&
                        isp, icp, iad)
            if (iad .gt. 0) then
                x = zr(jcesv-1+iad)
                if (x .gt. valmax) then
                    imamax = imai
                    iptmax = zi(jcone+ipt-1)
                    ispmax = isp
                    cmpmax = nocmp
                    valmax = x
                endif
                if (abs(x) .gt. vaamax) then
                    imaaax = imai
                    ipamax = zi(jcone+ipt-1)
                    isamax = isp
                    cmamax = nocmp
                    vaamax = abs(x)
                endif
                if (x .lt. valmin) then
                    imamin = imai
                    iptmin = zi(jcone+ipt-1)
                    ispmin = isp
                    cmpmin = nocmp
                    valmin = x
                endif
                if (abs(x) .lt. vaamin) then
                    imaain = imai
                    ipamin = zi(jcone+ipt-1)
                    isamin = isp
                    cmamin = nocmp
                    vaamin = abs(x)
                endif
            endif
130          continue
120          continue
110      continue
100  end do
!
    call jenuno(jexnum(ma//'.NOMMAI', imamax), mamax)
    call jenuno(jexnum(ma//'.NOMMAI', imamin), mamin)
    call jenuno(jexnum(ma//'.NOMNOE', iptmax), nomax)
    call jenuno(jexnum(ma//'.NOMNOE', iptmin), nomin)
!
    call jenuno(jexnum(ma//'.NOMMAI', imaaax), maamax)
    call jenuno(jexnum(ma//'.NOMMAI', imaain), maamin)
    call jenuno(jexnum(ma//'.NOMNOE', ipamax), noamax)
    call jenuno(jexnum(ma//'.NOMNOE', ipamin), noamin)
!
! --- MENAGE
    call detrsd('CHAM_ELEM_S', chams1)
    call jedetr(mesmai)
    call jedetr('&&PREXEL.NOM_CMP')
!
end subroutine
