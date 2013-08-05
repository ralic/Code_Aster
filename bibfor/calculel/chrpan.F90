subroutine chrpan(modele, carte, chelem)
    implicit       none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterc/r8pi.h"
#include "asterfort/angvx.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
    character(len=*) :: modele, carte, chelem
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
! ======================================================================
!     COMMANDE MODI_REPERE :
!     SURCHARGE ALPHA ET BETA DANS LA CARTE '.CARORIE'
!     ------------------------------------------------------------------
! IN  : MODELE : MODELE
! IN  : CARTE  : CARTE A TRANSFORMER EN CHAM ELEM
! OUT : CHELEM : CHAM ELEM AVEC ANGLES EVENTUELLEMENT VARIABLES
!     ------------------------------------------------------------------
!
    integer :: ibid, ioc, n1, n2, na, nvec, iret, nrep, nbma, nbmail, jmail
    integer :: ialpha, ibeta, iad1, iad2, ima, numma, ncmax, icesk, icesl, icesv
    integer :: icesc, icesd, ie, nncp
    real(kind=8) :: ang(2), vect(3)
    logical :: ltout
    character(len=8) :: k8b, noma, motcls(2), typmcl(2)
    character(len=19) :: chelms
    character(len=24) :: mesmai, ligrmo
    integer :: iarg
! --- ------------------------------------------------------------------
    call getfac('AFFE', nrep)
    if (nrep .eq. 0) goto 9999
! --- ------------------------------------------------------------------
! --- PASSAGE PAR UN CHAM_ELEM_S
    k8b = ' '
    chelms = '&&CHRPAN.ELEM_S  '
    call carces(carte, 'ELEM', k8b, 'V', chelms,&
                'A', iret)
!
    call jeveuo(chelms//'.CESK', 'L', icesk)
    call jeveuo(chelms//'.CESC', 'L', icesc)
    call jeveuo(chelms//'.CESD', 'L', icesd)
    call jeveuo(chelms//'.CESL', 'E', icesl)
    call jeveuo(chelms//'.CESV', 'E', icesv)
!
    noma = zk8(icesk)
    nbmail = zi(icesd)
    ncmax = zi(icesd+1)
! --- ------------------------------------------------------------------
! --- INDICE DE 'ALPHA' ET 'BETA' DANS LA CARTE
    ialpha = indik8 ( zk8(icesc), 'ALPHA   ', 1, ncmax )
    ibeta = indik8 ( zk8(icesc), 'BETA    ', 1, ncmax )
    ASSERT(ialpha.eq.1.and.ibeta.eq.2)
!
    motcls(1) = 'GROUP_MA'
    motcls(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
    mesmai = '&&CHRPAN.MES_MAILLES'
!
! --- ------------------------------------------------------------------
    do 10 ioc = 1, nrep
        call getvtx('AFFE', 'MAILLE', ioc, iarg, 0,&
                    k8b, n1)
        call getvtx('AFFE', 'GROUP_MA', ioc, iarg, 0,&
                    k8b, n2)
        if (n1+n2 .eq. 0) then
            ltout = .true.
            nbma = nbmail
        else
            call reliem(' ', noma, 'NU_MAILLE', 'AFFE', ioc,&
                        2, motcls, typmcl, mesmai, nbma)
            if (nbma .ne. 0) call jeveuo(mesmai, 'L', jmail)
            ltout = .false.
        endif
!
        ang(1) = 0.d0
        ang(2) = 0.d0
        call getvr8('AFFE', 'ANGL_REP', ioc, iarg, 2,&
                    ang, na)
        call getvr8('AFFE', 'VECTEUR', ioc, iarg, 3,&
                    vect, nvec)
        if (nvec .ne. 0) then
            call angvx(vect, ang(1), ang(2))
            ang(1)= ang(1)*180.d0/r8pi()
            ang(2)= ang(2)*180.d0/r8pi()
        endif
!
        do 30 ima = 1, nbma
            if (ltout) then
                numma = ima
            else
                numma = zi(jmail+ima-1)
            endif
!
            call cesexi('C', icesd, icesl, numma, 1,&
                        1, ialpha, iad1)
            if (iad1 .gt. 0) then
                zr(icesv-1+iad1) = ang(1)
            else if (iad1 .lt. 0) then
                iad1 = -iad1
                zl(icesl-1+iad1) = .true.
                zr(icesv-1+iad1) = ang(1)
            endif
!
            call cesexi('C', icesd, icesl, numma, 1,&
                        1, ibeta, iad2)
            if (iad2 .gt. 0) then
                zr(icesv-1+iad2) = ang(2)
            else if (iad2 .lt. 0) then
                iad2 = -iad2
                zl(icesl-1+iad2) = .true.
                zr(icesv-1+iad2) = ang(2)
            endif
!
30      continue
!
        if (.not. ltout) call jedetr(mesmai)
!
10  end do
!
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ibid,&
                ligrmo, ie)
    call cescel(chelms, ligrmo, 'REPE_GENE', 'PANGREP', 'NON',&
                nncp, 'V', chelem, 'F', ibid)
!
    call detrsd('CHAM_ELEM_S', chelms)
!
9999  continue
end subroutine
