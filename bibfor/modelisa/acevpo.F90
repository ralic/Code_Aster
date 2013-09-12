subroutine acevpo(nbocc, nlm, nlg, ier)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/codent.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
    integer :: nbocc, nlm, nlg, ier
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     VERIFICATION DES MOTS CLES POUR L'ELEMENT POUTRE
! ----------------------------------------------------------------------
! IN  : NBOCC  : NOMBRE D'OCCURENCE
! OUT : NLM    : NOMBRE TOTAL DE MAILLE
! OUT : NLG    : NOMBRE TOTAL DE GROUPE DE MAILLE
! ----------------------------------------------------------------------
    real(kind=8) :: r8b
    logical :: bon
    character(len=8) :: k8b, nomu, cara(100), kioc
    character(len=16) :: sec, vsec, concep, cmd
    integer :: vali(3)
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ioc, nc, ncar, ng, nm, ns
    integer :: nsom, nv, nval, nvs
!-----------------------------------------------------------------------
    call jemarq()
    call getres(nomu, concep, cmd)
!
    nlm = 0
    nlg = 0
    do 100 ioc = 1, nbocc
        call codent(ioc, 'G', kioc)
        call getvtx('POUTRE', 'GROUP_MA', iocc=ioc, nbval=0, nbret=ng)
        call getvtx('POUTRE', 'MAILLE', iocc=ioc, nbval=0, nbret=nm)
        call getvtx('POUTRE', 'SECTION', iocc=ioc, scal=sec, nbret=ns)
        call getvtx('POUTRE', 'VARI_SECT', iocc=ioc, scal=vsec, nbret=nvs)
        call getvtx('POUTRE', 'CARA', iocc=ioc, nbval=0, nbret=nc)
        ncar = -nc
        call getvtx('POUTRE', 'CARA', iocc=ioc, nbval=ncar, vect=cara,&
                    nbret=nc)
        call getvr8('POUTRE', 'VALE', iocc=ioc, nbval=0, nbret=nv)
        nval = -nv
!
        if (nval .ne. ncar) then
            vali (1) = ioc
            vali (2) = ncar
            vali (3) = nval
            call u2mesg('E', 'MODELISA9_31', 0, ' ', 3,&
                        vali, 0, 0.d0)
            ier = ier + 1
        endif
!
        if (sec .eq. 'RECTANGLE') then
            if (vsec .eq. 'AFFINE') then
!
            endif
        else if (sec .eq. 'CERCLE') then
            if (vsec .eq. 'CONSTANT') then
                bon = .false.
                do 20 i = 1, ncar
                    if (cara(i) .eq. 'R') bon = .true.
20              continue
                if (.not. bon) then
                    call u2mesk('E', 'MODELISA_66', 1, kioc)
                    ier = ier + 1
                endif
            endif
        endif
!
! ---    GROUP_MA + GROUP_NO + NOEUD + MAILLE
        nsom = ng + nm
        if (nsom .eq. ng .or. nsom .eq. nm) then
            nlm = max(nlm,-nm)
            nlg = max(nlg,-ng)
        endif
!
100  end do
!
    call jedema()
end subroutine
