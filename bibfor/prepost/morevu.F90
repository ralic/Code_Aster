subroutine morevu(tabpus, dinst, nbsect, sect, voltub,&
                  volobs)
    implicit none
#include "jeveux.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbexv1.h"
#include "asterfort/tbliva.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
    integer :: nbsect
    real(kind=8) :: dinst, voltub(*), volobs(*), sect(*)
    character(len=*) :: tabpus
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     OPERATEUR  "MODI_OBSTACLE"
!     RECUPERATION DES VOLUMES USES DANS LA TABLE "POST_USURE"
!
! ----------------------------------------------------------------------
    character(len=24) :: valk
!
    integer :: ibid, nis, nbvpu, jinst, nbss2, jsect, i, iret
    integer :: vali
    real(kind=8) :: prec, votub(12), voobs(12), sec(12)
    real(kind=8) :: valr
    complex(kind=8) :: c16b
    character(len=8) :: k8b, crit
    character(len=19) :: nomta
    character(len=16) :: valek(2)
    integer :: iarg
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomta = tabpus
    prec = 1.d-06
    crit = 'RELATIF '
!
!     VERIFICATION DES PARAMETRES DE LA TABLE
    call tbexp2(nomta, 'INST')
    call tbexp2(nomta, 'SECTEUR')
    call tbexp2(nomta, 'V_USUR_TUBE_CUMU')
    call tbexp2(nomta, 'V_USUR_OBST_CUMU')
    call tbexp2(nomta, 'ANGLE_DEBUT')
!
    call getvr8(' ', 'INST', scal=dinst, nbret=nis)
    if (nis .eq. 0) then
        call tbexv1(nomta, 'INST', '&&MOREVU.INST', 'V', nbvpu,&
                    k8b)
        call jeveuo('&&MOREVU.INST', 'L', jinst)
        dinst = zr(jinst+nbvpu-1)
        call jedetr('&&MOREVU.INST')
    endif
!
    call tbexv1(nomta, 'SECTEUR', '&&MOREVU.SECT', 'V', nbvpu,&
                k8b)
    call jeveuo('&&MOREVU.SECT', 'L', jsect)
    nbsect = zi(jsect+nbvpu-1)
    if (nbsect .ne. 10 .and. nbsect .ne. 12) then
        call u2mess('F', 'PREPOST3_63')
    endif
    nbss2 = nbsect / 2
!
    valek(1) = 'INST'
    valek(2) = 'SECTEUR'
!
    do 10 i = 1, nbsect
!
        call tbliva(nomta, 2, valek, i, dinst,&
                    c16b, k8b, crit, prec, 'V_USUR_TUBE_CUMU',&
                    k8b, ibid, votub(i), c16b, k8b,&
                    iret)
!                   ----------------
        if (iret .ne. 0) then
            valr = dinst
            valk = 'V_USUR_TUBE_CUMU'
            vali = i
            call u2mesg('F', 'PREPOST5_54', 1, valk, 1,&
                        vali, 1, valr)
        endif
!
        call tbliva(nomta, 2, valek, i, dinst,&
                    c16b, k8b, crit, prec, 'V_USUR_OBST_CUMU',&
                    k8b, ibid, voobs(i), c16b, k8b,&
                    iret)
!                   ----------------
        if (iret .ne. 0) then
            valr = dinst
            valk = 'V_USUR_OBST_CUMU'
            vali = i
            call u2mesg('F', 'PREPOST5_54', 1, valk, 1,&
                        vali, 1, valr)
        endif
!
        call tbliva(nomta, 2, valek, i, dinst,&
                    c16b, k8b, crit, prec, 'ANGLE_DEBUT',&
                    k8b, ibid, sec(i), c16b, k8b,&
                    iret)
!                          -----------
        if (iret .ne. 0) then
            valr = dinst
            valk = 'ANGLE_DEBUT'
            vali = i
            call u2mesg('F', 'PREPOST5_54', 1, valk, 1,&
                        vali, 1, valr)
        endif
!
10  end do
!
! --- LES ANGLES ETAIENT COMPRIS ENTRE -180 ET +180 DEGREES
!     ON LES PASSE ENTRE 0 ET 360 DEGRES
!
    do 20 i = 1, nbsect
        if (i .le. nbss2) then
            sect(i) = sec(nbss2+i)
            voltub(i) = votub(nbss2+i)
            volobs(i) = voobs(nbss2+i)
        else
            sect(i) = sec(i-nbss2) + 360.d0
            voltub(i) = votub(i-nbss2)
            volobs(i) = voobs(i-nbss2)
        endif
20  end do
!
    call jedetr('&&MOREVU.SECT')
!
    call jedema()
end subroutine
