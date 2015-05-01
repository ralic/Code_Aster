subroutine rechth(temps, nval2, tbinth, tabthr, tempa,&
                  tempb)
!
    implicit none
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbextb.h"
#include "asterfort/tbexve.h"
#include "asterfort/utmess.h"
    integer :: nval2
    real(kind=8) :: temps, tempa, tempb
    character(len=8) :: tabthr
    character(len=19) :: tbinth
! ======================================================================
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : RECUPERATION DES TEMPERATURES AUX POINTES DE LA FISSURE ----
! ======================================================================
! IN  : TEMPS  : INSTANT DE CALCUL COURANT -----------------------------
! --- : TABTHR : TABLE DES CHAMPS THERMIQUES ---------------------------
! OUT : TEMPA  : TEMPERATURE EN POINTE A -------------------------------
! --- : TEMPB  : TEMPERATURE EN POINTE B -------------------------------
! ======================================================================
! ======================================================================
    integer :: jinsth, ibid, ith, jteth1, jteth2, notot, iret
    real(kind=8) :: lprec, temph1, temph2
    complex(kind=8) :: cbid
    character(len=8) :: lcrit, k8b
    character(len=19) :: tmpth1, tmpth2, defth1, defth2
    character(len=24) :: valk(2)
! ======================================================================
    call jemarq()
! ======================================================================
! --- INITIALISATION ---------------------------------------------------
! ======================================================================
    cbid=(0.d0,0.d0)
    lcrit = 'RELATIF'
    lprec = 1.0d-06
    tmpth1 = '&&RECHTH.TMPTH1'
    tmpth2 = '&&RECHTH.TMPTH2'
    defth1 = '&&RECHTH.DEFTH1'
    defth2 = '&&RECHTH.DEFTH2'
    call jeveuo(tbinth, 'L', jinsth)
! ======================================================================
! --- DETERMINATION DES INSTANTS THERMIQUES ----------------------------
! --- ENCADRANT L'INSTANT MECANIQUE ------------------------------------
! ======================================================================
    do 10 ith = 2, nval2
        if (temps .le. zr(jinsth+ith-1)) then
            temph1 = zr(jinsth+ith-2)
            temph2 = zr(jinsth+ith-1)
            goto 20
        endif
10  end do
20  continue
! ======================================================================
! --- RECUPERATION DES SOUS-TABLES ASSOCIEES A L'INSTANT COURANT -------
! ======================================================================
    call tbextb(tabthr, 'V', tmpth1, 1, 'INST',&
                'EQ', [ibid], [temph1], [cbid], k8b,&
                [lprec], lcrit, iret)
    if (iret .eq. 10) then
        valk(1) = 'INST'
        valk(2) = tabthr
        call utmess('F', 'UTILITAI7_1', nk=2, valk=valk)
    else if (iret .eq. 20) then
        valk(1) = tabthr
        valk(2) = 'INST'
        call utmess('F', 'UTILITAI7_3', nk=2, valk=valk)
    endif
    call tbextb(tabthr, 'V', tmpth2, 1, 'INST',&
                'EQ', [ibid], [temph2], [cbid], k8b,&
                [lprec], lcrit, iret)
    if (iret .eq. 10) then
        valk(1) = 'INST'
        valk(2) = tabthr
        call utmess('F', 'UTILITAI7_1', nk=2, valk=valk)
    else if (iret .eq. 20) then
        valk(1) = tabthr
        valk(2) = 'INST'
        call utmess('F', 'UTILITAI7_3', nk=2, valk=valk)
    endif
! ======================================================================
! --- RECUPERATION DE LA LISTE DE TEMPERATURE TEMPH1 -------------------
! ======================================================================
    call tbexve(tmpth1, 'TEMP', defth1, 'V', notot,&
                k8b)
! ======================================================================
! --- RECUPERATION DE LA LISTE DE TEMPERATURE TEMPH2 -------------------
! ======================================================================
    call tbexve(tmpth2, 'TEMP', defth2, 'V', ibid,&
                k8b)
! ======================================================================
! --- RECUPERATION DES VECTEURS ASSOCIES -------------------------------
! ======================================================================
    call jeveuo(defth1, 'L', jteth1)
    call jeveuo(defth2, 'L', jteth2)
! ======================================================================
! --- CALCUL DE LA TEMPERATURE EN POINTE A, A L'INSTANT TEMPS ----------
! ======================================================================
    tempa = zr(jteth1-1+1) + ( zr(jteth2-1+1) - zr(jteth1-1+1) ) / ( temph2 - temph1 ) * ( temps &
            &- temph1 )
! ======================================================================
! --- CALCUL DE LA TEMPERATURE EN POINTE B, A L'INSTANT TEMPS ----------
! ======================================================================
    tempb = zr(jteth1-1+notot) + ( zr(jteth2-1+notot) - zr(jteth1-1+notot) ) / ( temph2 - temph1 &
            &) * ( temps - temph1 )
! ======================================================================
! --- DESTRUCTION DES TABLES INUTILES ----------------------------------
! ======================================================================
    call detrsd('TABLE', tmpth1)
    call detrsd('TABLE', tmpth2)
    call jedetr(defth1)
    call jedetr(defth2)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
