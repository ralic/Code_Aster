subroutine extdch(typext, valinc, nocham, nocmp, dval)
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/barych.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsred.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmchex.h"
    real(kind=8) :: dval
    character(len=8) :: typext
    character(len=16) :: nocham, nocmp
    character(len=19) :: valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
!    CALCUL D'UN EXTREMUM (MIN, MAX, EN VALEUR ABSOLUE OU NON)
!    DE L'INCREMENT D'UN CHAMP ('DEPL', 'SIEL_ELGA', OU 'VARI_ELGA')
!
! ----------------------------------------------------------------------
!
!
! IN  TYPEXT : TYPE D'EXTREMUM : MIN(), MAX(), MIN(ABS()), MAX(ABS())
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  NOCHAM : NOM DU CHAMP
! IN  NOCMP  : NOM DE LA COMPOSANTE
! OUT DVAL   : EXTREMUM
!
!
!
!
!
    integer :: jcnsv, jcnsl, jcnsd
    integer :: nbno, ino, ib
    integer :: jcesd, jcesl, jcesv
    integer :: nbma, ima, ipt, isp, icmp, nbpt, nbsp, nbcmp, iad
    real(kind=8) :: valeur
    character(len=6) :: nompro
    character(len=16) :: typch
    character(len=19) :: dch, dchs, chplu, chmoi
    parameter   (nompro = 'EXTDCH')
!
!      REAL*8  TMP
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call assert(typext .eq. 'MIN' .or. typext&
                .eq. 'MAX' .or. typext .eq.&
                'MIN_ABS' .or. typext .eq. 'MAX_ABS'&
                .or. typext .eq. 'MIN_VAR')
!
    call assert(nocham .eq. 'VARI_ELGA' .or. nocham .eq. 'SIEF_ELGA' .or. nocham .eq. 'DEPL')
!
!     DECOMPACTION DES VARIABLES CHAPEAUX
    if (nocham .eq. 'VARI_ELGA') then
        call nmchex(valinc, 'VALINC', 'VARMOI', chmoi)
        call nmchex(valinc, 'VALINC', 'VARPLU', chplu)
        typch = 'CHAM_ELGA'
    else if (nocham.eq.'SIEF_ELGA') then
        call nmchex(valinc, 'VALINC', 'SIGMOI', chmoi)
        call nmchex(valinc, 'VALINC', 'SIGPLU', chplu)
        typch = 'CHAM_ELGA'
    else if (nocham.eq.'DEPL') then
        call nmchex(valinc, 'VALINC', 'DEPMOI', chmoi)
        call nmchex(valinc, 'VALINC', 'DEPPLU', chplu)
        typch = 'CHAM_NO'
    endif
!
!     INITIALISATION DE L'EXTREMUM
    if (typext .eq. 'MIN' .or. typext .eq. 'MIN_ABS' .or. typext .eq. 'MIN_VAR') dval = &
                                                                                 r8maem()
!
    if (typext .eq. 'MAX') dval = r8miem()
    if (typext .eq. 'MAX_ABS') dval = 0.d0
!
!     CALCUL DE L'INCREMENT DU CHAMP
!     DCH = CHPLU - CHMOI
    dch = '&&'//nompro//'.DELTACH   '
    dchs = '&&'//nompro//'.DELTACHS  '
    call barych(chplu, chmoi, 1.d0, -1.d0, dch,&
                'V')
!
!     ON APPELLERA MEMAX QUAND CETTE ROUTINE SERA MIEUX PROGRAMMEE
    if (typch(1:7) .eq. 'CHAM_EL') then
!
        call celces(dch, 'V', dchs)
        call cesred(dchs, 0, ib, 1, nocmp,&
                    'V', dchs)
        call jeveuo(dchs//'.CESD', 'L', jcesd)
        call jeveuo(dchs//'.CESL', 'L', jcesl)
        call jeveuo(dchs//'.CESV', 'L', jcesv)
        nbma = zi(jcesd-1+1)
        do 40,ima = 1,nbma
        nbpt = zi(jcesd-1+5+4*(ima-1)+1)
        nbsp = zi(jcesd-1+5+4*(ima-1)+2)
        nbcmp = zi(jcesd-1+5+4*(ima-1)+3)
        do 30,ipt = 1,nbpt
        do 20,isp = 1,nbsp
        do 10,icmp = 1,nbcmp
        call cesexi('C', jcesd, jcesl, ima, ipt,&
                    isp, icmp, iad)
        if (iad .gt. 0) then
            valeur = zr(jcesv-1+iad)
!
!
            if (typext(5:7) .eq. 'ABS') valeur = abs( valeur)
            if (typext(5:7) .eq. 'VAR') then
                if (abs(valeur) .gt. r8prem()) then
                    valeur =1.d-3/abs(valeur)
!                      DVAL = MIN(DVAL,TMP)
                else
                    valeur=r8maem()
                endif
!
            endif
            if (typext(1:3) .eq. 'MIN') then
                dval = min(dval,valeur)
!
            else if (typext(1:3).eq.'MAX') then
                dval = max(dval,valeur)
!
            endif
!
        endif
10      continue
20      continue
30      continue
40      continue
!
    else if (typch.eq.'CHAM_NO') then
!
        call cnocns(dch, 'V', dchs)
        call cnsred(dchs, 0, ib, 1, nocmp,&
                    'V', dchs)
        call jeveuo(dchs//'.CNSV', 'L', jcnsv)
        call jeveuo(dchs//'.CNSL', 'L', jcnsl)
        call jeveuo(dchs//'.CNSD', 'L', jcnsd)
        nbno = zi(jcnsd-1+1)
        do 60,ino=1,nbno
        if (zl(jcnsl-1+ino)) then
            valeur = abs(zr(jcnsv-1+ino))
            if (typext(5:7) .eq. 'ABS') valeur = abs(valeur)
            if (typext(1:3) .eq. 'MIN') then
                dval = min(dval,valeur)
            else if (typext(1:3).eq.'MAX') then
                dval = max(dval,valeur)
            endif
        endif
60      continue
!
    endif
!
    call jedema()
end subroutine
