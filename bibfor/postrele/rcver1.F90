subroutine rcver1(phenoz, tablz, tably)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbexv1.h"
#include "asterfort/tbexve.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: tablz, tably, phenoz
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
!     ------------------------------------------------------------------
!      OPERATEUR POST_RCCM
!
!     IN  TABLZ: TABLE DE REFERENCE
!     IN  TABLY: TABLE A COMPARER A TABLZ
!     IN  PHENOZ : PHENOMENE (THERMIQUE OU MECANIQUE)
!
!      VERIFICATIONS SUR LES 2 TABLES:
!      - MEMES COORDONNEES  (CAS "EVOLUTION" ET "UNITAIRE"):
!          -> SI 'MECANIQUE': MEME NOMBRE DE NOEUDS ET MEMES COORDONNEES
!          -> SI 'THERMIQUE': MEMES NOEUDS EXTREMITES
!      - MEME NOMBRE DE LIGAMENTS, MEMES INSTANTS (CAS "EVOLUTION")
!     ------------------------------------------------------------------
    integer :: n1, nbno1, nbno2, jordo1, jordo2, nbint1, nbint2, i, j
    integer :: vali(2), nbins1, nbins2, jinst1, jinst2, ibid
    real(kind=8) :: valr(2), eps, r8b, v1, v2
    parameter(eps=1.d-6)
    character(len=8) :: k8b, valk(3), tyva
    character(len=16) :: tabref, tabcom, typmec, valek(5), phenom
    character(len=24) :: ordo1, ordo2, intit1, intit2, inst1, inst2
    logical :: exi1, exi2, exi3, exist
!
    call jemarq()
!
    valek(1) = 'COOR_X          '
    valek(2) = 'COOR_Y          '
    valek(3) = 'COOR_Z          '
    valek(4) = 'INTITULE        '
    valek(5) = 'INST            '
!
    ordo1 = '&&RCVER1_TABREF_ORDO'
    ordo2 = '&&RCVER1_TABCOM_ORDO'
    intit1 = '&&RCVER1_TABREF_INTITU'
    intit2 = '&&RCVER1_TABCOM_INTITU'
    inst1 = '&&RCVER1_TABREF_INST'
    inst2 = '&&RCVER1_TABCOM_INST'
!
    tabref = tablz
    tabcom = tably
    phenom = phenoz
!
! --- VERIFICATION DES VALEURS DES COORDONNEES
!     ----------------------------------------
!
!     VERIFICATION DE LA PRESENCE DES COORDONNEES DANS LA TABLE 'TABCOM'
    call tbexip(tabcom, valek(1), exi1, k8b)
    call tbexip(tabcom, valek(2), exi2, k8b)
    call tbexip(tabcom, valek(3), exi3, k8b)
    if (.not.exi1 .and. .not.exi2 .and. .not.exi3) then
        call u2mesk('I', 'POSTRCCM_39', 1, tabcom)
        goto 999
    endif
!
    do 10 j = 1, 3
        call tbexve(tabref, valek(j), ordo1, 'V', nbno1,&
                    k8b)
        call jeveuo(ordo1, 'L', jordo1)
        call tbexve(tabcom, valek(j), ordo2, 'V', nbno2,&
                    k8b)
        call jeveuo(ordo2, 'L', jordo2)
!       ON COMPARE LES COORDONNEES DE TOUS LES NOEUDS
        if (phenom .eq. 'MECANIQUE') then
            ASSERT(nbno1.eq.nbno2)
            do 20 i = 1, nbno1
                if (abs(zr(jordo1+i-1)-zr(jordo2+i-1)) .gt. eps) then
                    valk(1)=tabref(1:8)
                    valk(2)=tabcom(1:8)
                    valk(3)=valek(j)(1:8)
                    valr(1)=zr(jordo1+i-1)
                    valr(2)=zr(jordo2+i-1)
                    call u2mesg('F', 'POSTRCCM_41', 3, valk, 0,&
                                ibid, 2, valr)
                endif
20          continue
!       ON COMPARE LES COORDONNEES DES NOEUDS EXTREMITES
!       (CAR ON N'A PAS FORCEMENT NBNO1 = NBNO2)
        else if (phenom.eq.'THERMIQUE') then
            do 40 i = 1, 2
                v1=zr(jordo1+(nbno1-1)*(i-1))
                v2=zr(jordo2+(nbno2-1)*(i-1))
                if (abs(v1-v2) .gt. eps) then
                    valk(1)=tabref(1:8)
                    valk(2)=tabcom(1:8)
                    valk(3)=valek(j)(1:8)
                    valr(1)=v1
                    valr(2)=v2
                    call u2mesg('F', 'POSTRCCM_41', 3, valk, 0,&
                                ibid, 2, valr)
                endif
40          continue
        endif
        call jedetr(ordo1)
        call jedetr(ordo2)
10  end do
!
! --- VERIFICATION DU NOMBRE DE LIGAMENTS
!     -----------------------------------
    call getvtx(' ', 'TYPE_RESU_MECA', scal=typmec, nbret=n1)
    call tbexip(tabcom, valek(4), exist, k8b)
    ASSERT(exist)
    call tbexv1(tabcom, valek(4), intit2, 'V', nbint2,&
                tyva)
!     CAS UNITAIRE: 1 SEUL LIGAMENT
    if (typmec .eq. 'UNITAIRE' .and. nbint2 .ne. 1) then
        call u2mesg('F', 'POSTRCCM_40', 1, tabcom, 1,&
                    nbint2, 0, r8b)
!     CAS EVOLUTION: MEME NOMBRE DE LIGAMENTS
    else if (typmec.eq.'EVOLUTION') then
        call tbexv1(tabref, valek(4), intit1, 'V', nbint1,&
                    tyva)
        if (nbint1 .ne. nbint2) then
            valk(1)=tabref(1:8)
            valk(2)=tabcom(1:8)
            vali(1)=nbint1
            vali(2)=nbint2
            call u2mesg('F', 'POSTRCCM_42', 2, valk, 2,&
                        vali, 0, r8b)
        endif
        call jedetr(intit1)
    endif
    call jedetr(intit2)
!
! --- VERIFICATION DE LA VALEUR DES INSTANTS (CAS 'EVOLUTION')
!     --------------------------------------------------------
    if (typmec .eq. 'EVOLUTION') then
        call tbexip(tabcom, valek(5), exist, k8b)
        ASSERT(exist)
        call tbexve(tabref, valek(5), inst1, 'V', nbins1,&
                    k8b)
        call jeveuo(inst1, 'L', jinst1)
        call tbexve(tabcom, valek(5), inst2, 'V', nbins2,&
                    k8b)
        call jeveuo(inst2, 'L', jinst2)
        ASSERT(nbins1.eq.nbins2)
        do 30 i = 1, nbins1
            if (abs(zr(jinst1+i-1)-zr(jinst2+i-1)) .gt. eps) then
                valk(1)=tabref(1:8)
                valk(2)=tabcom(1:8)
                valr(1)=zr(jinst1+i-1)
                valr(2)=zr(jinst2+i-1)
                call u2mesg('F', 'POSTRCCM_43', 2, valk, 0,&
                            ibid, 2, valr)
            endif
30      continue
        call jedetr(inst1)
        call jedetr(inst2)
    endif
!
!
999  continue
!
    call jedema()
!
end subroutine
