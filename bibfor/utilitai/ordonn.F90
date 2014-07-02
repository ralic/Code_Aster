subroutine ordonn(nomfon, iret)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/foverf.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/ordon1.h"
#include "asterfort/ordon2.h"
#include "asterfort/ordonp.h"
#include "asterfort/utmess.h"
#include "asterfort/uttrif.h"
!
    character(len=19) :: nomfon
    integer :: iret
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
! ----------------------------------------------------------------------
! person_in_charge: mathieu.courtois at edf.fr
! ----------------------------------------------------------------------
!     1. DECLENCHE UNE ERREUR <F> SI LES ABSCISSES NE SONT PAS MONOTONES
!        AU SENS LARGE, APPEL DE FOVERF POUR CELA
!     2. ORDONNE LES POINTS PAR ABSCISSES CROISSANTES,
!        SI ELLES SONT DECROISSANTES
! ----------------------------------------------------------------------
! IN     : NOMFON : FONCTION A VERIFIER
!             (LES VALEURS SONT EN IN/OUT)
! IN     : IRET   : SI 1, ON S'ARRETE EN <F> SI ABSC. NON MONOTONES
!                   SI 0, ON FORCE LE TRI
! ----------------------------------------------------------------------
    integer :: ival
    integer :: nbpara
    integer :: nbval, nbpts, ier, i
    aster_logical :: isnap, inv
    character(len=1) :: codmes
    character(len=16) :: typfon
    character(len=24) :: chval
    real(kind=8), pointer :: para(:) => null()
    character(len=24), pointer :: prol(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    chval=nomfon//'.VALE'
    call jeveuo(nomfon//'.PROL', 'L', vk24=prol)
    typfon = prol(1)(1:16)
!
    isnap = .false.
    inv = .false.
    codmes = ' '
!     --------------------------------------------
    if (typfon .eq. 'CONSTANT') then
!        ON N'A RIEN A FAIRE
!     --------------------------------------------
    else if (typfon.eq.'INTERPRE') then
!        ON N'A RIEN A FAIRE
!     --------------------------------------------
    else if (typfon.eq.'FONCTION') then
        call jeveuo(chval, 'E', ival)
        call jelira(chval, 'LONUTI', nbval)
        nbpts=nbval/2
        if (nbpts .eq. 1) goto 999
        ier=0
        call foverf(zr(ival), nbpts, ier)
        if (ier .eq. 0) then
            if (iret .eq. 1) then
                codmes = 'F'
                goto 999
            else
                codmes = 'A'
                call uttrif(zr(ival), nbpts, typfon)
            endif
        else if (ier.eq.-2) then
            codmes = 'A'
            inv = .true.
            call ordon1(zr(ival), nbpts)
        else if (ier.eq.1 .or. ier.eq.-1) then
            codmes = 'F'
            goto 999
        endif
!     --------------------------------------------
    else if (typfon.eq.'FONCT_C') then
        call jeveuo(chval, 'E', ival)
        call jelira(chval, 'LONUTI', nbval)
        nbpts=nbval/3
        if (nbpts .eq. 1) goto 999
        ier=0
        call foverf(zr(ival), nbpts, ier)
        if (ier .eq. 0) then
            if (iret .eq. 1) then
                codmes = 'F'
                goto 999
            else
                codmes = 'A'
                call uttrif(zr(ival), nbpts, typfon)
            endif
        else if (ier.eq.-2) then
            codmes = 'A'
            inv = .true.
            call ordon2(zr(ival), nbpts)
        else if (ier.eq.1 .or. ier.eq.-1) then
            codmes = 'F'
            goto 999
        endif
!     --------------------------------------------
    else if (typfon.eq.'NAPPE') then
        isnap=.true.
        call jeveuo(nomfon//'.PARA', 'E', vr=para)
        call jelira(nomfon//'.PARA', 'LONUTI', nbpara)
        ier=0
        call foverf(para, nbpara, ier)
        if (ier .le. 0) then
!           LES PARAMETRES NE SONT PAS CROISSANTS (SENS LARGE)
            call ordonp(nomfon)
            call utmess('A', 'UTILITAI3_37')
        endif
!        VERIFIER CHAQUE FONCTION COMME CI-DESSUS
        typfon='FONCTION'
        do 100 i = 1, nbpara
            call jelira(jexnum(chval, i), 'LONMAX', nbval)
            call jeveuo(jexnum(chval, i), 'E', ival)
            nbpts=nbval/2
            if (nbpts .eq. 1) goto 99
            ier=0
            call foverf(zr(ival), nbpts, ier)
            if (ier .eq. 0) then
                if (iret .eq. 1) then
                    codmes = 'F'
                    goto 999
                else
                    codmes = 'A'
                    call uttrif(zr(ival), nbpts, typfon)
                endif
            else if (ier.eq.-2) then
                codmes = 'A'
                inv = .true.
                call ordon1(zr(ival), nbpts)
            else if (ier.eq.1 .or. ier.eq.-1) then
                codmes = 'F'
                goto 999
            endif
 99         continue
100     continue
!     --------------------------------------------
    else
        call utmess('F', 'UTILITAI3_38')
    endif
!
999 continue
!
    if (codmes .ne. ' ') then
        if (.not. isnap) then
            call utmess(codmes//'+', 'FONCT0_62', sk=nomfon)
        else
            call utmess(codmes//'+', 'FONCT0_63', sk=nomfon)
        endif
        if (codmes .eq. 'F') then
            call utmess(codmes, 'FONCT0_64')
        else if (codmes .eq. 'A' .and. .not. inv) then
            call utmess(codmes, 'FONCT0_65')
        else if (codmes .eq. 'A' .and. inv) then
            call utmess(codmes, 'FONCT0_66')
        endif
    endif
    call jedema()
end subroutine
