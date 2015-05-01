subroutine rsinch(nomsd, nomch, acces, rval, chextr,&
                  proldr, prolga, istop, base, ier)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/barych.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxliis.h"
#include "asterfort/rsbary.h"
#include "asterfort/rsexch.h"
#include "asterfort/rslipa.h"
#include "asterfort/rsutro.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: istop, ier
    real(kind=8) :: rval
    character(len=*) :: nomsd, nomch, acces, chextr, proldr, prolga
    character(len=*) :: base
! ----------------------------------------------------------------------
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
! ======================================================================
!      INTERPOLATION D'UN CHAMP_19 A PARTIR D'1 SD RESULTAT-COMPOSE
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : NOMCH  : NOM SYMBOLIQUE DU CHAMP CHERCHE.
! IN  : ACCES  : NOM SYMBOLIQUE DE LA VARIABLE D'ACCES.
! IN  : RVAL   : VALEUR REEL DE LA VARIABLE D'ACCES.
! IN  : CHEXTR : NOM DU CHAMP A CREER. (S'IL EXISTE, ON LE DETRUIT).
! IN  : PROLDR : 'CONSTANT', 'LINEAIRE', OU 'EXCLU'
!                          (PROLONGEMENT VOULU A DROITE)
! IN  : PROLGA : 'CONSTANT', 'LINEAIRE', OU 'EXCLU'
!                          (PROLONGEMENT VOULU A GAUCHE)
! IN  : ISTOP  :  EN CAS D'ERREUR D'INTERPOLATION:
!                 0  --> N'ECRIT PAS DE MESSAGE , NE FAIT PAS STOP.
!                 1  --> ECRIT MESSAGES , NE FAIT PAS STOP.
!                 2  --> ECRIT MESSAGES , FAIT STOP.
! IN  : BASE   : BASE DU CHAMP CREE
!
! OUT : IER    : CODE_RETOUR :
!                LE CHAMP EST CALCULE:
!                00 --> LE CHAMP EST INTERPOLE ENTRE 2 VALEURS.
!                01 --> LE CHAMP EST PROLONGE A GAUCHE.
!                02 --> LE CHAMP EST PROLONGE A DROITE.
!
!                LE CHAMP N'EST PAS CALCULE:
!                10 --> IL N'EXISTE AUCUN CHAMP POUR L'INTERPOLATION.
!                11 --> LE PROLONGEMENT A GAUCHE INTERDIT.
!                12 --> SI PROLONGEMENT A DROITE INTERDIT.
!                20 --> LA VARIABLE D'ACCES EST ILLICITE.
! ----------------------------------------------------------------------
    real(kind=8) :: r1, r2, rbase
    real(kind=8) :: valr
    integer :: l1, l2
    character(len=1) :: stp, base2
    character(len=4) :: type, tysca
    character(len=8) :: nomobj, k8debu, k8maxi, k8ent
    character(len=19) :: ch1, ch2
    character(len=8) :: prold2, prolg2
    character(len=19) :: noms2
    character(len=16) :: acce2, nomc2
    character(len=19) :: chext2
    character(len=24) :: valk(3)
!
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, iacces, iaobj, iatach
    integer :: iatava, ibid, idebu, ier1, ier2, ierr1, ierr2
    integer :: iloty, imaxi, inomch, ip1, ip2, iposit, nbord2
    integer :: nbordr
    aster_logical, pointer :: lexi(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    acce2 = acces
    noms2 = nomsd
    nomc2 = nomch
    prold2 = proldr
    prolg2 = prolga
    chext2 = chextr
    base2 = base
!
!     -- VERIFICATION DE LA VARIABLE D'ACCES:
!     ---------------------------------------
!
    call jenonu(jexnom(noms2//'.NOVA', acce2), iacces)
    if (iacces .eq. 0) then
        ier = 20
        goto 998
    endif
!
    call jeveuo(jexnum(noms2//'.TAVA', iacces), 'L', iatava)
    nomobj = zk8(iatava-1+1)
    k8debu = zk8(iatava-1+2)
    call lxliis(k8debu, idebu, ier1)
    k8maxi = zk8(iatava-1+3)
    call lxliis(k8maxi, imaxi, ier2)
    ASSERT((abs(ier1)+abs(ier2)).eq.0)
    if (ier2 .ne. 0) then
        ier = 20
        goto 998
    endif
!
    call jelira(noms2//nomobj, 'TYPE', cval=type)
    call jelira(noms2//nomobj, 'LTYP', iloty)
    call codent(iloty, 'G', k8ent)
    tysca = type(1:1)//k8ent(1:3)
    if (tysca .ne. 'R8  ') then
        ier = 20
        goto 998
    endif
!
    call rslipa(noms2, acces, '&&RSINCH.LIR8', iaobj, nbordr)
!
!
!     -- VERIFICATION DU NOM DE CHAMP:
!     --------------------------------
    call jenonu(jexnom(noms2//'.DESC', nomc2), inomch)
    if (inomch .eq. 0) then
        ier = 21
        goto 998
    endif
!
!     -- ON INTERPOLE :
!     -----------------
!
!     -- ON REPERE QUELS SONT LES CHAMPS EXISTANT REELLEMENT:
    AS_ALLOCATE(vl=lexi, size=nbordr)
    call jenonu(jexnom(noms2//'.DESC', nomc2), ibid)
    call jeveuo(jexnum(noms2//'.TACH', ibid), 'L', iatach)
    nbord2=0
    do i = 1, nbordr
        if (zk24(iatach-1+i) (1:1) .eq. ' ') then
            lexi(i) = .false.
        else
            lexi(i) = .true.
            nbord2=nbord2+1
        endif
    end do
!
    call rsbary(zr(iaobj), nbordr, .false._1, lexi, rval,&
                i1, i2, iposit)
    if (iposit .eq. -2) then
        ier = 10
        goto 998
    endif
    call rsutro(nomsd, i1, ip1, ierr1)
    call rsutro(nomsd, i2, ip2, ierr2)
    ASSERT(ierr1+ierr2.le.0)
    rbase = zr(iaobj-1+i2) - zr(iaobj-1+i1)
!
    call rsexch(' ', nomsd, nomc2, ip1, ch1,&
                l1)
    call rsexch(' ', nomsd, nomc2, ip2, ch2,&
                l2)
    ASSERT(l1+l2.le.0)
!
!     -- SI LES 2 POINTS IP1 ET IP2 ONT MEME ABSCISSE, ON RECOPIE
!     -- SIMPLEMENT LE CHAMP(IP1) DANS CHEXT2.
    if (rbase .eq. 0.0d0) then
        if (iposit .eq. 0) then
            call copisd('CHAMP_GD', base2, ch1(1:19), chext2(1:19))
            ier = 0
            goto 998
        else
!         -- CAS DE L'EVOL_XXX QUI N'A QU'UN SEUL INSTANT :
!            ON AUTORISE LE PROLONGEMENT CONSTANT ET ON ALARME
            ASSERT(nbord2.eq.1)
            r1=1.d0
            r2=0.d0
            if ((prold2.ne.'CONSTANT') .or. (prolg2.ne.'CONSTANT')) then
                prold2 = 'CONSTANT'
                prolg2 = 'CONSTANT'
                if (rval .ne. zr(iaobj-1+i1)) then
                    call utmess('A', 'CALCULEL_28', sk=nomsd)
                endif
            endif
        endif
    else
        r1 = (zr(iaobj-1+i2)-rval)/rbase
        r2 = (rval-zr(iaobj-1+i1))/rbase
    endif
!
!     -- INTERPOLATION VRAIE:
!     -----------------------
    if (iposit .eq. 0) then
        call barych(ch1, ch2, r1, r2, chext2,&
                    base2)
        ier = 0
        goto 998
!
!        -- PROLONGEMENT A GAUCHE:
!        -------------------------
    else if (iposit.eq.-1) then
        ier = 1
        if (prolg2(1:8) .eq. 'LINEAIRE') then
            call barych(ch1, ch2, r1, r2, chext2,&
                        base2)
        else if (prolg2(1:8).eq.'CONSTANT') then
            call copisd('CHAMP_GD', base2, ch1(1:19), chext2(1:19))
        else
            ier = 11
        endif
        goto 998
!
!        -- PROLONGEMENT A DROITE:
!        -------------------------
    else if (iposit.eq.1) then
        ier = 2
        if (prold2(1:8) .eq. 'LINEAIRE') then
            call barych(ch1, ch2, r1, r2, chext2,&
                        base2)
        else if (prold2(1:8).eq.'CONSTANT') then
            call copisd('CHAMP_GD', base2, ch2(1:19), chext2(1:19))
        else
            ier = 12
        endif
        goto 998
!
    endif
998 continue
!
!     -- MESSAGES, ARRET?
!     -------------------
    if (istop .eq. 0) then
        goto 999
    else if (istop.eq.1) then
        stp = 'A'
    else if (istop.eq.2) then
        stp = 'F'
    endif
!
!
    if (ier .eq. 11) then
        call utmess(stp//'+', 'UTILITAI8_32')
    else if (ier.eq.12) then
        call utmess(stp//'+', 'UTILITAI8_33')
    else if (ier.eq.10) then
        valk (1)= nomc2
        call utmess(stp//'+', 'UTILITAI8_34', sk=valk(1))
    else if (ier.eq.20) then
        valk (1)= acce2
        call utmess(stp//'+', 'UTILITAI8_35', sk=valk(1))
    else if (ier.eq.21) then
        valk (1)= nomc2
        call utmess(stp//'+', 'UTILITAI8_36', sk=valk(1))
    endif
!
    if (ier .ge. 10) then
        valk (1) = nomsd
        valk (2) = nomch
        valk (3) = acces
        valr = rval
        call utmess(stp, 'UTILITAI8_37', nk=3, valk=valk, sr=valr)
    endif
!
!
999 continue
    call jedetr('&&RSINCH.LIR8')
    AS_DEALLOCATE(vl=lexi)
!
    call jedema()
end subroutine
