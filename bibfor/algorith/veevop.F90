subroutine veevop(nomo, fnocal, instan, lisch2)
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
!
    implicit     none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lischk.h"
#include "asterfort/liscrs.h"
#include "asterfort/lisdef.h"
#include "asterfort/lisnnl.h"
#include "asterfort/lissav.h"
#include "asterfort/rsinch.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
    character(len=8) :: fnocal, nomo
    real(kind=8) :: instan
    character(len=19) :: lisch2
!
! ----------------------------------------------------------------------
!
! CALCUL EVOL_CHAR
!
! PREPARATION LISTE DES CHARGES EFFECTIVES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO   : NOM DU MODELE
! IN  FNOCAL : NOM DE LA SD EVOL_CHAR
! IN  INSTAN : INSTANT COURANT
! OUT LISCH2 : SD LISTE DES CHARGES
!
! ----------------------------------------------------------------------
!
    integer :: nmxech
    parameter    (nmxech=8)
    character(len=8) :: evolch(nmxech)
!
    character(len=8) :: chfnoe
    integer :: ier, ichar, nbchar
    character(len=24) :: valk(4)
    real(kind=8) :: valr
    integer :: vali
    logical :: l2d, l3d
    character(len=8) :: charge
    character(len=16) :: typapp, typfct
    integer :: codcha
    character(len=8) :: typech, nomfct
    character(len=13) :: prefob
    real(kind=8) :: r8bid
    integer :: ibid
    character(len=8) :: typsd, k8bid
    integer :: nbcham
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nbchar = 0
    l2d = .false.
    l3d = .false.
!
! --- QUELQUES VERIFICATIONS
!
    call gettco(fnocal, typsd)
    if (typsd .ne. 'EVOL_CHAR') then
        call u2mesk('F', 'ALGORITH7_15', 1, fnocal)
    endif
    call dismoi('F', 'NB_CHAMP_UTI', fnocal, 'RESULTAT', nbcham,&
                k8bid, ier)
    if (nbcham .le. 0) then
        call u2mesk('F', 'ALGORITH7_16', 1, fnocal)
    endif
!
! --- EFFORTS VOLUMIQUES
!
    call rsinch(fnocal, 'FVOL_3D', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        nbchar = nbchar + 1
        evolch(nbchar) = chfnoe
        l3d = .true.
        goto 10
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        valk(1) = fnocal
        valr = instan
        vali = ier
        call u2mesg('F', 'ALGORITH13_56', 1, valk, 1,&
                    vali, 1, valr)
    endif
!
    call rsinch(fnocal, 'FVOL_2D', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        nbchar = nbchar + 1
        evolch(nbchar) = chfnoe
        l2d = .true.
        goto 10
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        valk(1) = fnocal
        valr = instan
        vali = ier
        call u2mesg('F', 'ALGORITH13_57', 1, valk, 1,&
                    vali, 1, valr)
    endif
!
! --- EFFORTS SURFACIQUES
!
10  continue
!
    call rsinch(fnocal, 'FSUR_3D', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        if (l2d) then
            valk(1) = fnocal
            valr = instan
            call u2mesg('F', 'ALGORITH13_58', 1, valk, 0,&
                        0, 1, valr)
        endif
        nbchar = nbchar + 1
        evolch(nbchar) = chfnoe
        goto 20
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        valk(1) = fnocal
        valr = instan
        vali = ier
        call u2mesg('F', 'ALGORITH13_59', 1, valk, 1,&
                    vali, 1, valr)
    endif
!
    call rsinch(fnocal, 'FSUR_2D', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        if (l3d) then
            valk(1) = fnocal
            valr = instan
            call u2mesg('F', 'ALGORITH13_60', 1, valk, 0,&
                        0, 1, valr)
        endif
        nbchar = nbchar + 1
        evolch(nbchar) = chfnoe
        goto 20
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        valk(1) = fnocal
        valr = instan
        vali = ier
        call u2mesg('F', 'ALGORITH13_61', 1, valk, 1,&
                    vali, 1, valr)
    endif
!
! --- PRESSIONS
!
20  continue
!
    call rsinch(fnocal, 'PRES', 'INST', instan, chfnoe,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        nbchar = nbchar + 1
        evolch(nbchar) = chfnoe
    else if (ier.eq.11 .or. ier.eq.12) then
        valk(1) = fnocal
        valk(2) = ' '
        valk(3) = ' '
        valk(4) = ' '
        valr = instan
        call u2mesg('F', 'ALGORITH13_62', 4, valk, 0,&
                    0, 1, valr)
    else if (ier.eq.20) then
        valk(1) = fnocal
        valk(2) = ' '
        valr = instan
        vali = ier
        call u2mesg('F', 'ALGORITH13_63', 2, valk, 1,&
                    vali, 1, valr)
    endif
!
    call assert(nbchar.le.nmxech)
!
! --- CREATION SD LISTE DES CHARGES
!
    call liscrs(lisch2, nbchar, 'V')
!
    do 100 ichar = 1, nbchar
!
! ----- LECTURE NOM DE LA CHARGE (PROVENANT DE AFFE_CHAR_*)
!
        charge = evolch(ichar)
!
! ----- PREFIXE DE L'OBJET DE LA CHARGE
!
        call lisnnl('MECANIQUE', charge, prefob)
!
! ----- GENRE DE LA CHARGE
!
        call lisdef('IDGE', prefob, ibid, k8bid, codcha)
!
! ----- TYPE DE LA CHARGE
!
        call lisdef('TYPC', prefob, codcha, typech, ibid)
!
! ----- TYPE D'APPLICATION DE LA CHARGE
!
        typapp = 'FIXE_CSTE'
!
! ----- RECUPERATION FONCTION MULTIPLICATRICE
!
        typfct = ' '
!
! ----- SAUVEGARDE DES INFORMATIONS
!
        call lissav(lisch2, ichar, charge, typech, codcha,&
                    prefob, typapp, nomfct, typfct, r8bid,&
                    ibid)
!
100 continue
!
! --- VERIFICATION DE LA LISTE DES CHARGES
!
    call lischk(nomo, 'MECANIQUE', ' ', lisch2)
!
    call jedema()
end subroutine
