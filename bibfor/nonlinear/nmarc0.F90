subroutine nmarc0(result, modele, mate, carele, fonact,&
                  sdcrit, sddyna, sdpost, carcri, sdcriq,&
                  sdpilo, lisch2, numarc, time_curr)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndaram.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmarcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rssepa.h"
    character(len=8) :: result
    integer :: numarc
    integer :: fonact(*)
    real(kind=8) :: time_curr
    character(len=19) :: sddyna, sdpost, sdpilo
    character(len=19) :: lisch2, sdcrit
    character(len=24) :: modele, mate, carele, sdcriq, carcri
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - ARCHIVAGE)
!
! ARCHIVAGE DES PARAMETRES
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM UTILISATEUR DU CONCEPT RESULTAT
! IN  MODELE : NOM DU MODELE
! IN  MATE   : CHAMP DE MATERIAU
! IN  SDCRIT : VALEUR DES CRITERES DE CONVERGENCE
! IN  SDPILO : SD PILOTAGE
! IN  CARCRI : CARTE DES CRITERES DE CONVERGENCE LOCAUX
! IN  SDCRIQ : SD CRITERE QUALITE
! IN  COMPOR : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE
! IN  LISCH2 : NOM DE LA SD INFO CHARGE POUR STOCKAGE DANS LA SD
! IN  NUMARC : NUMERO D'ARCHIVAGE
! IN  INSTAN : VALEUR DE L'INSTANT
!
! ----------------------------------------------------------------------
!
    character(len=16) :: valk
    character(len=19) :: k19bla
    aster_logical :: lerrt, lthm, lflam, lstab, lpilo, ldyna
    aster_logical :: lvibr, lexge
    character(len=24) :: errthm, typsel, typpil
    real(kind=8) :: taberr(2), theta, valr, chcrit, freqr, coef, chstab, time_prev
    integer :: iret
    integer :: jinst, jerrt, jpara, jacces
    integer :: jacce2
    real(kind=8), pointer :: plir(:) => null()
    real(kind=8), pointer :: crtr(:) => null()
    character(len=24), pointer :: pltk(:) => null()
    character(len=16), pointer :: crde(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    k19bla = ' '
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE' )
    lexge = ndynlo(sddyna,'EXPL_GENE' )
    lflam = isfonc(fonact,'CRIT_STAB')
    lstab = isfonc(fonact,'DDL_STAB')
    lvibr = isfonc(fonact,'MODE_VIBR' )
    lerrt = isfonc(fonact,'ERRE_TEMPS_THM')
    lthm = isfonc(fonact,'THM' )
    lpilo = isfonc(fonact,'PILOTAGE' )
!
! --- ARCHIVAGE DE THETA EN THM
!
    if (lthm) then
        call jeveuo(carcri(1:19)//'.VALV', 'L', jpara)
        theta = zr(jpara+3)
        call rsadpa(result, 'E', 1, 'PARM_THETA', numarc,&
                    0, sjv=jinst)
        zr(jinst) = theta
    endif
!
! --- ARCHIVAGE DE LA CHARGE CRITIQUE DU MODE DE FLAMBEMENT
!
    if (lflam) then
        call nmarcp('FLAM', sdpost, k19bla, chcrit, iret)
        if (iret .ne. 0) then
            call rsadpa(result, 'E', 1, 'CHAR_CRIT', numarc,&
                        0, sjv=jacces)
            zr(jacces) = chcrit
        endif
    endif
!
! --- ARCHIVAGE DE LA CHARGE CRITIQUE DU MDOE DE STABILITE
!
    if (lstab) then
        call nmarcp('STAB', sdpost, k19bla, chstab, iret)
        if (iret .ne. 0) then
            call rsadpa(result, 'E', 1, 'CHAR_STAB', numarc,&
                        0, sjv=jacce2)
            zr(jacce2) = chstab
        endif
    endif
!
! --- ARCHIVAGE DE LA FREQUENCE DU MODE VIBRATOIRE
!
    if (lvibr) then
        ASSERT(ldyna)
        call nmarcp('VIBR', sdpost, k19bla, freqr, iret)
        if (iret .ne. 0) then
            call rsadpa(result, 'E', 1, 'FREQ', numarc,&
                        0, sjv=jacces)
            zr(jacces) = freqr
        endif
    endif
!
! --- ARCHIVAGE DE L'INSTANT
!
    call rsadpa(result, 'E', 1, 'INST', numarc,&
                0, sjv=jinst)
    zr(jinst) = time_curr
!
! --- ARCHIVAGE DE L'INSTANT PRECEDENT
!
    if (ldyna) then
        time_prev = ndynre(sddyna,'INST_PREC')
        call rsadpa(result, 'E', 1, 'INST_PREC', numarc,&
                    0, sjv=jinst)
        zr(jinst) = time_prev
    endif
!
! --- ARCHIVAGE DU MODELE, MATERIAU, CARA_ELEM ET DE LA SD CHARGE
!
    call rssepa(result, numarc, modele(1:8), mate(1:8), carele(1:8),&
                lisch2)
!
! --- ARCHIVAGE DES CRITERES DE CONVERGENCE
!
    call jeveuo(sdcrit//'.CRTR', 'L', vr=crtr)
    call jeveuo(sdcrit//'.CRDE', 'L', vk16=crde)
    valr = crtr(1)
    valk = crde(1)
    call rsadpa(result, 'E', 1, valk, numarc,&
                0, sjv=jpara)
    zi(jpara) = nint(valr)
    valr = crtr(5)
    valk = crde(5)
    call rsadpa(result, 'E', 1, valk, numarc,&
                0, sjv=jpara)
    zr(jpara) = valr
    valr = crtr(6)
    valk = crde(6)
    call rsadpa(result, 'E', 1, valk, numarc,&
                0, sjv=jpara)
    zr(jpara) = valr
!
! --- ARCHIVAGE DES INDICATEURS D'ERREUR EN TEMPS EN THM UNIQUEMENT
!
    if (lerrt) then
        errthm = sdcriq(1:19)//'.ERRT'
        call jeveuo(errthm, 'L', jerrt)
        taberr(1) = zr(jerrt-1+1)
        taberr(2) = zr(jerrt-1+2)
        call rsadpa(result, 'E', 1, 'ERRE_TPS_LOC', numarc,&
                    0, sjv=jinst)
        zr(jinst) = taberr(1)
        call rsadpa(result, 'E', 1, 'ERRE_TPS_GLOB', numarc,&
                    0, sjv=jinst)
        zr(jinst) = taberr(2)
    endif
!
! --- ARCHIVAGE DE COEF_MULT SI PILOTAGE
!
    if (lpilo) then
        call jeveuo(sdpilo(1:19)//'.PLTK', 'L', vk24=pltk)
        typpil = pltk(1)
        typsel = pltk(6)
        if ((typpil.eq.'LONG_ARC'.or.typpil.eq.'SAUT_LONG_ARC') .and. typsel .eq.&
            'ANGL_INCR_DEPL') then
            call jeveuo(sdpilo(1:19)//'.PLIR', 'L', vr=plir)
            coef = plir(1)
            call rsadpa(result, 'E', 1, 'COEF_MULT', numarc,&
                        0, sjv=jinst)
            zr(jinst) = coef
        endif
    endif
!
! --- ARCHIVAGE DEPL/VITE/ACCE GENERALISES
!
    if (lexge) then
        call ndaram(result, sddyna, numarc)
    endif
!
    call jedema()
end subroutine
