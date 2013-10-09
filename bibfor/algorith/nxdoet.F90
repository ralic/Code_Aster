subroutine nxdoet(modele, numedd, lreuse, lostat, sdieto,&
                  initpr, instin)
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
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdoin.h"
#include "asterfort/nmetl1.h"
#include "asterfort/nmetl2.h"
#include "asterfort/nmetnc.h"
#include "asterfort/ntetl3.h"
#include "asterfort/utmess.h"
    character(len=24) :: modele
    logical :: lostat, lreuse
    character(len=24) :: numedd, sdieto
    real(kind=8) :: instin
    integer :: initpr
!
! ----------------------------------------------------------------------
!
! ROUTINE THER_NON_LINE (INITIALISATION)
!
! SAISIE DES CHAMPS A L'ETAT INITIAL
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NUME_DDL
! IN  LREUSE : .TRUE. SI REUSE
! OUT LOSTAT : .TRUE. SI L'ON CALCULE UN CAS STATIONNAIRE
! OUT INITPR : TYPE D'INITIALISATION
!              -1 : PAS D'INITIALISATION. (VRAI STATIONNAIRE)
!               0 : CALCUL STATIONNAIRE
!               1 : VALEUR UNIFORME
!               2 : CHAMP AUX NOEUDS
!               3 : RESULTAT D'UN AUTRE CALCUL
! OUT INSTIN : INSTANT INITIAL
!                R8VIDE SI NON DEFINI
!
!
!
!
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: nbcham, zioch
    character(len=8) :: k8bid, calcri, result
    character(len=24) :: repsta, evol
    character(len=24) :: nomcha, nomchs
    integer :: icham
    character(len=16) :: motfac
    integer :: i, neq, nocc, numein
    integer :: jtemp
    real(kind=8) :: tempct
    integer :: ifm, niv
    logical :: evonol, leinit
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- INITIALISATIONS
!
    initpr = -2
    instin = r8vide()
    evonol = .false.
    lostat = .false.
    leinit = .false.
    motfac = 'ETAT_INIT'
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- ACCES SD IN ET OUT
!
    ioinfo = sdieto(1:19)//'.INFO'
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(ioinfo, 'L', jioinf)
    call jeveuo(iolcha, 'L', jiolch)
    zioch = zi(jioinf+4-1)
    nbcham = zi(jioinf+1-1)
!
! --- ON VERIFIE QUE LE MODELE SAIT CALCULER UNE RIGIDITE
!
    call dismoi('CALC_RIGI', modele, 'MODELE', repk=calcri)
    if (calcri .ne. 'OUI') then
        call utmess('F', 'CALCULEL2_65', sk=modele)
    endif
!
! --- PAS D'ETAT INITIAL EN PRESENCE D'UN CONCEPT REENTRANT
!
    call getfac(motfac, nocc)
    ASSERT(nocc.le.1)
    leinit = nocc.gt.0
    if (leinit) then
        if (niv .ge. 2) then
            write (ifm,*) '<THERNONLINE> LECTURE ETAT INITIAL'
        endif
    else
        if (lreuse) then
            call utmess('A', 'ETATINIT_1')
        else
            call utmess('I', 'ETATINIT_20')
        endif
    endif
!
! --- CONCEPT EVOL_THER DONNE DANS ETAT_INIT
!
    call getvid(motfac, 'EVOL_THER', iocc=1, scal=evol, nbret=nocc)
    ASSERT(nocc.le.1)
    evonol = nocc.gt.0
!
! --- INSTANT INITIAL
!
    if (niv .ge. 2) then
        write (ifm,*) '<THERNONLINE> ... INSTANT INITIAL'
    endif
    call nmdoin(evol, evonol, instin, numein)
    if (niv .ge. 2) then
        if (instin .eq. r8vide()) then
            write (ifm,*) '<THERNONLINE> ...... NON DEFINI PAR ETAT_INIT'
        else
            write (ifm,*) '<THERNONLINE> ...... VALEUR    : ',instin
            write (ifm,*) '<THERNONLINE> ...... NUME_ORDRE: ',numein
        endif
    endif
!
! --- PAS DE PRECISION --> C'EST UN CALCUL STATIONNAIRE
!
    call getfac(motfac, nocc)
    ASSERT(nocc.le.1)
    if (nocc .eq. 0) then
        lostat = .true.
        initpr = -1
        goto 99
    endif
!
! --- BOUCLE SUR LES CHAMPS A LIRE
!
    do icham = 1, nbcham
!
! ------- ETAT INITIAL DEFINI PAR UN CONCEPT DE TYPE EVOL_THER
!
        result = evol(1:8)
!
! ------- LECTURE DU CHAMP - ETAT_INIT/EVONOL
!
        if (evonol) then
            initpr = 3
            call nmetl1(result, numein, sdieto, icham)
        else
!
! --------- NOM DU CHAMP DANS SD RESULTAT
!
            nomchs = zk24(jiolch+zioch*(icham-1)+1-1)
!
! --------- NOM DU CHAMP DANS OPERATEUR
!
            call nmetnc(sdieto, icham, nomcha)
!
            if (nomchs .eq. 'TEMP') then
                call jeveuo(nomcha(1:19)//'.VALE', 'E', jtemp)
!
! ----------- TEMPERATURE INITIALE PAR UN CHAMP
!
                call getvid(motfac, 'CHAM_NO', iocc=1, scal=k8bid, nbret=nocc)
                if (nocc .eq. 1) then
                    initpr = 2
                    call nmetl2(motfac, sdieto, icham)
                endif
!
! ----------- TEMPERATURE INITIALE STATIONNAIRE
!
                call getvtx(motfac, 'STATIONNAIRE', iocc=1, scal=repsta, nbret=nocc)
                if (nocc .gt. 0) then
                    if (repsta(1:3) .eq. 'OUI') then
                        lostat = .true.
                        initpr = 0
                        zk24(jiolch+zioch*(icham-1)+4-1) =&
                        'STATIONNAIRE'
                    endif
                endif
!
! ----------- TEMPERATURE INITIALE UNIFORME
!
                call getvr8(motfac, 'VALE', iocc=1, scal=tempct, nbret=nocc)
                if (nocc .gt. 0) then
                    initpr = 1
                    do i = 1, neq
                        zr(jtemp+i-1) = tempct
                        zk24(jiolch+zioch*(icham-1)+4-1) = 'VALE'
                    end do
                endif
!
            else
                call nmetl2(motfac, sdieto, icham)
            endif
        endif
!
! ------- LECTURE DU CHAMP - VERIFICATIONS
!
        call ntetl3(result, sdieto, icham, tempct)
    end do
!
 99 continue
!
    call jedema()
!
end subroutine
