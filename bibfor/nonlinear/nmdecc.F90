subroutine nmdecc(nomlis, linfo, optdez, deltat, instam,&
                  ratio, typdec, nbrpas, deltac, dtmin,&
                  retdec)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: optdez
    character(len=24) :: nomlis
    character(len=4) :: typdec
    integer :: nbrpas, retdec
    real(kind=8) :: instam, deltat, ratio, dtmin, deltac
    logical(kind=1) :: linfo
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
!
! DECOUPE DU PAS DE TEMPS - REMPLISSAGE DE LA LISTE
!
! ----------------------------------------------------------------------
!
! UNIFORME
!  *---*---*---*---*---*---*---*---*
! PROGRESSIF
!  *-------*---*---*---*---*---*---*
! DEGRESSIF
!  *---*---*---*---*---*---*-------*
!
!
! IN  NOMLIS : NOM DE LA LISTE DES INSTANTS A AJOUTER
! IN  LINFO  : .TRUE. SI ON FAIT UN AFFICHAGE
! IN  OPTDEC : OPTION DE DECOUPE
!     'UNIFORME'   - DECOUPE REGULIERE ET UNIFORME
!     'PROGRESSIF' - DECOUPE EN DEUX ZONES, UN PAS LONG+ UNE SERIE
!                    DE PAS UNIFORMES
!     'DEGRESSIF'  - DECOUPE EN DEUX ZONES, UNE SERIE DE PAS
!                    UNIFORMES + UN PAS LONG
! IN  INSTAM : INSTANT INITIAL
! IN  DELTAT : INCREMENT DE TEMPS
! IN  RATIO  : RATIO DE LA PREMIERE DECOUPE SI NON-UNIFORME
! IN  TYPDEC : TYPE DE DECOUPE
!     'SUBD' - SUBDIVISION PAR UN NOMBRE DE PAS DONNE
!     'DELT' - SUBDIVISION PAR UN INCREMENT DONNE
! I/O DELTAC : INCREMENT DE TEMPS CIBLE
!               SI TYPDEC='SUBD' -> C'EST OUT
!               SI TYPDEC='DELT' -> C'EST IN
! I/O NBRPAS : NOMBRE DE PAS DE TEMPS
!               SI TYPDEC='SUBD' -> C'EST IN
!               SI TYPDEC='DELT' -> C'EST OUT
! OUT DTMIN  : INTERVALLE DE TEMPS MINIMAL SUR LA LISTE CREEE
! OUT RETDEC : CODE RETOUR DECOUPE
!     0 - ECHEC DE LA DECOUPE
!     1 - ON A DECOUPE
!     2 - PAS DE DECOUPE
!
!
!
!
    real(kind=8) :: inst, pasdt, premie, suivan, valr(3)
    integer :: ipas, jinst
    character(len=16) ::  optdec
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    dtmin = r8gaem()
    optdec = optdez
    retdec = 0
!
! --- CONSTRUCTION DONNEES MANQUANTES
!
    if (optdec .eq. 'UNIFORME') then
        if (typdec .eq. 'SUBD') then
            pasdt = deltat/nbrpas
        else if (typdec.eq.'DELT') then
            nbrpas = nint(deltat/deltac)
            pasdt = deltat/nbrpas
        else
            ASSERT(.false.)
        endif
    else if (optdec.eq.'PROGRESSIF') then
        if (typdec .eq. 'SUBD') then
            premie = ratio*deltat
            suivan = ((1.d0-ratio)*deltat)/(nbrpas-1)
            if (premie .le. r8prem() .or. suivan .le. r8prem()) then
                retdec = 0
                goto 99
            endif
        else if (typdec.eq.'DELT') then
            ASSERT(.false.)
        else
            ASSERT(.false.)
        endif
    else if (optdec.eq.'DEGRESSIF') then
        if (typdec .eq. 'SUBD') then
            premie = ((1.d0-ratio)*deltat)/(nbrpas-1)
            suivan = ratio*deltat
            if (premie .le. r8prem() .or. suivan .le. r8prem()) then
                retdec = 0
                goto 99
            endif
        else if (typdec.eq.'DELT') then
            premie = ((1.d0-ratio)*deltat)
            suivan = ratio*deltat
            nbrpas = nint(premie/deltac) + 1
            premie = deltac
            suivan = deltat-(nbrpas-1)*deltac
            if (premie .le. r8prem() .or. suivan .le. r8prem()) then
                retdec = 0
                goto 99
            endif
        else
            ASSERT(.false.)
        endif
    else
        ASSERT(.false.)
    endif
!
! --- DECOUPE INUTILE ?
!
    if (nbrpas .le. 1) then
        retdec = 2
        goto 99
    endif
!
! --- CONSTRUCTION DE LA LISTE
!
    call wkvect(nomlis, 'V V R', nbrpas, jinst)
!
! --- REMPLISSAGE DE LA LISTE
!
    inst = instam
    if (optdec .eq. 'UNIFORME') then
        do 10 ipas = 1, nbrpas
            inst = inst + pasdt
            zr(jinst+ipas-1) = inst
            dtmin = min(dtmin ,pasdt)
10      continue
    else if (optdec.eq.'PROGRESSIF') then
        do 15 ipas = 1, nbrpas
            if (ipas .eq. 1) then
                pasdt = premie
            else
                pasdt = suivan
            endif
            inst = inst + pasdt
            zr(jinst+ipas-1) = inst
            dtmin = min(dtmin ,pasdt)
15      continue
    else if (optdec.eq.'DEGRESSIF') then
        do 20 ipas = 1, nbrpas
            if (ipas .eq. nbrpas) then
                pasdt = suivan
            else
                pasdt = premie
            endif
            inst = inst + pasdt
            zr(jinst+ipas-1) = inst
            dtmin = min(dtmin ,pasdt)
20      continue
    else
        ASSERT(.false.)
    endif
!
! --- AFFICHAGE
!
    retdec = 1
    if (linfo) then
        if (optdec .eq. 'UNIFORME') then
            valr(1) = instam
            valr(2) = pasdt
            call utmess('I', 'SUBDIVISE_10', si=nbrpas, nr=2, valr=valr)
        else if (optdec.eq.'PROGRESSIF') then
            valr(1) = instam
            valr(2) = premie
            valr(3) = suivan
            call utmess('I', 'SUBDIVISE_11', si=nbrpas, nr=3, valr=valr)
        else if (optdec.eq.'DEGRESSIF') then
            valr(1) = instam
            valr(2) = premie
            valr(3) = suivan
            call utmess('I', 'SUBDIVISE_12', si=nbrpas, nr=3, valr=valr)
        else
            ASSERT(.false.)
        endif
    endif
!
99  continue
!
    if (retdec.eq.0) then
        call utmess('I', 'SUBDIVISE_50')
        call utmess('I', 'SUBDIVISE_53', sr = premie)
    endif
!
    call jedema()
!
end subroutine
