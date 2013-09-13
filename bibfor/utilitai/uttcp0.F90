subroutine uttcp0(indi, para, nbv, temps)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
#include "asterc/uttcsm.h"
#include "asterc/uttrst.h"
#include "asterfort/utmess.h"
    integer :: indi, nbv
    real(kind=8) :: temps(nbv)
    character(len=*) :: para
! ----------------------------------------------------------------------
!  ROUTINE DE MESURE DU TEMPS CPU.
!  UTILISE LES ROUTINES C SUIVANTES (ADHERENCE CRAY/SOLARIS)
!     UTTCSM  : RENVOI LE  TEMPS CONSOMME TOTAL DU PROCESSUS (SEC)
!     UTTRST  : RENVOI LES TEMPS RESTANT  TOTAL DU PROCESSUS (SEC)
!
! IN  INDI    : INDICE D'APPEL ( 0 =< INDI =< 200 )
! * L'INDICE 0 EST RESERVE (RETOURNE SEULEMENT TEMPS(1))
! * LES INDICES 1 A 100 SONT A UTILISER SI ON APPELLE UTTCP0 DIRECTEMENT
!   (CE QU'IL FAUT EVITER)
! * LES INDICES 101 A 200 SONT UTILISES PAR UTTCPU
!
! IN  PARA    : PARAMETRE D'INTIALISATION DES APPELS POUR L'INDICE INDI
!               PARA = 'INIT'  LES PARAMETRES SONT (RE)MIS A ZERO
!               PARA = 'DEBUT' LA MESURE DE TEMPS COMMENCE
!               PARA = 'FIN'   LA MESURE DE TEMPS S'ARRETE
! IN  NBV     : NOMBRE DE VALEURS A RECUPERER
! OUT TEMPS   : TEMPS(1) TEMPS CPU RESTANT EN SECONDES
!               TEMPS(2) NOMBRE D'APPEL EFFECTUE AVEC L'INDICE INDI
!               TEMPS(3) TEMPS CPU TOTAL POUR L'INDICE INDI
!               TEMPS(4) TEMPS CPU MOYEN POUR L'INDICE INDI
!               TEMPS(5) TEMPS CPU USER TOTAL POUR L'INDICE INDI
!               TEMPS(6) TEMPS CPU SYSTEME TOTAL POUR L'INDICE INDI
!               TEMPS(7) TEMPS ELAPSED POUR L'INDICE INDI
! ----------------------------------------------------------------------
    integer :: nb1, nb2, nb
    parameter        ( nb1 = 0 , nb2 = 200 , nb = nb2-nb1+1)
    real(kind=8) :: uscpui(nb1:nb2), sycpui(nb1:nb2), elapsi(nb1:nb2)
    real(kind=8) :: uscpu(nb1:nb2), sycpu(nb1:nb2), elaps(nb1:nb2)
    integer :: nbappe(nb1:nb2), k, nbt
    character(len=5) :: kpara, parini(nb1:nb2)
!
    real(kind=8) :: t(7), tcsm(3), tpres
!
    save      uscpui,sycpui,uscpu,sycpu,nbappe,parini
    save      elapsi,elaps
!
    data      uscpui,sycpui,uscpu,sycpu,nbappe,parini&
     &         /nb*0d0,nb*0d0,nb*0d0,nb*0d0,nb*0,nb*'INIT '/
    data      elapsi,elaps /nb*0d0,nb*0d0/
! ----------------------------------------------------------------------
!
!     VERIFS ET INITIALISATIONS
!
    if (indi .lt. nb1 .or. indi .gt. nb2) then
        call utmess('F', 'UTILITAI5_54')
    endif
!
    do 1 k = 1, 7
        t(k) = 0.d0
 1  end do
    kpara = para
!
!     TEMPS RESTANT
!
    call uttrst(tpres)
!
!     SI INDI = 0 ACTUALISATION DE T(1) SEULEMENT
!
    t(1) = tpres
    if (indi .eq. 0) then
        nbt = 1
        goto 9999
    else
        nbt = 7
    endif
!
!     TEMPS CONSOMME
!
    call uttcsm(tcsm)
!
!     INITIALISATIONS - MISE A ZERO
!
    if (kpara .eq. 'INIT') then
        parini(indi) = 'INIT'
        uscpui(indi) = 0.0d0
        uscpu(indi) = 0.0d0
        sycpui(indi) = 0.0d0
        sycpu(indi) = 0.0d0
        elapsi(indi) = 0.0d0
        elaps(indi) = 0.0d0
        nbappe(indi) = 0
!
!     DEBUT DE LA MESURE
!
    else if (kpara .eq. 'DEBUT') then
        if (parini(indi) .ne. 'FIN' .and. parini(indi) .ne. 'INIT') then
            call utmess('F', 'UTILITAI5_55')
        endif
        parini(indi) = 'DEBUT'
        uscpui(indi) = tcsm(1)
        sycpui(indi) = tcsm(2)
        elapsi(indi) = tcsm(3)
!
!     FIN DE LA MESURE - CALCUL PAR DIFFERENCE
!
    else if (kpara .eq. 'FIN') then
        if (parini(indi) .ne. 'DEBUT') then
            call utmess('F', 'UTILITAI5_56')
        endif
        parini(indi) = 'FIN'
        nbappe(indi) = nbappe(indi) + 1
        uscpu (indi) = uscpu (indi) + tcsm(1) - uscpui(indi)
        sycpu (indi) = sycpu (indi) + tcsm(2) - sycpui(indi)
        elaps (indi) = elaps (indi) + tcsm(3) - elapsi(indi)
        t(2) = nbappe(indi)
        t(3) = uscpu(indi) + sycpu(indi)
        t(4) = t(3)/nbappe(indi)
        t(5) = uscpu(indi)
        t(6) = sycpu(indi)
        t(7) = elaps(indi)
!
    else
        call utmess('F', 'UTILITAI5_57', sk=kpara)
    endif
!
9999  continue
!
!     RENVOI DU NB DEMANDE DE VALEURS
!
    do 100 k = 1, min(nbv, nbt)
        temps(k) = t(k)
100  end do
end subroutine
