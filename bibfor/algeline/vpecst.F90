subroutine vpecst(ifm, typres, omgmin, omgmax, nbfre1,&
                  nbfre2, nbfreq, nblagr, typep, typcon,&
                  dimc1, zimc1)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/freqom.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
    integer :: ifm, nbfre1, nbfre2, nbfreq, nblagr
    real(kind=8) :: omgmin, omgmax, dimc1
    complex(kind=8) :: zimc1
    character(len=1) :: typep
    character(len=8) :: typcon
    character(len=16) :: typres
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!   PRINTING OF THE NUMBER OF THE EIGENVALUES THAT BELONG TO A CHOOSEN
!   PATTERN
!     ------------------------------------------------------------------
! IN  OMGMIN : R8 : PULSATION MIN
! IN  OMGMAX : R8 : PULSATION MAX
! IN  NBFRE1 : IS : NB DE PULSATION INFERIEURE A OMGMIN
! IN  NBFRE2 : IS : NB DE PULSATION INFERIEURE A OMGMAX
! IN/OUT NBFREQ : IS : OUTPUT: NBRE DE FREQ DANS LA BANDE OMGMIN OMGMAX
!                    INPUT: NUMERO DE LA BANDE (SEULEMENT SI TYPEP='D')
! IN  NBLAGR : IS : NB DE DDLS DE LAGRANGE
! IN  TYPRES : TX : TYPE DE CALCUL (DYNAMIQUE OU FLAMBEMENT)
! IN  TYPEP  : K1 : TYPE D EIGENVALUE-PROBLEM
!    'R' GEP REEL POUR MODE_ITER_SIMULT/BANDE OU INFO_MODE SUR UNE
!        SEULE BANDE OU MODE_ITER_INV
!    'F' IDEM 'R' MAIS SANS CALCUL NBFREQ, IL EST DONNE EN INPUT.
!        UTILISE POUR OPTION 'SIMULT/'BANDE' AVEC TABLE ISSUE
!        D'INFO_MODE
!    'D' GEP REEL POUR INFO_MODE SUR LA BANDE NUMERO NFREQ
!    'C' GEP COMPLEXE OU QEP POUR INFO_MODE
!    'S' GEP REEL POUR TEST DE STURM POSTTRAITEMENT MODE_ITER_SIMULT
! IN  TYPCON : K8 : TYPE DE CONTOUR (LICITE SI TYPEP='C')
! IN  DIMC1  : R8 : DIMENSION CHARACTERISTIQUE REEL N 1 DU CONTOUR
!                   (LICITE SI TYPEP='C')
! IN  ZIMC1  : C16: DIMENSION CHARACTERISTIQUE COMPLEXE N 1 DU CONTOUR
!                   (LICITE SI TYPEP='C')
!     ------------------------------------------------------------------
!     REMARQUE:  NBFRE1 ET NBFRE2  SONT CALCULES PAR VPSTUR
!     ------------------------------------------------------------------
    real(kind=8) :: fmin, fmax, valr(3)
    integer :: ibande, vali(2)
!     ------------------------------------------------------------------
!
!   --- ON RECUPERE LE NUMERO DE LA BANDE SI NECESSAIRE
    if (typep .eq. 'D') ibande=nbfreq
!
!   --- WE ARE ON THE REEL PLANE (STURM TEST)
    if ((typep.eq.'R') .or. (typep.eq.'S') .or. (typep.eq.'D') .or. (typep.eq.'F')) then
!
!   --- TEST DE STURM
        if (typep .ne. 'F') then
            if (typres .eq. 'DYNAMIQUE') then
                nbfreq = abs( nbfre2 - nbfre1 )
            else
                if ((omgmin *omgmax) .ge. 0.d0) then
                    nbfreq=abs(nbfre2-nbfre1)
                else
                    nbfreq=abs(nbfre2+nbfre1-2*nblagr)
                endif
            endif
        endif
        if (nbfreq .gt. 9999) call u2mesi('A', 'ALGELINE3_64', 1, nbfreq)
!
!   --- AFFICHAGE SI MODE_ITER_SIMULT+BANDE OU MODE_ITER_INV+
!   --- SEPARE/AJUSTE OU INFO_MODE+MIN/MAX
        if ((typep.eq.'R') .or. (typep.eq.'F')) then
            if (typres .eq. 'DYNAMIQUE') then
                fmin=freqom(omgmin)
                fmax=freqom(omgmax)
                write(ifm,950)
                if (typep .eq. 'R') then
                    call u2mess('I', 'ALGELINE6_33')
                else if (typep.eq.'F') then
                    call u2mess('I', 'ALGELINE6_34')
                endif
                if (nbfreq .eq. 0) then
                    valr(1)=fmin
                    valr(2)=fmax
                    vali(1)=1
                    call u2mesg('I', 'ALGELINE6_35', 0, ' ', 1,&
                                vali, 2, valr)
                else if (fmin.eq.0.d0) then
                    valr(1)=fmax
                    vali(1)=nbfreq
                    call u2mesg('I', 'ALGELINE6_36', 0, ' ', 1,&
                                vali, 1, valr)
                else
                    valr(1)=fmin
                    valr(2)=fmax
                    vali(1)=1
                    vali(2)=nbfreq
                    call u2mesg('I', 'ALGELINE6_37', 0, ' ', 2,&
                                vali, 2, valr)
                endif
            else
                if (typep .eq. 'R') then
                    call u2mess('I', 'ALGELINE6_28')
                else if (typep.eq.'F') then
                    call u2mess('I', 'ALGELINE6_29')
                endif
                if (nbfreq .eq. 0) then
                    valr(1)=omgmin
                    valr(2)=omgmax
                    vali(1)=1
                    call u2mesg('I', 'ALGELINE6_30', 0, ' ', 1,&
                                vali, 2, valr)
                else if (omgmin .eq. 0.d0) then
                    valr(1)=omgmax
                    vali(1)=nbfreq
                    call u2mesg('I', 'ALGELINE6_31', 0, ' ', 1,&
                                vali, 1, valr)
                else
                    valr(1)=omgmin
                    valr(2)=omgmax
                    vali(1)=1
                    vali(2)=nbfreq
                    call u2mesg('I', 'ALGELINE6_32', 0, ' ', 2,&
                                vali, 2, valr)
                endif
            endif
!
!   --- AFFICHAGE SI INFO_MODE+LISTE
        else if (typep.eq.'D') then
            if (typres .eq. 'DYNAMIQUE') then
                fmin=freqom(omgmin)
                fmax=freqom(omgmax)
                if (ibande .eq. 1) then
                    write(ifm,950)
                    call u2mess('I', 'ALGELINE6_33')
                endif
                valr(1)=fmin
                valr(2)=fmax
                vali(1)=ibande
                if (nbfreq .eq. 0) then
                    call u2mesg('I', 'ALGELINE6_35', 0, ' ', 1,&
                                vali, 2, valr)
                else
                    vali(2)=nbfreq
                    call u2mesg('I', 'ALGELINE6_37', 0, ' ', 2,&
                                vali, 2, valr)
                endif
            else
                if (ibande .eq. 1) then
                    write(ifm,950)
                    call u2mess('I', 'ALGELINE6_28')
                endif
                valr(1)=omgmin
                valr(2)=omgmax
                vali(1)=ibande
                if (nbfreq .eq. 0) then
                    call u2mesg('I', 'ALGELINE6_30', 0, ' ', 1,&
                                vali, 2, valr)
                else
                    vali(2)=nbfreq
                    call u2mesg('I', 'ALGELINE6_32', 0, ' ', 2,&
                                vali, 2, valr)
                endif
            endif
!
        else if (typep.eq.'S') then
!   --- AFFICHAGE DEDIE DS UNE DES ROUTINES APPELLANTES
        endif
!
!   --- WE ARE ON THE COMPLEX PLANE (APM TEST)
    else if (typep.eq.'C') then
        nbfreq=nbfre2
        if (nbfreq .gt. 9999) then
            call u2mess('A', 'ALGELINE3_64')
            call u2mesi('I', 'ALGELINE7_19', 1, nbfreq)
        endif
        write(ifm,950)
        call u2mess('I', 'ALGELINE6_38')
        if (nbfreq .eq. 0) then
            if (typcon(1:6) .eq. 'CERCLE') then
                valr(1)=dble(zimc1)
                valr(2)=dimag(zimc1)
                valr(3)=dimc1
                call u2mesr('I', 'ALGELINE6_39', 3, valr)
            endif
        else
            if (typcon(1:6) .eq. 'CERCLE') then
                valr(1)=dble(zimc1)
                valr(2)=dimag(zimc1)
                valr(3)=dimc1
                vali(1)=nbfreq
                call u2mesg('I', 'ALGELINE6_40', 0, ' ', 1,&
                            vali, 3, valr)
            endif
        endif
!
!   --- ILLEGAL OPTION
    else
        call assert(.false.)
    endif
    if (typep .ne. 'S') write(ifm,950)
!
    950 format(72('-'),/)
end subroutine
