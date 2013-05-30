subroutine vpecst(ifm, typres, omgmin, omgmax, nbfre1,&
                  nbfre2, nbfreq, nblagr, typep, typcon,&
                  dimc1, zimc1)
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/freqom.h'
    include 'asterfort/u2mess.h'
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
    real(kind=8) :: fmin, fmax
    integer :: ibande
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
        if (nbfreq .gt. 9999) then
            call u2mess('A', 'ALGELINE3_64')
            write(ifm,*)' NOMBRE DE VALEURS PROPRES : ',nbfreq
        endif
!
!   --- AFFICHAGE SI MODE_ITER_SIMULT+BANDE OU MODE_ITER_INV+
!   --- SEPARE/AJUSTE OU INFO_MODE+MIN/MAX
        if ((typep.eq.'R') .or. (typep.eq.'F')) then
            if (typres .eq. 'DYNAMIQUE') then
                fmin=freqom(omgmin)
                fmax=freqom(omgmax)
                if (typep .eq. 'R') then
                    write(ifm,900)
                else if (typep.eq.'F') then
                    write(ifm,1900)
                endif
                if (nbfreq .eq. 0) then
                    write(ifm,901)fmin,fmax
                else if (fmin.eq.0.d0) then
                    write(ifm,902)nbfreq,fmax
                else
                    write(ifm,903)fmin,fmax,nbfreq
                endif
            else
                if (typep .eq. 'R') then
                    write(ifm,800)
                else if (typep.eq.'F') then
                    write(ifm,1800)
                endif
                if (nbfreq .eq. 0) then
                    write(ifm,801) omgmin, omgmax
                else if (omgmin .eq. 0.d0) then
                    write(ifm,802)nbfreq,omgmax
                else
                    write(ifm,803)omgmin,omgmax,nbfreq
                endif
            endif
!
!   --- AFFICHAGE SI INFO_MODE+LISTE
        else if (typep.eq.'D') then
            if (typres .eq. 'DYNAMIQUE') then
                fmin=freqom(omgmin)
                fmax=freqom(omgmax)
                if (ibande .eq. 1) write(ifm,900)
                if (nbfreq .eq. 0) then
                    write(ifm,1901)ibande,fmin,fmax
                else
                    write(ifm,1903)ibande,fmin,fmax,nbfreq
                endif
            else
                if (ibande .eq. 1) write(ifm,800)
                if (nbfreq .eq. 0) then
                    write(ifm,1801)ibande,omgmin,omgmax
                else
                    write(ifm,1803)ibande,omgmin,omgmax,nbfreq
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
            write(ifm,*)' NOMBRE DE VALEURS PROPRES : ',nbfreq
        endif
        write(ifm,910)
        if (nbfreq .eq. 0) then
            if (typcon(1:6) .eq. 'CERCLE') write(ifm, 911)dble(zimc1), dimag(zimc1), dimc1
        else
            if (typcon(1:6) .eq. 'CERCLE') write(ifm, 912)dble(zimc1), dimag(zimc1), dimc1,&
                                           nbfreq
        endif
!
!   --- ILLEGAL OPTION
    else
        call assert(.false.)
    endif
!
    if (typep .ne. 'S') write(ifm,950)
!     ------------------------------------------------------------------
    800 format(/,72('-'),/,'   VERIFICATION DU SPECTRE DE CHARGES ',&
     &'CRITIQUES (METHODE DE STURM)',/)
    1800 format(/,72('-'),/,'   VERIFICATION DU SPECTRE DE CHARGES ',&
     &'CRITIQUES (METHODE DE STURM - '/&
     &'RESULTAT ISSU D''UN INFO_MODE PRECEDENT)',/)
!
    801 format(1x,'PAS DE CHARGE CRITIQUE DANS LA BANDE (',1pe10.3,',',&
     &                                                 1pe10.3,') ')
    1801 format(1x,'PAS DE CHARGE CRITIQUE DANS LA BANDE N ',i3,' DE '/&
     &       ' BORNES (',1pe10.3,',',1pe10.3,') ')
!
    802 format(1x,i4,' CHARGES CRITIQUES INFERIEURES A ',1pe10.3,' HZ')
!
    803 format(1x,'LE NOMBRE DE CHARGES CRITIQUES DANS LA BANDE (',&
     &                               1pe10.3,',',1pe10.3,') EST ',i4)
    1803 format(1x,'LE NOMBRE DE CHARGES CRITIQUES DANS LA BANDE N ',i3,&
     &       ' DE BORNES (',1pe10.3,',',1pe10.3,') EST ',i4)
!
    900 format(/,72('-'),/,'   VERIFICATION DU SPECTRE DE FREQUENCES ',&
     &'(METHODE DE STURM)',/)
    1900 format(/,72('-'),/,'   VERIFICATION DU SPECTRE DE FREQUENCES ',&
     &'(METHODE DE STURM - '/&
     &'RESULTAT ISSU D''UN INFO_MODE PRECEDENT)',/)
!
    901 format(1x,'PAS DE FREQUENCE DANS LA BANDE (',1pe10.3,',',&
     &                                                 1pe10.3,') ')
    1901 format(1x,'PAS DE FREQUENCE DANS LA BANDE N ',i3,' DE BORNES '/&
     &       '(',1pe10.3,',',1pe10.3,')')
!
    902 format(1x,i4,' FREQUENCES PROPRES INFERIEURES A ',1pe10.3,' HZ')
!
    903 format(1x,'LE NOMBRE DE FREQUENCES DANS LA BANDE (',1pe10.3,&
     &                                         ',',1pe10.3,') EST ',i4)
    1903 format(1x,'LE NOMBRE DE FREQUENCES DANS LA BANDE N ',i3,' DE '/&
     &       'BORNES (',1pe10.3,',',1pe10.3,') EST ',i4)
!
    910 format(/,72('-'),/,'   VERIFICATION DU SPECTRE EN FREQUENCE ',&
     &  '(METHODE DE L''ARGUMENT PRINCIPAL)',/)
    911 format(1x,'PAS DE FREQUENCE DANS LE DISQUE CENTRE EN (',1pe10.3,&
     &  ',',1pe10.3,')',/,' ET DE RAYON ',1pe10.3)
    912 format(1x,'LE NOMBRE DE FREQUENCES DANS LE DISQUE CENTRE EN (',&
     &  1pe10.3,',',1pe10.3,')',/,' ET DE RAYON ',1pe10.3,' EST ',i4)
!
    950 format(72('-'),/)
!     ------------------------------------------------------------------
end subroutine
