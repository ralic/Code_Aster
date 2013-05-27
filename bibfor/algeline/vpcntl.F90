subroutine vpcntl(cty, mode, option, omemin, omemax,&
                  seuil, nfreq, ipos, lmat, omecor,&
                  precdc, ier, vpinf, vpmax, freq,&
                  err, charge, typres, nblagr, solveu,&
                  nbrssa, precsh)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_21
!     CONTROLE DE VALIDITE DES MODES TROUVES
!-----------------------------------------------------------------------
! IN CTY   : K1 : COMPORTEMENT EN CAS D'ERREUR ('A' OU 'F')
! IN MODE  : K* : TYPE DE RESULTAT
! IN OPTION: K* : TYPE DE CALCUL. SI ' ' ON NE FAIT PAS STURM
! IN OMEMIN/MAX: R8 : BORNES DE L'INTERVALLE
! IN SEUIL  : R8 : POUR TEST DE VALIDITE DES MODES (VERI_MODE/SEUIL)
! IN NFREQ : IN : NBRE DE FREQS (CHAR_CRITS) CALCULEES
! IN IPOS(*) : IN(*) : VECTEUR DE POSITIONS MODALES
! IN LMAT(*) : IN(*) : VECTEUR DES DESCRIPTEURS DES MATRICES DU PB
!            LMAT(1)  : MATRICE DE RAIDEUR
!            LMAT(2)  : MATRICE DE MASSE
!            LMAT(3)  : RESULTAT DE LA MATRICE SHIFTEE FACTORISEE
! IN OMECOR : R8 : VALEUR MINIMALE ADMISSIBLE (SEUIL_FREQ)
! IN PRECDC : R8 : POURCENTAGE DE DECALAGE (VERI_MODE/PREC_SHIFT)
! IN NBRSSA : IN : NBRE DE DECALAGES ADMISSIBLES (NMAX_ITER_SHIFT)
! OUT IER   : IN : CODE RETOUR
!            0 TOUT C'EST BIEN PASSE
!            > 0 NOMBRE D'ERREURS TROUVEES
! IN VPINF/MAX: R8 : REDONDANT AVEC OMEMIN/MAX ?
! IN FREQ(*)/CHARGE(*)/ERR(*): R8(*) : LISTE DES FREQS/CHAR_CRITS ET
!            DES ERREURS ASSOCIEES
! IN TYPRES: K* : TYPE DE RESULTAT
! IN NBLAGR: IN : NBRE DE LAGRANGES
! IN SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
! IN PRECSH  : R8 : POURCENTAGE DE DECALAGE (CALC_FREQ/PREC_SHIFT)
!-----------------------------------------------------------------------
!
    implicit none
!
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterfort/freqom.h'
    include 'asterfort/infniv.h'
    include 'asterfort/omega2.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mesr.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vpfopr.h'
    integer :: nfreq, ipos(*), lmat(3), ier, nblagr, nbrssa, ibid2(2)
    real(kind=8) :: vpinf, vpmax, omemin, omemax, seuil, precdc, omecor
    real(kind=8) :: charge(nfreq), freq(nfreq), err(nfreq), precsh
    character(len=1) :: cty
    character(len=16) :: k16b, nomcmd
    character(len=19) :: solveu
    character(len=24) :: valk
    character(len=*) :: mode, option, typres
!
!     ------------------------------------------------------------------
    real(kind=8) :: zmin, zmax, omega, valr(2), rbid, det(2)
    integer :: ifm, niv, ifreq, nfreqt, vali(2), idet(2)
!     ------------------------------------------------------------------
    ier = 0
!
!     ---RECUPERATION DU NIVEAU D'IMPRESSION----
    call infniv(ifm, niv)
    if (niv .ge. 1) then
        write(ifm,1000)
        write(ifm,1100)
        write(ifm,1200)
    endif
!
!     ------------------------------------------------------------------
!     ------------------ CONTROLE DES NORMES D'ERREURS -----------------
!     ------------------------------------------------------------------
!
    if (seuil .gt. 0.0d0) then
!
        do 100 ifreq = 1, nfreq
            if (err(ifreq) .gt. seuil) then
                ier = ier + 1
                valk = mode
                vali (1) = ipos(ifreq)
                call u2mesg(cty//'+', 'ALGELINE5_15', 1, valk, 1,&
                            vali, 0, 0.d0)
                if (typres .eq. 'DYNAMIQUE') then
                    valr (1) = freq(ifreq)
                    call u2mesr(cty//'+', 'ALGELINE5_16', 1, valr)
                else
                    valr (1) = charge(ifreq)
                    call u2mesr(cty//'+', 'ALGELINE5_17', 1, valr)
                endif
                valr (1) = err(ifreq)
                valr (2) = seuil
                call u2mesr(cty//'+', 'ALGELINE5_18', 2, valr)
!
                call getres(k16b, k16b, nomcmd)
                if (typres .eq. 'DYNAMIQUE') then
                    valk = 'FREQ'
                else
                    valk = 'CHAR_CRIT'
                endif
                if (nomcmd(1:16) .eq. 'MODE_ITER_SIMULT') then
                    call u2mesk(cty, 'ALGELINE5_77', 1, 'NMAX_'//valk)
                else
                    call u2mesk(cty, 'ALGELINE5_78', 1, 'CALC_'//valk)
                endif
!
            endif
100      continue
    endif
!     ------------------------------------------------------------------
!     -- OPTION BANDE :                                              ---
!     -- VERIFICATION QUE LES FREQUENCES TROUVEES SONT DANS LA BANDE ---
!     ------------------------------------------------------------------
    if (option .eq. 'BANDE') then
        zmax = (1.d0 + sign(precdc,omemax)) * omemax
        zmin = (1.d0 - sign(precdc,omemin)) * omemin
        if (abs(omemin) .le. omecor) zmin = - omecor
        do 210 ifreq = 1, nfreq
            if (typres .eq. 'DYNAMIQUE') then
                omega = omega2(freq(ifreq))
            else
                omega = charge(ifreq)
            endif
            if (omega .lt. zmin .or. omega .gt. zmax) then
                ier = ier + 1
                valk = mode
                vali (1) = ipos(ifreq)
                call u2mesg(cty//'+', 'ALGELINE5_15', 1, valk, 1,&
                            vali, 0, 0.d0)
                if (typres .eq. 'DYNAMIQUE') then
                    valr (1) = freq(ifreq)
                    call u2mesr(cty//'+', 'ALGELINE5_16', 1, valr(1))
                    valr (1) = freqom(omemin)
                    valr (2) = freqom(omemax)
                    call u2mesr(cty, 'ALGELINE5_20', 2, valr)
                else
                    valr (1) = charge(ifreq)
                    call u2mesr(cty//'+', 'ALGELINE5_17', 1, valr(1))
                    valr (1) = omemin
                    valr (2) = omemax
                    call u2mesr(cty, 'ALGELINE5_20', 2, valr)
                endif
            endif
210      continue
    endif
!
!     ------------------------------------------------------------------
!     -- POUR TOUTES LES OPTIONS :                                   ---
!     -- VERIFICATION QU'ON A LE BON NOMBRE DE FREQUENCES            ---
!     ------------------------------------------------------------------
!
!        --- RECHERCHE DE LA PLUS PETITE ET DE LA PLUS GRANDE FREQUENCES
!
    if (option .ne. ' ') then
!
! --- POUR OPTIMISER ON NE CALCULE PAS LE DET, ON NE GARDE PAS LA FACTO
! --- (SI MUMPS)
        k16b=typres
        call vpfopr('STURM', k16b, lmat(2), lmat(1), lmat(3),&
                    vpinf, vpmax, rbid, nfreqt, ibid2,&
                    omecor, precsh, nbrssa, nblagr, solveu,&
                    det, idet)
!
        if (nfreqt .ne. nfreq) then
            ier = ier + 1
            valk = mode
            call u2mesk(cty//'+', 'ALGELINE5_23', 1, valk)
            if (typres .eq. 'DYNAMIQUE') then
                valr (1) = freqom(vpinf)
                valr (2) = freqom(vpmax)
                vali (1) = nfreqt
                vali (2) = nfreq
                call u2mesg(cty//'+', 'ALGELINE5_24', 0, ' ', 2,&
                            vali, 2, valr)
            else
                valr (1) = vpinf
                valr (2) = vpmax
                vali (1) = nfreqt
                vali (2) = nfreq
                call u2mesg(cty//'+', 'ALGELINE5_25', 0, ' ', 2,&
                            vali, 2, valr)
            endif
            call u2mess(cty, 'ALGELINE5_26')
        else
            if (niv .ge. 1) then
                if (typres .eq. 'DYNAMIQUE') then
                    write(ifm,1300) freqom(vpinf), freqom(vpmax)
                    write(ifm,1400) nfreqt
                else
                    write(ifm,1300) vpinf, vpmax
                    write(ifm,1401) nfreqt
                endif
            endif
        endif
    endif
    if (niv .ge. 1) write(ifm,1000)
!
    1000 format (72('-'),/)
    1100 format (10x,'VERIFICATION A POSTERIORI DES MODES')
    1200 format (3x)
    1300 format (3x,'DANS L''INTERVALLE (',1pe12.5,',',1pe12.5,') ')
    1400 format (3x,'IL Y A BIEN ',i4,' FREQUENCE(S) ')
    1401 format (3x,'IL Y A BIEN ',i4,' CHARGE(S) CRITIQUE(S) ')
!
end subroutine
