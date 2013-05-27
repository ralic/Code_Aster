subroutine transi(np1, np2, np3, np4, nbm,&
                  nbnl, npfmax, npfts, dttr, ttrans,&
                  eps, fext, text, fextts, textts,&
                  fexttr, fextt0, masgi, amori, pulsi,&
                  phii, typch, nbseg, rc, alpha,&
                  beta, gamma, orig, theta, vitg,&
                  depg, amor, pulsd, omegaf, aa,&
                  bb, old, s0, z0, sr0,&
                  za1, za2, za3, za4, za5,&
                  zitr, zin, mtrans, amor00, puls00,&
                  accg0, vitg0, depg0, iconfb, tconf1,&
                  ftest0, ier)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE  CRP_21
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DE LA REPONSE DYNAMIQUE TRANSITOIRE D'UNE
! -----------   STRUCTURE PAR UNE METHODE INTEGRALE
!               (VERSION MULTI-MODALE)
!
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/adimve.h'
    include 'asterfort/calfft.h'
    include 'asterfort/caltra.h'
    include 'asterfort/comptr.h'
    include 'asterfort/defttr.h'
    include 'asterfort/estitr.h'
    include 'asterfort/inialg.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    integer :: np1, np2, np3, np4, nbm, nbnl, npfmax, npfts
    real(kind=8) :: dttr, ttrans, eps, fext(np4, *), text(*), fextts(np4, *)
    real(kind=8) :: textts(*), fexttr(*), fextt0(*), masgi(*), amori(*)
    real(kind=8) :: pulsi(*), phii(np2, np1, *)
    integer :: typch(*), nbseg(*)
    real(kind=8) :: rc(np3, *), alpha(2, *), beta(2, *), gamma(2, *), orig(3, *)
    real(kind=8) :: theta(np3, *), vitg(*), depg(*), amor(*), pulsd(*)
    real(kind=8) :: omegaf(*), aa(*), bb(*), old(9, *)
    complex(kind=8) :: s0(*), z0(*), sr0(*), za1(*), za2(*), za3(*), za4(np4, *)
    complex(kind=8) :: za5(np4, *), zitr(*), zin(*)
    real(kind=8) :: mtrans(2, 2, *), amor00(*), puls00(*), accg0(*), vitg0(*)
    real(kind=8) :: depg0(*)
    integer :: iconfb(*)
    real(kind=8) :: tconf1(4, *), ftest0
    integer :: ier
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ichtr, npf, nttr, ntrans, ntramx
    real(kind=8) :: ttran0
    real(kind=8) :: valr
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL    ADIMVE, CALFFT, CALTRA, COMPTR, DEFTTR, ESTITR,
!    &            INIALG
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
! 1.  PREMIERE ESTIMATION DE LA DUREE DU REGIME TRANSITOIRE
!     -----------------------------------------------------
!
    ier = 0
    call estitr(nbm, amori, masgi, eps, ttrans,&
                npf, npfmax, text, ier)
    if (ier .ne. 0) then
        valr = ttrans
        call u2mesg('F', 'ALGORITH14_68', 0, ' ', 0,&
                    0, 1, valr)
    endif
!
! 2.  REPETER JUSQU'A VALIDATION DE LA DUREE DU REGIME TRANSITOIRE
!     ------------------------------------------------------------
!
    ntrans = 0
    ntramx = 10000
!
100  continue
!
! 2.1 SORTIE EN ERREUR FATALE SI DEPASSEMENT DU NOMBRE D'ITERATIONS
!     MAXIMAL
!
    if (ntrans .ge. ntramx) call u2mess('F', 'ALGORITH10_96')
!
! 2.2 CARACTERISATION DE L'ECHANTILLON TEMPOREL CONSIDERE
!     (AJUSTEMENT DE LA DUREE DU REGIME TRANSITOIRE)
!     RECUPERATION DES FORCES EXTERNES SUR CET ECHANTILLON
!
    call defttr(np1, np4, nbm, npf, nttr,&
                ntrans, ttran0, ttrans, text, fext,&
                fextt0, fexttr, dttr)
    if (nttr .gt. npfmax) call u2mess('F', 'ALGORITH10_97')
!
! 2.3 A LA PREMIERE ESTIMATION DE LA DUREE DU REGIME TRANSITOIRE,
!     CALCUL DES COEFFICIENTS DE FOURIER SUR L'ECHANTILLON TEMPOREL
!
    if (ntrans .eq. 0) call calfft(np1, np4, nbm, nttr, dttr,&
                                   fext, omegaf, aa, bb)
!
! 2.4 CALCUL DE LA REPONSE MODALE SUR L'INTERVALLE (TTRAN0,TTRANS)
!
    call caltra(np1, np4, nbm, nttr, ttrans,&
                ttran0, vitg, depg, vitg0, depg0,&
                masgi, amor, pulsi, pulsd, mtrans,&
                s0, z0, sr0, za1, za2,&
                za3, za4, za5, zitr, zin,&
                fextt0, fexttr, dttr, omegaf, aa,&
                bb, ntrans)
!
! 2.5 VERIFICATION QUE L'ON N'A PAS ATTEINT UNE BUTEE
!
    ichtr = 0
    call comptr(np1, np2, np3, nbm, nbnl,&
                ichtr, depg, vitg, phii, typch,&
                nbseg, alpha, beta, gamma, orig,&
                rc, theta, old)
!
! 2.6 SI LE SYSTEME EST EN PHASE DE VOL A L'ISSUE DU REGIME TRANSITOIRE,
!     ON INITIALISE LES PARAMETRES PUIS ON RETOURNE A L'APPELANT MDITM2
!     POUR PASSER A LA SIMULATION EN REGIME ETABLI
!
    if (ichtr .eq. 0) then
        call adimve(nbm, fexttr, masgi)
        call inialg(np1, np2, np3, np4, nbm,&
                    nbnl, nttr, npfmax, npfts, depg,&
                    vitg, depg0, vitg0, accg0, amor00,&
                    puls00, fexttr, fext, text, fextts,&
                    textts, typch, nbseg, phii, alpha,&
                    beta, gamma, orig, rc, theta,&
                    iconfb, tconf1, ftest0)
!
! 2.7 DANS LE CAS CONTRAIRE, INITIALISATION DES VECTEURS DEPLACEMENTS ET
!     VITESSES GENERALISES POUR UNE NOUVELLE ESTIMATION DE LA DUREE DU
!     REGIME TRANSITOIRE
!
    else
        ntrans = ntrans + 1
        do 101 i = 1, nbm
            depg0(i) = depg(i)
101      continue
        do 102 i = 1, nbm
            vitg0(i) = vitg(i)
102      continue
!
! ------ RETOURNER A REPETER
        goto 100
!
    endif
!
! --- FIN DE TRANSI.
end subroutine
