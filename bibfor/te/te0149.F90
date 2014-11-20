subroutine te0149(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/poefgr.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/porigi.h"
#include "asterfort/posigr.h"
#include "asterfort/posipr.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rhoequ.h"
#include "asterfort/utmess.h"
#include "asterfort/vecma.h"
    character(len=*) :: option, nomte
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
!     CALCUL DU VECTEUR ELEMENTAIRE CONTRAINTE
!     POUR LES ELEMENTS DE POUTRE D'EULER ET DE TIMOSHENKO.
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!        'SIPM_ELNO'
!        'SIPO_ELNO'
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
!        'MECA_POU_D_EM': POUTRE D'EULER MULTIFIBRE (SIPM_ELNO UNIQUEMENT)
!        'MECA_POU_D_TGM': POUTRE DE TIMOSHENKO MULTIFIBRE (SIPM_ELNO UNIQUEMENT)
!     ------------------------------------------------------------------
!
    integer :: nbres, nbref
    parameter     (nbres=3,nbref=6)
    integer :: lmater, jmat, nbmat, imat, icomp, npg, lopt, itsec
    integer :: labsc, jeffo, iret, nbpar
    real(kind=8) :: valres(nbres), valref(nbref)
    integer :: codres(nbres), codref(nbref)
    integer :: nbfib, inbf, isief, ino, i
    real(kind=8) :: sixx, simin, simax
    character(len=8) :: nompar
    character(len=16) :: nomres(nbres), nomref(nbref)
    character(len=24) :: suropt, messk(2)
    real(kind=8) :: zero, e, nu, rho, valpar, r1, ep1, absmoy, rhos, rhofi
    real(kind=8) :: rhofe, cm, phie, phii
    real(kind=8) :: klv(78), klc(12, 12), efge(12)
    aster_logical :: okopt
!     ------------------------------------------------------------------
    data nomres / 'E', 'NU', 'RHO'/
    data nomref / 'E', 'NU' , 'RHO' , 'PROF_RHO_F_INT' , 'PROF_RHO_F_EXT' ,&
     &             'COEF_MASS_AJOU'/
! --- ------------------------------------------------------------------
    integer, parameter :: nb_cara1 = 3
    real(kind=8) :: vale_cara1(nb_cara1)
    character(len=8) :: noms_cara1(nb_cara1)
    data noms_cara1 /'R1','EP1','TSEC'/
!-----------------------------------------------------------------------
    okopt = (option.eq.'SIPM_ELNO') .or. (option.eq.'SIPO_ELNO')
    ASSERT(okopt)
!
!   SIPM_ELNO pour les PMF
    if (nomte.eq.'MECA_POU_D_EM' .or. nomte.eq.'MECA_POU_D_TGM')then
        call jevech('PNBSP_I', 'L', inbf)
        nbfib = zi(inbf)
        call jevech('PSIEFNOR', 'L', isief)
        call jevech('PSIMXRR', 'E', jeffo)
        do ino = 1,2
            simax = zr(isief-1+nbfib*(ino-1)+1)
            simin = zr(isief-1+nbfib*(ino-1)+1)
            do i = 2, nbfib
                sixx = zr(isief-1+nbfib*(ino-1)+i)
                if (sixx .gt. simax) then
                    simax = sixx
                elseif (sixx .lt. simin)then
                    simin = sixx
                endif
            enddo
            zr(jeffo-1+2*(ino-1)+1) = simin
            zr(jeffo-1+2*(ino-1)+2) = simax
        enddo
    else



    zero = 0.d0
! --- ------------------------------------------------------------------
! --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
    call jevech('PMATERC', 'L', lmater)
! --- ------------------------------------------------------------------
!     BLINDAGE : OPTION VALIDE AVEC UN SEUL PHENOMENE : ELAS
    jmat = zi(lmater)
    nbmat= zi(jmat)
!     UN SEUL MATERIAU
    if (nbmat .ne. 1) then
        messk(1) = option
        call utmess('F', 'ELEMENTS4_59', sk=messk(1))
    endif
!     LE 1ER MATERIAU
    imat = jmat+zi(jmat+nbmat+1)
!     SEUL ELAS EST AUTORISE
    do icomp = 1, zi(imat+1)
        if (zk32(zi(imat)+icomp-1)(1:4) .ne. 'ELAS') then
            messk(1) = option
            messk(2) = zk32(zi(imat)+icomp-1)(1:24)
            call utmess('F', 'ELEMENTS4_64', nk=2, valk=messk)
        endif
    end do
! --- ------------------------------------------------------------------
!
    npg = 3
!
    call moytem('RIGI', npg, 1, '+', valpar,&
                iret)
    nompar = 'TEMP'
    nbpar = 1
!
    call jevech('PSUROPT', 'L', lopt)
    suropt = zk24(lopt)
! --- ------------------------------------------------------------------
    if (suropt .eq. 'MASS_FLUI_STRU') then
        call poutre_modloc('CAGEPO', noms_cara1, nb_cara1, lvaleur=vale_cara1)
        itsec = nint(vale_cara1(3))
        if (itsec .eq. 2) then
! ---       SECTION CIRCULAIRE SECTIONS INITIALE ET FINALE
            r1 = vale_cara1(1)
            ep1 = vale_cara1(2)
        else
            call utmess('F', 'ELEMENTS3_30')
        endif
        call jevech('PABSCUR', 'L', labsc)
        absmoy = ( zr(labsc-1+1) + zr(labsc-1+2) ) /2.d0
        call rcvalb('RIGI', 1, 1, '+', zi(lmater),&
                    ' ', 'ELAS_FLUI', 1, 'ABSC', [absmoy],&
                    nbref, nomref, valref, codref, 1)
        e = valref(1)
        nu = valref(2)
        rhos = valref(3)
        rhofi = valref(4)
        rhofe = valref(5)
        cm = valref(6)
        phie = r1*2.d0
        if (phie .eq. 0.d0) then
            call utmess('F', 'ELEMENTS3_26')
        endif
        phii = ( phie - 2.d0*ep1 )
        call rhoequ(rho, rhos, rhofi, rhofe, cm,&
                    phii, phie)
! --- ------------------------------------------------------------------
    else
        call rcvalb('RIGI', 1, 1, '+', zi(lmater),&
                    ' ', 'ELAS', nbpar, nompar, [valpar],&
                    2, nomres, valres, codres, 1)
        call rcvalb('RIGI', 1, 1, '+', zi(lmater),&
                    ' ', 'ELAS', nbpar, nompar, [valpar],&
                    1, nomres(3), valres(3), codres(3), 0)
        if (codres(3) .ne. 0) valres(3) = zero
        e = valres(1)
        nu = valres(2)
        rho = valres(3)
    endif
!
! --- ------------------------------------------------------------------
! --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE ---
    call porigi(nomte, e, nu, -1.d0, klv)
! --- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE
    call vecma(klv, 78, klc, 12)
!
! --- ------------------------------------------------------------------
    if (option .eq. 'SIPM_ELNO') then
! ---    CALCUL DU VECTEUR ELEMENTAIRE EFFORT GENERALISE ---
        call poefgr(nomte, klc, zi(lmater), e, nu,&
                    rho, efge)
! ---    ---------------------------------------------------------------
!        NOEUD 1  EFGE(1)  = N   EFGE(2)  = VY   EFGE(3)  = VZ
!                 EFGE(4)  = MT  EFGE(5)  = MFY  EFGE(6)  = MFZ
!        NOEUD 2  EFGE(7)  = N   EFGE(8)  = VY   EFGE(9)  = VZ
!                 EFGE(10) = MT  EFGE(11) = MFY  EFGE(12) = MFZ
        call jevech('PSIMXRR', 'E', jeffo)
        call posigr(nomte, efge, zr(jeffo))
!
! --- ------------------------------------------------------------------
    else if (option .eq. 'SIPO_ELNO') then
! ---    CALCUL DU VECTEUR ELEMENTAIRE EFFORT GENERALISE ---
        call poefgr(nomte, klc, zi(lmater), e, nu,&
                    rho, efge)
! ---    ---------------------------------------------------------------
!        NOEUD 1  EFGE(1)  = N   EFGE(2)  = VY   EFGE(3)  = VZ
!                 EFGE(4)  = MT  EFGE(5)  = MFY  EFGE(6)  = MFZ
!        NOEUD 2  EFGE(7)  = N   EFGE(8)  = VY   EFGE(9)  = VZ
!                 EFGE(10) = MT  EFGE(11) = MFY  EFGE(12) = MFZ
        call jevech('PCONTPO', 'E', jeffo)
        call posipr(nomte, efge, zr(jeffo))
    endif
!   fin if PMF
    endif
end subroutine
