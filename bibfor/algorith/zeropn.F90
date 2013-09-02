subroutine zeropn(kstop, degre, ai, racine, ier)
! =====================================================================
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
! =====================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/u2mesi.h"
#include "asterfort/vphqrp.h"
    integer :: degre
    real(kind=8) :: ai(degre), racine(2*degre)
    character(len=1) :: kstop
! =====================================================================
! --- RECHERCHE DES RACINES D'UN POLYNOME PAR LA METHODE --------------
! --- COMPANION MATRIX POLYNOMIAL -------------------------------------
! --- LE POLYNOME ETANT DE LA FORME : ---------------------------------
! --- P(X) = X^N+A_(N-1).X^(N-1)+...A_K.X^K+...+A_1.X+A_0 -------------
!
! KSTOP : COMPORTEMENT EN CAS DE PROBLEME :
!        / 'A' : ON EMET UNE ALARME
!        / 'F' : ON EMET UNE ERREUR FATALE
!        / ' ' : ON N'EMET PAS DE MESSAGE
! DEGRE : DEGRE DU POLYNOME
! AI : COEFFICIENTS DU POLYNOME
! RACINE : RACINES DU POLYNOME
! OUT IER : PARAMETRE  D'ERREUR
!           IER = 0 OK
!           IER = J > 0 , NON CONVERGENCE POUR LA J-IEME RACINE
!                LES J PREMIERES RACINES NE SONT PAS CALCULEES
! =====================================================================
! =====================================================================
! =====================================================================
! =====================================================================
    integer :: ii, ier, icode, ibid, degmax
    parameter   (degmax=4)
    real(kind=8) :: bidon(2*degre), vbid(2*degre)
    real(kind=8) :: comapo(degmax*degmax)
! =====================================================================
! --- INITIALISATIONS ET COHERENCES -----------------------------------
! =====================================================================
    ier = 0
    do 2 ii = 1, 2*degre
        racine(ii) = 0.0d0
 2  end do
! =====================================================================
! --- INITIALISATION DE LA MATRICE ------------------------------------
! =====================================================================
    ASSERT(degre.le.degmax)
    do 10 ii = 1, degmax*degmax
        comapo(ii) = 0.d0
10  end do
! =====================================================================
! --- CREATION DE LA MATRICE DE TYOPE HESSENBERG ----------------------
! =====================================================================
! --- REMPLISSAGE DE LA PREMIERE SOUS DIAGONALE -----------------------
! =====================================================================
    do 20 ii = 1, degre - 1
        comapo(ii*(degre+1)) = 1.0d0
20  end do
    do 30 ii = 1, degre
        comapo(ii*degre) = - ai(ii)
30  end do
    icode = 0
    call vphqrp(comapo, degre, degre, icode, racine,&
                bidon, 1, vbid, 30, ier,&
                ibid)
! =====================================================================
    if (kstop .eq. ' ') goto 40
!
    if (ier .ne. 0) then
        call u2mesi(kstop, 'ALGORITH17_6', 1, ier)
    endif
!
40  continue
! =====================================================================
end subroutine
