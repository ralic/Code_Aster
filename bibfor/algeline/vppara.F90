subroutine vppara(modes, typcon, knega, lraide, lmasse,&
                  lamor, mxresf, neq, nfreq, omecor,&
                  dlagr, dbloq, vectr, vectc, nbpari,&
                  nbparr, nbpark, nopara, mod45, resui,&
                  resur, resuk, ktyp, lcomod, icom1,&
                  icom2, typres, nfreqg)
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexis.h"
#include "asterfort/vpermc.h"
#include "asterfort/vpermo.h"
#include "asterfort/vpnorx.h"
#include "asterfort/vppcom.h"
#include "asterfort/vppfac.h"
#include "asterfort/vppgec.h"
#include "asterfort/vppgen.h"
#include "asterfort/vpstor.h"
#include "asterfort/wpermo.h"
#include "asterfort/wpnorx.h"
#include "asterfort/wppgen.h"
    character(len=4) :: mod45
    character(len=8) :: modes, knega
    character(len=1) :: ktyp
    character(len=16) :: typcon, typres
    character(len=*) :: resuk(*), nopara(*)
    integer :: lraide, lmasse, lamor, mxresf, neq, nfreq, dlagr(*), dbloq(*)
    integer :: resui(*)
    integer :: nbpari, nbparr, nbpark, icom1, icom2, nfreqg
    real(kind=8) :: vectr(*), resur(*), omecor
    complex(kind=8) :: vectc(*)
    aster_logical :: lcomod
!     ------------------------------------------------------------------
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
!
!     CALCUL DES PARAMETRES MODAUX, DES NORMES D'ERREUR ET STOCKAGE DES
!     INFORMATIONS
!     ------------------------------------------------------------------
! IN  MODES    : K8  : NOM UTILISATEUR DU CONCEPT MODAL PRODUIT
! IN  KTYP     : K1  : TYPE DE LA MATRICE DE RAIDEUR
! IN  TYPCON   : K16 : TYPE DE LA STRUCTURE DE DONNEES PRODUITE
! IN  KNEGA    : K8  : VALEUR DU MOT-CLE NUME_MODE_NEGA
! IN  LRAIDE   : IS  : DESCRIPTEUR DE LA MATRICE DE "RAIDEUR"
! IN  LMASSE   : IS  : DESCRIPTEUR DE LA MATRICE DE "MASSE"
! IN  LAMOR    : IS  : DESCRIPTEUR DE LA MATRICE DE "AMORTISSEMENT"
! IN  MXRESF   : IS  : PARAMETRE DE DIMENSIONNEMENT DE RESUR
! IN  NEQ      : IS  : NPMBRE DE DDL
! IN/OUT  NFREQ    : IS  : NOMBRE DE FREQUENCES (OUT SI LCOMOD=.TRUE.)
! IN  FCORIG   : IS  : VALEUR SEUIL EN FREQUNCE
! IN DLAGR     : IS  : POSITION DES DDL DE LAGRANGE
! IN DBLOQ     : IS  : POSITION DES DDL BLOQUES
! IN/OUT VECTR : R   : VECTEURS PROPRES REELS
! IN/OUT VECTC : C   : VECTEURS PROPRES COMPLEXES
! OUT RESUR    : R   : STRUTURE DE DONNEES RESULTAT CONTENANT TOUS LES
!                      PARAMETRES MODAUX
! IN LCOMOD   : LOG  : MACRO_MODE_MECA// OR NOT
! IN ICOM1/ICOM2 :  IN   : PARAMETRES // ASSOCIES A LCOMOD=.TRUE.
! IN TYPRES    :  IN  : TYPE DE RESULTATS (DYNAMIQUE OU FLAMB)
! IN NFREQG    : IN   : NBRE TOTAL DE MODES
!     ------------------------------------------------------------------
!
    integer :: ineg, iprec, iret, ilgcon, nrscr
    real(kind=8) :: rbid
    complex(kind=8) :: zbid
    aster_logical :: lns
!     ------------------------------------------------------------------
!
    call jemarq()
    zbid=(0.d0,0.d0)
    rbid=0.d0
!
!     --- PRISE EN COMPTE DES MODES NEGATIFS ?
    ineg = +1
    if (knega .eq. 'OUI') ineg = -1
!
!     --- PREPARATION AU STOCKAGE DANS LA STRUCTURE DE DONNEES ---
    ilgcon = lxlgut(typcon)
    if (typcon(ilgcon-1:ilgcon) .eq. '_C') ilgcon = ilgcon -2
    call rsexis(modes, iret)
!
    if (lcomod) then
        nrscr=nfreqg
    else
        nrscr=nfreq
    endif
    if (iret .eq. 0) call rscrsd('G', modes, typcon(:ilgcon), nrscr)
    iprec = 0
!
!     --- MATRICE K ET/OU M NON SYMETRIQUE(S)
    if (zi(lraide+4)*zi(lmasse+4) .eq. 0) then
        lns=.true.
    else
        lns=.false.
    endif
!
!
!     --- NORMALISATION ET CALCUL DES PARAMETRES MODAUX ---
!     -----------------------------------------------------
!
    if (( lamor .eq. 0 ) .and. ((ktyp.eq.'R').and.(.not.lns))) then
!
! --- GENERALISE MODES REELS
!
        if (mod45 .ne. 'STAB') then
!
!        - NORMALISATION A LA + GRANDE DES COMPOSANTES /= LAGRANGE --
            call vpnorx(nfreq, neq, dlagr, vectr, resuk)
!
!
!        - CALCUL DES PARAMETRES GENERALISES ---
            call vppgen(lmasse, lamor, lraide, resur(4*mxresf+1), resur(6*mxresf+1),&
                        resur(5*mxresf+1), vectr, neq, nfreq, dbloq)
!
!
!        CALCUL DES FACTEURS DE PARTICIPATIONS ET DES MASSES EFFECTIVES
            call vppfac(lmasse, resur(4*mxresf+1), vectr, neq, nfreq,&
                        mxresf, resur(7*mxresf+1), resur(10*mxresf+1))
!
!
!        - CALCUL DE LA NORME D'ERREUR SUR LE MODE ---
            call vpermo(lmasse, lraide, nfreq, vectr, resur(mxresf+1),&
                        dbloq, omecor, resur(3*mxresf+1))
!
        endif
!        - SI PARALLELISME MACRO_MODE_MECA, COM DES DONNEES:
!        - VECTEURS PROPRES ET DES RESUI/R/K
        call vppcom(lcomod, icom1, icom2, resui, resur,&
                    resuk, nbpari, nbparr, nbpark, mxresf,&
                    vectr, nfreq, neq, typres)
!
!        - STOCKAGE DES VECTEURS PROPRES ---
        call vpstor(ineg, 'R', modes, nfreq, neq,&
                    vectr, [zbid], mxresf, nbpari, nbparr,&
                    nbpark, nopara, mod45, resui, resur,&
                    resuk, iprec)
!
    else if (( lamor .eq. 0 ).and.((ktyp.eq.'C').or.lns)) then
        if (lcomod) ASSERT(.false.)
! --- GENERALISE MODES COMPLEXES
!        - NORMALISATION A LA + GRANDE DES COMPOSANTES /= LAGRANGE --
        call wpnorx(nfreq, neq, dlagr, vectc, resuk)
!
!        - CALCUL DES PARAMETRES GENERALISES ---
        call vppgec(lmasse, lamor, lraide, resur(4*mxresf+1), resur(6* mxresf+1),&
                    resur(5*mxresf+1), vectc, neq, nfreq, dbloq)
!
!        CALCUL DES FACTEURS DE PARTICIPATIONS ET DES MASSES EFFECTIVES
!         CALL VPPFAC(LMASSE,RESUR(4*MXRESF+1),VECTR,NEQ,NFREQ,MXRESF,
!     &               RESUR(7*MXRESF+1),RESUR(10*MXRESF+1))
!
!        - CALCUL DE LA NORME D'ERREUR SUR LE MODE ---
        call vpermc(lmasse, lraide, nfreq, vectc, resur(mxresf+1),&
                    resur( 2*mxresf+1), dbloq, omecor, resur(3*mxresf+1))
!
!        - STOCKAGE DES VECTEURS PROPRES ---
        call vpstor(ineg, 'C', modes, nfreq, neq,&
                    [rbid], vectc, mxresf, nbpari, nbparr,&
                    nbpark, nopara, '    ', resui, resur,&
                    resuk, iprec)
!
    else if (( lamor .ne. 0 ).and.(ktyp.eq.'R')) then
        if (lcomod) ASSERT(.false.)
! --- QUADRATIQUE MODES COMPLEXES AVEC K REELLE
!        - NORMALISATION A LA + GRANDE DES COMPOSANTES /= LAGRANGE --
        call wpnorx(nfreq, neq, dlagr, vectc, resuk)
!
!        - CALCUL DES PARAMETRES GENERALISES ---
        call wppgen(lmasse, lamor, lraide, resur(4*mxresf+1), resur(6* mxresf+1),&
                    resur(5*mxresf+1), vectc, neq, nfreq, dbloq)
!
!        - CALCUL DE LA NORME D'ERREUR SUR LE MODE ---
        call wpermo(lmasse, lraide, lamor, nfreq, vectc,&
                    resur(mxresf+1), resur(2*mxresf+1), dbloq, omecor, resur(3*mxresf+1))
!
!        - STOCKAGE DES VECTEURS PROPRES ---
        call vpstor(ineg, 'C', modes, nfreq, neq,&
                    [rbid], vectc, mxresf, nbpari, nbparr,&
                    nbpark, nopara, '    ', resui, resur,&
                    resuk, iprec)
    else if (( lamor .ne. 0 ).and.(ktyp.eq.'C')) then
        if (lcomod) ASSERT(.false.)
        if (lns) ASSERT(.false.)
! --- QUADRATIQUE MODES COMPLEXES AVEC K COMPLEXE
!        - NORMALISATION A LA + GRANDE DES COMPOSANTES /= LAGRANGE --
        call wpnorx(nfreq, neq, dlagr, vectc, resuk)
!
!        - CALCUL DES PARAMETRES GENERALISES ---
        call wppgen(lmasse, lamor, lraide, resur(4*mxresf+1), resur(6* mxresf+1),&
                    resur(5*mxresf+1), vectc, neq, nfreq, dbloq)
!
!        - CALCUL DE LA NORME D'ERREUR SUR LE MODE ---
        call wpermo(lmasse, lraide, lamor, nfreq, vectc,&
                    resur(mxresf+1), resur(2*mxresf+1), dbloq, omecor, resur(3*mxresf+1))
!
!        - STOCKAGE DES VECTEURS PROPRES ---
        call vpstor(ineg, 'C', modes, nfreq, neq,&
                    [rbid], vectc, mxresf, nbpari, nbparr,&
                    nbpark, nopara, '    ', resui, resur,&
                    resuk, iprec)
    endif
!     ------------------------------------------------------------------
    call jedema()
end subroutine
