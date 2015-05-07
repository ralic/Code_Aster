subroutine dhrc_lc(epsm, deps, vim, pgl, option,&
                   sig, vip, a0, c0, aa_t,&
                   ga_t, ab, gb, ac, gc,&
                   aa_c, ga_c, cstseu, crit, codret,&
                   dsidep)
! aslint: disable=W1504
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sebastien.fayolle at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/coqrep.h"
#include "asterfort/dhrc_calc_a.h"
#include "asterfort/dhrc_calc_b.h"
#include "asterfort/dhrc_calc_c.h"
#include "asterfort/dhrc_calc_g.h"
#include "asterfort/dhrc_calc_n.h"
#include "asterfort/dhrc_calc_q.h"
#include "asterfort/dhrc_jacob.h"
#include "asterfort/dhrc_mat_tan.h"
#include "asterfort/dhrc_sig.h"
#include "asterfort/dxefro.h"
#include "asterfort/jevech.h"
#include "asterfort/matini.h"
#include "asterfort/mgauss.h"
#include "asterfort/r8inir.h"
#include "asterfort/utbtab.h"
#include "asterfort/utmess.h"
#include "blas/dgeev.h"
!
    integer :: codret
!
    real(kind=8) :: epsm(6), deps(6), vim(*), crit(*), cstseu(6)
    real(kind=8) :: a0(6, 6), c0(2, 2, 2)
    real(kind=8) :: aa_t(6, 6, 2), ab(6, 2, 2), ac(2, 2, 2)
    real(kind=8) :: ga_t(6, 6, 2), gb(6, 2, 2), gc(2, 2, 2)
    real(kind=8) :: aa_c(6, 6, 2)
    real(kind=8) :: ga_c(6, 6, 2)
    real(kind=8) :: dsidep(6, 6)
    real(kind=8) :: sig(8), vip(*)
!
    character(len=16) :: option
! ----------------------------------------------------------------------
!
!      LOI GLOBALE POUR LES PLAQUES/COQUES DKTG - DHRC
!
! IN:
!       A0     : RAIDEUR ELASTIQUE (D=0)
!       AA     : PARAMETRE ALPHA DE LA FONCTION D'ENDOMMAGEMENT
!       GA     : PARAMETRE GAMMA DE LA FONCTION D'ENDOMMAGEMENT
!       AB     : PARAMETRE ALPHA DE LA FONCTION D'ENDOMMAGEMENT
!       GB     : PARAMETRE GAMMA DE LA FONCTION D'ENDOMMAGEMENT
!       C0     : RAIDEUR ELASTIQUE (D=0)
!       AC     : PARAMETRE ALPHA DE LA FONCTION D'ENDOMMAGEMENT
!       GC     : PARAMETRE GAMMA DE LA FONCTION D'ENDOMMAGEMENT
!       VIM     : VARIABLES INTERNES EN T-
!       OPTION  : TOUTES
!       CRIT    : CRITERES DE CONVERGENCE LOCAUX
!              (1) = NB ITERATIONS MAXI A CONVERGENCE
!                    (ITER_INTE_MAXI == ITECREL)
!              (2) = TYPE DE JACOBIEN A T+DT
!                    (TYPE_MATR_COMP == MACOMP)
!                     0 = EN VITESSE     >SYMETRIQUE
!                     1 = EN INCREMENTAL >NON-SYMETRIQUE
!              (3) = VALEUR TOLERANCE DE CONVERGENCE
!                    (RESI_INTE_RELA == RESCREL)
!              (5) = NOMBRE D'INCREMENTS POUR LE
!                    REDECOUPAGE LOCAL DU PAS DE TEMPS
!                    (ITER_INTE_PAS  == ITEDEC)
!                    -1,0,1 = PAS DE REDECOUPAGE
!                     N = NOMBRE DE PALIERS
!              (6) = TYPE D INTEGRATION LOCAL POUR LA LOI DE
!                    COMPORTEMENT (ALGO_INTE)
! OUT:
!       SIG     : CONTRAINTE
!               (NXX NYY NXY MXX MYY MXY)
!       VIP     : VARIABLES INTERNES EN T+
!       CODRET  : CODE RETOUR DE L'INTEGRATION DE LA LDC
!                 0 => PAS DE PROBLEME
!                 1 => ABSENCE DE CONVERGENCE
! ----------------------------------------------------------------------
!
    aster_logical :: rigi, resi
    aster_logical :: lelas
!
    integer :: k, itmax, indi(6), nbact, l, i, iret, iter, iter2
!
    integer :: jcara
    blas_int :: info
    real(kind=8) :: wr(6), wi(6), work(18), vl(1), vr(1)
!
    real(kind=8) :: eps(8)
    real(kind=8) :: vint(6)
    real(kind=8) :: a(6, 6), b(6, 2, 2), c(2, 2, 2), ates(6, 6)
    real(kind=8) :: ap1(6, 6), ap2(6, 6), as1(6, 6), as2(6, 6)
    real(kind=8) :: bp1(6, 2), bp2(6, 2), bs1(6, 2), bs2(6, 2)
    real(kind=8) :: cp1(2, 2), cp2(2, 2), cs1(2, 2), cs2(2, 2)
    real(kind=8) :: seuils(6), seuact(6), told
    real(kind=8) :: g1, g2
    real(kind=8) :: neta1(2), neta2(2)
    real(kind=8) :: jacob(6, 6), bocaj(6, 6), det
!
    real(kind=8) :: alpha, beta, cosi, sinu
    real(kind=8) :: pgl(3, 3), t2ev2(2, 2), t2ve2(2, 2), epsg(8), sigg(8)
    real(kind=8) :: t1ve(3, 3), dsideg(6, 6)
    real(kind=8) :: dsidem(3, 3), dsidec(3, 3), dsidef(3, 3)
    real(kind=8) :: dsidmg(3, 3), dsidcg(3, 3), dsidfg(3, 3)
    real(kind=8) :: xab1(3, 3)
    real(kind=8) :: epse(6), epsmg(6)
!
! --  OPTION ET MODELISATION
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    lelas = option .eq.'RIGI_MECA       '
!
! -- INITIALISATION
    if (lelas) then
        call r8inir(6, 0.0d0, epsm, 1)
        call r8inir(6, 0.0d0, deps, 1)
    endif
!
! --  CALCUL DES EPSILON INITIAUX
    if (resi) then
        do k = 1, 6
            eps(k) = epsm(k) + deps(k)
        end do
    else
        do k = 1, 6
            eps(k) = epsm(k)
        end do
    endif
!
    eps(7)=0.0d0
    eps(8)=0.0d0
!
! -- EPS EST FOURNI DANS LE REPERE LOCAL DE L'ELEMENT
!    ON PASSE EPS DANS LE REPERE GLOBAL => EPSG
! -- POUR CE FAIRE ON A BESOIN DE ALPHA ET BETA DONNES PAR ANGLE_REP
!    DANS AFFE_CARA_ELEM
! ---------------------------------------------------------------------
    call jevech('PCACOQU', 'L', jcara)
    alpha = zr(jcara+1) * r8dgrd()
    beta  = zr(jcara+2) * r8dgrd()
    call coqrep(pgl, alpha, beta, t2ev2, t2ve2, cosi, sinu)
!
! ---   PASSAGE DES DEFORMATIONS EPS DU REPERE INTRINSEQUE
! ---   A L'ELEMENT AU REPERE GLOBAL DE LA COQUE
    eps(3)=eps(3)*0.5d0
    eps(6)=eps(6)*0.5d0
    epsm(3)=epsm(3)*0.5d0
    epsm(6)=epsm(6)*0.5d0
!
    call r8inir(8, 0.0d0, epsg, 1)
    call r8inir(6, 0.0d0, epsmg, 1)
!
    call dxefro(1, t2ev2, eps, epsg)
    call dxefro(1, t2ev2, epsm, epsmg)
!
    epsg(3)=epsg(3)*2.d0
    epsg(6)=epsg(6)*2.d0
    epsm(3)=epsm(3)*2.d0
    epsm(6)=epsm(6)*2.d0
    epsmg(3)=epsmg(3)*2.d0
    epsmg(6)=epsmg(6)*2.d0
!
! ---------------------------------------------------------------------
!
! --  INITIALISATION DE D1, D2, EPSP1 ET EPSP2
! --  STOCKAGE DES VARIABLES INTERNES DANS UN VECTEUR VINT
! --  VINT=(D1,D2,EPSP1X,EPSP1Y,EPSP2X,EPSP2Y)
!
    if (lelas) then
        call r8inir(6, 0.0d0, vint, 1)
    else
        do k = 1, 6
            vint(k) = vim(k)
        end do
    endif
!
    do k = 1, 6
        indi(k)=0
    end do
!
    if (resi) then
        iter=0
!
        told = crit(3)
        itmax = nint(crit(1))
!
! --  CALCUL DES TENSEURS DE RAIDEUR A,B,C EN FONCTION DE
!     L'ENDOMMAGEMENT ET DE LEURS DERIVEES PAR RAPPORT A D1 ET D2
!
        call dhrc_calc_b(ab, gb, vint, b, bp1, bp2, bs1, bs2)
        call dhrc_calc_c(c0, ac, gc, vint, c, cp1, cp2, cs1, cs2)
        call dhrc_calc_a(a0, aa_t, ga_t, aa_c, ga_c, epsmg, vim, a, ap1, ap2, as1, as2)
        call dhrc_calc_q(a, b, vint, epsg, epse)
        call dhrc_calc_a(a0, aa_t, ga_t, aa_c, ga_c, epse, vint, a, ap1, ap2, as1, as2)
!
! ----------------------------------------------------------------------
! -------CALCUL DES FORCES THERMODYNAMIQUES -------
! ----------------------------------------------------------------------
!
        call dhrc_calc_n(epsg, vint, b, c, neta1, neta2)
        call dhrc_calc_g(epsg, vint, ap1, bp1, cp1, ap2, bp2, cp2, g1, g2)
!
! ----------------------------------------------------------------------
! -------CALCUL DES SEUILS-------
! ----------------------------------------------------------------------
!
!     SEUILS D'ENDOMMAGEMENT
!
        seuils(1)=g1/cstseu(1)-1.0d0
        seuils(2)=g2/cstseu(2)-1.0d0
!
!     SEUILS DE PLASTICITE
!
        seuils(3)= (neta1(1)/cstseu(3))**2.0d0-1.0d0
        seuils(4)= (neta1(2)/cstseu(4))**2.0d0-1.0d0
        seuils(5)= (neta2(1)/cstseu(5))**2.0d0-1.0d0
        seuils(6)= (neta2(2)/cstseu(6))**2.0d0-1.0d0
!
222     continue
!
! --  COMPTEUR D'ITERATIONS
        iter=iter+1
!
        if (iter .gt. itmax) then
            codret=1
            goto 999
        end if
!
! --  NOMBRE DE SEUILS ACTIVES
        nbact=0
!
! --  CREATION DE L'INDICATRICE DES SEUILS ACTIVES
        do k = 1, 6
            if (seuils(k) .gt. told) then
                indi(k)=k
                nbact=nbact+1
            endif
        end do
!
! --  SI PAS DE SEUILS ATTEINTS, ON PASSE DIRECTEMENT AU CALCUL DES
!     CONTRAINTES
        if (nbact .eq. 0) then
            goto 555
        else
!
! --  SINON
!
! --  CREATION DU VECTEUR SEUILS ACTIVES
            call r8inir(6, 0.0d0, seuact, 1)
            nbact=0
            do k = 1, 6
                if (k .eq. indi(k)) then
                    nbact=nbact+1
                    seuact(nbact)=seuils(k)
                endif
            end do
!
            iter2=0
!-----------------------------------------------------------------------
! --  BOUCLE DE RESOLUTION DE L'EVOLUTION DES VARIABLES --
! ----------------------------------------------------------------------
111         continue
!
! --  COMPTEUR D'ITERATIONS
            iter2=iter2+1
!
            if (iter2 .gt. itmax) then
                codret=1
                goto 999
            endif
!
! --  CALCUL DE LA JACOBIENNE => JACOB(NBACT,NBACT)
!
            call matini(6, 6, 0.0d0, jacob)
!
            call dhrc_jacob(epsg, vint, c, bp1, cp1, bp2, cp2, as1, bs1,&
                            cs1, as2, bs2, cs2, indi, neta1, neta2, cstseu, jacob)
!
!
! --  VERIFICATION DE LA CONVEXITE DE L'ENERGIE LIBRE
! --  POUR CE FAIRE ON REGARDE SI LA JACOBIENNE EST DEFINIE POSITIVE
!
! --  INVERSION DE LA JACOBIENNE => BOCAJ(NBACT,NBACT)
!
            call matini(6, 6, 0.0d0, bocaj)
!
            do k = 1, nbact
                bocaj(k,k)=1.0d0
            end do
!
            call mgauss('NFSP', jacob, bocaj, 6, nbact, 6, det, iret)
!
! --  MISE A JOUR DES VARIABLES INTERNES
            l=0
            do k = 1, 6
                if (k .eq. indi(k)) then
                    l=l+1
                    do i = 1, nbact
                        vint(k)=vint(k)-bocaj(l,i)*seuact(i)
                    end do
                endif
            end do
!
! --  VERIFICATION DE LA CROISSANCE DES D1 ET D2
            if (vint(1) .lt. 0.0d0) then
                vint(1)=vim(1)
            endif
!
            if (vint(2) .lt. 0.0d0) then
                vint(2)=vim(2)
            endif
!
            if ((indi(1).eq.1) .or. (indi(2).eq.2)) then
! --  CALCUL DES TENSEURS DE RAIDEUR A,B,C EN FONCTION DE
!     L'ENDOMMAGEMENT ET DE LEURS DERIVEES PAR RAPPORT A D1 ET D2
!
                call dhrc_calc_b(ab, gb, vint, b, bp1, bp2, bs1, bs2)
                call dhrc_calc_c(c0, ac, gc, vint, c, cp1, cp2, cs1, cs2)
                call dhrc_calc_a(a0, aa_t, ga_t, aa_c, ga_c, epsmg, vim, a, ap1, ap2, as1, as2)
                call dhrc_calc_q(a, b, vint, epsg, epse)
                call dhrc_calc_a(a0, aa_t, ga_t, aa_c, ga_c, epse, vint, a, ap1, ap2, as1, as2)
            endif
!
! --  CALCUL DES SEUILS AVEC VARIABLES ACTUALISEES
!
! ----------------------------------------------------------------------
! ----CALCUL DES FORCES THERMODYNAMIQUES AVEC VARIABLES ACTUALISEES-----
! ----------------------------------------------------------------------
!
            call dhrc_calc_n(epsg, vint, b, c, neta1, neta2)
            call dhrc_calc_g(epsg, vint, ap1, bp1, cp1, ap2, bp2, cp2, g1, g2)
!
! ----------------------------------------------------------------------
! -------CALCUL DES SEUILS AVEC VARIABLES ACTUALISEES-------
! ----------------------------------------------------------------------
!
!     SEUILS D'ENDOMMAGEMENT
!
            seuils(1)=g1/cstseu(1)-1.0d0
            seuils(2)=g2/cstseu(2)-1.0d0
!
!     SEUILS DE PLASTICITE
!
            seuils(3)= (neta1(1)/cstseu(3))**2.0d0-1.0d0
            seuils(4)= (neta1(2)/cstseu(4))**2.0d0-1.0d0
            seuils(5)= (neta2(1)/cstseu(5))**2.0d0-1.0d0
            seuils(6)= (neta2(2)/cstseu(6))**2.0d0-1.0d0
!
            l=0
            do k = 1, 6
                if (k .eq. indi(k)) then
                    l=l+1
                    seuact(l)=seuils(k)
                endif
            end do
!
            do i = 1, nbact
                if (seuact(i) .gt. told) then
                    goto 111
                endif
            end do
!
! ----------------------------------------------------------------------
        endif
!
! ----------------------------------------------------------
! -- VERIFICATION QUE D'AUTRES SEUILS NE SONT PAS ACTIVES --
! ----------------------------------------------------------
        goto 222
!
555     continue
!
        do k = 1, 6
            vip(k)=vint(k)
        end do
! --  CALCUL DE LA DISSIPATION
        vip(7)=(vip(1)*cstseu(1)+vip(2)*cstseu(2))
        vip(8)=vim(8)+(abs(vip(3)-vim(3))*cstseu(3)+abs(vip(4)-vim(4))*cstseu(4)&
                      +abs(vip(5)-vim(5))*cstseu(5)+abs(vip(6)-vim(6))*cstseu(6))
        vip(10)=1.d0-(a(1,1)*a(2,2)*a(3,3))**(1.d0/3.d0)/(a0(1,1)*a0(2,2)*a0(3,3))**(1.d0/3.d0)
        vip(11)=1.d0-(a(4,4)*a(5,5)*a(6,6))**(1.d0/3.d0)/(a0(4,4)*a0(5,5)*a0(6,6))**(1.d0/3.d0)
!
    else
        do k = 1, 11
            if (lelas) then
                vip(k)=0.0d0
            else
                vip(k)=vim(k)
            endif
        end do
    endif
!
    do k = 1, 2
        if (vip(k) .lt. vim(k)) then
            codret=1
            goto 999
        endif
    end do
!
    call dhrc_calc_b(ab, gb, vip, b, bp1, bp2, bs1, bs2)
    call dhrc_calc_c(c0, ac, gc, vip, c, cp1, cp2, cs1, cs2)
    call dhrc_calc_a(a0, aa_t, ga_t, aa_c, ga_c, epsmg, vim, a, ap1, ap2, as1, as2)
    call dhrc_calc_q(a, b, vint, epsg, epse)
    call dhrc_calc_a(a0, aa_t, ga_t, aa_c, ga_c, epse, vip, a, ap1, ap2, as1, as2)
    call dhrc_calc_n(epsg, vip, b, c, neta1, neta2)
!
    if (resi) then
! --  CALCUL DES CONTRAINTES
        call dhrc_sig(epsg, vip, a, b, sigg)
    endif
!
! ----------------------------------------------------------------------
! --  CALCUL DE LA MATRICE TANGENTE
! ----------------------------------------------------------------------
!
    nbact=0
    do k = 1, 6
        if (indi(k) .eq. k) then
            nbact=nbact+1
        endif
    end do
!
    if ((.not.rigi) .or. (nbact.eq.0)) then
!
! --  CALCUL DE LA MATRICE ELASTIQUE
        do k = 1, 6
            do i = 1, 6
                dsideg(k,i)=a(k,i)
            end do
        end do
!
    else
!
! --  CALCUL DE LA MATRICE TANGENTE
!
! --  CALCUL DE LA JACOBIENNE => JACOB(NBACT,NBACT)
!
        call matini(6, 6, 0.0d0, jacob)
!
        call dhrc_jacob(epsg, vip, c, bp1, cp1, bp2, cp2, as1, bs1,&
                        cs1, as2, bs2, cs2, indi, neta1, neta2, cstseu, jacob)
!
! --  INVERSION DE LA JACOBIENNE => BOCAJ(NBACT,NBACT)
!
        call matini(6, 6, 0.0d0, bocaj)
!
        do k = 1, nbact
            bocaj(k,k)=1.0d0
        end do
!
        call mgauss('NFSP', jacob, bocaj, 6, nbact, 6, det, iret)
!
        call dhrc_mat_tan(a, ap1, ap2, b, bp1, bp2, bocaj, neta1, neta2, indi,&
                          cstseu, epsg, vip, dsideg)
    endif
!
    call matini(6, 6, 0.0d0, ates)
!
    do k = 1, 6
        do i = 1, 6
            ates(k,i)=dsideg(k,i)
        end do
    end do
!
    call dgeev('N', 'N', 6, ates, 6, wr, wi, vl, 1, vr, 1, work, 18, info)
!
!     ECRITURE DES VALEURS PROPRES
!
    do k = 1, 6
        if (wr(k) .lt. 0.0d0) call utmess('A', 'COMPOR4_71', si=k, sr=wr(k))
    end do
!
! ---  PASSAGE DES EFFORTS GENERALISES SIGG DU REPERE GLOBAL DE LA COQUE
! ---  AU REPERE INTRINSEQUE A L'ELEMENT => SIG
    if (resi) then
        call r8inir(8, 0.0d0, sig, 1)
        call dxefro(1, t2ve2, sigg, sig)
    endif
!
! ---  PASSAGE DE LA MATRICE TANGENTE DSIDEG DU REPERE GLOBAL DE LA
! ---  COQUE AU REPERE INTRINSEQUE A L'ELEMENT => DSIDEP
!      CALCUL DE LA MATRICE T1VE DE PASSAGE D'UNE MATRICE
!      (3,3) DU REPERE DE LA VARIETE AU REPERE ELEMENT
    t1ve(1,1) = cosi*cosi
    t1ve(1,2) = sinu*sinu
    t1ve(1,3) = cosi*sinu
    t1ve(2,1) = t1ve(1,2)
    t1ve(2,2) = t1ve(1,1)
    t1ve(2,3) =-t1ve(1,3)
    t1ve(3,1) =-t1ve(1,3)*2.d0
    t1ve(3,2) = t1ve(1,3)*2.d0
    t1ve(3,3) = t1ve(1,1)-t1ve(1,2)
!
    call matini(3, 3, 0.0d0, dsidmg)
    call matini(3, 3, 0.0d0, dsidcg)
    call matini(3, 3, 0.0d0, dsidfg)
!
    do k = 1, 3
        do l = 1, 3
!     MEMBRANE
            dsidmg(k,l)=dsideg(k,l)
!     COUPLAGE MEMBRANE-FLEXION
            dsidcg(k,l)=dsideg(k,l+3)
!     FLEXION
            dsidfg(k,l)=dsideg(k+3,l+3)
        end do
    end do
!
    call utbtab('ZERO', 3, 3, dsidmg, t1ve, xab1, dsidem)
    call utbtab('ZERO', 3, 3, dsidcg, t1ve, xab1, dsidec)
    call utbtab('ZERO', 3, 3, dsidfg, t1ve, xab1, dsidef)
!
    call matini(6, 6, 0.0d0, dsidep)
!
    do k = 1, 3
        do l = 1, 3
            dsidep(k,l)=dsidem(k,l)
            dsidep(k,l+3)=dsidec(k,l)
            dsidep(l+3,k)=dsidec(k,l)
            dsidep(k+3,l+3)=dsidef(k,l)
        end do
    end do
!
    vip(9)=vip(7)+vip(8)
!
999 continue
end subroutine
