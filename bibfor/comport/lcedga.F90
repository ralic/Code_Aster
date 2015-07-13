subroutine lcedga(fami, kpg, ksp, ndim, imat,&
                  crit, typmod, instam, instap, coord,&
                  deps2, sigm2, vim, option, sigp,&
                  vip, dsidep, iret)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/edgani.h"
#include "asterfort/edgequ.h"
#include "asterfort/edgini.h"
#include "asterfort/edgmat.h"
#include "asterfort/edgrep.h"
#include "asterfort/mgauss.h"
#include "asterfort/rcvarc.h"
#include "asterfort/verift.h"
#include "asterfort/get_meta_type.h"
#include "asterfort/get_meta_phasis.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    integer, intent(in) :: ndim
    integer, intent(in) :: imat
    real(kind=8), intent(in) :: crit(*)
    character(len=8), intent(in) :: typmod(2)
    real(kind=8), intent(in) :: instam
    real(kind=8), intent(in) :: instap
    real(kind=8), intent(in) :: coord(3)
    real(kind=8), intent(in) :: deps2(*)
    real(kind=8), intent(in) :: sigm2(*)
    real(kind=8), intent(in) :: vim(2)
    character(len=16), intent(in) :: option
    real(kind=8), intent(out) :: sigp(*)
    real(kind=8), intent(out) :: vip(2)
    real(kind=8), intent(out) :: dsidep(6, 6)
    integer, intent(out) :: iret
!
! --------------------------------------------------------------------------------------------------
!
! Comportment
!
! META_LEMA_ANI
!
! --------------------------------------------------------------------------------------------------
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  IMAT    : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  COORD   : COORDONNEES DU POINT DE GAUSS
! IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
! IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
! IN  EPSM2   : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT*SQRT(2)
! IN  DEPS2   : INCREMENT DE DEFORMATION*SQRT(2)
! IN  SIGM2   : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT*SQRT(2)
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
!
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL*SQRT(2)
! OUT VIP     : DEUX VARIABLES INTERNES A L'INSTANT ACTUEL
!               VIP(1)=DEFORMATION PLASTIQUE CUMULEE
!               VIP(2)= INDICE D ELASTICITE MEME SI SANS SEUIL
! OUT DSIDEP  : MATRICE CARREE
!     IRET    : CODE RETOUR DE LA RESOLUTION DE L'EQUATION SCALAIRE
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ECHEC
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX YY ZZ SQRT(2)XY SQRT(2)XZ SQRT(2)YZ
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, j, k, nb_phasis, ndimsi, meta_type
    integer :: ire2
    integer :: iter, itemax
    real(kind=8) :: tm, tp, tref, temp, dt
    real(kind=8) :: phase(5), phasm(5), zalpha
    real(kind=8) :: zero, prec, rbid, tole_bound
    real(kind=8) :: mum, mu, troiskm, troisk, anic(6, 6)
    real(kind=8) :: ani(6, 6)
    real(kind=8) :: m(3), n(3), gamma(3), depsth
    real(kind=8) :: deps(2*ndim), sigm(2*ndim)
    real(kind=8) :: trdeps, trsigm, trsigp, epsthe(3)
    real(kind=8) :: dvdeps(2*ndim), dvepel(2*ndim)
    real(kind=8) :: dvsigm(2*ndim), dvsitr(2*ndim), dvsigp(2*ndim)
    real(kind=8) :: eqsitr, eqeptr
    real(kind=8) :: pm, dp
    real(kind=8) :: y(2*ndim+1), g(2*ndim+1), maxg, dgdy(2*ndim+1, 2*ndim+1)
    real(kind=8) :: vect(2*ndim), mat(2*ndim+1, 2*ndim+1)
    real(kind=8) :: r1(2*ndim+1, 2*ndim), h1(2*ndim, 2*ndim)
    character(len=1) :: poum
    aster_logical :: resi, rigi
    logical :: zcylin
    real(kind=8), parameter :: kron(6) = (/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/)
!
! LEXIQUE SUR LE NOM DES VARIABLES VALABLES DANS TOUTES LES ROUTINES
! INDICE I QUAND SOMMATION SUR LA DIMENSION DE L ESPACE
! INDICE K QUAND SOMMATION SUR LES TROIS PHASES
! TRX POUR LA TRACE DE X/3
! DVX POUR LA PARTIE DEVIATORIQUE DE X
! EQX POUR EQUIVALENT AU SENS DE HILL
!
! --------------------------------------------------------------------------------------------------
!
    do i = 1, 2*ndim
        sigp(i) = 0.d0
    end do
    vip(1:2)        = 0.d0
    dsidep(1:6,1:6) = 0.d0
    iret            = 0
    ndimsi          = 2*ndim
    resi            = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi            = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
    dt              = instap-instam
    zero            = 100.d0*r8prem()
    tole_bound      = 100.d0*r8prem()
!
! - Get temperatures
!
    call rcvarc('F', 'TEMP', '-', fami, kpg,&
                ksp, tm, ire2)
    call rcvarc('F', 'TEMP', 'REF', fami, 1,&
                1, tref, ire2)
    call rcvarc('F', 'TEMP', '+', fami, kpg,&
                ksp, tp, ire2)
    if (resi) then
        temp=tp
        poum='+'
    else
        temp=tm
        poum='-'
    endif
!
! - Get metallurgy type
!
    call get_meta_type(meta_type, nb_phasis)
    ASSERT(meta_type.eq.2)
    ASSERT(nb_phasis.eq.3)
!
! - Get phasis
!
    if (resi) then
        call get_meta_phasis(fami     , '+'  , kpg   , ksp , meta_type,&
                             nb_phasis, phase, zcold_ = zalpha, tole_bound_ = tole_bound)
        call get_meta_phasis(fami     , '-'  , kpg   , ksp , meta_type,&
                             nb_phasis, phasm)
    else
        call get_meta_phasis(fami     , '-'  , kpg   , ksp , meta_type,&
                             nb_phasis, phase, zcold_ = zalpha, tole_bound_ = tole_bound)
    endif
!
! **************************************
! 2 - RECUPERATION DES CARACTERISTIQUES
! **************************************
! ATTENTION CAR LA MATRICE D ANISOTROPIE EST DONNEE DANS LE
! REPERE (R - T - Z) DONC IL FAUT FAIRE UN CHANGEMENT DE REPERE
! EN AXI C EST SIMPLE CAR IL SUFFIT D INVERSER LES TERMES 2 ET 3
!
    call edgmat(fami   , kpg   , ksp   , imat  , poum ,&
                zalpha , temp  , dt    , mum   , mu ,&
                troiskm, troisk, anic  , m     , n  ,&
                gamma  , zcylin)
!
! CHANGEMENT DE REPERE DE LA MATRICE D ANISOTROPIE
! SEULEMENT SI ON EST EN COORDONNEES CYLINDRIQUES
!
    if (zcylin) then
        call edgrep(typmod, coord, anic, ani)
    else
        do i = 1,6
            do k = 1,6
                ani(i,k) = anic(i,k)
            end do
        end do
    endif
!
    if (resi) then
!
! ******************************************************
! 3 - PREPARATION DE L ALGORITHME
!     SEPARATION DES PARTIES SPHERIQUE ET DEVIATORIQUE
!     DE LA CONTRAINTE
!     SEULE LA PARTIE DEVIATORIQUE EST INCONNUE
! *****************************************************
!
! 3.1 - JE PREFERE REPASSER LES CONTRAINTES ET DEFORMATIONS
!       SANS LE SQRT(2)
!
        do i = 1, ndimsi
            sigm(i)=sigm2(i)
            deps(i)=deps2(i)
            if (i .ge. 4) then
                sigm(i)=sigm2(i)/sqrt(2.d0)
                deps(i)=deps2(i)/sqrt(2.d0)
            endif
        end do
!
! 3.2 - TRACE
!
        call verift(fami, kpg, ksp, 'T', imat,&
                    vepsth=epsthe)
        depsth = phase(nb_phasis)*epsthe(1) + zalpha*epsthe(2)
        trdeps = (deps(1)+deps(2)+deps(3))/3.d0
        trsigm = (sigm(1)+sigm(2)+sigm(3))/3.d0
        trsigp = trsigm*troisk/troiskm + troisk*(trdeps-depsth)
!
! 3.3 - DEVIATEUR DE LA CONTRAINTE ESSAI CONNUE DVSITR
!
        do i = 1, ndimsi
            dvdeps(i) = deps(i) - trdeps * kron(i)
            dvsigm(i) = sigm(i) - trsigm * kron(i)
        end do
        do i = 1, ndimsi
            dvsitr(i) = mu*dvsigm(i)/mum + 2.d0*mu*dvdeps(i)
        end do
!
! 3.4 - CONTRAINTE EQUIVALENTE ESSAI CONNUE EQSITR
!
        eqsitr = edgequ (ndimsi,dvsitr,ani)
        eqeptr = eqsitr/(2.d0*mu)
        pm=vim(1)
!
! ************************
! 4 - RESOLUTION
! ************************
! 4.1 - SI LA CONTRAINTE EQUIVALENTE ESSAI EST NULLE
!       ALORS SIGP=DVSITR + TRSIGP ET VIP(1)=VIM(1)
!
        if (eqeptr .le. 1.d-05) then
            do i = 1, ndimsi
                sigp(i) = dvsitr(i)+trsigp*kron(i)
            end do
            vip(1)=vim(1)
            vip(2)=0.d0
        else
!
! 4.2 - SYSTEME NON LINEAIRE A RESOUDRE EN [DVEPEL,DP]
!       TEL QUE DVSIGP=2*MU*DVEPEL
!       DVEPEL PLUTOT QUE DVSIGP CAR MEME UNITE QUE DP
!       G(Y)=0
!       DIM=NDIMSI+1
!       Y(1)=DVEPEL(1)
!       Y(2)=DVEPEL(2)
!       Y(3)=DVEPEL(3)
!       Y(4)=DVEPEL(4)
!       Y(5)=DVEPEL(5) (EN 3D)
!       Y(6)=DVEPEL(6) (EN 3D)
!       Y(DIM) = DP
!       POUR I ET J = 1 A NDIMSI
!       G(I)=Y(I)+Y(DIM)*ANI(I,J)*Y(J)/EQEPSEL-DVEPTR(I)
!       G(DIM)=EQEPSEL-GAMMA(K)*((PM+Y(DIM))**M(K))*(Y(DIM)**N(K))
!
!       CE SYSTEME EST RESOLU PAR UNE METHODE DE NEWTON
!       DG(I)/DY(J)*DY(J)=-G(I) => DY(I)=-(DG(I)/DY(J)**-1)*G(J)
!
!      L INVERSION DU SYSTEME EST FAITE DANS MGAUSS
!      MGAUSS RESOUD AX=B
!      EN ENTREE A ET B
!      EN SORTIE (A**-1)*B STOCKEE DANS B
!      CORRESPONDANCE A=DG/DY
!                     B=-G D OU G=-G EN FAIT
!                     X=DY
!
! 4.2.1 - INITIALISATION DE LA METHODE DE NEWTON
!         CALCUL DE LA SOLUTION DU MODELE EDGAR
!         CORRESPONDANT A LA MATRICE ANI ISOTROPE
!         ON SE RAMENE A UNE SEULE EQUATION EN DP
!
            itemax=nint(crit(1))
            prec=crit(3)
!
            call edgini(itemax, prec, pm, eqsitr, mu,&
                        gamma, m, n, dp, ire2)
            if (ire2 .gt. 0) then
                iret = 1
                goto 998
            endif
!
            do i = 1, ndimsi
                dvsigp(i) = (1.d0-3.d0*mu*dp/eqsitr)*dvsitr(i)
                dvepel(i)= dvsigp(i)/(2.d0*mu)
                y(i)=dvepel(i)
            end do
            y(ndimsi+1)=dp
!
! 4.2.2 - CALCUL DE G SA DERIVEE ET LE CRITERE D ARRET
!         LE CRITERE D ARRET EST LE MAX DE G
!
            call edgani(ndimsi+1, y, pm, dvsitr, eqsitr,&
                        mu, ani, gamma, m, n,&
                        g, maxg, dgdy)
!
! 4.2.3 - ITERATION DE NEWTON
! ATTENTION SI W MATRICE DGDY MODIFIE
!
            do iter = 1, itemax
                if (maxg .le. prec) goto 999
!
                call mgauss('NFSP', dgdy, g, ndimsi+1, ndimsi+1,&
                            1, rbid, ire2)
!
                if (ire2 .gt. 0) then
                    iret = 1
                    goto 998
                endif
!
                do i = 1, ndimsi+1
                    y(i)=y(i)+g(i)
                    if (i .le. ndimsi) dvsigp(i)=2.d0*mu*y(i)
                end do
!
                if (y(ndimsi+1) .le. 0.d0) then
                    iret = 1
                    goto 998
                endif
!
                call edgani(ndimsi+1, y, pm, dvsitr, eqsitr,&
                            mu, ani, gamma, m, n,&
                            g, maxg, dgdy)
!
            end do
!
            iret = 1
            goto 998
!
999         continue
!
! 4.2.3 - CALCUL DE SIGMA ET P
!
            do i = 1, ndimsi
                sigp(i) = dvsigp(i)+trsigp*kron(i)
            end do
            dp=y(ndimsi+1)
            vip(1)=vim(1)+dp
            vip(2)=1.d0
        endif
!
    endif
!
! *******************************
! 5 - MATRICE TANGENTE DSIGDE
! *******************************
! SI RIGI               => MATRICE ELASTIQUE A TM
! SI FULL MAIS VIP(2)=0 => MATRICE ELASTIQUE A TP
! SI FULL MAIS VIP(2)=1 => MATRICE COHERENTE A TP
!
    if (rigi) then
        if ((option(1:4).eq.'RIGI') .or. ((option(1:4).eq.'FULL').and.( vip(2).le.0.5d0))) then
!
            do i = 1, ndimsi
                do j = 1, ndimsi
                    dsidep(i,j)=0.d0
                end do
            end do
!
            do i = 1, ndimsi
                if (i .le. 3) dsidep(i,i)=(4.d0*mu/3.d0)+troisk/3.d0
                if (i .gt. 3) dsidep(i,i)=2.d0*mu
            end do
!
            do i = 1, 3
                do j = 1, 3
                    if (i .ne. j) dsidep(i,j)=(-2.d0*mu/3.d0)+troisk/ 3.d0
                end do
            end do
        endif
!
        if ((option(1:4).eq.'FULL') .and. (vip(2).ge.0.5d0)) then
!
            do j = 1, ndimsi
                do i = 1, ndimsi+1
                    r1(i,j)=0.d0
                end do
                r1(j,j)=1.d0
            end do
!
            do i = 1, ndimsi+1
                do k = 1, ndimsi+1
                    mat(i,k)=dgdy(i,k)
                    if (k .ge. 4) mat(i,k)=mat(i,k)/2.d0
                end do
            end do
!
            call mgauss('NFSP', mat, r1, ndimsi+1, ndimsi+1,&
                        ndimsi, rbid, ire2)
            if (ire2 .gt. 0) then
                iret = 1
                goto 998
            endif
!
            do j = 1, ndimsi
                do i = 1, ndimsi
                    h1(i,j)=r1(i,j)
                end do
            end do
!
! ON COMPLETE
!
            do i = 1, ndimsi
                vect(i)=h1(i,1)+h1(i,2)+h1(i,3)
                vect(i)=-2.d0*mu*vect(i)
                if (i .le. 3) vect(i)=vect(i)+troisk
                vect(i)=vect(i)/3.d0
            end do
!
            do i = 1, ndimsi
                do j = 1, ndimsi
                    h1(i,j)=2.d0*mu*h1(i,j)
                    if (j .le. 3) h1(i,j)=h1(i,j)+vect(i)
                end do
            end do
!
! ON AFFECTE H1 A DSIDEP AVEC LES RACINE DE 2 POUR I NE J
!
            do i = 1, ndimsi
                do j = 1, ndimsi
                    dsidep(i,j)=h1(i,j)
                    if ((i.eq.j) .and. (i.ge.4)) dsidep(i,j)=2.d0* dsidep(i,j)/4.d0
                    if ((i.ne.j) .and. ((i.ge.4).or.(j.ge.4))) then
                        dsidep(i,j)=sqrt(2.d0)*dsidep(i,j)/2.d0
                    endif
                end do
            end do
        endif
    endif
!
! *************************************
! 6 - ON REPASSE SIGP AVEC LE SQRT(2)
! *************************************
!
    if (resi) then
        do i = 4, ndimsi
            sigp(i)=sigp(i)*sqrt(2.d0)
        end do
    endif
!
998 continue
!
end subroutine
