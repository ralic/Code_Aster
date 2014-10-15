subroutine lcecli(fami, kpg, ksp, ndim, mate,&
                  option, lamb, saut, sigma, dsidep,&
                  vim, vip, r)
!
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
! person_in_charge: patrick.massin at edf.fr
!
! aslint: disable=W1306
    implicit none
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: mate, ndim, kpg, ksp
    real(kind=8) :: lamb(ndim), saut(ndim), sigma(6), dsidep(6, 6)
    real(kind=8) :: vim(*), vip(*), r, laug(ndim)
    character(len=16) :: option
    character(len=*) :: fami
!
!-----------------------------------------------------------------------
! LOI DE COMPORTEMENT COHESIVE : CZM_LIN_MIX
! Elements cohésifs X-FEM
!
! In : fami     => Schéma d'intégration
! In : kpg, ksp => numéro point et sous-point d'intégration
! In : ndim     => Dimension de l'espace
! In : mate     => Matériau
! In : option   => Option de calcul
! In : lamb     => Champ lambda (en base locale)
! In : saut     => Saut de deplacement (base locale)
! Out: sigma    => Contrainte cohésive (base locale)
! Out: dsidep   => Dérivée de la contrainte par rapport
!                  au multiplicateur augmenté (base locale)
! In : vim      => Variables internes
! Out: vip      => Variables internes actualisées
! Out: r        => Paramètre d'augmentation
!-----------------------------------------------------------------------
!
    aster_logical :: resi, rigi, elas
    integer :: i, j, diss, cass
    real(kind=8) :: sc, gc, lc, val(3), rtan, kapp, zero
    real(kind=8) :: na, ka, kap, rk, ra, coef, coef2
    integer :: cod(3)
    character(len=16) :: nom(3)
    character(len=1) :: poum
    parameter  (zero = 0.d0)
!
! OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE
!
    resi = option(1:9).eq.'FULL_MECA' .or. option.eq.'RAPH_MECA'
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
    elas = option.eq.'FULL_MECA_ELAS' .or. option.eq.'RIGI_MECA_ELAS'
!
!
! les sauts et lambda sont deja ceux a l instant que l on veut calculer
!
!
! RECUPERATION DES PARAMETRES PHYSIQUES
!
    nom(1) = 'GC'
    nom(2) = 'SIGM_C'
    nom(3) = 'PENA_LAGR'
!
    if (option .eq. 'RIGI_MECA_TANG') then
        poum = '-'
    else
        poum = '+'
    endif
!
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'RUPT_FRAG', 0, ' ', [0.d0],&
                3, nom, val, cod, 2)
!
    gc = val(1)
    sc = val(2)
    r = val(3)*sc*sc/gc
    lc = 2*gc/sc
!
! INITIALISATION
!
    ka = max(sc,vim(1))
    rtan = 0.d0
!
! --- CALCUL DE LA FORCE COHESIVE AUGMENTEE
!
    do 1 i=1,ndim
        laug(i) = lamb(i) + r*saut(i)
1   end do
!
! --- FORCE COHESIVE EQUIVALENTE
!
    do 10 i = 2, ndim
        rtan=rtan+laug(i)**2
10  end do
    na = sqrt( max(zero,laug(1))**2 + rtan )
!
! --- MODULE TANGENT POUR LES REGIMES ELASTIQUES
!
    rk = sc/ka + ( 1.d0/(r*lc/sc-1.d0) )*(sc/ka-1.d0)
!
!
! INITIALISATION COMPLEMENTAIRE POUR RIGI_MECA_TANG (SECANTE PENALISEE)
!
    if (.not. resi) then
!
        if (elas) then
            diss = 0
        else
            diss = nint(vim(2))
        endif
!
        cass = nint(vim(3))
!
        goto 5000
    endif
!
! CALCUL DE LA CONTRAINTE
!
    call r8inir(6, 0.d0, sigma, 1)
!
!     CONTRAINTE DE CONTACT : PARTIE NEGATIVE ET NORMALE DE LA FORCE AUGMENTEE
!
    sigma(1) = min(zero,laug(1))
!
!     CONTRAINTE DE FISSURATION
!
    if ( (na.ge.(lc*r)) .or. (ka.ge.(lc*r)) ) then
!
        diss = 0
        cass = 2
!
    else
!
        if (na .le. ka) then
!
            diss = 0
            if (ka .gt. sc) then
                cass = 1
            else
                cass = 0
            endif
            sigma(1) = sigma(1) + rk*max(zero,laug(1))
            do 20 i = 2, ndim
                sigma(i) = sigma(i) + rk*laug(i)
20          continue
!
        else
!
            diss = 1
            cass = 1
            ra = sc/na + ( 1.d0/(r*lc/sc-1.d0) ) * (sc/na-1.d0)
            sigma(1) = sigma(1) + ra*max(zero,laug(1))
            do 30 i = 2, ndim
                sigma(i) = sigma(i) + ra*laug(i)
30          continue
!
        endif
!
    endif
!
! ACTUALISATION DES VARIABLES INTERNES
!
! on conserve la distinction à suivre
    kap = max(ka,na)
    kapp = max(vim(1),na)
    vip(1) = kapp
    vip(2) = diss
    vip(3) = cass
!
! --- VARIABLE INTERNES DE POST-TRAITEMENT PAS REMPLIES POUR L INSTANT
!
! -- MATRICE TANGENTE
!
5000  continue
    if (.not. rigi) goto 9999
!
    call r8inir(36, 0.d0, dsidep, 1)
!
!    MATRICE TANGENTE DE CONTACT
!
    if (laug(1) .le. 0.d0) dsidep(1,1) = dsidep(1,1) + 1.d0
!
! DANS LE CAS OU L'ELEMENT EST TOTALEMENT CASSE ON INTRODUIT UNE
! RIGIDITE ARTIFICIELLE DANS LA MATRICE TANGENTE POUR ASSURER
! LA CONVERGENCE
!
    if (cass .eq. 2) then
        if (laug(1) .gt. 0.d0) dsidep(1,1) = dsidep(1,1) - 1.d-8
        do 39 i = 2, ndim
            dsidep(i,i) = dsidep(i,i) - 1.d-8
39      continue
        goto 9999
    endif
!
!    MATRICE TANGENTE DE FISSURATION
    if ((diss.eq.0) .or. elas) then
!
        if (laug(1) .gt. 0.d0) dsidep(1,1) = dsidep(1,1) + rk
!
        do 40 i = 2, ndim
            dsidep(i,i) = dsidep(i,i) + rk
40      continue
!
    else
!
        coef = sc/na + ( 1.d0/(r*lc/sc-1.d0) ) * (sc/na-1.d0)
        coef2 = -sc*(1.d0 + 1.d0/(r*lc/sc-1.d0) )/(na**3)
!
        if (laug(1) .le. 0.d0) then
!
            do 42 i = 2, ndim
                dsidep(i,i) = dsidep(i,i) + coef + coef2*laug(i)*laug(i)
42          continue
!
            if (ndim .eq. 3) then
                dsidep(2,3) = dsidep(2,3) + coef2*laug(2)*laug(3)
                dsidep(3,2) = dsidep(3,2) + coef2*laug(3)*laug(2)
            endif
!
        else
!
            do 44 i = 1, ndim
                dsidep(i,i) = dsidep(i,i) + coef + coef2*laug(i)*laug(i)
44          continue
!
            do 46 j = 1, ndim-1
                do 47 i = j+1, ndim
                    dsidep(j,i) = dsidep(j,i) + coef2*laug(j)*laug(i)
                    dsidep(i,j) = dsidep(i,j) + coef2*laug(i)*laug(j)
47              continue
46          continue
!
        endif
!
    endif
9999  continue
!
!
end subroutine
