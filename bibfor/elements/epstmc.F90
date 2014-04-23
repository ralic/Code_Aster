subroutine epstmc(fami, ndim, instan, poum, igau,&
                  isgau, xyzgau, repere, mater, option,&
                  epsth)
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
!.======================================================================
    implicit none
!
!      EPSTMC :   CALCUL DES DEFORMATIONS THERMIQUES / HYDRIQUE (RETRAIT
!                 ENDOGENE) / DE SECHAGE (RETRAIT DE DESSICCATION)
!                 POUR LES ELEMENTS ISOPARAMETRIQUES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    FAMI           IN     K4       FAMILLE DU POINT DE GAUSS
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    POUM           IN     K1       T+ OU T-
!    IGAU           IN     I        POINT DE GAUSS
!    ISGAU          IN     I        SOUS-POINT DE GAUSS
!    XYZGAU         IN     R        COORDONNEES DU POINT DE GAUSS
!    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    MATER          IN     I        MATERIAU
!    OPTION         IN     K16      OPTION DE CALCUL
!    EPSTH(6)       IN     R        VECTEUR DES DEFORMATIONS THERMIQUES
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "asterfort/matrot.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utrcyl.h"
#include "asterfort/verift.h"
    character(len=*) :: fami, poum
    character(len=16) :: option
    real(kind=8) :: instan, epsth(6), xyzgau(3)
    real(kind=8) :: repere(7)
    integer :: ndim, igau, isgau
! -----  VARIABLES LOCALES
!-----------------------------------------------------------------------
    integer :: mater, nbres, nbv
    real(kind=8) :: biot, e, zero
!-----------------------------------------------------------------------
    parameter (nbres = 3)
!
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), nompar
    character(len=16) :: phenom
!
    real(kind=8) :: valres(nbres), valpar, bendog, kdessi, angl(3)
    real(kind=8) :: dire(3), orig(3), p(3, 3), epsthl(6), troisk
    real(kind=8) :: vepst1(6), vepst2(6), hydr, sech, sref, ptot, nu
    integer :: k, iret, irepto
    character(len=6) :: epsa(6)
    data epsa   / 'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ',&
     &              'EPSAYZ'/
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! ---- INITIALISATIONS
!      ---------------
    zero = 0.d0
    nompar = 'INST'
    valpar = instan
    epsth(1:6) = zero
!
    call rcvarc(' ', 'HYDR', poum, fami, igau,&
                isgau, hydr, iret)
    if (iret .eq. 1) hydr=0.d0
!
    call rcvarc(' ', 'SECH', poum, fami, igau,&
                isgau, sech, iret)
    if (iret .eq. 1) sech=0.d0
    call rcvarc(' ', 'PTOT', poum, fami, igau,&
                isgau, ptot, irepto)
    call rcvarc(' ', 'SECH', 'REF', fami, 1,&
                1, sref, iret)
    if (iret .eq. 1) sref=0.d0
!
!
! ---- ------------------------------------------------------------
! ---- CALCUL DES DEFORMATIONS HYDRIQUES (OPTION CHAR_MECA_HYDR_R)
! ---- ------------------------------------------------------------
!
    if (option(11:14) .eq. 'HYDR') then
        if (hydr .ne. 0.d0) then
!
            phenom = 'ELAS'
            nomres(1) = 'B_ENDOGE'
            nbv = 1
!
! ----   RECUPERATION DES CARACTERISTIQUES MECANIQUES
!     ----------------------------------------------------------
            call rcvalb(fami, igau, isgau, poum, mater,&
                        ' ', phenom, 1, nompar, [valpar],&
                        nbv, nomres, valres, icodre, 0)
!
            if (icodre(1) .eq. 0) then
!
                bendog = valres(1)
!
                epsth(1) = - bendog*hydr
                epsth(2) = - bendog*hydr
                epsth(3) = - bendog*hydr
!
            else
!
                call utmess('I', 'ELEMENTS_58', sk=phenom)
!
            endif
        endif
!
! ---- ---------------------------------------------------------------
! ---- CALCUL DES DEFORMATIONS DUES A LA PRESSION DE FLUIDE
! ---  (OPTION CHAR_MECA_PTOT_R)
! ---- ---------------------------------------------------------------
    else if (option(11:14).eq.'PTOT') then
!
        if (irepto .eq. 0) then
!
! ----      RECUPERATION DU COEFFICIENT DE BIOT
!           ----------------------------------------------------------
!
            phenom = 'THM_DIFFU'
            nomres(1) = 'BIOT_COE'
            nbv = 1
!
            call rcvalb(fami, igau, isgau, poum, mater,&
                        ' ', phenom, 1, nompar, [valpar],&
                        nbv, nomres, valres, icodre, 0)
!
            if (icodre(1) .ne. 0) valres(1)=0.d0
!
            biot = valres(1)
!
! ----      RECUPERATION DU COEFFICIENT 3K
!           ----------------------------------------------------------
!
            phenom = 'ELAS'
            nomres(1)='E'
            nomres(2)='NU'
            nbv = 2
!
            call rcvalb(fami, igau, isgau, poum, mater,&
                        ' ', phenom, 1, nompar, [valpar],&
                        nbv, nomres, valres, icodre, 0)
            e = valres(1)
            nu = valres(2)
            troisk = e/(1.d0-2.d0*nu)
!
            epsth(1) = biot/troisk*ptot
            epsth(2) = epsth(1)
            epsth(3) = epsth(1)
!
        endif
!
! ---- ---------------------------------------------------------------
! ---- CALCUL DES DEFORMATIONS DU AU SECHAGE (OPTION CHAR_MECA_SECH_R)
! ---- ---------------------------------------------------------------
    else if (option(11:14).eq.'SECH') then
!
        phenom = 'ELAS'
        nomres(1) = 'K_DESSIC'
        nbv = 1
!
! ----      INTERPOLATION DE K_DESSICCA EN FONCTION DE LA TEMPERATURE
!           DE L HYDRATATION OU DU SECHAGE
!           ----------------------------------------------------------
        call rcvalb(fami, igau, isgau, poum, mater,&
                    ' ', phenom, 1, nompar, [valpar],&
                    nbv, nomres, valres, icodre, 0)
!
        if (icodre(1) .ne. 0) valres(1)=0.d0
!
        kdessi = valres(1)
!
        epsth(1) = - kdessi*(sref-sech)
        epsth(2) = - kdessi*(sref-sech)
        epsth(3) = - kdessi*(sref-sech)
!
!
! ---- ---------------------------------------------------------------
! ---- CALCUL DES DEFORMATIONS ANELASTIQUE (OPTION CHAR_MECA_EPSA_R)
! ---- ---------------------------------------------------------------
    else if (option(11:14).eq.'EPSA') then
        do k = 1, 6
            call rcvarc(' ', epsa(k), poum, fami, igau,&
                        isgau, epsth(k), iret)
            if (iret .eq. 1) epsth(k)=0.d0
        end do
!
!
! ---- ------------------------------------------------------------
! ---- CALCUL DES DEFORMATIONS THERMIQUES (OPTION CHAR_MECA_TEMP_R)
! ---- ------------------------------------------------------------
    else
!
!
! ---- RECUPERATION DU TYPE DU MATERIAU DANS PHENOM
!      --------------------------------------------
        call rccoma(mater, 'ELAS', 1, phenom, icodre(1))
!
!      ------------
! ---- CAS ISOTROPE
!      ------------
        if (phenom .eq. 'ELAS') then
!
            call verift(fami, igau, isgau, poum, mater,&
                        epsth=epsth(1) )
            epsth(2) = epsth(1)
            epsth(3) = epsth(1)
!
!      --------------
! ---- CAS ORTHOTROPE
!      --------------
        else if (phenom.eq.'ELAS_ORTH') then
!
            if (repere(1) .gt. 0.d0) then
                angl(1) = repere(2)
                angl(2) = repere(3)
                angl(3) = repere(4)
                call matrot(angl, p)
            else
                dire(1) = repere(2)
                dire(2) = repere(3)
                dire(3) = repere(4)
!
                orig(1) = repere(5)
                orig(2) = repere(6)
                orig(3) = repere(7)
                call utrcyl(xyzgau, dire, orig, p)
            endif
!
            call verift(fami, igau, isgau, poum, mater,&
                        vepsth=epsthl)
!
            epsthl(4) = 0.d0
            epsthl(5) = 0.d0
            epsthl(6) = 0.d0
!
!
!
            vepst1(1)=epsthl(1)
            vepst1(2)=epsthl(4)
            vepst1(3)=epsthl(2)
            vepst1(4)=epsthl(5)
            vepst1(5)=epsthl(6)
            vepst1(6)=epsthl(3)
!
!
!        PASSAGE DES DEFORMATIONS DANS LE REPERE D ORTHOTROPIE
!        AU REPERE GLOBAL
            call utpslg(1, 3, p, vepst1, vepst2)
            epsth(1)=vepst2(1)
            epsth(2)=vepst2(3)
            epsth(3)=vepst2(6)
            epsth(4)=vepst2(2)
            epsth(5)=vepst2(4)
            epsth(6)=vepst2(5)
            if (ndim .eq. 2) epsth(3)=epsthl(3)
!
!      -----------------------
! ---- CAS ISOTROPE-TRANSVERSE
!      -----------------------
        else if (phenom.eq.'ELAS_ISTR') then
!
            if (repere(1) .gt. 0.d0) then
                angl(1) = repere(2)
                angl(2) = repere(3)
                angl(3) = repere(4)
                call matrot(angl, p)
            else
                dire(1) = repere(2)
                dire(2) = repere(3)
                dire(3) = repere(4)
!
                orig(1) = repere(5)
                orig(2) = repere(6)
                orig(3) = repere(7)
                call utrcyl(xyzgau, dire, orig, p)
            endif
!
            call verift(fami, igau, isgau, poum, mater,&
                        vepsth=epsthl)
!
            epsthl(3) = epsthl(2)
            epsthl(2) = epsthl(1)
            epsthl(4) = 0.d0
            epsthl(5) = 0.d0
            epsthl(6) = 0.d0
!
!
            vepst1(1)=epsthl(1)
            vepst1(2)=epsthl(4)
            vepst1(3)=epsthl(2)
            vepst1(4)=epsthl(5)
            vepst1(5)=epsthl(6)
            vepst1(6)=epsthl(3)
!
!
!        PASSAGE DES DEFORMATIONS DANS LE REPERE D ORTHOTROPIE
!        AU REPERE GLOBAL
            call utpslg(1, 3, p, vepst1, vepst2)
            epsth(1)=vepst2(1)
            epsth(2)=vepst2(3)
            epsth(3)=vepst2(6)
            epsth(4)=vepst2(2)
            epsth(5)=vepst2(4)
            epsth(6)=vepst2(5)
            if (ndim .eq. 2) epsth(3)=epsthl(3)
        else if (phenom.eq.'ELAS_HYPER') then
        else
            call utmess('F', 'ELEMENTS_15', sk=phenom)
        endif
    endif
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
