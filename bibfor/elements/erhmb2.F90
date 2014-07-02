subroutine erhmb2(perman, ino, nbs, ndim, theta,&
                  instpm, jac, nx, ny, tx,&
                  ty, nbcmp, geom, ivois, sielnp,&
                  sielnm, adsip, iagd, tbref2, iade2,&
                  iava2, ncmpm2, iaptm2, iade3, iava3,&
                  ncmpm3, iaptm3, tm2h1b)
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
! =====================================================================
!  ERREUR EN HYDRO-MECANIQUE - TERME DE BORD - DIMENSION 2
!  **        *     *                    *                *
! =====================================================================
!  - FONCTION REALISEE :  CALCUL DE L'ERREUR DUE A LA NON-VERIFICATION
!                         DES CONDITIONS DE NEUMANN EN MECANIQUE ET
!                         EN HYDRAULIQUE DANS L'INDICATEUR
!                         HM PERMANENT.
!
!    REMARQUE : CETTE ROUTINE RESSEMBLE BEAUCOUP A ERMES2
!          SI ON A IMPOSE UNE CONDITION DE NEUMANN NULLE IMPLICITEMENT,
!          LA CONTRIBUTION A L'INDICATEUR SERA NULLE
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN PERMAN : PERMANENT OU INSTATIONNAIRE ?
! IN INO    : NUMERO DU NOEUD X1
! IN NBS    : NOMBRE D'ARETES
! IN NDIM   : DIMENSION DE L'ESPACE
! IN THETA  : PARAMETRE THETA DE LA DISCRETISATION TEMPORELLE
! IN INSTPM : TABLEAU DES INSTANTS (ACTUEL ET PRECEDENT)
! IN JAC    : VECTEUR DES JACOBIENS DE LA TRANSFORMATION AUX NOEUDS
! IN NX     : VECTEUR DES ABSCISSES DES NORMALES AUX NOEUDS
! IN NY     : VECTEUR DES ORDONNEES DES NORMALES AUX NOEUDS
! IN TX     : VECTEUR DES ABSCISSES DES TANGENTES AUX NOEUDS
! IN TY     : VECTEUR DES ORDONNEES DES TANGENTES AUX NOEUDS
! IN NBCMP  : NOMBRE DE COMPOSANTES DES CONTRAINTES GENERALISEES
!             AUX NOEUDS
! IN GEOM   : TABLEAU DES COORDONNEES
! IN IVOIS  : ADRESSE DES VOISINS
! IN SIELNP : CONTRAINTES AUX NOEUDS PAR ELEMENT A L'INSTANT ACTUEL
! IN SIELNM : CONTRAINTES AUX NOEUDS PAR ELEMENT A L'INSTANT PRECEDENT
! IN ADSIP  : ADRESSE DANS ZR DU TABLEAU DES CONTRAINTES DE PRESSION
!             DE LA MECANIQUE
! IN IAGD   : ADRESSE DU VECTEUR GRANDEUR
! IN TBREF2 : TABLEAU DES CHARGEMENTS DE TYPE PRES_REP
! IN IADE2  : ADRESSE DE L'OBJET .DESC DE LA CARTE2
! IN IAVA2  : ADRESSE DE L'OBJET .VALE DE LA CARTE2
! IN NCMPM2 : NOMBRE DE COMPOSANTES DE LA GRANDEUR ASSOCIEE A LA
!             CARTE 2
! IN IAPTM2 : ADRESSE DU .PTMA DE LA CARTE ETENDUE 2
! IN IADE3  : ADRESSE DE L'OBJET .DESC DE LA CARTE3
! IN IAVA3  : ADRESSE DE L'OBJET .VALE DE LA CARTE3
! IN NCMPM3 : NOMBRE DE COMPOSANTES DE LA GRANDEUR ASSOCIEE A LA
!             CARTE 3
! IN IAPTM3 : ADRESSE DU .PTMA DE LA CARTE ETENDUE 3
!
!      SORTIE :
!-------------
! OUT TM2H1B : TABLEAU CONTENANT LES TERMES DE BORD DES TERMES DIFFUSIFS
!              (2 POUR LA MECANIQUE, 1 POUR L'HYDRAULIQUE)
!  1 : MECANIQUE
!  2 : DERIVEE TEMPORELLE DE LA MECA
!  3 : HYDRAULIQUE
! ......................................................................
!
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES :
!       MESSAGE     : UTMESK
!       UTILITAIRES : FOINTE
!     FONCTION INTRINSEQUE : SQRT
!   -------------------------------------------------------------------
!
! aslint: disable=W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPEL
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/fointe.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
    aster_logical :: perman
    integer :: ino, nbs, ndim
    real(kind=8) :: jac(3), nx(3), ny(3), tx(3), ty(3)
    integer :: nbcmp, ivois, iagd
    integer :: adsip
    real(kind=8) :: theta, sielnp(140), sielnm(140), instpm(2)
    integer :: tbref2(12), iade2, iava2, ncmpm2, iaptm2, iade3, iava3, ncmpm3, iaptm3
    real(kind=8) :: tm2h1b(3)
    real(kind=8) :: geom(ndim, *)
!
!
! DECLARATION VARIABLES LOCALES
!
    aster_logical :: flagm, flagh
    integer :: imav, ient2, numgd2, ient3, numgd3
    integer :: jno, mno, ibid
    integer :: idec1, idec2, idec3
    integer :: ier1, ier2, ier3, ier4, ier5, ier6
    integer :: ier11, ier21, ier31, ier41, ier51, ier61
    real(kind=8) :: valpar(3), prp(3), prm(3), cip(3), cim(3), fluxhp(3), fluxhm(3), inte1, inte2
    real(kind=8) :: inte3, inted1, inted2, inted3, sig11(3), sig22(3), sig12(3), fh11x(3)
    real(kind=8) :: fh11y(3)
    real(kind=8) :: ta1
    character(len=4) :: nompar(3)
    character(len=8) :: prf, cif, fluxhf
    character(len=19) :: nomgd2, nomgd3
! =====================================================================
    if (.not. perman) then
        ibid = 1
    else
        ibid = 0
        theta = 1.d0
    endif
    ta1 = 1.d0 - theta
! =====================================================================
! 1. RECUPERATION SUR LA MAILLE COURANTE AUX NOEUDS INO ET JNO DE :
!     . CONTRAINTES EFFECTIVES (SIGMA MECANIQUE : SIXX, SIYY, SIXY)
!     . CONTRAINTES DE PRESSION (TBIOT*PRESSION  : SIPXX, SIPYY, SIPXY)
!     . FLUX DE PRESSION (FH11X, FH11Y)
!
!              X1          X2          X3
!               O-----------O-----------O
!              INO         MNO         JNO
!
!         POINTS  1 --> INO PREMIER POINT DE L'ARETE COURANTE
!                 2 --> JNO DEUXIEME POINT  DE L'ARETE COURANTE
!                 3 --> MNO NOEUD MILIEU (IL EXISTE FORCEMENT EN THM)
! =====================================================================
!
    if (ino .eq. nbs) then
        jno = 1
    else
        jno = ino+1
    endif
!
    mno = nbs+ino
!
    imav = zi(ivois+ino)
!
! =====================================================================
! 2. ON TRAITE LA PARTIE MECANIQUE
! =====================================================================
!
    flagm = .true.
    call r8inir(3, 0.d0, prp, 1)
    call r8inir(3, 0.d0, cip, 1)
!
    if (.not. perman) then
        call r8inir(3, 0.d0, prm, 1)
        call r8inir(3, 0.d0, cim, 1)
    endif
!
! --------------------------------------------------------------------
! ** ON A IMPOSE DES CONDITIONS AUX LIMITES DE NEUMANN
!    EN MECANIQUE SUR UNE PARTIE DU BORD EXPLICITEMENT (I.E. IADE2 <> 0)
! --------------------------------------------------------------------
!
    if (iade2 .ne. 0) then
!
! --------------------------------------------------------------------
! 2.1. RECHERCHE DES ADRESSES POUR LA CONDITION LIMITE MECANIQUE
! --------------------------------------------------------------------
!
        if (iaptm2 .eq. 0) then
! CARTE CONSTANTE
            ient2 = 1
        else
! LA CARTE A ETE ETENDUE
            ient2 = zi(iaptm2 -1 +imav)
        endif
        numgd2 = tbref2(8)
        nomgd2 = zk8(iagd-1+numgd2)
!
! --------------------------------------------------------------------
! 2.2. LA CONDITION DE NEUMANN MECANIQUE EST DE TYPE PRES_
! --------------------------------------------------------------------
!
        if (nomgd2(1:5) .eq. 'PRES_') then
!
! 2.2.1. DETERMINATION DES VALEURS SI CONSTANTE
!
            if (nomgd2(1:6) .eq. 'PRES_R') then
!
                flagm = .false.
!
                prp(1) = zr(iava2-1+(ient2-1)*ncmpm2+1)
                prp(2) = prp(1)
                prp(3) = prp(1)
                cip(1) = zr(iava2-1+(ient2-1)*ncmpm2+2)
                cip(2) = cip(1)
                cip(3) = cip(1)
!
! 2.2.2. DETERMINATION DES VALEURS SI FONCTION
!
            else if (nomgd2(1:6).eq.'PRES_F') then
!
                prf = zk8(iava2-1+(ient2-1)*ncmpm2+1)
                cif = zk8(iava2-1+(ient2-1)*ncmpm2+2)
!
! CE QUI SUIT EST LE SEUL MOYEN AUJOURD'HUI DE PIEGER LES BORDS A
! DIRICHLET. EN EFFET ASTER LES AFFECTE AUTOMATIQUEMENT D'UN NEUMANN
! VALANT &FOZERO. CELA SUPPOSE QUE LES VRAIS BORDS A NEUMANN NUL
! ONT ETE DECLARES EXPLICITEMENT DANS LES COMMANDES
!
                if (prf .eq. '&FOZERO' .and. cif .eq. '&FOZERO') then
                    goto 3333
                endif
!
                flagm = .false.
!
                nompar(1) = 'X'
                nompar(2) = 'Y'
                nompar(3) = 'INST'
!
                valpar(1) = geom(1,ino)
                valpar(2) = geom(2,ino)
                valpar(3) = instpm(1)
!
                call fointe('FM', prf, 3, nompar, valpar,&
                            prp(1), ier1)
                call fointe('FM', cif, 3, nompar, valpar,&
                            cip(1), ier2)
!
                if (.not. perman) then
                    valpar(3) = instpm(2)
                    call fointe('FM', prf, 3, nompar, valpar,&
                                prm(1), ier11)
                    call fointe('FM', cif, 3, nompar, valpar,&
                                cim(1), ier21)
                endif
!
                valpar(1) = geom(1,jno)
                valpar(2) = geom(2,jno)
                valpar(3) = instpm(1)
!
                call fointe('FM', prf, 3, nompar, valpar,&
                            prp(2), ier3)
                call fointe('FM', cif, 3, nompar, valpar,&
                            cip(2), ier4)
!
                if (.not. perman) then
                    valpar(3) = instpm(2)
                    call fointe('FM', prf, 3, nompar, valpar,&
                                prm(2), ier31)
                    call fointe('FM', cif, 3, nompar, valpar,&
                                cim(2), ier41)
                endif
!
                valpar(1) = geom(1,mno)
                valpar(2) = geom(2,mno)
                valpar(3) = instpm(1)
!
                call fointe('FM', prf, 3, nompar, valpar,&
                            prp(3), ier5)
                call fointe('FM', cif, 3, nompar, valpar,&
                            cip(3), ier6)
!
                if (.not. perman) then
                    valpar(3) = instpm(2)
                    call fointe('FM', prf, 3, nompar, valpar,&
                                prm(3), ier51)
                    call fointe('FM', cif, 3, nompar, valpar,&
                                cim(3), ier61)
                endif
!
! 2.2.3. ERREUR
!
            else
                call utmess('F', 'INDICATEUR_90', sk=nomgd2)
            endif
!
! 2.2.4. ERREUR
!
        else
            call utmess('F', 'INDICATEUR_91', sk=nomgd2)
        endif
!
    endif
!
! 2.3. TERME DE BORD
!
! ------- RECUPERATION DE SIGMA TOTAL SUR LA MAILLE COURANTE -----------
!
    if (flagm) then
!
! ----- PAS DE CHARGEMENT DE NEUMANN EXPLICITE SUR LE BORD
!       ==> ON PREND SIGMA TOTAL NUL
!
        call r8inir(3, 0.d0, sig11, 1)
        call r8inir(3, 0.d0, sig22, 1)
        call r8inir(3, 0.d0, sig12, 1)
!
    else
! ---- DANS CE QUI SUIT ON  PREND EN COMPTE DANS LE CALCUL DE
!      LA CONTRAINTE TOTALE LES TERMES DU TENSEUR SIP
        idec1 = nbcmp*(ino-1)
!
        sig11(1) = sielnp(idec1+1) + sielnp(idec1+adsip)
        sig22(1) = sielnp(idec1+2) + sielnp(idec1+adsip+1)
        sig12(1) = sielnp(idec1+4) + sielnp(idec1+adsip+3)
!
        idec2 = nbcmp*(jno-1)
!
        sig11(2) = sielnp(idec2+1) + sielnp(idec2+adsip)
        sig22(2) = sielnp(idec2+2) + sielnp(idec2+adsip+1)
        sig12(2) = sielnp(idec2+4) + sielnp(idec2+adsip+3)
!
        idec3 = nbcmp*(mno-1)
!
        sig11(3) = sielnp(idec3+1) + sielnp(idec3+adsip)
        sig22(3) = sielnp(idec3+2) + sielnp(idec3+adsip+1)
        sig12(3) = sielnp(idec3+4) + sielnp(idec3+adsip+3)
!
    endif
!
    inte1 = jac(1)*( ( - prp(1) * nx(1) + cip(1) * tx(1) - sig11(1) * nx(1) - sig12(1) * ny(1) )*&
            &*2 + ( - prp(1) * ny(1) + cip(1) * ty(1) - sig12(1)*nx(1) - sig22(1) * ny(1) )**2 )
!
    inte2 = jac(2)*( ( - prp(2) * nx(2) + cip(2) * tx(2) - sig11(2) * nx(2) - sig12(2) * ny(2) )*&
            &*2 + ( - prp(2) * ny(2) + cip(2) * ty(2) - sig12(2) * nx(2) - sig22(2) * ny(2) )**2 &
            &)
!
    inte3 = jac(3)*( ( - prp(3) * nx(3) + cip(3) * tx(3) - sig11(3) * nx(3) - sig12(3) * ny(3) )*&
            &*2 + ( - prp(3) * ny(3) + cip(3) * ty(3) - sig12(3) * nx(3) - sig22(3) * ny(3) )**2 &
            &)
!
    tm2h1b(1) = tm2h1b(1) + (inte1+4.d0*inte3+inte2)/3.d0
!
    if (.not. perman) then
!
        if (flagm) then
!
! ----- PAS DE CHARGEMENT DE NEUMANN EXPLICITE SUR LE BORD
!       ==> ON PREND LA DERIVEE DE SIGMA TOTAL NULLE
!
            call r8inir(3, 0.d0, sig11, 1)
            call r8inir(3, 0.d0, sig22, 1)
            call r8inir(3, 0.d0, sig12, 1)
!
        else
! ---- DANS CE QUI SUIT ON  PREND EN COMPTE DANS LE CALCUL DE
!      LA CONTRAINTE TOTALE LES TERMES DU TENSEUR SIP
            idec1 = nbcmp*(ino-1)
!
            sig11(1) = sielnp(idec1+1) - sielnm(idec1+1) + sielnp( idec1+adsip) - sielnm(idec1+ad&
                       &sip)
            sig22(1) = sielnp(idec1+2) - sielnm(idec1+2) + sielnp( idec1+adsip+1) - sielnm(idec1+&
                       &adsip+1)
            sig12(1) = sielnp(idec1+4) - sielnm(idec1+4) + sielnp( idec1+adsip+3) - sielnm(idec1+&
                       &adsip+3)
!
            idec2 = nbcmp*(jno-1)
!
            sig11(2) = sielnp(idec2+1) - sielnm(idec2+1) + sielnp( idec2+adsip) - sielnm(idec2+ad&
                       &sip)
            sig22(2) = sielnp(idec2+2) - sielnm(idec2+2) + sielnp( idec2+adsip+1) - sielnm(idec2+&
                       &adsip+1)
            sig12(2) = sielnp(idec2+4) - sielnm(idec2+4) + sielnp( idec2+adsip+3) - sielnm(idec2+&
                       &adsip+3)
!
            idec3 = nbcmp*(mno-1)
!
            sig11(3) = sielnp(idec3+1) - sielnm(idec3+1) + sielnp( idec3+adsip) - sielnm(idec3+ad&
                       &sip)
            sig22(3) = sielnp(idec3+2) - sielnm(idec3+2) + sielnp( idec3+adsip+1) - sielnm(idec3+&
                       &adsip+1)
            sig12(3) = sielnp(idec3+4) - sielnm(idec3+4) + sielnp( idec3+adsip+3) - sielnm(idec3+&
                       &adsip+3)
!
        endif
!
        inted1 = jac(1)*( (- (prp(1)-prm(1)) * nx(1) + (cip(1)-cim(1)) * tx(1) - ( sig11(1) * nx(&
                 &1) + sig12(1) * ny(1) ))**2 + (- ( prp(1)-prm(1)) * ny(1) + (cip(1)-cim(1)) * t&
                 &y(1) - ( sig12(1) * nx(1) + sig22(1) * ny(1)))**2)
!
        inted2 = jac(2)*( (- (prp(2)-prm(2)) * nx(2) + (cip(2)-cim(2)) * tx(2) - ( sig11(2) * nx(&
                 &2) + sig12(2) * ny(2) ))**2 + (- ( prp(2)-prm(2)) * ny(2) + (cip(2)-cim(2)) * t&
                 &y(2) - ( sig12(2) * nx(2) + sig22(2) * ny(2)))**2)
!
        inted3 = jac(3)*( (- (prp(3)-prm(3)) * nx(3) + (cip(3)-cim(3)) * tx(3) - ( sig11(3) * nx(&
                 &3) + sig12(3) * ny(3) ))**2 + (- ( prp(3)-prm(3)) * ny(3) + (cip(3)-cim(3)) * t&
                 &y(3) - ( sig12(3) * nx(3) +sig22(3) * ny(3)))**2)
!
        tm2h1b(2) = tm2h1b(2) + (inted1+4.d0*inted3+inted2)/3.d0
!
    endif
!
3333 continue
!
! =====================================================================
! 3. ON TRAITE LA PARTIE HYDRAULIQUE
! =====================================================================
!
    flagh = .true.
    call r8inir(3, 0.d0, fluxhp, 1)
!
    if (.not. perman) then
        call r8inir(3, 0.d0, fluxhm, 1)
    endif
!
! --------------------------------------------------------------------
! ** ON A IMPOSE DES CONDITIONS AUX LIMITES DE NEUMANN
!    EN HYDRAULIQUE SUR UNE PARTIE DU BORD EXPLICITEMENT
!    (I.E. IADE3 <> 0)
! --------------------------------------------------------------------
!
    if (iade3 .ne. 0) then
!
! --------------------------------------------------------------------
! 3.1. RECHERCHE DES ADRESSES POUR LA CONDITION LIMITE HYDRAULIQUE
! --------------------------------------------------------------------
!
        if (iaptm3 .eq. 0) then
! CARTE CONSTANTE
            ient3 = 1
        else
! LA CARTE A ETE ETENDUE
            ient3 = zi(iaptm3 -1 +imav)
        endif
!
        numgd3 = tbref2(12)
        nomgd3 = zk8(iagd-1+numgd3)
!
! --------------------------------------------------------------------
! 3.2. LA CONDITION DE NEUMANN HYDRAULIQUE EST DE TYPE FTHM_
! --------------------------------------------------------------------
!
        if (nomgd3(1:5) .eq. 'FTHM_') then
!
! 3.2.1. DETERMINATION DES VALEURS SI CONSTANTE
!
            if (nomgd3(1:6) .eq. 'FTHM_R') then
!
                flagh = .false.
!
                fluxhp(1) = zr(iava3-1+(ient3-1)*ncmpm3+1)
                fluxhp(2) = fluxhp(1)
                fluxhp(3) = fluxhp(1)
!
! 3.2.2. DETERMINATION DES VALEURS SI FONCTION
!
            else if (nomgd3(1:6).eq.'FTHM_F') then
                fluxhf = zk8(iava3-1+(ient3-1)*ncmpm3+1)
!
! CE QUI SUIT EST LE SEUL MOYEN AUJOURD'HUI DE PIEGER LES BORDS
! A DIRICHLET. EN EFFET ASTER LES AFFECTE AUTOMATIQUEMENT D'UN NEUMANN
! VALANT &FOZERO. CELA SUPPOSE QUE LES VRAIS BORDS A NEUMANN NUL
! ONT ETE DECLARES EXPLICITEMENT DANS LES COMMANDES.
!
                if (fluxhf .eq. '&FOZERO') then
                    goto 999
                endif
!
                flagh = .false.
!
                nompar(1) = 'X'
                nompar(2) = 'Y'
                nompar(3) = 'INST'
                valpar(1) = geom(1,ino)
                valpar(2) = geom(2,ino)
                valpar(3) = instpm(1)
                call fointe('FM', fluxhf, 3, nompar, valpar,&
                            fluxhp(1), ier1)
!
                if (.not. perman) then
                    valpar(3) = instpm(2)
                    call fointe('FM', fluxhf, 3, nompar, valpar,&
                                fluxhm(1), ier11)
                endif
!
                valpar(1) = geom(1,jno)
                valpar(2) = geom(2,jno)
                valpar(3) = instpm(1)
                call fointe('FM', fluxhf, 3, nompar, valpar,&
                            fluxhp(2), ier2)
!
                if (.not. perman) then
                    valpar(3) = instpm(2)
                    call fointe('FM', fluxhf, 3, nompar, valpar,&
                                fluxhm(2), ier21)
                endif
!
                valpar(1) = geom(1,mno)
                valpar(2) = geom(2,mno)
                valpar(3) = instpm(1)
                call fointe('FM', fluxhf, 3, nompar, valpar,&
                            fluxhp(3), ier3)
!
                if (.not. perman) then
                    valpar(3) = instpm(2)
                    call fointe('FM', fluxhf, 3, nompar, valpar,&
                                fluxhm(3), ier31)
                endif
!
! 3.2.3. ERREUR
!
            else
                call utmess('F', 'INDICATEUR_90', sk=nomgd3)
            endif
!
! 3.2.4. ERREUR
!
        else
            call utmess('F', 'INDICATEUR_91', sk=nomgd3)
        endif
!
    endif
!
! 3.3. TERME DE BORD
!
    if (.not. perman) then
!
        if (flagh) then
!
! ----- PAS DE FLUX HYDRAULIQUE NUL DECLARE EXPLICITEMENT
!       SUR LE BORD
!       ==> ON PREND DERIVEE DU FLUX HYDRAULIQUE NULLE
!
            call r8inir(3, 0.d0, fh11x, 1)
            call r8inir(3, 0.d0, fh11y, 1)
!
        else
!
            idec1 = nbcmp*(ino-1)+adsip+ibid+1+5
            fh11x(1) = theta *sielnp(idec1) + ta1*sielnm(idec1)
            fh11y(1) = theta *sielnp(idec1+1) + ta1*sielnm(idec1+1)
!
            idec2 = nbcmp*(jno-1)+adsip+ibid+1+5
            fh11x(2) = theta *sielnp(idec2) + ta1*sielnm(idec2)
            fh11y(2) = theta *sielnp(idec2+1) + ta1*sielnm(idec2+1)
!
            idec3 = nbcmp*(mno-1)+adsip+ibid+1+5
            fh11x(3) = theta *sielnp(idec3) + ta1*sielnm(idec3)
            fh11y(3) = theta *sielnp(idec3+1) + ta1*sielnm(idec3+1)
!
        endif
!
        inte1 = jac(1)* ( theta * fluxhp(1)+ ta1 * fluxhm(1) - fh11x( 1)*nx(1)-fh11y(1)*ny(1))**2
!
        inte2 = jac(2)* ( theta * fluxhp(2)+ ta1 * fluxhm(2) - fh11x( 2)*nx(2)-fh11y(2)*ny(2))**2
!
        inte3 = jac(3)* ( theta * fluxhp(3)+ ta1 * fluxhm(3) - fh11x( 3)*nx(3)-fh11y(3)*ny(3))**2
!
        tm2h1b(3) = tm2h1b(3) + ( inte1+4.d0*inte3+inte2 )/3.d0
!
    else
!
        if (flagh) then
!
! ----- PAS DE FLUX HYDRAULIQUE NUL DECLARE EXPLICITEMENT
!       SUR LE BORD
!       ==> ON PREND FLUX HYDRAULIQUE NUL
!
            call r8inir(3, 0.d0, fh11x, 1)
            call r8inir(3, 0.d0, fh11y, 1)
!
        else
!
            idec1 = nbcmp*(ino-1)+adsip+ibid+1+5
            fh11x(1) = sielnp(idec1)
            fh11y(1) = sielnp(idec1+1)
!
            idec2 = nbcmp*(jno-1)+adsip+ibid+1+5
            fh11x(2) = sielnp(idec2)
            fh11y(2) = sielnp(idec2+1)
!
            idec3 = nbcmp*(mno-1)+adsip+ibid+1+5
            fh11x(3) = sielnp(idec3)
            fh11y(3) = sielnp(idec3+1)
!
        endif
!
        inte1 = jac(1)* ( fluxhp(1) - ( fh11x(1)*nx(1)+fh11y(1)*ny(1)) )**2
!
        inte2 = jac(2)* ( fluxhp(2) - ( fh11x(2)*nx(2)+fh11y(2)*ny(2)) )**2
!
        idec3 = nbcmp*(mno-1)+adsip+ibid+1+5
!
        inte3 = jac(3)* ( fluxhp(3) - ( fh11x(3)*nx(3)+fh11y(3)*ny(3)) )**2
!
        tm2h1b(3) = tm2h1b(3) + ( inte1+4.d0*inte3+inte2 )/3.d0
!
    endif
!
999 continue
!
end subroutine
