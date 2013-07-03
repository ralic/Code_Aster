subroutine vdsiro(np, nbsp, matev, sens, goun,&
                  tens1, tens2)
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
    implicit none
! BUT : CHANGER LE REPERE : INTRINSEQUE <-> UTILISATEUR
!       POUR DES TENSEURS DE CONTRAINTE OU DE DEFORMATION.
!      (MODELISATION : 'COQUE_3D')
!
!        CETTE ROUTINE EST ANALOGUE A DXSIRO QUI EST
!        OPERATIONELLE POUR LES ELEMENTS DE PLAQUE
!        A L'EXCEPTION DES MATRICES DE PASSAGE QUI
!        SONT DEFINIES EN DES POINTS DE L'ELEMENT.
!
!
!   ARGUMENT        E/S   TYPE     ROLE
!    NP             IN     I    NOMBRE DE POINTS OU SONT CALCULES
!                               LES TENSEURS (I.E. IL S'AGIT DES
!                               NOEUDS OU DES POINTS D'INTEGRATION
!                               DE L'ELEMENT)
!    NSP            IN     I    NOMBRE DE SOUS-POINTS OU SONT CALCULES
!                               LES TENSEURS
!    MATEV(2,2,10)  IN     R    MATRICES DE PASSAGE DES REPERES
!                               INTRINSEQUES AUX POINTS  DE
!                               L'ELEMENT AU REPERE UTILISATEUR
!        MATEV EST OBTENUE PAR VDREPE :
!           MATEVN : POUR UN CHAMP AUX NOEUDS
!           MATEVG : POUR UN CHAMP AUX POINTS DE GAUSS
!    SENS           IN     K2   : "SENS" DU CHANGEMENT DE REPERE :
!                                 / 'IU' : INTRINSEQUE -> UTILISATEUR
!                                 / 'UI' : UTILISATEUR -> INTRINSEQUE
!    GOUN           IN     K1   : /'G' (GAUSS) /'N' (NOEUD)
!      (GOUN NE SERT QUE POUR EVITER UN BUG A CORRIGER)
!    TENS1(1)       IN     R    TENSEURS DES CONTRAINTES OU DES
!                               DEFORMATIONS DANS LE REPERE 1 :
!                                   SIXX SIYY SIXY SIXZ SIYZ
!                               OU  EPXX EPYY EPXY EPXZ EPYZ
!    TENS2(1)       OUT    R    TENSEUR DES CONTRAINTES OU DES
!                               DEFORMATIONS DANS LE REPERE 2
!
!  REMARQUE : ON PEUT APPELER CETTE ROUTINE AVEC LE MEME TABLEAU
!             POUR TENS1 ET TENS2 (PAS D'EFFET DE BORD)
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/utbtab.h"
    real(kind=8) :: matev(2, 2, 1), tens1(1), tens2(1)
    character(len=2) :: sens
    character(len=1) :: goun
    integer :: np, nbsp
! -----  VARIABLES LOCALES
    real(kind=8) :: workel(4), worklo(4), xab(2, 2)
    real(kind=8) :: tampon(2), mattmp(2, 2)
    integer :: i, kpt, ksp, kpt2
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call assert(sens.eq.'IU'.or.sens.eq.'UI')
    call assert(goun.eq.'G'.or.goun.eq.'N')
!
! --- BOUCLE SUR LES POINTS OU SONT CALCULES LES TENSEURS
! --- (I.E. LES NOEUDS OU LES POINTS D'INTEGRATION) :
!     ============================================
    do 10 kpt = 1, np
!
!       -- IL Y A UN BUG DANS VDREPE : ON NE CALCULE PAS
!          LES MATRICES POUR FAMI='MASS'
        kpt2=kpt
        if (goun .eq. 'G') kpt2=1
!
!       -- RECOPIE DE MATEV(KPT) DANS MATTMP :
        mattmp(1,1)=matev(1,1,kpt2)
        mattmp(2,2)=matev(2,2,kpt2)
!
!
        if (sens .eq. 'IU') then
            mattmp(1,2)=matev(1,2,kpt2)
            mattmp(2,1)=matev(2,1,kpt2)
        else
            mattmp(1,2)=matev(2,1,kpt2)
            mattmp(2,1)=matev(1,2,kpt2)
        endif
!
        do 20 ksp = 1, nbsp
            i=(kpt-1)*nbsp+ksp
            workel(1) = tens1(1+6*(i-1))
            workel(2) = tens1(4+6*(i-1))
            workel(3) = tens1(4+6*(i-1))
            workel(4) = tens1(2+6*(i-1))
!
            call utbtab('ZERO', 2, 2, workel, mattmp(1, 1),&
                        xab, worklo)
!
            tens2(1+6*(i-1)) = worklo(1)
            tens2(2+6*(i-1)) = worklo(4)
            tens2(3+6*(i-1)) = tens1(3+6*(i-1))
            tens2(4+6*(i-1)) = worklo(2)
            tampon(1)=tens1(5+6*(i-1))
            tampon(2)=tens1(6+6*(i-1))
            tens2(5+6*(i-1)) = tampon(1) * mattmp(1,1) + tampon(2) * mattmp(2,1)
            tens2(6+6*(i-1)) = tampon(1) * mattmp(1,2) + tampon(2) * mattmp(2,2)
!
20      continue
10  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
