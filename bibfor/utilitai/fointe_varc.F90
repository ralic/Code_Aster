subroutine fointe_varc(codmes, fami, kpg, ksp, poum,&
                       nomf, nbpu, nompu, valpu,&
                       resu, ier)
use calcul_module, only : ca_jvcnom_, ca_nbcvrc_
implicit none
#include "jeveux.h"
#include "asterfort/fointe.h"
#include "asterfort/rcvarc.h"
    character(len=*), intent(in) :: codmes, fami, poum
    character(len=*), intent(in) :: nomf
    integer, intent(in) :: nbpu, kpg, ksp
    character(len=*), intent(in) :: nompu(*)
    real(kind=8), intent(in) :: valpu(*)
    real(kind=8), intent(out) :: resu
    integer, intent(out) :: ier
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! But : INTERPOLATION POUR CALCULER RESU = F(X,Y,Z,...)
!       C'est une routine "chapeau" de fointe.F90 pour ajouter les variables de commande
!       Attention, contrairement à fointe cette routine ne peut être appelée que sous un te
!
! IN  CODMES : 'F','E','A','I',... PARAMETRE TRANSMIT A UTMESS, UTMESK
! IN  NOMF   : NOM DE LA FONCTION OU DE LA NAPPE
! IN  NBPU   : NOMBRE DE PARAMETRES DANS NOMPU ET VALPU
! IN  NOMPU  : NOMS DES PARAMETRES "UTILISATEUR"
! IN  VALPU  : VALEURS DES PARAMETRES "UTILISATEUR"
! OUT RESU   : RESULTAT DE L'INTERPOLATION
! OUT IER    : CODE RETOUR
!
! CODE RETOUR DE FOLOCX :
! IER = 10  : MOINS DE 1 POINT
! IER = 20  : EXTRAPOLATION INCONNUE
! IER = 30  : ON DEBORDE A GAUCHE
! IER = 40  : ON DEBORDE A DROITE
!
! CODE RETOUR DE FOCOLI :
! IER = 200 : INTERPOLATION DE LA FONCTION NON PERMISE
! IER = 210 : PARAMETRE EN DOUBLE
! IER = 220 : PARAMETRE ATTENDUS,PARAMETRES RECUS
! IER = 230 : TYPE D'INTERPOLATION DE LA FONCTION INCONNU
! IER = 240 : RECHERCHE DE LA VALEUR INCONNUE (COLI)
!
! CODE RETOUR DE FOINTE :
! IER = 100 : TYPE DE FONCTION NON VALIDE
! IER = 110 : PAS ASSEZ DE PARAMETRES
! IER = 120 : PARAMETRE EN DOUBLE
! IER = 130 : PARAMETRE ATTENDUS,PARAMETRES RECUS
! IER = 140 : TYPE D'INTERPOLATION SUR LES PARA DE LA NAPPE INCONNU
! IER = 150 : TYPE DE FONCTION NON TRAITE
! IER = 160 : PAS ASSEZ DE PARAMETRES
! IER = 170 : INTERPOLATION SUR LES PARAMETRES DE LA NAPPE NON PERMISE
! IER = 200 : ERREUR AVEC UNE FORMULE
! ----------------------------------------------------------------------
!
    integer :: nbpamx, nbpar2, ipar, nbpart, ierc
    parameter (nbpamx=10)
    real(kind=8) :: valpa2(nbpamx), valvrc
    character(len=8) :: nompa2(nbpamx), novrc
! ----------------------------------------------------------------------
!   -- s'il n'y a pas de varc, il n'y a qu'a appeler fointe :
    if (ca_nbcvrc_ .eq. 0) then
        call fointe(codmes, nomf, nbpu, nompu, valpu,&
                       resu, ier)
        goto 9999
    endif
    

!   -- sinon, on ajoute les varc au debut de la liste des parametres
!      car fointa donne priorite aux derniers :
    nbpar2 = 0
    do ipar=1,ca_nbcvrc_
        novrc=zk8(ca_jvcnom_-1+ipar)
        call rcvarc(' ', novrc, poum, fami, kpg,&
                    ksp, valvrc, ierc)
        if (ierc .eq. 0) then
            nbpar2=nbpar2+1
            nompa2(nbpar2)=novrc
            valpa2(nbpar2)=valvrc
        endif
    enddo

    do ipar=1,nbpu
        nompa2(nbpar2+ipar) = nompu(ipar)
        valpa2(nbpar2+ipar) = valpu(ipar)
    enddo

    nbpart=nbpu+nbpar2

    call fointe(codmes, nomf, nbpart, nompa2, valpa2,&
                       resu, ier)
9999  continue
end subroutine
