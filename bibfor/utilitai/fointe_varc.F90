subroutine fointe_varc(codmes   , fami        , kpg           , ksp           , poum,&
                       func_name, nb_para_user, para_name_user, para_vale_user,&
                       resu     , ier)
use calcul_module, only : ca_jvcnom_, ca_nbcvrc_, ca_iactif_
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/rcvarc.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: codmes
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    character(len=*), intent(in) :: poum
    character(len=*), intent(in) :: func_name
    integer, intent(in) :: nb_para_user
    character(len=*), intent(in) :: para_name_user(*)
    real(kind=8), intent(in) :: para_vale_user(*)
    real(kind=8), intent(out) :: resu
    integer, intent(out) :: ier
!
! --------------------------------------------------------------------------------------------------
!
! FUNCTION
!
! Prepare interpolation of function when use external state variables
!
! --------------------------------------------------------------------------------------------------
!
! In  codmes           : behaviour of error messages
! In  fami             : Gauss family for integration point rule
! In  poum             : parameters evaluation
!                     '-' at beginning of step time
!                     '+' at end of step time
!                   'REF' for reference value
! In  kpg              : current point gauss
! In  ksp              : current "sous-point" gauss
! In  nb_para_user     : number of parameters from user
! In  para_name_user   : name of parameters from user
! In  para_vale_user   : value of parameters from user
! Out resu             : value of function with interpolation of parameters
! Out ier              : error code
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
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbpamx=10
    integer :: nb_para, i_para, nb_para_tota, ierc
    real(kind=8) :: para_vale(nbpamx), varc_vale
    character(len=8) :: para_name(nbpamx), varc_name
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(ca_iactif_ .gt. 0)
    if (ca_nbcvrc_ .eq. 0) then
        call fointe(codmes      , func_name,&
                    nb_para_user, para_name_user, para_vale_user,&
                    resu        , ier)
    else
! ----- Get list of external state variables
        nb_para = 0
        do i_para = 1, ca_nbcvrc_
            varc_name = zk8(ca_jvcnom_-1+i_para)
            call rcvarc(' ', varc_name, poum, fami, kpg,&
                        ksp, varc_vale, ierc)
            if (ierc .eq. 0) then
                nb_para=nb_para+1
                para_name(nb_para)=varc_name
                para_vale(nb_para)=varc_vale
            endif
        enddo
! ----- Add external state variables to function variable
        do i_para = 1, nb_para_user
            para_name(nb_para+i_para) = para_name_user(i_para)
            para_vale(nb_para+i_para) = para_vale_user(i_para)
        enddo
! ----- Interpolation of function
        nb_para_tota = nb_para_user+nb_para
        call fointe(codmes      , func_name,&
                    nb_para_tota, para_name, para_vale,&
                    resu        , ier)
    endif
end subroutine
