subroutine impcmp(icmp, numedd, chaine)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/rgndas.h"
    integer :: icmp
    character(len=24) :: numedd
    character(len=16) :: chaine
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (AFFICHAGE - UTILITAIRE)
!
! RETOURNE UNE CHAINE FORMATEE K16 POUR LES INFOS SUR UNE COMPOSANTE
!
! ----------------------------------------------------------------------
!
!
! IN  ICMP   : NUMERO DE L'EQUATION
! IN  NUMEDD : NUMEROTATION NUME_DDL
! OUT CHAINE : CHAINE DU NOM DU NOEUD OU 'LIAISON_DDL'
!              CHAINE DU NOM DE LA CMP OU NOM DU LIGREL DE CHARGE
!
!
!
!
    character(len=8) :: nomno, nomcmp, load
    character(len=1) :: tyddl
!
! ----------------------------------------------------------------------
!
    chaine = ' '
!
    if (icmp .ne. 0) then
        call rgndas(numedd, icmp, l_print = .false., type_equaz = tyddl, name_nodez = nomno,&
                    name_cmpz = nomcmp, ligrelz = load)
        if (tyddl .eq. 'A') then
            chaine(1:8)  = nomno
            chaine(9:16) = nomcmp
        elseif (tyddl .eq. 'B') then
            chaine(1:16) = 'Link : '//nomno
        elseif (tyddl .eq. 'C') then
            chaine(1:16) = 'Link : '//load
        endif
    endif
!
end subroutine
