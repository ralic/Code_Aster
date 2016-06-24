function mdtr74grd(nomgrd)
!
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
    implicit none
    integer :: mdtr74grd
    character(len=*) :: nomgrd
!
#include "asterfort/assert.h"
#include "asterfort/iunifi.h"
!
    integer :: imess
!
! --------------------------------------------------------------------------------------------------
!
!       Dimension pour les comportements non-linéaires de DYNA_VIBRA//TRAN/GENE
!
! --------------------------------------------------------------------------------------------------
!
!   MAXVINT         : Nombre maximal de variables internes ==> 5 <== DIS_ECRO_TRAC
!
!   FLAMBAGE        : Nombre de variables internes
!                       1   écrasement cumulé
!
!   DIS_VISC        : Nombre de variables internes
!                       4   sigma, epsivisq, puiss, tangente
!
!   DIS_ECRO_TRAC   : Nombre de variables internes
!                       5   force, Up, puiss, ip, tangente
!
!   PARCHO          : nombre de composantes réelles  de PARCHO
!
!   PAINCHO         : nombre de composantes entières de PAINCHO
!
!   SCHOR           : nombre de composantes de SCHOR
!                       force de choc
!                           saucho(i,1) saucho(i,2) saucho(i,3)
!                       déplacement local du noeud noeud_1
!                           saucho(i,4) saucho(i,5) saucho(i,6)
!                       vecteur vitesse
!                           saucho(i,7) saucho(i,8) saucho(i,9)
!                       déplacement local du noeud noeud_2
!                           saucho(i,10) saucho(i,11) saucho(i,12)
!                       indicateur adhérence
!                           saucho(i,13)
!
! --------------------------------------------------------------------------------------------------
!
    mdtr74grd = 0
    if      ( nomgrd(1:6) .eq.'PARCHO' ) then
        mdtr74grd = 64
    else if ( nomgrd(1:7) .eq.'PAINCHO') then
        mdtr74grd = 3
    else if ( nomgrd(1:7) .eq.'MAXVINT') then
        mdtr74grd = 5
    else if ( nomgrd(1:8) .eq.'DIS_VISC') then
        mdtr74grd = 4
    else if ( nomgrd(1:13).eq.'DIS_ECRO_TRAC') then
        mdtr74grd = 5
    else if ( nomgrd(1:8) .eq.'FLAMBAGE') then
        mdtr74grd = 1
    else if ( nomgrd(1:5) .eq.'SCHOR') then
        mdtr74grd = 13
    else if ( nomgrd(1:6) .eq.'LOGCHO') then
        mdtr74grd = 6
    else
        imess = iunifi('MESSAGE')
        write(imess,'(A)') 'Pour info nomgrd :'//nomgrd
        ASSERT(.false.)
    endif
end function
