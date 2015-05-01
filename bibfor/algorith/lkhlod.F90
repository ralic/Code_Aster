function lkhlod(gamcjs, rcos3t)
!
    implicit none
    real(kind=8) :: gamcjs, rcos3t, lkhlod
! =================================================================
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
! =================================================================
! --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
! =================================================================
! --- BUT : CALCUL DE H(T) OU T DESIGNE L'ANGLE DE LODE
! --- LA DIFFERENCE PAR RAPPORT A LA FONCTION HLODE EST LE SIGNE --
! =================================================================
! IN  : GAMCJS : PARAMETRE DE FORME DE LA SURFACDE DE CHARGE ------
! ------------ : DANS LE PLAN DEVIATOIRE --------------------------
! --- : RCOS3T : COS(3T) ------------------------------------------
! OUT : LKHLOD  = (1-GAMMA_CJS*COS3T)**(1/6) ----------------------
! =================================================================
    real(kind=8) :: un, six
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( six    =  6.0d0  )
! =================================================================
    lkhlod = (un-gamcjs*rcos3t)**(un/six)
! =================================================================
end function
