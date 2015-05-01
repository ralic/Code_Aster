subroutine lkdgde(val, vintr, dt, seuive, ucrim,&
                  im, sm, vinm, nbmat, mater,&
                  depsv, dgamv, retcom)
!
    implicit    none
#include "asterfort/lcdevi.h"
#include "asterfort/lkbpri.h"
#include "asterfort/lkcalg.h"
#include "asterfort/lkcaln.h"
#include "asterfort/lkdfds.h"
#include "asterfort/lkdhds.h"
#include "asterfort/lkds2h.h"
#include "asterfort/lkvacv.h"
#include "asterfort/lkvarv.h"
    integer :: nbmat, retcom, val
    real(kind=8) :: seuive, ucrim, im, sm(6), vintr
    real(kind=8) :: mater(nbmat, 2), vinm(7), depsv(6), dgamv
    real(kind=8) :: dt
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
! --- BUT : DEFINITION DE LA DEFORMATION VISQUEUSE ET DU PARAMETRE
! ---- D ECROUISSAGE VISQUEUX
! =================================================================
! IN  : VAL   :  INDICATEUR POUR LES LOIS DE DILATANCE ------------
! --- : VINTR  :  INDICATEUR CONTRACTANCE OU  DILATANCE ------------
! --- : DT    :  PAS DE TEMPS -------------------------------------
! --- : SEUIVE:  SEUIL VISQUEUX EN FONCTION DE LA PREDICITION------
!---- : UCRIM :  EN FONCTION DES CONTRAINTES A LINSTANT MOINS------
! --- : IM    :  INVARIANT DES CONTRAINTES A L INSTANT MOINS-------
! --- : SM    :  DEVIATEUR DES CONTRAINTES A L INSTANT MOINS-------
! --- : VINM  :  VARIABLES INTERNES -------------------------------
! --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
! ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
! OUT : DEPSV : DEFORMATIONS VISQUEUSES ---------------------------
!     : DGAMV : PARAMETRE D ECROUISSAGE VISQUEUX ------------------
! --- : RETCOM: CODE RETOUR POUR REDECOUPAGE DU PAS DE TEMPS-------
! =================================================================
    common /tdim/   ndt , ndi
    integer :: i, ndi, ndt
    real(kind=8) :: a, n, pa
    real(kind=8) :: bidon, deux, trois, zero
    real(kind=8) :: paravi(3), varvi(4)
    real(kind=8) :: dhds(6), ds2hds(6), dfdsv(6)
    real(kind=8) :: bprime, vecnv(6), gv(6)
    real(kind=8) :: ddepsv(6)
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( zero    =  0.0d0   )
    parameter       ( deux    =  2.0d0   )
    parameter       ( trois   =  3.0d0   )
! =================================================================
! --- RECUPERATION DES DONNEES MATERIAUX --------------------------
! =================================================================
    pa = mater(1,2)
    a = mater(21,2)
    n = mater(22,2)
! =================================================================
! --- CALCUL DE DF/DSIG ------------------------------------
! =================================================================
!
    call lkdhds(nbmat, mater, im, sm, dhds,&
                retcom)
    call lkds2h(nbmat, mater, im, sm, dhds,&
                ds2hds, retcom)
    call lkvarv(vintr, nbmat, mater, paravi)
!
    call lkvacv(nbmat, mater, paravi, varvi)
    call lkdfds(nbmat, mater, sm, paravi, varvi,&
                ds2hds, ucrim, dfdsv)
!
    bprime = lkbpri (val,vinm,nbmat,mater,paravi,im,sm)
!
    call lkcaln(sm, bprime, vecnv, retcom)
! =================================================================
! --- CALCUL DE GVISC ------------------------------------
! =================================================================
    call lkcalg(dfdsv, vecnv, gv, bidon)
! =================================================================
! --- CALCUL DE DEPSV ------------------------------------
! =================================================================
    do 10 i = 1, ndt
        if (seuive .le. zero) then
            depsv(i) = zero
        else
            depsv(i) = a * (seuive/pa)**n*gv(i)*dt
        endif
10  end do
!
! =================================================================
! --- CALCUL DU DEVIATEUR DU TENSEUR DES DEFORMATIONS VISQUEUSES -
! =================================================================
    call lcdevi(depsv, ddepsv)
!
! =================================================================
! --- CALCUL DE DGAMV ------------------------------------
! =================================================================
!
    dgamv = 0.d0
!
    do 20 i = 1, ndt
        dgamv = dgamv + ddepsv(i)**2
20  end do
    dgamv = sqrt(deux/trois * dgamv)
! =================================================================
end subroutine
