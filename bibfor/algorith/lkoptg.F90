subroutine lkoptg(val, dum, dt, nbmat, mater,&
                  invar, s, iel, sel, ucrpm,&
                  ucrvm, ucriv, seuilv, vinm, de,&
                  depsv, dside, retcom)
!
    implicit   none
#include "asterfort/lcprmm.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcprte.h"
#include "asterfort/lctrma.h"
#include "asterfort/lkbpri.h"
#include "asterfort/lkcalg.h"
#include "asterfort/lkcaln.h"
#include "asterfort/lkdepp.h"
#include "asterfort/lkdepv.h"
#include "asterfort/lkdfds.h"
#include "asterfort/lkdfdx.h"
#include "asterfort/lkdhds.h"
#include "asterfort/lkdphi.h"
#include "asterfort/lkds2h.h"
#include "asterfort/lkdvds.h"
#include "asterfort/lkvacp.h"
#include "asterfort/lkvacv.h"
#include "asterfort/lkvarp.h"
#include "asterfort/lkvarv.h"
#include "asterfort/r8inir.h"
    integer :: val, dum, nbmat, retcom
    real(kind=8) :: dt, invar, s(6), iel, sel(6), mater(nbmat, 2), vinm(7)
    real(kind=8) :: dside(6, 6), de(6, 6)
    real(kind=8) :: ucrpm, ucrvm, ucriv, seuilv
! =================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --- BUT : DERIVEE DE L ACCROISSEMENT DU MULTIPLICATEUR PLASTIQUE
! --- PAR RAPPORT A L4ACROISSEMENT DE LA DEFORMATION
! =================================================================
! IN  : VAL   : INDICATEUR POUR DISTINGUER LES LOIS DE DILATANCE --
! --- : DUM   : INDICATEUR CONTRACTANCE OU DILATANCE --------------
! --- : DGAMV :  ACCROISSEMENT DE GAMMA VISCOPLASTIQUE ------------
! --- : DT    :  PAS DE TEMPS -------------------------------------
! --- : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
! --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
! ----------- :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
! ----------- :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
! --- : INVAR :  INVARIANT DES CONTRAINTES ------------------------
! --- : S     :  DEVIATEUR DES CONTRAINTES ------------------------
! --- : IEL   :  INVARIANT DES CONTRAINTES DE LA PREDICTION--------
! --- : SEL   :  DEVIATEUR DES CONTRAINTES DE LA PREDICTION--------
! --- : UCRPM :  VALEUR DE U PLAS POUR LES CONT.  A L INSTANT MOINS
! --- : UCRVM :  VALEUR DE U VISC POUR LES CONT.  A L INSTANT MOINS
! --- : UCRIV :  VALEUR DE U VISC POUR LES CONT.  A LA PREDICTION--
! --- : SEUILV:  VALEUR DU SEUIL VISQUEUX A LA PREDICTION----------
! --- : VINM  :  VARIABLES INTERNES -------------------------------
! --- : DE    :  MATRICE ELASTIQUE --------------------------------
! --- : DEPSV :  ACCROISSEMENT DES DEFORMATIONS VISCOPLASTIQUE A T
! --- : DSIDE :  COMPOSANTS DE L OPERATEUR TANGENT  --------------
! ----: RETCOM: CODE RETOUR POUR REDECOUPAGE DU PAS DE TEMPS ------
! =================================================================
    common /tdim/   ndt , ndi
    integer :: ndi, ndt, i, k
    real(kind=8) :: paraep(3), varpl(4), derpar(3)
    real(kind=8) :: paravi(3), varvi(4)
    real(kind=8) :: dhds(6), ds2hds(6), dfdsp(6)
    real(kind=8) :: dhdsv(6), ds2hdv(6), dfdsv(6)
    real(kind=8) :: dhdsve(6), ds2hde(6), dfdsve(6)
    real(kind=8) :: vecnp(6), vecnv(6), gp(6), gv(6), devgii
    real(kind=8) :: degv(6), degp(6), dgp(6, 6), dgv(6, 6)
    real(kind=8) :: dfdegp, dfdxip, dphigv(6, 6), dedgp(6, 6), dedgv(6, 6)
    real(kind=8) :: dphi(6), ddlam(6), dvds(6, 6)
    real(kind=8) :: aa(6, 6), cc(6, 6), dd(6), nume(6)
    real(kind=8) :: depsv(6), ddepsv(6), ddgamv(6), dgamv
    real(kind=8) :: bprimp, bprimv
    real(kind=8) :: deux, trois, bidon, vintr
    real(kind=8) :: aat(6, 6), cct(6, 6)
! =================================================================
! --- INITIALISATION DE PARAMETRES --------------------------------
! =================================================================
    parameter       ( deux   =  2.0d0   )
    parameter       ( trois  =  3.0d0   )
! =================================================================
    vintr = vinm(3)
!
    call lkvarp(vinm, nbmat, mater, paraep)
!
    call lkvacp(nbmat, mater, paraep, varpl)
!
    call lkdepp(vinm, nbmat, mater, paraep, derpar)
!
    call lkvarv(vintr, nbmat, mater, paravi)
!
    call lkvacv(nbmat, mater, paravi, varvi)
!
! =================================================================
! --- RECUPERATION DE DFd/DSIGM(-) --------------------------------
! =================================================================
    call lkdhds(nbmat, mater, invar, s, dhds,&
                retcom)
    call lkds2h(nbmat, mater, invar, s, dhds,&
                ds2hds, retcom)
!
    call lkdfds(nbmat, mater, s, paraep, varpl,&
                ds2hds, ucrpm, dfdsp)
! =================================================================
! --- RECUPERATION DE DFv/DSIGM (-) -------------------------------
! =================================================================
    call lkdhds(nbmat, mater, invar, s, dhdsv,&
                retcom)
    call lkds2h(nbmat, mater, invar, s, dhdsv,&
                ds2hdv, retcom)
!
    call lkdfds(nbmat, mater, s, paravi, varvi,&
                ds2hdv, ucrvm, dfdsv)
!
! =================================================================
! --- RECUPERATION DE DFv/DSIGM(E) --------------------------------
! =================================================================
    call lkdhds(nbmat, mater, iel, sel, dhdsve,&
                retcom)
    call lkds2h(nbmat, mater, iel, sel, dhdsve,&
                ds2hde, retcom)
!
    call lkdfds(nbmat, mater, sel, paravi, varvi,&
                ds2hde, ucriv, dfdsve)
! =================================================================
! --- RECUPERATION DE GPLAS ---------------------------------------
! =================================================================
    bprimp = lkbpri(val,vinm,nbmat,mater,paraep,invar,s)
!
    call lkcaln(s, bprimp, vecnp, retcom)
!
    call lkcalg(dfdsp, vecnp, gp, devgii)
! =================================================================
! --- RECUPERATION DE GVISC ---------------------------------------
! =================================================================
    val = 0
    bprimv = lkbpri(val,vinm,nbmat,mater,paravi,invar,s)
!
    call lkcaln(s, bprimv, vecnv, retcom)
!
    call lkcalg(dfdsv, vecnv, gv, bidon)
! =================================================================
! --- RECUPERATION DE DPHI/DEPS ET SA MULTIPLICATION PAR GVISC
! =================================================================
    call lkdphi(nbmat, mater, de, seuilv, dfdsve,&
                dphi)
!
    call lcprmv(de, gv, degv)
!
    call lcprte(degv, dphi, dphigv)
!
    do 10 i = 1, ndt
        do 20 k = 1, ndt
            aa(i,k) = de(i,k) - dphigv(i,k)*dt
20      end do
10  end do
!
! =================================================================
! --- PRODUIT DE DF/DSIG PAR AA -----------------------------------
! =================================================================
    call lctrma(aa, aat)
    call lcprmv(aat, dfdsp, nume)
!
! =================================================================
! --- RECUPERATION DE DF/DXIP -------------------------------------
! =================================================================
    call lkdfdx(nbmat, mater, ucrpm, invar, s,&
                paraep, varpl, derpar, dfdxip)
! =================================================================
! --- PRODUIT DE DE PAR G -----------------------------------------
! =================================================================
    call r8inir(6, 0.d0, degp, 1)
    call lcprmv(de, gp, degp)
!
! =================================================================
! --- PRODUIT DE DF/DSIG PAR DEGP----------------------------------
! =================================================================
    call lcprsc(dfdsp, degp, dfdegp)
!
! =================================================================
! --- CALCUL DE DGAMV/ DEPS----------------------------------------
! =================================================================
    call lkdepv(nbmat, mater, depsv, ddepsv, dgamv,&
                ddgamv)
! =================================================================
! --- CALCUL DE DEPSV/ DSIG----------------------------------------
! =================================================================
    call r8inir(6*6, 0.d0, dvds, 1)
    call r8inir(6*6, 0.d0, cc, 1)
    call r8inir(6, 0.d0, dd, 1)
!
    call lkdvds(dt, nbmat, mater, gv, dfdsve,&
                seuilv, dvds)
!
    call lcprmm(dvds, de, cc)
    call lctrma(cc, cct)
    call lcprmv(cct, ddgamv, dd)
! =================================================================
! --- CALCUL DE DLAM ----------------------------------------------
! =================================================================
!
    do 30 i = 1, ndt
!
        if (dum .eq. 0) then
!
            ddlam(i) = nume(i)/(dfdegp - dfdxip*sqrt(deux/trois)* devgii)
!
        else
            ddlam(i) = ( nume(i) + dfdxip*dd(i))/ (dfdegp - dfdxip* sqrt(deux/trois)*devgii )
        endif
!
30  end do
! =================================================================
! --- CALCUL DE L OPERATEUR TANGENT -------------------------------
! =================================================================
    call r8inir(6*6, 0.d0, dgp, 1)
    call r8inir(6*6, 0.d0, dgv, 1)
    call r8inir(6*6, 0.d0, dedgp, 1)
    call r8inir(6*6, 0.d0, dedgv, 1)
!
    call lcprte(degp, ddlam, dedgp)
!
    call r8inir(6*6, 0.d0, dside, 1)
!
    do 40 i = 1, ndt
        do 50 k = 1, ndt
            dside(i,k) = de(i,k) - dedgp(i,k) - dphigv(i,k)*dt
50      end do
40  end do
! =================================================================
end subroutine
