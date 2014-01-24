subroutine dizeng(option, nomte, ndim, nbt, nno,&
                  nc, ulm, dul, pgl, iret)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
!        MODÈLE DE D'AMORTISSEUR DE ZENZER GÉNÉRALISÉ
!
!                         e2
!          e1     |-----======-----|
!      ---=====---|                |-----
!                 |--=====----=]---|
!                      e3    n3,a3
!
!  IN
!     option   : option de calcul
!     nomte    : nom terme élémentaire
!     ndim     : dimension du problème
!     nbt      : nombre de terme dans la matrice de raideur
!     nno      : nombre de noeuds de l'élément
!     nc       : nombre de composante par noeud
!     ulm      : déplacement moins
!     dul      : incrément de déplacement
!     pgl      : matrice de passage de global à local
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=*) :: option, nomte
    integer :: ndim, nbt, nno, nc, iret
    real(kind=8) :: ulm(12), dul(12), pgl(3, 3)
!
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/assert.h"
#include "asterfort/infdis.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rk5adp.h"
#include "asterfort/tecael.h"
#include "asterfort/ut2mlg.h"
#include "asterfort/ut2vlg.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vecma.h"
#include "asterfort/zengen.h"
#include "blas/dcopy.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: imat, ivarim, jdc, irep, jtp, jtm, ifono, icontp, ivarip, iadzi, iazk24, icompo
    integer :: icarcr
    integer :: icontm, ii, neq
    real(kind=8) :: r8bid, raidex, fl(12), klv(78), klc(144),raideurDeno
    character(len=8) :: k8bid
    character(len=24) :: messak(5)
!   pour la loi de comportement
    integer :: nbpara
!   SOUPL_1  RAIDE_2  SOUPL_3  RAID_VISQ   PUIS_VISQ
    parameter  (nbpara=5)
    real(kind=8) :: ldcpar(nbpara)
    real(kind=8) :: temps0, temps1, dtemps
!   équations du système : sigma, epsivis, epsi, puiss
    integer :: nbequa, nbdecp
    parameter  (nbequa=4)
    real(kind=8) :: y0(nbequa), dy0(nbequa), resu(nbequa*2)
    real(kind=8) :: errmax
!
    real(kind=8) :: precis
    parameter (precis=1.0e-08)
!
!   paramètres issus de DEFI_MATERIAU
    integer :: nbcar, ie1, ie2, ie3, in3, ia3, is1, is2, is3
    parameter  (nbcar=8, ie1=1, ie2=2, ie3=3, in3=4, ia3=5, is1=6,is2=7, is3=8)
    character(len=8) :: nomcar(nbcar)
    real(kind=8) :: valcar(nbcar)
    integer :: codcar(nbcar)
    data nomcar /'K1','K2','K3','C','PUIS_ALP','UNSUR_K1','UNSUR_K2','UNSUR_K3'/
! --------------------------------------------------------------------------------------------------
!
    neq = nno*nc
!
    call jevech('PCOMPOR', 'L', icompo)
!   récupération du matériau
    call jevech('PMATERC', 'L', imat)
!   variables a t-
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PCONTMR', 'L', icontm)
!   récupération des caractéristiques élastique
    call jevech('PCADISK', 'L', jdc)
    call infdis('REPK', irep, r8bid, k8bid)
!   seulement en repère local : irep = 2
    if (irep .ne. 2) then
        messak(1) = nomte
        messak(2) = option
        messak(3) = zk16(icompo+3)
        messak(4) = zk16(icompo)
        call tecael(iadzi, iazk24)
        messak(5) = zk24(iazk24-1+3)
        call utmess('I', 'DISCRETS_5', nk=5, valk=messak)
    endif
!   les caractéristiques sont toujours dans le repère local. on fait seulement une copie
    call dcopy(nbt, zr(jdc), 1, klv, 1)
!
!   les incréments de déplacement sont nuls
!       ==> récupération de la matrice tangente précédente, si possible
!       ==> si pas possible, calcul d'une tangente pas trop mauvaise, après lecture des paramètres
    if (option .eq. 'RIGI_MECA_TANG') then
!       tangente précédente
        if (abs(zr(ivarim+3)) .gt. r8miem()) then
            raidex = zr(ivarim+3)
            goto 800
        endif
    endif
!
!   récupère tous les paramètres
    valcar(:) = 0.0d0
    call rcvalb('FPG1', 1, 1, '+', zi(imat),&
                ' ', 'DIS_VISC', 0, ' ', [0.0d0],&
                nbcar, nomcar, valcar, codcar, 0)
!   examen des codcar. assert pas nécessaire ? car un_parmi(a,b) dans les catalogues
    ASSERT(codcar(ie1)+codcar(is1) .eq. 1)
    ASSERT(codcar(ie2)+codcar(is2) .eq. 1)
    ASSERT(codcar(ie3)+codcar(is3) .eq. 1)
    ASSERT(codcar(in3) .eq. 0)
    ASSERT(codcar(ia3) .eq. 0)
!
!   paramètres de la loi de comportement
!     souple1  raide2  souple3  raide_vi  puiss_vi
!
!   cara 1 : souplesse = 1/raideur
    if (codcar(ie1) .eq. 0) then
        ldcpar(1) = 1.0d0/valcar(ie1)
    else
        ldcpar(1) = valcar(is1)
    endif
!   cara 2 : raideur = 1/souplesse
    if (codcar(ie2) .eq. 0) then
        ldcpar(2) = valcar(ie2)
    else
        ldcpar(2) = 1.0d0/valcar(is2)
    endif
!   cara 3 : souplesse = 1/raideur
    if (codcar(ie3) .eq. 0) then
        ldcpar(3) = 1.0d0/valcar(ie3)
    else
        ldcpar(3) = valcar(is3)
    endif
!
    raideurDeno = (ldcpar(1)+ldcpar(3)+ldcpar(2)*ldcpar(1)*ldcpar(3))
    if ( raideurDeno .le. r8miem() ) then
        messak(1) = nomte
        messak(2) = option
        messak(3) = zk16(icompo+3)
        messak(4) = zk16(icompo)
        call tecael(iadzi, iazk24)
        messak(5) = zk24(iazk24-1+3)
        call utmess('F', 'DISCRETS_4', nk=5, valk=messak)
    endif
!
    ldcpar(4) = valcar(in3)
    ldcpar(5) = valcar(ia3)
!
!   les incréments de déplacement sont nuls
!       ==> la récupération de la matrice tangente précédente a échouée
!       ==> calcul d'une tangente pas trop mauvaise
    if (option .eq. 'RIGI_MECA_TANG') then
        raidex=(1.0d0 + ldcpar(2)*ldcpar(3))/raideurDeno
        goto 800
    endif
!
!   loi de comportement non-linéaire : récupération du temps + et - , calcul de dt
    call jevech('PINSTPR', 'L', jtp)
    call jevech('PINSTMR', 'L', jtm)
    temps0 = zr(jtm)
    temps1 = zr(jtp)
    dtemps = temps1 - temps0
!   contrôle de rk5 : découpage successif, erreur maximale
    call jevech('PCARCRI', 'L', icarcr)
!   nombre d'itérations maxi (ITER_INTE_MAXI)
    nbdecp = int(zr(icarcr))
!   tolérance de convergence (RESI_INTE_RELA)
    errmax = zr(icarcr+2)
!   comportement non-linéaire suivant le x local
!   équations du système :
!              1       2         3     4
!       yy   : sigma, epsivisq, epsi,  puiss
!       vari : sigma, epsivisq, puiss, tangente
    if (nno .eq. 1) then
        y0(3) = ulm(1)
        dy0(3) = dul(1)/dtemps
    else
        y0(3) = ulm(1+nc) - ulm(1)
        dy0(3) = (dul(1+nc) - dul(1))/dtemps
    endif
!   récupération de l'effort précédent, suivant l'axe x local
    y0(1) = zr(icontm)
!   récupération des variables internes : epsivis  puiss  tangente
    y0(2) = zr(ivarim+1)
    y0(4) = zr(ivarim+2)
    call rk5adp(nbequa, ldcpar, temps0, dtemps, nbdecp,&
                errmax, y0, dy0, zengen, resu,&
                iret)
!   resu(1:nbeq)            : variables intégrées
!   resu(nbeq+1:2*nbeq)     : d(resu)/d(t) a t+dt
    if (iret .ne. 0) goto 999
!   calcul de la tangente au comportement
    if (abs(resu(nbequa+3)) .gt. precis) then
        raidex = resu(nbequa + 1)/resu(nbequa + 3)
    else
        raidex = resu(nbequa + 1)
    endif
    if ( abs(raidex) .lt. precis ) then
        raidex=(1.0d0 + ldcpar(2)*ldcpar(3))/raideurDeno
    endif
!   actualisation de la matrice quasi-tangente
800  continue
!
    if (nomte .eq. 'MECA_DIS_TR_L') then
        klv(1) = raidex
        klv(28) = raidex
        klv(22) = -raidex
    else if (nomte.eq.'MECA_DIS_TR_N') then
        klv(1) = raidex
    else if (nomte.eq.'MECA_DIS_T_L') then
        klv(1) = raidex
        klv(10) = raidex
        klv(7) = -raidex
    else if (nomte.eq.'MECA_DIS_T_N') then
        klv(1) = raidex
    else if (nomte.eq.'MECA_2D_DIS_T_L') then
        klv(1) = raidex
        klv(6) = raidex
        klv(4) = -raidex
    else if (nomte.eq.'MECA_2D_DIS_T_N') then
        klv(1) = raidex
    else if (nomte.eq.'MECA_2D_DIS_TR_L') then
        klv(1) = raidex
        klv(10) = raidex
        klv(7) = -raidex
    else if (nomte.eq.'MECA_2D_DIS_TR_N') then
        klv(1) = raidex
    endif
!   actualisation de la matrice quasi-tangente
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
        call jevech('PMATUUR', 'E', imat)
        if (ndim .eq. 3) then
            call utpslg(nno, nc, pgl, klv, zr(imat))
        else if (ndim.eq.2) then
            call ut2mlg(nno, nc, pgl, klv, zr(imat))
        endif
    endif
!
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
!       calcul des efforts généralisés, des forces nodales
        call jevech('PVECTUR', 'E', ifono)
        call jevech('PCONTPR', 'E', icontp)
!       demi-matrice klv transformée en matrice pleine klc
        call vecma(klv, nbt, klc, neq)
!       calcul de fl = klc.dul (incrément d'effort)
        call pmavec('ZERO', neq, klc, dul, fl)
!       efforts généralisés aux noeuds 1 et 2 (repère local)
!       on change le signe des efforts sur le premier noeud pour les MECA_DIS_TR_L et MECA_DIS_T_L
        if (nno .eq. 1) then
            do ii = 1, nc
                zr(icontp-1+ii) = fl(ii) + zr(icontm-1+ii)
                fl(ii) = fl(ii) + zr(icontm-1+ii)
            enddo
            zr(icontp) = resu(1)
            fl(1) = resu(1)
        else if (nno.eq.2) then
            do ii = 1, nc
                zr(icontp-1+ii) = -fl(ii) + zr(icontm-1+ii)
                zr(icontp-1+ii+nc) = fl(ii+nc) + zr(icontm-1+ii+nc)
                fl(ii) = fl(ii) - zr(icontm-1+ii)
                fl(ii+nc) = fl(ii+nc) + zr(icontm-1+ii+nc)
            enddo
            zr(icontp) = resu(1)
            zr(icontp+nc) = resu(1)
            fl(1) = -resu(1)
            fl(1+nc) = resu(1)
        endif
!       forces nodales aux noeuds 1 et 2 (repère global)
        if (nc .ne. 2) then
            call utpvlg(nno, nc, pgl, fl, zr(ifono))
        else
            call ut2vlg(nno, nc, pgl, fl, zr(ifono))
        endif
!       mise à jour des variables internes : sigma  epsivis  puiss tangente
        call jevech('PVARIPR', 'E', ivarip)
        zr(ivarip) = resu(1)
        zr(ivarip+1) = resu(2)
        zr(ivarip+2) = resu(4)
        zr(ivarip+3) = raidex
        if (nno .eq. 2) then
            zr(ivarip+4) = resu(1)
            zr(ivarip+4+1) = resu(2)
            zr(ivarip+4+2) = resu(4)
            zr(ivarip+4+3) = raidex
        endif
    endif
999  continue
end subroutine
