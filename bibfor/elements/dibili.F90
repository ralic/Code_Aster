subroutine dibili(option, nomte, ndim, nbt, nno,&
                  nc, ulm, dul, pgl, iret)
    implicit none
#include "jeveux.h"
#include "asterfort/dinon4.h"
#include "asterfort/dinona.h"
#include "asterfort/dinonc.h"
#include "asterfort/infdis.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/u2mesk.h"
#include "asterfort/ut2mlg.h"
#include "asterfort/ut2vlg.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vecma.h"
#include "blas/dcopy.h"
!
    character(len=*) :: option, nomte
    integer :: ndim, nbt, nno, nc, iret
    real(kind=8) :: ulm(12), dul(12), pgl(3, 3)
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
    integer :: imat, ivarim, jdc, irep, iadzi, iazk24, neq, ivarip, ii, ifono, icontp, icompo
    integer :: icontm, iretvc
    real(kind=8) :: r8bid, klv(78), raide(6), ulp(12), temper, temp1, temp2, klc(144), fl(12)
    character(len=8) :: k8bid
    character(len=24) :: messak(5)
!
!   loi bi-linéaire sur 6 composantes
!       nbparc : 3 paramètres par composante
!       nbvint : 1 variables internes par composantes
    integer :: nbparc, nbvint
    parameter   (nbparc = 3, nbvint = 1*6)
!       nbpart : nombre paramètres total
!       valpar : valeur paramètres de la loi
!       nompar : nom des paramètres de la loi
    integer :: nbpart
    parameter   (nbpart = nbparc*6)
    real(kind=8) :: valpar(nbpart), coeflo(6, nbparc), vardnl(nbvint)
    integer :: codret(nbpart)
    character(len=8) :: nompar(nbpart)
    logical :: okdire(6)
!   nbparc paramètres par composante
    data nompar /'KDEB_DX','KFIN_DX','FPRE_DX',&
                 'KDEB_DY','KFIN_DY','FPRE_DY',&
                 'KDEB_DZ','KFIN_DZ','FPRE_DZ',&
                 'KDEB_RX','KFIN_RX','FPRE_RX',&
                 'KDEB_RY','KFIN_RY','FPRE_RY',&
                 'KDEB_RZ','KFIN_RZ','FPRE_RZ'/
!
! --------------------------------------------------------------------------------------------------
!
!   récupération du matériau
    call jevech('PMATERC', 'L', imat)
!   variables a t-
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PCONTMR', 'L', icontm)
!   récupération des caractéristiques
    call jevech('PCADISK', 'L', jdc)
    call jevech('PCOMPOR', 'L', icompo)
!
    call infdis('REPK', irep, r8bid, k8bid)
!   seulement en repere local : irep = 2
    if (irep .ne. 2) then
        messak(1) = nomte
        messak(2) = option
        messak(3) = zk16(icompo+3)
        messak(4) = zk16(icompo)
        call tecael(iadzi, iazk24)
        messak(5) = zk24(iazk24-1+3)
        call u2mesk('F', 'DISCRETS_5', 5, messak)
    endif
!   récupère tous les paramètres
!   température : si 2 noeuds ==> moyenne
    call rcvarc(' ', 'TEMP', '+', 'RIGI', 1,&
                1, temp1, iretvc)
    if (nno .eq. 2) then
        call rcvarc(' ', 'TEMP', '+', 'RIGI', 2,&
                    1, temp2, iretvc)
        temper = (temp1+temp2)*0.5d0
    endif
    valpar(:) = 0.0d0
    call rcvalb('FPG1', 1, 1, '+', zi(imat),&
                ' ', 'DIS_BILI_ELAS', 1, 'TEMP', temper,&
                nbpart, nompar, valpar, codret, 0)
!   les caractéristiques sont toujours dans le repère local on fait seulement une copie
    call dcopy(nbt, zr(jdc), 1, klv, 1)
!   si un ddl n'est pas affecte d'un comportement non-linéaire
!   il est donc élastique dans cette direction. ==> dinonc
    raide(:) = 0.0d0
    coeflo(:,:) = -1.0d0
!   examen des codret, valpar. on affecte raide, les paramètres
    call dinonc(nomte, codret, valpar, klv, raide,&
                nbparc, coeflo, okdire)
!   loi de comportement non-linéaire
    neq = nno*nc
    ulp(1:12) = ulm(1:12) + dul(1:12)
    vardnl(:) = 0.0d0
    call dinon4(neq, ulm, dul, ulp, nno,&
                nc, zr(ivarim), raide, nbparc, coeflo,&
                okdire, vardnl)
!   actualisation de la matrice quasi-tangente
    call dinona(nomte, raide, klv)
!   actualisation de la matrice quasi-tangente
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
        call jevech('PMATUUR', 'E', imat)
        if (ndim .eq. 3) then
            call utpslg(nno, nc, pgl, klv, zr(imat))
        elseif (ndim.eq.2) then
            call ut2mlg(nno, nc, pgl, klv, zr(imat))
        endif
    endif
!   calcul des efforts généralisés, des forces nodales et mise à jour des variables internes
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
        call jevech('PVECTUR', 'E', ifono)
        call jevech('PCONTPR', 'E', icontp)
!       demi-matrice klv transformée en matrice pleine klc
        call vecma(klv, nbt, klc, neq)
!       calcul de fl = klc.dul (incrément d'effort)
        call pmavec('ZERO', neq, klc, dul, fl)
!       efforts généralisés aux noeuds 1 et 2 (repère local)
!       on change le signe des efforts sur le premier noeud pour les MECA_DIS_TR_L et MECA_DIS_T_L
        if (nno .eq. 1) then
            do ii = 1, neq
                zr(icontp-1+ii) = fl(ii) + zr(icontm-1+ii)
                fl(ii) = fl(ii) + zr(icontm-1+ii)
            enddo
        elseif (nno.eq.2) then
            do ii = 1, nc
                zr(icontp-1+ii) = -fl(ii) + zr(icontm-1+ii)
                zr(icontp-1+ii+nc) = fl(ii+nc) + zr(icontm-1+ii+nc)
                fl(ii) = fl(ii) - zr(icontm-1+ii)
                fl(ii+nc) = fl(ii+nc) + zr(icontm-1+ii+nc)
            enddo
        endif
!       forces nodales aux noeuds 1 et 2 (repère global)
        if (nc .ne. 2) then
            call utpvlg(nno, nc, pgl, fl, zr(ifono))
        else
            call ut2vlg(nno, nc, pgl, fl, zr(ifono))
        endif
!       mise à jour des variables internes
        call jevech('PVARIPR', 'E', ivarip)
        do ii = 1, nbvint
            zr(ivarip+ii-1) = vardnl(ii)
        enddo
    endif
end subroutine
