subroutine diisotrope(option, nomte, ndim, nbt, nno,&
                  nc, ulm, dul, pgl, iret)
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
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
!        COMPORTEMENT ISOTROPE
!
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
#include "asterfort/rk5adp.h"
#include "asterfort/tecael.h"
#include "asterfort/ut2mlg.h"
#include "asterfort/ut2vlg.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vecma.h"
#include "asterfort/disc_isotr.h"
#include "blas/dcopy.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: imat, ivarim, jdc, irep, jtp, jtm, ifono, icontp, ivarip, iadzi, iazk24, icompo
    integer :: icarcr, idf, ipi, imate, imater, jmat, nbmat, nbvale, jvale, jprol
    integer :: icontm, ii, neq, kk
    real(kind=8) :: r8bid, raidex, fl(12), klv(78), klc(144)
    character(len=8) :: k8bid
    character(len=24) :: messak(6)
!   pour la loi de comportement
    integer :: nbpara, nbfct
    parameter  (nbpara=2, nbfct=1*3)
    integer      :: ldcfct(nbfct)
    real(kind=8) :: ldcpar(nbpara)
    real(kind=8) :: temps0, temps1, dtemps
!   équations du système : force, Up, U, Puiss, P
    integer :: nbequa, nbdecp
    parameter  (nbequa=5)
    real(kind=8) :: y0(nbequa), dy0(nbequa), resu(nbequa*2)
    real(kind=8) :: errmax
!
    real(kind=8) :: precis
    parameter (precis=1.0e-08)
!
! --------------------------------------------------------------------------------------------------
!   Paramètres associés au matériau codé
    integer :: lmat, lfct
    parameter  ( lmat = 9 , lfct = 10 )
! --------------------------------------------------------------------------------------------------
!
    neq = nno*nc
!   Comportement
    call jevech('PCOMPOR', 'L', icompo)
!   Récupération du matériau
    call jevech('PMATERC', 'L', imater)
!   Variables internes a t- : Force  Up  Puiss  tangente
    call jevech('PVARIMR', 'L', ivarim)
!   Effort à t-
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
!   Adresse de la SD mater
    jmat = zi(imater)
!   Nombre de matériau sur la maille : 1 seul autorisé
    nbmat=zi(jmat)
    ASSERT(nbmat.eq.1)
!   Adresse du matériau codé
    imate = jmat+zi(jmat+nbmat+1)
!   Recherche du comportement dans la SD compor
    ipi = 0
    do kk = 1, zi(imate+1)
        if ( zk32(zi(imate)+kk-1)(1:16) .eq. zk16(icompo) ) then
            ipi = zi(imate+2+kk-1)
            goto 10
        endif
    enddo
    messak(1) = nomte
    messak(2) = option
    messak(3) = zk16(icompo+3)
    messak(4) = zk16(icompo)
    call tecael(iadzi, iazk24)
    messak(5) = zk24(iazk24-1+3)
    call utmess('F', 'DISCRETS_7', nk=5, valk=messak)
10  continue
!
    ldcfct(:) = -1
    idf = zi(ipi)+zi(ipi+1)
    do kk = 1, zi(ipi+2)
        if ('FX  ' .eq. zk16(zi(ipi+3)+idf+kk-1)) then
            ldcfct(1) = ipi+lmat-1+lfct*(kk-1)
        endif
    enddo
    ASSERT( ldcfct(1) .ne. -1 )
!   Nombre de point de la fonction
    nbvale = zi(ldcfct(1))
!   Adresse des informations sur le type de fonction
    jprol  = zi(ldcfct(1)+1)
!   Adresse des valeurs de la fonction
    jvale  = zi(ldcfct(1)+2)
!   Pour l'intégration
    ldcfct(1) = nbvale
    ldcfct(2) = jprol
    ldcfct(3) = jvale
!   les incréments de déplacement sont nuls
!       ==> récupération de la matrice tangente précédente, si possible
!       ==> si pas possible, pente initiale de la courbe
    if (option .eq. 'RIGI_MECA_TANG') then
!       La tangente est donnée par la pente initiale
        raidex= zr(jvale+nbvale+1)/zr(jvale+1)
!       Tangente précédente
        if (abs(zr(ivarim+5)) .gt. r8miem()) then
            raidex = zr(ivarim+5)
        endif
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
!              1      2   3      4      5     6
!       yy   : force, Up, U,     puiss, pcum
!       vari : force, U,  puiss, Up,    pcum, tangente
    if (nno .eq. 1) then
        y0(3)  = ulm(1)
        dy0(3) = dul(1)/dtemps
    else
        y0(3)  = (ulm(1+nc) - ulm(1))
        dy0(3) = (dul(1+nc) - dul(1))/dtemps
    endif
!   récupération de l'effort précédent, suivant l'axe x local
    y0(1) = zr(icontm)
!   récupération des variables internes
    y0(2) = zr(ivarim+3)
    y0(4) = zr(ivarim+2)
    y0(5) = zr(ivarim+4)
!   Le seuil élastique et le déplacement correspondant
    ldcpar(1) = zr(jvale+nbvale+1)
    ldcpar(2) = zr(jvale+1)
!
    call rk5adp(nbequa, ldcpar, temps0, dtemps, nbdecp,&
                errmax, y0, dy0, disc_isotr, resu,&
                iret, fonction=ldcfct)
!   resu(1:nbeq)            : variables intégrées
!   resu(nbeq+1:2*nbeq)     : d(resu)/d(t) a t+dt
    if (iret .ne. 0) goto 999
!
!   calcul de la tangente au comportement
    raidex=ldcpar(1)/ldcpar(2)
    if (abs(resu(nbequa+3)) .gt. precis) then
        raidex = resu(nbequa + 1)/resu(nbequa + 3)
    endif
!   actualisation de la matrice quasi-tangente
800 continue
!
    if (nomte .eq. 'MECA_DIS_TR_L') then
        klv(1)  =  raidex
        klv(28) =  raidex
        klv(22) = -raidex
    else if (nomte.eq.'MECA_DIS_TR_N') then
        klv(1)  =  raidex
    else if (nomte.eq.'MECA_DIS_T_L') then
        klv(1)  =  raidex
        klv(10) =  raidex
        klv(7)  = -raidex
    else if (nomte.eq.'MECA_DIS_T_N') then
        klv(1)  =  raidex
    else if (nomte.eq.'MECA_2D_DIS_T_L') then
        klv(1)  =  raidex
        klv(6)  =  raidex
        klv(4)  = -raidex
    else if (nomte.eq.'MECA_2D_DIS_T_N') then
        klv(1)  = raidex
    else if (nomte.eq.'MECA_2D_DIS_TR_L') then
        klv(1)  =  raidex
        klv(10) =  raidex
        klv(7)  = -raidex
    else if (nomte.eq.'MECA_2D_DIS_TR_N') then
        klv(1)  =  raidex
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
                fl(ii)          = fl(ii) + zr(icontm-1+ii)
            enddo
            zr(icontp) = resu(1)
            fl(1)      = resu(1)
        else if (nno.eq.2) then
            do ii = 1, nc
                zr(icontp-1+ii)    = -fl(ii)    + zr(icontm-1+ii)
                zr(icontp-1+ii+nc) =  fl(ii+nc) + zr(icontm-1+ii+nc)
                fl(ii)             =  fl(ii)    - zr(icontm-1+ii)
                fl(ii+nc)          =  fl(ii+nc) + zr(icontm-1+ii+nc)
            enddo
            zr(icontp)    =  resu(1)
            zr(icontp+nc) =  resu(1)
            fl(1)         = -resu(1)
            fl(1+nc)      =  resu(1)
        endif
!       forces nodales aux noeuds 1 et 2 (repère global)
        if (nc .ne. 2) then
            call utpvlg(nno, nc, pgl, fl, zr(ifono))
        else
            call ut2vlg(nno, nc, pgl, fl, zr(ifono))
        endif
!       mise à jour des variables internes
        call jevech('PVARIPR', 'E', ivarip)
        zr(ivarip)   = resu(1)
        zr(ivarip+1) = resu(3)
        zr(ivarip+2) = resu(4)
        zr(ivarip+3) = resu(2)
        zr(ivarip+4) = resu(5)
        zr(ivarip+5) = raidex
        if (nno .eq. 2) then
            zr(ivarip+6)   = resu(1)
            zr(ivarip+6+1) = resu(3)
            zr(ivarip+6+2) = resu(4)
            zr(ivarip+6+3) = resu(2)
            zr(ivarip+6+4) = resu(5)
            zr(ivarip+6+5) = raidex
        endif
    endif
999 continue
end subroutine
