subroutine mnlali(reprise, modini, imat, xcdl, parcho,&
                  adime, ninc, nd, nchoc, h,&
                  hf, ampl, xvect, lnm, num_ordr)
    implicit none
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
! ----------------------------------------------------------------------------
!
!       MODE NON LINEAIRE - ALGORITHME - INITIALISATION
!       -         -        -             --           -
! ----------------------------------------------------------------------------
!
! INITIALISATION DU POINT DE DEPART DE LA MAN
! ----------------------------------------------------------------------------
! IN   REPRISE  : L    : INDIQUE SI ON EFFECTUE UNE REPRISE OU NON
! IN   MODINI   : K8   : RESULTAT POUR LA REPRISE (SI REPRISE=TRUE)
! IN   IMAT     : I(2) : DESCRIPTEUR DES MATRICES :
!                       - IMAT(1) => MATRICE DE RAIDEUR
!                       - IMAT(2) => MATRICE DE MASSE
! IN   XCDL     : K14  : INDICE DES CONDITIONS AUX LIMITES
! IN   PARCHO   : K14  : SD PARAMETRE DES CONTACTEURS
! IN   ADIME    : K14  : SD PARAMETRE POUR ADIMENSIONNEMENT
! IN   NINC     : I    : NOMBRE D INCONNUES DU SYSTEME
! IN   ND       : I    : NOMBRE DE DEGRES DE LIBERTE ACTIFS
! IN   NCHOC    : I    : NOMBRE DE CONTACTEURS
! IN   H        : I    : NOMBRE D'HARMONIQUES POUR U
! IN   HF       : I    : NOMBRE D'HARMONIQUES POUR F
! IN   AMPL     : R8   : AMPLITUDE DE DEPART
! OUT  XVECT    : K14  : NOM DU VECTEUR A INITIALISER
! IN   LNM      : K8   : NOM DU CONCEPT MODE LINE POUR INITIALISER
! IN   NUM_ORDR : K8   : NUMERO D'ORDRE DU CONCEPT MODE LINE POUR INITIALISER
! ----------------------------------------------------------------------------
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
#include "blas/idamax.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mnlbil.h"
#include "asterfort/mnlcir.h"
#include "asterfort/mnluil.h"
#include "asterfort/nmop45.h"
#include "asterc/r8depi.h"
#include "asterfort/rsadpa.h"
#include "asterfort/vprecu.h"
#include "asterfort/wkvect.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedetc.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
    aster_logical :: reprise
    integer :: imat(2), ninc, nd, nchoc, h, hf, num_ordr
    character(len=8) :: modini
    character(len=14) :: parcho, adime, xcdl, xvect
    real(kind=8) :: ampl
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    character(len=19) :: matrma, matrig, clnm
    character(len=8) :: lnm
    character(len=14) :: xdep1, xdep2, xtemp
    character(len=24) :: typmod
    character(len=16) :: k16bid
    real(kind=8) :: omega, lambda, ampref, alpha, eta, jeu
    integer :: ivect, neq, neqv, nbmode, nbpari, nbparr, nbpark, ilnm, ifreq
    integer :: iadim, i, j, k, icdl
    integer :: neqs, ijmax, nddlx, nddly
    integer :: nt, nddl, ht, idep1, idep2, itemp
    real(kind=8), pointer :: raid(:) => null()
    real(kind=8), pointer :: reg(:) => null()
    character(len=8), pointer :: type(:) => null()
    integer, pointer :: vneqs(:) => null()
    real(kind=8), pointer :: vjeu(:) => null()
    real(kind=8), pointer :: orig(:) => null()
    integer, pointer :: vnddl(:) => null()
!
    call jemarq()
! ----------------------------------------------------------------------
! --- RECUPERATION DES NOMS ET DE LA TAILLE DES
! ---                                   MATRICES DE RIGIDITE ET DE MASSE
! ----------------------------------------------------------------------
    matrig=zk24(zi(imat(1)+1))(1:19)
    matrma=zk24(zi(imat(2)+1))(1:19)
    neq = zi(imat(1)+2)
! ----------------------------------------------------------------------
! --- QUELQUES VALEURS UTILES
! ----------------------------------------------------------------------
    lambda = 0.d0
    call jeveuo(adime, 'L', iadim)
    call jeveuo(xcdl, 'L', icdl)
    call jeveuo(parcho//'.NDDL', 'L', vi=vnddl)
    call jeveuo(parcho//'.REG', 'L', vr=reg)
    call jeveuo(parcho//'.JEU', 'L', vr=vjeu)
    call jeveuo(parcho//'.JEUMAX', 'L', ijmax)
    call jeveuo(parcho//'.TYPE', 'L', vk8=type)
    call jeveuo(parcho//'.RAID', 'L', vr=raid)
    call jeveuo(parcho//'.NEQS', 'L', vi=vneqs)
    call jeveuo(parcho//'.ORIG', 'L', vr=orig)
! ----------------------------------------------------------------------
! --- RECUPERATION ET MISE A ZERO DU VECTEUR D'INITIALISATION
! ----------------------------------------------------------------------
    call jeveuo(xvect, 'E', ivect)
! ----------------------------------------------------------------------
! --- INITIALISATION
! ----------------------------------------------------------------------
    call dscal(ninc, 0.d0, zr(ivect), 1)
! ----------------------------------------------------------------------
! --- RECUPERATION DE LA FREQUENCE PROPRE DU MODE LINEAIRE
! ----------------------------------------------------------------------
    if (reprise) then
        call rsadpa(modini, 'L', 1, 'FREQ', 2,&
                    0, sjv=ifreq, styp=k16bid)
    else
        call rsadpa(lnm, 'L', 1, 'FREQ', num_ordr,&
                    0, sjv=ifreq, styp=k16bid)
    endif
    omega = r8depi()*zr(ifreq)/zr(iadim-1+3)
! ----------------------------------------------------------------------
! --- RECUPERATION DU MODE D'INITIALISATION
! ----------------------------------------------------------------------
    clnm='&&MNLALI.RECUP     '
    if (reprise) then
        call vprecu(modini, 'DEPL', -1, [0], clnm,&
                    0, '', '', '', '',&
                    neqv, nbmode, typmod, nbpari, nbparr,&
                    nbpark)
    else
        call vprecu(lnm, 'DEPL', -1, [0], clnm,&
                    0, '', '', '', '',&
                    neqv, nbmode, typmod, nbpari, nbparr,&
                    nbpark)
    endif
    call jeveuo(clnm, 'L', ilnm)
!
! ----------------------------------------------------------------------
! --- COPIE DU MODE PROPRE DANS LE VECTEUR D'INITIALISATION
! ----------------------------------------------------------------------
    if (reprise) then
        ht=(nbmode-1)/2
        do 10 j = 1, nbmode
            i=0
            do 11 k = 1, neq
                if (zi(icdl-1+k) .eq. 0) then
                    i=i+1
                    if (h .gt. ht .and. j .gt. ht+1) then
                        zr(ivect-1+(h-ht+j-1)*nd+i)=zr(ilnm-1+(j-1)*&
                        neq+k)
                        else if(h.lt.ht.and.j.gt.(h+1).and.j.le.(2*h+1))&
                    then
                        zr(ivect-1+(j-1)*nd+i)=zr(ilnm-1+(ht-h+j-1)*&
                        neq+k)
                    else
                        zr(ivect-1+(j-1)*nd+i)=zr(ilnm-1+(j-1)*neq+k)
                    endif
                endif
 11         continue
 10     continue
    else
        i=0
        do 31 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                zr(ivect-1+nd+i)=zr(ilnm-1+k+(num_ordr-1)*neq)
            endif
 31     continue
    endif
! ----------------------------------------------------------------------
! --- ADIMENSIONNEMENT
! ----------------------------------------------------------------------
    if (reprise) then
        call dscal(nd*(2*h+1), 1.d0/zr(ijmax), zr(ivect), 1)
    else
! --- MISE A L'ECHELLE PAR L'AMPLITUDE DE DEPART
!        iamax=idamax(nd,zr(ivect+nd),1)
!        ampref=zr(ivect-1+nd+iamax)
        ampref=1.d0
        call dscal(nd, ampl/ampref, zr(ivect+nd), 1)
    endif
! ----------------------------------------------------------------------
! --- REMPLISSAGE DES FORCES DE CHOCS ET DES VECTEURS AUXILIAIRES
! ----------------------------------------------------------------------
    xdep1='&&MNLALI.DEP1'
    xdep2='&&MNLALI.DEP2'
    xtemp='&&MNLALI.TEMP'
    call wkvect(xdep1, 'V V R', 2*h+1, idep1)
    call wkvect(xdep2, 'V V R', 2*h+1, idep2)
    call wkvect(xtemp, 'V V R', ninc, itemp)
    nt = int(2**int(dlog(2.d0*dble(hf)+1.d0)/dlog(2.d0)+1.d0))
    neqs=0
    do 20 i = 1, nchoc
! ---   ON RECUPERE LES PARAMETRES DE CHOCS
        alpha=raid(i)/zr(iadim)
        eta=reg(i)
        jeu=vjeu(i)/zr(ijmax)
        if (type(i)(1:7) .eq. 'BI_PLAN') then
            nddl=vnddl(6*(i-1)+1)
            call dscal(2*h+1, 0.d0, zr(idep1), 1)
            call daxpy(2*h+1, 1.d0/jeu, zr(ivect-1+nddl), nd, zr(idep1),&
                       1)
            call mnlbil(zr(idep1), omega, alpha, eta, h,&
                        hf, nt, zr(ivect+ nd*(2*h+1)+neqs*(2*hf+1)))
        else if (type(i)(1:6).eq.'CERCLE') then
            nddlx=vnddl(6*(i-1)+1)
            nddly=vnddl(6*(i-1)+2)
            call dscal(2*h+1, 0.d0, zr(idep1), 1)
            call dscal(2*h+1, 0.d0, zr(idep2), 1)
            call dcopy(2*h+1, zr(ivect-1+nddlx), nd, zr(idep1), 1)
            zr(idep1)=zr(idep1)-orig(3*(i-1)+1)
            call dscal(2*h+1, 1.d0/jeu, zr(idep1), 1)
            call dcopy(2*h+1, zr(ivect-1+nddly), nd, zr(idep2), 1)
            zr(idep2)=zr(idep2)-orig(3*(i-1)+2)
            call dscal(2*h+1, 1.d0/jeu, zr(idep2), 1)
            call mnlcir(xdep1, xdep2, omega, alpha, eta,&
                        h, hf, nt, xtemp)
            call dcopy(4*(2*hf+1), zr(itemp), 1, zr(ivect+nd*(2*h+1)+ neqs*(2*hf+1)), 1)
        else if (type(i)(1:4).eq.'PLAN') then
            nddl=vnddl(6*(i-1)+1)
            call dscal(2*h+1, 0.d0, zr(idep1), 1)
            call daxpy(2*h+1, 1.d0/jeu, zr(ivect-1+nddl), nd, zr(idep1),&
                       1)
            call mnluil(zr(idep1), omega, alpha, eta, h,&
                        hf, nt, zr(ivect+ nd*(2*h+1)+neqs*(2*hf+1)))
        endif
        neqs=neqs+vneqs(i)
 20 continue
!
! ----------------------------------------------------------------------
! --- REMPLISSAGE DES PARAMETRES
! ----------------------------------------------------------------------
! --- ON REMPLI LA PARTIE DU VECTEUR CORRESPONDANT A GAMMA1=LAMBDA*OMEGA
    zr(ivect+ninc-4) = lambda*omega
! --- ON REMPLI LA PARTIE DU VECTEUR CORRESPONDANT A GAMMA2=OMEGA*OMEGA
    zr(ivect+ninc-3) = omega*omega
! --- ON REMPLI LA PARTIE DU VECTEUR CORRESPONDANT A LAMBDA
    zr(ivect+ninc-2) = lambda
! --- ON REMPLI LA PARTIE DU VECTEUR CORRESPONDANT A OMEGA
    zr(ivect+ninc-1) = omega
! ----------------------------------------------------------------------
! --- DESTRUCTION DE LA SD_RESULTAT CONTENANT LE(S) MODE(S) LINEAIRE(S)
! ----------------------------------------------------------------------
!
!
!    call jedetr(clnm)
    call jedetr(xdep1)
    call jedetr(xdep2)
    call jedetr(xtemp)
    call jedema()
!
end subroutine
