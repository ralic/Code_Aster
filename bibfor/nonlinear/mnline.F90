subroutine mnline(imat, xcdl, parcho, adime, xvect,&
                  ninc, nd, nchoc, h, hf,&
                  xline)
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
! ----------------------------------------------------------------------
!
!     MODE_NON_LINE PARTIE LINEAIRE
!     -    -               ---
! ----------------------------------------------------------------------
!
! REGROUPE LES TERMES LINEAIRES DU PROBLEME A RESOUDRE
! ----------------------------------------------------------------------
! IN  IMAT   : I(2) : DESCRIPTEUR DES MATRICES :
!                        - IMAT(1) => MATRICE DE RAIDEUR
!                        - IMAT(2) => MATRICE DE MASSE
! IN  XCDL   : K14  : INDICE DES CONDITIONS AUX LIMITES
! IN  PARCHO : K14  : NOM DE LA SD PARAMETRE DES CONTACTEURS
! IN  ADIME  : K14  : SD PARAMETRE POUR ADIMENSIONNEMENT
! IN  XVECT  : K14  : NOM DU VECTEUR SOLUTION
! IN  NINC   : I    : NOMBRE D INCONNUES DU SYSTEME
! IN  ND     : I    : NOMBRE DE DEGRES DE LIBERTE
! IN  NCHOC  : I    : NOMBRE DE CONTACTEURS
! IN  H      : I    : NOMBRE D'HARMONIQUES de X
! IN  HF     : I    : NOMBRE D'HARMONIQUES POUR F
! OUT XLINE  : I    : NOM DU VECTEUR DES TERMES LINEAIRES
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/wkvect.h"
    integer :: imat(2), ninc, nd, nchoc, h, hf
    character(len=14) :: xcdl, parcho, adime, xvect, xline
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    real(kind=8) :: alpha, eta, jeu
    integer :: neq, ivect, icdl, iline, ivect1, ivect2, j, i, k
    integer ::     nddl, iadim
    integer ::  neqs, ncmp,   nddlx, nddly
    real(kind=8), pointer :: orig(:) => null()
    real(kind=8), pointer :: reg(:) => null()
    real(kind=8), pointer :: raid(:) => null()
    real(kind=8), pointer :: vjeu(:) => null()
    real(kind=8), pointer :: jeumax(:) => null()
    integer, pointer :: vnddl(:) => null()
    character(len=8), pointer :: type(:) => null()
    integer, pointer :: vneqs(:) => null()
    integer, pointer :: vncmp(:) => null()
!
    call jemarq()
! ----------------------------------------------------------------------
! --- RECUPERATION DU NOM DE LA MATRICE ET TAILLE DE LA MATRICE
! ----------------------------------------------------------------------
    neq = zi(imat(1)+2)
! ----------------------------------------------------------------------
! --- RECUPERATION POINTEUR DE XVECT, L(XVECT) ET XCDL
! ----------------------------------------------------------------------
    call jeveuo(xvect, 'L', ivect)
    call jeveuo(xcdl, 'L', icdl)
    call jeveuo(adime, 'L', iadim)
    call jeveuo(xline, 'E', iline)
    call jeveuo(parcho//'.RAID', 'L', vr=raid)
    call jeveuo(parcho//'.REG', 'L', vr=reg)
    call jeveuo(parcho//'.NDDL', 'L', vi=vnddl)
    call jeveuo(parcho//'.JEU', 'L', vr=vjeu)
    call jeveuo(parcho//'.JEUMAX', 'L', vr=jeumax)
    call jeveuo(parcho//'.NCMP', 'L', vi=vncmp)
    call jeveuo(parcho//'.NEQS', 'L', vi=vneqs)
    call jeveuo(parcho//'.TYPE', 'L', vk8=type)
    call jeveuo(parcho//'.ORIG', 'L', vr=orig)
    call dscal(ninc-1, 0.d0, zr(iline), 1)
! ----------------------------------------------------------------------
! --- CREATION D'UN VECTEUR TEMPORAIRE
! ----------------------------------------------------------------------
    call wkvect('&&MNLINE.VECT1', 'V V R', neq*(2*h+1), ivect1)
    call wkvect('&&MNLINE.VECT2', 'V V R', neq*(2*h+1), ivect2)
! --- COPIE DE XVECT DANS UN VECTEUR AVEC DDLS NON-ACTIFS
    do 10 j = 1, 2*h+1
        i=0
        do 11 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                zr(ivect1-1+(j-1)*neq+k)=zr(ivect-1+(j-1)*nd+i)
                if (abs(zr(ivect-1+(j-1)*nd+i)-1.d0) .lt. 1.d-16) then
                endif
            endif
11      continue
10  continue
! ----------------------------------------------------------------------
! --- EQUATION DE LA DYNAMIQUE (TRAITEMENT DU DEPLACEMENT)
! --- XLINE(1:ND*(2*H+1))=K*XVECT(1:ND*(2*H+1))
! ----------------------------------------------------------------------
    call mrmult('ZERO', imat(1), zr(ivect1), zr(ivect2), 2*h+1,&
                .false._1)
! --- COPIE DE XVECT1 DANS XVECT EN SUPPRIMANT LES DDLS NON ACTIFS
    do 20 j = 1, 2*h+1
        i=0
        do 21 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                zr(iline-1+(j-1)*nd+i)=zr(ivect2-1+(j-1)*neq+k)/zr(&
                iadim)
            endif
21      continue
20  continue
! ----------------------------------------------------------------------
! --- EQUATION DE LA DYNAMIQUE (TRAITEMENT DE LA FORCE NON-LINEAIRE)
! --- XLINE(1:ND*(2*H+1))=XLINE(1:ND*(2*H+1))+F
! ----------------------------------------------------------------------
    neqs=0
    do 100 i = 1, nchoc
        eta=reg(i)
        jeu=vjeu(i)/jeumax(1)
        ncmp=vncmp(i)
        do 101 j = 1, ncmp
            nddl=vnddl(6*(i-1)+j)
! ---       CSTE & COS
            call daxpy(h+1, jeu, zr(ivect+nd*(2*h+1)+(neqs+j-1)*(2* hf+1)), 1,&
                           zr(iline-1+nddl), nd)
! ---       SIN
            call daxpy(h, jeu, zr(ivect+nd*(2*h+1)+(neqs+j-1)*(2* hf+1)+hf+1), 1,&
                           zr(iline-1+nd*(h+1)+nddl), nd)
101      continue
        neqs=neqs+vneqs(i)
100  continue
! ----------------------------------------------------------------------
! --- EQUATION SUPPLEMENTAIRE POUR DEFINIR LA FORCE NON-LINEAIRE
! --- XLINE(ND*(2*H+1)+1:ND*(2*H+1)+2*NCHOC*(2*HF+1))
! ----------------------------------------------------------------------
    neqs=0
    do 110 i = 1, nchoc
        alpha=raid(i)/zr(iadim-1+1)
        eta=reg(i)
        jeu=vjeu(i)/jeumax(1)
        if (type(i)(1:7) .eq. 'BI_PLAN') then
            nddl=vnddl(6*(i-1)+1)
! ---     F -ETA*XG
! ---       -ETA*XG (CSTE & COS)
            call daxpy(h+1, -eta/jeu, zr(ivect-1+nddl), nd,&
                       zr(iline-1+ nd*(2*h+1)+neqs*(2*hf+1)+1), 1)
! ---       -ETA*XG (SIN)
            call daxpy(h, -eta/jeu, zr(ivect-1+nd*(h+1)+nddl), nd,&
                       zr(iline-1+nd*(2*h+1)+neqs*(2*hf+1)+hf+2), 1)
! ---      + F
            call daxpy(2*hf+1, 1.d0, zr(ivect-1+nd*(2*h+1)+neqs*(2*hf+1) +1), 1,&
                       zr(iline-1+nd*(2*h+1)+neqs*(2*hf+1)+1), 1)
! ---     Z
            call daxpy(2*hf+1, 1.d0, zr(ivect-1+nd*(2*h+1)+(neqs+1)*(2* hf+1)+1), 1,&
                       zr(iline-1+nd*(2*h+1)+(neqs+1)*(2*hf+1)+1), 1)
        else if (type(i)(1:6).eq.'CERCLE') then
            nddlx=vnddl(6*(i-1)+1)
            nddly=vnddl(6*(i-1)+2)
! ---     + ORIG1*[FN]
            call daxpy(2*hf+1, orig(1+3*(i-1))/jeu, zr(ivect+nd*(2*h+ 1)+(neqs+3)*(2*hf+1)), 1,&
                       zr(iline-1+nd*(2*h+1)+neqs*(2*hf+ 1)+1), 1)
! ---     + ORIG2*[FN]
            call daxpy(2*hf+1, orig(1+3*(i-1)+1)/jeu, zr(ivect+nd*(2* h+1)+(neqs+3)*(2*hf+1)),&
                       1, zr(iline-1+nd*(2*h+1)+(neqs+1)* (2*hf+1)+1), 1)
! ---     + 2*ORIG1*UX + 2*ORIG2*UY (CSTE & COS)
            call daxpy(h+1, 2*orig(1+3*(i-1))/jeu**2, zr(ivect-1+ nddlx), nd,&
                       zr(iline-1+nd*(2*h+1)+(neqs+2)*(2*hf+1)+1), 1)
            call daxpy(h+1, 2*orig(1+3*(i-1)+1)/jeu**2, zr(ivect-1+ nddly), nd,&
                       zr(iline-1+nd*(2*h+1)+(neqs+2)*(2*hf+1)+1), 1)
! ---     + 2*ORIG1*UX + 2*ORIG2*UY (SIN)
            call daxpy(h, 2*orig(1+3*(i-1))/jeu**2, zr(ivect-1+nd*(h+ 1)+nddlx), nd,&
                       zr(iline-1+nd*(2*h+1)+(neqs+2)*(2*hf+1)+hf+ 2), 1)
            call daxpy(h, 2*orig(1+3*(i-1)+1)/jeu**2, zr(ivect-1+nd*( h+1)+nddly), nd,&
                       zr(iline-1+nd*(2*h+1)+(neqs+2)*(2*hf+1)+ hf+2), 1)
! ---     FN
            call dcopy(2*hf+1, zr(ivect+nd*(2*h+1)+(neqs+3)*(2*hf+1)), 1,&
                       zr(iline+nd*(2*h+1)+(neqs+3)*(2*hf+1)), 1)
        else if (type(i)(1:4).eq.'PLAN') then
! ---     F
            call dcopy(2*hf+1, zr(ivect+nd*(2*h+1)+neqs*(2*hf+1)), 1,&
                       zr(iline+nd*(2*h+1)+neqs*(2*hf+1)), 1)
        endif
        neqs=neqs+vneqs(i)
110  continue
! ----------------------------------------------------------------------
! --- AUTRES EQUATIONS
! ----------------------------------------------------------------------
! --- GAMMA1
    zr(iline-1+ninc-3) = zr(ivect-1+ninc-3)
! --- GAMMA2
    zr(iline-1+ninc-2) = zr(ivect-1+ninc-2)
! --- EQUATION DE PHASE
    zr(iline-1+ninc-1) = 0.d0
! ----------------------------------------------------------------------
! --- DESTRUCTION DES VECTEURS TEMPORAIRES
! ----------------------------------------------------------------------
    call jedetr('&&MNLINE.VECT1')
    call jedetr('&&MNLINE.VECT2')
!
    call jedema()
!
end subroutine
