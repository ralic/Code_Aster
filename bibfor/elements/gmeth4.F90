subroutine gmeth4(nnoff, ndimte, fond, gthi, milieu,&
                  pair, gs, objcur, gi, gxfem)
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
!
! ......................................................................
!      METHODE LAGRANGE_REGU POUR LE CALCUL DE G(S)
!
! ENTREE
!
!   NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!   NDIMTE   --> NOMBRE de CHAMPS THETA CHOISIS
!   FOND     --> NOMS DES NOEUDS DU FOND DE FISSURE
!   GTHI     --> VALEURS DE G POUR LES CHAMPS THETAI
!   OBJCUR   --> ABSCISSES CURVILIGNES S
!
!
! SORTIE
!
!   GS      --> VALEUR DE G(S)
!   GI      --> VALEUR DE GI
! ......................................................................
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/gsyste.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: i, i1, iabsc, iadrno, imatr
    integer :: kk, ndimte, nn, nnoff, nump
    real(kind=8) :: gthi(1), gs(1), gi(1), s1, s2, s3, delta
    character(len=24) :: matr, fond, objcur
    aster_logical :: connex, milieu, pair, gxfem
!
!
! OBJET DECRIVANT LE MAILLAGE
!
    call jemarq()
!
    connex = .false.
    if (.not. gxfem) then
        call jeveuo(fond, 'L', iadrno)
        if (zk8(iadrno+1-1) .eq. zk8(iadrno+nnoff-1)) connex = .true.
    endif
!
!     ABSCISSES CURVILIGNES DES NOEUDS DU FOND DE FISSURE
    call jeveuo(objcur, 'L', iabsc)
!
! CONSTRUCTION DE LA MATRICE (NDIMTE x NDIMTE)
    matr = '&&METHO4.MATRI'
    call wkvect(matr, 'V V R8', ndimte*ndimte, imatr)
!
    i1 = 2
    if (milieu) then
        i1 = 4
    endif
    do 120 i = 1, ndimte-2
        nump = 2*i-1
        if (milieu) nump = 4*i-3
        s1 = zr(iabsc-1+nump)
        s2 = zr(iabsc-1+nump+i1)
        delta = (s2-s1)/6.d0
        kk = imatr+(i-1 )*ndimte+i-1
        zr(kk )= zr(kk) + 2.d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1 )= 1.d0*delta
        zr(imatr+(i-1 )*ndimte+i-1+1)= 1.d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1+1)= 2.d0*delta
120 end do
!
    i = ndimte -1
    nump = 2*(i-1)
    if (pair) then
        s1 = zr(iabsc-1+nump)
        s2 = zr(iabsc-1+nump+i1/2)
        delta = (s2-s1)/6.d0
        kk = imatr+(i-1 )*ndimte+i-1
        zr(kk )= zr(kk) + 3.5d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1 )= 1.d0*delta
        zr(imatr+(i-1 )*ndimte+i-1+1)= 1.d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1+1)= 0.5d0*delta
    else
        s1 = zr(iabsc-1+nump+1)
        s2 = zr(iabsc-1+nump+i1+1)
        delta = (s2-s1)/6.d0
        kk = imatr+(i-1 )*ndimte+i-1
        zr(kk )= zr(kk) + 2.d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1 )= 1.d0*delta
        zr(imatr+(i-1 )*ndimte+i-1+1)= 1.d0*delta
        zr(imatr+(i-1+1)*ndimte+i-1+1)= 2.d0*delta
    endif
!
    if (nnoff .eq. 2) then
        s1 = zr(iabsc-1+1)
        s2 = zr(iabsc-1+2)
        delta = (s2-s1)/6.d0
        zr(imatr + 0)= 3.5d0*delta
        zr(imatr + 1)= 1.d0*delta
        zr(imatr + 2)= 1.d0*delta
        zr(imatr + 3)= 0.5d0*delta
    endif
!
    if (connex) then
        zr(imatr) = 2.d0*zr(imatr)
        s1 = zr(iabsc-1+nump-i1+1)
        s2 = zr(iabsc-1+nump+1)
        delta = (s2-s1)/6.d0
        zr(imatr+(1-1)*ndimte+ndimte-1-1)= 1.d0*delta
        kk = imatr+(ndimte-1)*ndimte+ndimte-1
        zr(kk) = 2.d0*zr(kk)
        s1 = zr(iabsc-1+1)
        s2 = zr(iabsc-1+i1+1)
        delta = (s2-s1)/6.d0
        zr(imatr+(ndimte-1)*ndimte+2-1)= 1.d0*delta
    endif
!
!  SYSTEME LINEAIRE:  MATR*GI = GTHI
!
    call gsyste(matr, ndimte, ndimte, gthi, gi)
!
    if (nnoff .eq. 2) then
        gs(1) = gi(1)
        gs(nnoff) = gi(ndimte)
    else
        do 200 i = 1, ndimte-1
            if (milieu) then
                nn = 4*i-3
                gs(nn) = gi(i)
                s1 = zr(iabsc-1+nn)
                s3 = zr(iabsc-1+nn+4)
                gs(nn+1)=gi(i)+(zr(iabsc-1+nn+1)-s1)*(gi(i+1)-gi(i))/(&
                s3-s1)
                gs(nn+2)=gi(i)+(zr(iabsc-1+nn+2)-s1)*(gi(i+1)-gi(i))/(&
                s3-s1)
                gs(nn+3)=gi(i)+(zr(iabsc-1+nn+3)-s1)*(gi(i+1)-gi(i))/(&
                s3-s1)
            else
                nn = 2*i-1
                gs(nn) = gi(i)
                s1 = zr(iabsc-1+nn)
                s2 = zr(iabsc-1+nn+1)
                s3 = zr(iabsc-1+nn+2)
                gs(nn+1) = gi(i)+(s2-s1)*(gi(i+1)-gi(i))/(s3-s1)
            endif
200     continue
        gs(nnoff) = gi(ndimte)
!
!     SI PAIR, ON CORRIGE LA VALEUR DE G AU DERNIER NOEUD
        if (pair) then
            nn=2*(ndimte-2)
            s1 = zr(iabsc-1+nn)
            s2 = zr(iabsc-1+nn+1)
            s3 = zr(iabsc-1+nn+2)
            gs(nnoff) = gs(nnoff-1)+ (s3-s2)*(gs(nnoff-2)-gs(nnoff-1)) /(s1-s2)
        endif
    endif
!
    call jedetr(matr)
!
    call jedema()
end subroutine
