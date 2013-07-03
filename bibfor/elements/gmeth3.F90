subroutine gmeth3(nnoff, fond, gthi, milieu, gs,&
                  objcur, gi, num, gxfem)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
#include "jeveux.h"
#include "asterc/getvtx.h"
#include "asterfort/gsyste.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: nnoff, num
    real(kind=8) :: gthi(1), gs(1), gi(1)
    character(len=24) :: fond, objcur
    logical :: milieu, gxfem
!
! ......................................................................
!      METHODE THETA-LAGRANGE ET G-LAGRANGE POUR LE CALCUL DE G(S)
!
! ENTREE
!
!     NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!     FOND     --> NOMS DES NOEUDS DU FOND DE FISSURE
!     GTHI     --> VALEURS DE G POUR LES CHAMPS THETAI
!     MILIEU   --> .TRUE.  : ELEMENT QUADRATIQUE
!                  .FALSE. : ELEMENT LINEAIRE
!     OBJCUR  --> ABSCISSES CURVILIGNES S
!
!  SORTIE
!
!      GS      --> VALEUR DE G(S)
!      GI      --> VALEUR DE GI
!      NUM     --> 3 (LAGRANGE-LAGRANGE)
!              --> 4 (NOEUD-NOEUD)
!
!
!
!
    integer :: i, kk, iadrno, iabsc
    integer :: imatr, ivect, ibid
!
    real(kind=8) :: delta, s1, s2, s3
!
    character(len=24) :: vect, matr, lissg
!
    logical :: connex
    integer :: iarg
!
! OBJET DECRIVANT LE MAILLAGE
!
! SI LE FOND DE FISSURE EST FERME (DERNIER NOEUD = PREMIER NOEUD)
! CONNEX = TRUE
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
    call getvtx('LISSAGE', 'LISSAGE_G', 1, iarg, 1,&
                lissg, ibid)
!
    if (lissg .eq. 'LAGRANGE_NO_NO') then
        vect = '&&METHO3.VECT'
        call wkvect(vect, 'V V R8', nnoff, ivect)
        num = 4
!
        if (milieu) then
            do 10 i = 1, nnoff-2, 2
                s1 = zr(iabsc-1+i)
                s3 = zr(iabsc-1+i+2)
                delta = (s3-s1)/6.d0
                zr(ivect+i -1)= zr(ivect+i-1) + delta
                zr(ivect+i+1-1)= 4.d0*delta
                zr(ivect+i+2-1)= delta
10          continue
            if (connex) then
                zr(ivect+nnoff-1)= zr(ivect+nnoff-1) + zr(ivect+1-1)
                zr(ivect+1 -1)= zr(ivect+nnoff-1)
            endif
        else
            do 20 i = 1, nnoff-1
                s1 = zr(iabsc-1+i)
                s2 = zr(iabsc-1+i+1)
                delta = (s2-s1)/3.d0
                zr(ivect+i -1)= zr(ivect+i-1) + delta
                zr(ivect+i+1-1)= 2.d0*delta
20          continue
            if (connex) then
                zr(ivect+nnoff-1)= zr(ivect+nnoff-1) + zr(ivect+1-1)
                zr(ivect+1 -1)= zr(ivect+nnoff-1)
            endif
        endif
        do 30 i = 1, nnoff
            gi(i) = gthi(i)/zr(ivect+i-1 )
30      continue
!
    else if (lissg.eq.'LAGRANGE') then
        matr = '&&METHO3.MATRI'
        call wkvect(matr, 'V V R8', nnoff*nnoff, imatr)
        num = 3
!
        if (milieu) then
            do 100 i = 1, nnoff-2, 2
                s1 = zr(iabsc-1+i)
                s3 = zr(iabsc-1+i+2)
                delta = (s3-s1)/30.d0
!
                kk = imatr+(i-1 )*nnoff+i-1
                zr(kk )= zr(kk) + 4.d0*delta
                zr(imatr+(i-1+1)*nnoff+i-1 )= 2.d0*delta
                zr(imatr+(i-1+2)*nnoff+i-1 )= -1.d0*delta
!
                zr(imatr+(i-1 )*nnoff+i-1+1)= 2.d0*delta
                zr(imatr+(i-1+1)*nnoff+i-1+1)= 16.d0*delta
                zr(imatr+(i-1+2)*nnoff+i-1+1)= 2.d0*delta
!
                zr(imatr+(i-1 )*nnoff+i-1+2)= -1.d0*delta
                zr(imatr+(i-1+1)*nnoff+i-1+2)= 2.d0*delta
                zr(imatr+(i-1+2)*nnoff+i-1+2)= 4.d0*delta
100          continue
            if (connex) then
                kk = imatr+(1-1 )*nnoff+1-1
                zr(kk )= zr(kk) + 5.d0*delta
                s1 = zr(iabsc-1+1)
                s3 = zr(iabsc-1+3)
                delta = (s3-s1)/30.d0
                kk = imatr+(nnoff-1)*nnoff+nnoff-1
                zr(kk )= zr(kk) + 5.d0*delta
            endif
        else
            do 120 i = 1, nnoff-1
                s1 = zr(iabsc-1+i)
                s2 = zr(iabsc-1+i+1)
                delta = (s2-s1)/6.d0
!
                kk = imatr+(i-1 )*nnoff+i-1
                zr(kk )= zr(kk) + 2.d0*delta
                zr(imatr+(i-1+1)*nnoff+i-1 )= 1.d0*delta
!
                zr(imatr+(i-1 )*nnoff+i-1+1)= 1.d0*delta
                zr(imatr+(i-1+1)*nnoff+i-1+1)= 2.d0*delta
120          continue
            if (connex) then
                kk = imatr+(1-1 )*nnoff+1-1
                zr(kk )= zr(kk) + 3.d0*delta
                s1 = zr(iabsc-1+1)
                s3 = zr(iabsc-1+3)
                delta = (s3-s1)/6.d0
                kk = imatr+(nnoff-1)*nnoff+nnoff-1
                zr(kk )= zr(kk) + 3.d0*delta
            endif
        endif
!
!  SYSTEME LINEAIRE:  MATR*GI = GTHI
!
        call gsyste(matr, nnoff, nnoff, gthi, gi)
    endif
!
    do 200 i = 1, nnoff
        gs(i) = gi(i)
200  end do
!
    call jedetr('&&METHO3.MATRI')
    call jedetr('&&METHO3.VECT')
!
end subroutine
