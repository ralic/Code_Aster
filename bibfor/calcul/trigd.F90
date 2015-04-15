subroutine trigd(dg1, deb1, dg2, deb2, cumul,&
                 ino, nno)
use module_calcul, only : ca_iachin_, ca_iachlo_, ca_ianueq_, ca_ilchlo_,&
     ca_itypgd_, ca_lprno_, ca_ncmpmx_, ca_nec_
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/exisdg.h"
    integer :: dg1(*), dg2(*), deb1, deb2, ino, nno
    aster_logical :: cumul
! ----------------------------------------------------------------------
!     ENTREES:
!       DG1    : DG DE LA GRANDEUR DE CHIN
!       DEB1   : ADRESSE DU DEBUT DE LA GRANDEUR DANS CHIN.VALE
!       DG2    : DG DE LA GRANDEUR DE CHLOC
!       DEB2   : ADRESSE DU DEBUT DE LA GRANDEUR DANS CHLOC.VALE
!       CUMUL  : / .F. : ON AFFECTE DANS DEB2 LA GRANDEUR LUE
!                / .T. : ON CUMULE DANS DEB2 LA GRANDEUR LUE
!                CUMUL= .T. => ON VEUT FAIRE "MOYENNE" CHNO -> ELEM
!       INO    : NUMERO DU NOEUD ASSOCIE A DEB1
!                (INO N'EST UTILISE QUE SI CUMUL)
!       NNO    : NOMBRE DE NEOUDS DE LA MAILLE
!                (NNO N'EST UTILISE QUE SI CUMUL)
!
!     SORTIES:
!       RECOPIE DE CHIN.VALE DANS CHLOC POUR LES CMPS VOULUS.
!       DEB2   : EST MIS A JOUR POUR LE NOEUD SUIVANT (SI EXCHNO).
!                ET POUR L'ELEMENT SUIVANT SI EXCART OU EXCHNO.
! ----------------------------------------------------------------------
!

!
!    -------------------------------------------------------------------
    aster_logical :: change
    integer :: cmp, ind1, nec2, nsav, ksav
    parameter(nec2=30)
    parameter(nsav=5)
    integer :: ind2(nsav), necold(nsav)
    integer :: dg1old(nec2, nsav), dg2old(nec2, nsav), poscmp(nec2*30, nsav)
    integer :: ieq, i, k
    save dg1old,dg2old,poscmp,ind2,necold
    data necold/nsav*0/
!
!----------------------------------------------------------------------
!     1. ON REGARDE SI ON NE TROUVE PAS UN POSCMP(*,KSAV) QUI CONVIENT.
!        (CALCUL DE KSAV)
!     -----------------------------------------------------------------
    do 20 k = 1, nsav
        if (ca_nec_ .ne. necold(k)) goto 20
        change=.false.
        do 10 i = 1, ca_nec_
            if (dg1(i) .ne. dg1old(i,k)) change=.true.
            if (dg2(i) .ne. dg2old(i,k)) change=.true.
 10     continue
        if (change) goto 20
!       -- ON A TROUVE UN KSAV CONVENABLE :
        ksav=k
        goto 80
!
 20 continue
!
!
!----------------------------------------------------------------
!     -- ON PLACE LE "ASSERT" ICI POUR LE FAIRE MOINS SOUVENT
    ASSERT(ca_nec_.le.nec2)
!
!     2.1 ON DECALE LES TABLEAUX VERS LE "FOND" :
!         ET ON AJOUTE LES NOUVELLES VALEURS EN KSAV=1
!     ------------------------------------------------
    do 50 k = nsav-1, 1, -1
        do 30 i = 1, necold(k)
            dg1old(i,k+1)=dg1old(i,k)
            dg2old(i,k+1)=dg2old(i,k)
 30     continue
        do 40 i = 1, ind2(k)
            poscmp(i,k+1)=poscmp(i,k)
 40     continue
        ind2(k+1)=ind2(k)
        necold(k+1)=necold(k)
 50 continue
!
    ksav=1
    necold(ksav)=ca_nec_
    do 60 i = 1, ca_nec_
        dg1old(i,ksav)=dg1(i)
        dg2old(i,ksav)=dg2(i)
 60 continue
!
!
!     2.2 REMPLISSAGE DE POSCMP(KSAV):
!     -------------------------------
    ind1=0
    ind2(ksav)=0
    do 70 cmp = 1, ca_ncmpmx_
        if (exisdg(dg1,cmp)) ind1=ind1+1
        if (exisdg(dg2,cmp)) then
            ind2(ksav)=ind2(ksav)+1
            if (exisdg(dg1,cmp)) then
                poscmp(ind2(ksav),ksav)=ind1
            else
                poscmp(ind2(ksav),ksav)=0
            endif
        endif
 70 continue
!
!
!
!----------------------------------------------------------------
 80 continue
!     3. RECOPIE DES VALEURS DANS LE CHAMP_LOCAL :
!     --------------------------------------------
    do 90 cmp = 1, ind2(ksav)
        if (poscmp(cmp,ksav) .gt. 0) then
            ieq=deb1-1+poscmp(cmp,ksav)
            if (ca_lprno_ .eq. 1) ieq=zi(ca_ianueq_-1+ieq)
!
            if (.not.cumul) then
                if (ca_itypgd_ .eq. 1) then
                    zr(ca_iachlo_-1+deb2-1+cmp)=zr(ca_iachin_-1+ieq)
                else if (ca_itypgd_.eq.2) then
                    zc(ca_iachlo_-1+deb2-1+cmp)=zc(ca_iachin_-1+ieq)
                else if (ca_itypgd_.eq.3) then
                    zi(ca_iachlo_-1+deb2-1+cmp)=zi(ca_iachin_-1+ieq)
                else if (ca_itypgd_.eq.4) then
                    zk8(ca_iachlo_-1+deb2-1+cmp)=zk8(ca_iachin_-1+ieq)
                else if (ca_itypgd_.eq.5) then
                    zk16(ca_iachlo_-1+deb2-1+cmp)=zk16(ca_iachin_-1+ieq)
                else if (ca_itypgd_.eq.6) then
                    zk24(ca_iachlo_-1+deb2-1+cmp)=zk24(ca_iachin_-1+ieq)
                else
                    ASSERT(.false.)
                endif
!
            else
                if (ca_itypgd_ .eq. 1) then
                    zr(ca_iachlo_-1+deb2-1+cmp)=zr(ca_iachlo_-1+deb2-1+cmp)+&
                    zr(ca_iachin_-1+ieq)
                else if (ca_itypgd_.eq.2) then
                    zc(ca_iachlo_-1+deb2-1+cmp)=zc(ca_iachlo_-1+deb2-1+cmp)+&
                    zc(ca_iachin_-1+ieq)
                else
                    ASSERT(.false.)
                endif
            endif
!
!
            if (cumul) then
                if (ino .eq. 1) then
                    zl(ca_ilchlo_-1+deb2-1+cmp)=.true.
                else
                endif
            else
                zl(ca_ilchlo_-1+deb2-1+cmp)=.true.
            endif
!
        else
            zl(ca_ilchlo_-1+deb2-1+cmp)=.false.
        endif
 90 continue
!
!
!----------------------------------------------------------------
    if (cumul) then
        if (ino .eq. nno) deb2=deb2+ind2(ksav)
    else
        deb2=deb2+ind2(ksav)
    endif
end subroutine
