subroutine ornofd(mafour, nomail, nbma, noeord, ndorig,&
                  ndextr, base, vecori)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/i2extf.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
!
    integer :: nbma
    character(len=24) :: mafour
    character(len=8) :: nomail, ndorig, ndextr
    character(len=24) :: noeord
    character(len=1) :: base
    real(kind=8) :: vecori(3)
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
!-----------------------------------------------------------------------
! FONCTION REALISEE:
!
!       ORNOFD -- ORDONNANCEMENT D'UNE LISTE DE NOEUD
!                 A PARTIR D'UN NOEUD ORIGINE
!                 UTILISE DANS DEFI_GROUP ET DEFI_FOND_FISS
!
!     ENTREES:
!        MAFOUR     : LISTE DES MAILLES SEG
!        NOMAIL     : NOM DU MAILLAGE
!        NBMA       : NOMBRE DE MAILLES TRAITEES
!        NOEORD     : NOM DE L'OBJET
!        NDORIG     : NOM DU NOEUD ORIGINE
!        NDEXTR     : NOM DU NOEUD EXTREMITE
!        BASE       : TYPE DE BASE DE SAUVEGARDE
!
!-----------------------------------------------------------------------
!
    real(kind=8) :: vecta(3), ps1, ps2
!
    integer :: iatyma, jtypm,   jmail
    integer :: im, nid, nig, njonc, n, i, k, nbno
    integer :: jrdm, jnoe, ntemp
    character(len=8) :: typm
    character(len=8) :: noeud
    character(len=24) :: conec, typp, nomnoe
    integer, pointer :: mailles_triee(:) => null()
    integer, pointer :: noeuds_extrem(:) => null()
    real(kind=8), pointer :: vale(:) => null()
! DEB-------------------------------------------------------------------
    call jemarq()
!
    conec = nomail//'.CONNEX'
    typp = nomail//'.TYPMAIL'
    nomnoe = nomail//'.NOMNOE'
!
!     RECUPERATION DES NOEUDS DESORDONNES
    call jeveuo(mafour, 'L', jmail)
!
!     ------------------------------------------------------------------
!     RECUPERATION DU TYPE DE MAILLE
!     ------------------------------------------------------------------
    call jeveuo(typp, 'L', iatyma)
    jtypm = iatyma-1+zi(jmail-1 + 1)
    call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypm)), typm)
!
!     ------------------------------------------------------------------
!     CONSTRUCTION D'UN VECTEUR DE TRAVAIL LOCAL CONTENANT
!     LES NOEUDS EXTREMITES  DE CHAQUE MAILLE
!     ------------------------------------------------------------------
    AS_ALLOCATE(vi=noeuds_extrem, size=2*nbma)
    do 30 im = 1, nbma
        call i2extf(zi(jmail-1+im), 1, conec(1:15), typp(1:16), nig,&
                    nid)
        noeuds_extrem(im) = nig
        noeuds_extrem(nbma+im) = nid
30  end do
!
!
!     ------------------------------------------------------------------
!     --- ORDONNANCEMENT DES MAILLES EN PARTANT DU NOEUD ORIGINE
!     ------------------------------------------------------------------
    call jenonu(jexnom(nomnoe, ndorig), njonc)
    n = 1
!     ------------------------------------------------------------------
!     CONSTRUCTION D'UN VECTEUR DE TRAVAIL LOCAL POUR
!     TRIER LES NOEUDS ET CONTENANT
!     LES MAILLES, LES NOEUDS SOMMET 1 ET LES NOEUDS SOMMET 2
!     ------------------------------------------------------------------
    AS_ALLOCATE(vi=mailles_triee, size=3*nbma)
!     EQUIVALENT D'UNE BOUCLE WHILE
550  continue
    do 552 i = n, nbma
        if (noeuds_extrem(i) .eq. njonc) then
            mailles_triee(n)=zi(jmail-1 + i)
            mailles_triee(nbma+n)=noeuds_extrem(i)
            mailles_triee(2*nbma+n)=noeuds_extrem(nbma+i)
            njonc =noeuds_extrem(nbma+i)
            goto 555
        endif
        if (noeuds_extrem(nbma+i) .eq. njonc) then
            mailles_triee(n)=zi(jmail-1 + i)
            mailles_triee(nbma+n)=noeuds_extrem(nbma+i)
            mailles_triee(2*nbma+n)=noeuds_extrem(i)
            njonc =noeuds_extrem(i)
            goto 555
        endif
552  end do
!
555  continue
    do 557 k = n, i-1
        mailles_triee(1+k)=zi(jmail-1 + k)
        mailles_triee(nbma+1+k)=noeuds_extrem(k)
        mailles_triee(2*nbma+1+k)=noeuds_extrem(nbma+k)
557  end do
    do 558 k = i+1, nbma
        mailles_triee(k)=zi(jmail-1 + k)
        mailles_triee(nbma+k)=noeuds_extrem(k)
        mailles_triee(2*nbma+k)=noeuds_extrem(nbma+k)
558  end do
    do 559 k = n, nbma
        zi(jmail-1 + k)=mailles_triee(k)
        noeuds_extrem(k)=mailles_triee(nbma+k)
        noeuds_extrem(nbma+k)=mailles_triee(2*nbma+k)
559  end do
    n=n+1
    if (n .gt. nbma) goto 560
    goto 550
!
!
560  continue
!
!
!     ------------------------------------------------------------------
!     --- SAUVEGARDE DES NOEUDS ORDONNES DANS LA STRUCTURE DE DONNEES
!     --- AVEC RAJOUT DES NOEUDS MILIEUX SI SEG3
!     ------------------------------------------------------------------
    if (typm(1:4) .eq. 'SEG2') then
!
        nbno=nbma+1
        call wkvect(noeord, base//' V I', nbno, jnoe)
        do 570 i = 1, nbma
            zi(jnoe-1 + i) = noeuds_extrem(i)
570      continue
        zi(jnoe-1 + nbma+1) = noeuds_extrem(2*nbma)
!
    else if (typm(1:4) .eq. 'SEG3') then
!
        nbno=2*nbma+1
        call wkvect(noeord, base//' V I', nbno, jnoe)
        do 575 i = 1, nbma
            zi(jnoe-1 + 2*i-1) = noeuds_extrem(i)
            call jeveuo(jexnum(conec, zi(jmail-1 + i)), 'L', jrdm)
            zi(jnoe-1 + 2*i) = zi(jrdm-1 + 3)
575      continue
        zi(jnoe-1 + 2*nbma+1) = noeuds_extrem(2*nbma)
!
    else if (typm(1:4) .eq. 'SEG4') then
!
        nbno=3*nbma+1
        call wkvect(noeord, base//' V I', nbno, jnoe)
        do 580 i = 1, nbma
            zi(jnoe-1 + 3*i-2) = noeuds_extrem(i)
            call jeveuo(jexnum(conec, zi(jmail-1 + i)), 'L', jrdm)
            ASSERT((zi(jrdm-1 + 1).eq.noeuds_extrem(i)) .or. (zi(jrdm-1 + 2).eq.noeuds_extrem(i)))
            if (zi(jrdm-1 + 1) .eq. noeuds_extrem(i)) then
                zi(jnoe-1 + 3*i-1) = zi(jrdm-1 + 3)
                zi(jnoe-1 + 3*i ) = zi(jrdm-1 + 4)
            else if (zi(jrdm-1 + 2).eq.noeuds_extrem(i)) then
                zi(jnoe-1 + 3*i-1) = zi(jrdm-1 + 4)
                zi(jnoe-1 + 3*i ) = zi(jrdm-1 + 3)
            endif
580      continue
        zi(jnoe-1 + 3*nbma+1) = noeuds_extrem(2*nbma)
!
    endif
!
!
!     ------------------------------------------------------------------
!     --- VERIFICATION DU NOEUD EXTREMITE LORSQU'IL EST DONNE
!     --- DANS LE CAS D UNE COURBE NON FERMEE
!     ------------------------------------------------------------------
    if (ndextr .ne. ' ') then
        call jenuno(jexnum(nomnoe, zi(jnoe-1 + nbno)), noeud)
        if (noeud .ne. ndextr) then
            call utmess('F', 'ELEMENTS_77', sk=ndextr)
        endif
    endif
!
!
!     -- SI VECORI EST RENSEIGNE (I.E. != 0),
!        IL FAUT EVENTUELLEMENT RETOURNER LA LISTE
!     ------------------------------------------------------------------
    ps1=ddot(3,vecori,1,vecori,1)
    if (ps1 .gt. 0.d0) then
        ASSERT(nbno.ge.3)
        ASSERT(zi(jnoe-1+1).eq.zi(jnoe-1+nbno))
        call jeveuo(nomail//'.COORDO    .VALE', 'L', vr=vale)
!
!       PS1 : DDOT(VECORI,(1,2))/NORME((1,2))
        do 77, k=1,3
        vecta(k)=vale(3*(zi(jnoe-1+2)-1)+k)
        vecta(k)=vecta(k)-vale(3*(zi(jnoe-1+1)-1)+k)
77      continue
        ps1=ddot(3,vecta,1,vecori,1)
        ps1=ps1/sqrt(ddot(3,vecta,1,vecta,1))
!
!       PS2 : DDOT(VECORI,(N,N-1))/NORME((N,N-1))
        do 78, k=1,3
        vecta(k)=vale(3*(zi(jnoe-1+nbno-1)-1)+k)
        vecta(k)=vecta(k)-vale(3*(zi(jnoe-1+nbno)-1)+k)
78      continue
        ps2=ddot(3,vecta,1,vecori,1)
        ps2=ps2/sqrt(ddot(3,vecta,1,vecta,1))
!
!       -- SI PS2 > PS1 : ON RETOURNE LA LISTE :
        if (ps2 .gt. ps1) then
            do 79, k=1,nbno/2
            ntemp=zi(jnoe-1+k)
            zi(jnoe-1+k)=zi(jnoe-1+nbno+1-k)
            zi(jnoe-1+nbno+1-k)=ntemp
79          continue
        endif
    endif
!
!
!     -- MENAGE :
    AS_DEALLOCATE(vi=mailles_triee)
    AS_DEALLOCATE(vi=noeuds_extrem)
!
    call jedema()
end subroutine
