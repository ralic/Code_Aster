subroutine xmulhm(contac, ddls, ddlc, ddlm, jaint, ifiss,&
                  jheano, vstnc, lact, lcalel, lelim,&
                  nfh, nfiss, ninter,&
                  nlact, nno, nnol, nnom, nnos,&
                  pla, pos, typma, jstano)
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
! IN TYPMA : TYPE DE MAILLE
! IN NINTER : NOMBRE DE POINTS D'INTERSECTION
! IN IAINT : ADRESSE TOPOFAC.AI POUR LA FISSURE COURANTE
! OUT LACT : LISTE LAMBDA ACTIFS
! OUT NLACT : NOMBRE LAMBDA ACTIFS
! IN NFISS : NOMBRE DE FISSURES VUES PAR L ELEMENT
! IN JSTNC : ADRESSE TABLEAU FISSURE ACTIVE OU NON
! IN NFH  : NOMBRE ENRICHISSEMENTS HEAVISIDE POUR L ELEMENT
! IN IFISS : INDICE DE LA FISSURE
! IN JHEANO : HEAVISIDE ACTIF AU NOEUD?
! IN CONTAC : P1P1 OU P2P1
! OUT NNOL : NOMBRE DE NOEUDS PORTEURS DE LAGRANGES
! IN NNO : NOMBRE TOT DE NOEUDS
! IN NNOS : NOMBRE NOEUDS SOMMETS
! IN DDLC : NOMBRE DE DDLS DE CONTACT PAR NOEUD
! IN DDLM :
! IN NNOM : NOMBRE DE NOEUDS MILIEUX
! IN LCALEL : SI TE DE CALCUL ELEMENTAIRE
! OUT PLA : PLACES DE LAGRANGES DE CONTACT DANS LA MATRICE
! OUT LELIM : YA-T-IL DES LAGRANGES DE CONTACT
!
! --- LISTE DES LAMBDAS ACTIFS
!
#include "asterfort/conare.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xplmat.h"
#include "asterfort/xxmmvd.h"
    integer :: contac, ddlc, ddlm, jaint, ifiss, jheano, vstnc(*)
    integer :: lact(16), nfh, nfiss, ninter, nlact(2)
    integer :: nno, nnol, nnom, nnos, pla(27), vit(16), nli
    integer :: pli, i, ddls, ino, iar , nvit, ino1, ino2
    integer :: ar(12,3), nbar, zxain, pos(16)
    integer, optional, intent(in) :: jstano
    aster_logical :: lelim, lcalel
    character(len=8) :: typma
! ----------------------------------------------------------------------
!
    do ino = 1, 16
        lact(ino) = 0
        vit(ino) = 0
    end do
    nlact(1:2) = 0
    call conare(typma, ar, nbar)
    zxain=xxmmvd('ZXAIN')
!
! --- ON ACTIVE LES NOEUDS CONNECTES AUX POINTS D'INTERSECTION
!
       do nli = 1, ninter
           iar=int(zr(jaint-1+zxain*(nli-1)+1))
           ino=int(zr(jaint-1+zxain*(nli-1)+2))
           nvit=int(zr(jaint-1+zxain*(nli-1)+5))
           if (ino .gt. 0) then
               lact(ino)=nli
           else if (iar.gt.0) then
               ino1=ar(iar,1)
               ino2=ar(iar,2)
               if (nvit .eq. 1) then
                   lact(ino1)=nli
                   vit(ino1)=1
                   lact(ino2)=nli
                   vit(ino2)=1
               else
                   if (vit(ino1) .eq. 0) lact(ino1)=nli
                   if (vit(ino2) .eq. 0) lact(ino2)=nli
               endif
           endif
       end do
! --- ON COMPTE LE NOMBRE DE NOEUDS ACTIFS
       do ino = 1, 8
           if (lact(ino) .ne. 0) nlact(1)=nlact(1)+1
       end do
!
    if (lcalel) then
        if (nlact(1).lt.nnos .or. nlact(2).lt.nnos) lelim = .true.
        if (nfiss .eq. 1) then
            do i = 1, nnos
                if (lact(i) .eq. 0) vstnc(i)=0
            end do
        else
            if (ninter.gt.0) then
               do i = 1, nnos
                   if (lact(i) .eq. 0) then
                      vstnc((i-1)*nfh+zi(jheano-1+(i-1)*nfiss+ifiss))=0
                   endif
               end do
            else
               do i = 1, nnos
                   if (lact(i) .eq. 0 .and. zi(jstano-1+(i-1)*nfiss+ifiss) .eq. 0) then
                       vstnc((i-1)*nfh+zi(jheano-1+(i-1)*nfiss+ifiss))=0
                   endif
               end do
            endif
!
        endif
    endif
! --- NOMBRE DE LAMBDAS ET LEUR PLACE DANS LA MATRICE
    if (contac .eq. 1) nnol=nno
    if (contac .eq. 2) nnol=nnos
    if (contac .eq. 3) nnol=nnos
    do i = 1, nnol
        call xplmat(ddls, ddlc, ddlm,&
                    nnos, nnom, i, pli)
        if (nfiss .eq. 1) then
            pla(i) = pli
            pos(i) = 1
        else
            pla(i) = pli+ddlc/nfh*(zi(jheano-1+(i-1)*nfiss+ifiss)-1)
            pos(i) = zi(jheano-1+(i-1)*nfiss+ifiss)
        endif
!
    end do
end subroutine
