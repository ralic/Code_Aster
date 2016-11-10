subroutine xdvois(typma, ino, noma, numa, jlsnd, jlsnl, jconx2,&
                  ch2 , lsn, nbmano, jma, adrma, ndim, coupee,&
                  nno, arete, milieu, lsno, voisin)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/conare.h"
#include "asterfort/ismali.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/vecini.h"
!
    integer :: ino, numa, jlsnd, jlsnl, nbmano, voisin(3), jma, adrma
    integer :: ndim, jconx2, nno
    character(len=8) :: typma, arete, noma
    character(len=19) :: ch2
    real(kind=8) :: lsno(3), lsn(4)
    aster_logical :: coupee, milieu
! ---------------------------------------------------------------------
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
! person_in_charge: daniele.colombo at ifpen.fr
!
!     DETERMINATION DE L'ENVIRONNEMENT DU NOEUD COURANT
!
! IN  INO    : NUMERO DU NOEUD COURANT
! IN  TYPMA  : TYPE DE LA MAILLE COURANTE
! IN  NOMA   : NOM DE LA MAILLE COURANTE
! IN  NUMA   : NUMERO DE LA MAILLE COURANTE
! IN  LSN    : NUMERO DE LA MAILLE COURANTE
! IN  NBMANO : NUMERO DE LA MAILLE COURANTE
!
! OUT COUPEE : L'ARETE EST-ELLE COUPEE
! OUT ARETE  : TYPE D'ARETE
! OUT MILIEU : EST-CE UN NOEUD MILIEU
! OUT LSNO   : LSN DES NOEUDS VOISINS
! OUT NNO    : NOMBRE DE NOEUDS SUR L'ARETE
! OUT VOISIN : NUMERO DES NOEUDS VOISINS
!
    integer :: nbar , ar(12,3), i, iad, ima, numav, itypma
    integer :: nuno2, ier, nbno, j
    real(kind=8) :: lsn1, lsn2
    character(len=8) :: typmav
    aster_logical :: connec, miconf
    integer, pointer :: connex(:) => null()
    integer, pointer :: nunotmp(:) => null()
    real(kind=8), pointer :: lsnv(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nbno=0
    call jeveuo(ch2//'.CESV', 'L', vr=lsnv)
! --- RECUPERATION DE LA LISTE DES NOEUDS AFFECTES PAR LA CONDITION
    call jeexin('&&CADDLI.LIST_NODE', ier)
    if (ier .ne. 0) then
        call jeveuo('&&CADDLI.LIST_NODE', 'L', vi=nunotmp)
        call jelira('&&CADDLI.LIST_NODE', 'LONMAX', nbno)
    endif
    call jeexin('&&CAFACI.LIST_NODE', ier)
    if (ier .ne. 0) then
        call jeveuo('&&CAFACI.LIST_NODE', 'L', vi=nunotmp)
        call jelira('&&CAFACI.LIST_NODE', 'LONMAX', nbno)
    endif
    call jeexin('&&CAAREI.LIST_NODE', ier)
    if (ier .ne. 0) then
        call jeveuo('&&CAAREI.LIST_NODE', 'L', vi=nunotmp)
        call jelira('&&CAAREI.LIST_NODE', 'LONMAX', nbno)
    endif
    ASSERT(nbno.gt.0)
! --- RECUPERATION DES MAILLES CONTENANT LE NOEUD
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
!     INDICATEUR POUR LES NOEUDS MILIEUX
    milieu = .false.
!     INDICATEUR, LE NOEUD APPARTIENT-IL A UNE ARETE COUPEE
    coupee = .false.
!     INDICATEUR, LE NOEUD EST UN NOEUD MILIEU ET APPARTIENT A UNE ARETE CONFORME
    miconf = .false.
!     INITIALISATION
    call vecini(3, 0.d0, lsno)
    voisin(1:3) = [0,0,0]
!
    arete = 'SE2'
    nno = 2
    if (.not.ismali(typma)) then
! --- SI ON EST SUR UN NOEUD MILIEU, ON IDENTIFIE LES NOEUDS EXTREMITES DE
!     L'ARETE
       arete = 'SE3'
       nno = 3
       call conare(typma, ar, nbar)
       do i = 1, nbar
          if (ino .eq. connex(zi(jconx2+numa-1)+ar(i,3)-1)) then
             milieu = .true.
             call cesexi('C', jlsnd, jlsnl,numa, ar(i,1) ,&
                         1, 1, iad)
             lsn1 = lsnv(iad)
             call cesexi('C', jlsnd, jlsnl,numa, ar(i,2),&
                         1, 1, iad)
             lsn2 = lsnv(iad)
             if (lsn1.eq.0.d0 .and. lsn2.eq.0.d0 ) then
!     L'ARETE EST CONFORME A LA FISSURE
                miconf=.true.
                coupee = .true.
             elseif (lsn1*lsn2 .ge. 0.d0) then
!     SI L'ARETE N'EST PAS TRAVERSEE PAR LA FISSURE, ON BLOQUE LES DDLS
!     ENRICHIS
                voisin(1) = connex(zi(jconx2+numa-1)+ar(i,1)-1)
                voisin(2) = connex(zi(jconx2+numa-1)+ar(i,2)-1)
                lsno(1) = lsn1
                lsno(2) = lsn2
             elseif (lsn1.lt. 0.d0) then
                voisin(1) = connex(zi(jconx2+numa-1)+ar(i,1)-1)
                voisin(2) = connex(zi(jconx2+numa-1)+ar(i,2)-1)
                lsno(1) = lsn1
                lsno(2) = lsn2
                coupee = .true.
             else
                voisin(1) = connex(zi(jconx2+numa-1)+ar(i,2)-1)
                voisin(2) = connex(zi(jconx2+numa-1)+ar(i,1)-1)
                lsno(2) = lsn1
                lsno(1) = lsn2
                coupee = .true.
             endif
             lsno(3) = lsn(1)
             voisin(3) = connex(zi(jconx2+numa-1)+ar(i,3)-1)
          endif
       end do
    endif
! --- SI L'ON EST PAS SUR UN NOEUD MILIEU, ON IDENTIFIE LES NOEUDS
!     VOISINS DE PART ET D'AUTRE DE LA LSN
    if (.not. milieu .or. miconf) then
! ---     BOUCLE SUR LES MAILLES CONTENANT LE NOEUD
       do ima = 1, nbmano
          numav = zi(adrma-1 + ima)
          itypma=zi(jma-1+numav)
          call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typmav)
          call conare(typmav, ar, nbar)
          if (nbar.eq.1 .and. ndim .eq. 2) goto 79
          if (nbar.le.4 .and. ndim .eq. 3) goto 79
! ---     BOUCLE SUR LES ARETES DE LA MAILLE
          do i = 1, nbar
! ---     ON REGARDE SI LES NOEUDS DE L'ARETE APPARTIENNENT AU GROUPE DE
!         NOEUD AFFECTE
             connec = .false.
             if (ino .eq. connex(zi(jconx2+numav-1)+ar(i,1)-1)) then
                do j = 1, nbno
                   nuno2 = nunotmp(j)
                   if (nuno2 .eq. connex(zi(jconx2+numav-1)+ar(i,2)-1)) then
                      connec = .true.
                      goto 78
                   endif
                end do
             else if (ino .eq. connex(zi(jconx2+numav-1)+ar(i,2)-1)) then
                do j = 1, nbno
                   nuno2 = nunotmp(j)
                   if (nuno2 .eq. connex(zi(jconx2+numav-1)+ar(i,1)-1)) then
                      connec = .true.
                      goto 78
                   endif
                end do
             endif
78           continue
!      SI ON A TROUVE UN NOEUD CONNECTE AU NOEUD COURANT
             if (connec .or. miconf) then
                call cesexi('C', jlsnd, jlsnl,numav, ar(i,1),&
                            1, 1, iad)
                lsn1 = lsnv(iad)
                call cesexi('C', jlsnd, jlsnl,numav, ar(i,2),&
                            1, 1, iad)
                lsn2 = lsnv(iad)
                if (lsn1*lsn2 .lt. 0.d0 .and. .not.miconf) then
                   coupee = .true.
                   if (lsn1 .lt. 0.d0) then
                      voisin(1)  = connex(zi(jconx2+numav-1)+ar(i,1)-1)
                      lsno(1) = lsn1
                      voisin(2)  = connex(zi(jconx2+numav-1)+ar(i,2)-1)
                      lsno(2) = lsn2
                   elseif (lsn1 .gt. 0.d0) then
                      voisin(2)  = connex(zi(jconx2+numav-1)+ar(i,1)-1)
                      lsno(2) = lsn1
                      voisin(1)  = connex(zi(jconx2+numav-1)+ar(i,2)-1)
                      lsno(1) = lsn2
                   endif
                   if (.not. ismali(typmav)) then
                      call cesexi('C', jlsnd, jlsnl,numav, ar(i,3),&
                                   1, 1, iad)
                      lsno(3) = lsnv(iad)
                      voisin(3) = connex(zi(jconx2+numav-1)+ar(i,3)-1)
                   endif
                endif
                if (lsn1*lsn2 .ge. 0.d0 .or. miconf) then
                   if (lsn(1) .eq. 0.d0) then
                      if (lsn1 .lt. 0.d0) then
                         voisin(1)  = connex(zi(jconx2+numav-1)+ar(i,1)-1)
                         lsno(1) = lsn1
                      elseif (lsn1 .gt.0.d0) then
                         voisin(2)  = connex(zi(jconx2+numav-1)+ar(i,1)-1)
                         lsno(2) = lsn1
                      elseif (lsn2 .lt. 0.d0) then
                         voisin(1)  = connex(zi(jconx2+numav-1)+ar(i,2)-1)
                         lsno(1) = lsn2
                      elseif (lsn2 .gt.0.d0) then
                         voisin(2)  = connex(zi(jconx2+numav-1)+ar(i,2)-1)
                         lsno(2) = lsn2
                      endif
                      coupee = .true.
                   endif
                endif
             endif
          end do
79        continue
       end do
    endif
!
    call jedema()
end subroutine
