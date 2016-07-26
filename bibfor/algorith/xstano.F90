subroutine xstano(noma, lisno, nmafis, jmafis, cnslt,&
                  cnsln, cnslj, rayon, cnxinv, stano,&
                  typdis)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/dismoi.h"
#include "asterfort/ismali.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/panbno.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
!
    real(kind=8) :: rayon
    integer :: nmafis, jmafis
    character(len=8) :: noma
    character(len=16) :: typdis
    character(len=19) :: cnslt, cnsln, cnslj, cnxinv
    character(len=24) :: lisno, stano
!     ------------------------------------------------------------------
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
! person_in_charge: samuel.geniaut at edf.fr
!
!                DETERMINER LE STATUT (ENRICHISSEMENT) DES NOEUDS
!                    1 : ENRICHISSEMENT HEAVISIDE
!                    2 : ENRICHISSEMENT CRACK TIP
!                    3 : ENRICHISSEMENT HEAVISIDE ET CRACK TIP
!
!     ENTREE
!         NOMA   : NOM DE L'OBJET MAILLAGE
!         LISNO  : LISTE DES NOEUDS DE GROUP_MA_ENRI
!         NMAFIS : NOMBRE DE MAILLES DE LA ZONE FISSURE
!         JMAFIS : ADRESSE DES MAILLES DE LA ZONE FISSURE
!         CNSLT  : LEVEL SET TANGENTE
!         CNSLN  : LEVEL SET NORMALE
!         CNSLJ  : LEVEL SET JONCTION
!         RAYON  : RAYON DE LA ZONE D'ENRICHISSEMENT DES NOEUDS EN FOND
!                  DE FISSURE
!         CNXINV : CONNECTIVITE INVERSE
!         STANO  : VECTEUR STATUT DES NOEUDS INITIALISE À 0
!
!     SORTIE
!         STANO  : VECTEUR STATUT DES NOEUDS
!     ------------------------------------------------------------------
!
    integer :: in, ar(12, 3), ia, i, j, k, nbnoe, nbnott(3)
    integer :: ino, ima, nuno, nrien, nbar, na, nm, nunom
    integer :: nb, nunoa, nunob, enr, enr1, enr2, jdlino, jma, jstano
    integer :: jconx2, itypma, ndim
    integer :: nfiss, ifiss
    integer :: nbma, jlmaf, nmasup, jmasup, isup, iret
    real(kind=8) :: minlsn, minlst, maxlsn, maxlst, lsna, lsnb, lsta, lstb
    real(kind=8) :: minlsj(10, 2), maxlsj(10), lsja(10, 2), lsjb(10, 2), lsjm(10,2)
    real(kind=8) :: lsjc(10, 2), lstm, lsnm, a1, b1, c1, a2, b2, c2, x1
    real(kind=8) :: lstc, lsn, a(3), b(3), c(3), lst, mincoh
    real(kind=8) :: ab(3), ac(3)
    character(len=8) :: typma
    character(len=19) :: mai, lmafis
    aster_logical :: ljonc, cohenr
    real(kind=8), pointer :: vale(:) => null()
    real(kind=8), pointer :: ljsv(:) => null()
    real(kind=8), pointer :: lnsv(:) => null()
    real(kind=8), pointer :: ltsv(:) => null()
    integer, pointer :: connex(:) => null()
    integer, pointer :: cnsd(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call jeveuo(noma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
    call jelira(lisno, 'LONMAX', nbnoe)
    call jeveuo(lisno, 'L', jdlino)
!
    call jeveuo(stano, 'E', jstano)
!
    call jeveuo(cnslt//'.CNSV', 'L', vr=ltsv)
    call jeveuo(cnsln//'.CNSV', 'L', vr=lnsv)
    call jeexin(cnslj//'.CNSD', iret)
    ljonc = .false.
    if (iret .ne. 0) then
        ljonc = .true.
        call jeveuo(cnslj//'.CNSD', 'L', vi=cnsd)
        nfiss = cnsd(2)/2
        call jeveuo(cnslj//'.CNSV', 'L', vr=ljsv)
    endif
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
    mai=noma//'.TYPMAIL'
    call jeveuo(mai, 'L', jma)
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
!
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
!
!     CREATION D'UN VECTEUR TEMPORAIRE LMAFIS POUR SAVOIR RAPIDEMENT
!     SI UNE MAILLE DU MAILLAGE APPARTIENT A MAFIS
    lmafis='&&XSTANO.LMAFIS'
    call wkvect(lmafis, 'V V I', nbma, jlmaf)
    do i = 1, nmafis
        ima=zi(jmafis-1+i)
        zi(jlmaf-1+ima)=1
    end do
!
!     BOUCLE SUR LES NOEUDS DE GROUP_ENRI
!
    do i = 1, nbnoe
        maxlsn=-1*r8maem()
        minlsn=r8maem()
        maxlst=-1*r8maem()
        minlst=r8maem()
        if (ljonc) then
            do ifiss = 1, nfiss
                maxlsj(ifiss)=-1*r8maem()
                minlsj(ifiss,1)= r8maem()
                minlsj(ifiss,2)= r8maem()
            end do
        endif
        ino=zi(jdlino-1+i)
        call jelira(jexnum(cnxinv, ino), 'LONMAX', nmasup)
        call jeveuo(jexnum(cnxinv, ino), 'L', jmasup)
!
!       ON VERIFIE SI LE NOEUD N'EST PAS ORPHELIN
        if (zi(jmasup) .eq. 0) goto 200
!
        isup=0
!
!       BOUCLE SUR LES MAILLES SUPPORT DE INO
        cohenr = .true.
        do j = 1, nmasup
            mincoh = r8maem()
            ima = zi(jmasup-1+j)
!
!         SI LA MAILLE N'APPARTIENT PAS A MAFIS, ON PASSE A LA SUIVANTE
            if (zi(jlmaf-1+ima) .eq. 0) goto 210
            isup = 1
!
!         MAILLE SUPPORT APPARTENANT A MAFIS :
!         ON CALCULE LST QUE SUR LES PTS OÙ LSN=0
!         ON CALCULE LSN SUR LES NOEUDS
            nrien=0
!         BOUCLE SUR LES ARETES DE LA MAILLE RETENUE
            itypma=zi(jma-1+ima)
            call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
            call conare(typma, ar, nbar)
            do ia = 1, nbar
                na=ar(ia,1)
                nb=ar(ia,2)
                nunoa=connex(zi(jconx2+ima-1)+na-1)
                nunob=connex(zi(jconx2+ima-1)+nb-1)
                lsna=lnsv((nunoa-1)+1)
                lsnb=lnsv((nunob-1)+1)
                lsta=ltsv((nunoa-1)+1)
                lstb=ltsv((nunob-1)+1)
                if (ljonc) then
                    do ifiss = 1, nfiss
                        lsja(ifiss,1)=ljsv(2*nfiss*(nunoa-1)+2*(&
                        ifiss-1)+1)
                        lsjb(ifiss,1)=ljsv(2*nfiss*(nunob-1)+2*(&
                        ifiss-1)+1)
                        lsja(ifiss,2)=ljsv(2*nfiss*(nunoa-1)+2*(&
                        ifiss-1)+2)
                        lsjb(ifiss,2)=ljsv(2*nfiss*(nunob-1)+2*(&
                        ifiss-1)+2)
                    end do
                endif
                if (.not.ismali(typma)) then
                  mincoh=-1
!               ON NE TRAITE PAS LA MULTI-FISSURATION EN QUADRATIQUE
                  nm=ar(ia,3)
                  nunom=connex(zi(jconx2+ima-1)+nm-1)
                  lsnm=lnsv((nunom-1)+1)
                  lstm=ltsv((nunom-1)+1)
                  if (ljonc) then
                      do ifiss = 1, nfiss
                           lsjm(ifiss,1)=ljsv(2*nfiss*(nunom-1)+2*(&
                           ifiss-1)+1)
                           lsjm(ifiss,2)=ljsv(2*nfiss*(nunom-1)+2*(&
                           ifiss-1)+2)
                      end do
                  endif
                  if (lsna .eq. 0.d0 .and. lsnb .eq. 0.d0) then
                      if (lsta .lt. minlst) minlst=lsta
                      if (lsta .gt. maxlst) maxlst=lsta
                      if (lstb .lt. minlst) minlst=lstb
                      if (lstb .gt. maxlst) maxlst=lstb
                      if (lstm .lt. minlst) minlst=lstm
                      if (lstm .gt. maxlst) maxlst=lstm
                      if (ljonc) then
                          do ifiss = 1, nfiss
                              if (lsjb(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjb(ifiss,1)
                              if (lsjb(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjb(ifiss,1)
                              if (lsja(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsja(ifiss,1)
                              if (lsja(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsja(ifiss,1)
                              if (lsjm(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjm(ifiss,1)
                              if (lsjm(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjm(ifiss,1)
                              if (lsjb(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjb(ifiss,2)
                              if (lsja(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsja(ifiss,2)
                              if (lsjm(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjm(ifiss,2)
                          end do
                      endif
                  else if (lsna .eq. 0 .and. lsnm .ne. 0) then
                      if (lsta .lt. minlst) minlst=lsta
                      if (lsta .gt. maxlst) maxlst=lsta
                      if (ljonc) then
                          do ifiss = 1, nfiss
                              if (lsja(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsja(ifiss,1)
                              if (lsja(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsja(ifiss,1)
                              if (lsja(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsja(ifiss,2)
                          end do
                      endif
                  else if (lsna .eq. 0 .and. lsnm .eq. 0) then
                      if (lsta .lt. minlst) minlst=lsta
                      if (lsta .gt. maxlst) maxlst=lsta
                      if (lstm .lt. minlst) minlst=lstm
                      if (lstm .gt. maxlst) maxlst=lstm
                      if (ljonc) then
                          do ifiss = 1, nfiss
                              if (lsja(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsja(ifiss,1)
                              if (lsja(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsja(ifiss,1)
                              if (lsjm(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjm(ifiss,1)
                              if (lsjm(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjm(ifiss,1)
                              if (lsja(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsja(ifiss,2)
                              if (lsjm(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjm(ifiss,2)
                          end do
                      endif
                  else if (lsnb .eq. 0 .and. lsnm .ne. 0) then
                      if (lstb .lt. minlst) minlst=lstb
                      if (lstb .gt. maxlst) maxlst=lstb
                      if (ljonc) then
                          do ifiss = 1, nfiss
                              if (lsjb(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjb(ifiss,1)
                              if (lsjb(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjb(ifiss,1)
                              if (lsjb(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjb(ifiss,2)
                          end do
                      endif
                  else if (lsnb .eq. 0 .and. lsnm .eq. 0) then
                      if (lstb .lt. minlst) minlst=lstb
                      if (lstb .gt. maxlst) maxlst=lstb
                      if (lstm .lt. minlst) minlst=lstm
                      if (lstm .gt. maxlst) maxlst=lstm
                      if (ljonc) then
                          do ifiss = 1, nfiss
                              if (lsjb(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjb(ifiss,1)
                              if (lsjb(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjb(ifiss,1)
                              if (lsjm(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjm(ifiss,1)
                              if (lsjm(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjm(ifiss,1)
                              if (lsjb(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjb(ifiss,2)
                              if (lsjm(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjm(ifiss,2)
                          end do
                      endif
                  else if (lsna*lsnb .lt. 0) then
!              CA VEUT DIRE QUE LSN S'ANNULE SUR L'ARETE AU PT C
!              (RETENU) ET ACTUALISATION DE MIN ET MAX POUR LST EN CE PT
                      do k = 1, ndim
                          a(k)=vale(3*(nunoa-1)+k)
                          b(k)=vale(3*(nunob-1)+k)
                          ab(k)=b(k)-a(k)
!                 INTERPOLATION DES COORDONNEES DE C ET DE LST EN C
                          ac(k)=c(k)-a(k)
                      end do
                      ASSERT(ddot(ndim, ab, 1, ab, 1).gt.r8prem())

!                 ON CALCULE LES COORDONNEES DU POINT D'INTERSECTION DANS L'ELEMENT DE REFERNCE
                      a1 = lsna+lsnb - 2.d0*lsnm
                      b1 = lsnb - lsna
                      c1 = 2.d0*lsnm
                      if (abs(a1) .lt. 1.d-8) then
                          x1 = lsna/(lsna-lsnb)
                          x1 = 2.d0*x1-1
                      else
                          x1 = (-b1 - sqrt(b1**2 - 4.d0*a1*c1))/(2.d0*a1)
                          if (abs(x1).gt.1.d0) then
                             x1 = (-b1 + sqrt(b1**2 - 4.d0*a1*c1))/(2.d0*a1)
                          endif
                      endif
                      a2 = lsta+lstb - 2.d0*lstm
                      b2 = lstb - lsta
                      c2 = 2.d0*lstm
                      lstc = (a2*x1**2+b2*x1+c2)*5.d-1
                      if (lstc .lt. minlst) minlst=lstc
                      if (lstc .gt. maxlst) maxlst=lstc
                      if (ljonc) then
                          do ifiss = 1, nfiss
                              a2 = lsja(ifiss,1)+lsjb(ifiss,1)-2.d0*lsjm(ifiss,1)
                              b2 = lsjb(ifiss,1)-lsja(ifiss,1)
                              c2 = 2.d0*lsjm(ifiss,1)
                              lsjc(ifiss,1) = (a2*x1**2+b2*x1+c2)*5.d-1
                              a2 = lsja(ifiss,2)+lsjb(ifiss,2)-2.d0*lsjm(ifiss,2)
                              b2 = lsjb(ifiss,2)-lsja(ifiss,2)
                              c2 = 2.d0*lsjm(ifiss,2)
                              lsjc(ifiss,2) = (a2*x1**2+b2*x1+c2)*5.d-1
                              if (lsjc(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjc(ifiss,1)
                              if (lsjc(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjc(ifiss,1)
                              if (lsjc(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjc(ifiss,2)
                          end do
                      endif
                  endif
                else
                  if (lsna .eq. 0.d0 .and. lsnb .eq. 0.d0) then
!               ON RETIENT LES 2 POINTS A ET B
!               ET ACTUALISATION DE MIN ET MAX POUR LST
                      if (lsta .lt. minlst) minlst=lsta
                      if (lsta .lt. mincoh) mincoh=lsta
                      if (lsta .gt. maxlst) maxlst=lsta
                      if (lstb .lt. minlst) minlst=lstb
                      if (lstb .lt. mincoh) mincoh=lstb
                      if (lstb .gt. maxlst) maxlst=lstb
                      if (ljonc) then
                          do ifiss = 1, nfiss
                              if (lsjb(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjb(ifiss,1)
                              if (lsjb(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjb(ifiss,1)
                              if (lsja(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsja(ifiss,1)
                              if (lsja(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsja(ifiss,1)
                              if (lsjb(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjb(ifiss,2)
                              if (lsja(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsja(ifiss,2)
                          end do
                      endif
                  else if ((lsna*lsnb).le.0.d0) then
!              CA VEUT DIRE QUE LSN S'ANNULE SUR L'ARETE AU PT C
!              (RETENU) ET ACTUALISATION DE MIN ET MAX POUR LST EN CE PT
                      do k = 1, ndim
                          a(k)=vale(3*(nunoa-1)+k)
                          b(k)=vale(3*(nunob-1)+k)
                          ab(k)=b(k)-a(k)
!                 INTERPOLATION DES COORDONNÉES DE C ET DE LST EN C
                          c(k)=a(k)-lsna/(lsnb-lsna)*ab(k)
                          ac(k)=c(k)-a(k)
                      end do
                      ASSERT(ddot(ndim, ab, 1, ab, 1).gt.r8prem())
                      lstc = lsta + (lstb-lsta) * ddot(ndim,ab,1,ac,1) / ddot(ndim,ab,1,ab,1)
                      if (lstc .lt. minlst) minlst=lstc
                      if (lstc .lt. mincoh) mincoh=lstc
                      if (lstc .gt. maxlst) maxlst=lstc
                      if (ljonc) then
                          do ifiss = 1, nfiss
                              lsjc(ifiss,1) = lsja(ifiss,1) + (lsjb( ifiss,1)-lsja(ifiss,1)) * ddot&
                                              &(ndim,ab,1, ac,1) / ddot(ndim,ab,1,ab,1)
                              lsjc(ifiss,2) = lsja(ifiss,2) + (lsjb( ifiss,2)-lsja(ifiss,2)) * ddot&
                                              &(ndim,ab,1, ac,1) / ddot(ndim,ab,1,ab,1)
                              if (lsjc(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjc(ifiss,1)
                              if (lsjc(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjc(ifiss,1)
                              if (lsjc(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjc(ifiss,2)
                          end do
                      endif
                  else
!               AUCUN POINT DE L'ARETE N'A LSN = 0,ALORS ON RETIENT RIEN
                      nrien=nrien+1
                  endif
                endif
!           AUCUNE ARETE SUR LAQUELLE LSN S'ANNULE
                ASSERT(nrien.ne.nbar)
            end do
!
            call panbno(itypma, nbnott)
!         BOUCLE SUR LES NOEUDS SOMMET DE LA MAILLE COURANTE
            do in = 1, nbnott(1)
                nuno=connex(zi(jconx2+ima-1)+in-1)
                lsn=lnsv((nuno-1)+1)
                if (lsn .lt. minlsn) minlsn=lsn
                if (lsn .gt. maxlsn) maxlsn=lsn
            end do
             if(mincoh.ge.0.d0) cohenr = .false.
210         continue
        end do
!
        enr=0
        enr1=0
        enr2=0
!
!       TEST S'IL Y A EU UNE MAILLE SUPPORT TROUVÉE DANS MAFIS
        if (isup .gt. 0) then
            if(typdis.eq.'COHESIF') then
                if ((minlsn*maxlsn.lt.0.d0).and.cohenr) enr1=1
            else
                if ((minlsn*maxlsn.lt.0.d0) .and. (maxlst.le.r8prem())) enr1=1
                if ((minlsn*maxlsn.le.r8prem()) .and.&
                (minlst* maxlst.le.r8prem())) then
                  enr2=-2
!                  enr2=+2
                endif
            endif
            if (ljonc) then
!       CORRECTION DU STATUT SI ON EST DU MAUVAIS COTÉ DE LA JONCTION
                do ifiss = 1, nfiss
                    if (minlsj(ifiss,1) .ge. 0 .and. minlsj(ifiss,2) .lt. 0) then
                        enr1=0
                        enr2=0
                    endif
!       CORRECTION DU STATUT SI ON EST SUR LA JONCTION
                    if (abs(enr2) .eq. 2 .and. minlsj(ifiss,1)*maxlsj(ifiss) .le. r8prem() .and.&
                        minlsj(ifiss,2) .lt. 0) then
                        enr2=0
                        if (minlsn*maxlsn .lt. 0.d0) enr1 = 1
                    endif
                end do
            endif
        endif
!
!       SI ON DEFINIT UN RAYON POUR LA ZONE D'ENRICHISSEMENT SINGULIER
        if (rayon .gt. 0.d0 .and. typdis.ne.'COHESIF') then
            lsn=lnsv((ino-1)+1)
            lst=ltsv((ino-1)+1)
            if (sqrt(lsn**2+lst**2) .le. rayon .and. enr2.ne.-2) enr2=2
        endif
!
!       ATTENTION, LE TRAITEMENT EVENTUEL DE NB_COUCHES N'EST PAS FAIT
!       ICI CAR ON NE CONNAIT PAS LA TAILLE DES MAILLES DU FOND DE FISS
!       CE TRAITEMENT SERA EFFECTUE APRES DANS XENRCH, DONC STANO SERA
!       PEUT ETRE MODIFIE
!
!  --- DEFINITION D UN NOUVEAU STATUT POUR LES NOEUDS EN FOND DE FISSURE
        if(enr2.eq.-2 .and. enr1.eq.1) enr2=2
!
!        if(abs(enr2).eq.2 .and. enr1.eq.1) enr2=0
!
        enr=enr1+enr2
!
!       ENREGISTREMENT DU STATUT DU NOEUD
        zi(jstano-1+(ino-1)+1)=enr
!
200     continue
    end do
!
    call jedetr(lmafis)
!
    call jedema()
end subroutine
