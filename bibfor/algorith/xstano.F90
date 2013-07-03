subroutine xstano(noma, lisno, nmafis, jmafis, cnslt,&
                  cnsln, cnslj, rayon, cnxinv, stano)
    implicit none
!
#include "jeveux.h"
!
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/dismoi.h"
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
    real(kind=8) :: rayon
    integer :: nmafis, jmafis
    character(len=8) :: noma
    character(len=19) :: cnslt, cnsln, cnslj, cnxinv
    character(len=24) :: lisno, stano
!     ------------------------------------------------------------------
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
    integer :: ino, ima, nuno, nrien, nbar, na
    integer :: nb, nunoa, nunob, enr, enr1, enr2, jdlino, jma, jstano
    integer :: jconx1, jconx2, jltsv, jlnsv, jcoor, itypma, ndim
    integer :: jljsd, jljsv, nfiss, ifiss
    integer :: nbma, ibid, jlmaf, nmasup, jmasup, isup, iret
    real(kind=8) :: minlsn, minlst, maxlsn, maxlst, lsna, lsnb, lsta, lstb
    real(kind=8) :: minlsj(10, 2), maxlsj(10), lsja(10, 2), lsjb(10, 2)
    real(kind=8) :: lsjc(10, 2)
    real(kind=8) :: lstc, lsn, a(3), b(3), c(3), lst
    real(kind=8) :: ab(3), ac(3)
    character(len=8) :: typma, k8b
    character(len=19) :: mai, lmafis
    logical :: ljonc
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
    call jelira(lisno, 'LONMAX', nbnoe, k8b)
    call jeveuo(lisno, 'L', jdlino)
!
    call jeveuo(stano, 'E', jstano)
!
    call jeveuo(cnslt//'.CNSV', 'L', jltsv)
    call jeveuo(cnsln//'.CNSV', 'L', jlnsv)
    call jeexin(cnslj//'.CNSD', iret)
    ljonc = .false.
    if (iret .ne. 0) then
        ljonc = .true.
        call jeveuo(cnslj//'.CNSD', 'L', jljsd)
        nfiss = zi(jljsd-1+2)/2
        call jeveuo(cnslj//'.CNSV', 'L', jljsv)
    endif
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    mai=noma//'.TYPMAIL'
    call jeveuo(mai, 'L', jma)
    call dismoi('F', 'DIM_GEOM', noma, 'MAILLAGE', ndim,&
                k8b, iret)
!
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8b, ibid)
!
!     CREATION D'UN VECTEUR TEMPORAIRE LMAFIS POUR SAVOIR RAPIDEMENT
!     SI UNE MAILLE DU MAILLAGE APPARTIENT A MAFIS
    lmafis='&&XSTANO.LMAFIS'
    call wkvect(lmafis, 'V V I', nbma, jlmaf)
    do 10 i = 1, nmafis
        ima=zi(jmafis-1+i)
        zi(jlmaf-1+ima)=1
10  end do
!
!     BOUCLE SUR LES NOEUDS DE GROUP_ENRI
    do 200 i = 1, nbnoe
        maxlsn=-1*r8maem()
        minlsn=r8maem()
        maxlst=-1*r8maem()
        minlst=r8maem()
        if (ljonc) then
            do 201 ifiss = 1, nfiss
                maxlsj(ifiss)=-1*r8maem()
                minlsj(ifiss,1)= r8maem()
                minlsj(ifiss,2)= r8maem()
201          continue
        endif
        ino=zi(jdlino-1+i)
        call jelira(jexnum(cnxinv, ino), 'LONMAX', nmasup, k8b)
        call jeveuo(jexnum(cnxinv, ino), 'L', jmasup)
!
!       ON VERIFIE SI LE NOEUD N'EST PAS ORPHELIN
        if (zi(jmasup) .eq. 0) goto 200
!
        isup=0
!
!       BOUCLE SUR LES MAILLES SUPPORT DE INO
        do 210 j = 1, nmasup
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
            do 212 ia = 1, nbar
                na=ar(ia,1)
                nb=ar(ia,2)
                nunoa=zi(jconx1-1+zi(jconx2+ima-1)+na-1)
                nunob=zi(jconx1-1+zi(jconx2+ima-1)+nb-1)
                lsna=zr(jlnsv-1+(nunoa-1)+1)
                lsnb=zr(jlnsv-1+(nunob-1)+1)
                lsta=zr(jltsv-1+(nunoa-1)+1)
                lstb=zr(jltsv-1+(nunob-1)+1)
                if (ljonc) then
                    do 211 ifiss = 1, nfiss
                        lsja(ifiss,1)=zr(jljsv-1+2*nfiss*(nunoa-1)+2*(&
                        ifiss-1)+1)
                        lsjb(ifiss,1)=zr(jljsv-1+2*nfiss*(nunob-1)+2*(&
                        ifiss-1)+1)
                        lsja(ifiss,2)=zr(jljsv-1+2*nfiss*(nunoa-1)+2*(&
                        ifiss-1)+2)
                        lsjb(ifiss,2)=zr(jljsv-1+2*nfiss*(nunob-1)+2*(&
                        ifiss-1)+2)
211                  continue
                endif
                if (lsna .eq. 0.d0 .and. lsnb .eq. 0.d0) then
!             ON RETIENT LES 2 POINTS A ET B
!             ET ACTUALISATION DE MIN ET MAX POUR LST
                    if (lsta .lt. minlst) minlst=lsta
                    if (lsta .gt. maxlst) maxlst=lsta
                    if (lstb .lt. minlst) minlst=lstb
                    if (lstb .gt. maxlst) maxlst=lstb
                    if (ljonc) then
                        do 214 ifiss = 1, nfiss
                            if (lsjb(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjb(ifiss,1)
                            if (lsjb(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjb(ifiss,1)
                            if (lsja(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsja(ifiss,1)
                            if (lsja(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsja(ifiss,1)
                            if (lsjb(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjb(ifiss,2)
                            if (lsja(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsja(ifiss,2)
214                      continue
                    endif
                else if ((lsna*lsnb).le.0.d0) then
!            CA VEUT DIRE QUE LSN S'ANNULE SUR L'ARETE AU PT C
!            (RETENU) ET ACTUALISATION DE MIN ET MAX POUR LST EN CE PT
                    do 21 k = 1, ndim
                        a(k)=zr(jcoor-1+3*(nunoa-1)+k)
                        b(k)=zr(jcoor-1+3*(nunob-1)+k)
                        ab(k)=b(k)-a(k)
!               INTERPOLATION DES COORDONNÉES DE C ET DE LST EN C
                        c(k)=a(k)-lsna/(lsnb-lsna)*ab(k)
                        ac(k)=c(k)-a(k)
21                  continue
                    call assert(ddot(ndim, ab, 1, ab, 1).gt.r8prem())
                    lstc = lsta + (lstb-lsta) * ddot(ndim,ab,1,ac,1) / ddot(ndim,ab,1,ab,1)
                    if (lstc .lt. minlst) minlst=lstc
                    if (lstc .gt. maxlst) maxlst=lstc
                    if (ljonc) then
                        do 215 ifiss = 1, nfiss
                            lsjc(ifiss,1) = lsja(ifiss,1) + (lsjb( ifiss,1)-lsja(ifiss,1)) * ddot&
                                            &(ndim,ab,1, ac,1) / ddot(ndim,ab,1,ab,1)
                            lsjc(ifiss,2) = lsja(ifiss,2) + (lsjb( ifiss,2)-lsja(ifiss,2)) * ddot&
                                            &(ndim,ab,1, ac,1) / ddot(ndim,ab,1,ab,1)
                            if (lsjc(ifiss,1) .lt. minlsj(ifiss,1)) minlsj(ifiss,1)=lsjc(ifiss,1)
                            if (lsjc(ifiss,1) .gt. maxlsj(ifiss)) maxlsj(ifiss)=lsjc(ifiss,1)
                            if (lsjc(ifiss,2) .lt. minlsj(ifiss,2)) minlsj(ifiss,2)=lsjc(ifiss,2)
215                      continue
                    endif
                else
!             AUCUN POINT DE L'ARETE N'A LSN = 0,ALORS ON RETIENT RIEN
                    nrien=nrien+1
                endif
!           AUCUNE ARETE SUR LAQUELLE LSN S'ANNULE
                call assert(nrien.ne.nbar)
212          continue
!
            call panbno(itypma, nbnott)
!         BOUCLE SUR LES NOEUDS SOMMET DE LA MAILLE COURANTE
            do 213 in = 1, nbnott(1)
                nuno=zi(jconx1-1+zi(jconx2+ima-1)+in-1)
                lsn=zr(jlnsv-1+(nuno-1)+1)
                if (lsn .lt. minlsn) minlsn=lsn
                if (lsn .gt. maxlsn) maxlsn=lsn
213          continue
!
210      continue
!
        enr=0
        enr1=0
        enr2=0
!
!       TEST S'IL Y A EU UNE MAILLE SUPPORT TROUVÉE DANS MAFIS
        if (isup .gt. 0) then
            if ((minlsn*maxlsn.lt.0.d0) .and. (maxlst.le.r8prem())) enr1=1
            if ((minlsn*maxlsn.le.r8prem()) .and. (minlst* maxlst.le.r8prem())) enr2=2
            if (ljonc) then
!       CORRECTION DU STATUT SI ON EST DU MAUVAIS COTÉ DE LA JONCTION
                do 220 ifiss = 1, nfiss
                    if (minlsj(ifiss,1) .ge. 0 .and. minlsj(ifiss,2) .lt. 0) then
                        enr1=0
                        enr2=0
                    endif
!       CORRECTION DU STATUT SI ON EST SUR LA JONCTION
                    if (enr2 .eq. 2 .and. minlsj(ifiss,1)*maxlsj(ifiss) .le. r8prem() .and.&
                        minlsj(ifiss,2) .lt. 0) then
                        enr2=0
                        if (minlsn*maxlsn .lt. 0.d0) enr1 = 1
                    endif
220              continue
            endif
        endif
!
!       SI ON DEFINIT UN RAYON POUR LA ZONE D'ENRICHISSEMENT SINGULIER
        if (rayon .gt. 0.d0) then
            lsn=zr(jlnsv-1+(ino-1)+1)
            lst=zr(jltsv-1+(ino-1)+1)
            if (sqrt(lsn**2+lst**2) .le. rayon) enr2=2
        endif
!
!       ATTENTION, LE TRAITEMENT EVENTUEL DE NB_COUCHES N'EST PAS FAIT
!       ICI CAR ON NE CONNAIT PAS LA TAILLE DES MAILLES DU FOND DE FISS
!       CE TRAITEMENT SERA EFFECTUE APRES DANS XENRCH, DONC STANO SERA
!       PEUT ETRE MODIFIE
!
        enr=enr1+enr2
!
!       ENREGISTREMENT DU STATUT DU NOEUD
        zi(jstano-1+(ino-1)+1)=enr
!
200  end do
!
    call jedetr(lmafis)
!
    call jedema()
end subroutine
