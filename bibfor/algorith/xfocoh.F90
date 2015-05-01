subroutine xfocoh(jbas, jconx1, jconx2, jcoor, jfon,&
                  cnsln, chgrn, chgrt, noma, listpt, ndim,&
                  nfon, nxptff, orient, nbmai)
!
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/padist.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
#include "jeveux.h"
!
!   Calcul du front de propagation initial pour une
!   fissure XFEM cohésive (TYPE_DISCONTINUITE='COHESIF')
!
!   In/out jbas => adresse base en fond de fissure
!   In jconx1, jconx2 => adresse pour 
!                        tables de connectivité noeuds/mailles
!   In jcoor => adresse coordonnées des noeuds du maillages
!   In/out jfon => adresse coordonnées points du fond
!   In cnsln => cham_no_s level-set normale
!   In cnslt => cham_no_s level-set tangente
!   In chgrn => cham_no_s gradient LSN
!   In chgrt => cham_no_s gradient level-set tangente
!   In noma  => nom du maillage
!   In listpt=> liste pour tri des points du fond
!   In ndim => dimension
!   Out nfon => nombre de points du fond
!   Out nxptff => nombre max de points du fond
!   Out orient => peut-on orienter le fond?
!   Out nbmai => nombre de mailles renseugnées par l'utilisateur
!   
    real(kind=8) :: a(3), b(3), gln(2,3), glt(2,3), glna(3)
    real(kind=8) :: glnb(3), glta(3), gltb(3), beta
    integer ::  ima, indipt, ino, ipt, itypma
    integer :: j, jbas, jconx1, jconx2, jcoor
    integer :: jfon, jlism, k, ia, nbar, nuno_av
    integer ::  i_inter, ar(12,3)
    character(len=19) :: lismai, listpt, listpt_temp
    character(len=19) :: chgrn, chgrt, cnsln
    real(kind=8) :: loncar, m(2,3), mm(3)
    integer :: nbmai, nbnoma, nbptma, ndim, ndime, nfon, nmaabs
    character(len=8) :: noma
    integer :: nuno, nuno1, nuno2, nxptff, ima_eff, ninter
    aster_logical :: orient
    real(kind=8) :: p(3), prec, lsna, lsnb, autre_lsn
    real(kind=8) :: lsn_precedente
    character(len=8) :: typma
    real(kind=8), pointer :: gn(:) => null()
    real(kind=8), pointer :: gt(:) => null()
    real(kind=8), pointer :: lsnv(:) => null()
    integer, pointer :: typmail(:) => null()
    integer, pointer :: tab_pt(:) => null()
    integer, pointer :: tab_pt_temp(:) => null()
! ----------------------------------------------
    call jemarq()
!
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
    call jeveuo(chgrt//'.CNSV', 'L', vr=gt)
    call jeveuo(chgrn//'.CNSV', 'L', vr=gn)
    call jeveuo(cnsln//'.CNSV', 'L', vr=lsnv)
!
!   Recuperation mailles de la liste
    prec = 1.d-4
    lismai = '&&XPTFON.LISMAI'
    call reliem(' ', noma, 'NU_MAILLE', 'DEFI_FISS', 1,&
                1, 'GROUP_MA_BORD', 'GROUP_MA', lismai, nbmai)
    call jeveuo(lismai, 'L', jlism)
    call wkvect(listpt, 'V V I', 2*nbmai, vi=tab_pt)
    ipt = 0
    ima_eff = 0
!
!   On boucle sur les mailles de la liste
    do 185 ima = 1, nbmai
!
!       On récupère les noeuds extrémités
        nmaabs = zi(jlism+nbmai-ima)
        itypma = typmail(nmaabs)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
        call dismoi('DIM_TOPO', typma, 'TYPE_MAILLE', repi=ndime)
!
!       Maille 3D: on ignore
        if(ndime.eq.3) goto 185
!
!       Cas recherche intersection avec mailles de bords
        if(ndime.eq.2) then
            call conare(typma, ar, nbar)
            ninter = 0
            autre_lsn = 0.d0
            nuno=0
            do ia=1,nbar
                nuno1 = ar(ia,1)
                nuno2 = ar(ia,2)
                lsna = lsnv(nuno1)
                lsnb = lsnv(nuno2)
                if(lsna.eq.0.d0.or.lsnb.eq.0.d0) then
                    lsn_precedente = autre_lsn
                    nuno_av = nuno
                    if(lsna.eq.0) then
                       nuno=nuno1
                       autre_lsn=lsnb
                    else if(lsnb.eq.0) then
                       nuno=nuno2
                       autre_lsn=lsna
                    endif
!
!                   Blindage cas un seul point en commun
                    if(lsn_precedente*autre_lsn.gt.0.d0) goto 188
!
!                   Noeud confondu deja vu
                    if(nuno_av.eq.nuno) goto 188
                    ninter = ninter+1
!
                    do j = 1, ndim
                        gln(ninter,j) = gn(ndim*(nuno-1)+j)
                        glt(ninter,j) = gt(ndim*(nuno-1)+j)
                        m(ninter,j) = zr(jcoor-1+3*(nuno-1)+j)
                    end do
!
                else if(lsna*lsnb.lt.0.d0) then
                    ninter = ninter+1
                    beta = lsna/(lsnb-lsna)
                    do j = 1, ndim
                        a(j)=zr(jcoor-1+3*(nuno1-1)+j)
                        b(j)=zr(jcoor-1+3*(nuno2-1)+j)
                        glna(j)=gn(ndim*(nuno1-1)+j)
                        glnb(j)=gn(ndim*(nuno2-1)+j)
                        glta(j)=gt(ndim*(nuno1-1)+j)
                        gltb(j)=gt(ndim*(nuno2-1)+j)
                    end do
!
                    do j = 1, ndim
                        m(ninter,j)=a(j)-beta*(b(j)-a(j))
                        gln(ninter,j) = glna(j)-beta*(glnb(j)-glna(j))
                        glt(ninter,j) = glta(j)-beta*(gltb(j)-glta(j))
                    end do
!
                endif
188             continue
            end do
            if(ninter.lt.2) goto 185
        endif
!
!       Cas où on donne directement les mailles 1D
        if (ndime .eq. 1) then
            nbnoma = zi(jconx2+nmaabs) - zi(jconx2+nmaabs-1)
!
!           On proscrit les elements quadratiques pour l'instant
            ASSERT(nbnoma .eq. 2)
!
!           Initialisation compteur local nombre de nouveaux points
            nbptma = 0
!
            nuno1=zi(jconx1-1+zi(jconx2+nmaabs-1)+1-1)
            nuno2=zi(jconx1-1+zi(jconx2+nmaabs-1)+2-1)
            do j = 1, ndim
                a(j)=zr(jcoor-1+3*(nuno1-1)+j)
                b(j)=zr(jcoor-1+3*(nuno2-1)+j)
            end do
            loncar = padist(ndim,a,b)
!
            do ino = 1,2
!
                nuno=zi(jconx1-1+zi(jconx2+nmaabs-1)+ino-1)
!
!
                do j = 1, ndim
                    gln(ino,j) = gn(ndim*(nuno-1)+j)
                    glt(ino,j) = gt(ndim*(nuno-1)+j)
                    m(ino,j) = zr(jcoor-1+3*(nuno-1)+j)
                end do
            end do
        endif
!
        ima_eff=ima_eff+1
!
!           VERIFICATION SI CE POINT A DEJA ETE TROUVE
        do i_inter=1,2
!
            do j=1,ndim
                mm(j)=m(i_inter,j)
            end do
!
            do j = 1, ipt
                p(1) = zr(jfon-1+11*(j-1)+1)
                p(2) = zr(jfon-1+11*(j-1)+2)
                p(3) = zr(jfon-1+11*(j-1)+3)
!
                if (padist(ndim,p,mm) .lt. (loncar*prec)) then
!               alors le point a déjà été trouvé, on renvoie son in
                    indipt = j
                    goto 402
                endif
            end do
!
!           CE POINT N'A PAS DEJA ETE TROUVE, ON LE GARDE
            ipt = ipt+1
!           AUGMENTER NXPTFF
            ASSERT(ipt.le.nxptff)
!           STOCKAGE DES COORDONNEES DU POINT M
!           ET DE LA BASE LOCALE (GRADIENT DE LSN ET LST)
            do 230 k = 1, ndim
                zr(jfon-1+11*(ipt-1)+k) = m(i_inter,k)
                zr(jbas-1+2*ndim*(ipt-1)+k) = gln(i_inter,k)
                zr(jbas-1+2*ndim*(ipt-1)+k+ndim)= glt(i_inter,k)
230          continue
!
            indipt = ipt
402          continue
!
!           N.B. Pas de redressement de la base initiale aux bords
            if (nbptma .eq. 0) then
!               à présent, on va remplir la liste LISTP
!               il faudra mettre sa création plus en amont
                tab_pt(2*(ima_eff-1)+1) = indipt
                nbptma = nbptma+1
            elseif ((nbptma.eq.1) .and. (indipt.ne.tab_pt(2*(ima_eff-1)+1))) then
                tab_pt(2*(ima_eff-1)+2) = indipt
                nbptma = nbptma+1
            endif
!
         end do
!
185  continue
!
!
    if(ima_eff.lt.nbmai) then
        listpt_temp = '&&XFOCOH.LISTEM'
        nbmai = ima_eff
        call wkvect(listpt_temp, 'V V I', 2*nbmai, vi=tab_pt_temp)
        do ima=1,nbmai
            tab_pt_temp(2*(ima-1)+1) = tab_pt(2*(ima-1)+1)
            tab_pt_temp(2*(ima-1)+2) = tab_pt(2*(ima-1)+2)
        end do
        call jedetr(listpt)
        call wkvect(listpt, 'V V I', 2*nbmai, vi=tab_pt)
        do ima=1,nbmai
            tab_pt(2*(ima-1)+1) = tab_pt_temp(2*(ima-1)+1)
            tab_pt(2*(ima-1)+2) = tab_pt_temp(2*(ima-1)+2)
        end do
    endif

!   nombre de points du fond trouves
    nfon = ipt
!   on met orient à .TRUE. pour les tests de xenrch à suivre
    orient=.true.
!
    call jedetr(lismai)
!
    call jedema()
end subroutine
