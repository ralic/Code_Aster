subroutine adalig_sd(ligr,part8,ntliel,nbtype,clas,teut,nteut)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jevtbl.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/sdpart.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=19), intent(in) :: ligr
    character(len=8), intent(in) :: part8
    character(len=24),intent(in) :: ntliel
    integer, intent(in) :: nbtype
    character(len=1), intent(in) :: clas
    integer, pointer, intent(in) :: nteut(:)
    integer, pointer, intent(in) :: teut(:)
!----------------------------------------------------------------------
! But: Reorganiser la collection .LIEL de ligrz afin de regrouper
!      les elements de meme TYPE_ELEM dans un meme GREL
!      et en respectant la partition part8.
!
! De plus, on veut :
!   * Limiter la taille des GRELS (pas plus de nelmx elements)
!   * Faire en sorte que les GRELS respectent PARTITION=part8 :
!     (tous les elements d'un GREL appartiennent aux sous-domaines traites par le
!      processeur qui traitera ce GREL)
!     * Pour chaque TYPE_ELEM :
!       * On decoupe le paquet d'elements en un nombre de grels multiple de nbproc.
!       * le GREL kgrel ne contient que des elements des sous-domaines affectes au
!           processeur kproc [0, ..., nbproc-1] avec : mod(kgrel,nbproc)=kproc
!
!    Remarques :
!      * L'equilibrage n'est pas toujours optimal : cela depend de la "qualite" de
!        la partition. Il est preferable que nbSD soit un multiple de nbproc,
!        mais ca ne suffit pas pour assurer un equlibrage "parfait"
!      * Le desequilibrage conduit parfois a creer des GRELS vides.
!
!
! Arguments d'entree:
!     ligrz  (o) : nom du ligrel
!     part9 (o) : nom de la sd_partition
!----------------------------------------------------------------------
    character(len=24) :: liel
    character(len=19) :: part19
    character(len=8) :: noma
    integer :: i, jliel
    integer ::  igrel, itype, nbma,ksd, kproc
    integer :: nbel, nbgrel, kgre1, nbgre1
    integer :: nbelmx, npaq, nbelgr, nel1, nel2, nel3, decal
    integer :: ktype, lont, nbgrel_av, rang, nbproc
    integer :: nbsd, igr, ktyp1, ktyp2, iel, numa, ngrmx, ngr1, ipaq
    integer, pointer :: traite_par(:) => null()
    integer, pointer :: nbel1(:) => null()
    integer, pointer :: gteut(:) => null()
    integer, pointer :: nbelgrel(:) => null()
    integer, pointer :: utilise(:) => null()
    integer, pointer :: utilise_1(:) => null()
    integer, pointer :: utilise_2(:) => null()
    integer, pointer :: ordre_stockage(:) => null()
    mpi_int :: mrank, msize

    integer, pointer :: feta(:) => null()
    integer, pointer :: tliel(:) => null()
    integer, pointer :: lctliel(:) => null()
    integer, pointer :: partsd(:) => null()
#define numail(igr,iel) tliel(lctliel(igr)+iel-1)
!----------------------------------------------------------------------

    call jemarq()

    liel=ligr//'.LIEL'
    part19=part8
    call dismoi('NOM_MAILLA', ligr, 'LIGREL', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call jelira(ntliel, 'NMAXOC', nbgrel_av)
    call jeveuo(ntliel, 'L', vi=tliel)
    call jeveuo(jexatr(ntliel, 'LONCUM'), 'L', vi=lctliel)
    call jelira(part19//'.FETA', 'NMAXOC', nbsd)

    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)


!   -- Calcul du vecteur traite_par:
!      traite_par(ima) = kproc
!   -----------------------------------------------
!   -- on repartit les sous-domaines comme on le fait pour PARALLELISME='SOUS_DOMAINE' :
    call wkvect('&&ADALIG_SD.PART.SD', 'V V I', nbsd, vi=partsd)
    call sdpart(nbsd, 0, partsd)

    AS_ALLOCATE(vi=traite_par, size=nbma)
    traite_par(:)=-99
    do ksd=1,nbsd
        if (partsd(ksd) .eq. 1) then
            call jeveuo(jexnum(part19//'.FETA',ksd), 'L', vi=feta)
            if (rang.eq.0) then
                do i =1, size(feta)
                    traite_par(feta(i))=nbproc-1
                enddo
            else
                do i =1, size(feta)
                    traite_par(feta(i))=rang-1
                enddo
            endif
        endif
    enddo
    call asmpi_comm_vect('MPI_MAX', 'I', nbval=nbma, vi=traite_par)
    call jedetr('&&ADALIG_SD.PART.SD')


!   -- Calcul du vecteur nbel1:
!      nbel1((ktype-1)*nbproc+kproc+1) = nel
!      nel est le nombre d'elements de type ktype qui seront
!      calcules par le proc kproc
!   -----------------------------------------------
    AS_ALLOCATE(vi=nbel1, size=nbtype*nbproc)
    nbel1(:)=0
    do igr=1,nbgrel_av
        nbelgr=lctliel(igr+1)-lctliel(igr) -1
        ktyp1=tliel(lctliel(igr)-1+nbelgr+1)
        ktype=0
        do ktyp2=1,nbtype
            if (ktyp1.eq.teut(ktyp2)) then
                ktype=ktyp2
                exit
            endif
        enddo
        ASSERT(ktype.gt.0)
        do iel =1, nbelgr
           numa = numail(igr,iel)
           kproc=traite_par(numa)
           ASSERT(kproc.ge.0)
           nbel1((ktype-1)*nbproc+kproc+1)=nbel1((ktype-1)*nbproc+kproc+1)+1
        enddo
    enddo


!   -- Calcul du nombre de grels du nouveau .LIEL
!      et de la dimension totale de la collection
!   -----------------------------------------------
!   gteut : nombre de grels du ligrel (par type_elem)
    AS_ALLOCATE(vi=gteut, size=nbtype)
    lont = 0
    nbgrel = 0
    nbelmx = int(jevtbl('TAILLE_GROUP_ELEM'))
    do ktype = 1, nbtype
        ngrmx=0
        do kproc = 0, nbproc-1
           nel1=nbel1((ktype-1)*nbproc+kproc+1)
           ngr1=nel1/nbelmx
           if (mod(nel1,nbelmx).gt.0) ngr1=ngr1+1
           ngrmx=max(ngrmx,ngr1)
        enddo
        nbgre1 = nbproc*ngrmx
        gteut(ktype) = nbgre1
        nbgrel = nbgrel + nbgre1
        nbel = nteut(ktype)
        lont = lont + nbel + nbgre1
    end do


!   -- Calcul du nombre d'elements des GRELS du nouveau .LIEL
!   -------------------------------------------------------------
    AS_ALLOCATE(vi=nbelgrel, size=nbgrel)
    igrel=0
    do ktype = 1, nbtype
        nbgre1=gteut(ktype)
        npaq=nbgre1/nbproc
        do ipaq = 1, npaq
            do kproc = 0, nbproc-1
               igrel=igrel+1
               nel1=nbel1((ktype-1)*nbproc+kproc+1)
               nel2= ipaq*nbelmx
               nel3= (ipaq-1)*nbelmx
               if (nel2.le.nel1) then
                   nbelgrel(igrel)=nbelmx
               else
                   if (nel3.gt.nel1) then
                       nbelgrel(igrel)=0
                   else
                       nbelgrel(igrel)=nel1-nel3
                   endif
               endif
            enddo
        enddo
    end do


!   -- Allocation du nouveau .LIEL
!   ------------------------------------
    call jecrec(liel, clas//' V I', 'NU', 'CONTIG', 'VARIABLE', nbgrel)
    call jeecra(liel, 'LONT', lont)
    igrel=0
    do ktype = 1, nbtype
        itype = teut(ktype)
        nbgre1 = gteut(ktype)
        do kgre1 = 1, nbgre1
            igrel=igrel+1
            nbelgr=nbelgrel(igrel)
            call jecroc(jexnum(liel, igrel))
            call jeecra(jexnum(liel, igrel), 'LONMAX', nbelgr+1)
            call jeveuo(jexnum(liel, igrel), 'E', jliel)
            zi(jliel+nbelgr) = itype
        end do
    end do
    ASSERT(nbgrel.eq.igrel)


!   -- Remplissage des nouveaux GREL
!   ----------------------------------
    AS_ALLOCATE(vi=ordre_stockage, size=nbma)
    AS_ALLOCATE(vi=utilise, size=nbproc)
    AS_ALLOCATE(vi=utilise_2, size=nbproc+1)
    AS_ALLOCATE(vi=utilise_1, size=nbproc)
    igrel=0
    do ktyp2 = 1, nbtype
        nbgre1 = gteut(ktyp2)
        npaq=nbgre1/nbproc

!       -- on remplit des objets qui facilitent le remplissage des grels :
        utilise(:)=0
        do igr=1,nbgrel_av
            nbelgr=lctliel(igr+1)-lctliel(igr) -1
            ktyp1=tliel(lctliel(igr)-1+nbelgr+1)
            if (ktyp1.ne.teut(ktyp2)) cycle
            do iel=1,nbelgr
                numa = numail(igr,iel)
                kproc=traite_par(numa)
                utilise(kproc+1)=utilise(kproc+1)+1
            enddo
        enddo
        utilise_2(1)=0
        do kproc=0,nbproc-1
            utilise_2(kproc+2)=utilise_2(kproc+1)+utilise(kproc+1)
        enddo

        ordre_stockage(:)=0
        utilise_1(:)=0
        do igr=1,nbgrel_av
            nbelgr=lctliel(igr+1)-lctliel(igr) -1
            ktyp1=tliel(lctliel(igr)-1+nbelgr+1)
            if (ktyp1.ne.teut(ktyp2)) cycle
            do iel=1,nbelgr
                numa = numail(igr,iel)
                kproc=traite_par(numa)
                utilise_1(kproc+1)=utilise_1(kproc+1)+1
                ordre_stockage(utilise_2(kproc+1)+utilise_1(kproc+1))=numa
            enddo
        enddo



        do ipaq = 1, npaq
            do kproc = 0, nbproc-1
               igrel=igrel+1
               call jeveuo(jexnum(liel, igrel), 'E', jliel)

               nbelgr=nbelgrel(igrel)
               decal=max(0,(ipaq-1)*nbelmx)
               do iel=1, nbelgr
                   numa=ordre_stockage(utilise_2(kproc+1)+decal+iel)
                   zi(jliel-1+iel)=numa
               enddo
            enddo
        enddo
    enddo
    ASSERT(nbgrel.eq.igrel)


    AS_DEALLOCATE(vi=traite_par)
    AS_DEALLOCATE(vi=nbel1)
    AS_DEALLOCATE(vi=gteut)
    AS_DEALLOCATE(vi=nbelgrel)
    AS_DEALLOCATE(vi=ordre_stockage)
    AS_DEALLOCATE(vi=utilise)
    AS_DEALLOCATE(vi=utilise_1)
    AS_DEALLOCATE(vi=utilise_2)

    call jedema()
end subroutine
