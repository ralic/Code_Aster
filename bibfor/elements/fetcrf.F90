subroutine fetcrf(sdpart1,nomo,nbsd)
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
! person_in_charge: jacques.pellet at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CREATION DE LA STRUCTURE DE DONNEES SD_PART1.
!----------------------------------------------------------------------
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/gmgnre.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/jxveri.h"
#include "asterfort/lxcadr.h"
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8),intent(in) :: sdpart1,nomo
    integer,intent(in) :: nbsd
!
!
! DECLARATION VARIABLES LOCALES
    integer :: nbno, lstgma, nomsd, jadr, i
    integer :: j, intbuf, nec30
    integer :: nbnot2, nbnoto, ialino, jtrav, ianbno
    integer :: nbmail, ialima, nbnosd, nb, ianbma, is9, incrs, l, xt
    integer :: yt, zt, k, nbma, linoma,  jprnm
    integer :: nec, n, ino, ialsk, ialspo, ipos, jtmp
    integer ::  nbmato
    integer :: nbmama
    integer ::  nbmatr
    integer :: vali(5), iafeta, ial, ials, itma, nber, lil, lils
    integer :: iao, iaos, iexi
    character(len=8) :: k8bid, ma
    character(len=19) :: sdpart, ligrmo
    character(len=24) :: nomsda, nomsdm

    character(len=24) ::  nomgma, nomref
    character(len=24) ::  k24buf
    character(len=8), pointer :: lgrf(:) => null()
!
!
! CORPS DU PROGRAMME
    call jemarq()
!
!**********************************************************************
! INITIALISATIONS
!**********************************************************************
    sdpart=sdpart1
    nomref=sdpart//'.FREF'
    nomsdm=sdpart//'.FDIM'
    nomsda=sdpart//'.FETA'
!
!
!   ligrel du modele
    ligrmo = nomo(1:8)//'.MODELE'


!     VECTEUR DES NBRE DE NOEUDS
    call wkvect('&&FETCRF.NBNO     ', 'V V I', nbsd, nbno)
!
!     LISTES DES GROUP_MA
    call wkvect('&&FETCRF.LSTGMA   ', 'V V K24', nbsd, lstgma)
    call wkvect('&&FETCRF.NOMSD    ', 'V V K24', nbsd, nomsd)
!
!     CREATION .FREF
    call wkvect(nomref, 'G V K8', 1, jadr)
    zk8(jadr)=nomo

!     MA: MAILLAGE ASSOCIE AU MODELE
    call jeveuo(ligrmo//'.LGRF', 'L', vk8=lgrf)
    ma = lgrf(1)

    call dismoi('NB_NO_MAILLA', ma, 'MAILLAGE', repi=nbnoto)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbmato)
    nbmatr=nbmato
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
    call jelira(ligrmo//'.PRNM', 'LONMAX', n)
    nec = n/nbnoto
    nec30=30*nec

!   -- noms des group_ma crees dans la routine creagm.F90 :
    do i = 1, nbsd
        write(k8bid,'(I4)') i-1
        call lxcadr(k8bid)
        zk24(lstgma-1+i)= 'SD'//k8bid
    end do

!   noms des sous-domaines : "SDD" + on ajoute le numero
    do i = 1, nbsd
        write(k8bid,'(I4)') i
        call lxcadr(k8bid)
        zk24(nomsd-1+i) = 'SDD'//k8bid
    end do

!
!     VECTEUR DES NBRE DE NOEUDS
    call wkvect('&&FETCRF.NB_NO    ', 'V V I', nbsd, ianbno)
!     VECTEUR DES NBRE DE MAILLES
    call wkvect('&&FETCRF.NB_MA    ', 'V V I', nbsd, ianbma)
!     STOCKAGE DES ADRESSES DES .FETA DE CHAQUE SD
    call wkvect('&&FETCRF.ADR_FETA', 'V V I', nbsd, iafeta)
!     VECTEUR DES DDL CUMULES DE CHAQUE SD
    call wkvect('&&FETCRF.L_K      ', 'V V I', nbsd, ialsk)
!
!***********************************************************************
! BOUCLE 1 SUR LES SOUS-DOMAINES POUR DETERMINER:
! PAR SD: NBRE DE MAILLES (NBMAIL), ADDRESSE GROUP_MA (IALIMA),
!         NB DE NOEUDS (IANBNO)
! NBRE TOTAL DE MAILLES VOLUMIQUES (NBMATO)/NOEUDS(NB) AVEC MULTIPLICITE
!***********************************************************************
!
    nbmato=0
    nb=0
    nbnot2=2*nbnoto
    call wkvect('&&FETCRF.TRAV ', 'V V I', nbnot2, jtrav)
    call wkvect('&&FETCRF.TMP  ', 'V V I', nbnot2, jtmp)
    do i = 1, nbsd
        nomgma=zk24(lstgma-1+i)
        call jeexin(jexnom('&&FETCRF.GROUPEMA', nomgma), iexi)
        if (iexi.gt.0) then
            call jelira(jexnom('&&FETCRF.GROUPEMA', nomgma), 'LONUTI', nbmail)
            call jeveuo(jexnom('&&FETCRF.GROUPEMA', nomgma), 'L', ialima)
        else
            call utmess('F','PARTITION1_2')
        endif
        zi(iafeta+i-1)=ialima
        zi(ianbma-1+i)=nbmail
        nbmato=nbmato+nbmail

!       gmgnre : donne la liste des noeuds ZI(JTMP) d'une liste de
!                mailles ZI(IALIMA)
!                sortie : liste des noeuds = ZI(JTMP)
!                         nombre de noeuds = ZI(IANBNO-1+I)
        call gmgnre(ma, nbnoto, zi(jtrav), zi(ialima), nbmail,&
                    zi(jtmp), nbnosd, 'TOUS')
        zi(ianbno-1+i)=nbnosd
        zi(nbno-1+i)=nbnosd
        nb=nb+nbnosd
    end do
!
! ****** ON PEUT MAINTENANT DIMENSIONNER EXACTEMENT:
!     LISTE DES NOEUDS AVEC MULTIPLICITES
    call wkvect('&&FETCRF.LISTE_NO ', 'V V I', nb, ialino)
!     NUMERO DE LA SD POUR UN NOEUD DONNE
    call wkvect('&&FETCRF.L_NO_GMA ', 'V V I', nb, linoma)
!     INDICE DANS CETTE SD POUR UN NOEUD DONNE
    call wkvect('&&FETCRF.L_NO_POS ', 'V V I', nb, ialspo)
!
!**********************************************************************
! BOUCLE 2 SUR LES SOUS-DOMAINES POUR DETERMINER:
! PAR SD: NBRE DE MAILLES (NBMAIL), DE NOEUDS (NBNOSD), DE DDLS (K)
!**********************************************************************
    nb=0
    do i = 1, nbsd
!       ON REFAIT UN COUP POUR CETTE FOIS REMPLIR IALINO
        nbmail=zi(ianbma-1+i)
        if (nbmail.eq.0) cycle
        ialima= zi(iafeta+i-1)
        call jerazo('&&FETCRF.TMP  ', nbnot2, 1)
        call gmgnre(ma, nbnoto, zi(jtrav), zi(ialima), nbmail,&
                    zi(jtmp), nbnosd, 'TOUS')
        k24buf=zk24(nomsd-1+i)
        intbuf=2*nbnosd
! **** SOUS-BOUCLE 2.1 SUR LES NOEUDS POUR DETERMINER LE NBRE DE DDLS
!      ET REMPLIR .FETB (SANS LES SIGNES DES INTERFACES)
        k=0
        do j = 1, nbnosd
            ino = zi(jtmp-1+j)
            do l = 1, nec30
                if (exisdg(zi(jprnm-1+nec*(ino-1)+1),l)) k=k+1
            end do
        end do
!       NB DDL TOTAL DU SOUS-DOMAINE I
        zi(ialsk-1+i)=k
! **** SOUS-BOUCLE 2.2 SUR LES NOEUDS POUR REMPLIR
!       VECTEUR DES NOEUDS :  ZI(IALINO-1+...NB)
!       VECTEUR DE CORRESPONDANCE NOEUD -> SD: ZI(LINOMA-1+...NB)
!       VECTEUR DE CORRESPONDANCE NOEUD -> POSITION DANS SD: ZI(IALSPO)
        ipos=1
        do j = 1, nbnosd
            zi(ialino+nb-1+j)= zi(jtmp-1+j)
            zi(linoma+nb-1+j)= i
            zi(ialspo+nb-1+j)= ipos
            ipos=ipos+1
        end do
        nb=nb+nbnosd
    end do
    call jedetr('&&FETCRF.TMP')
    call jedetr('&&FETCRF.TRAV')


!***********************************************************************
! BOUCLE 3 SUR LES SOUS-DOMAINES POUR REMPLIR .FETA
!          RAJOUT DES MAILLES SURFACIQUES A NBMATO
!***********************************************************************
    call wkvect('&&FETCRF.TESTMA ', 'V V I', nbmatr, itma)
    call jecrec(nomsda, 'G V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
    do i = 1, nbsd
        k24buf=zk24(nomsd-1+i)
        call jecroc(jexnom(nomsda, k24buf))

!       NB MAILLES/ADDRESSE DE LEURS NUMEROS
        nbmail=zi(ianbma-1+i)
        ialima=zi(iafeta+i-1)

!       NB MAILLES TOTALES DU SD I
!       REMPLISSAGE EFFECTIF DE FETA
        call jeecra(jexnom(nomsda, k24buf), 'LONMAX', max(nbmail,1))
        call jeveuo(jexnom(nomsda, k24buf), 'E', jadr)
!       MAILLES VOLUMIQUES
        do j = 1, nbmail
            ial=zi(ialima+j-1)
            zi(jadr+j-1)=ial
            zi(itma+ial-1)=1
        end do
    end do
!
! **** TEST NBRE DE MAILLES MODELE = SOMME DES MAILLES DES GROUP_MA
    call jelira(nomo//'.MAILLE', 'LONMAX', nbma)
    call jeveuo(nomo//'.MAILLE', 'L', ial)
    nbmama=0
    nber=0
    do i = 1, nbma
!       MAILLE DU MODELE
        if (zi(ial+i-1) .ne. 0) then
            nbmama=nbmama+1
!         MAILLE DANS UN SD OU PAS ?
            if (zi(itma-1+i) .eq. 0) nber=nber+1
        endif
    end do
    if (nbmama .ne. nbmato) then
        vali(1)=nbmama
        vali(2)=nbmato
        call utmess('F', 'ELEMENTS5_24', ni=2, vali=vali)
    endif
    if (nber .gt. 0) then
        call utmess('F', 'ELEMENTS5_28', si=nber)
    endif
    nber=0
    do i = 1, nbmato
!       MAILLE DANS UN SD
        if (zi(itma-1+i) .ne. 0) then
!         MAILLE DU MODELE ?
            if (zi(ial+i-1) .eq. 0) nber=nber+1
        endif
    end do
    if (nber .gt. 0) then
        call utmess('F', 'ELEMENTS5_29', si=nber)
    endif
    call jedetr('&&FETCRF.TESTMA ')

!
!**********************************************************************
! FIN .FETA ET .FETB (SAUF SIGNE) ET AFFICHAGE
!**********************************************************************
!
!
!**********************************************************************
! ON REORDONNE LA LISTE DE TOUS LES NOEUDS DES SD (ET LEUR SD)
!   (INSPIRE DU TRI A BULLE UTTRIF.F)
!   PARTIE A OPTIMISER (VOIR TRI PAR INSERTION)
!**********************************************************************
!
!        --- TRI BULLE ---
    if (nb .gt. 1) then
!         --- CHOIX DE L'INCREMENT ---
        incrs = 1
        is9 = nb / 9
 11     continue
        if (incrs .lt. is9) then
            incrs = 3*incrs+1
            goto 11
        endif
!         --- REMONTEE DES BULLES ---
 12     continue
        do j = incrs+1, nb
            l = j-incrs
 13         continue
            if (l .gt. 0) then
                ial=ialino-1+l
                ials=ial+incrs
                lil=linoma-1+l
                lils=lil+incrs
                iao=ialspo-1+l
                iaos=iao+incrs
                if (zi(ial) .gt. zi(ials)) then
!            --- PERMUTATION DES VALEURS ---
                    xt = zi(ial)
                    zi(ial) = zi(ials)
                    zi(ials) = xt
                    yt = zi(lil)
                    zi(lil) = zi(lils)
                    zi(lils) = yt
                    zt = zi(iao)
                    zi(iao) = zi(iaos)
                    zi(iaos) = zt
                    l = l - incrs
                    goto 13
                endif
            endif
        end do
        incrs = incrs/3
        if (incrs .ge. 1) goto 12
    endif
!
!
!
!***********************************************************************
! ETAPE 11 CREATION .FDIM
!***********************************************************************
!
    call wkvect(nomsdm, 'G V I', 1, jadr)
    zi(jadr)=nbsd
    zi(jadr+4)=nbnoto
!
!
!
!***********************************************************************
! ETAPE 15 DESTRUCTION OBJETS TEMPORAIRES
!***********************************************************************
    call jedetr('&&FETCRF.ADR_FETA')
    call jedetr('&&FETCRF.L_K')
    call jedetr('&&FETCRF.L_NO_GMA')
    call jedetr('&&FETCRF.L_NO_POS')
    call jedetr('&&FETCRF.LISTE_NO')
    call jedetr('&&FETCRF.LSTGMA')
    call jedetr('&&FETCRF.NB_MA')
    call jedetr('&&FETCRF.NB_NO')
    call jedetr('&&FETCRF.NBNO')
    call jedetr('&&FETCRF.NOMSD')
    call jedetr('&&FETCRF.TESTMA')
    call jedetr('&&FETCRF.TMP')
    call jedetr('&&FETCRF.TRAV')
    call jedetr('&&FETCRF.GROUPEMA')
    call jedema()
end subroutine
