subroutine ace_masse_repartie(nbocc, infdonn, grplmax, lmax, infcarte, nbdisc)
!
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
!
! --------------------------------------------------------------------------------------------------
!     AFFE_CARA_ELEM
!
!     MASSES RÉPARTIES
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
    use cara_elem_parameter_module
    use cara_elem_info_type
    use cara_elem_carte_type
    implicit none
    integer :: nbocc
    type (cara_elem_info) :: infdonn
    character(len=24) :: grplmax(*)
    integer :: lmax
    type (cara_elem_carte) :: infcarte(*)
    integer :: nbdisc
!
#include "jeveux.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/calcul_cara_maille.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/in_liste_entier.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/nocart.h"
#include "asterfort/affdis.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: iocc, ii, jj, kk, iret, nbgrp, nb, ldgm, nm, nb_mail_grp, nb_noeu_grp, ifm, irep
    integer :: ndim, appui, ltypmail, imail, ntopo, isym, iv
    integer :: nfct, compte_maille, nb_noeud_uniq, ll, ncmp, nbsurchpoi1
    integer :: ivr(4)
    real(kind=8) :: lamasse, valfongro, surfacetotale, zero(6)
    real(kind=8) :: eta, maillesurf(2), maillecdg(3)
    character(len= 8) :: typm, fongro, discretm, discretk
    character(len=24) :: magrma, connex

    integer :: jdc(3), jdv(3), dimcar
    integer :: jdcinf, jdvinf
    character(len=19) :: cart(3), cartdi, masse_type
    logical :: repartition, ok
!
    integer :: nbnoeu, nbmail
    character(len=8) :: noma
!
! --------------------------------------------------------------------------------------------------
    integer     ,pointer :: noeuds(:)       => null()
    real(kind=8),pointer :: coord(:)        => null()
!
    integer     ,pointer :: nummaisur(:)        => null()
    integer     ,pointer :: lstnumnoe(:)        => null()
    integer     ,pointer :: lstnummaipoi1(:)    => null()
    real(kind=8),pointer :: lstcoenoe(:)        => null()
! --------------------------------------------------------------------------------------------------
    integer           :: vmessi(3)
    character(len=24) :: vmessk(5)
! --------------------------------------------------------------------------------------------------
!
    if ( nbocc.eq.0 ) goto 999
!
    call jemarq()
!
    noma   = infdonn%maillage
    nbnoeu = infdonn%nbnoeu
    ndim   = infdonn%dimmod
    nbmail = infdonn%nbmail
    ivr(:) = infdonn%ivr(:)
!
!   Pour les discrets c'est obligatoirement du 2d ou 3d
    ASSERT( (ndim.eq.2).or.(ndim.eq.3) )
!   Dimension de l'appui
    if (ndim .eq. 2) then
        appui = 1
    else
!       Dimension pas encore déterminée
        appui = -1
    endif
!
!   Les cartes sont déjà construites : ace_crea_carte
    cartdi = infcarte(ACE_CAR_DINFO)%nom_carte
    jdcinf = infcarte(ACE_CAR_DINFO)%adr_cmp
    jdvinf = infcarte(ACE_CAR_DINFO)%adr_val
    dimcar = infcarte(ACE_CAR_DINFO)%nbr_cmp
!
    cart(1) = infcarte(ACE_CAR_DISCK)%nom_carte
    jdc(1)  = infcarte(ACE_CAR_DISCK)%adr_cmp
    jdv(1)  = infcarte(ACE_CAR_DISCK)%adr_val
!
    cart(2) = infcarte(ACE_CAR_DISCM)%nom_carte
    jdc(2)  = infcarte(ACE_CAR_DISCM)%adr_cmp
    jdv(2)  = infcarte(ACE_CAR_DISCM)%adr_val
!
    cart(3) = infcarte(ACE_CAR_DISCA)%nom_carte
    jdc(3)  = infcarte(ACE_CAR_DISCA)%adr_cmp
    jdv(3)  = infcarte(ACE_CAR_DISCA)%adr_val
!
    ifm = ivr(4)
!
    magrma = noma//'.GROUPEMA'
    connex = noma//'.CONNEX'
    call jeveuo(noma//'.TYPMAIL', 'L', ltypmail)
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=coord)
!
!   Comptage des mailles POI1 communes entre les différentes occurences
    if ( nbocc .ge. 2 ) then
        AS_ALLOCATE(vi=lstnummaipoi1, size=nbdisc)
        compte_maille = 0
!       Les mailles de la 1ère occurence
        call getvtx('MASS_REP','GROUP_MA_POI1', iocc=1, nbval=lmax, vect=grplmax, nbret=nbgrp)
!       on éclate les GROUP_MA en mailles
        do ii = 1, nbgrp
            call jelira(jexnom(magrma, grplmax(ii)), 'LONUTI', nb)
            call jeveuo(jexnom(magrma, grplmax(ii)), 'L', ldgm)
            ASSERT( nbdisc.ge.nb )
            do jj = ldgm, ldgm+nb-1
                compte_maille = compte_maille + 1
                lstnummaipoi1(compte_maille) = zi(jj)
            enddo
        enddo
!       Les mailles des autres occurences
        nbsurchpoi1 = 0
        do iocc = 2, nbocc
            call getvtx('MASS_REP','GROUP_MA_POI1',iocc=iocc,nbval=lmax,vect=grplmax,nbret=nbgrp)
!           on éclate les GROUP_MA en mailles
            do ii = 1, nbgrp
                call jelira(jexnom(magrma, grplmax(ii)), 'LONUTI', nb)
                call jeveuo(jexnom(magrma, grplmax(ii)), 'L', ldgm)
                do jj = ldgm, ldgm+nb-1
                    imail = zi(jj)
                    if ( in_liste_entier(imail, lstnummaipoi1(1:compte_maille)) ) then
                        nbsurchpoi1 = nbsurchpoi1 + 1
                    else
                        compte_maille = compte_maille + 1
                        ASSERT( nbdisc.ge.compte_maille )
                        lstnummaipoi1(compte_maille) = imail
                    endif
                enddo
            enddo
        enddo
        if ( nbsurchpoi1 .ne. 0 ) then
            vmessk(1) = 'MASS_REP'
            vmessi(1) = nbocc
            vmessi(2) = nbsurchpoi1
            call utmess('I', 'AFFECARAELEM_20',nk=1,valk=vmessk,ni=2,vali=vmessi)
        endif
        AS_DEALLOCATE(vi=lstnummaipoi1)
    endif
!
    do iocc = 1, nbocc
!       Pour les messages
        vmessi(1) = iocc
        vmessk(1) = 'MASS_REP'
!
        nb_mail_grp = 0
        nb_noeu_grp = 0
        call getvtx('MASS_REP', 'GROUP_MA', iocc=iocc, nbval=lmax, vect=grplmax, nbret=nbgrp)
!       on éclate les GROUP_MA en mailles pour déterminer les noeuds concernés
        do ii = 1, nbgrp
            call jelira(jexnom(magrma, grplmax(ii)), 'LONUTI', nb)
            call jeveuo(jexnom(magrma, grplmax(ii)), 'L', ldgm)
            nb_mail_grp = nb_mail_grp + nb
            do jj = ldgm, ldgm+nb-1
                imail = zi(jj)
                call jenuno(jexnum('&CATA.TM.NOMTM', zi(ltypmail-1+imail)), typm)
                call dismoi('DIM_TOPO', typm, 'TYPE_MAILLE', repi=ntopo)
!               La dimension de la première maille définit l'appui si pas encore déterminé
                if (appui .eq. -1) then
                    appui = ntopo
                    if ( (appui.ne.1).and.(appui.ne.2) ) then
                        vmessk(2) = grplmax(ii)
                        call jenuno(jexnum(noma//'.NOMMAI', imail), vmessk(3) )
                        vmessk(4) = typm
                        vmessi(2) = appui
                        call utmess('F', 'AFFECARAELEM_11',nk=4,valk=vmessk,ni=2,vali=vmessi)
                    endif
                endif
                if (ntopo .ne. appui) then
                    vmessk(2) = grplmax(ii)
                    call jenuno(jexnum(noma//'.NOMMAI', imail), vmessk(3) )
                    vmessk(4) = typm
                    vmessi(2) = appui
                    vmessi(3) = ntopo
                    call utmess('F', 'AFFECARAELEM_10',nk=4,valk=vmessk,ni=3,vali=vmessi)
                endif
!               Noeuds de la maille
                call jelira(jexnum(connex, imail), 'LONMAX', nm)
                call jeveuo(jexnum(connex, imail), 'L', vi=noeuds)
!               Si pas le bon nombre de noeuds
                if ( .not. in_liste_entier(nm,[2, 3, 4, 6, 7, 8, 9]) ) then
                    vmessk(2) = grplmax(ii)
                    call jenuno(jexnum(noma//'.NOMMAI', imail), vmessk(3) )
                    vmessk(4) = typm
                    vmessk(5) = '2 3 4 6 7 8 9'
                    vmessi(2) = nm
                    call utmess('F', 'AFFECARAELEM_13',nk=5,valk=vmessk,ni=2,vali=vmessi)
                endif
!               Tout semble ok
                nb_noeu_grp = nb_noeu_grp + nm
            enddo
        enddo
!
!       La masse à répartir
        call getvr8('MASS_REP', 'VALE', iocc=iocc, scal=lamasse)
!       Le type de masse : TOTALE LINEIQUE SURFACIQUE
        call getvtx('MASS_REP', 'TYPE', iocc=iocc, scal=masse_type)
!
        if ( appui.eq.2 .and. masse_type.eq.'LINEIQUE' ) then
            call utmess('F', 'AFFECARAELEM_17',nk=1,valk=vmessk,ni=1,vali=vmessi)
        endif
        if ( appui.eq.1 .and. masse_type.eq.'SURFACIQUE' ) then
            call utmess('F', 'AFFECARAELEM_18',nk=1,valk=vmessk,ni=1,vali=vmessi)
        endif
!
        if ( masse_type.eq.'TOTALE' ) then
            repartition = .true.
        else
            repartition = .false.
        endif
        call getvid('MASS_REP', 'FONC_MULT', iocc=iocc, scal=fongro,  nbret=nfct)
        ASSERT( nfct.eq.0 .or. nfct.eq.1 )
!
!       Numéro des mailles de surface, pour vérifier qu'il n'y a pas de doublon
        AS_ALLOCATE(vi=nummaisur, size=nb_mail_grp)
!       Numéro des noeuds de la surface, maille POI1, la masse pondérée
        AS_ALLOCATE(vi=lstnumnoe,     size=nb_noeu_grp)
        AS_ALLOCATE(vi=lstnummaipoi1, size=nb_noeu_grp)
        AS_ALLOCATE(vr=lstcoenoe,     size=nb_noeu_grp)
!
        lstnumnoe(:) = 0
        lstcoenoe(:) = 0.0
        nummaisur(:) = 0
!       Va permmettre de vérifier la bijectivité entre les noeuds de la surface et les POI1
!           Valeur négative pour ne pas être égale à un numéro de maille
        lstnummaipoi1(:) = -2
!
        nb_noeud_uniq = 0
        compte_maille = 0
        surfacetotale = 0.0
        do ii = 1, nbgrp
            call jelira(jexnom(magrma, grplmax(ii)), 'LONUTI', nb)
            call jeveuo(jexnom(magrma, grplmax(ii)), 'L', ldgm)
            do jj = ldgm, ldgm+nb-1
                imail = zi(jj)
!               Vérification que la maille n'est pas en double dans les groupes
                if ( in_liste_entier(imail, nummaisur(1:compte_maille)) ) then
                    vmessk(2) = grplmax(ii)
                    call jenuno(jexnum(noma//'.NOMMAI', imail), vmessk(3) )
                    call utmess('F', 'AFFECARAELEM_12',nk=3,valk=vmessk,ni=1,vali=vmessi)
                endif
!               Calcul sur la maille
                compte_maille = compte_maille + 1
                nummaisur(compte_maille) = imail
                call jelira(jexnum(connex, imail), 'LONMAX', nm)
                call jeveuo(jexnum(connex, imail), 'L', vi=noeuds)
                call calcul_cara_maille( coord, noeuds(1:nm), appui, maillesurf, maillecdg )
                surfacetotale = surfacetotale + maillesurf(1)
                valfongro = 1.0
                if ( nfct.eq.1 ) then
                    call fointe('F ', fongro, 3, ['X','Y','Z'], maillecdg, valfongro, iret)
                endif
                do kk = 1 , nm
                    ok = in_liste_entier(noeuds(kk), lstnumnoe(1:nb_noeud_uniq),indx=ll)
                    if ( .not. ok ) then
                        nb_noeud_uniq = nb_noeud_uniq + 1
                        lstnumnoe(nb_noeud_uniq) = noeuds(kk)
                        lstcoenoe(nb_noeud_uniq) = lamasse*maillesurf(2)*valfongro
                    else
                        lstcoenoe(ll) = lstcoenoe(ll) + lamasse*maillesurf(2)*valfongro
                    endif
                enddo
            enddo
        enddo
!
!       Doit-on diviser par la surface totale
        if ( repartition ) then
            do ll = 1 , nb_noeud_uniq
                lstcoenoe(ll) = lstcoenoe(ll)/surfacetotale
            enddo
        endif
!
!       on éclate les GROUP_MA_POI1 pour vérifier que les noeuds sont dans le GROUP_MA
        call getvtx('MASS_REP', 'GROUP_MA_POI1', iocc=iocc, nbval=lmax, vect=grplmax, nbret=nbgrp)
        do ii = 1, nbgrp
            call jelira(jexnom(magrma, grplmax(ii)), 'LONUTI', nb)
            call jeveuo(jexnom(magrma, grplmax(ii)), 'L', ldgm)
            do jj = ldgm, ldgm+nb-1
                call jelira(jexnum(connex, zi(jj)), 'LONMAX', nm)
                if ( nm.ne.1 ) then
                    vmessk(2) = grplmax(ii)
                    call jenuno(jexnum(noma//'.NOMMAI', zi(jj) ), vmessk(3) )
                    call utmess('F', 'AFFECARAELEM_16',nk=3,valk=vmessk,ni=1,vali=vmessi)
                endif
                call jeveuo(jexnum(connex, zi(jj)), 'L', vi=noeuds)
                ok = in_liste_entier(noeuds(1), lstnumnoe(1:nb_noeud_uniq),indx=kk)
!               Si le noeud POI1 n'est pas dans la liste des noeuds de la surface ==> Pas bien
                if ( .not. ok ) then
                    vmessk(2) = grplmax(ii)
                    call jenuno(jexnum(noma//'.NOMMAI', zi(jj) ), vmessk(3) )
                    call utmess('F', 'AFFECARAELEM_14',nk=3,valk=vmessk,ni=1,vali=vmessi)
                endif
!               Si on déjà mis une maille POI1 en face du noeud  ==> Pas bien
                if ( lstnummaipoi1(kk) .ne. -2 ) then
                    vmessk(2) = grplmax(ii)
                    call jenuno(jexnum(noma//'.NOMMAI', zi(jj) ), vmessk(3) )
                    call jenuno(jexnum(noma//'.NOMMAI', lstnummaipoi1(kk) ), vmessk(4) )
                    call jenuno(jexnum(noma//'.NOMNOE', noeuds(1) ), vmessk(5) )
                    call utmess('F', 'AFFECARAELEM_19',nk=5,valk=vmessk,ni=1,vali=vmessi)
                endif
                lstnummaipoi1(kk) = zi(jj)
            enddo
        enddo
!       La relation doit être bijective ==> on ne doit plus avoir lstnummaipoi1 = -2.
!           Tous les noeuds doivent avoir une maille POI1 en face
        do ll = 1 , nb_noeud_uniq
            if ( lstnummaipoi1(ll) .eq. -2 ) then
                call jenuno(jexnum(noma//'.NOMNOE', lstnumnoe(ll) ), vmessk(2) )
                call utmess('F', 'AFFECARAELEM_15',nk=2,valk=vmessk,ni=1,vali=vmessi)
            endif
        enddo
!
        AS_DEALLOCATE(vi=nummaisur)
!
        if ( ivr(3).eq.2 ) then
            write(ifm,100) iocc, surfacetotale
        endif
        lamasse = 0.0
!       Par défaut on est dans le repère global, matrices symétriques
        irep = 1; isym = 1; eta = 0.0; zero(:) = 0.0
        discretm = 'M_T_D_N'; discretk = 'K_T_D_N'
        do ii = 1 , nb_noeud_uniq
            iv = 1
            if ( ivr(3).eq.2 ) then
                call jenuno(jexnum(noma//'.NOMMAI', lstnummaipoi1(ii) ), vmessk(1) )
                call jenuno(jexnum(noma//'.NOMNOE', lstnumnoe(ii) ), vmessk(2) )
                write(ifm,110) vmessk(1), vmessk(2) ,lstcoenoe(ii)
            endif
            lamasse = lamasse + lstcoenoe(ii)
            call affdis(ndim, irep, eta, discretm, lstcoenoe(ii:), &
                        jdc, jdv, ivr, iv, ['K','M','A'], &
                        ncmp, ll, jdcinf, jdvinf, isym )
            call nocart(cart(ll), 3, ncmp,   mode='NUM', nma=1, limanu=[lstnummaipoi1(ii)])
            call nocart(cartdi,   3, dimcar, mode='NUM', nma=1, limanu=[lstnummaipoi1(ii)])
!           On met 0 sur les raideurs
            iv = 1
            call affdis(ndim, irep, eta, discretk, zero, &
                        jdc, jdv, ivr, iv, ['K','M','A'], &
                        ncmp, ll, jdcinf, jdvinf, isym )
            call nocart(cart(ll), 3, ncmp,   mode='NUM', nma=1, limanu=[lstnummaipoi1(ii)])
            call nocart(cartdi,   3, dimcar, mode='NUM', nma=1, limanu=[lstnummaipoi1(ii)])
        enddo
        if ( ivr(3).eq.2 ) then
            write(ifm,120) lamasse
        endif
!
        AS_DEALLOCATE(vi=lstnumnoe)
        AS_DEALLOCATE(vr=lstcoenoe)
        AS_DEALLOCATE(vi=lstnummaipoi1)
    enddo
!
!   Pour l'impression des valeurs affectées : Maille , Noeud , valeur
100 format(/,'OCCURRENCE : ',i4,'. SURFACE TOTALE : ',1pe12.5 )
110 format(  '   MAILLE : ',A8,'. NOEUD : ',A8, '. MASSE : ',1pe12.5)
120 format(  '   POUR CETTE OCCURRENCE. MASSE TOTALE : ',1pe12.5)
!
call jedema()
!
999 continue
end subroutine
