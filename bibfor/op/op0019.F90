subroutine op0019()
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!                O P E R A T E U R    AFFE_CARA_ELEM
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
    use cara_elem_module
    implicit none
!
! aslint: disable=W1502
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/aceaba.h"
#include "asterfort/aceaca.h"
#include "asterfort/aceaco.h"
#include "asterfort/aceadi.h"
#include "asterfort/aceagb.h"
#include "asterfort/aceama.h"
#include "asterfort/aceamb.h"
#include "asterfort/aceamr.h"
#include "asterfort/aceaor.h"
#include "asterfort/aceapc.h"
#include "asterfort/aceapf.h"
#include "asterfort/aceapo.h"
#include "asterfort/acearm.h"
#include "asterfort/acearp.h"
#include "asterfort/acecel.h"
#include "asterfort/aceinc.h"
#include "asterfort/acevba.h"
#include "asterfort/acevca.h"
#include "asterfort/acevco.h"
#include "asterfort/acevdi.h"
#include "asterfort/acevgb.h"
#include "asterfort/acevma.h"
#include "asterfort/acevmb.h"
#include "asterfort/acevmr.h"
#include "asterfort/acevor.h"
#include "asterfort/acevpc.h"
#include "asterfort/acevpf.h"
#include "asterfort/acevpo.h"
#include "asterfort/acevrm.h"
#include "asterfort/acevrp.h"
#include "asterfort/alcart.h"
#include "asterfort/calcul.h"
#include "asterfort/coqucf.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/pmfd00.h"
#include "asterfort/tecart.h"
#include "asterfort/utmess.h"
#include "asterfort/verima.h"
#include "asterfort/wkvect.h"
#include "asterfort/verif_affe.h"
#include "asterfort/detrsd_vide.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: nbtel, nbtel0
    parameter  (nbtel0=ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE + &
                       ACE_NB_BARRE + ACE_NB_MASSIF + ACE_NB_GRILLE + ACE_NB_MEMBRANE + &
                       ACE_NB_THHMM)
!
    integer             :: element_support_num(nbtel0)
    character(len=16)   :: element_support_nom(nbtel0)
    integer             :: element_support_typ(nbtel0)
    integer             :: nombre_type_element(ACE_NB_ELEMENT)

! --------------------------------------------------------------------------------------------------
    integer :: nbocc(ACE_NB_MCLEF)
    character(len=8) :: mclef_type
!
    integer :: ivr(3), nbcart, iret, jadr, ii
    integer :: nbver, nlm, nlg, lxc, lxo, nln, nlj, lxa
    integer :: lxb, lxm, lxpf, lxgb, lxmb, lmax, ifm, niv, lxp, nbvm
    integer :: lxd, nboccd, lxrp, noemaf, lxrm, noemf2, nbmail
    integer :: lxmr, noemf3
    integer :: npoutr, ncable, nbarre
    integer :: iclf, ioc, icle, ng
    integer :: depart, jdnm, ixnw
    aster_logical :: locaco, locagb, locamb
    character(len=8) :: ver(3), nomu, nomo, noma, lpain(3), lpaout(1)
    character(len=16) :: concep, cmd, mclef, k16bid
    character(len=19) :: cartcf, ligrmo, lchin(3), lchout(1)
    character(len=24) :: mlgnma, modnom, modnem, tmpncf
!
! --------------------------------------------------------------------------------------------------
    integer, pointer            :: zjdlm(:) => null()
    character(len=24), pointer  :: zjdls(:) => null()
! --------------------------------------------------------------------------------------------------
    call jemarq()
!   CALL ONERRF('ABORT', K16BID, IRET)
    iret=0
! --------------------------------------------------------------------------------------------------
!   Récupération des arguments de la commande
    call getres(nomu, concep, cmd)
! --------------------------------------------------------------------------------------------------
!   Modèle
    call getvid(' ', 'MODELE', scal=nomo, nbret=nbvm)
!   Enregistre le nom du modèle dans la SD de AFFE_CARA_ELEM
    call wkvect(nomu//'.MODELE', 'G V K8', 1, jadr)
    zk8(jadr) = nomo
!   Construction des noms jeveux du concept modèle
    modnom = nomo//'.MODELE    .LGRF'
    modnem = nomo//'.MODELE    .NEMA'
    call jeexin(modnem, ixnw)
    if ( ixnw .ne. 0 ) then
        call utmess('F', 'MODELISA5_20')
    endif
!   Récupération du nom du maillage associé
    call jeveuo(modnom, 'L', jdnm)
    noma = zk8(jdnm)
!   Construction des noms jeveux du concept maillage associé
    mlgnma = noma//'.NOMMAI'
!   Nombre de mailles du maillage
    call jelira(mlgnma, 'NOMMAX', nbmail)
!
! --------------------------------------------------------------------------------------------------
!   Vérifications supplémentaires de syntaxe
    call getvtx(' ', 'VERIF', nbval=2, vect=ver, nbret=nbver)
    ivr(:)=0
    if (nbver .gt. 0) then
        do ii = 1, nbver
            if (ver(ii) .eq. 'MAILLE  ') ivr(1) = 1
        enddo
    endif
!
! --------------------------------------------------------------------------------------------------
!   Occurence des mots clefs facteur
    do ii = 1, ACE_NB_MCLEF
        call getfac(ACE_MCLEF(ii), nbocc(ii))
    enddo
!
! --------------------------------------------------------------------------------------------------
!   Initialisation des éléments pouvant être affetés :
!       element_support_nom  element_support_typ
    nbtel = 0
    do ii = nbtel+1, nbtel+ACE_NB_POUTRE
        element_support_nom(ii) = ACE_EL_POUTRE(ii-nbtel)
        element_support_typ(ii) = ACE_NU_POUTRE
    enddo
    nbtel = nbtel + ACE_NB_POUTRE
!
    do ii = nbtel+1, nbtel+ACE_NB_DISCRET
        element_support_nom(ii) = ACE_EL_DISCRET(ii-nbtel)
        element_support_typ(ii) = ACE_NU_DISCRET
    enddo
    nbtel = nbtel + ACE_NB_DISCRET
!
    do ii = nbtel+1, nbtel+ACE_NB_COQUE
        element_support_nom(ii) = ACE_EL_COQUE(ii-nbtel)
        element_support_typ(ii) = ACE_NU_COQUE
    enddo
    nbtel = nbtel + ACE_NB_COQUE
!
    do ii = nbtel+1, nbtel+ACE_NB_CABLE
        element_support_nom(ii) = ACE_EL_CABLE(ii-nbtel)
        element_support_typ(ii) = ACE_NU_CABLE
    enddo
    nbtel = nbtel + ACE_NB_CABLE
!
    do ii = nbtel+1, nbtel+ACE_NB_BARRE
        element_support_nom(ii) = ACE_EL_BARRE(ii-nbtel)
        element_support_typ(ii) = ACE_NU_BARRE
    enddo
    nbtel = nbtel + ACE_NB_BARRE
!
    do ii = nbtel+1, nbtel+ACE_NB_MASSIF
        element_support_nom(ii) = ACE_EL_MASSIF(ii-nbtel)
        element_support_typ(ii) = ACE_NU_MASSIF
    enddo
    nbtel = nbtel + ACE_NB_MASSIF
!
    do ii = nbtel+1, nbtel+ACE_NB_GRILLE
        element_support_nom(ii) = ACE_EL_GRILLE(ii-nbtel)
        element_support_typ(ii) = ACE_NU_GRILLE
    enddo
    nbtel = nbtel + ACE_NB_GRILLE
!
    do ii = nbtel+1, nbtel+ACE_NB_MEMBRANE
        element_support_nom(ii) = ACE_EL_MEMBRANE(ii-nbtel)
        element_support_typ(ii) = ACE_NU_MEMBRANE
    enddo
    nbtel = nbtel + ACE_NB_MEMBRANE
!
    do ii = nbtel+1, nbtel+ACE_NB_THHMM
        element_support_nom(ii) = ACE_EL_THHMM(ii-nbtel)
        element_support_typ(ii) = ACE_NU_THHMM
    enddo
    nbtel = nbtel + ACE_NB_THHMM
    ASSERT( nbtel .eq. nbtel0 )
! --------------------------------------------------------------------------------------------------
!   Récuperation des numéros des types éléments
    do ii = 1, nbtel
        call jenonu(jexnom('&CATA.TE.NOMTE', element_support_nom(ii)), element_support_num(ii))
    enddo
!
! --------------------------------------------------------------------------------------------------
!   Vérification de l'existence des GROUP_MA et MAILLE déclarés
!       Après cette vérification il n'est plus nécessaire d'utiliser les routines
!           verima   getvem(getvtx+verima)
!   Comptage des GROUP_MA et MAILLE. Pour ne pas faire des ALLOCATE dans la boucle.
    lmax = 10
    do iclf = 1, ACE_NB_MCLEF
        do ioc = 1, nbocc(iclf)
            do icle = 1, ACE_NB_GRMA_MA
                ii = MCLEF_GRP_MA(icle + (iclf-1)*ACE_NB_GRMA_MA)
                if ( ii .ne. ACE_NOTHING ) then
                    mclef      = ACE_GRMA_MA( ii )
                    call getvtx(ACE_MCLEF(iclf), mclef, iocc=ioc, nbval=0, nbret=ng)
                    lmax = max(lmax,-ng)
                endif
            enddo
        enddo
    enddo
    ASSERT( lmax.ge. 0 )
!   Vérification
    if ( lmax .ge. 1 ) then
        AS_ALLOCATE(vk24=zjdls, size=lmax)
        do iclf = 1, ACE_NB_MCLEF
            do ioc = 1, nbocc(iclf)
                do icle = 1, ACE_NB_GRMA_MA
                    ii = MCLEF_GRP_MA(icle + (iclf-1)*ACE_NB_GRMA_MA)
                    if ( ii .ne. ACE_NOTHING ) then
                        mclef      = ACE_GRMA_MA( ii )
                        mclef_type = ACE_GRMA_TY( ii )
                        call getvtx(ACE_MCLEF(iclf), mclef, iocc=ioc, nbval=lmax, vect=zjdls,&
                                nbret=ng)
                        call verima(noma, zjdls, ng, mclef_type)
                    endif
                enddo
            enddo
        enddo
        AS_DEALLOCATE(vk24=zjdls)
    endif
!
! --------------------------------------------------------------------------------------------------
!   Vérification de la syntaxe pour :
!       ACE_CABLE   : éléments CABLE CABLE_POULIE
!                           call acevca(nbocc(ACE_CABLE), nlm, nlg, iret)
!
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS POUTRE
    lxp = 0
    if (nbocc(ACE_POUTRE) .ne. 0) then
        call acevpo(nbocc(ACE_POUTRE), nlm, nlg, iret)
        lxp = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS COQUE
    lxc = 0
    if (nbocc(ACE_COQUE) .ne. 0) then
        call acevco(nbocc(ACE_COQUE), nlm, nlg, iret)
        lxc = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ORIENTATIONS DES ELEMENTS
    lxo = 0
    if (nbocc(ACE_ORIENTATION) .ne. 0) then
        call acevor(nbocc(ACE_ORIENTATION), nlm, nlg, nln, nlj, iret)
        lxo = max(nlm,nln,nlj,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES POUTRES COURBES
    lxa = 0
    if (nbocc(ACE_DEFI_ARC) .ne. 0) then
        call acevpc(nbocc(ACE_DEFI_ARC), nlm, nlg, iret)
        lxa = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS BARRE
    lxb = 0
    if (nbocc(ACE_BARRE) .ne. 0) then
        call acevba(nbocc(ACE_BARRE), nlm, nlg, iret)
        lxb = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS MASSIF :
    lxm = 0
    if (nbocc(ACE_MASSIF) .ne. 0) then
        call acevma(nbocc(ACE_MASSIF), nlm, nlg)
        lxm = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS POUTRE_FLUI
    lxpf = 0
    if (nbocc(ACE_POUTRE_FLUI) .ne. 0) then
        if (nbocc(ACE_POUTRE) .eq. 0) then
            call utmess('F', 'MODELISA5_56')
        endif
        call acevpf(nbocc(ACE_POUTRE_FLUI), nlm, nlg)
        lxpf = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS "GRILLE"
    lxgb = 0
    if (nbocc(ACE_GRILLE) .ne. 0) then
        call acevgb(nbocc(ACE_GRILLE), nlm, nlg)
        lxgb = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS "MEMBRANE"
    lxmb = 0
    if (nbocc(ACE_MEMBRANE) .ne. 0) then
        call acevmb(nbocc(ACE_MEMBRANE), nlm, nlg)
        lxmb = max(nlm,nlg)
    endif
! --------------------------------------------------------------------------------------------------
!   LONGUEUR MAXIMUM D UNE LISTE DE MAILLE/NOEUD/GROUP_MA/GROUP_NO
    lmax = max(lmax,lxp,lxc,lxo,lxa,lxb,lxm,lxpf,lxgb,lxmb)
! --------------------------------------------------------------------------------------------------
!   RECUPERATION DU NIVEAU D'IMPRESSION
    call infmaj()
    call infniv(ifm, niv)
    if (niv .eq. 2) ivr(3) = 1
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA SYNTAXE DES ELEMENTS DISCRET
    lxd = 0
    if (nbocc(ACE_DISCRET) .ne. 0 .or. nbocc(ACE_DISCRET_2D) .ne. 0) then
        nboccd = nbocc(ACE_DISCRET) + nbocc(ACE_DISCRET_2D)
        if (nbocc(ACE_DISCRET) .ne. 0)      k16bid = ACE_MCLEF(ACE_DISCRET)
        if (nbocc(ACE_DISCRET_2D) .ne. 0)   k16bid = ACE_MCLEF(ACE_DISCRET_2D)
        call acevdi(nboccd, noma, nomo, k16bid, nlm, nlg, nln, nlj, iret)
        lxd = max(nlm,nln,nlg,nlj)
        lmax = max(lmax,lxd)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA DIMENSION DES RAIDEURS REPARTIES
    lxrp = 0
    if (nbocc(ACE_RIGI_PARASOL) .ne. 0) then
        call acevrp(nbocc(ACE_RIGI_PARASOL), noma, lxrp, noemaf)
        lmax = max(lmax,lxrp)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA DIMENSION DES RAIDEURS MISS
    lxrm = 0
    if (nbocc(ACE_RIGI_MISS_3D) .ne. 0) then
        call acevrm(nbocc(ACE_RIGI_MISS_3D), noma, lxrm, noemf2)
        lmax = max(lmax,lxrm)
    endif
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA DIMENSION DES MASSES REPARTIES
    lxmr = 0
    if (nbocc(ACE_MASS_AJOU) .ne. 0) then
        call acevmr(nbocc(ACE_MASS_AJOU), noma, lxmr, noemf3)
        lmax = max(lmax,lxmr)
    endif
!
! --------------------------------------------------------------------------------------------------
!   Pour mémoriser les éléments affectés. Si traité zjdlm(i)=element_support_num sinon 0
    AS_ALLOCATE(vi=zjdlm, size=nbmail)
!
! --------------------------------------------------------------------------------------------------
!   COMPTEUR D'ELEMENTS ET VERIFICATION COHERENCE DES AFFECTATIONS
    call acecel(noma, nomo, nbocc, nbtel, element_support_num, element_support_typ, &
                nombre_type_element, zjdlm, iret)
!
    npoutr = nombre_type_element(ACE_NU_POUTRE)
    ncable = nombre_type_element(ACE_NU_CABLE)
    nbarre = nombre_type_element(ACE_NU_BARRE)
!
    if (iret .ne. 0) then
        call utmess('F', 'MODELISA5_57')
    endif
!
! --------------------------------------------------------------------------------------------------
!   VERIFICATION DE LA BONNE  AFFECTATION  DES  CARACTERISTIQUES
!     POUR TOUTES LES MAILLES ET NOEUDS AFFECTES , IMPR SI DEMANDE
!     INCREMENTATION DES COMPTEURS D APPELS A NOCART (DISCRET,COQUE,
!     DEFI_ARC,CABLE,POUTRE,BARRE)
    iret=0
    call aceinc(noma, nomo, ACE_NB_MCLEF, ACE_MCLEF, element_support_num,&
                nbocc, ivr, ACE_NB_POUTRE, ACE_NB_DISCRET, ACE_NB_COQUE,&
                ACE_NB_CABLE, ACE_NB_BARRE, ACE_NB_MASSIF, ACE_NB_GRILLE, ACE_NB_MEMBRANE, &
                nbtel, locaco, locagb, locamb, zjdlm,&
                lmax, iret)
    if (iret .ne. 0) then
        call utmess('F', 'MODELISA5_59')
    endif
!     FABRICATION DE LA CARTE COMMUNE A TOUS LES ELEMENTS LINEIQUE
!     S'IL Y EN A D'AFFECTE
    nbcart = 0
    if (nbocc(ACE_POUTRE) + nbocc(ACE_BARRE) + nbocc(ACE_CABLE) .ne. 0) then
        nbcart = npoutr + nbarre + ncable
        if (nbcart .gt. 0) then
            cartcf = nomu//'.CVENTCXF'
            call alcart('G', cartcf, noma, 'VENTCX_F')
        endif
    endif
    if ((nbocc(ACE_POUTRE).eq.0) .and. (npoutr.ne.0)) then
        call utmess('A', 'MODELISA5_60')
    endif
    if ((nbocc(ACE_BARRE).eq.0) .and. (nbarre.ne.0)) then
        call utmess('A', 'MODELISA5_61')
    endif
    if ((nbocc(ACE_CABLE).eq.0) .and. (ncable.ne.0)) then
        call utmess('A', 'MODELISA5_62')
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES ORIENTATIONS AUX ELEMENTS POUTRES ET DISCRETS  ET
!     BARRES ET AFFECTATION DE LA CARTE ORIENTATION
    if (nbocc(ACE_POUTRE).ne.0 .or. nbocc(ACE_DISCRET).ne.0 .or. nbocc(ACE_DISCRET_2D).ne.0 .or. &
        nbocc(ACE_BARRE).ne.0 .or.  nbocc(ACE_RIGI_PARASOL).ne.0) then
        call aceaor(noma, nomo, lmax, ACE_NB_POUTRE, nbtel, &
                    element_support_num, element_support_nom, ivr, ifm, nbocc)
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES CARACTERISTIQUES AUX ELEMENTS POUTRES
    if (nbocc(ACE_POUTRE) .ne. 0) then
!        NBEPO + NBEDI + NBECO + NBECA + NBEBA + NBEMA + NBEGB
        depart = 1
        call aceapo(noma, nomo, lmax, npoutr, nbocc(ACE_POUTRE),&
                    ACE_MCLEF(ACE_POUTRE), ACE_NB_POUTRE, &
                    element_support_num(depart), ivr, ifm, zjdlm)
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES EPAISSEURS/COURBURES/ANGLES AUX ELEMENTS COQUES
    if (nbocc(ACE_COQUE) .ne. 0) then
        call aceaco(nomu, noma, lmax, locagb, locamb, nbocc(ACE_COQUE))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES MATRICES AUX ELEMENTS DISCRETS
    if (nbocc(ACE_DISCRET) .ne. 0 .or. nbocc(ACE_DISCRET_2D) .ne. 0) then
        nboccd = nbocc(ACE_DISCRET) + nbocc(ACE_DISCRET_2D)
        if (nbocc(ACE_DISCRET) .ne. 0)    k16bid = ACE_MCLEF(ACE_DISCRET)
        if (nbocc(ACE_DISCRET_2D) .ne. 0) k16bid = ACE_MCLEF(ACE_DISCRET_2D)
        call aceadi(noma, nomo, k16bid, lmax, nboccd, ivr, ifm)
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES COURBURES AUX ELEMENTS POUTRES COURBES
    if (nbocc(ACE_DEFI_ARC) .ne. 0) then
        call aceapc(nomu, noma, lmax, nbocc(ACE_DEFI_ARC))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES SECTIONS AUX ELEMENTS CABLE :
    if (nbocc(ACE_CABLE) .ne. 0) then
        call aceaca(nomu, noma, lmax, nbocc(ACE_CABLE))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES CARACTERISTIQUES AUX ELEMENTS BARRE
    if (nbocc(ACE_BARRE) .ne. 0) then
!        NBEPO + NBEDI + NBECO + NBECA + NBEBA + NBEMA + NBEGB
        depart = ACE_NB_POUTRE + ACE_NB_DISCRET + ACE_NB_COQUE + ACE_NB_CABLE + 1
        call aceaba(noma, nomo, lmax, nbarre, nbocc(ACE_BARRE),&
                    ACE_MCLEF(ACE_BARRE), ACE_NB_BARRE, &
                    element_support_num(depart), ivr, ifm, zjdlm)
    endif

! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES REPERES AUX ELEMENTS THERMIQUES ET MECANIQUES
    if (nbocc(ACE_MASSIF) .ne. 0) then
        call aceama(nomu, noma, lmax, nbocc(ACE_MASSIF))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES REPERES AUX ELEMENTS POUTRE_FLUI
    if (nbocc(ACE_POUTRE_FLUI) .ne. 0) then
        call aceapf(nomu, noma, lmax, nbocc(ACE_POUTRE_FLUI))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES MATRICES AUX RAIDEURS REPARTIES
    if (nbocc(ACE_RIGI_PARASOL) .ne. 0) then
        call acearp(noma, nomo, lmax, noemaf, nbocc(ACE_RIGI_PARASOL), ivr, ifm)
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT "GRILLE"
    if (nbocc(ACE_GRILLE) .ne. 0) then
        call aceagb(nomu, noma, lmax, locamb, nbocc(ACE_GRILLE))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES MATRICES AUX RAIDEURS MISS
    if (nbocc(ACE_RIGI_MISS_3D) .ne. 0) then
        call acearm(noma, nomo, lmax, noemf2, nbocc(ACE_RIGI_MISS_3D), ivr, ifm)
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES CARACTERISTIQUES POUR L'ELEMENT "MEMBRANE"
    if (nbocc(ACE_MEMBRANE) .ne. 0) then
        call aceamb(nomu, noma, lmax, nbocc(ACE_MEMBRANE))
    endif
! --------------------------------------------------------------------------------------------------
!   AFFECTATION DES MATRICES AUX MASSES REPARTIES
    if (nbocc(ACE_MASS_AJOU) .ne. 0) then
        call aceamr(noma, nomo, lmax, noemf3, nbocc(ACE_MASS_AJOU), ivr, ifm)
    endif
! --------------------------------------------------------------------------------------------------
!   COMPACTAGE DE LA CARTE : '.CVENTCXF'
    if (nbcart .gt. 0) then
!        PAS APPELE POUR UNE SURCHARGE "FINE" MAIS POUR LE COMPACTAGE
        call tecart(cartcf)
!        DESTRUCTION DES CHAMPS
        tmpncf = cartcf//'.NCMP'
        call jedetr(tmpncf)
        tmpncf = cartcf//'.VALV'
        call jedetr(tmpncf)
    endif
!
!     POUR LES COQUES, GRILLES IL PEUT EXISTER UNE CARTE FONCTION
!     IL FAUT L'EVALUER ET METTRE LE RESULTAT DANS LA CARTE DES REELS
    if ((nbocc(ACE_COQUE).ne.0) .or. (nbocc(ACE_GRILLE).ne.0)) then
        call coqucf(nomu)
    endif
!
! --------------------------------------------------------------------------------------------------
!   TRAITEMENT DES MOTS CLES
!           MULTIFIBRE  /  GEOM_FIBRE
!           COQUE       /  COQUE_NCOU
!           GRILLE      /  COQUE_NCOU
!           MEMBRANE    /  COQUE_NCOU
!           POUTRE      /  TUYAU_NCOU
!           POUTRE      /  TUYAU_NSEC
    call pmfd00()
! --------------------------------------------------------------------------------------------------
!   APPEL DE L'OPTION DE VERIFICATION VERI_CARA_ELEM :
    lpain(1)='PCACOQU'
    lchin(1)=nomu//'.CARCOQUE'
    lpaout(1)='PBIDON'
    lchout(1)='&&OP0019.BIDON'
    ligrmo=nomo//'.MODELE'
    call calcul('C', 'VERI_CARA_ELEM', ligrmo, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V', 'OUI')
! --------------------------------------------------------------------------------------------------
!   Certaines cartes peuvent etre vides : il faut les detruire :
    call detrsd_vide('CARTE',nomu//'.CARDISCA')


! - Audit assignments :
    call verif_affe(modele=nomo,sd=nomu)
!
    AS_DEALLOCATE(vi=zjdlm)
!
    call jedema()
end subroutine
