subroutine feting(nbsd, sdfeti, chsecm, colaui, infofe,&
                  ifm, ilimpi, rang)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    - FONCTION REALISEE:  INITIALISE LA COLLECTION TEMPORAIRE COLAUI
!      EN CROISANT LES INFORMATIONS DU PROF_CHNO ET DE SD_FETI
!      SOIT V=COLAUI(NUM_SD) ET W=SDFETI.FETG(NUM_SD)
!      V(2*(I-1)+1)=NUMERO D'EQUATION DE LA PREMIERE COMPOSANTE DU
!                   NOEUD POINTE PAR W(2*(I-1)+2) DANS LE PROF_CHNO
!                  DU SOUS-DOMAINE NUM_SD
!      V(2*(I-1)+2)=SON NOMBRE DE COMPOSANTES
!
!     IN     NBSD  :  IN : NOMBRE DE SOUS-DOMAINES
!     IN     SDFETI: CH19: SD DECRIVANT LE PARTIONNEMENT FETI
!     IN    CHSECM : CH19: CHAM_NO SECOND MEMBRE GLOBAL POUR RECUPERER
!                          LES INFOS RELATIVES AUX PROF_CHNOS
!     IN/OUT COLAUI : CH24: NOM DE LA COLLECTION TEMPORAIRE
!     IN     ILIMPI : IN : ADRESSE JEVEUX DE L'OBJET PERMETTANT D'ASSO
!                          CIER DES SOUS-DOMAINES A UN PROCESSEUR
!     IN RANG     : IN  : RANG DU PROCESSEUR
!
! ATTENTION: ON SUPPOSE QUE LA RENUMEROTATION A L'ORIGINE DU PROF_CHNO
!            TRAVAILLE PAR NOEUD ET NON PAR DDL. C'EST-A-DIRE QUE LES
!            NUMERO D'EQUATIONS DES DDLS SE SUIVENT.
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/utimsd.h'
    integer :: nbsd, ifm, ilimpi, rang
    character(len=19) :: sdfeti, chsecm
    character(len=24) :: colaui, infofe
!
!
! DECLARATION VARIABLES LOCALES
    integer :: idd, ifetg, iaux1, nb, k, ifetc, ideeq, nbddl, irefe, iaux2
    integer :: nbddl1, iaux3, icmp, ieq, nbcmp, ifetb, icol, j, testa
    character(len=8) :: k8bid, nomsd
    character(len=19) :: chsmdd, prfchn
    character(len=24) :: ch24b, chdeeq, chfeta, chfetb
    logical :: first
!
! CORPS DU PROGRAMME
    call jemarq()
!
! INITIALISATION
    call jeveuo(chsecm//'.FETC', 'L', ifetc)
    ch24b=sdfeti//'.FETG'
    chfeta=sdfeti//'.FETA'
    chfetb=sdfeti//'.FETB'
    call jecrec(colaui, 'V V I', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsd)
!
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
    do 100 idd = 1, nbsd
        if (zi(ilimpi+idd) .eq. 1) then
!
            call jemarq()
! SECOND MEMBRE LOCAL AU SOUS-DOMAINE
            chsmdd=zk24(ifetc+idd-1)(1:19)
            call jeveuo(chsmdd//'.REFE', 'L', irefe)
! PROF_CHNO DU SOUS-DOMAINE IDD
            prfchn=zk24(irefe+1)(1:19)
!
! NOMBRE DE DDL DU SOUS-DOMAINE IDD
            chdeeq=prfchn//'.DEEQ'
            call jeveuo(chdeeq, 'L', ideeq)
            call jelira(chdeeq, 'LONMAX', nbddl, k8bid)
            nbddl1=(nbddl/2)-1
            call jenuno(jexnum(chfeta, idd), nomsd)
!
            call jeveuo(jexnom(ch24b, nomsd), 'L', ifetg)
            call jelira(jexnom(ch24b, nomsd), 'LONMAX', nb, k8bid)
!
! CREATION DE LA COLLECTION COLAUI
            call jecroc(jexnom(colaui, nomsd))
            call jeecra(jexnom(colaui, nomsd), 'LONMAX', nb, k8bid)
! NOMBRE D'ELEMENTS A RECHERCHER DANS .FETG
            nb=(nb/2)-1
! ZONE DE STOCKAGE DANS COLAUI
            call jeveuo(jexnom(colaui, nomsd), 'E', icol)
!
! STRUCTURE DE DONNEES LIEE AUX DDLS
            call jeveuo(jexnom(chfetb, nomsd), 'L', ifetb)
!----------------------------------------------------------------------
! BOUCLE SUR LES ELEMENTS DE SDFETI.FETG
!----------------------------------------------------------------------
            do 50 k = 0, nb
!
! NUMERO DU NOEUD DANS LE MAILLAGE
                iaux1=zi(ifetg+2*k+1)
                iaux1=abs(zi(ifetb+2*(iaux1-1)))
!
                first=.true.
                nbcmp=0
                ieq=0
!----------------------------------------------------------------------
! BOUCLE SUR LES ELEMENTS DE PROF_CHNO(IDD).DEEQ
!----------------------------------------------------------------------
                do 30 j = 0, nbddl1
! SON NUMERO DE NOEUD
                    iaux2=zi(ideeq+2*j)
!
! ON A TROUVE LE MEME NOEUD DU MAILLAGE QUE DANS .FETG
                    if (iaux2 .eq. iaux1) then
!
! SON NUMERO DE COMPOSANTE
                        iaux3=zi(ideeq+2*j+1)
!
                        if (iaux3 .gt. 0) then
! CE N'EST PAS UN NOEUD DE DUALISATION DE LAGRANGE
                            if (first) then
                                first=.false.
! PREMIER NUMERO DE COMPOSANTE TROUVE POUR LE NOEUD IAUX2
                                icmp=iaux3
! NUMERO D'EQUATION CORRESPONDANTE
                                ieq=j+1
                            endif
!
! TESTS DE VALIDITE POUR VERIFIER QUE LES COMPOSANTES SONT PAR ORDRE
! CROISSANT (RENUMEROTATION MD, MDA ET METIS)
                            call assert(iaux3.ge.icmp)
                            icmp=iaux3
                            nbcmp=nbcmp+1
                        endif
                    endif
!
30              continue
!
! TESTS DE COHERENCE
                testa=ieq*nbcmp
                call assert(testa.gt.0)
!
                zi(icol+2*k)=ieq
                zi(icol+2*k+1)=nbcmp
!
50          continue
            call jedema()
!
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
        endif
100  end do
!
! MONITORING
    if (infofe(1:1) .eq. 'T') write (ifm, *)'<FETI/FETING', rang, '> CREATION OBJET JEVEUX ',&
                              colaui(1:19)
    if (infofe(2:2) .eq. 'T') call utimsd(ifm, 2, .false., .true., colaui(1:19),&
                                          1, ' ')
    call jedema()
end subroutine
