subroutine rvpar0(nomtab, mcf, nbpost)
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
! IN  NOMTAB  : NOM DE LA TABLE PRINCIPALE PRODUITE PAR LA COMMANDE
! IN  MCF     : MOT-CLE FACTEUR
! IN  NBPOST  : NOMBRE DE POST-TRAITEMENT A CONSIDERER
!     ------------------------------------------------------------------
!
!     INITIALISE LES TABLES DE POST_RELEVE_T
!
!     ------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterc/getvid.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juveca.h'
    include 'asterfort/rvpara.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    character(len=6) :: mcf
    character(len=8) :: nomtab
    integer :: nbpost
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'RVPAR0' )
!
    integer :: ifm, niv
    integer :: iocc, iaux
    integer :: nbtbmx, admemo, nbtabl
    integer :: nbresu
!
    character(len=8) :: k8b
    character(len=18) :: nomstr
    integer :: iarg
!     ------------------------------------------------------------------
!
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    if (niv .ge. 2) call u2mesk('I', 'POSTRELE_8', 1, nomtab)
!
    nomstr = '&&'//nompro//'.NOMPARAMS'
!
!====
! 2. ON REPERE LE NOMBRE DE TABLES QU'IL FAUDRA INITIALISER :
!====
!
! 2.1. ==> ON ALLOUE LE TABLEAU DE MEMORISATION POUR 20 TABLES.
!          S'IL S'AVERE QU'IL Y EN A BESOIN DE DAVANTAGE, ON ALLONGERA.
!
    nbtbmx = 20
    call wkvect(nomstr, 'G V K8', 2*nbtbmx, admemo)
!
! 2.2. ==> ON PARCOURT TOUTES LES ACTIONS DEMANDEES
!
    nbtabl = 0
!
    do 22 , iocc = 1, nbpost
!
! 2.2.1. ==> EST-CE UN RESULTAT ?
!
    call getvid(mcf, 'RESULTAT', iocc, iarg, 0,&
                k8b, nbresu)
!
!
! 2.2.2.1. ==> ON CHERCHE SI CETTE CONFIGURATION EST DEJA ENREGISTREE ;
!              SI OUI, ON PASSE AU PARAMETRE SUIVANT (GOTO 220)
!
    do 222 , iaux = 1 , nbtabl
    if (zk8(admemo+2*iaux-2) .eq. nomtab) goto 220
222  continue
!
! 2.2.2.2. ==> LA CONFIGURATION EST NOUVELLE
!              ON L'ENREGISTRE, EN ALLONGEANT EVENTUELLEMENT SI LA PLACE
!              RESERVEE EST TROP PETITE
!
    if (nbtabl .ge. nbtbmx) then
        nbtbmx = nbtbmx + 50
        iaux = 2*nbtbmx
        call juveca(nomstr, iaux)
        call jeveuo(nomstr, 'E', admemo)
    endif
    call jelira(nomstr, 'LONUTI', iaux, k8b)
    iaux = iaux + 2
    call jeecra(nomstr, 'LONUTI', iaux, k8b)
!
    nbtabl = nbtabl + 1
    zk8(admemo+2*nbtabl-2) = nomtab
!
    22 end do
!
220  continue
!
!====
! 3. ON INITIALISE LES TABLES DEMANDEES
!====
!
    do 30 , iaux = 1 , nbtabl
!
    nomtab = zk8(admemo+2*iaux-2)
    call rvpara(nomtab, mcf, nbpost)
!
    30 end do
!
!====
! 4. MENAGE
!====
!
    call jedetr(nomstr)
!
    call jedema()
!
end subroutine
