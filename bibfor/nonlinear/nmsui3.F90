subroutine nmsui3(sdimpr, typcha, nbma, nbno, nbpi,&
                  nbspi, nbcmp, extrch, extrcp, extrga,&
                  listma, chnoeu, chelga, champ, isuiv)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/celces.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nmsuiy.h'
    include 'asterfort/sdmpic.h'
    integer :: nbcmp, nbno, nbma
    integer :: nbpi, nbspi
    character(len=4) :: typcha
    character(len=24) :: sdimpr
    character(len=8) :: extrch, extrcp, extrga
    character(len=19) :: champ, chnoeu, chelga
    character(len=24) :: listma
    integer :: isuiv
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (SUIVI_DDL - UTILITAIRE)
!
! EXTRAIRE LES VALEURS - ECRITURE DANS LE TABLEAU DE CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  TYPCHA : TYPE DU CHAMP
! IN  NBCMP  : NOMBRE DE COMPOSANTES DANS LA SD
! IN  NBNO   : NOMBRE DE NOEUDS DANS LA SD
! IN  NBMA   : NOMBRE DE MAILLES DANS LA SD
! IN  NBPI   : NOMBRE DE POINTS D'INTEGRATION
! IN  NBSPI  : NOMBRE DE SOUS-POINTS D'INTEGRATION
! IN  LISTMA : LISTE CONTENANT LES MAILLES
! IN  EXTRGA : TYPE D'EXTRACTION SUR UNE MAILLE
! IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
! IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
! IN  CHNOEU : VECTEUR DE TRAVAIL CHAMPS AUX NOEUDS
! IN  CHELGA : VECTEUR DE TRAVAIL CHAMPS AUX ELEMENTS
! IN  CHAMP  : CHAMP A EXTRAIRE
! I/O ISUIV  : NUMERO COURANT DU SUIVI_DDL
!
!
!
!
    integer :: ino, ima, ipi, ispi, nummai
    integer :: nbnor, nbmar
    integer :: ivalcp, jcesd, jma
    real(kind=8) :: valr
    integer :: nvalcp
    integer :: npi, nspi, nmapt, nmaspt, nbpir, nbspir
    integer :: jnoeu, jelga
    character(len=19) :: cheles
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    cheles = '&&NMSUI3.TRAV'
!
! --- PASSAGE EN CHAM_ELEM_S
!
    if (typcha .eq. 'ELGA') then
        call sdmpic('CHAM_ELEM', champ)
        call celces(champ, 'V', cheles)
        call jeveuo(cheles(1:19)//'.CESD', 'L', jcesd)
    endif
!
! --- NOMBRE DE NOEUDS POUR LA BOUCLE
!
    if (typcha .eq. 'NOEU') then
        if (extrch .eq. 'VALE') then
            nbnor = nbno
            elseif ((extrch.eq.'MIN').or. (extrch.eq.'MAX').or. (&
        extrch.eq.'MOY').or. (extrch.eq.'MINI_ABS').or. (&
        extrch.eq.'MAXI_ABS')) then
            nbnor = 1
        else
            call assert(.false.)
        endif
    endif
!
! --- NOMBRE DE MAILLES POUR LA BOUCLE
!
    if (typcha .eq. 'ELGA') then
        if (extrch .eq. 'VALE') then
            nbmar = nbma
            elseif ((extrch.eq.'MIN').or. (extrch.eq.'MAX').or. (&
        extrch.eq.'MOY').or. (extrch.eq.'MINI_ABS').or. (&
        extrch.eq.'MAXI_ABS')) then
            nbmar = 1
        else
            call assert(.false.)
        endif
    endif
!
! --- NOMBRE DE COMPOSANTES POUR LA BOUCLE
!
    if (extrcp .eq. ' ') then
        nvalcp = nbcmp
    else
        nvalcp = 1
    endif
!
! --- VALEUR NODALES
!
    if (typcha .eq. 'NOEU') then
        call jeveuo(chnoeu, 'L', jnoeu)
!
        do 20 ino = 1, nbnor
!
! ------- ECRITURE DES VALEURS
!
            do 21 ivalcp = 1, nvalcp
                valr = zr(jnoeu+ivalcp-1 +nbcmp*(ino-1))
                call nmsuiy(sdimpr, valr, isuiv)
21          continue
20      continue
    endif
!
! --- VALEURS AUX POINTS DE GAUSS
!
    if (typcha .eq. 'ELGA') then
        call jeveuo(chelga, 'L', jelga)
        call jeveuo(listma, 'L', jma)
!
! ----- BOUCLE SUR LES MAILLES
!
        do 30 ima = 1, nbmar
!
! ----- MAILLE COURANTE
!
            nummai = zi(jma-1+ima)
!
! ------- NOMBRE EFFECTIF DE POINTS/SOUS-POINTS SUR LA MAILLE
!
            nmapt = zi(jcesd+5+4*(nummai-1))
            nmaspt = zi(jcesd+5+4*(nummai-1)+1)
!
! ------- PLAFONNEMENT
!
            npi = nbpi
            nspi = nbspi
            if (npi .gt. nmapt) npi = nmapt
            if (nspi .gt. nmaspt) nspi = nmaspt
!
! ------- NOMBRE DE POINTS/SOUS-POINTS POUR LA BOUCLE
!
            if (extrga .eq. 'VALE') then
                nbpir = npi
                nbspir = nspi
            else
                nbpir = 1
                nbspir = 1
            endif
!
! ------- BOUCLE SUR LES POINTS/SOUS_POINTS
!
            do 45 ipi = 1, nbpir
                do 46 ispi = 1, nbspir
!
! ----------- LECTURE DES VALEURS
!
                    do 47 ivalcp = 1, nvalcp
                        valr = zr(&
                               jelga+nbcmp*nbpi*nbspi*(ima-1) +nbpi*nbspi*(ivalcp-1) +nbspi*(ipi-&
                               &1) +(ispi- 1)&
                               )
                        call nmsuiy(sdimpr, valr, isuiv)
47                  continue
46              continue
45          continue
30      continue
    endif
!
    call jedetr(cheles)
    call jedema()
!
end subroutine
