subroutine nmobs2(noma, sdobse, nomtab, instan, titobs,&
                  typcha, nomcha, nomchs, nbma, nbno,&
                  nbpi, nbspi, nbcmp, extrga, extrch,&
                  extrcp, listno, listma, listpi, listsp,&
                  listcp, champ, chnoeu, chelga, nobsef)
!
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
! aslint: disable=W1504
    implicit      none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nmobsz.h"
#include "asterfort/sdmpic.h"
    character(len=8) :: noma
    character(len=24) :: nomcha, nomchs
    character(len=19) :: sdobse, nomtab
    character(len=80) :: titobs
    integer :: nbcmp, nbno, nbma
    integer :: nbpi, nbspi, nobsef
    character(len=4) :: typcha
    real(kind=8) :: instan
    character(len=24) :: listno, listma, listpi, listcp, listsp
    character(len=8) :: extrga, extrch, extrcp
    character(len=19) :: chnoeu, chelga
    character(len=19) :: champ
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (OBSERVATION - UTILITAIRE)
!
! EXTRAIRE LES VALEURS - SAUVEGARDE DANS LA TABLE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  TITOBS : TITRE DE L'OBSERVATION
! IN  TYPCHA : TYPE DU CHAMP
! IN  NOMCHA : NOM DU CHAMP
! IN  NOMCHS : NOM DU CHAMP SIMPLE
! IN  NBCMP  : NOMBRE DE COMPOSANTES DANS LA SD
! IN  NBNO   : NOMBRE DE NOEUDS DANS LA SD
! IN  NBMA   : NOMBRE DE MAILLES DANS LA SD
! IN  NBPI   : NOMBRE DE POINTS D'INTEGRATION
! IN  NBSPI  : NOMBRE DE SOUS-POINTS D'INTEGRATION
! IN  EXTRGA : TYPE D'EXTRACTION SUR UNE MAILLE
! IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
! IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
! IN  LISTNO : LISTE CONTENANT LES NOEUDS
! IN  LISTMA : LISTE CONTENANT LES MAILLES
! IN  LISTCP : LISTE DES COMPOSANTES
! IN  LISTPI : LISTE CONTENANT LES POINTS D'EXTRACTION
! IN  LISTSP : LISTE CONTENANT LES SOUS-POINTS D'EXTRACTION
! IN  CHAMP  : CHAMP A EXTRAIRE
! IN  CHNOEU : VECTEUR DE TRAVAIL CHAMPS AUX NOEUDS
! IN  CHELGA : VECTEUR DE TRAVAIL CHAMPS AUX ELEMENTS
! I/O NOBSEF : NOMBRE EFFECTIF D'OBSERVATIONS
!
! ----------------------------------------------------------------------
!
    integer :: nparx
    parameter    (nparx=20)
    character(len=8) :: nompar(nparx)
!
    integer :: ino, ima, ipi, ispi, icmp
    integer :: nbnor, nbmar, iret
    integer :: ivalcp, jcesd
    real(kind=8) :: valr
    integer :: nvalcp
    integer :: num, snum, npi, nspi, nmapt, nmaspt, nbpir, nbspir
    integer :: numnoe, nummai
    character(len=8) :: nomnoe, nommai, nomcmp
    integer :: jno, jma, jpi, jspi, jnoeu, jelga, jcmp
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- PASSAGE EN CHAM_ELEM_S
!
    if (typcha .eq. 'ELGA') then
        call jeexin(nomchs, iret)
        if (iret .eq. 0) then
            call sdmpic('CHAM_ELEM', champ)
            call celces(champ, 'V', nomchs)
        endif
        call jeveuo(nomchs(1:19)//'.CESD', 'L', jcesd)
    endif
!
! --- NOMBRE DE NOEUDS POUR LA BOUCLE
!
    if (typcha .eq. 'NOEU') then
        if (extrch .eq. 'VALE') then
            nbnor = nbno
            elseif ((extrch.eq.'MIN').or. (extrch.eq.'MAX').or. (&
        extrch.eq.'MAXI_ABS').or. (extrch.eq.'MINI_ABS').or. (&
        extrch.eq.'MOY')) then
            nbnor = 1
        else
            ASSERT(.false.)
        endif
    endif
!
! --- NOMBRE DE MAILLES POUR LA BOUCLE
!
    if (typcha .eq. 'ELGA') then
        if (extrch .eq. 'VALE') then
            nbmar = nbma
            elseif ((extrch.eq.'MIN').or. (extrch.eq.'MAX').or. (&
        extrch.eq.'MAXI_ABS').or. (extrch.eq.'MINI_ABS').or. (&
        extrch.eq.'MOY')) then
            nbmar = 1
        else
            ASSERT(.false.)
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
! --- RECOPIE DU NOM DES COMPOSANTES
!
    call jeveuo(listcp, 'L', jcmp)
    do 280 icmp = 1, nbcmp
        nompar(icmp) = zk8(jcmp-1+icmp)
280  end do
!
! --- VALEUR NODALES
!
    if (typcha .eq. 'NOEU') then
        call jeveuo(chnoeu, 'L', jnoeu)
        call jeveuo(listno, 'L', jno)
!
        do 20 ino = 1, nbnor
!
! ------- NOEUD COURANT
!
            numnoe = zi(jno-1+ino)
            call jenuno(jexnum(noma(1:8)//'.NOMNOE', numnoe), nomnoe)
!
! ------- ECRITURE DES VALEURS
!
            do 21 ivalcp = 1, nvalcp
                valr = zr(jnoeu+ivalcp-1 +nbcmp*(ino-1))
                nomcmp = nompar(ivalcp)
                call nmobsz(sdobse, nomtab, titobs, nomcha, typcha,&
                            extrch, extrcp, extrga, nomcmp, nomnoe,&
                            nommai, num, snum, instan, valr)
                nobsef = nobsef + 1
21          continue
20      continue
    endif
!
! --- VALEURS AUX POINTS DE GAUSS
!
    if (typcha .eq. 'ELGA') then
        call jeveuo(chelga, 'L', jelga)
        call jeveuo(listma, 'L', jma)
        call jeveuo(listpi, 'L', jpi)
        call jeveuo(listsp, 'L', jspi)
!
! ----- BOUCLE SUR LES MAILLES
!
        do 30 ima = 1, nbmar
!
! ------- MAILLE COURANTE
!
            nummai = zi(jma-1+ima)
            call jenuno(jexnum(noma(1:8)//'.NOMMAI', nummai), nommai)
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
! ----------- NUMERO DES POINTS/SOUS-POINTS
!
                    if (extrga .eq. 'VALE') then
                        num = zi(jpi-1+ipi )
                        snum = zi(jspi-1+ispi )
                    else
                        num = ipi
                        snum = ispi
                    endif
!
! ----------- LECTURE DES VALEURS
!
                    do 47 ivalcp = 1, nvalcp
                        valr = zr(&
                               jelga+nbcmp*nbpi*nbspi*(ima-1) +nbpi*nbspi*(ivalcp-1) +nbspi*(ipi-&
                               &1) +(ispi- 1)&
                               )
                        nomcmp = nompar(ivalcp)
                        call nmobsz(sdobse, nomtab, titobs, nomcha, typcha,&
                                    extrch, extrcp, extrga, nomcmp, nomnoe,&
                                    nommai, num, snum, instan, valr)
                        nobsef = nobsef + 1
47                  continue
46              continue
45          continue
30      continue
    endif
!
    call jedema()
!
end subroutine
