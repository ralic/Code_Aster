subroutine deffen(base, nuor, imodi, nbmr, nbm,&
                  iaxe, long, nbnfen, nofe, discfe,&
                  nbp1, nbp2, discff, defm)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     EXTRACTION DES COMPOSANTES DES DEFORMEES MODALES SUR LA
!     FENETRE EXCITEE, DANS LES DEUX DIRECTIONS ORTHOGONALES A LA
!     POUTRE. INTERPOLATION SUR LA DISCRETISATION DES FONCTIONS DE FORME
!     ASSOCIEES A L'EXCITATION.
!     APPELANT : SPECFF
!-----------------------------------------------------------------------
! IN  : BASE   : NOM DU CONCEPT MELASFLU
! IN  : NUOR   : NUMEROS D'ORDRE DES MODES DU CONCEPT MELASFLU
! IN  : IMODI  : INDICE DU PREMIER MODE PRIS EN COMPTE
! IN  : NBMR   : NOMBRE DE MODES PRIS EN COMPTE
! IN  : NBM    : NOMBRE DE MODES DU CONCEPT MELASFLU
! IN  : IAXE   : ENTIER DEFINISSANT L'AXE DIRECTEUR
!       IAXE = 1 L'AXE DIRECTEUR EST L'AXE DES X DU REPERE GLOBAL
!       IAXE = 2 L'AXE DIRECTEUR EST L'AXE DES Y DU REPERE GLOBAL
!       IAXE = 3 L'AXE DIRECTEUR EST L'AXE DES Z DU REPERE GLOBAL
! IN  : LONG   : VALEUR DE LA LONGUEUR EXCITEE
! IN  : NBNFEN : NOMBRE DE NOEUDS APPARTENANT A LA FENETRE EXCITEE
! IN  : NOFE   : LISTE DES NUMEROS DES NOEUDS APPARTENANT A LA
!                FENETRE EXCITEE
! IN  : DISCFE : LISTE DES VALEURS DE LA COORDONNEE D'ESPACE SUR LA
!                POUTRE POUR LES NOEUDS APPARTENANT A LA FENETRE
!                EXCITEE. CES VALEURS ONT ETE PREALABLEMENT TRANSLATEES
!                AFIN DE LES SUPERPOSER AU DOMAINE DE DEFINITION DES
!                FONCTIONS DE FORME ASSOCIEES A L'EXCITATION.
! IN  : NBP1   : NOMBRE DE POINTS DE DISCRETISATION DES FONCTIONS DE
!                FORME SUR L'INTERVALLE 0,L
! IN  : NBP2   : NOMBRE DE POINTS DE DISCRETISATION DES FONCTIONS DE
!                FORME SUR L'INTERVALLE L,2L
! IN  : DISCFF : DISCRETISATION DES FONCTIONS DE FORME SUR 0,2L
! OUT : DEFM   : TABLEAU DES DEFORMEES MODALES
!
! REMARQUE
!
!  DEFM EST UN TABLEAU DE DIMENSION (NBP,NBMR).
!
!  DANS CHAQUE COLONNE (I.E. POUR CHAQUE MODE PRIS EN COMPTE), LES
!  NBP1 PREMIERES VALEURS DONNENT LA COMPOSANTE DE LA DEFORMEE DANS
!  LA PREMIERE DIRECTION ORTHOGONALE A LA POUTRE. LES NBP2 SUIVANTES
!  DONNENT LA COMPOSANTE DANS LA DEUXIEME DIRECTION ORTHOGONALE A LA
!  POUTRE.
!
!  DANS UN PREMIER TEMPS, CES VALEURS SONT EXTRAITES SUR LA
!  DISCRETISATION DISCFE PUIS SONT INTERPOLEES SUR LA DISCRETISATION
!  DES FONCTIONS DE FORME DISCFF. LES VALEURS INTERPOLEES SONT ENSUITE
!  RANGEES DANS LE TABLEAU DEFM.
!
!
#include "jeveux.h"
#include "asterfort/fointr.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: imodi, nbmr, nbm, nuor(nbm)
    integer :: iaxe, nbnfen, nofe(nbnfen), nbp1, nbp2
    real(kind=8) :: long, discfe(nbnfen), discff(nbp1+nbp2)
    real(kind=8) :: defm(nbp1+nbp2, nbmr)
    character(len=19) :: base
!
    character(len=8) :: nompar
    character(len=24) :: chvale
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: idir1, idir2, ier1, ier2, ifen2, imod, imodf
    integer :: imr, iprol, iv, ivale, ivale1, ivale2, j
    integer :: numnoe
!-----------------------------------------------------------------------
    call jemarq()
!
! --- 1.INITIALISATIONS
!
    imodf = imodi + nbmr - 1
    iv = 1
    if (iaxe .eq. 1) then
        idir1 = 2
        idir2 = 3
        nompar = 'X'
    else if (iaxe.eq.2) then
        idir1 = 3
        idir2 = 1
        nompar = 'Y'
    else
        idir1 = 1
        idir2 = 2
        nompar = 'Z'
    endif
!
! --- 2.CREATION DE VECTEURS DE TRAVAIL POUR FOINTR
!
! --- 2.1.CREATION D'UN CHPROL
!
    call wkvect('&&DEFFEN.TEMP.PROL', 'V V K24', 6, iprol)
    zk24(iprol) = 'FONCTION'
    zk24(iprol+1) = 'LIN LIN '
    zk24(iprol+2) = nompar
    zk24(iprol+3) = 'TOUTRESU'
    zk24(iprol+4) = 'CC      '
    zk24(iprol+5) = '&&DEFFEN.TEMP.PR'
!
! --- 2.2.CREATION D'UNE DISCRETISATION TRANSLATEE SUR LA FENETRE
! ---     EXCITEE, UTILE POUR L'INTERPOLATION SUR L,2L
!
    call wkvect('&&DEFFEN.TEMP.FEN2', 'V V R', nbnfen, ifen2)
    do 10 j = 1, nbnfen
        zr(ifen2+j-1) = discfe(j) + long
10  end do
!
! --- 3.EXTRACTION DES COMPOSANTES DES DEFORMEES SUR LA DISCRETISATION
! ---   DE LA FENETRE EXCITEE ET INTERPOLATION SUR LA DISCRETISATION DES
! ---   FONCTIONS DE FORME
!
    call wkvect('&&DEFFEN.TEMP.VALE1', 'V V R', nbnfen, ivale1)
    call wkvect('&&DEFFEN.TEMP.VALE2', 'V V R', nbnfen, ivale2)
!
    do 20 imod = imodi, imodf
!
        write(chvale,'(A8,A5,2I3.3,A5)') base(1:8),'.C01.',nuor(imod),&
        iv,'.VALE'
        call jeveuo(chvale, 'L', ivale)
!
!-------EXTRACTION
!
        do 40 j = 1, nbnfen
            numnoe = nofe(j)
            zr(ivale1+j-1) = zr(ivale+6*(numnoe-1)+idir1-1)
            zr(ivale2+j-1) = zr(ivale+6*(numnoe-1)+idir2-1)
40      continue
        call jelibe(chvale)
!
!-------INTERPOLATION
!
        imr = imod - imodi + 1
        call fointr(' ', zk24(iprol), nbnfen, discfe, zr(ivale1),&
                    nbp1, discff, defm(1, imr), ier1)
        call fointr(' ', zk24(iprol), nbnfen, zr(ifen2), zr(ivale2),&
                    nbp2, discff(nbp1+1), defm(nbp1+1, imr), ier2)
        if (ier1 .ne. 0 .or. ier2 .ne. 0) then
            call utmess('F', 'MODELISA4_39')
        endif
!
20  end do
!
    call jedetr('&&DEFFEN.TEMP.PROL')
    call jedetr('&&DEFFEN.TEMP.FEN2')
    call jedetr('&&DEFFEN.TEMP.VALE1')
    call jedetr('&&DEFFEN.TEMP.VALE2')
    call jedema()
end subroutine
